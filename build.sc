import mill._
import mill.scalalib._
import mill.scalalib.publish._
import mill.scalalib.scalafmt._
import mill.modules.Util
import $file.dependencies.chisel3.build
import $file.dependencies.firrtl.build
import $file.dependencies.treadle.build
import $file.dependencies.chiseltest.build
import $file.common

object v {
  val scala = "2.13.10"
}

object myfirrtl extends dependencies.firrtl.build.firrtlCrossModule(v.scala) {
  override def millSourcePath = os.pwd / "dependencies" / "firrtl"

  override val checkSystemAntlr4Version = false
  override val checkSystemProtocVersion = false
  override val protocVersion = os.proc("protoc", "--version").call().out.text.dropRight(1).split(' ').last
  override val antlr4Version = os.proc("antlr4").call().out.text.split('\n').head.split(' ').last
}

object mytreadle extends dependencies.treadle.build.treadleCrossModule(v.scala) {
  override def millSourcePath = os.pwd / "dependencies" / "treadle"

  def firrtlModule: Option[PublishModule] = Some(myfirrtl)
}

object mychisel3 extends dependencies.chisel3.build.chisel3CrossModule(v.scala) {
  override def millSourcePath = os.pwd / "dependencies" / "chisel3"

  def firrtlModule: Option[PublishModule] = Some(myfirrtl)

  def treadleModule: Option[PublishModule] = Some(mytreadle)

  def chiseltestModule: Option[PublishModule] = Some(mychiseltest)
}

object mychiseltest extends dependencies.chiseltest.build.chiseltestCrossModule(v.scala) {
  override def millSourcePath = os.pwd / "dependencies" / "chiseltest"

  def chisel3Module: Option[PublishModule] = Some(mychisel3)

  def treadleModule: Option[PublishModule] = Some(mytreadle)
}

object `chisel-circt-binder` extends common.ChiselCIRCTBinderModule with ScalafmtModule {
  m =>
  def scalaVersion = T {
    v.scala
  }

  override def javacOptions: T[Seq[String]] = {
    Seq("--enable-preview", "--release", "19")
  }

  def chisel3Module = Some(mychisel3)

  def chisel3PluginJar = T {
    Some(mychisel3.plugin.jar())
  }

  def jextractTarGz = T.persistent {
    Util.download(s"https://download.java.net/java/early_access/jextract/2/openjdk-19-jextract+2-3_linux-x64_bin.tar.gz", os.rel / "jextract.tar.gz")
    PathRef(T.dest / "jextract.tar.gz")
  }

  def jextract = T.persistent {
    os.proc("tar", "xvf", jextractTarGz().path).call(T.dest)
    PathRef(T.dest / "jextract-19" / "bin" / "jextract")
  }

  // Generate all possible bindings
  def dumpAllIncludes = T {
    val f = os.temp()
    os.proc(
      jextract().path,
      circt.install().path / "include" / "circt-c" / "Dialect" / "FIRRTL.h",
      "-I", circt.install().path / "include",
      "--dump-includes", f
    ).call()
    os.read.lines(f).filter(s => s.nonEmpty && !s.startsWith("#"))
  }

  def includeFunctions = T {
    Seq(
      "firrtlCreateContext",
      "firrtlDestroyContext",
      "firrtlSetErrorHandler",
      "firrtlVisitCircuit",
      "firrtlVisitModule",
      "firrtlVisitPort",
      "firrtlExportFirrtl",
      "firrtlDestroyString",
    )
  }

  def includeMacros = T {
    Seq.empty[String]
  }

  def includeStructs = T {
    Seq(
      "MlirStringRef",
      "FirrtlContext",
      "FirrtlTypeUInt",
      "FirrtlTypeSInt",
      "FirrtlTypeClock",
      "FirrtlTypeReset",
      "FirrtlTypeAsyncReset",
      "FirrtlTypeAnalog",
      "FirrtlTypeVector",
      "FirrtlTypeBundleField",
      "FirrtlTypeBundle",
      "FirrtlType",
    )
  }

  def includeTypedefs = T {
    Seq(
      "FirrtlStringRef",
      "FirrtlErrorHandler",
      // enums (FIXME)
      "FirrtlPortDirection",
      "FirrtlTypeKind",
    )
  }

  def includeUnions = T {
    Seq(
      "FirrtlTypeUnion",
    )
  }

  def includeVars = T {
    Seq.empty[String]
  }

  override def generatedSources: T[Seq[PathRef]] = T {
    circt.circtTag()
    os.proc(
      Seq(
        jextract().path.toString,
        (circt.install().path / "include" / "circt-c" / "Dialect" / "FIRRTL.h").toString,
        "-I", (circt.install().path / "include").toString,
        "-t", "org.llvm.circt.firrtl",
        "-l", "CIRCTCAPIFIRRTL",
        "--header-class-name", "CIRCTCAPIFIRRTL",
        "--source",
        "--output", T.dest.toString
      ) ++ includeFunctions().flatMap(f => Seq("--include-function", f)) ++
        includeMacros().flatMap(f => Seq("--include-macro", f)) ++
        includeStructs().flatMap(f => Seq("--include-struct", f)) ++
        includeTypedefs().flatMap(f => Seq("--include-typedef", f)) ++
        includeUnions().flatMap(f => Seq("--include-union", f)) ++
        includeVars().flatMap(f => Seq("--include-var", f))
    ).call()
    Lib.findSourceFiles(os.walk(T.dest).map(PathRef(_)), Seq("java")).map(PathRef(_))
  }

  object tests extends Tests with TestModule.Utest {
    def scalacOptions = {
      m.scalacOptions()
    }

    override def forkArgs: T[Seq[String]] = {
      Seq(
        "--enable-native-access=ALL-UNNAMED",
        "--enable-preview",
        s"-Djava.library.path=${circt.install().path / "lib"}"
      )
    }

    def ivyDeps = Agg(ivy"com.lihaoyi::utest:0.8.1")
  }
}

object circt extends Module {
  def circtSourcePath = os.pwd / "dependencies" / "circt"

  def circtTag = T.input {
    os.proc("git", "describe", "--dirty").call(circtSourcePath)
  }

  def install = T.persistent {
    circtTag()
    os.proc(
      "cmake",
      "-B", T.dest,
      "-G", "Ninja",
      s"-DCMAKE_INSTALL_PREFIX=${T.dest / "install"}",
      "-DCMAKE_BUILD_TYPE=Release",
      "-DLLVM_ENABLE_PROJECTS=mlir",
      "-DLLVM_TARGETS_TO_BUILD=host",
      "-DLLVM_ENABLE_ASSERTIONS=OFF",
      "-DLLVM_BUILD_EXAMPLES=OFF",
      "-DLLVM_ENABLE_OCAMLDOC=OFF",
      "-DLLVM_ENABLE_BINDINGS=OFF",
      "-DLLVM_CCACHE_BUILD=OFF",
      "-DLLVM_BUILD_TOOLS=OFF",
      "-DLLVM_OPTIMIZED_TABLEGEN=ON",
      "-DLLVM_USE_SPLIT_DWARF=ON",
      "-DLLVM_BUILD_LLVM_DYLIB=OFF",
      "-DLLVM_LINK_LLVM_DYLIB=OFF",
      "-DLLVM_EXTERNAL_PROJECTS=circt",
      "-DBUILD_SHARED_LIBS=ON",
      s"-DLLVM_EXTERNAL_CIRCT_SOURCE_DIR=$circtSourcePath"
    ).call(circtSourcePath / "llvm" / "llvm")
    os.proc("ninja", "install").call(T.dest)
    PathRef(T.dest / "install")
  }
}
