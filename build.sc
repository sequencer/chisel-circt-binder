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

object circt extends Module {
  def circtSourcePath = os.pwd / "dependencies" / "circt"

  def llvmSourcePath = os.pwd / "dependencies" / "llvm-project"

  def installDirectory = T {
    T.dest
  }

  def install = T {
    os.proc("ninja", "-j64", "install").call(cmake())
  }

  def cmake = T.persistent {
    os.proc(
      "cmake",
      "-S", llvmSourcePath / "llvm",
      "-B", T.dest,
      "-G", "Ninja",
      s"-DCMAKE_INSTALL_PREFIX=${installDirectory()}",
      "-DCMAKE_BUILD_TYPE=Release",
      "-DLLVM_ENABLE_PROJECTS=mlir",
      "-DLLVM_TARGETS_TO_BUILD=X86",
      "-DLLVM_ENABLE_ASSERTIONS=OFF",
      "-DLLVM_BUILD_EXAMPLES=OFF",
      "-DLLVM_INCLUDE_EXAMPLES=OFF",
      "-DLLVM_INCLUDE_TESTS=OFF",
      "-DLLVM_INSTALL_UTILS=OFF",
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
    ).call(T.dest)
    T.dest
  }
}

object `circt-jextract` extends common.ChiselCIRCTBinderPublishModule
  with JavaModule {
  def javacVersion = T.input {
    val version = os.proc("javac", "-version").call().out.text.split(' ').last.split('.').head.toInt
    require(version >= 19, "Java 19 or higher is required")
    version
  }

  override def javacOptions: T[Seq[String]] = {
    Seq("--enable-preview", "--source", javacVersion().toString)
  }

  def jextractTarGz = T.persistent {
    val f = T.dest / "jextract.tar.gz"
    if (!os.exists(f))
      Util.download(s"https://download.java.net/java/early_access/jextract/2/openjdk-19-jextract+2-3_linux-x64_bin.tar.gz", os.rel / "jextract.tar.gz")
    PathRef(f)
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
      circt.installDirectory() / "include" / "circt-c" / "Dialect" / "FIRRTL.h",
      "-I", circt.installDirectory() / "include",
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
      "firrtlVisitExtModule",
      "firrtlVisitParameter",
      "firrtlVisitPort",
      "firrtlVisitStatement",
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
      "FirrtlParameterInt",
      "FirrtlParameterDouble",
      "FirrtlParameterString",
      "FirrtlParameterRaw",
      "FirrtlParameter",
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
      "FirrtlStatementAttachOperand",
      "FirrtlStatementAttach",
      "FirrtlStatement",
    )
  }

  def includeTypedefs = T {
    Seq(
      "FirrtlStringRef",
      "FirrtlErrorHandler",
      // enums (FIXME)
      "FirrtlPortDirection",
      "FirrtlParameterKind",
      "FirrtlTypeKind",
      "FirrtlStatementKind",
    )
  }

  def includeUnions = T {
    Seq(
      "FirrtlParameterUnion",
      "FirrtlTypeUnion",
      "FirrtlStatementUnion",
    )
  }

  def includeVars = T {
    Seq.empty[String]
  }

  override def generatedSources: T[Seq[PathRef]] = T {
    circt.install()
    os.proc(
      Seq(
        jextract().path.toString,
        (circt.installDirectory() / "include" / "circt-c" / "Dialect" / "FIRRTL.h").toString,
        "-I", (circt.installDirectory() / "include").toString,
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
    Lib.findSourceFiles(os.walk(T.dest).map(PathRef(_)), Seq("java")).distinct.map(PathRef(_))
  }

  // mill doesn't happy with the --enable-preview flag, so we work around it
  final override def compile: T[mill.scalalib.api.CompilationResult] = T {
    os.proc(Seq("javac", "-d", T.dest.toString) ++ javacOptions() ++ allSourceFiles().map(_.path.toString)).call(T.dest)
    mill.scalalib.api.CompilationResult(os.root, PathRef(T.dest))
  }
}

object `chisel-circt-binder` extends common.ChiselCIRCTBinderModule
  with ScalaModule
  with ScalafmtModule {
  m =>
  def scalaVersion = T {
    v.scala
  }

  def circtJextractModule = `circt-jextract`

  def chisel3Module = Some(mychisel3)

  def chisel3PluginJar = T {
    Some(mychisel3.plugin.jar())
  }

  object tests extends Tests with TestModule.Utest {
    override def forkArgs: T[Seq[String]] = {
      Seq(
        "--enable-native-access=ALL-UNNAMED",
        "--enable-preview",
        s"-Djava.library.path=${circt.installDirectory() / "lib"}"
      )
    }

    def ivyDeps = Agg(ivy"com.lihaoyi::utest:0.8.1")
  }
}
