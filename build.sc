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
  }

  def includeFunctions = T {
    Seq("mlirGetDialectHandle__firrtl__")
  }

  def includeMacros = T {
    Seq.empty[String]
  }

  def includeStructs = T {
    Seq.empty[String]
  }

  def includeTypedefs = T {
    Seq.empty[String]
  }

  def includeUnions = T {
    Seq.empty[String]
  }

  def includeVars = T {
    Seq.empty[String]
  }

  override def generatedSources: T[Seq[PathRef]] = T {
    os.proc(
      Seq(
        jextract().path.toString,
        (circt.install().path / "include" / "circt-c" / "Dialect" / "FIRRTL.h").toString,
        "-I", (circt.install().path / "include").toString,
        "--source",
        "--output", T.dest.toString
      ) ++ includeFunctions().flatMap(f => Seq("--include-function", f)) ++
        includeMacros().flatMap(f => Seq("--include-macro", f)) ++
        includeStructs().flatMap(f => Seq("--include-function", f)) ++
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

    def ivyDeps = Agg(ivy"com.lihaoyi::utest:0.8.1")
  }
}

object circt extends Module {
  def circtSourcePath = os.pwd / "dependencies" / "circt"

  def llvmSourcePath = os.pwd / "dependencies" / "llvm-project"

  def install = T.persistent {
    os.proc(
      "cmake",
      "-B", T.dest,
      "-G", "Ninja",
      s"-DCMAKE_INSTALL_PREFIX=${T.dest / "install"}",
      "-DCMAKE_BUILD_TYPE=Debug",
      "-DLLVM_ENABLE_PROJECTS=mlir",
      "-DLLVM_ENABLE_ASSERTIONS=ON",
      "-DLLVM_BUILD_EXAMPLES=OFF",
      "-DLLVM_ENABLE_OCAMLDOC=OFF",
      "-DLLVM_ENABLE_BINDINGS=OFF",
      "-DLLVM_CCACHE_BUILD=OFF",
      "-DLLVM_BUILD_TOOLS=ON",
      "-DLLVM_OPTIMIZED_TABLEGEN=ON",
      "-DLLVM_INCLUDE_TOOLS=ON",
      "-DLLVM_USE_SPLIT_DWARF=ON",
      "-DLLVM_BUILD_LLVM_DYLIB=ON",
      "-DLLVM_LINK_LLVM_DYLIB=ON",
      "-DLLVM_EXTERNAL_PROJECTS=circt",
      s"-DLLVM_EXTERNAL_CIRCT_SOURCE_DIR=$circtSourcePath"
    ).call(llvmSourcePath / "llvm")
    os.proc(
      "ninja",
      "install-mlir-headers-stripped",
      "install-circt-headers-stripped",
      "install-llvm-headers-stripped"
    ).call(T.dest)
    PathRef(T.dest / "install")
  }
}
