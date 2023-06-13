import mill._
import mill.scalalib._
import mill.scalalib.publish._
import mill.scalalib.scalafmt._
import mill.modules.Util
import $file.dependencies.chisel3.build
import $file.common

object v {
  val scala = "2.13.10"
}

object mychisel3 extends dependencies.chisel3.build.Chisel(v.scala) {
  override def millSourcePath = os.pwd / "dependencies" / "chisel3"
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
    // @formatter:off
    os.proc(
      "cmake",
      "-S", llvmSourcePath / "llvm",
      "-B", T.dest,
      "-G", "Ninja",
      s"-DCMAKE_INSTALL_PREFIX=${installDirectory()}",
      "-DCMAKE_BUILD_TYPE=Debug",
      "-DLLVM_ENABLE_PROJECTS=mlir",
      "-DLLVM_TARGETS_TO_BUILD=X86",
      "-DLLVM_ENABLE_ASSERTIONS=ON",
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
    // @formatter:on
    T.dest
  }
}

object `circt-jextract` extends common.ChiselCIRCTBinderPublishModule with JavaModule {
  def javacVersion = T.input {
    val version = os
      .proc("javac", "-version")
      .call()
      .out
      .text
      .split(' ')
      .last
      .split('.')
      .head
      .toInt
    require(version >= 20, "Java 20 or higher is required")
    version
  }

  override def javacOptions: T[Seq[String]] = {
    Seq("--enable-preview", "--source", javacVersion().toString)
  }

  def jextractTarGz = T.persistent {
    val f = T.dest / "jextract.tar.gz"
    if (!os.exists(f))
      Util.download(
        s"https://download.java.net/java/early_access/jextract/1/openjdk-20-jextract+1-2_linux-x64_bin.tar.gz",
        os.rel / "jextract.tar.gz"
      )
    PathRef(f)
  }

  def jextract = T.persistent {
    os.proc("tar", "xvf", jextractTarGz().path).call(T.dest)
    PathRef(T.dest / "jextract-20" / "bin" / "jextract")
  }

  // Generate all possible bindings
  def dumpAllIncludes = T {
    val f = os.temp()
    // @formatter:off
    os.proc(
      jextract().path,
      circt.installDirectory() / "include" / "circt-c" / "Dialect" / "FIRRTL.h",
      "-I", circt.installDirectory() / "include",
      "-I", circt.llvmSourcePath / "mlir" / "include",
      "--dump-includes", f
    ).call()
    // @formatter:on
    os.read.lines(f).filter(s => s.nonEmpty && !s.startsWith("#"))
  }

  def includeFunctions = T {
    Seq(
      //
      // MLIR
      "mlirContextCreate",
      "mlirContextDestroy",
      "mlirGetDialectHandle__firrtl__",
      "mlirGetDialectHandle__chirrtl__",
      "mlirDialectHandleLoadDialect",
      // "mlirStringRefCreate", // inline function cannot be generated
      "mlirStringRefCreateFromCString",
      "mlirLocationUnknownGet",
      "mlirModuleCreateEmpty",
      "mlirModuleDestroy",
      "mlirModuleGetBody",
      "mlirModuleGetOperation",
      "mlirOperationStateGet",
      "mlirNamedAttributeGet",
      "mlirIntegerAttrGet",
      "mlirStringAttrGet",
      "mlirArrayAttrGet",
      "mlirTypeAttrGet",
      "mlirArrayAttrGet",
      "mlirUnitAttrGet",
      ////////////////////
      // Integer types
      ////////////////////
      "mlirIntegerTypeGet",
      "mlirIntegerTypeUnsignedGet",
      "mlirIntegerTypeSignedGet",
      ////////////////////
      "mlirIdentifierGet",
      // "mlirAttributeParseGet", // We should not "parse" anything
      "mlirOperationStateAddOperands",
      "mlirOperationStateAddResults",
      "mlirOperationStateAddAttributes",
      "mlirOperationGetResult",
      "mlirRegionCreate",
      "mlirOperationCreate",
      "mlirBlockCreate",
      "mlirBlockGetArgument",
      "mlirBlockAppendOwnedOperation",
      "mlirRegionAppendOwnedBlock",
      "mlirOperationStateAddOwnedRegions",
      "mlirOperationDump",
      "mlirExportFIRRTL",
      //
      // FIRRTL Type
      "firrtlGetTypeUInt",
      "firrtlGetTypeSInt",
      "firrtlGetTypeClock",
      "firrtlGetTypeReset",
      "firrtlGetTypeAsyncReset",
      "firrtlGetTypeAnalog",
      "firrtlGetTypeVector",
      "firrtlGetTypeBundle",
      //
      // FIRRTL Attribute
      "firrtlGetAttrPortDirections",
      "firrtlGetAttrNameKind",
      "firrtlGetAttrRUW",
      "firrtlGetAttrMemoryInit",
      //
      // CHIRRTL Attribute
      "chirrtlGetTypeCMemory",
      "chirrtlGetTypeCMemoryPort"
    )
  }

  def includeConstants = T {
    Seq(
      // enum FIRRTLPortDirection
      "FIRRTL_PORT_DIRECTION_INPUT",
      "FIRRTL_PORT_DIRECTION_OUTPUT",
      // enum FIRRTLNameKind
      "FIRRTL_NAME_KIND_DROPPABLE_NAME",
      "FIRRTL_NAME_KIND_INTERESTING_NAME",
      // enum FIRRTLRUW
      "FIRRTL_RUW_UNDEFINED",
      "FIRRTL_RUW_OLD",
      "FIRRTL_RUW_NEW"
    )
  }

  def includeStructs = T {
    Seq(
      "MlirContext",
      "MlirDialectHandle",
      "MlirStringRef",
      "MlirType",
      "MlirValue",
      "MlirLocation",
      "MlirAttribute",
      "MlirModule",
      "MlirBlock",
      "MlirRegion",
      "MlirOperation",
      "MlirOperationState",
      "MlirNamedAttribute",
      "FIRRTLBundleField"
    )
  }

  def includeTypedefs = T {
    Seq(
      "MlirStringCallback"
    )
  }

  def includeUnions = T {
    Seq.empty[String]
  }

  def includeVars = T {
    Seq.empty[String]
  }

  override def generatedSources: T[Seq[PathRef]] = T {
    circt.install()
    // @formatter:off
    os.proc(
      Seq(
        jextract().path.toString,
        "chisel-circt-binder/jextract-headers.h",
        "-I", (circt.installDirectory() / "include").toString,
        "-I", (circt.llvmSourcePath / "mlir" / "include").toString,
        "-t", "org.llvm.circt",
        "-l", "MLIRCAPIIR",
        "-l", "CIRCTCAPIFIRRTL",
        "-l", "CIRCTCAPICHIRRTL",
        "-l", "CIRCTCAPIExportFIRRTL",
        "-l", "CIRCTCAPIExportVerilog",
        "-l", "CIRCTFIRRTL",
        "-l", "CIRCTExportFIRRTL",
        "-l", "CIRCTExportVerilog",
        "-l", "MLIRCAPIRegisterEverything",
        "--header-class-name", "c_api",
        "--source",
        "--output", T.dest.toString
      ) ++ includeFunctions().flatMap(f => Seq("--include-function", f)) ++
        includeConstants().flatMap(f => Seq("--include-constant", f)) ++
        includeStructs().flatMap(f => Seq("--include-struct", f)) ++
        includeTypedefs().flatMap(f => Seq("--include-typedef", f)) ++
        includeUnions().flatMap(f => Seq("--include-union", f)) ++
        includeVars().flatMap(f => Seq("--include-var", f))
    ).call()
    // @formatter:on
    Lib
      .findSourceFiles(os.walk(T.dest).map(PathRef(_)), Seq("java"))
      .distinct
      .map(PathRef(_))
  }

  // mill doesn't happy with the --enable-preview flag, so we work around it
  final override def compile: T[mill.scalalib.api.CompilationResult] = T {
    os.proc(
      Seq("javac", "-d", T.dest.toString) ++ javacOptions() ++ allSourceFiles()
        .map(_.path.toString)
    ).call(T.dest)
    mill.scalalib.api.CompilationResult(os.root, PathRef(T.dest))
  }
}

object `chisel-circt-binder` extends common.ChiselCIRCTBinderModule with ScalaModule with ScalafmtModule {
  m =>
  def scalaVersion = T {
    v.scala
  }

  def circtJextractModule = `circt-jextract`

  def chisel3Module = Some(mychisel3)

  override def scalacPluginIvyDeps = Agg(
    ivy"edu.berkeley.cs:::chisel3-plugin:3.6.0"
  )

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
