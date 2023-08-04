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

case class circtJextract(includePath: os.Path, jextractPath: os.Path)
    extends common.ChiselCIRCTBinderPublishModule
    with JavaModule {
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

  // Generate all possible bindings
  def dumpAllIncludes = T {
    val f = os.temp()
    // @formatter:off
    os.proc(
      jextractPath,
      includePath / "circt-c" / "Dialect" / "FIRRTL.h",
      "-I", includePath,
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
      "mlirLocationGetAttribute",
      "mlirLocationUnknownGet",
      "mlirLocationFileLineColGet",
      "mlirModuleCreateEmpty",
      "mlirModuleDestroy",
      "mlirModuleGetBody",
      "mlirModuleGetOperation",
      "mlirOperationStateGet",
      "mlirNamedAttributeGet",
      "mlirIntegerAttrGet",
      "mlirFloatAttrDoubleGet",
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
      "mlirF64TypeGet",
      "mlirNoneTypeGet",
      ////////////////////
      "mlirIdentifierGet",
      "mlirFlatSymbolRefAttrGet",
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
      "firrtlTypeGetUInt",
      "firrtlTypeGetSInt",
      "firrtlTypeGetClock",
      "firrtlTypeGetReset",
      "firrtlTypeGetAsyncReset",
      "firrtlTypeGetAnalog",
      "firrtlTypeGetVector",
      "firrtlTypeGetBundle",
      //
      // FIRRTL Attribute
      "firrtlAttrGetPortDirs",
      "firrtlAttrGetParamDecl",
      "firrtlAttrGetNameKind",
      "firrtlAttrGetRUW",
      "firrtlAttrGetMemoryInit",
      "firrtlAttrGetMemDir",
      //
      // CHIRRTL Attribute
      "chirrtlTypeGetCMemory",
      "chirrtlTypeGetCMemoryPort"
    )
  }

  def includeConstants = T {
    Seq(
      // enum FIRRTLPortDirection
      "FIRRTL_PORT_DIR_INPUT",
      "FIRRTL_PORT_DIR_OUTPUT",
      // enum FIRRTLNameKind
      "FIRRTL_NAME_KIND_DROPPABLE_NAME",
      "FIRRTL_NAME_KIND_INTERESTING_NAME",
      // enum FIRRTLRUW
      "FIRRTL_RUW_UNDEFINED",
      "FIRRTL_RUW_OLD",
      "FIRRTL_RUW_NEW",
      // enum FIRRTLMemDir
      "FIRRTL_MEM_DIR_INFER",
      "FIRRTL_MEM_DIR_READ",
      "FIRRTL_MEM_DIR_WRITE",
      "FIRRTL_MEM_DIR_READ_WRITE"
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
      "MlirIdentifier",
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
    // @formatter:off
    os.proc(
      Seq(
        jextractPath.toString,
        "chisel-circt-binder/jextract-headers.h",
        "-I", includePath.toString,
        "-t", "org.llvm.circt",
        "-l", "MLIRCAPIIR",
        "-l", "CIRCTCAPIFIRRTL",
        "-l", "CIRCTCAPICHIRRTL",
        "-l", "CIRCTCAPIHW",
        "-l", "CIRCTCAPIExportFIRRTL",
        "-l", "CIRCTCAPIExportVerilog",
        "-l", "CIRCTFIRRTL",
        "-l", "CIRCTHW",
        "-l", "CIRCTExportFIRRTL",
        "-l", "CIRCTExportVerilog",
        "-l", "MLIRCAPIRegisterEverything",
        "--header-class-name", "CAPI",
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

trait CIRCTPanamaModule extends common.ChiselCIRCTBinderPublishModule with JavaModule {
  def includePath:  os.Path
  def libraryPath:  os.Path
  def jextractPath: os.Path

  val jextract = circtJextract(includePath, jextractPath)
}

object circtPanama extends CIRCTPanamaModule {
  def includePath = os.Path("/usr/local/include")
  def libraryPath = os.Path("/usr/local/lib")
  def jextractPath = os.Path("/home/asuna/dev/jextract-20/bin/jextract")
}

object `chisel-circt-binder` extends common.ChiselCIRCTBinderModule with ScalaModule with ScalafmtModule {
  m =>
  def scalaVersion = T {
    v.scala
  }

  def circtPanamaModule = circtPanama

  def chisel3Module = Some(mychisel3)

  override def scalacPluginIvyDeps = Agg(
    ivy"edu.berkeley.cs:::chisel3-plugin:3.6.0"
  )

  object tests extends Tests with TestModule.Utest {
    override def forkArgs: T[Seq[String]] = {
      Seq(
        "--enable-native-access=ALL-UNNAMED",
        "--enable-preview",
        s"-Djava.library.path=${circtPanama.libraryPath}"
      )
    }

    def ivyDeps = Agg(ivy"com.lihaoyi::utest:0.8.1")
  }
}
