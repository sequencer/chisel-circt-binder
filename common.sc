import mill._
import mill.scalalib._
import mill.scalalib.publish._

import $ivy.`de.tototec::de.tobiasroeser.mill.vcs.version::0.1.4`
import de.tobiasroeser.mill.vcs.version.VcsVersion

trait ChiselCIRCTBinderPublishModule extends PublishModule { m =>
  def publishVersion =
    de.tobiasroeser.mill.vcs.version.VcsVersion.vcsState().format()
  def pomSettings = T {
    PomSettings(
      description = artifactName(),
      organization = "me.jiuyang",
      url = "https://github.com/sequencer/chisel-circt-binder",
      licenses = Seq(License.`Apache-2.0`),
      versionControl = VersionControl.github("seqeuncer", "chisel-circt-binder"),
      developers = Seq(
        Developer("sequencer", "Jiuyang Liu", "https://github.com/sequencer")
      )
    )
  }
}

trait ChiselCIRCTBinderModule extends ChiselCIRCTBinderPublishModule with ScalaModule {
  def circtJextractModule: CIRCTJextractModule

  def chiselModule: Option[PublishModule]

  def chiselPluginJar: T[Option[PathRef]]

  // User should not override lines below
  override def moduleDeps = Seq(circtJextractModule) ++ chiselModule

  override def scalacPluginClasspath = T {
    super.scalacPluginClasspath() ++ chiselPluginJar()
  }

  override def scalacOptions = T {
    super.scalacOptions() ++ chiselPluginJar().map(path => s"-Xplugin:${path.path}") ++ Some("-Ymacro-annotations")
  }

  override def javacOptions = T {
    super.javacOptions() ++ Seq("--enable-preview", "--release", "20")
  }
}

trait CIRCTJextractModule extends JavaModule {
  override def javacOptions: T[Seq[String]] = {
    Seq("--enable-preview", "--source", javacVersion().toString)
  }

  def includeFunctions = T {
    Seq(
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
        jextract().path.toString,
        "chisel-circt-binder/jextract-headers.h",
        "-t", "org.llvm.circt",
        // link
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
