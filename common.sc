import mill._
import mill.scalalib._
import mill.scalalib.publish._

trait HasJextractGeneratedSources extends JavaModule {
  def includePaths: T[Seq[PathRef]]
  def libraryPaths: T[Seq[PathRef]]

  def header:           T[PathRef]
  def includeFunctions: T[Seq[String]]
  def includeConstants: T[Seq[String]]
  def includeStructs:   T[Seq[String]]
  def includeTypedefs:  T[Seq[String]]
  def includeUnions:    T[Seq[String]]
  def includeVars:      T[Seq[String]]
  def linkLibraries:    T[Seq[String]]
  def target:           T[String]
  def headerClassName:  T[String]

  def dumpAllIncludes = T {
    val f = os.temp()
    os.proc(
      Seq("jextract", header().path.toString)
        ++ includePaths().flatMap(p => Seq("-I", p.path.toString))
        ++ Seq("--dump-includes", f.toString)
    ).call()
    os.read.lines(f).filter(s => s.nonEmpty && !s.startsWith("#"))
  }

  override def generatedSources: T[Seq[PathRef]] = T {
    super.generatedSources() ++ {
      // @formatter:off
      os.proc(
        Seq("jextract", header().path.toString)
          ++ includePaths().flatMap(p => Seq("-I", p.path.toString))
          ++ Seq(
            "-t", target(),
            "--header-class-name", headerClassName(),
            "--source",
            "--output", T.dest.toString
          ) ++ includeFunctions().flatMap(f => Seq("--include-function", f)) ++
          includeConstants().flatMap(f => Seq("--include-constant", f)) ++
          includeStructs().flatMap(f => Seq("--include-struct", f)) ++
          includeTypedefs().flatMap(f => Seq("--include-typedef", f)) ++
          includeUnions().flatMap(f => Seq("--include-union", f)) ++
          includeVars().flatMap(f => Seq("--include-var", f)) ++
          linkLibraries().flatMap(l => Seq("-l", l))
      ).call(T.dest)
      // @formatter:on
      Lib
        .findSourceFiles(os.walk(T.dest).map(PathRef(_)), Seq("java"))
        .distinct
        .map(PathRef(_))
    }
  }

  override def javacOptions = T(super.javacOptions() ++ Seq("--enable-preview", "--release", "20"))
}

trait HasChisel extends ScalaModule {
  // Define these for building chisel from source
  def chiselModule: Option[ScalaModule]

  override def moduleDeps = super.moduleDeps ++ chiselModule

  def chiselPluginJar: T[Option[PathRef]]

  override def scalacOptions = T(super.scalacOptions() ++ chiselPluginJar().map(path => s"-Xplugin:${path.path}"))

  override def scalacPluginClasspath: T[Agg[PathRef]] = T(super.scalacPluginClasspath() ++ chiselPluginJar())

  // Define these for building chisel from ivy
  def chiselIvy: Option[Dep]

  override def ivyDeps = T(super.ivyDeps() ++ chiselIvy)

  def chiselPluginIvy: Option[Dep]

  override def scalacPluginIvyDeps: T[Agg[Dep]] = T(
    super.scalacPluginIvyDeps() ++ chiselPluginIvy.map(Agg(_)).getOrElse(Agg.empty[Dep])
  )
}

trait ChiselCIRCTPanamaBinderModule extends ScalaModule with HasJextractGeneratedSources with HasChisel {

  def includeFunctions = T(
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
  )

  def includeConstants = T(
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
  )

  def includeStructs = T(
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
  )

  def includeTypedefs = T(
    Seq(
      "MlirStringCallback"
    )
  )

  def includeUnions = T(Seq.empty[String])

  def includeVars = T(Seq.empty[String])

  def linkLibraries = T(
    Seq(
      "MLIRCAPIIR",
      "CIRCTCAPIFIRRTL",
      "CIRCTCAPICHIRRTL",
      "CIRCTCAPIHW",
      "CIRCTCAPIExportFIRRTL",
      "CIRCTCAPIExportVerilog",
      "CIRCTFIRRTL",
      "CIRCTHW",
      "CIRCTExportFIRRTL",
      "CIRCTExportVerilog",
      "MLIRCAPIRegisterEverything"
    )
  )

  def target:          T[String] = T("org.llvm.circt")
  def headerClassName: T[String] = T("CAPI")
  def header = T(PathRef(millSourcePath / "jextract-headers.h"))
}

trait HasChiselCIRCTPanamaBinderModule extends ScalaModule {
  def chiselCIRCTPanamaBinderModule: ChiselCIRCTPanamaBinderModule
  override def moduleDeps = super.moduleDeps ++ Some(chiselCIRCTPanamaBinderModule)
  override def javacOptions = T(super.javacOptions() ++ Seq("--enable-preview", "--release", "20"))
  override def forkArgs: T[Seq[String]] = T(
    super.forkArgs() ++ Seq("--enable-native-access=ALL-UNNAMED", "--enable-preview")
      ++ chiselCIRCTPanamaBinderModule
        .libraryPaths()
        .map(p => s"-Djava.library.path=${p.path}")
  )
}
