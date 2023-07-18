// SPDX-License-Identifier: Apache-2.0

package chisel3.internal.panama

import chisel3.{Aggregate, Data => ChiselData, Element, Mem, SyncReadMem, Vec, VecLike}
import chisel3.experimental.{BaseModule, SourceInfo}
import chisel3.internal.{
  AggregateViewBinding,
  BundleLitBinding,
  ChildBinding,
  ConditionalDeclarable,
  ConstrainedBinding,
  CrossModuleBinding,
  DontCareBinding,
  ElementLitBinding,
  LitBinding,
  MemTypeBinding,
  MemoryPortBinding,
  NamedComponent,
  OpBinding,
  PortBinding,
  ReadOnlyBinding,
  RegBinding,
  SampleElementBinding,
  SecretPortBinding,
  TopBinding,
  UnconstrainedBinding,
  VecLitBinding,
  ViewBinding,
  WireBinding
}

import scala.collection.mutable
import scala.math._
import chisel3.internal.firrtl._
import chisel3.internal.HasId
import firrtl.ir.HasName
import firrtl.{ir => fir}
import chisel3.SyncReadMem

import chisel3.internal.CIRCTConverter
import chisel3.internal.panama.circt._

case class Region(
  region: MlirRegion,
  blocks: Seq[MlirBlock]) {

  def get(): MlirRegion = region
  def block(i: Int): MlirBlock = blocks(i)
}

case class Op(
  state:   MlirOperationState,
  op:      MlirOperation,
  regions: Seq[Region],
  results: Seq[MlirValue]) {

  def region(i: Int): Region = {
    regions(i)
  }
}

sealed abstract class Reference(data: ChiselData)
object Reference {
  final case class Value(ref: MlirValue, data: ChiselData) extends Reference(data)
  final case class SubField(index: Int, data: ChiselData) extends Reference(data)
  final case class SubIndex(index: Int, data: ChiselData) extends Reference(data)
}

sealed abstract class FirTypeOrChiselData
object FirTypeOrChiselData {
  final case class Type(tpe: fir.Type) extends FirTypeOrChiselData
  final case class Data(data: ChiselData) extends FirTypeOrChiselData

  implicit def apply(tpe:  fir.Type) = Type(tpe)
  implicit def apply(data: ChiselData) = Data(data)
}

case class Refered(value: MlirValue, private var tpeOrData: FirTypeOrChiselData) {
  def tpe(): fir.Type = {
    tpeOrData match {
      case FirTypeOrChiselData.Type(tpe) => tpe
      case FirTypeOrChiselData.Data(data) =>
        val tpe = Converter.extractType(data, null)
        tpeOrData = FirTypeOrChiselData.Type(tpe)
        tpe
    }
  }
}

class PanamaCIRCTConverter extends CIRCTConverter {
  val circt = new PanamaCIRCT

  val module = circt.mlirModuleCreateEmpty(circt.unkLoc)
  val moduleItems = mutable.Map.empty[Long, MlirValue]
  var circuit:   Op = null
  var firModule: Op = null

  // TODO: refactor

  case class WhenContext(ctx: Op, var isElse: Boolean)
  var whenCtx: mutable.Stack[WhenContext] = mutable.Stack.empty

  object util {
    def convert(firType: fir.Type): MlirType = {
      def convertFirWidth(width: fir.Width) = width match {
        case fir.UnknownWidth => -1
        case fir.IntWidth(v)  => v.toInt
      }

      firType match {
        case t: fir.UIntType => circt.firrtlTypeGetUInt(convertFirWidth(t.width))
        case t: fir.SIntType => circt.firrtlTypeGetSInt(convertFirWidth(t.width))
        case fir.ClockType      => circt.firrtlTypeGetClock()
        case fir.ResetType      => circt.firrtlTypeGetReset()
        case fir.AsyncResetType => circt.firrtlTypeGetAsyncReset()
        case t: fir.AnalogType => circt.firrtlTypeGetAnalog(convertFirWidth(t.width))
        case t: fir.VectorType => circt.firrtlTypeGetVector(convert(t.tpe), t.size)
        case t: fir.BundleType =>
          circt.firrtlTypeGetBundle(
            t.fields.map(field =>
              new FIRRTLBundleField(
                field.name,
                field.flip match {
                  case fir.Default => false
                  case fir.Flip    => true
                },
                convert(field.tpe)
              )
            )
          )
      }
    }

    def bitLength(n: Int): Int = {
      var bits = 0
      var num = n
      while (num != 0) {
        bits += 1
        num >>>= 1
      }
      bits
    }

    case class OpBuilder(opName: String, parent: MlirBlock, loc: MlirLocation) {
      var regionsBlocks: Seq[Seq[(Seq[MlirType], Seq[MlirLocation])]] = Seq.empty
      var attrs:         Seq[MlirNamedAttribute] = Seq.empty
      var operands:      Seq[MlirValue] = Seq.empty
      var results:       Seq[MlirType] = Seq.empty

      def withRegion(block: Seq[(Seq[MlirType], Seq[MlirLocation])]): OpBuilder = {
        regionsBlocks = regionsBlocks :+ block
        this
      }
      def withRegions(blocks: Seq[Seq[(Seq[MlirType], Seq[MlirLocation])]]): OpBuilder = {
        regionsBlocks = regionsBlocks ++ blocks
        this
      }

      def withNamedAttr(name: String, attr: MlirAttribute): OpBuilder = {
        attrs = attrs :+ circt.mlirNamedAttributeGet(name, attr)
        this
      }
      def withNamedAttrs(as: Seq[(String, MlirAttribute)]): OpBuilder = {
        as.foreach(a => withNamedAttr(a._1, a._2)); this
      }

      def withOperand(o: MlirValue): OpBuilder = { operands = operands :+ o; this }
      def withOperands(os: Seq[MlirValue]): OpBuilder = { operands = operands ++ os; this }

      def withResult(r: MlirType): OpBuilder = { results = results :+ r; this }
      def withResults(rs: Seq[MlirType]): OpBuilder = { results = results ++ rs; this }

      def build(): Op = {
        val state = circt.mlirOperationStateGet(opName, loc)

        circt.mlirOperationStateAddAttributes(state, attrs)
        circt.mlirOperationStateAddOperands(state, operands)
        circt.mlirOperationStateAddResults(state, results)

        val builtRegions = regionsBlocks.foldLeft(Seq.empty[Region]) {
          case (builtRegions, blocks) => {
            val region = circt.mlirRegionCreate()
            val builtBlocks = blocks.map {
              case (blockArgTypes, blockArgLocs) => {
                val block = circt.mlirBlockCreate(blockArgTypes, blockArgLocs)
                circt.mlirRegionAppendOwnedBlock(region, block)
                block
              }
            }
            builtRegions :+ Region(region, builtBlocks)
          }
        }
        circt.mlirOperationStateAddOwnedRegions(state, builtRegions.map(_.region))

        val op = circt.mlirOperationCreate(state)
        circt.mlirBlockAppendOwnedOperation(parent, op)

        val resultVals = results.zipWithIndex.map {
          case (_, i) => circt.mlirOperationGetResult(op, i)
        }

        Op(state, op, builtRegions, resultVals)
      }
    }

    def newConstantValue(
      resultType: fir.Type,
      valueType:  MlirType,
      value:      Int
    ): MlirValue = {
      util
        .OpBuilder("firrtl.constant", parentBlock, circt.unkLoc)
        .withNamedAttr("value", circt.mlirIntegerAttrGet(valueType, value))
        .withResult(util.convert(resultType))
        .build()
        .results(0)
    }

    def parentBlock: MlirBlock = {
      if (whenCtx.nonEmpty) {
        val w = whenCtx.top
        w.ctx.region(if (w.isElse) 1 else 0).block(0)
      } else {
        firModule.region(0).block(0)
      }
    }

    // Get reference chain for a node
    def valueReferenceChain(id: HasId): Seq[Reference] = {
      def rec(id: HasId, chain: Seq[Reference]): Seq[Reference] = {
        def referToPort(data: ChiselData, enclosure: BaseModule): Reference = {
          val index = enclosure
            .getChiselPorts(chisel3.experimental.UnlocatableSourceInfo)
            .indexWhere(_._2 == data)
          val value = circt.mlirBlockGetArgument(firModule.region(0).block(0), index)
          Reference.Value(value, data)
        }

        def referToElement(data: ChiselData): Reference = {
          data.binding.getOrElse(throw new Exception("non-child data")) match {
            case binding: ChildBinding =>
              binding.parent match {
                case vec:    Vec[_] => Reference.SubIndex(vec.elementsIterator.indexWhere(_ == data), data)
                case record: chisel3.Record =>
                  Reference.SubField(record.elements.size - record.elements.values.iterator.indexOf(data) - 1, data)
              }
            case SampleElementBinding(vec) => Reference.SubIndex(vec.elementsIterator.indexWhere(_ == data), data)
            case _                         => throw new Exception("non-child data")
          }
        }

        def referToValue(data: ChiselData) = Reference.Value(
          moduleItems.get(data._id) match {
            case Some(value) => value
            case None        => throw new Exception(s"data $data not found")
          },
          data
        )

        id match {
          case module: BaseModule => chain
          case data:   ChiselData =>
            data.binding.getOrElse(throw new Exception("unbound data")) match {
              case PortBinding(enclosure)                   => rec(enclosure, chain :+ referToPort(data, enclosure))
              case SecretPortBinding(enclosure)             => rec(enclosure, chain :+ referToPort(data, enclosure))
              case ChildBinding(parent)                     => rec(parent, chain :+ referToElement(data))
              case SampleElementBinding(parent)             => rec(parent, chain :+ referToElement(data))
              case MemoryPortBinding(enclosure, visibility) => rec(enclosure, chain :+ referToValue(data))
              case WireBinding(enclosure, visibility)       => rec(enclosure, chain :+ referToValue(data))
              case OpBinding(enclosure, visibility)         => rec(enclosure, chain :+ referToValue(data))
              case RegBinding(enclosure, visibility)        => rec(enclosure, chain :+ referToValue(data))
              case unhandled                                => throw new Exception(s"unhandled binding $unhandled")
            }
          case mem:  Mem[ChiselData]         => chain :+ referToValue(mem.t)
          case smem: SyncReadMem[ChiselData] => chain :+ referToValue(smem.t)
          case unhandled => throw new Exception(s"unhandled node $unhandled")
        }
      }
      rec(id, Seq()).reverse // Reverse to make it root first
    }

    def referTo(id: HasId): Refered = {
      val indexType = circt.mlirIntegerTypeGet(32)

      val refChain = valueReferenceChain(id)

      // Root value will be the first element in the chain
      // So the initialization value of the `foldLeft` is unnecessary
      refChain.foldLeft[Refered](null) {
        case (parent, ref: Reference) => {
          ref match {
            case Reference.Value(value, data) => Refered(value, data)
            case Reference.SubField(index, data) =>
              val tpe = Converter.extractType(data, null)
              Refered(
                util
                  .OpBuilder("firrtl.subfield", parentBlock, circt.unkLoc)
                  .withNamedAttr("fieldIndex", circt.mlirIntegerAttrGet(indexType, index))
                  .withOperand(parent.value)
                  .withResult(util.convert(tpe))
                  .build()
                  .results(0),
                tpe
              )
            case Reference.SubIndex(index, data) =>
              val tpe = Converter.extractType(data, null)
              Refered(
                util
                  .OpBuilder("firrtl.subindex", parentBlock, circt.unkLoc)
                  .withNamedAttr("index", circt.mlirIntegerAttrGet(indexType, index))
                  .withOperand(parent.value)
                  .withResult(util.convert(tpe))
                  .build()
                  .results(0),
                tpe
              )
          }
        }
      }
    }

    def referTo(arg: Arg): Refered = {
      def referToNewConstant(n: Int, w: Width, isSigned: Boolean): Refered = {
        val (firWidth, valWidth) = w match {
          case _: UnknownWidth =>
            val bitLen = util.bitLength(n)
            (fir.IntWidth(bitLen), bitLen)
          case w: KnownWidth => (fir.IntWidth(w.get), w.get)
        }
        val resultType = if (isSigned) fir.SIntType(firWidth) else fir.UIntType(firWidth)
        val valueType =
          if (isSigned) circt.mlirIntegerTypeSignedGet(valWidth) else circt.mlirIntegerTypeUnsignedGet(valWidth)
        Refered(util.newConstantValue(resultType, valueType, n.toInt), resultType)
      }

      arg match {
        case Node(id)           => referTo(id)
        case ULit(value, width) => referToNewConstant(value.toInt, width, false)
        case SLit(value, width) => referToNewConstant(value.toInt, width, true)
        case unhandled          => throw new Exception(s"unhandled arg type to be reference: $unhandled")
      }
    }

    def newNode(id: Long, name: String, resultType: fir.Type, input: MlirValue): Unit = {
      val op = util
        .OpBuilder("firrtl.node", parentBlock, circt.unkLoc)
        .withNamedAttr("name", circt.mlirStringAttrGet(name))
        .withNamedAttr("nameKind", circt.firrtlAttrGetNameKind(FIRRTLNameKind.InterestingName))
        .withNamedAttr("annotations", circt.emptyArrayAttr)
        .withOperand(input)
        .withResult(util.convert(resultType))
        // .withResult( /* ref */ )
        .build()
      moduleItems += ((id, op.results(0)))
    }
  }

  // TODO:
  def dump(): Unit = {
    circt.mlirOperationDump(circt.mlirModuleGetOperation(module))
  }

  // TODO:
  def exportFIRRTL(): Unit = {
    circt.mlirExportFIRRTL(module, message => print(message))
  }

  def visitCircuit(name: String): Unit = {
    circuit = util
      .OpBuilder("firrtl.circuit", circt.mlirModuleGetBody(module), circt.unkLoc)
      .withRegion(Seq((Seq.empty, Seq.empty)))
      .withNamedAttr("name", circt.mlirStringAttrGet(name))
      .withNamedAttr("annotations", circt.emptyArrayAttr)
      .build()
  }

  def visitDefModule(defModule: DefModule): Unit = {
    moduleItems.clear()

    val ports = defModule.ports.map(Converter.convert(_))
    val (portsTypes, portsTypeAttrs) = ports.foldLeft((Seq.empty[MlirType], Seq.empty[MlirAttribute])) {
      case ((types, attrs), port) => {
        val tpe = util.convert(port.tpe)
        (types :+ tpe, attrs :+ circt.mlirTypeAttrGet(tpe))
      }
    }
    val portsLocs = ports.map(_ => circt.unkLoc)
    val portsAnnotationsAttrs = ports.map(_ => circt.emptyArrayAttr)

    firModule = util
      .OpBuilder("firrtl.module", circuit.region(0).block(0), circt.unkLoc)
      .withRegion(Seq((portsTypes, portsLocs)))
      .withNamedAttr("sym_name", circt.mlirStringAttrGet(defModule.name))
      .withNamedAttr(
        "portDirections",
        circt.firrtlAttrGetPortDirs(
          ports.map(_.direction match {
            case fir.Input  => FIRRTLPortDir.Input
            case fir.Output => FIRRTLPortDir.Output
          })
        )
      )
      .withNamedAttr("portNames", circt.mlirArrayAttrGet(ports.map(port => circt.mlirStringAttrGet(port.name))))
      .withNamedAttr("portTypes", circt.mlirArrayAttrGet(portsTypeAttrs))
      .withNamedAttr("portAnnotations", circt.mlirArrayAttrGet(portsAnnotationsAttrs))
      .withNamedAttr("portSyms", circt.emptyArrayAttr)
      .withNamedAttr(
        "portLocations",
        // TODO: figure out the relationship between `MlirAttribute` and `MlirLocation`
        circt.mlirArrayAttrGet(portsLocs.map(loc => MlirAttribute(loc.ptr)))
      )
      .withNamedAttr("annotations", circt.emptyArrayAttr)
      .build()
  }

  def visitAltBegin(altBegin: AltBegin): Unit = {
    // TODO
    assert(false, "unimplemented")
  }

  def visitAttach(attach: Attach): Unit = {
    util
      .OpBuilder("firrtl.attach", util.parentBlock, circt.unkLoc)
      .withOperands(attach.locs.map(loc => util.referTo(loc.id).value))
      .build()
  }

  def visitConnect(connect: Connect): Unit = {
    util
      .OpBuilder("firrtl.connect", util.parentBlock, circt.unkLoc)
      .withOperand( /* dest */ util.referTo(connect.loc.id).value)
      .withOperand( /* src */ util.referTo(connect.exp).value)
      .build()
  }

  def visitDefWire(defWire: DefWire): Unit = {
    val wireName = Converter.getRef(defWire.id, defWire.sourceInfo).name
    val op = util
      .OpBuilder("firrtl.wire", util.parentBlock, circt.unkLoc)
      .withNamedAttr("name", circt.mlirStringAttrGet(wireName))
      .withNamedAttr("nameKind", circt.firrtlAttrGetNameKind(FIRRTLNameKind.InterestingName))
      .withNamedAttr("annotations", circt.emptyArrayAttr)
      .withResult(util.convert(Converter.extractType(defWire.id, defWire.sourceInfo)))
      // .withResult( /* ref */ )
      .build()
    moduleItems += ((defWire.id._id, op.results(0)))
  }

  def visitDefInvalid(defInvalid: DefInvalid): Unit = {
    val dest = util.referTo(defInvalid.arg)

    val invalidValue = util
      .OpBuilder("firrtl.invalidvalue", util.parentBlock, circt.unkLoc)
      .withResult(util.convert(dest.tpe))
      .build()
      .results(0)

    util
      .OpBuilder("firrtl.connect", util.parentBlock, circt.unkLoc)
      .withOperand( /* dest */ dest.value)
      .withOperand( /* src */ invalidValue)
      .build()
  }

  def visitOtherwiseEnd(otherwiseEnd: OtherwiseEnd): Unit = {
    // TODO:
    assert(otherwiseEnd.firrtlDepth == 1)
  }

  def visitWhenBegin(whenBegin: WhenBegin): Unit = {
    val cond = util.referTo(whenBegin.pred)

    val op = util
      .OpBuilder("firrtl.when", util.parentBlock, circt.unkLoc)
      .withRegion( /* then */ Seq((Seq.empty, Seq.empty)))
      .withRegion( /* else */ Seq((Seq.empty, Seq.empty)))
      .withOperand( /* condition */ cond.value)
      .build()

    whenCtx.push(WhenContext(op, false))
  }

  def visitWhenEnd(whenEnd: WhenEnd): Unit = {
    // TODO:
    assert(whenEnd.firrtlDepth == 0)
    assert(whenEnd.hasAlt == false)

    whenCtx.pop()
  }

  def visitDefSeqMemory(defSeqMemory: DefSeqMemory): Unit = {
    val name = Converter.getRef(defSeqMemory.id, defSeqMemory.sourceInfo).name

    val op = util
      .OpBuilder("chirrtl.seqmem", util.parentBlock, circt.unkLoc)
      .withNamedAttr(
        "ruw",
        circt.firrtlAttrGetRUW(
          defSeqMemory.readUnderWrite match {
            case fir.ReadUnderWrite.Undefined => firrtlAttrGetRUW.Undefined
            case fir.ReadUnderWrite.Old       => firrtlAttrGetRUW.Old
            case fir.ReadUnderWrite.New       => firrtlAttrGetRUW.New
          }
        )
      )
      .withNamedAttr("name", circt.mlirStringAttrGet(name))
      .withNamedAttr("nameKind", circt.firrtlAttrGetNameKind(FIRRTLNameKind.InterestingName))
      .withNamedAttr("annotations", circt.emptyArrayAttr)
      .withResult(
        circt.chirrtlTypeGetCMemory(
          util.convert(Converter.extractType(defSeqMemory.t, defSeqMemory.sourceInfo)),
          defSeqMemory.size.intValue
        )
      )
      .build()
    moduleItems += ((defSeqMemory.t._id, op.results(0)))
  }

  def visitDefMemPort[T <: ChiselData](defMemPort: DefMemPort[T]): Unit = {
    val op = util
      .OpBuilder("chirrtl.memoryport", util.parentBlock, circt.unkLoc)
      .withNamedAttr(
        "direction",
        circt.firrtlAttrGetMemDir(
          defMemPort.dir match {
            case MemPortDirection.READ  => FIRRTLMemDir.Read
            case MemPortDirection.WRITE => FIRRTLMemDir.Write
            case MemPortDirection.RDWR  => FIRRTLMemDir.ReadWrite
            case MemPortDirection.INFER => FIRRTLMemDir.Infer
          }
        )
      )
      .withNamedAttr("name", circt.mlirStringAttrGet(Converter.getRef(defMemPort.id, defMemPort.sourceInfo).name))
      .withNamedAttr("annotations", circt.emptyArrayAttr)
      .withOperand( /* memory */ util.referTo(defMemPort.source.id).value)
      .withResult( /* data */ util.convert(Converter.extractType(defMemPort.id, defMemPort.sourceInfo)))
      .withResult( /* port */ circt.chirrtlTypeGetCMemoryPort())
      .build()

    util
      .OpBuilder("chirrtl.memoryport.access", util.parentBlock, circt.unkLoc)
      .withOperand( /* port */ op.results(1))
      .withOperand( /* index */ util.referTo(defMemPort.index).value)
      .withOperand( /* clock */ util.referTo(defMemPort.clock).value)
      .build()

    moduleItems += ((defMemPort.id._id, op.results(0)))
  }

  def visitDefMemory(defMemory: DefMemory): Unit = {
    val op = util
      .OpBuilder("chirrtl.combmem", util.parentBlock, circt.unkLoc)
      .withNamedAttr("name", circt.mlirStringAttrGet(Converter.getRef(defMemory.id, defMemory.sourceInfo).name))
      .withNamedAttr("nameKind", circt.firrtlAttrGetNameKind(FIRRTLNameKind.InterestingName))
      .withNamedAttr("annotations", circt.emptyArrayAttr)
      .withResult(
        circt.chirrtlTypeGetCMemory(
          util.convert(Converter.extractType(defMemory.t, defMemory.sourceInfo)),
          defMemory.size.intValue
        )
      )
      .build()
    moduleItems += ((defMemory.t._id, op.results(0)))
  }

  def visitDefPrim[T <: ChiselData](defPrim: DefPrim[T]): Unit = {
    def arg(index: Int): Refered = {
      util.referTo(defPrim.args(index))
    }

    def litArg(index: Int): BigInt = {
      defPrim.args(index) match {
        case ILit(value)    => value
        case ULit(value, _) => value
        case unhandled      => throw new Exception(s"unhandled lit arg type to be extracted: $unhandled")
      }
    }

    val name = Converter.getRef(defPrim.id, defPrim.sourceInfo).name

    val (attrs, operands, resultType) = defPrim.op match {
      // Operands
      //   lhs: sint or uint
      //   rhs: sint or uint
      // Results
      //   result: sint or uint : <max(lhs, rhs) + 1>
      case PrimOp.AddOp | PrimOp.SubOp =>
        val (lhs, rhs) = (arg(0), arg(1))
        val retType = (lhs.tpe, rhs.tpe) match {
          case (fir.SIntType(lhsWidth), fir.SIntType(rhsWidth)) =>
            fir.SIntType(lhsWidth.max(rhsWidth) + fir.IntWidth(1))
          case (fir.UIntType(lhsWidth), fir.UIntType(rhsWidth)) =>
            fir.UIntType(lhsWidth.max(rhsWidth) + fir.IntWidth(1))
        }
        (Seq.empty, Seq(lhs, rhs), retType)

      // Attributes
      //   amount: 32-bit signless integer
      // Operands
      //   input: sint or uint
      // Results
      //   result: uint : <input - amount>
      case PrimOp.TailOp =>
        val (input, amount) = (arg(0), litArg(1).toInt)
        val width = input.tpe match {
          case fir.SIntType(fir.IntWidth(inputWidth)) => inputWidth - amount
          case fir.UIntType(fir.IntWidth(inputWidth)) => inputWidth - amount
        }
        val attrs = Seq(
          ("amount", circt.mlirIntegerAttrGet(circt.mlirIntegerTypeGet(32), amount))
        )
        (attrs, Seq(input), fir.UIntType(fir.IntWidth(width)))

      // Attributes
      //   amount: 32-bit signless integer
      // Operands
      //   input: sint or uint
      // Results
      //   result: uint : <amount>
      case PrimOp.HeadOp =>
        val (input, amount) = (arg(0), litArg(1).toInt)
        val width = input.tpe match {
          case fir.SIntType(_) => amount
          case fir.UIntType(_) => amount
        }
        val attrs = Seq(
          ("amount", circt.mlirIntegerAttrGet(circt.mlirIntegerTypeGet(32), amount))
        )
        (attrs, Seq(input), fir.UIntType(fir.IntWidth(width)))

      // Operands
      //   lhs: sint or uint
      //   rhs: sint or uint
      // Results
      //   result: sint or uint : <lhs + rhs>
      case PrimOp.TimesOp =>
        val (lhs, rhs) = (arg(0), arg(1))
        val retType = (lhs.tpe, rhs.tpe) match {
          case (fir.SIntType(lhsWidth), fir.SIntType(rhsWidth)) => fir.SIntType(lhsWidth + rhsWidth)
          case (fir.UIntType(lhsWidth), fir.UIntType(rhsWidth)) => fir.UIntType(lhsWidth + rhsWidth)
        }
        (Seq.empty, Seq(lhs, rhs), retType)

      // Operands
      //   lhs: sint or uint
      //   rhs: sint or uint
      // Results
      //   result: sint or uint : <if {uint} then {lhs} else {lhs + 1}>
      case PrimOp.DivideOp =>
        val (lhs, rhs) = (arg(0), arg(1))
        val retType = (lhs.tpe, rhs.tpe) match {
          case (fir.SIntType(lhsWidth), fir.SIntType(rhsWidth)) => fir.SIntType(lhsWidth + fir.IntWidth(1))
          case (fir.UIntType(lhsWidth), fir.UIntType(rhsWidth)) => fir.UIntType(lhsWidth)
        }
        (Seq.empty, Seq(lhs, rhs), retType)

      // Operands
      //   lhs: sint or uint
      //   rhs: sint or uint
      // Results
      //   result: sint or uint : <min(lhs, rhs)>
      case PrimOp.RemOp =>
        val (lhs, rhs) = (arg(0), arg(1))
        val retType = (lhs.tpe, rhs.tpe) match {
          case (fir.SIntType(lhsWidth), fir.SIntType(rhsWidth)) => fir.SIntType(lhsWidth.min(rhsWidth))
          case (fir.UIntType(lhsWidth), fir.UIntType(rhsWidth)) => fir.UIntType(lhsWidth.min(rhsWidth))
        }
        (Seq.empty, Seq(lhs, rhs), retType)

      // Attributes
      //   amount: 32-bit signless integer
      // Operands
      //   input: sint or uint
      // Results
      //   result: sint or uint : <input + amount>
      case PrimOp.ShiftLeftOp =>
        val (input, amount) = (arg(0), litArg(1).toInt)
        val (width, retTypeFn) = input.tpe match {
          case fir.SIntType(fir.IntWidth(inputWidth)) => (inputWidth + amount, fir.SIntType)
          case fir.UIntType(fir.IntWidth(inputWidth)) => (inputWidth + amount, fir.UIntType)
        }
        val attrs = Seq(
          ("amount", circt.mlirIntegerAttrGet(circt.mlirIntegerTypeGet(32), amount))
        )
        (attrs, Seq(input), retTypeFn(fir.IntWidth(width)))

      // Attributes
      //   amount: 32-bit signless integer
      // Operands
      //   input: sint or uint
      // Results
      //   result: sint or uint : <max(input - amount, 1)>
      case PrimOp.ShiftRightOp =>
        val (input, amount) = (arg(0), litArg(1).toInt)
        val (width, retTypeFn) = input.tpe match {
          case fir.SIntType(fir.IntWidth(inputWidth)) => (max((inputWidth - amount).toInt, 1), fir.SIntType)
          case fir.UIntType(fir.IntWidth(inputWidth)) => (max((inputWidth - amount).toInt, 1), fir.UIntType)
        }
        val attrs = Seq(
          ("amount", circt.mlirIntegerAttrGet(circt.mlirIntegerTypeGet(32), amount))
        )
        (attrs, Seq(input), retTypeFn(fir.IntWidth(width)))

      // Operands
      //   lhs: sint or uint
      //   rhs: uint
      // Results
      //   result: sint or uint : <lhs + 2^rhs - 1>
      case PrimOp.DynamicShiftLeftOp =>
        val (lhs, rhs) = (arg(0), arg(1))
        val retType = (lhs.tpe, rhs.tpe) match {
          case (fir.SIntType(fir.IntWidth(lhsWidth)), fir.UIntType(fir.IntWidth(rhsWidth))) =>
            fir.SIntType(fir.IntWidth(lhsWidth.toInt + (1 << rhsWidth.toInt) - 1))
          case (fir.UIntType(fir.IntWidth(lhsWidth)), fir.UIntType(fir.IntWidth(rhsWidth))) =>
            fir.UIntType(fir.IntWidth(lhsWidth.toInt + (1 << rhsWidth.toInt) - 1))
        }
        (Seq.empty, Seq(lhs, rhs), retType)

      // Operands
      //   lhs: sint or uint
      //   rhs: uint
      // Results
      //   result: sint or uint : <lhs>
      case PrimOp.DynamicShiftRightOp =>
        val (lhs, rhs) = (arg(0), arg(1))
        val retType = (lhs.tpe, rhs.tpe) match {
          case (fir.SIntType(lhsWidth), fir.UIntType(rhsWidth)) => fir.SIntType(lhsWidth)
          case (fir.UIntType(lhsWidth), fir.UIntType(rhsWidth)) => fir.UIntType(lhsWidth)
        }
        (Seq.empty, Seq(lhs, rhs), retType)

      // Operands
      //   lhs: sint or uint
      //   rhs: sint or uint
      // Results
      //   result: uint : <max(lhs, rhs)>
      case PrimOp.BitAndOp | PrimOp.BitOrOp | PrimOp.BitXorOp =>
        val (lhs, rhs) = (arg(0), arg(1))
        val width = (lhs.tpe, rhs.tpe) match {
          case (fir.SIntType(lhsWidth), fir.SIntType(rhsWidth)) => lhsWidth.max(rhsWidth)
          case (fir.UIntType(lhsWidth), fir.UIntType(rhsWidth)) => lhsWidth.max(rhsWidth)
        }
        (Seq.empty, Seq(lhs, rhs), fir.UIntType(width))

      // Operands
      //   input: sint or uint
      // Results
      //   result: uint : <input>
      case PrimOp.BitNotOp =>
        val input = arg(0)
        val width = input.tpe match {
          case fir.SIntType(width) => width
          case fir.UIntType(width) => width
        }
        (Seq.empty, Seq(input), fir.UIntType(width))

      // Operands
      //   lhs: sint or uint
      //   rhs: sint or uint
      // Results
      //   result: uint : <lhs + rhs>
      case PrimOp.ConcatOp =>
        val (lhs, rhs) = (arg(0), arg(1))
        val width = (lhs.tpe, rhs.tpe) match {
          case (fir.SIntType(lhsWidth), fir.SIntType(rhsWidth)) => lhsWidth + rhsWidth
          case (fir.UIntType(lhsWidth), fir.UIntType(rhsWidth)) => lhsWidth + rhsWidth
        }
        (Seq.empty, Seq(lhs, rhs), fir.UIntType(width))

      // Attributes
      //   hi: 32-bit signless integer
      //   lo: 32-bit signless integer
      // Operands
      //   input: sint or uint
      // Results
      //   result: uint : <hi - lo + 1>
      case PrimOp.BitsExtractOp =>
        val (input, hi, lo) =
          (arg(0), litArg(1).toInt, litArg(2).toInt)
        val width = hi - lo + 1
        val intType = circt.mlirIntegerTypeGet(32)
        val attrs = Seq(
          ("hi", circt.mlirIntegerAttrGet(intType, hi)),
          (
            "lo",
            circt.mlirIntegerAttrGet(intType, lo)
          )
        )
        (attrs, Seq(input), fir.UIntType(fir.IntWidth(width)))

      // Operands
      //   lhs: sint or uint
      //   rhs: sint or uint
      // Results
      //   result: 1-bit uint
      case PrimOp.LessOp | PrimOp.LessEqOp | PrimOp.GreaterOp | PrimOp.GreaterEqOp | PrimOp.EqualOp |
          PrimOp.NotEqualOp =>
        val (lhs, rhs) = (arg(0), arg(1))
        (Seq.empty, Seq(lhs, rhs), fir.UIntType(fir.IntWidth(1)))

      // Attributes
      //   amount: 32-bit signless integer
      // Operands
      //   input: sint or uint
      // Results
      //   result: sint or uint : <max(input, amount)>
      case PrimOp.PadOp =>
        val (input, amount) = (arg(0), litArg(1).toInt)
        val (width, retTypeFn) = input.tpe match {
          case fir.SIntType(fir.IntWidth(inputWidth)) => (max(inputWidth.toInt, amount), fir.SIntType)
          case fir.UIntType(fir.IntWidth(inputWidth)) => (max(inputWidth.toInt, amount), fir.UIntType)
        }
        val attrs = Seq(
          (
            "amount",
            circt.mlirIntegerAttrGet(circt.mlirIntegerTypeGet(32), amount)
          )
        )
        (attrs, Seq(input), retTypeFn(fir.IntWidth(width)))

      // Operands
      //   input: sint or uint
      // Results
      //   result: sint : <input + 1>
      case PrimOp.NegOp =>
        val input = arg(0)
        val width = input.tpe match {
          case fir.SIntType(inputWidth) => inputWidth + fir.IntWidth(1)
          case fir.UIntType(inputWidth) => inputWidth + fir.IntWidth(1)
        }
        (Seq.empty, Seq(input), fir.SIntType(width))

      // Operands
      //   sel: 1-bit uint or uint with uninferred width
      //   high: a passive base type (contain no flips)
      //   low: a passive base type (contain no flips)
      // Results
      //   result: a passive base type (contain no flips)
      case PrimOp.MultiplexOp =>
        val (sel, high, low) = (arg(0), arg(1), arg(2))
        assert(high.tpe == low.tpe)
        (Seq.empty, Seq(sel, high, low), high.tpe)

      // Operands
      //   input: sint or uint
      // Results
      //   result: 1-bit uint
      case PrimOp.AndReduceOp | PrimOp.OrReduceOp | PrimOp.XorReduceOp =>
        val input = arg(0)
        (Seq.empty, Seq(input), fir.UIntType(fir.IntWidth(1)))

      // Operands
      //   input: sint or uint
      // Results
      //   result: sint <if {uint} then {input + 1} else {input}>
      case PrimOp.ConvertOp =>
        val input = arg(0)
        val width = input.tpe match {
          case fir.SIntType(inputWidth) => inputWidth
          case fir.UIntType(inputWidth) => inputWidth + fir.IntWidth(1)
        }
        (Seq.empty, Seq(input), fir.SIntType(width))

      // Operands
      //   input: base type
      // Results
      //   result: uint(AsUInt) sint(AsSInt) : <if {sint or uint} then {input} else {1}>
      case PrimOp.AsUIntOp | PrimOp.AsSIntOp =>
        val input = arg(0)
        val width = input.tpe match {
          case fir.SIntType(inputWidth)                           => inputWidth
          case fir.UIntType(inputWidth)                           => inputWidth
          case fir.ClockType | fir.ResetType | fir.AsyncResetType => fir.IntWidth(1)
        }
        val retTypeFn = defPrim.op match {
          case PrimOp.AsUIntOp => fir.UIntType
          case PrimOp.AsSIntOp => fir.SIntType
        }
        (Seq.empty, Seq(input), retTypeFn(width))

      case PrimOp.AsFixedPointOp | PrimOp.AsIntervalOp | PrimOp.WrapOp | PrimOp.SqueezeOp | PrimOp.ClipOp |
          PrimOp.SetBinaryPoint | PrimOp.IncreasePrecision | PrimOp.DecreasePrecision =>
        throw new Exception(s"deprecated primitive op: $defPrim")

      // Operands
      //   input: 1-bit uint/sint/analog, reset, asyncreset, or clock
      // Results
      //   result: clock
      case PrimOp.AsClockOp =>
        val input = arg(0)
        (Seq.empty, Seq(input), fir.ClockType)

      // Operands
      //   input: 1-bit uint/sint/analog, reset, asyncreset, or clock
      // Results
      //   result: clock
      case PrimOp.AsAsyncResetOp =>
        val input = arg(0)
        (Seq.empty, Seq(input), fir.ClockType)

      case _ => throw new Exception(s"defPrim: $defPrim")
    }

    val op = util
      .OpBuilder(s"firrtl.${defPrim.op.toString}", util.parentBlock, circt.unkLoc)
      .withNamedAttrs(attrs)
      .withOperands(operands.map(_.value))
      .withResult(util.convert(resultType))
      .build()
    util.newNode(defPrim.id._id, name, resultType, op.results(0))
  }

  def visitDefReg(defReg: DefReg): Unit = {
    val op = util
      .OpBuilder("firrtl.reg", util.parentBlock, circt.unkLoc)
      .withNamedAttr("name", circt.mlirStringAttrGet(Converter.getRef(defReg.id, defReg.sourceInfo).name))
      .withNamedAttr("nameKind", circt.firrtlAttrGetNameKind(FIRRTLNameKind.InterestingName))
      .withNamedAttr("annotations", circt.emptyArrayAttr)
      .withOperand( /* clockVal */ util.referTo(defReg.clock).value)
      .withResult( /* result */ util.convert(Converter.extractType(defReg.id, defReg.sourceInfo)))
      .build()
    moduleItems += ((defReg.id._id, op.results(0)))
  }

  def visitDefRegInit(defRegInit: DefRegInit): Unit = {
    val op = util
      .OpBuilder("firrtl.regreset", util.parentBlock, circt.unkLoc)
      .withNamedAttr("name", circt.mlirStringAttrGet(Converter.getRef(defRegInit.id, defRegInit.sourceInfo).name))
      .withNamedAttr("nameKind", circt.firrtlAttrGetNameKind(FIRRTLNameKind.InterestingName))
      .withNamedAttr("annotations", circt.emptyArrayAttr)
      .withOperand( /* clockVal */ util.referTo(defRegInit.clock).value)
      .withOperand( /* reset */ util.referTo(defRegInit.reset).value)
      .withOperand( /* init */ util.referTo(defRegInit.init).value)
      .withResult( /* result */ util.convert(Converter.extractType(defRegInit.id, defRegInit.sourceInfo)))
      .build()
    moduleItems += ((defRegInit.id._id, op.results(0)))
  }

  def visitPrintf(parent: Component, printf: Printf): Unit = {
    val (fmt, args) = Converter.unpack(printf.pable, parent)
    util
      .OpBuilder("firrtl.printf", util.parentBlock, circt.unkLoc)
      .withNamedAttr("formatString", circt.mlirStringAttrGet(fmt))
      .withNamedAttr("name", circt.mlirStringAttrGet(Converter.getRef(printf.id, printf.sourceInfo).name))
      .withOperand( /* clock */ util.referTo(printf.clock).value)
      .withOperand(
        /* cond */ util.newConstantValue(
          fir.UIntType(fir.IntWidth(1)),
          circt.mlirIntegerTypeUnsignedGet(1),
          1
        )
      )
      .withOperands( /* substitutions */ args.map(util.referTo(_).value))
      .build()
  }

  def visitStop(stop: Stop): Unit = {
    util
      .OpBuilder("firrtl.stop", util.parentBlock, circt.unkLoc)
      .withNamedAttr("exitCode", circt.mlirIntegerAttrGet(circt.mlirIntegerTypeGet(32), stop.ret))
      .withNamedAttr("name", circt.mlirStringAttrGet(Converter.getRef(stop.id, stop.sourceInfo).name))
      .withOperand( /* clock */ util.referTo(stop.clock).value)
      .withOperand(
        /* cond */ util.newConstantValue(
          fir.UIntType(fir.IntWidth(1)),
          circt.mlirIntegerTypeUnsignedGet(1),
          1
        )
      )
      .build()
  }

  def visitVerification[T <: chisel3.VerificationStatement](
    verifi: Verification[T],
    opName: String,
    args:   Seq[Arg]
  ): Unit = {
    util
      .OpBuilder(opName, util.parentBlock, circt.unkLoc)
      .withNamedAttr("message", circt.mlirStringAttrGet(verifi.message))
      .withNamedAttr("name", circt.mlirStringAttrGet(Converter.getRef(verifi.id, verifi.sourceInfo).name))
      .withOperand( /* clock */ util.referTo(verifi.clock).value)
      .withOperand( /* predicate */ util.referTo(verifi.predicate).value)
      .withOperand(
        /* enable */ util.newConstantValue(
          fir.UIntType(fir.IntWidth(1)),
          circt.mlirIntegerTypeUnsignedGet(1),
          1
        )
      )
      .withOperands( /* substitutions */ args.map(util.referTo(_).value))
      .build()
  }

  def visitAssert(assert: Verification[chisel3.assert.Assert]): Unit = {
    visitVerification(assert, "firrtl.assert", Seq.empty)
  }

  def visitAssume(assume: Verification[chisel3.assume.Assume]): Unit = {
    // TODO: CIRCT emits `assert` for this, is it expected?
    visitVerification(assume, "firrtl.assume", Seq.empty)
  }

  def visitCover(cover: Verification[chisel3.cover.Cover]): Unit = {
    // TODO: CIRCT emits `assert` for this, is it expected?
    visitVerification(cover, "firrtl.cover", Seq.empty)
  }
}

private[chisel3] object PanamaCIRCTConverter {
  def convert(circuit: Circuit): CIRCTConverter = {
    implicit val cvt = new PanamaCIRCTConverter
    visitCircuit(circuit)
    // TODO:
    cvt.dump() // debug
    cvt.exportFIRRTL() // debug
    cvt
  }

  def visitCircuit(circuit: Circuit)(implicit cvt: CIRCTConverter): Unit = {
    cvt.visitCircuit(circuit.name)
    circuit.components.foreach {
      case defBlackBox: DefBlackBox =>
        visitDefBlackBox(defBlackBox)
      case defModule: DefModule =>
        visitDefModule(defModule)
    }
  }
  def visitDefBlackBox(defBlackBox: DefBlackBox)(implicit cvt: CIRCTConverter): Unit = {
    // TODO
    throw new Exception("TODO")
  }
  def visitDefModule(
    defModule: DefModule
  )(
    implicit cvt: CIRCTConverter
  ): Unit = {
    cvt.visitDefModule(defModule)
    defModule.commands.foreach {
      // Command
      case altBegin: AltBegin =>
        visitAltBegin(altBegin)
      case attach: Attach =>
        visitAttach(attach)
      case connect: Connect =>
        visitConnect(connect)
      case connectInit: ConnectInit =>
        visitConnectInit(connectInit)
      case defInvalid: DefInvalid =>
        visitDefInvalid(defInvalid)
      case otherwiseEnd: OtherwiseEnd =>
        visitOtherwiseEnd(otherwiseEnd)
      case whenBegin: WhenBegin =>
        visitWhenBegin(whenBegin)
      case whenEnd: WhenEnd =>
        visitWhenEnd(whenEnd)

      // Definition
      case defInstance: DefInstance =>
        visitDefInstance(defInstance)
      case defMemPort: DefMemPort[ChiselData] =>
        visitDefMemPort(defMemPort)
      case defMemory: DefMemory =>
        visitDefMemory(defMemory)
      case defPrim: DefPrim[ChiselData] =>
        visitDefPrim(defPrim)
      case defReg: DefReg =>
        visitDefReg(defReg)
      case defRegInit: DefRegInit =>
        visitDefRegInit(defRegInit)
      case defSeqMemory: DefSeqMemory =>
        visitDefSeqMemory(defSeqMemory)
      case defWire: DefWire =>
        visitDefWire(defWire)
      case printf: Printf =>
        visitPrintf(defModule, printf)
      case stop: Stop =>
        visitStop(stop)
      case assert: Verification[chisel3.assert.Assert] =>
        visitVerfiAssert(assert)
      case assume: Verification[chisel3.assume.Assume] =>
        visitVerfiAssume(assume)
      case cover: Verification[chisel3.cover.Cover] =>
        visitVerfiCover(cover)
      case printf: Verification[chisel3.printf.Printf] =>
        visitVerfiPrintf(printf)
      case stop: Verification[chisel3.stop.Stop] =>
        visitVerfiStop(stop)
      case unhandled =>
        throw new Exception(s"unhandled op: $unhandled")
    }
  }
  def visitAltBegin(altBegin: AltBegin)(implicit cvt: CIRCTConverter): Unit = {
    cvt.visitAltBegin(altBegin)
  }
  def visitAttach(attach: Attach)(implicit cvt: CIRCTConverter): Unit = {
    cvt.visitAttach(attach)
  }
  def visitConnect(connect: Connect)(implicit cvt: CIRCTConverter): Unit = {
    cvt.visitConnect(connect)
  }
  def visitConnectInit(connectInit: ConnectInit)(implicit cvt: CIRCTConverter): Unit = {
    // Not used anywhere
    throw new Exception("unimplemented")
  }
  def visitDefInvalid(defInvalid: DefInvalid)(implicit cvt: CIRCTConverter): Unit = {
    cvt.visitDefInvalid(defInvalid)
  }
  def visitOtherwiseEnd(otherwiseEnd: OtherwiseEnd)(implicit cvt: CIRCTConverter): Unit = {
    cvt.visitOtherwiseEnd(otherwiseEnd)
  }
  def visitWhenBegin(whenBegin: WhenBegin)(implicit cvt: CIRCTConverter): Unit = {
    cvt.visitWhenBegin(whenBegin)
  }
  def visitWhenEnd(whenEnd: WhenEnd)(implicit cvt: CIRCTConverter): Unit = {
    cvt.visitWhenEnd(whenEnd)
  }
  def visitDefInstance(defInstance: DefInstance)(implicit cvt: CIRCTConverter): Unit = {
    // TODO
    throw new Exception("TODO")
  }
  def visitDefMemPort[T <: ChiselData](defMemPort: DefMemPort[T])(implicit cvt: CIRCTConverter): Unit = {
    cvt.visitDefMemPort(defMemPort)
  }
  def visitDefMemory(defMemory: DefMemory)(implicit cvt: CIRCTConverter): Unit = {
    cvt.visitDefMemory(defMemory)
  }
  def visitDefPrim[T <: ChiselData](defPrim: DefPrim[T])(implicit cvt: CIRCTConverter): Unit = {
    cvt.visitDefPrim(defPrim)
  }
  def visitDefReg(defReg: DefReg)(implicit cvt: CIRCTConverter): Unit = {
    cvt.visitDefReg(defReg)
  }
  def visitDefRegInit(defRegInit: DefRegInit)(implicit cvt: CIRCTConverter): Unit = {
    cvt.visitDefRegInit(defRegInit)
  }
  def visitDefSeqMemory(defSeqMemory: DefSeqMemory)(implicit cvt: CIRCTConverter): Unit = {
    cvt.visitDefSeqMemory(defSeqMemory)
  }
  def visitDefWire(defWire: DefWire)(implicit cvt: CIRCTConverter): Unit = {
    cvt.visitDefWire(defWire)
  }
  def visitPrintf(parent: Component, printf: Printf)(implicit cvt: CIRCTConverter): Unit = {
    cvt.visitPrintf(parent, printf)
  }
  def visitStop(stop: Stop)(implicit cvt: CIRCTConverter): Unit = {
    cvt.visitStop(stop)
  }
  def visitVerfiAssert(assert: Verification[chisel3.assert.Assert])(implicit cvt: CIRCTConverter): Unit = {
    cvt.visitAssert(assert)
  }
  def visitVerfiAssume(assume: Verification[chisel3.assume.Assume])(implicit cvt: CIRCTConverter): Unit = {
    cvt.visitAssume(assume)
  }
  def visitVerfiCover(cover: Verification[chisel3.cover.Cover])(implicit cvt: CIRCTConverter): Unit = {
    cvt.visitCover(cover)
  }
  def visitVerfiPrintf(printf: Verification[chisel3.printf.Printf])(implicit cvt: CIRCTConverter): Unit = {
    // TODO: Not used anywhere?
    throw new Exception("unimplemented")
  }
  def visitVerfiStop(stop: Verification[chisel3.stop.Stop])(implicit cvt: CIRCTConverter): Unit = {
    // TODO: Not used anywhere?
    throw new Exception("unimplemented")
  }
}
