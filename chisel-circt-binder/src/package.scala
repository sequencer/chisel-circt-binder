// SPDX-License-Identifier: Apache-2.0

package chisel3.internal.panama

import chisel3.{Aggregate, Data, Element, Mem, SyncReadMem, Vec, VecLike}
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

import scala.collection.mutable.{ArrayBuffer, Stack}
import scala.math._
import chisel3.internal.firrtl._
import firrtl.ir.HasName
import firrtl.{ir => fir}
import chisel3.SyncReadMem

import chisel3.internal.CIRCTConverter
import chisel3.internal.panama.circt._

// TODO: Remove these imports

import java.lang.foreign._
import java.lang.foreign.MemorySegment.NULL
import java.lang.foreign.ValueLayout._
import scala.collection.mutable

class PanamaCIRCTConverter extends CIRCTConverter {
  val circt = new PanamaCIRCT

  val module = circt.mlirModuleCreateEmpty(circt.unkLoc)
  val moduleBody = circt.mlirModuleGetBody(module)

  // TODO: refactor

  abstract class Reference {}
  case class RefValue(ref: MlirValue, firData: Data) extends Reference
  case class SubField(index: Int, firData: Data) extends Reference
  case class SubIndex(index: Int, firData: Data) extends Reference

  case class Region(
    region: MlirRegion,
    blocks: Seq[MlirBlock]) {

    def get(): MlirRegion = region
    def block(i: Int): MlirBlock = blocks(i)
  }

  case class Op(
    state:   MlirOperationState,
    op:      MlirOperation,
    results: Seq[MlirValue])

  case class OpWithBody(
    state:   MlirOperationState,
    op:      MlirOperation,
    regions: Seq[Region]) {

    def region(i: Int): Region = {
      regions(i)
    }
  }

  val moduleItems = mutable.Map.empty[Long, MlirValue]

  var circuit:   OpWithBody = null
  var firModule: OpWithBody = null

  case class WhenContext(ctx: OpWithBody, var isElse: Boolean)
  var whenCtx: Stack[WhenContext] = Stack.empty

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

    def buildOp(
      parent:   MlirBlock,
      opName:   String,
      attrs:    Seq[MlirNamedAttribute],
      operands: Seq[MlirValue],
      results:  Seq[MlirType],
      loc:      MlirLocation
    ): Op = {
      val state = circt.mlirOperationStateGet(opName, loc)

      circt.mlirOperationStateAddAttributes(state, attrs)
      circt.mlirOperationStateAddOperands(state, operands)
      circt.mlirOperationStateAddResults(state, results)

      val op = circt.mlirOperationCreate(state)
      circt.mlirBlockAppendOwnedOperation(parent, op)

      val resultVals = results.zipWithIndex.map {
        case (_, i) => circt.mlirOperationGetResult(op, i)
      }

      Op(state, op, resultVals)
    }

    def newConstantValue(
      resultType: fir.Type,
      valueType:  MlirType,
      value:      Int
    ): MlirValue = {
      buildOp(
        parentBlock(),
        "firrtl.constant",
        Seq(circt.mlirNamedAttributeGet("value", circt.mlirIntegerAttrGet(valueType, value))),
        Seq.empty,
        Seq( /* result */ util.convert(resultType)),
        circt.unkLoc
      ).results(0)
    }

    // TODO: waiting for refactoring

    def parentBlock(): MlirBlock = {
      if (!whenCtx.isEmpty) {
        val w = whenCtx.top
        if (!w.isElse) {
          w.ctx.region(0).block(0)
        } else {
          w.ctx.region(1).block(0)
        }
      } else {
        firModule.region(0).block(0)
      }
    }

    // TODO: refactor me
    def buildOpWithBody(
      parent:       MlirBlock,
      inputRegions: Seq[Seq[(Seq[MlirType], Seq[MlirLocation])]],
      opName:       String,
      attrs:        Seq[MlirNamedAttribute],
      operands:     Seq[MlirValue],
      results:      Seq[MlirType],
      loc:          MlirLocation
    ): OpWithBody = {
      val (regions, regionsResult) = inputRegions.foldLeft(Seq.empty[MlirRegion], Seq.empty[Region]) {
        case ((regions, regionsResult), blocks) => {
          val region = circt.mlirRegionCreate()

          val blocksResult = blocks.map {
            case block @ (blockArgTypes, blockArgLocs) => {
              val block = circt.mlirBlockCreate(blockArgTypes, blockArgLocs)
              circt.mlirRegionAppendOwnedBlock(region, block)
              block
            }
          }

          (regions :+ region, regionsResult :+ Region(region, blocksResult))
        }
      }

      val state = circt.mlirOperationStateGet(opName, loc)

      circt.mlirOperationStateAddAttributes(state, attrs)
      circt.mlirOperationStateAddOperands(state, operands)
      circt.mlirOperationStateAddResults(state, results)
      circt.mlirOperationStateAddOwnedRegions(state, regions)

      val op = circt.mlirOperationCreate(state)
      circt.mlirBlockAppendOwnedOperation(parent, op)

      OpWithBody(state, op, regionsResult)
    }

    // got port index inside module
    def portIndex(data: Data): Reference = {
      def getPort(enclosure: BaseModule): MlirValue = {
        val index = enclosure
          .getChiselPorts(chisel3.experimental.UnlocatableSourceInfo)
          .indexWhere(_._2 == data)
        circt.mlirBlockGetArgument(firModule.region(0).block(0), index)
      }

      data.binding.map {
        case PortBinding(enclosure)       => RefValue(getPort(enclosure), data)
        case SecretPortBinding(enclosure) => RefValue(getPort(enclosure), data)
        case _                            => throw new Exception("got non-port data")
      }.getOrElse(throw new Exception("got unbound data"))
    }

    // got data index for Vec or Record
    def elementIndex(data: Data): Reference = data.binding.map {
      case binding: ChildBinding =>
        binding.parent match {
          case vec:    Vec[_] => SubIndex(vec.elementsIterator.indexWhere(_ == data), data)
          case record: chisel3.Record =>
            SubField(
              record.elements.size - record.elements.values.iterator.indexOf(data) - 1,
              data
            )
        }
      case SampleElementBinding(vec) =>
        SubIndex(vec.elementsIterator.indexWhere(_ == data), data)
      case _ => throw new Exception("got non-child data")
    }.getOrElse(throw new Exception("got unbound data"))

    // Got Reference for a node
    def reference(node: chisel3.internal.HasId, refChain: Seq[Reference]): Seq[Reference] = {

      def dataRef(id: Long): MlirValue = {
        moduleItems.get(id) match {
          case Some(value) => value
          case None        => throw new Exception(s"item $id not found")
        }
      }

      node match {
        // reference to instance
        case module: BaseModule =>
          // access the instance
          return refChain
        case data: Data =>
          // got a data
          data.binding.getOrElse(throw new Exception("got unbound data")) match {
            case PortBinding(enclosure)       => return reference(enclosure, refChain :+ portIndex(data))
            case SecretPortBinding(enclosure) => return reference(enclosure, refChain :+ portIndex(data))
            case ChildBinding(parent)         => return reference(parent, refChain :+ elementIndex(data))
            case SampleElementBinding(parent) => return reference(parent, refChain :+ elementIndex(data))
            case MemoryPortBinding(enclosure, visibility) =>
              return reference(enclosure, refChain :+ RefValue(dataRef(data._id), data))
            // case MemTypeBinding(parent)                   =>
            case WireBinding(enclosure, visibility) =>
              return reference(enclosure, refChain :+ RefValue(dataRef(data._id), data))
            case OpBinding(enclosure, visibility) =>
              return reference(enclosure, refChain :+ RefValue(dataRef(data._id), data))
            case RegBinding(enclosure, visibility) =>
              return reference(enclosure, refChain :+ RefValue(dataRef(data._id), data))
            case unhandled => throw new Exception(s"unhandled binding $unhandled")
          }
        case mem:  Mem[Data] => return refChain :+ RefValue(dataRef(mem.t._id), mem.t) // TODO: really no parent?
        case smem: SyncReadMem[Data] =>
          return refChain :+ RefValue(dataRef(smem.t._id), smem.t) // TODO: really no parent?
        case unhandled => throw new Exception(s"unhandled node $unhandled")
      }
      Seq()
    }

    def createReferenceWithType(
      id: chisel3.internal.HasId
    ): (MlirValue, fir.Type) = {
      val indexType = circt.mlirIntegerTypeGet(32)

      val refChain = reference(id, Seq())

      // After reverse, `Port` or `wire` should be the first element in the chain, so the initialization value of the `foldLeft` is unnecessary
      refChain.reverse.foldLeft[(MlirValue, fir.Type)]((null, null)) {
        case ((parent, parent_tpe), ref: Reference) => {
          ref match {
            case RefValue(value, firData) => (value, Converter.extractType(firData, null))
            case SubField(index, firData) =>
              val tpe = Converter.extractType(firData, null)
              (
                buildOp(
                  parentBlock(),
                  "firrtl.subfield",
                  Seq(circt.mlirNamedAttributeGet("fieldIndex", circt.mlirIntegerAttrGet(indexType, index))),
                  Seq(parent),
                  Seq(util.convert(tpe)),
                  circt.unkLoc
                ).results(0),
                tpe
              )
            case SubIndex(index, firData) =>
              val tpe = Converter.extractType(firData, null)
              (
                buildOp(
                  parentBlock(),
                  "firrtl.subindex",
                  Seq(circt.mlirNamedAttributeGet("index", circt.mlirIntegerAttrGet(indexType, index))),
                  Seq(parent),
                  Seq(util.convert(tpe)),
                  circt.unkLoc
                ).results(0),
                tpe
              )
          }
        }
      }
    }

    def createReferenceWithTypeFromArg(
      arg: Arg
    ): (MlirValue, fir.Type) = {
      def bitLength(n: Long): Int = {
        var bits = 0
        var num = n
        while (num != 0) {
          bits += 1
          num >>>= 1
        }
        bits
      }

      arg match {
        case Node(id) => createReferenceWithType(id)
        case ULit(value, width) =>
          val constantValue = value.toInt
          val (firWidth, valWidth) = width match {
            case _: UnknownWidth =>
              val bitLen = bitLength(constantValue)
              (fir.IntWidth(bitLen), bitLen)
            case w: KnownWidth => (fir.IntWidth(w.get), w.get)
          }
          val resultType = fir.UIntType(firWidth)
          val valueType = circt.mlirIntegerTypeUnsignedGet(valWidth)
          (util.newConstantValue(resultType, valueType, constantValue), resultType)
        case SLit(value, width) => // TODO: almost as the same with ULit, dedup me
          val constantValue = value.toInt
          val (firWidth, valWidth) = width match {
            case _: UnknownWidth =>
              val bitLen = bitLength(constantValue)
              (fir.IntWidth(bitLen), bitLen)
            case w: KnownWidth => (fir.IntWidth(w.get), w.get)
          }
          val resultType = fir.SIntType(firWidth)
          val valueType = circt.mlirIntegerTypeSignedGet(valWidth)
          (util.newConstantValue(resultType, valueType, constantValue), resultType)
        case unhandled => throw new Exception(s"unhandled arg type to be reference: $unhandled")
      }
    }

    def createReference(id: chisel3.internal.HasId): MlirValue = {
      createReferenceWithType(id)._1
    }

    def createNode(
      id:         Long,
      name:       String,
      resultType: fir.Type,
      input:      MlirValue
    ): Unit = {
      val op = buildOp(
        parentBlock(),
        "firrtl.node",
        Seq(
          circt.mlirNamedAttributeGet("name", circt.mlirStringAttrGet(name)),
          circt.mlirNamedAttributeGet("nameKind", circt.firrtlAttrGetNameKind(FIRRTLNameKind.InterestingName)),
          circt.mlirNamedAttributeGet("annotations", circt.emptyArrayAttr)
          // attr: inner_sym
          // attr: forceable
        ),
        Seq(input),
        Seq(
          /* result */ util.convert(resultType)
          /* ref */
        ),
        circt.unkLoc
      )
      moduleItems += ((id, op.results(0)))
    }
  }

  def dump(): Unit = {
    circt.mlirOperationDump(circt.mlirModuleGetOperation(module))
  }

  def exportFIRRTL(): Unit = {
    circt.mlirExportFIRRTL(module, message => print(message))
  }

  def visitCircuit(name: String): Unit = {
    circuit = util.buildOpWithBody(
      moduleBody,
      Seq(Seq((Seq.empty, Seq.empty))),
      "firrtl.circuit",
      Seq(
        circt.mlirNamedAttributeGet("name", circt.mlirStringAttrGet(name)),
        circt.mlirNamedAttributeGet("annotations", circt.emptyArrayAttr)
      ),
      Seq.empty,
      Seq.empty,
      circt.unkLoc
    )
  }

  // TODO: refactor me
  def visitDefModule(defModule: DefModule): Unit = {
    import org.llvm.{circt => c}
    import org.llvm.circt.c_api
    import java.lang.foreign._
    import java.lang.foreign.ValueLayout._

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

    firModule = util.buildOpWithBody(
      circuit.region(0).block(0),
      Seq(Seq((portsTypes, portsLocs))),
      "firrtl.module",
      Seq(
        circt.mlirNamedAttributeGet("sym_name", circt.mlirStringAttrGet(defModule.name)),
        circt.mlirNamedAttributeGet(
          "portDirections",
          circt.firrtlAttrGetPortDirs(
            ports.map(_.direction match {
              case fir.Input  => FIRRTLPortDir.Input
              case fir.Output => FIRRTLPortDir.Output
            })
          )
        ),
        circt.mlirNamedAttributeGet(
          "portNames",
          circt.mlirArrayAttrGet(ports.map(port => circt.mlirStringAttrGet(port.name)))
        ),
        circt.mlirNamedAttributeGet(
          "portTypes",
          circt.mlirArrayAttrGet(portsTypeAttrs)
        ),
        circt.mlirNamedAttributeGet(
          "portAnnotations",
          circt.mlirArrayAttrGet(portsAnnotationsAttrs)
        ),
        circt.mlirNamedAttributeGet(
          "portSyms",
          circt.emptyArrayAttr
        ),
        circt.mlirNamedAttributeGet(
          "portLocations",
          // TODO: figure out the relationship between `MlirAttribute` and `MlirLocation`
          circt.mlirArrayAttrGet(portsLocs.map(loc => MlirAttribute(loc.ptr)))
        ),
        circt.mlirNamedAttributeGet(
          "annotations",
          circt.emptyArrayAttr
        )
      ),
      Seq.empty,
      Seq.empty,
      circt.unkLoc
    )
  }

  def visitAltBegin(altBegin: AltBegin): Unit = {
    assert(false, "unimplemented")
  }

  def visitAttach(attach: Attach): Unit = {
    util.buildOp(
      util.parentBlock(),
      "firrtl.attach",
      Seq.empty,
      attach.locs.map(loc => util.createReference(loc.id)),
      Seq.empty,
      circt.unkLoc
    )
  }

  def visitConnect(connect: Connect): Unit = {
    util.buildOp(
      util.parentBlock(),
      "firrtl.connect",
      Seq.empty,
      Seq(
        /* dest */ util.createReference(connect.loc.id),
        /* src */ util.createReferenceWithTypeFromArg(connect.exp)._1
      ),
      Seq.empty,
      circt.unkLoc
    )
  }

  def visitDefWire(defWire: DefWire): Unit = {
    val wireName = Converter.getRef(defWire.id, defWire.sourceInfo).name
    val op = util.buildOp(
      util.parentBlock(),
      "firrtl.wire",
      Seq(
        circt.mlirNamedAttributeGet("name", circt.mlirStringAttrGet(wireName)),
        circt.mlirNamedAttributeGet("nameKind", circt.firrtlAttrGetNameKind(FIRRTLNameKind.InterestingName)),
        circt.mlirNamedAttributeGet("annotations", circt.emptyArrayAttr)
        // attr: inner_sym
        // attr: forceable
      ),
      Seq.empty,
      Seq(
        /* result */ util.convert(Converter.extractType(defWire.id, defWire.sourceInfo))
        /* ref */
      ),
      circt.unkLoc
    )
    moduleItems += ((defWire.id._id, op.results(0)))
  }

  def visitDefInvalid(defInvalid: DefInvalid): Unit = {
    val (dest, destType) = util.createReferenceWithTypeFromArg(defInvalid.arg)

    val invalidValue = util
      .buildOp(
        util.parentBlock(),
        "firrtl.invalidvalue",
        Seq.empty,
        Seq.empty,
        Seq(util.convert(destType)),
        circt.unkLoc
      )
      .results(0)

    util.buildOp(
      util.parentBlock(),
      "firrtl.connect",
      Seq.empty,
      Seq(
        /* dest */ dest,
        /* src */ invalidValue
      ),
      Seq.empty,
      circt.unkLoc
    )
  }

  def visitOtherwiseEnd(otherwiseEnd: OtherwiseEnd): Unit = {
    assert(otherwiseEnd.firrtlDepth == 1)
  }

  def visitWhenBegin(whenBegin: WhenBegin): Unit = {
    val cond = util.createReferenceWithTypeFromArg(whenBegin.pred)

    val op = util.buildOpWithBody(
      util.parentBlock(),
      Seq(
        // then region
        Seq((Seq.empty, Seq.empty)),
        // else region
        Seq((Seq.empty, Seq.empty))
      ),
      "firrtl.when",
      Seq.empty,
      Seq(
        /* condition */ cond._1
      ),
      Seq.empty,
      circt.unkLoc
    )

    whenCtx.push(WhenContext(op, false))
  }

  def visitWhenEnd(whenEnd: WhenEnd): Unit = {
    assert(whenEnd.firrtlDepth == 0)
    assert(whenEnd.hasAlt == false)

    whenCtx.pop()
  }

  def visitDefSeqMemory(defSeqMemory: DefSeqMemory): Unit = {
    val name = Converter.getRef(defSeqMemory.id, defSeqMemory.sourceInfo).name

    val op = util.buildOp(
      util.parentBlock(),
      "chirrtl.seqmem",
      Seq(
        circt.mlirNamedAttributeGet(
          "ruw",
          circt.firrtlAttrGetRUW(
            defSeqMemory.readUnderWrite match {
              case fir.ReadUnderWrite.Undefined => firrtlAttrGetRUW.Undefined
              case fir.ReadUnderWrite.Old       => firrtlAttrGetRUW.Old
              case fir.ReadUnderWrite.New       => firrtlAttrGetRUW.New
            }
          )
        ),
        circt.mlirNamedAttributeGet(
          "name",
          circt.mlirStringAttrGet(name)
        ),
        circt.mlirNamedAttributeGet("nameKind", circt.firrtlAttrGetNameKind(FIRRTLNameKind.InterestingName)),
        circt.mlirNamedAttributeGet(
          "annotations",
          circt.emptyArrayAttr
        )
        // attr: inner_sym
        // init
      ),
      Seq.empty,
      Seq(
        /* result */ circt.chirrtlTypeGetCMemory(
          util.convert(Converter.extractType(defSeqMemory.t, defSeqMemory.sourceInfo)),
          defSeqMemory.size.intValue
        )
      ),
      circt.unkLoc
    )
    moduleItems += ((defSeqMemory.t._id, op.results(0)))
  }

  def visitDefMemPort[T <: Data](defMemPort: DefMemPort[T]): Unit = {
    val op = util.buildOp(
      util.parentBlock(),
      "chirrtl.memoryport",
      Seq(
        circt.mlirNamedAttributeGet(
          "direction",
          circt.firrtlAttrGetMemDir(
            defMemPort.dir match {
              case MemPortDirection.READ  => FIRRTLMemDir.Read
              case MemPortDirection.WRITE => FIRRTLMemDir.Write
              case MemPortDirection.RDWR  => FIRRTLMemDir.ReadWrite
              case MemPortDirection.INFER => FIRRTLMemDir.Infer
            }
          )
        ),
        circt.mlirNamedAttributeGet(
          "name",
          circt.mlirStringAttrGet(Converter.getRef(defMemPort.id, defMemPort.sourceInfo).name)
        ),
        circt.mlirNamedAttributeGet(
          "annotations",
          circt.emptyArrayAttr
        )
      ),
      Seq(
        /* memory */ util.createReference(defMemPort.source.id)
      ),
      Seq(
        /* data */ util.convert(Converter.extractType(defMemPort.id, defMemPort.sourceInfo)),
        /* port */ circt.chirrtlTypeGetCMemoryPort()
      ),
      circt.unkLoc
    )

    util.buildOp(
      util.parentBlock(),
      "chirrtl.memoryport.access",
      Seq.empty,
      Seq(
        /* port */ op.results(1),
        /* index */ util.createReferenceWithTypeFromArg(defMemPort.index)._1,
        /* clock */ util.createReferenceWithTypeFromArg(defMemPort.clock)._1
      ),
      Seq.empty,
      circt.unkLoc
    )
    moduleItems += ((defMemPort.id._id, op.results(0)))
  }

  def visitDefMemory(defMemory: DefMemory): Unit = {
    val op = util.buildOp(
      util.parentBlock(),
      "chirrtl.combmem",
      Seq(
        circt.mlirNamedAttributeGet(
          "name",
          circt.mlirStringAttrGet(Converter.getRef(defMemory.id, defMemory.sourceInfo).name)
        ),
        circt.mlirNamedAttributeGet("nameKind", circt.firrtlAttrGetNameKind(FIRRTLNameKind.InterestingName)),
        circt.mlirNamedAttributeGet(
          "annotations",
          circt.emptyArrayAttr
        )
        // attr: inner_sym
        // init
      ),
      Seq.empty,
      Seq(
        /* result */ circt.chirrtlTypeGetCMemory(
          util.convert(Converter.extractType(defMemory.t, defMemory.sourceInfo)),
          defMemory.size.intValue
        )
      ),
      circt.unkLoc
    )
    moduleItems += ((defMemory.t._id, op.results(0)))
  }

  def visitDefPrim[T <: Data](defPrim: DefPrim[T]): Unit = {
    def refArg(index: Int): (MlirValue, fir.Type) = {
      util.createReferenceWithTypeFromArg(defPrim.args(index))
    }
    def signlessLitArg(
      index: Int
    ): BigInt = {
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
        val (lhs, rhs) = (refArg(0), refArg(1))
        val retType = (lhs._2, rhs._2) match {
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
        val (input, amount) = (refArg(0), signlessLitArg(1).toInt)
        val width = input._2 match {
          case fir.SIntType(fir.IntWidth(inputWidth)) => inputWidth - amount
          case fir.UIntType(fir.IntWidth(inputWidth)) => inputWidth - amount
        }
        val attrs = Seq(
          circt.mlirNamedAttributeGet(
            "amount",
            circt.mlirIntegerAttrGet(circt.mlirIntegerTypeGet(32), amount)
          )
        )
        (attrs, Seq(input), fir.UIntType(fir.IntWidth(width)))

      // Attributes
      //   amount: 32-bit signless integer
      // Operands
      //   input: sint or uint
      // Results
      //   result: uint : <amount>
      case PrimOp.HeadOp =>
        val (input, amount) = (refArg(0), signlessLitArg(1).toInt)
        val width = input._2 match {
          case fir.SIntType(_) => amount
          case fir.UIntType(_) => amount
        }
        val attrs = Seq(
          circt.mlirNamedAttributeGet(
            "amount",
            circt.mlirIntegerAttrGet(circt.mlirIntegerTypeGet(32), amount)
          )
        )
        (attrs, Seq(input), fir.UIntType(fir.IntWidth(width)))

      // Operands
      //   lhs: sint or uint
      //   rhs: sint or uint
      // Results
      //   result: sint or uint : <lhs + rhs>
      case PrimOp.TimesOp =>
        val (lhs, rhs) = (refArg(0), refArg(1))
        val retType = (lhs._2, rhs._2) match {
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
        val (lhs, rhs) = (refArg(0), refArg(1))
        val retType = (lhs._2, rhs._2) match {
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
        val (lhs, rhs) = (refArg(0), refArg(1))
        val retType = (lhs._2, rhs._2) match {
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
        val (input, amount) = (refArg(0), signlessLitArg(1).toInt)
        val (width, retTypeFn) = input._2 match {
          case fir.SIntType(fir.IntWidth(inputWidth)) => (inputWidth + amount, fir.SIntType)
          case fir.UIntType(fir.IntWidth(inputWidth)) => (inputWidth + amount, fir.UIntType)
        }
        val attrs = Seq(
          circt.mlirNamedAttributeGet(
            "amount",
            circt.mlirIntegerAttrGet(circt.mlirIntegerTypeGet(32), amount)
          )
        )
        (attrs, Seq(input), retTypeFn(fir.IntWidth(width)))

      // Attributes
      //   amount: 32-bit signless integer
      // Operands
      //   input: sint or uint
      // Results
      //   result: sint or uint : <max(input - amount, 1)>
      case PrimOp.ShiftRightOp =>
        val (input, amount) = (refArg(0), signlessLitArg(1).toInt)
        val (width, retTypeFn) = input._2 match {
          case fir.SIntType(fir.IntWidth(inputWidth)) => (max((inputWidth - amount).toInt, 1), fir.SIntType)
          case fir.UIntType(fir.IntWidth(inputWidth)) => (max((inputWidth - amount).toInt, 1), fir.UIntType)
        }
        val attrs = Seq(
          circt.mlirNamedAttributeGet(
            "amount",
            circt.mlirIntegerAttrGet(circt.mlirIntegerTypeGet(32), amount)
          )
        )
        (attrs, Seq(input), retTypeFn(fir.IntWidth(width)))

      // Operands
      //   lhs: sint or uint
      //   rhs: uint
      // Results
      //   result: sint or uint : <lhs + 2^rhs - 1>
      case PrimOp.DynamicShiftLeftOp =>
        val (lhs, rhs) = (refArg(0), refArg(1))
        val retType = (lhs._2, rhs._2) match {
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
        val (lhs, rhs) = (refArg(0), refArg(1))
        val retType = (lhs._2, rhs._2) match {
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
        val (lhs, rhs) = (refArg(0), refArg(1))
        val width = (lhs._2, rhs._2) match {
          case (fir.SIntType(lhsWidth), fir.SIntType(rhsWidth)) => lhsWidth.max(rhsWidth)
          case (fir.UIntType(lhsWidth), fir.UIntType(rhsWidth)) => lhsWidth.max(rhsWidth)
        }
        (Seq.empty, Seq(lhs, rhs), fir.UIntType(width))

      // Operands
      //   input: sint or uint
      // Results
      //   result: uint : <input>
      case PrimOp.BitNotOp =>
        val input = refArg(0)
        val width = input._2 match {
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
        val (lhs, rhs) = (refArg(0), refArg(1))
        val width = (lhs._2, rhs._2) match {
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
          (refArg(0), signlessLitArg(1).toInt, signlessLitArg(2).toInt)
        val width = hi - lo + 1
        val intType = circt.mlirIntegerTypeGet(32)
        val attrs = Seq(
          circt.mlirNamedAttributeGet(
            "hi",
            circt.mlirIntegerAttrGet(intType, hi)
          ),
          circt.mlirNamedAttributeGet(
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
        val (lhs, rhs) = (refArg(0), refArg(1))
        (Seq.empty, Seq(lhs, rhs), fir.UIntType(fir.IntWidth(1)))

      // Attributes
      //   amount: 32-bit signless integer
      // Operands
      //   input: sint or uint
      // Results
      //   result: sint or uint : <max(input, amount)>
      case PrimOp.PadOp =>
        val (input, amount) = (refArg(0), signlessLitArg(1).toInt)
        val (width, retTypeFn) = input._2 match {
          case fir.SIntType(fir.IntWidth(inputWidth)) => (max(inputWidth.toInt, amount), fir.SIntType)
          case fir.UIntType(fir.IntWidth(inputWidth)) => (max(inputWidth.toInt, amount), fir.UIntType)
        }
        val attrs = Seq(
          circt.mlirNamedAttributeGet(
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
        val input = refArg(0)
        val width = input._2 match {
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
        val (sel, high, low) = (refArg(0), refArg(1), refArg(2))
        assert(high._2 == low._2)
        (Seq.empty, Seq(sel, high, low), high._2)

      // Operands
      //   input: sint or uint
      // Results
      //   result: 1-bit uint
      case PrimOp.AndReduceOp | PrimOp.OrReduceOp | PrimOp.XorReduceOp =>
        val input = refArg(0)
        (Seq.empty, Seq(input), fir.UIntType(fir.IntWidth(1)))

      // Operands
      //   input: sint or uint
      // Results
      //   result: sint <if {uint} then {input + 1} else {input}>
      case PrimOp.ConvertOp =>
        val input = refArg(0)
        val width = input._2 match {
          case fir.SIntType(inputWidth) => inputWidth
          case fir.UIntType(inputWidth) => inputWidth + fir.IntWidth(1)
        }
        (Seq.empty, Seq(input), fir.SIntType(width))

      // Operands
      //   input: base type
      // Results
      //   result: uint(AsUInt) sint(AsSInt) : <if {sint or uint} then {input} else {1}>
      case PrimOp.AsUIntOp | PrimOp.AsSIntOp =>
        val input = refArg(0)
        val width = input._2 match {
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
        val input = refArg(0)
        (Seq.empty, Seq(input), fir.ClockType)

      // Operands
      //   input: 1-bit uint/sint/analog, reset, asyncreset, or clock
      // Results
      //   result: clock
      case PrimOp.AsAsyncResetOp =>
        val input = refArg(0)
        (Seq.empty, Seq(input), fir.ClockType)

      case _ => throw new Exception(s"defPrim: $defPrim")
    }

    val op = util.buildOp(
      util.parentBlock(),
      s"firrtl.${defPrim.op.toString}",
      attrs,
      operands.map(_._1),
      Seq(util.convert(resultType)),
      circt.unkLoc
    )
    util.createNode(defPrim.id._id, name, resultType, op.results(0))
  }

  def visitDefReg(defReg: DefReg): Unit = {
    val op = util.buildOp(
      util.parentBlock(),
      "firrtl.reg",
      Seq(
        circt.mlirNamedAttributeGet(
          "name",
          circt.mlirStringAttrGet(Converter.getRef(defReg.id, defReg.sourceInfo).name)
        ),
        circt.mlirNamedAttributeGet("nameKind", circt.firrtlAttrGetNameKind(FIRRTLNameKind.InterestingName)),
        circt.mlirNamedAttributeGet(
          "annotations",
          circt.emptyArrayAttr
        )
        // inner_sym
        // forceable
      ),
      Seq(
        /* clockVal */ util.createReferenceWithTypeFromArg(defReg.clock)._1
      ),
      Seq(
        /* result */ util.convert(Converter.extractType(defReg.id, defReg.sourceInfo))
        /* ref */
      ),
      circt.unkLoc
    )
    moduleItems += ((defReg.id._id, op.results(0)))
  }

  def visitDefRegInit(defRegInit: DefRegInit): Unit = {
    val op = util.buildOp(
      util.parentBlock(),
      "firrtl.regreset",
      Seq(
        circt.mlirNamedAttributeGet(
          "name",
          circt.mlirStringAttrGet(Converter.getRef(defRegInit.id, defRegInit.sourceInfo).name)
        ),
        circt.mlirNamedAttributeGet("nameKind", circt.firrtlAttrGetNameKind(FIRRTLNameKind.InterestingName)),
        circt.mlirNamedAttributeGet(
          "annotations",
          circt.emptyArrayAttr
        )
        // inner_sym
        // forceable
      ),
      Seq(
        /* clockVal */ util.createReferenceWithTypeFromArg(defRegInit.clock)._1,
        /* reset */ util.createReferenceWithTypeFromArg(defRegInit.reset)._1,
        /* init */ util.createReferenceWithTypeFromArg(defRegInit.init)._1
      ),
      Seq(
        /* result */ util.convert(Converter.extractType(defRegInit.id, defRegInit.sourceInfo))
        /* ref */
      ),
      circt.unkLoc
    )
    moduleItems += ((defRegInit.id._id, op.results(0)))
  }

  def visitPrintf(parent: Component, printf: Printf): Unit = {
    val (fmt, args) = Converter.unpack(printf.pable, parent)
    util.buildOp(
      util.parentBlock(),
      "firrtl.printf",
      Seq(
        circt.mlirNamedAttributeGet(
          "formatString",
          circt.mlirStringAttrGet(fmt)
        ),
        circt.mlirNamedAttributeGet(
          "name",
          circt.mlirStringAttrGet(Converter.getRef(printf.id, printf.sourceInfo).name)
        )
      ),
      Seq(
        /* clock */ util.createReferenceWithTypeFromArg(printf.clock)._1,
        /* cond */ util.newConstantValue(
          fir.UIntType(fir.IntWidth(1)),
          circt.mlirIntegerTypeUnsignedGet(1),
          1
        )
      ) ++ /* substitutions */ args.map(util.createReferenceWithTypeFromArg(_)._1),
      Seq.empty,
      circt.unkLoc
    )
  }

  def visitStop(stop: Stop): Unit = {
    util.buildOp(
      util.parentBlock(),
      "firrtl.stop",
      Seq(
        circt.mlirNamedAttributeGet(
          "exitCode",
          circt.mlirIntegerAttrGet(circt.mlirIntegerTypeGet(32), stop.ret)
        ),
        circt.mlirNamedAttributeGet(
          "name",
          circt.mlirStringAttrGet(Converter.getRef(stop.id, stop.sourceInfo).name)
        )
      ),
      Seq(
        /* clock */ util.createReferenceWithTypeFromArg(stop.clock)._1,
        /* cond */ util.newConstantValue(
          fir.UIntType(fir.IntWidth(1)),
          circt.mlirIntegerTypeUnsignedGet(1),
          1
        )
      ),
      Seq.empty,
      circt.unkLoc
    )
  }

  def visitVerification[T <: chisel3.VerificationStatement](
    verifi: Verification[T],
    opName: String,
    args:   Seq[Arg]
  ): Unit = {
    util.buildOp(
      util.parentBlock(),
      opName,
      Seq(
        circt.mlirNamedAttributeGet(
          "message",
          circt.mlirStringAttrGet(verifi.message)
        ),
        circt.mlirNamedAttributeGet(
          "name",
          circt.mlirStringAttrGet(Converter.getRef(verifi.id, verifi.sourceInfo).name)
        )
        /* isConcurrent */ // TODO
        /* eventControl */ // TODO
      ),
      Seq(
        /* clock */ util.createReferenceWithTypeFromArg(verifi.clock)._1,
        /* predicate */ util.createReferenceWithTypeFromArg(verifi.predicate)._1,
        /* enable */ util.newConstantValue(
          fir.UIntType(fir.IntWidth(1)),
          circt.mlirIntegerTypeUnsignedGet(1),
          1
        )
      ) ++ /* substitutions */ args.map(util.createReferenceWithTypeFromArg(_)._1),
      Seq.empty,
      circt.unkLoc
    )
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
  // Some initialize code when JVM start.

  def convert(circuit: Circuit): CIRCTConverter = {
    implicit val cvt = new PanamaCIRCTConverter
    visitCircuit(circuit)
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
      case defMemPort: DefMemPort[Data] =>
        visitDefMemPort(defMemPort)
      case defMemory: DefMemory =>
        visitDefMemory(defMemory)
      case defPrim: DefPrim[Data] =>
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
  def visitDefMemPort[T <: Data](defMemPort: DefMemPort[T])(implicit cvt: CIRCTConverter): Unit = {
    cvt.visitDefMemPort(defMemPort)
  }
  def visitDefMemory(defMemory: DefMemory)(implicit cvt: CIRCTConverter): Unit = {
    cvt.visitDefMemory(defMemory)
  }
  def visitDefPrim[T <: Data](defPrim: DefPrim[T])(implicit cvt: CIRCTConverter): Unit = {
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
