// SPDX-License-Identifier: Apache-2.0

package chisel3.circt

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

import java.lang.foreign._
import java.lang.foreign.MemorySegment.NULL
import java.lang.foreign.ValueLayout._
import scala.collection.mutable.ArrayBuffer
import chisel3.internal.firrtl._
import firrtl.ir.HasName
import org.llvm.circt._
import org.llvm.circt.c_api._
import org.llvm.circt.FIRRTLBundleField
import firrtl.{ir => fir}
import chisel3.SyncReadMem

private[chisel3] object converter {
  // Some initialize code when JVM start.

  def convert(circuit: Circuit): ConverterContext = {
    implicit val ctx = new ConverterContext
    visitCircuit(circuit)
    ctx.dump() // debug
    ctx.exportFIRRTL() // debug
    ctx
  }
  // Context for storing a MLIR Builder
  class ConverterContext {
    // TODO
    def verilog: String = ""

    // TODO
    def firrtl: String = ""

    def arena = Arena.openConfined()

    val ctx = mlirContextCreate(arena)
    val firrtlDialect = mlirGetDialectHandle__firrtl__(arena)
    val chirrtlDialect = mlirGetDialectHandle__chirrtl__(arena)
    mlirDialectHandleLoadDialect(arena, firrtlDialect, ctx)
    mlirDialectHandleLoadDialect(arena, chirrtlDialect, ctx)

    val unkLoc = mlirLocationUnknownGet(arena, ctx)
    val emptyArrayAttr = mlirArrayAttrGet(arena, ctx, 0, NULL)

    val module = mlirModuleCreateEmpty(arena, unkLoc)
    val moduleBody = mlirModuleGetBody(arena, module)
    val wires = ArrayBuffer.empty[(Long, MemorySegment /* MlirValue */ )]
    val nodes = ArrayBuffer.empty[(Long, MemorySegment /* MlirValue */ )]
    val smem = ArrayBuffer.empty[(Long, MemorySegment /* MlirValue */ )]
    val cmem = ArrayBuffer.empty[(Long, MemorySegment /* MlirValue */ )]
    val mport = ArrayBuffer.empty[(Long, MemorySegment /* MlirValue */ )]
    val regs = ArrayBuffer.empty[(Long, MemorySegment /* MlirValue */ )]

    var circuit:   OpWithBody = null
    var firModule: OpWithBody = null

    // TODO: Support `when` with depth more than 1
    case class WhenContext(ctx: OpWithBody, var isElse: Boolean)
    var whenCtx: Option[WhenContext] = None

    private[converter] def createMlirStr(str: String): MemorySegment = {
      val strBytes = str.getBytes()
      val strSeg =
        MemorySegment.allocateNative(strBytes.length + 1, arena.scope())
      strSeg.copyFrom(MemorySegment.ofArray(strBytes))
      mlirStringRefCreateFromCString(arena, strSeg)
    }

    private[converter] def fromMlirStrRef(mlirStr: MemorySegment): String = {
      var strSlice = MlirStringRef.data$get(mlirStr).asSlice(0, MlirStringRef.length$get(mlirStr))
      new String(strSlice.toArray(JAVA_BYTE))
    }

    private[converter] def createStrAttr(str: String): MemorySegment /* MlirAttribute */ = {
      mlirStringAttrGet(arena, ctx, createMlirStr(str))
    }

    private[converter] def createMlirType(firType: fir.Type): MemorySegment /* MlirAttribute */ = {
      def convertFirWidth(width: fir.Width): Int = {
        width match {
          case fir.UnknownWidth => -1
          case fir.IntWidth(v)  => v.toInt
        }
      }

      firType match {
        case t: fir.UIntType => firrtlGetTypeUInt(arena, ctx, convertFirWidth(t.width))
        case t: fir.SIntType => firrtlGetTypeSInt(arena, ctx, convertFirWidth(t.width))
        case fir.ClockType      => firrtlGetTypeClock(arena, ctx)
        case fir.ResetType      => firrtlGetTypeReset(arena, ctx)
        case fir.AsyncResetType => firrtlGetTypeAsyncReset(arena, ctx)
        case t: fir.AnalogType => firrtlGetTypeAnalog(arena, ctx, convertFirWidth(t.width))
        case t: fir.VectorType =>
          firrtlGetTypeVector(arena, ctx, createMlirType(t.tpe), t.size)
        case t: fir.BundleType =>
          val fieldsSeg = FIRRTLBundleField.allocateArray(t.fields.length, arena)
          t.fields.zipWithIndex.foreach {
            case (field, i) =>
              val fieldSeg = fieldsSeg.asSlice(FIRRTLBundleField.sizeof() * i, FIRRTLBundleField.sizeof())
              val flip = field.flip match {
                case fir.Default => false
                case fir.Flip    => true
              }
              FIRRTLBundleField.flip$set(fieldSeg, flip)
              FIRRTLBundleField.name$slice(fieldSeg).copyFrom(createStrAttr(field.name))
              FIRRTLBundleField.type$slice(fieldSeg).copyFrom(createMlirType(field.tpe))
          }
          firrtlGetTypeBundle(arena, ctx, fieldsSeg, t.fields.length)
      }
    }

    // TODO: merge `createNamedAttrs`, `createOperands` and `createResults` into a general function

    private[converter] def createNamedAttrs(
      attrs: Seq[MemorySegment]
    ): (MemorySegment, Int) = {
      val attrsSeg = MlirNamedAttribute.allocateArray(attrs.length, arena)
      attrs.zipWithIndex.foreach {
        case (attr: MemorySegment, i: Int) => {
          val slice = attrsSeg.asSlice(
            MlirNamedAttribute.sizeof() * i,
            MlirNamedAttribute.sizeof()
          )
          slice.copyFrom(attr)
        }
      }
      (attrsSeg, attrs.length)
    }

    private[converter] def createOperands(
      operands: Seq[MemorySegment /* MlirValue */ ]
    ): (MemorySegment, Int) = {
      val operandsSeg = MlirValue.allocateArray(operands.length, arena)
      operands.zipWithIndex.foreach {
        case (attr: MemorySegment, i: Int) => {
          val slice = operandsSeg.asSlice(
            MlirValue.sizeof() * i,
            MlirValue.sizeof()
          )
          slice.copyFrom(attr)
        }
      }
      (operandsSeg, operands.length)
    }

    private[converter] def createResults(
      results: Seq[MemorySegment /* MlirType */ ]
    ): (MemorySegment, Int) = {
      val resultsSeg = MlirType.allocateArray(results.length, arena)
      results.zipWithIndex.foreach {
        case (attr: MemorySegment, i: Int) => {
          val slice = resultsSeg.asSlice(
            MlirType.sizeof() * i,
            MlirType.sizeof()
          )
          slice.copyFrom(attr)
        }
      }
      (resultsSeg, results.length)
    }

    private[converter] def parentBlock(): MemorySegment /* MlirBlock */ = {
      whenCtx match {
        case Some(w) =>
          if (!w.isElse) {
            w.ctx.region(0).block(0)
          } else {
            w.ctx.region(1).block(0)
          }
        case None => firModule.region(0).block(0)
      }
    }

    private[converter] def createConstantValue(
      resultType: fir.Type,
      valueType:  MemorySegment /* MlirType */,
      value:      Int
    ): MemorySegment /* MlirValue */ = {
      buildOp(
        parentBlock(),
        "firrtl.constant",
        Seq(
          mlirNamedAttributeGet(
            arena,
            mlirIdentifierGet(arena, ctx, createMlirStr("value")),
            mlirIntegerAttrGet(arena, valueType, value)
          )
        ),
        Seq.empty,
        Seq( /* result */ createMlirType(resultType)),
        unkLoc
      ).results(0)
    }

    case class Region(
      region: MemorySegment /* MlirRegion */,
      blocks: Seq[MemorySegment /* MlirBlock */ ]) {

      def get(): MemorySegment /* MlirRegion */ = {
        region
      }
      def block(i: Int): MemorySegment /* MlirRegion */ = {
        blocks(i)
      }
    }

    case class Op(
      state:   MemorySegment /* MlirOperationState */,
      op:      MemorySegment /* MlirOperation */,
      results: Seq[MemorySegment /* MlirValue */ ])

    case class OpWithBody(
      state:   MemorySegment /* MlirOperationState */,
      op:      MemorySegment /* MlirOperation */,
      regions: Seq[Region]) {

      def region(i: Int): Region = {
        regions(i)
      }
    }

    private[converter] def buildOp(
      parent:   MemorySegment /* MlirBlock */,
      opName:   String,
      attrs:    Seq[MemorySegment /* MlirNamedAttribute */ ],
      operands: Seq[MemorySegment /* MlirValue */ ],
      results:  Seq[MemorySegment /* MlirType */ ],
      loc:      MemorySegment /* MlirLocation */
    ): Op = {
      var state: MemorySegment /* MlirOperationState */ = NULL
      var op:    MemorySegment /* MlirOperation */ = NULL

      state = mlirOperationStateGet(arena, createMlirStr(opName), loc)

      if (!attrs.isEmpty) {
        val (mlirAttrs, attrsLen) = createNamedAttrs(attrs)
        mlirOperationStateAddAttributes(state, attrsLen, mlirAttrs)
      }
      if (!operands.isEmpty) {
        val (mlirOperands, operandsLen) = createOperands(operands)
        mlirOperationStateAddOperands(state, operandsLen, mlirOperands)
      }
      if (!results.isEmpty) {
        val (mlirResults, resultsLen) = createResults(results)
        mlirOperationStateAddResults(state, resultsLen, mlirResults)
      }

      op = mlirOperationCreate(arena, state)
      mlirBlockAppendOwnedOperation(parent, op)

      val resultVals = results.zipWithIndex.map {
        case (_, i) => {
          mlirOperationGetResult(arena, op, i)
        }
      }

      Op(state, op, resultVals)
    }

    private[converter] def buildOpWithBody(
      parent:   MemorySegment /* MlirBlock */,
      regions:  Seq[Seq[(Int, MemorySegment /* MlirType[] */, MemorySegment /* MlirLocation[] */ )]],
      opName:   String,
      attrs:    Seq[MemorySegment /* MlirNamedAttribute */ ],
      operands: Seq[MemorySegment /* MlirValue */ ],
      results:  Seq[MemorySegment /* MlirType */ ],
      loc:      MemorySegment /* MlirLocation */
    ): OpWithBody = {
      var state: MemorySegment /* MlirOperationState */ = NULL
      var op:    MemorySegment /* MlirOperation */ = NULL

      val mlirRegions = MlirRegion.allocateArray(regions.length, arena)

      val regionsResult = regions.zipWithIndex.map {
        case (blocks, i) => {
          val region = mlirRegionCreate(arena)

          val blocksResult = blocks.map {
            case block @ (blockArgLen, blockArgTypes, blockArgLocs) => {
              val block = mlirBlockCreate(arena, blockArgLen, blockArgTypes, blockArgLocs)
              mlirRegionAppendOwnedBlock(region, block)
              block
            }
          }

          mlirRegions
            .asSlice(MlirRegion.sizeof() * i, MlirRegion.sizeof())
            .copyFrom(region)

          Region(region, blocksResult)
        }
      }

      state = mlirOperationStateGet(arena, createMlirStr(opName), loc)

      if (!attrs.isEmpty) {
        val (mlirAttrs, attrsLen) = createNamedAttrs(attrs)
        mlirOperationStateAddAttributes(state, attrsLen, mlirAttrs)
      }
      if (!operands.isEmpty) {
        val (mlirOperands, operandsLen) = createOperands(operands)
        mlirOperationStateAddOperands(state, operandsLen, mlirOperands)
      }
      if (!results.isEmpty) {
        val (mlirResults, resultsLen) = createResults(results)
        mlirOperationStateAddResults(state, resultsLen, mlirResults)
      }

      mlirOperationStateAddOwnedRegions(state, regions.length, mlirRegions)

      op = mlirOperationCreate(arena, state)
      mlirBlockAppendOwnedOperation(parent, op)

      OpWithBody(state, op, regionsResult)
    }

    abstract class Reference {}
    case class Port(index: Int, tpe: fir.Type) extends Reference
    case class Wire(ref: MemorySegment, tpe: fir.Type) extends Reference
    case class Node_(ref: MemorySegment, tpe: fir.Type) extends Reference
    case class SMem(ref: MemorySegment, tpe: fir.Type) extends Reference
    case class CMem(ref: MemorySegment, tpe: fir.Type) extends Reference
    case class MPort(ref: MemorySegment, tpe: fir.Type) extends Reference
    case class Reg(ref: MemorySegment, tpe: fir.Type) extends Reference
    case class SubField(index: Int, tpe: fir.Type) extends Reference
    case class SubIndex(index: Int, tpe: fir.Type) extends Reference

    // got port index inside module
    def portIndex(data: Data): Reference = data.binding.map {
      case SecretPortBinding(enclosure) =>
        Port(
          enclosure
            .getChiselPorts(chisel3.experimental.UnlocatableSourceInfo)
            .indexWhere(_._2 == data),
          Converter.extractType(data, null)
        )
      case PortBinding(enclosure) =>
        Port(
          enclosure
            .getChiselPorts(chisel3.experimental.UnlocatableSourceInfo)
            .indexWhere(_._2 == data),
          Converter.extractType(data, null)
        )
      case _ => throw new Exception("got non-port data")
    }.getOrElse(throw new Exception("got unbound data"))

    def wireRef(data: Data): Reference = {
      val value = wires.find {
        case (id, _) => id == data._id
      } match {
        case Some((_, value)) => value
        case None             => throw new Exception("wire not found")
      }
      Wire(value, Converter.extractType(data, null))
    }

    def nodeRef(data: Data): Reference = {
      val value = nodes.find {
        case (id, _) => id == data._id
      } match {
        case Some((_, value)) => value
        case None             => throw new Exception("node not found")
      }
      Node_(value, Converter.extractType(data, null))
    }

    def smemRef(data: Data): Reference = {
      val value = smem.find {
        case (id, _) => id == data._id
      } match {
        case Some((_, value)) => value
        case None             => throw new Exception("smem not found")
      }
      SMem(value, Converter.extractType(data, null))
    }

    def cmemRef(data: Data): Reference = {
      val value = cmem.find {
        case (id, _) => id == data._id
      } match {
        case Some((_, value)) => value
        case None             => throw new Exception("cmem not found")
      }
      CMem(value, Converter.extractType(data, null))
    }

    def mportRef(data: Data): Reference = {
      val value = mport.find {
        case (id, _) => id == data._id
      } match {
        case Some((_, value)) => value
        case None             => throw new Exception("mport not found")
      }
      MPort(value, Converter.extractType(data, null))
    }

    def regRef(data: Data): Reference = {
      val value = regs.find {
        case (id, _) => id == data._id
      } match {
        case Some((_, value)) => value
        case None             => throw new Exception("reg not found")
      }
      Reg(value, Converter.extractType(data, null))
    }

    // got data index for Vec or Record
    def elementIndex(data: Data): Reference = data.binding.map {
      case binding: ChildBinding =>
        binding.parent match {
          case vec:    Vec[_] => SubIndex(vec.elementsIterator.indexWhere(_ == data), Converter.extractType(data, null))
          case record: chisel3.Record =>
            SubField(
              record.elements.size - record.elements.values.iterator.indexOf(data) - 1,
              Converter.extractType(data, null)
            )
        }
      case SampleElementBinding(vec) =>
        SubIndex(vec.elementsIterator.indexWhere(_ == data), Converter.extractType(data, null))
      case _ => throw new Exception("got non-child data")
    }.getOrElse(throw new Exception("got unbound data"))

    // Got Reference for a node
    def reference(node: chisel3.internal.HasId, refChain: Seq[Reference]): Seq[Reference] = {
      node match {
        // reference to instance
        case module: BaseModule =>
          // access the instance
          return refChain
        case data: Data =>
          // got a data
          data.binding.getOrElse(throw new Exception("got unbound data")) match {
            case PortBinding(enclosure)                   => return reference(enclosure, refChain :+ portIndex(data))
            case SecretPortBinding(enclosure)             => return reference(enclosure, refChain :+ portIndex(data))
            case ChildBinding(parent)                     => return reference(parent, refChain :+ elementIndex(data))
            case SampleElementBinding(parent)             => return reference(parent, refChain :+ elementIndex(data))
            case MemoryPortBinding(enclosure, visibility) => return reference(enclosure, refChain :+ mportRef(data))
            // case MemTypeBinding(parent)                   =>
            case WireBinding(enclosure, visibility) => return reference(enclosure, refChain :+ wireRef(data))
            case OpBinding(enclosure, visibility)   => return reference(enclosure, refChain :+ nodeRef(data))
            case RegBinding(enclosure, visibility)  => return reference(enclosure, refChain :+ regRef(data))
            case unhandled                          => throw new Exception(s"unhandled binding $unhandled")
          }
        case mem:  Mem[Data]         => return refChain :+ cmemRef(mem.t) // TODO: really no parent?
        case smem: SyncReadMem[Data] => return refChain :+ smemRef(smem.t) // TODO: really no parent?
        case unhandled => throw new Exception(s"unhandled node $unhandled")
      }
      Seq()
    }

    private[converter] def createReferenceWithType(
      id: chisel3.internal.HasId
    ): (MemorySegment /* MlirValue */, fir.Type) = {
      val idFieldIndex = mlirIdentifierGet(arena, ctx, createMlirStr("fieldIndex"))
      val idIndex = mlirIdentifierGet(arena, ctx, createMlirStr("index"))
      val indexType = mlirIntegerTypeGet(arena, ctx, 32)

      val refChain = reference(id, Seq())

      // After reverse, `Port` or `wire` should be the first element in the chain, so the initialization value of the `foldLeft` is unnecessary
      refChain.reverse.foldLeft[(MemorySegment, fir.Type)]((NULL, null)) {
        case ((parent: MemorySegment /* MlirValue */, parent_tpe), ref: Reference) => {
          ref match {
            case Port(ref, tpe) =>
              (mlirBlockGetArgument(arena, firModule.region(0).block(0), ref), tpe)
            case Wire(value, tpe)  => (value, tpe)
            case Node_(value, tpe) => (value, tpe)
            case SMem(value, tpe)  => (value, tpe)
            case CMem(value, tpe)  => (value, tpe)
            case MPort(value, tpe) => (value, tpe)
            case Reg(value, tpe)   => (value, tpe)
            case SubField(index, tpe) =>
              (
                buildOp(
                  parentBlock(),
                  "firrtl.subfield",
                  Seq(mlirNamedAttributeGet(arena, idFieldIndex, mlirIntegerAttrGet(arena, indexType, index))),
                  Seq(parent),
                  Seq(createMlirType(tpe)),
                  unkLoc
                ).results(0),
                tpe
              )
            case SubIndex(index, tpe) =>
              (
                buildOp(
                  parentBlock(),
                  "firrtl.subindex",
                  Seq(mlirNamedAttributeGet(arena, idIndex, mlirIntegerAttrGet(arena, indexType, index))),
                  Seq(parent),
                  Seq(createMlirType(tpe)),
                  unkLoc
                ).results(0),
                tpe
              )
          }
        }
      }
    }

    private[converter] def createReferenceWithTypeFromArg(
      arg: Arg
    ): (MemorySegment /* MlirValue */, fir.Type) = {
      arg match {
        case Node(id) => createReferenceWithType(id)
        case ULit(value, width) =>
          def bitLength(n: Long): Int = {
            var bits = 0
            var num = n
            while (num != 0) {
              bits += 1
              num >>>= 1
            }
            bits
          }

          val constantValue = value.toInt
          val (firWidth, valWidth) = width match {
            case _: UnknownWidth =>
              val bitLen = bitLength(constantValue)
              (fir.IntWidth(bitLen), bitLen)
            case w: KnownWidth => (fir.IntWidth(w.get), w.get)
          }
          val resultType = fir.UIntType(firWidth)
          val valueType = mlirIntegerTypeUnsignedGet(arena, ctx, valWidth)
          (createConstantValue(resultType, valueType, constantValue), resultType)
        case unhandled => throw new Exception(s"unhandled arg type to be reference: $unhandled")
      }
    }

    private[converter] def createReference(id: chisel3.internal.HasId): MemorySegment /* MlirValue */ = {
      createReferenceWithType(id)._1
    }

    private[converter] def createNode(
      id:         Long,
      name:       String,
      resultType: fir.Type,
      input:      MemorySegment /* MlirValue */
    ): Unit = {
      val op = buildOp(
        parentBlock(),
        "firrtl.node",
        Seq(
          mlirNamedAttributeGet(
            arena,
            mlirIdentifierGet(arena, ctx, createMlirStr("name")),
            createStrAttr(name)
          ),
          mlirNamedAttributeGet(
            arena,
            mlirIdentifierGet(arena, ctx, createMlirStr("nameKind")),
            firrtlGetAttrNameKind(arena, ctx, FIRRTL_NAME_KIND_INTERESTING_NAME)
          ),
          mlirNamedAttributeGet(
            arena,
            mlirIdentifierGet(arena, ctx, createMlirStr("annotations")),
            emptyArrayAttr
          )
          // attr: inner_sym
          // attr: forceable
        ),
        Seq(input),
        Seq(
          /* result */ createMlirType(resultType)
          /* ref */
        ),
        unkLoc
      )
      nodes += ((id, op.results(0)))
    }

    private[converter] def dump(): Unit = {
      mlirOperationDump(mlirModuleGetOperation(arena, module))
    }

    private[converter] def exportFIRRTL(): Unit = {
      val cb = new MlirStringCallback {
        def apply(message: MemorySegment, userData: MemorySegment): Unit = {
          print(fromMlirStrRef(message))
        }
      }
      val stub = MlirStringCallback.allocate(cb, arena.scope())
      mlirExportFIRRTL(arena, module, stub, NULL)
    }

    private[converter] def visitCircuit(name: String): Unit = {
      circuit = buildOpWithBody(
        moduleBody,
        Seq(Seq((0, NULL, NULL))),
        "firrtl.circuit",
        Seq(
          mlirNamedAttributeGet(
            arena,
            mlirIdentifierGet(arena, ctx, createMlirStr("name")),
            createStrAttr(name)
          ),
          mlirNamedAttributeGet(
            arena,
            mlirIdentifierGet(arena, ctx, createMlirStr("annotations")),
            emptyArrayAttr
          )
        ),
        Seq.empty,
        Seq.empty,
        unkLoc
      )
    }

    private[converter] def visitDefModule(defModule: DefModule): Unit = {
      wires.clear()

      val portsLength = defModule.ports.length

      // FIXME: Currently jextract does not export `enum FIRRTLPortDirection`, so we assume the size of it is 4.
      val sizeOfFIRRTLPortDirection = 4
      val directionsAttr =
        MemorySegment.allocateNative(sizeOfFIRRTLPortDirection * portsLength, arena.scope())
      val namesAttr = MlirAttribute.allocateArray(portsLength, arena)
      val typesAttr = MlirAttribute.allocateArray(portsLength, arena)
      val annotationsAttr = MlirAttribute.allocateArray(portsLength, arena)
      val locsAttr = MlirAttribute.allocateArray(portsLength, arena)

      val types = MlirType.allocateArray(portsLength, arena)
      val locs = MlirLocation.allocateArray(portsLength, arena)

      defModule.ports.zipWithIndex.foreach {
        case (port, i) => {
          val firPort = Converter.convert(port)

          val direction = (firPort.direction match {
            case fir.Input  => FIRRTL_PORT_DIRECTION_INPUT()
            case fir.Output => FIRRTL_PORT_DIRECTION_OUTPUT()
          })

          def fillAttr(attrs: MemorySegment, index: Int, value: MemorySegment) = {
            attrs
              .asSlice(MlirAttribute.sizeof() * index, MlirAttribute.sizeof())
              .copyFrom(value)
          }

          val mlirType = createMlirType(firPort.tpe)

          directionsAttr.setAtIndex(C_INT, i, direction)
          fillAttr(namesAttr, i, createStrAttr(firPort.name))
          fillAttr(typesAttr, i, mlirTypeAttrGet(arena, mlirType))
          fillAttr(annotationsAttr, i, emptyArrayAttr)
          // TODO: syms
          fillAttr(locsAttr, i, unkLoc)

          types
            .asSlice(MlirType.sizeof() * i, MlirType.sizeof())
            .copyFrom(mlirType)
          locs
            .asSlice(MlirLocation.sizeof() * i, MlirLocation.sizeof())
            .copyFrom(unkLoc)
        }
      }

      firModule = buildOpWithBody(
        circuit.region(0).block(0),
        Seq(Seq((portsLength, types, locs))),
        "firrtl.module",
        Seq(
          mlirNamedAttributeGet(
            arena,
            mlirIdentifierGet(arena, ctx, createMlirStr("sym_name")),
            createStrAttr(defModule.name)
          ),
          mlirNamedAttributeGet(
            arena,
            mlirIdentifierGet(arena, ctx, createMlirStr("portDirections")),
            firrtlGetAttrPortDirections(arena, ctx, directionsAttr, portsLength)
          ),
          mlirNamedAttributeGet(
            arena,
            mlirIdentifierGet(arena, ctx, createMlirStr("portNames")),
            mlirArrayAttrGet(arena, ctx, portsLength, namesAttr)
          ),
          mlirNamedAttributeGet(
            arena,
            mlirIdentifierGet(arena, ctx, createMlirStr("portTypes")),
            mlirArrayAttrGet(arena, ctx, portsLength, typesAttr)
          ),
          mlirNamedAttributeGet(
            arena,
            mlirIdentifierGet(arena, ctx, createMlirStr("portAnnotations")),
            mlirArrayAttrGet(arena, ctx, portsLength, annotationsAttr)
          ),
          mlirNamedAttributeGet(
            arena,
            mlirIdentifierGet(arena, ctx, createMlirStr("portSyms")),
            emptyArrayAttr
          ),
          mlirNamedAttributeGet(
            arena,
            mlirIdentifierGet(arena, ctx, createMlirStr("portLocations")),
            mlirArrayAttrGet(arena, ctx, portsLength, locsAttr)
          ),
          mlirNamedAttributeGet(
            arena,
            mlirIdentifierGet(arena, ctx, createMlirStr("annotations")),
            emptyArrayAttr
          )
        ),
        Seq.empty,
        Seq.empty,
        unkLoc
      )
    }

    private[converter] def visitAltBegin(altBegin: AltBegin): Unit = {
      whenCtx match {
        case Some(w) => w.isElse = true
      }
    }

    private[converter] def visitAttach(attach: Attach): Unit = {
      buildOp(
        parentBlock(),
        "firrtl.attach",
        Seq.empty,
        attach.locs.map(loc => createReference(loc.id)),
        Seq.empty,
        unkLoc
      )
    }

    private[converter] def visitConnect(connect: Connect): Unit = {
      buildOp(
        parentBlock(),
        "firrtl.connect",
        Seq.empty,
        Seq(
          /* dest */ createReference(connect.loc.id),
          /* src */ createReferenceWithTypeFromArg(connect.exp)._1
        ),
        Seq.empty,
        unkLoc
      )
    }

    private[converter] def visitDefWire(defWire: DefWire): Unit = {
      val wireName = Converter.getRef(defWire.id, defWire.sourceInfo).name
      val op = buildOp(
        parentBlock(),
        "firrtl.wire",
        Seq(
          mlirNamedAttributeGet(
            arena,
            mlirIdentifierGet(arena, ctx, createMlirStr("name")),
            createStrAttr(wireName)
          ),
          mlirNamedAttributeGet(
            arena,
            mlirIdentifierGet(arena, ctx, createMlirStr("nameKind")),
            firrtlGetAttrNameKind(arena, ctx, FIRRTL_NAME_KIND_INTERESTING_NAME)
          ),
          mlirNamedAttributeGet(
            arena,
            mlirIdentifierGet(arena, ctx, createMlirStr("annotations")),
            emptyArrayAttr
          )
          // attr: inner_sym
          // attr: forceable
        ),
        Seq.empty,
        Seq(
          /* result */ createMlirType(Converter.extractType(defWire.id, defWire.sourceInfo))
          /* ref */
        ),
        unkLoc
      )
      wires += ((defWire.id._id, op.results(0)))
    }

    private[converter] def visitDefInvalid(defInvalid: DefInvalid): Unit = {
      val (dest, destType) = createReferenceWithTypeFromArg(defInvalid.arg)

      val invalidValue = buildOp(
        parentBlock(),
        "firrtl.invalidvalue",
        Seq.empty,
        Seq.empty,
        Seq(createMlirType(destType)),
        unkLoc
      ).results(0)

      buildOp(
        parentBlock(),
        "firrtl.connect",
        Seq.empty,
        Seq(
          /* dest */ dest,
          /* src */ invalidValue
        ),
        Seq.empty,
        unkLoc
      )
    }

    private[converter] def visitOtherwiseEnd(otherwiseEnd: OtherwiseEnd): Unit = {
      assert(otherwiseEnd.firrtlDepth == 1)
    }

    private[converter] def visitWhenBegin(whenBegin: WhenBegin): Unit = {
      val cond = createReferenceWithTypeFromArg(whenBegin.pred)

      val op = buildOpWithBody(
        parentBlock(),
        Seq(
          // then region
          Seq((0, NULL, NULL)),
          // else region
          Seq((0, NULL, NULL))
        ),
        "firrtl.when",
        Seq.empty,
        Seq(
          /* condition */ cond._1
        ),
        Seq.empty,
        unkLoc
      )

      whenCtx = Some(WhenContext(op, false))
    }

    private[converter] def visitWhenEnd(whenEnd: WhenEnd): Unit = {
      assert(whenEnd.firrtlDepth == 0)
      assert(whenEnd.hasAlt == false)

      whenCtx = None
    }

    private[converter] def visitDefSeqMemory(defSeqMemory: DefSeqMemory): Unit = {
      val name = Converter.getRef(defSeqMemory.id, defSeqMemory.sourceInfo).name

      val op = buildOp(
        parentBlock(),
        "chirrtl.seqmem",
        Seq(
          mlirNamedAttributeGet(
            arena,
            mlirIdentifierGet(arena, ctx, createMlirStr("ruw")),
            firrtlGetAttrRUW(
              arena,
              ctx,
              defSeqMemory.readUnderWrite match {
                case fir.ReadUnderWrite.Undefined => FIRRTL_RUW_UNDEFINED
                case fir.ReadUnderWrite.Old       => FIRRTL_RUW_OLD
                case fir.ReadUnderWrite.New       => FIRRTL_RUW_NEW
              }
            )
          ),
          mlirNamedAttributeGet(
            arena,
            mlirIdentifierGet(arena, ctx, createMlirStr("name")),
            createStrAttr(name)
          ),
          mlirNamedAttributeGet(
            arena,
            mlirIdentifierGet(arena, ctx, createMlirStr("nameKind")),
            firrtlGetAttrNameKind(arena, ctx, FIRRTL_NAME_KIND_INTERESTING_NAME)
          ),
          mlirNamedAttributeGet(
            arena,
            mlirIdentifierGet(arena, ctx, createMlirStr("annotations")),
            emptyArrayAttr
          )
          // attr: inner_sym
          // init
        ),
        Seq.empty,
        Seq(
          /* result */ chirrtlGetTypeCMemory(
            arena,
            ctx,
            createMlirType(Converter.extractType(defSeqMemory.t, defSeqMemory.sourceInfo)),
            defSeqMemory.size.longValue
          )
        ),
        unkLoc
      )
      smem += ((defSeqMemory.t._id, op.results(0)))
    }

    private[converter] def visitDefMemPort[T <: Data](defMemPort: DefMemPort[T]): Unit = {
      val op = buildOp(
        parentBlock(),
        "chirrtl.memoryport",
        Seq(
          mlirNamedAttributeGet(
            arena,
            mlirIdentifierGet(arena, ctx, createMlirStr("direction")),
            firrtlGetAttrMemDir(
              arena,
              ctx,
              defMemPort.dir match {
                case MemPortDirection.READ  => FIRRTL_MEM_DIR_READ
                case MemPortDirection.WRITE => FIRRTL_MEM_DIR_WRITE
                case MemPortDirection.RDWR  => FIRRTL_MEM_DIR_READ_WRITE
                case MemPortDirection.INFER => FIRRTL_MEM_DIR_INFER
              }
            )
          ),
          mlirNamedAttributeGet(
            arena,
            mlirIdentifierGet(arena, ctx, createMlirStr("name")),
            createStrAttr(Converter.getRef(defMemPort.id, defMemPort.sourceInfo).name)
          ),
          mlirNamedAttributeGet(
            arena,
            mlirIdentifierGet(arena, ctx, createMlirStr("annotations")),
            emptyArrayAttr
          )
        ),
        Seq(
          /* memory */ createReference(defMemPort.source.id)
        ),
        Seq(
          /* data */ createMlirType(Converter.extractType(defMemPort.id, defMemPort.sourceInfo)),
          /* port */ chirrtlGetTypeCMemoryPort(arena, ctx)
        ),
        unkLoc
      )

      buildOp(
        parentBlock(),
        "chirrtl.memoryport.access",
        Seq.empty,
        Seq(
          /* port */ op.results(1),
          /* index */ createReferenceWithTypeFromArg(defMemPort.index)._1,
          /* clock */ createReferenceWithTypeFromArg(defMemPort.clock)._1
        ),
        Seq.empty,
        unkLoc
      )
      mport += ((defMemPort.id._id, op.results(0)))
    }

    private[converter] def visitDefMemory(defMemory: DefMemory): Unit = {
      val op = buildOp(
        parentBlock(),
        "chirrtl.combmem",
        Seq(
          mlirNamedAttributeGet(
            arena,
            mlirIdentifierGet(arena, ctx, createMlirStr("name")),
            createStrAttr(Converter.getRef(defMemory.id, defMemory.sourceInfo).name)
          ),
          mlirNamedAttributeGet(
            arena,
            mlirIdentifierGet(arena, ctx, createMlirStr("nameKind")),
            firrtlGetAttrNameKind(arena, ctx, FIRRTL_NAME_KIND_INTERESTING_NAME)
          ),
          mlirNamedAttributeGet(
            arena,
            mlirIdentifierGet(arena, ctx, createMlirStr("annotations")),
            emptyArrayAttr
          )
          // attr: inner_sym
          // init
        ),
        Seq.empty,
        Seq(
          /* result */ chirrtlGetTypeCMemory(
            arena,
            ctx,
            createMlirType(Converter.extractType(defMemory.t, defMemory.sourceInfo)),
            defMemory.size.longValue
          )
        ),
        unkLoc
      )
      cmem += ((defMemory.t._id, op.results(0)))
    }

    private[converter] def visitDefPrim[T <: Data](defPrim: DefPrim[T]): Unit = {
      def refArg(index: Int): (MemorySegment /* MlirValue */, fir.Type) = {
        createReferenceWithTypeFromArg(defPrim.args(index))
      }
      def arg(index: Int): Arg = {
        defPrim.args(index)
      }

      def signlessExtractLitFromArg(
        arg: Arg
      ): BigInt = {
        arg match {
          case ILit(value)    => value
          case ULit(value, _) => value
          case unhandled      => throw new Exception(s"unhandled lit arg type to be extracted: $unhandled")
        }
      }

      val name = Converter.getRef(defPrim.id, defPrim.sourceInfo).name

      val (opName, attrs, operands, resultType) = defPrim.op match {
        // case PrimOp.AddOp =>
        // case PrimOp.SubOp =>
        // case PrimOp.TailOp =>
        // case PrimOp.HeadOp =>
        // case PrimOp.TimesOp =>
        // case PrimOp.DivideOp =>
        // case PrimOp.RemOp =>
        // case PrimOp.ShiftLeftOp =>
        // case PrimOp.ShiftRightOp =>
        // case PrimOp.DynamicShiftLeftOp =>
        // case PrimOp.DynamicShiftRightOp =>

        // Operands
        //   lhs: sint or uint
        //   rhs: sint or uint
        // Results
        //   result: uint
        case PrimOp.BitAndOp =>
          val (lhs, rhs) = (refArg(0), refArg(1))
          val width = (lhs._2, rhs._2) match {
            case (fir.SIntType(lhs_width), fir.SIntType(rhs_width)) => assert(lhs_width == rhs_width); lhs_width
            case (fir.UIntType(lhs_width), fir.UIntType(rhs_width)) => assert(lhs_width == rhs_width); lhs_width
          }
          ("firrtl.and", Seq.empty, Seq(lhs, rhs), fir.UIntType(width))

        // case PrimOp.BitOrOp =>
        // case PrimOp.BitXorOp =>
        // case PrimOp.BitNotOp =>
        // case PrimOp.ConcatOp =>

        // Attributes
        //   hi: 32-bit signless integer
        //   lo: 32-bit signless integer
        // Operands
        //   input: sint or uint
        // Results
        //   result: uint
        case PrimOp.BitsExtractOp =>
          val (input, hi, lo) = (refArg(0), arg(1), arg(2))
          val (width, widthValue) = input._2 match {
            case fir.SIntType(width @ fir.IntWidth(widthValue)) => (width, widthValue)
            case fir.UIntType(width @ fir.IntWidth(widthValue)) => (width, widthValue)
          }
          val intType = mlirIntegerTypeGet(arena, ctx, widthValue.toInt)
          val attrs = Seq(
            mlirNamedAttributeGet(
              arena,
              mlirIdentifierGet(arena, ctx, createMlirStr("hi")),
              mlirIntegerAttrGet(arena, intType, signlessExtractLitFromArg(hi).toInt)
            ),
            mlirNamedAttributeGet(
              arena,
              mlirIdentifierGet(arena, ctx, createMlirStr("lo")),
              mlirIntegerAttrGet(arena, intType, signlessExtractLitFromArg(lo).toInt)
            )
          )

          ("firrtl.bits", attrs, Seq(input), fir.UIntType(width))

        // case PrimOp.LessOp =>
        // case PrimOp.LessEqOp =>
        // case PrimOp.GreaterOp =>
        // case PrimOp.GreaterEqOp =>

        // Operands
        //   lhs: sint or uint
        //   rhs: sint or uint
        // Results
        //   result: 1-bit uint
        case PrimOp.EqualOp =>
          val (lhs, rhs) = (refArg(0), refArg(1))
          ("firrtl.eq", Seq.empty, Seq(lhs, rhs), fir.UIntType(fir.IntWidth(1)))

        // case PrimOp.PadOp =>
        // case PrimOp.NotEqualOp =>
        // case PrimOp.NegOp =>
        // case PrimOp.MultiplexOp =>
        // case PrimOp.AndReduceOp =>
        // case PrimOp.OrReduceOp =>
        // case PrimOp.XorReduceOp =>
        // case PrimOp.ConvertOp =>
        // case PrimOp.AsUIntOp =>
        // case PrimOp.AsSIntOp =>
        // case PrimOp.AsFixedPointOp =>
        // case PrimOp.AsIntervalOp =>
        // case PrimOp.WrapOp =>
        // case PrimOp.SqueezeOp =>
        // case PrimOp.ClipOp =>
        // case PrimOp.SetBinaryPoint =>
        // case PrimOp.IncreasePrecision =>
        // case PrimOp.DecreasePrecision =>
        // case PrimOp.AsClockOp =>
        // case PrimOp.AsAsyncResetOp =>
        case _ => throw new Exception(s"defPrim: $defPrim")
      }

      val op = buildOp(
        parentBlock(),
        opName,
        attrs,
        operands.map(_._1),
        Seq(createMlirType(resultType)),
        unkLoc
      )
      createNode(defPrim.id._id, name, resultType, op.results(0))
    }

    private[converter] def visitDefReg(defReg: DefReg): Unit = {
      val op = buildOp(
        parentBlock(),
        "firrtl.reg",
        Seq(
          mlirNamedAttributeGet(
            arena,
            mlirIdentifierGet(arena, ctx, createMlirStr("name")),
            createStrAttr(Converter.getRef(defReg.id, defReg.sourceInfo).name)
          ),
          mlirNamedAttributeGet(
            arena,
            mlirIdentifierGet(arena, ctx, createMlirStr("nameKind")),
            firrtlGetAttrNameKind(arena, ctx, FIRRTL_NAME_KIND_INTERESTING_NAME)
          ),
          mlirNamedAttributeGet(
            arena,
            mlirIdentifierGet(arena, ctx, createMlirStr("annotations")),
            emptyArrayAttr
          )
          // inner_sym
          // forceable
        ),
        Seq(
          /* clockVal */ createReferenceWithTypeFromArg(defReg.clock)._1
        ),
        Seq(
          /* result */ createMlirType(Converter.extractType(defReg.id, defReg.sourceInfo))
          /* ref */
        ),
        unkLoc
      )
      regs += ((defReg.id._id, op.results(0)))
    }

    private[converter] def visitDefRegInit(defRegInit: DefRegInit): Unit = {
      val op = buildOp(
        parentBlock(),
        "firrtl.regreset",
        Seq(
          mlirNamedAttributeGet(
            arena,
            mlirIdentifierGet(arena, ctx, createMlirStr("name")),
            createStrAttr(Converter.getRef(defRegInit.id, defRegInit.sourceInfo).name)
          ),
          mlirNamedAttributeGet(
            arena,
            mlirIdentifierGet(arena, ctx, createMlirStr("nameKind")),
            firrtlGetAttrNameKind(arena, ctx, FIRRTL_NAME_KIND_INTERESTING_NAME)
          ),
          mlirNamedAttributeGet(
            arena,
            mlirIdentifierGet(arena, ctx, createMlirStr("annotations")),
            emptyArrayAttr
          )
          // inner_sym
          // forceable
        ),
        Seq(
          /* clockVal */ createReferenceWithTypeFromArg(defRegInit.clock)._1,
          /* reset */ createReferenceWithTypeFromArg(defRegInit.reset)._1,
          /* init */ createReferenceWithTypeFromArg(defRegInit.init)._1
        ),
        Seq(
          /* result */ createMlirType(Converter.extractType(defRegInit.id, defRegInit.sourceInfo))
          /* ref */
        ),
        unkLoc
      )
      regs += ((defRegInit.id._id, op.results(0)))
    }
  }

  def visitCircuit(circuit: Circuit)(implicit ctx: ConverterContext): Unit = {
    ctx.visitCircuit(circuit.name)
    circuit.components.foreach {
      case defBlackBox: DefBlackBox =>
        visitDefBlackBox(defBlackBox)
      case defModule: DefModule =>
        visitDefModule(defModule)
    }
  }
  def visitDefBlackBox(defBlackBox: DefBlackBox)(implicit ctx: ConverterContext): Unit = {
    // TODO: Call C-API Here
  }
  def visitDefModule(
    defModule: DefModule
  )(
    implicit ctx: ConverterContext
  ): Unit = {
    ctx.visitDefModule(defModule)
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
        visitPrintf(printf)
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
  def visitAltBegin(altBegin: AltBegin)(implicit ctx: ConverterContext): Unit = {
    ctx.visitAltBegin(altBegin)
  }
  def visitAttach(attach: Attach)(implicit ctx: ConverterContext): Unit = {
    ctx.visitAttach(attach)
  }
  def visitConnect(connect: Connect)(implicit ctx: ConverterContext): Unit = {
    ctx.visitConnect(connect)
  }
  def visitConnectInit(connectInit: ConnectInit)(implicit ctx: ConverterContext): Unit = {
    // Not used anywhere
  }
  def visitDefInvalid(defInvalid: DefInvalid)(implicit ctx: ConverterContext): Unit = {
    ctx.visitDefInvalid(defInvalid)
  }
  def visitOtherwiseEnd(otherwiseEnd: OtherwiseEnd)(implicit ctx: ConverterContext): Unit = {
    ctx.visitOtherwiseEnd(otherwiseEnd)
  }
  def visitWhenBegin(whenBegin: WhenBegin)(implicit ctx: ConverterContext): Unit = {
    ctx.visitWhenBegin(whenBegin)
  }
  def visitWhenEnd(whenEnd: WhenEnd)(implicit ctx: ConverterContext): Unit = {
    ctx.visitWhenEnd(whenEnd)
  }
  def visitDefInstance(defInstance: DefInstance)(implicit ctx: ConverterContext): Unit = {
    // TODO: unimplemented
  }
  def visitDefMemPort[T <: Data](defMemPort: DefMemPort[T])(implicit ctx: ConverterContext): Unit = {
    ctx.visitDefMemPort(defMemPort)
  }
  def visitDefMemory(defMemory: DefMemory)(implicit ctx: ConverterContext): Unit = {
    ctx.visitDefMemory(defMemory)
  }
  def visitDefPrim[T <: Data](defPrim: DefPrim[T])(implicit ctx: ConverterContext): Unit = {
    ctx.visitDefPrim(defPrim)
  }
  def visitDefReg(defReg: DefReg)(implicit ctx: ConverterContext): Unit = {
    ctx.visitDefReg(defReg)
  }
  def visitDefRegInit(defRegInit: DefRegInit)(implicit ctx: ConverterContext): Unit = {
    ctx.visitDefRegInit(defRegInit)
  }
  def visitDefSeqMemory(defSeqMemory: DefSeqMemory)(implicit ctx: ConverterContext): Unit = {
    ctx.visitDefSeqMemory(defSeqMemory)
  }
  def visitDefWire(defWire: DefWire)(implicit ctx: ConverterContext): Unit = {
    ctx.visitDefWire(defWire)
  }
  def visitPrintf(printf: Printf)(implicit ctx: ConverterContext): Unit = {
    println(s"printf: $printf")
  }
  def visitStop(stop: Stop)(implicit ctx: ConverterContext): Unit = {
    println(s"stop: $stop")
  }
  def visitVerfiAssert(assert: Verification[chisel3.assert.Assert]): Unit = {
    println(s"assert: $assert")
  }
  def visitVerfiAssume(assume: Verification[chisel3.assume.Assume]): Unit = {
    println(s"assume: $assume")
  }
  def visitVerfiCover(cover: Verification[chisel3.cover.Cover]): Unit = {
    println(s"cover: $cover")
  }
  def visitVerfiPrintf(printf: Verification[chisel3.printf.Printf]): Unit = {
    println(s"printf: $printf")
  }
  def visitVerfiStop(stop: Verification[chisel3.stop.Stop]): Unit = {
    println(s"stop: $stop")
  }
}
