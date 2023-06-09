// SPDX-License-Identifier: Apache-2.0

package chisel3.circt

import chisel3.{Aggregate, Data, Element, MemBase, Vec, VecLike}
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
    val wires = ArrayBuffer.empty[(String, MemorySegment /* MlirValue */ )]

    var circuit:   OpWithBody = null
    var firModule: OpWithBody = null

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

    case class Op(
      state:   (MemorySegment /* MlirOperationState */ ),
      op:      (MemorySegment /* MlirOperation */ ),
      results: (Seq[MemorySegment /* MlirValue */ ]))

    case class OpWithBody(
      state:  (MemorySegment /* MlirOperationState */ ),
      op:     (MemorySegment /* MlirOperation */ ),
      region: (MemorySegment /* MlirRegion */ ),
      block:  (MemorySegment /* MlirBlock */ ))

    private[converter] def buildOp(
      parent:   Option[MemorySegment /* MlirBlock */ ],
      opName:   String,
      attrs:    Seq[MemorySegment /* MlirNamedAttribute */ ],
      operands: Seq[MemorySegment /* MlirValue */ ],
      results:  Seq[MemorySegment /* MlirType */ ],
      loc:      MemorySegment /* MlirLocation */
    ): Op = {
      var state: MemorySegment /* MlirOperationState */ = NULL
      var op:    MemorySegment /* MlirOperation */ = NULL

      state = mlirOperationStateGet(arena, createMlirStr(opName), loc)

      val (mlirAttrs, attrsLen) = createNamedAttrs(attrs)
      val (mlirOperands, operandsLen) = createOperands(operands)
      val (mlirResults, resultsLen) = createResults(results)

      mlirOperationStateAddAttributes(state, attrsLen, mlirAttrs)
      mlirOperationStateAddOperands(state, operandsLen, mlirOperands)
      mlirOperationStateAddResults(state, resultsLen, mlirResults)

      op = mlirOperationCreate(arena, state)
      parent match {
        case Some(parent) =>
          mlirBlockAppendOwnedOperation(parent, op)
        case None => {}
      }

      val resultVals = results.zipWithIndex.map {
        case (_, i) => {
          mlirOperationGetResult(arena, op, i)
        }
      }

      Op(state, op, resultVals)
    }

    private[converter] def buildCircuit(
      parent: Option[MemorySegment /* MlirBlock */ ],
      opName: String,
      attrs:  Seq[MemorySegment /* MlirNamedAttribute */ ],
      loc:    MemorySegment /* MlirLocation */
    ): OpWithBody = {
      var state:  MemorySegment /* MlirOperationState */ = NULL
      var op:     MemorySegment /* MlirOperation */ = NULL
      var region: MemorySegment /* MlirRegion */ = NULL
      var block:  MemorySegment /* MlirBlock */ = NULL

      region = mlirRegionCreate(arena)
      block = mlirBlockCreate(arena, 0, NULL, NULL)
      mlirRegionAppendOwnedBlock(region, block)

      state = mlirOperationStateGet(arena, createMlirStr(opName), loc)

      val (mlirAttrs, length) = createNamedAttrs(attrs)
      mlirOperationStateAddAttributes(state, length, mlirAttrs)
      mlirOperationStateAddOwnedRegions(state, 1, region)

      op = mlirOperationCreate(arena, state)
      parent match {
        case Some(parent) =>
          mlirBlockAppendOwnedOperation(parent, op)
        case None => {}
      }

      OpWithBody(state, op, region, block)
    }

    private[converter] def buildModule(
      parent:    Option[MemorySegment /* MlirBlock */ ],
      opName:    String,
      attrs:     Seq[MemorySegment /* MlirNamedAttribute */ ],
      loc:       MemorySegment /* MlirLocation */,
      portsLen:  Int,
      portTypes: MemorySegment /* MlirType[] */,
      portLocs:  MemorySegment /* MlirLocation[] */
    ): OpWithBody = {
      var state:  MemorySegment /* MlirOperationState */ = NULL
      var op:     MemorySegment /* MlirOperation */ = NULL
      var region: MemorySegment /* MlirRegion */ = NULL
      var block:  MemorySegment /* MlirBlock */ = NULL

      region = mlirRegionCreate(arena)
      block = mlirBlockCreate(arena, portsLen, portTypes, portLocs)
      mlirRegionAppendOwnedBlock(region, block)

      state = mlirOperationStateGet(arena, createMlirStr(opName), loc)

      val (mlirAttrs, length) = createNamedAttrs(attrs)
      mlirOperationStateAddAttributes(state, length, mlirAttrs)
      mlirOperationStateAddOwnedRegions(state, 1, region)

      op = mlirOperationCreate(arena, state)
      parent match {
        case Some(parent) =>
          mlirBlockAppendOwnedOperation(parent, op)
        case None => {}
      }

      OpWithBody(state, op, region, block)
    }

    abstract class Reference {}
    case class Port(index: Int, tpe: fir.Type) extends Reference
    case class Wire(ref: MemorySegment, tpe: fir.Type) extends Reference
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
        case (name, _) => name == data.instanceName
      } match {
        case Some((_, value)) => value
        case None             => throw new Exception("wire not found")
      }
      Wire(value, Converter.extractType(data, null))
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
            case MemoryPortBinding(enclosure, visibility) => // TODO
            case MemTypeBinding(parent)                   => // TODO
            case WireBinding(enclosure, visibility)       => return reference(enclosure, refChain :+ wireRef(data))
            case _                                        => assert(false)
          }
        case base: MemBase[_] => // TODO
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
              (mlirBlockGetArgument(arena, firModule.block, ref), tpe)
            case Wire(value, tpe) => (value, tpe)
            case SubField(index, tpe) =>
              (
                buildOp(
                  Some(firModule.block),
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
                  Some(firModule.block),
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

    private[converter] def createReference(id: chisel3.internal.HasId): MemorySegment /* MlirValue */ = {
      createReferenceWithType(id)._1
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
      circuit = buildCircuit(
        Some(moduleBody),
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

      firModule = buildModule(
        Some(circuit.block),
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
        unkLoc,
        portsLength,
        types,
        locs
      )
    }

    private[converter] def visitAttach(attach: Attach): Unit = {
      buildOp(
        Some(firModule.block),
        "firrtl.attach",
        Seq.empty,
        attach.locs.map(loc => createReference(loc.id)),
        Seq.empty,
        unkLoc
      )
    }

    private[converter] def visitConnect(connect: Connect): Unit = {
      buildOp(
        Some(firModule.block),
        "firrtl.connect",
        Seq.empty,
        Seq(
          /* dest */ createReference(connect.loc.id),
          /* src */ createReference(connect.exp match {
            case Node(id) => id
            case _        => throw new Exception("unhandled exp type in connect op")
          })
        ),
        Seq.empty,
        unkLoc
      )
    }

    private[converter] def visitDefWire(defWire: DefWire): Unit = {
      val wireName = Converter.getRef(defWire.id, defWire.sourceInfo).name
      val op = buildOp(
        Some(firModule.block),
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
      wires += ((wireName, op.results(0)))
    }

    private[converter] def visitDefInvalid(defInvalid: DefInvalid): Unit = {
      val id = defInvalid.arg match {
        case Node(id) => id
        case _        => throw new Exception("unhandled arg type in invalid op")
      }
      val (dest, destType) = createReferenceWithType(id)

      val invalidValue = buildOp(
        Some(firModule.block),
        "firrtl.invalidvalue",
        Seq.empty,
        Seq.empty,
        Seq(createMlirType(destType)),
        unkLoc
      ).results(0)

      buildOp(
        Some(firModule.block),
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

    private[converter] def visitDefSeqMemory(defSeqMemory: DefSeqMemory): Unit = {
      val typeInt32 = mlirIntegerTypeGet(arena, ctx, 32)
      val typeInt64 = mlirIntegerTypeGet(arena, ctx, 64)

      val op = buildOp(
        Some(firModule.block),
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
            createStrAttr(Converter.getRef(defSeqMemory.id, defSeqMemory.sourceInfo).name)
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
    }

    private[converter] def visitDefMemory(defMemory: DefMemory): Unit = {
      val op = buildOp(
        Some(firModule.block),
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
      case defMemPort: DefMemPort[_] =>
        visitDefMemPort(defMemPort)
      case defMemory: DefMemory =>
        visitDefMemory(defMemory)
      case defPrim: DefPrim[_] =>
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
    }
  }
  def visitAltBegin(altBegin: AltBegin)(implicit ctx: ConverterContext): Unit = {
    println(s"altBegin: $altBegin")
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
    println(s"otherwiseEnd: $otherwiseEnd")
  }
  def visitWhenBegin(whenBegin: WhenBegin)(implicit ctx: ConverterContext): Unit = {
    println(s"whenBegin: $whenBegin")
  }
  def visitWhenEnd(whenEnd: WhenEnd)(implicit ctx: ConverterContext): Unit = {
    println(s"whenEnd: $whenEnd")
  }
  def visitDefInstance(defInstance: DefInstance)(implicit ctx: ConverterContext): Unit = {
    // TODO: unimplemented
  }
  def visitDefMemPort(defMemPort: DefMemPort[_])(implicit ctx: ConverterContext): Unit = {
    println(s"defMemPort: $defMemPort")
  }
  def visitDefMemory(defMemory: DefMemory)(implicit ctx: ConverterContext): Unit = {
    ctx.visitDefMemory(defMemory)
  }
  def visitDefPrim(defPrim: DefPrim[_])(implicit ctx: ConverterContext): Unit = {
    println(s"defPrim: $defPrim")
  }
  def visitDefReg(defReg: DefReg)(implicit ctx: ConverterContext): Unit = {
    println(s"defReg: $defReg")
  }
  def visitDefRegInit(defRegInit: DefRegInit)(implicit ctx: ConverterContext): Unit = {
    println(s"defRegInit: $defRegInit")
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
