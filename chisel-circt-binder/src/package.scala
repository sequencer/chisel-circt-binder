// SPDX-License-Identifier: Apache-2.0

package chisel3.circt

import java.lang.foreign._
import java.lang.foreign.MemorySegment.NULL
import java.lang.foreign.ValueLayout._
import scala.collection.mutable.ArrayBuffer
import chisel3.internal.firrtl._
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
    mlirDialectHandleLoadDialect(arena, firrtlDialect, ctx)

    val unkLoc = mlirLocationUnknownGet(arena, ctx)
    val emptyArrayAttr = firrtlGetAttrArray(arena, ctx, NULL, 0)

    val module = mlirModuleCreateEmpty(arena, unkLoc)
    val moduleBody = mlirModuleGetBody(arena, module)

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
      firrtlGetAttrString(arena, ctx, createMlirStr(str))
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

    private[converter] def createMlirTypeAttr(firType: fir.Type): MemorySegment /* MlirAttribute */ = {
      firrtlGetAttrType(arena, createMlirType(firType))
    }

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

    case class OpWithBody(
      state:  (MemorySegment /* MlirOperationState */ ),
      op:     (MemorySegment /* MlirOperation */ ),
      region: (MemorySegment /* MlirRegion */ ),
      block:  (MemorySegment /* MlirBlock */ ))

    private[converter] def buildOpWithBody(
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
      println(s"!!!")
      mlirExportFIRRTL(arena, module, stub, NULL)
    }

    private[converter] def visitCircuit(name: String): Unit = {
      circuit = buildOpWithBody(
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
      val portsLength = defModule.ports.length

      // FIXME: Currently jextract does not export `enum FIRRTLPortDirection`, so we assume the size of it is 4.
      val sizeOfFIRRTLPortDirection = 4
      val directionsAttr =
        MemorySegment.allocateNative(sizeOfFIRRTLPortDirection * portsLength, arena.scope())
      val namesAttr = MlirAttribute.allocateArray(portsLength, arena)
      val typesAttr = MlirAttribute.allocateArray(portsLength, arena)
      val annotationsAttr = MlirAttribute.allocateArray(portsLength, arena)
      val locsAttr = MlirAttribute.allocateArray(portsLength, arena)

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

          directionsAttr.setAtIndex(C_INT, i, direction)
          fillAttr(namesAttr, i, createStrAttr(firPort.name))
          fillAttr(typesAttr, i, createMlirTypeAttr(firPort.tpe))
          fillAttr(annotationsAttr, i, emptyArrayAttr)
          // TODO: syms
          fillAttr(locsAttr, i, unkLoc)
        }
      }

      firModule = buildOpWithBody(
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
            firrtlGetAttrArray(arena, ctx, namesAttr, portsLength)
          ),
          mlirNamedAttributeGet(
            arena,
            mlirIdentifierGet(arena, ctx, createMlirStr("portTypes")),
            firrtlGetAttrArray(arena, ctx, typesAttr, portsLength)
          ),
          mlirNamedAttributeGet(
            arena,
            mlirIdentifierGet(arena, ctx, createMlirStr("portAnnotations")),
            firrtlGetAttrArray(arena, ctx, annotationsAttr, portsLength)
          ),
          mlirNamedAttributeGet(
            arena,
            mlirIdentifierGet(arena, ctx, createMlirStr("portSyms")),
            emptyArrayAttr
          ),
          mlirNamedAttributeGet(
            arena,
            mlirIdentifierGet(arena, ctx, createMlirStr("portLocations")),
            firrtlGetAttrArray(arena, ctx, locsAttr, portsLength)
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
  def visitAltBegin(
    altBegin: AltBegin
  )(
    implicit ctx: ConverterContext
  ): Unit = {
    // TODO: Call C-API Here
  }
  def visitAttach(attach: Attach)(implicit ctx: ConverterContext): Unit = {
    // TODO: Call C-API Here
  }
  def visitConnect(connect: Connect)(implicit ctx: ConverterContext): Unit = {
    // TODO: Call C-API Here
  }
  def visitConnectInit(
    connectInit: ConnectInit
  )(
    implicit ctx: ConverterContext
  ): Unit = {
    // TODO: Call C-API here
  }
  def visitDefInvalid(
    defInvalid: DefInvalid
  )(
    implicit ctx: ConverterContext
  ): Unit = {
    // TODO: Call C-API here
  }
  def visitOtherwiseEnd(
    otherwiseEnd: OtherwiseEnd
  )(
    implicit ctx: ConverterContext
  ): Unit = {
    // TODO: Call C-API here
  }
  def visitWhenBegin(
    whenBegin: WhenBegin
  )(
    implicit ctx: ConverterContext
  ): Unit = {
    // TODO: Call C-API here
  }
  def visitWhenEnd(whenEnd: WhenEnd)(implicit ctx: ConverterContext): Unit = {
    // TODO: Call C-API here
  }
  def visitDefInstance(
    defInstance: DefInstance
  )(
    implicit ctx: ConverterContext
  ): Unit = {
    // TODO: Call C-API Here
  }
  def visitDefMemPort(
    defMemPort: DefMemPort[_]
  )(
    implicit ctx: ConverterContext
  ): Unit = {
    // TODO: Call C-API here
  }
  def visitDefMemory(
    defMemory: DefMemory
  )(
    implicit ctx: ConverterContext
  ): Unit = {
    // TODO: Call C-API here
  }
  def visitDefPrim(
    defPrim: DefPrim[_]
  )(
    implicit ctx: ConverterContext
  ): Unit = {
    // TODO: Call C-API here
  }
  def visitDefReg(defReg: DefReg)(implicit ctx: ConverterContext): Unit = {
    // TODO: Call C-API here
  }
  def visitDefRegInit(
    defRegInit: DefRegInit
  )(
    implicit ctx: ConverterContext
  ): Unit = {
    // TODO: Call C-API here
  }
  def visitDefSeqMemory(
    defSeqMemory: DefSeqMemory
  )(
    implicit ctx: ConverterContext
  ): Unit = {
    // TODO: Call C-API here
  }
  def visitDefWire(defWire: DefWire)(implicit ctx: ConverterContext): Unit = {
    // TODO: Call C-API here
  }
  def visitPrintf(printf: Printf)(implicit ctx: ConverterContext): Unit = {
    // TODO: Call C-API here
  }
  def visitStop(stop: Stop)(implicit ctx: ConverterContext): Unit = {
    // TODO: Call C-API here
  }
  def visitVerfiAssert(assert: Verification[chisel3.assert.Assert]): Unit = {
    // TODO: Call C-API here
  }
  def visitVerfiAssume(assume: Verification[chisel3.assume.Assume]): Unit = {
    // TODO: Call C-API here
  }
  def visitVerfiCover(cover: Verification[chisel3.cover.Cover]): Unit = {
    // TODO: Call C-API here
  }
  def visitVerfiPrintf(printf: Verification[chisel3.printf.Printf]): Unit = {
    // TODO: Call C-API here
  }
  def visitVerfiStop(stop: Verification[chisel3.stop.Stop]): Unit = {
    // TODO: Call C-API here
  }
}
