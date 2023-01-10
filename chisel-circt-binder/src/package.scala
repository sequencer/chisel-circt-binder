// SPDX-License-Identifier: Apache-2.0

package chisel3.circt

import chisel3.internal.firrtl._
import chisel3.internal.sourceinfo._
import chisel3.SpecifiedDirection
import firrtl.{ir => fir}
import java.lang.foreign._
import java.lang.foreign.MemoryAddress.NULL
import java.lang.foreign.ValueLayout._
import org.llvm.circt.firrtl._
import org.llvm.circt.firrtl.CIRCTCAPIFIRRTL._
import chisel3.experimental._

private[chisel3] object converter {
  // Some initialize code when JVM start.

  def convert(circuit: Circuit): ConverterContext = {
    implicit val ctx = new ConverterContext
    visitCircuit(circuit)
    val firrtl = exportFirrtl()
    println(s"exported:\n$firrtl")
    ctx
  }
  // Context for storing a MLIR Builder
  class ConverterContext {
    def memorySession: MemorySession = MemorySession.openConfined()

    // TODO: check the memory leaks in the following methods
    def createMlirStr(str: String): MemorySegment = {
      val strBytes = str.getBytes()
      val strSeg = MemorySegment.allocateNative(strBytes.length, memorySession)
      strSeg.copyFrom(MemorySegment.ofArray(strBytes))

      val mlirStr = MemorySegment.allocateNative(MlirStringRef.$LAYOUT(), memorySession)
      MlirStringRef.data$set(mlirStr, strSeg.address());
      MlirStringRef.length$set(mlirStr, strBytes.length);

      mlirStr
    }
    def fromMlirStrRef(mlirStr: MemorySegment): String = {
      var strSlice = MemorySegment.ofAddress(MlirStringRef.data$get(mlirStr), MlirStringRef.length$get(mlirStr), memorySession)
      new String(strSlice.toArray(JAVA_BYTE))
    }

    // TODO
    def verilog: String = ""

    // TODO
    def firrtl: String = ""

    // TODO: destory the context when destructing
    val ctx: MemorySegment = firrtlCreateContext(SegmentAllocator.newNativeArena(memorySession))

    val errorHandler = new FirrtlErrorHandler {
      def apply(message: MemorySegment, userData: MemoryAddress): Unit = {
        var str = fromMlirStrRef(message)
        println(s"!!! $str")
      }
    }
    var errorHandlerStub: MemorySegment = FirrtlErrorHandler.allocate(errorHandler, memorySession)

    firrtlSetErrorHandler(ctx, errorHandlerStub, NULL)

    //firrtlDestroyContext(ctx)

    private[converter] def exportFirrtl(): String = {
      val result = firrtlExportFirrtl(SegmentAllocator.newNativeArena(memorySession), ctx);
      fromMlirStrRef(result)
    }

    private[converter] def visitCircuit(name: String): Unit = {
      firrtlVisitCircuit(ctx, createMlirStr(name))
    }

    private[converter] def visitDefModule(name: String): Unit = {
      firrtlVisitModule(ctx, createMlirStr(name))
    }

    private[converter] def visitExtModule(name: String, defName: String): Unit = {
      firrtlVisitExtModule(ctx, createMlirStr(name), createMlirStr(defName))
    }

    private[converter] def visitParameter(name: String, param: Param): Unit = {
      def createType(kind: Int, set_union: (MemorySegment) => Unit): MemorySegment = {
        val seg = MemorySegment.allocateNative(FirrtlType.$LAYOUT(), memorySession)
        FirrtlType.kind$set(seg, kind)
        set_union(FirrtlType.u$slice(seg))
        seg
      }

      val ffiParam = param match {
        case v: IntParam => createType(0 /* FIRRTL_PARAMETER_KIND_INT */, u => FirrtlParameterInt.value$set(u, v.value.toInt))
        case v: DoubleParam => createType(1 /* FIRRTL_PARAMETER_KIND_DOUBLE */, u => FirrtlParameterDouble.value$set(u, v.value))
        case v: StringParam => createType(2 /* FIRRTL_PARAMETER_KIND_STRING */, u => FirrtlParameterString.value$slice(u).copyFrom(createMlirStr(v.value)))
        case v: RawParam => createType(3 /* FIRRTL_PARAMETER_KIND_RAW */, u => FirrtlParameterRaw.value$slice(u).copyFrom(createMlirStr(v.value)))
      }

      firrtlVisitParameter(ctx, createMlirStr(name), ffiParam)
    }

    private[converter] def visitPort(name: String, port: Port): Unit = {
      import chisel3.SpecifiedDirection._;

      def createType(kind: Int, set_union: (MemorySegment) => Unit): MemorySegment = {
        val seg = MemorySegment.allocateNative(FirrtlType.$LAYOUT(), memorySession)
        FirrtlType.kind$set(seg, kind)
        set_union(FirrtlType.u$slice(seg))
        seg
      }

      def convertFirWidth(width: fir.Width): Int = {
        width match {
          case fir.UnknownWidth => -1
          case fir.IntWidth(v) => v.toInt
        }
      }

      // TODO: We should use enum constants here instead of literal integers, but because of a jextract bug they are not generated.
      //       I have reported this bug (internal review ID: 9074500), currently it is not public.

      def firTypeToFfiType(ty: fir.Type): MemorySegment = {
        ty match {
          case t: fir.UIntType => createType(0 /* FIRRTL_TYPE_KIND_UINT */, u => FirrtlTypeUInt.width$set(u, convertFirWidth(t.width)))
          case t: fir.SIntType => createType(1 /* FIRRTL_TYPE_KIND_SINT */, u => FirrtlTypeSInt.width$set(u, convertFirWidth(t.width)))
          case _: fir.FixedType => /* 2 FIRRTL_TYPE_KIND_FIXED */ throw new AssertionError("FixedPoint type is not supported yet in CIRCT")
          case _: fir.IntervalType => /* 3 FIRRTL_TYPE_KIND_INTERVAL */ throw new AssertionError("Interval type is not supported yet in CIRCT")
          case fir.ClockType => createType(4 /* FIRRTL_TYPE_KIND_CLOCK */, _ => {})
          case fir.ResetType => createType(5 /* FIRRTL_TYPE_KIND_RESET */, _ => {})
          case fir.AsyncResetType => createType(6 /* FIRRTL_TYPE_KIND_ASYNC_RESET */, _ => {})
          case t: fir.AnalogType => createType(7 /* FIRRTL_TYPE_KIND_ANALOG */, u => FirrtlTypeAnalog.width$set(u, convertFirWidth(t.width)))
          case t: fir.VectorType => createType(8 /* FIRRTL_TYPE_KIND_VECTOR */, u => {
            FirrtlTypeVector.type$set(u, firTypeToFfiType(t.tpe).address())
            FirrtlTypeVector.count$set(u, t.size)
          })
          case t: fir.BundleType => createType(9 /* FIRRTL_TYPE_KIND_BUNDLE */, u => {
            val fieldsSeg = MemorySegment.allocateNative(FirrtlTypeBundleField.sizeof() * t.fields.length, memorySession)
            t.fields.zipWithIndex.foreach(fi => {
              val (f, i) = fi;
              val field = fieldsSeg.asSlice(FirrtlTypeBundleField.sizeof() * i, FirrtlTypeBundleField.sizeof())
              FirrtlTypeBundleField.flip$set(field, f.flip match {
                case fir.Default => false
                case fir.Flip => true
              })
              FirrtlTypeBundleField.name$slice(field).copyFrom(createMlirStr(f.name))
              FirrtlTypeBundleField.type$set(field, firTypeToFfiType(f.tpe).address())
            })
            FirrtlTypeBundle.fields$set(u, fieldsSeg.address())
            FirrtlTypeBundle.count$set(u, t.fields.length)
          })
        }
      }

      val firPort = Converter.convert(port);

      val dir = firPort.direction match {
        case fir.Input  => 0 // FIRRTL_PORT_DIRECTION_INPUT
        case fir.Output => 1 // FIRRTL_PORT_DIRECTION_OUTPUT
      }
      val ty = firTypeToFfiType(firPort.tpe)

      firrtlVisitPort(ctx, createMlirStr(name), dir, ty);
    }

    private[converter] def _createStmt(kind: Int, set_union: (MemorySegment) => Unit): MemorySegment = {
      val seg = MemorySegment.allocateNative(FirrtlStatement.$LAYOUT(), memorySession)
      FirrtlStatement.kind$set(seg, kind)
      set_union(FirrtlStatement.u$slice(seg))
      seg
    }

    private[converter] def visitAttach(exprs: Seq[String]): Unit = {
      val stmt = _createStmt(0 /* FIRRTL_STATEMENT_KIND_ATTACH */, u => {
        val operandsSeg = MemorySegment.allocateNative(FirrtlStatementAttachOperand.sizeof() * exprs.length, memorySession)
        exprs.zipWithIndex.foreach { case(s, i) => {
          val operand = operandsSeg.asSlice(FirrtlStatementAttachOperand.sizeof() * i, FirrtlStatementAttachOperand.sizeof())
          FirrtlStatementAttachOperand.expr$slice(operand).copyFrom(createMlirStr(s))
        }}
        FirrtlStatementAttach.operands$set(u, operandsSeg.address())
        FirrtlStatementAttach.count$set(u, exprs.length)
      })
      firrtlVisitStatement(ctx, stmt.address());
    }
  }
  def exportFirrtl()(implicit ctx: ConverterContext): String = {
    ctx.exportFirrtl()
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

    defBlackBox.id match {
      case extModule: ExtModule => ctx.visitExtModule(defBlackBox.name, "")
    }

    // TODO: `defBlackBox.params` or `(defBlackBox.id as ExtModule).params`, which one shoule we use?
    defBlackBox.params.foreach { case (name, value) =>
      visitParameter(name, value)
    }

    defBlackBox.ports.foreach { port =>
      visitPort(port)
    }
  }
  def visitDefModule(defModule: DefModule)(implicit ctx: ConverterContext): Unit = {
    ctx.visitDefModule(defModule.name)

    defModule.ports.foreach { port =>
      visitPort(port)
    }
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
      case partialConnect: PartialConnect =>
        visitPartialConnect(partialConnect)
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
    // TODO: Call C-API Here
  }
  def visitAttach(attach: Attach)(implicit ctx: ConverterContext): Unit = {
    ctx.visitAttach(attach.locs.map(_.localName));
  }
  def visitConnect(connect: Connect)(implicit ctx: ConverterContext): Unit = {
    // TODO: Call C-API Here
  }
  def visitConnectInit(connectInit: ConnectInit)(implicit ctx: ConverterContext): Unit = {
    // TODO: Call C-API here
  }
  def visitDefInvalid(defInvalid: DefInvalid)(implicit ctx: ConverterContext): Unit = {
    // TODO: Call C-API here
  }
  def visitOtherwiseEnd(otherwiseEnd: OtherwiseEnd)(implicit ctx: ConverterContext): Unit = {
    // TODO: Call C-API here
  }
  def visitPartialConnect(partialConnect: PartialConnect)(implicit ctx: ConverterContext): Unit = {
    // TODO: Call C-API here
  }
  def visitWhenBegin(whenBegin: WhenBegin)(implicit ctx: ConverterContext): Unit = {
    // TODO: Call C-API here
  }
  def visitWhenEnd(whenEnd: WhenEnd)(implicit ctx: ConverterContext): Unit = {
    // TODO: Call C-API here
  }
  def visitDefInstance(defInstance: DefInstance)(implicit ctx: ConverterContext): Unit = {
    // TODO: Call C-API Here

    defInstance.ports.foreach { port =>
      visitPort(port)
    }
  }
  def visitParameter(name: String, param: Param)(implicit ctx: ConverterContext): Unit = {
    ctx.visitParameter(name, param)
  }
  def visitPort(port: Port)(implicit ctx: ConverterContext): Unit = {
    ctx.visitPort(Converter.getRef(port.id, port.sourceInfo).name, port)
  }
  def visitDefMemPort(defMemPort: DefMemPort[_])(implicit ctx: ConverterContext): Unit = {
    // TODO: Call C-API here
  }
  def visitDefMemory(defMemory: DefMemory)(implicit ctx: ConverterContext): Unit = {
    // TODO: Call C-API here
  }
  def visitDefPrim(defPrim: DefPrim[_])(implicit ctx: ConverterContext): Unit = {
    // TODO: Call C-API here
  }
  def visitDefReg(defReg: DefReg)(implicit ctx: ConverterContext): Unit = {
    // TODO: Call C-API here
  }
  def visitDefRegInit(defRegInit: DefRegInit)(implicit ctx: ConverterContext): Unit = {
    // TODO: Call C-API here
  }
  def visitDefSeqMemory(defSeqMemory: DefSeqMemory)(implicit ctx: ConverterContext): Unit = {
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
