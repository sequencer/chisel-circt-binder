// SPDX-License-Identifier: Apache-2.0

package tests.chisel3.circt

import utest._
import chisel3._
import chisel3.experimental._

object Smoke extends TestSuite {
  class SmokeModule extends RawModule

  class PortTestModule extends Module {
    val in = IO(Input(UInt(8.W)))
    val out = IO(Output(UInt(8.W)))
    val flipped = IO(Flipped(Output(UInt(8.W))))
    val inOutNonFlip = IO(new Bundle {
      val in = Input(UInt(8.W))
      val out = Output(UInt(8.W))
    })
    val inNonFlip = IO(new Bundle {
      val in1 = Input(UInt(8.W))
      val in2 = Input(UInt(8.W))
    })
    val outNonFlip = IO(new Bundle {
      val out1 = Output(UInt(8.W))
      val out2 = Output(UInt(8.W))
    })
    val inOut = IO(Flipped(new Bundle {
      val in = Input(UInt(8.W))
      val out = Output(UInt(8.W))
    }))
    val nestedVec = IO(Input(Vec(2, Vec(3, UInt(8.W)))))

    val a1 = IO(Analog(8.W))
    val a2 = IO(new Bundle { val sub = Analog(8.W) })
    val a3 = IO(new Bundle { val sub1 = new Bundle { val sub2 = Vec(2, Analog(8.W)) } })

    val a4 = IO(new Bundle { val s1 = Analog(8.W); val s2 = Analog(8.W) })
    val a5 = IO(new Bundle {
      val s1 = new Bundle {
        val s11 = Analog(8.W)
        val s12 = Analog(8.W)
      }
      val s2 = new Bundle {
        val s21 = Analog(8.W)
        val s22 = Analog(8.W)
      }
      val s3 = new Bundle {
        val s21 = Analog(8.W)
        val s22 = new Bundle {
          val s221 = Analog(8.W)
          val s222 = Analog(8.W)
        }
      }
    })

    attach(a1)
    attach(a5.s3.s22.s222)
    attach(a3.sub1.sub2(0), a2.sub)
    attach(a4.s1)
    attach(a4.s2)

    val wireTest = Wire(UInt(8.W))
    val wireInvalid = Wire(UInt(8.W))
    in <> wireTest
    wireInvalid := DontCare

    val width: Int = 32
    val io = IO(new Bundle {
      val enable = Input(Bool())
      val write = Input(Bool())
      val addr = Input(UInt(10.W))
      val dataIn = Input(UInt(width.W))
      val dataOut = Output(UInt(width.W))
    })

    val syncReadMem = SyncReadMem(1024, UInt(32.W))
    val mem = Mem(1024, UInt(32.W))
    wireTest := !io.write
    io.dataOut := syncReadMem.read(io.dataOut, io.enable && !io.write)
    when(io.enable) {
      when(io.write) {
        mem.write(io.addr, io.dataIn)
      }
    }

    val regOfVec = Reg(Vec(4, UInt(32.W))) // Register of 32-bit UInts
    regOfVec(0) := 123.U // Assignments to elements of the Vec
    regOfVec(1) := 456.U
    regOfVec(2) := 789.U
    regOfVec(3) := regOfVec(0)

    val initRegOfVec = RegInit(VecInit(Seq.fill(4)(0.U(32.W))))

    val printfIn = IO(Input(UInt(8.W)))
    printf(cf"in = $printfIn $printfIn\n")
    stop("???a")
  }

  val tests = Tests {
    test("cigarette") {
      Seq(
        new chisel3.stage.phases.Elaborate,
        chisel3.circt.Convert
      ).foldLeft(
        firrtl.AnnotationSeq(
          Seq(
            chisel3.stage.ChiselGeneratorAnnotation(() => new SmokeModule)
          )
        )
      ) { case (annos, phase) => phase.transform(annos) }
    }

    test("all (circt)") {
      println(_root_.circt.stage.ChiselStage.emitCHIRRTL(new PortTestModule))
    }

    test("all (binder)") {
      Seq(
        new chisel3.stage.phases.Elaborate,
        chisel3.circt.Convert
      ).foldLeft(
        firrtl.AnnotationSeq(
          Seq(
            chisel3.stage.ChiselGeneratorAnnotation(() => new PortTestModule)
          )
        )
      ) { case (annos, phase) => phase.transform(annos) }
    }
  }
}
