// SPDX-License-Identifier: Apache-2.0

package tests.chisel3.circt

import utest._
import chisel3._
import chisel3.experimental._

// TODO:
// - Infered width of types
// - `extmodule` and `parameter`

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
    val a2 = IO(new Bundle { var sub = Analog(8.W) })
    val a3 = IO(new Bundle { var sub = new Bundle { var sub = Analog(8.W) } })
    attach(a3.sub.sub, a2.sub)
  }

  val tests = Tests {
    //test("cigarette") {
    //  Seq(
    //    new chisel3.stage.phases.Elaborate,
    //    chisel3.circt.Convert
    //  ).foldLeft(firrtl.AnnotationSeq(Seq(
    //    chisel3.stage.ChiselGeneratorAnnotation(() => new SmokeModule)
    //  ))) { case (annos, phase) => phase.transform(annos) }
    //}
    test("port") {
      Seq(
        new chisel3.stage.phases.Elaborate,
        chisel3.circt.Convert
      ).foldLeft(firrtl.AnnotationSeq(Seq(
        chisel3.stage.ChiselGeneratorAnnotation(() => new PortTestModule)
      ))) { case (annos, phase) => phase.transform(annos) }
    }
    //test("without binder") {
    //  println(chisel3.stage.ChiselStage.emitFirrtl(new SmokeModule))
    //}
    test("port (without binder") {
      println(chisel3.stage.ChiselStage.emitFirrtl(new PortTestModule))
    }
  }

}
