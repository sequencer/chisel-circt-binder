// SPDX-License-Identifier: Apache-2.0

package tests.chisel3.circt

import utest._
import chisel3._

object Smoke extends TestSuite {
  val tests = Tests {
    test("cigarette") {
      class SmokeModule extends RawModule
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
  }
}
