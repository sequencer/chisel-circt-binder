// SPDX-License-Identifier: Apache-2.0

package chisel3.internal.panama

import chisel3.internal.firrtl.Converter
import chisel3.stage.ChiselCircuitAnnotation
import chisel3.stage.phases.Elaborate
import firrtl.AnnotationSeq
import firrtl.options.{Dependency, Phase}
import firrtl.stage.FirrtlCircuitAnnotation

object Convert extends Phase {
  override def prerequisites = Seq(Dependency[Elaborate])
  override def optionalPrerequisites = Seq.empty
  override def optionalPrerequisiteOf = Seq.empty
  override def invalidates(a: Phase) = false

  def transform(annotations: AnnotationSeq): AnnotationSeq =
    annotations.flatMap {
      case ChiselCircuitAnnotation(circuit) =>
        PanamaCIRCTConverter.convert(circuit)
        // TODO: return some handler but don't return the circuit
        None
      case a => Some(a)
    }
}
