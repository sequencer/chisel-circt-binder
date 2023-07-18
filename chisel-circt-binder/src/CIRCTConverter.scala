// SPDX-License-Identifier: Apache-2.0

package chisel3.internal

import chisel3.Data
import chisel3.internal.firrtl._

abstract class CIRCTConverter {
  def dump()
  def exportFIRRTL()

  def visitCircuit(name:                     String)
  def visitDefModule(defModule:              DefModule)
  def visitAltBegin(altBegin:                AltBegin)
  def visitAttach(attach:                    Attach)
  def visitConnect(connect:                  Connect)
  def visitDefWire(defWire:                  DefWire)
  def visitDefInvalid(defInvalid:            DefInvalid)
  def visitOtherwiseEnd(otherwiseEnd:        OtherwiseEnd)
  def visitWhenBegin(whenBegin:              WhenBegin)
  def visitWhenEnd(whenEnd:                  WhenEnd)
  def visitDefSeqMemory(defSeqMemory:        DefSeqMemory)
  def visitDefMemPort[T <: Data](defMemPort: DefMemPort[T])
  def visitDefMemory(defMemory:              DefMemory)
  def visitDefPrim[T <: Data](defPrim:       DefPrim[T])
  def visitDefReg(defReg:                    DefReg)
  def visitDefRegInit(defRegInit:            DefRegInit)
  def visitPrintf(parent:                    Component, printf: Printf)
  def visitStop(stop:                        Stop)
  def visitVerification[T <: chisel3.VerificationStatement](
    verifi: Verification[T],
    opName: String,
    args:   Seq[Arg]
  )
  def visitAssert(assert: Verification[chisel3.assert.Assert])
  def visitAssume(assume: Verification[chisel3.assume.Assume])
  def visitCover(cover:   Verification[chisel3.cover.Cover])
}
