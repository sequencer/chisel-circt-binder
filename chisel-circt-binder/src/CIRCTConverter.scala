// SPDX-License-Identifier: Apache-2.0

package chisel3.internal

import chisel3.Data
import chisel3.internal.firrtl._

abstract class CIRCTConverter {
  def exportFIRRTL(): String

  def visitCircuit(name:                     String):        Unit
  def visitDefModule(defModule:              DefModule):     Unit
  def visitAltBegin(altBegin:                AltBegin):      Unit
  def visitAttach(attach:                    Attach):        Unit
  def visitConnect(connect:                  Connect):       Unit
  def visitDefWire(defWire:                  DefWire):       Unit
  def visitDefInvalid(defInvalid:            DefInvalid):    Unit
  def visitOtherwiseEnd(otherwiseEnd:        OtherwiseEnd):  Unit
  def visitWhenBegin(whenBegin:              WhenBegin):     Unit
  def visitWhenEnd(whenEnd:                  WhenEnd):       Unit
  def visitDefSeqMemory(defSeqMemory:        DefSeqMemory):  Unit
  def visitDefMemPort[T <: Data](defMemPort: DefMemPort[T]): Unit
  def visitDefMemory(defMemory:              DefMemory):     Unit
  def visitDefPrim[T <: Data](defPrim:       DefPrim[T]):    Unit
  def visitDefReg(defReg:                    DefReg):        Unit
  def visitDefRegInit(defRegInit:            DefRegInit): Unit
  def visitPrintf(parent:                    Component, printf: Printf): Unit
  def visitStop(stop:                        Stop):          Unit
  def visitVerification[T <: chisel3.VerificationStatement](
    verifi: Verification[T],
    opName: String,
    args:   Seq[Arg]
  ): Unit
  def visitAssert(assert: Verification[chisel3.assert.Assert]): Unit
  def visitAssume(assume: Verification[chisel3.assume.Assume]): Unit
  def visitCover(cover:   Verification[chisel3.cover.Cover]):   Unit
}
