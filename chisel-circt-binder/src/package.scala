// SPDX-License-Identifier: Apache-2.0

package chisel3.circt

import chisel3.internal.firrtl._

private[chisel3] object converter {
  def convert(circuit: Circuit): Unit = {
    implicit val visitor = new Visitor()
    visitCircuit(circuit)
  }
  // Context for storing a MLIR Builder
  class Visitor(){
    circt.binding.FIRRTL.mlirGetDialectHandle__firrtl__$MH()
  }
  def visitCircuit(circuit: Circuit)(implicit visitor: Visitor): Unit = {
    // TODO: Call C-API Here
    circuit.components.foreach {
      case defBlackBox: DefBlackBox =>
        visitDefBlackBox(defBlackBox)
      case defModule: DefModule =>
        visitDefModule(defModule)
    }
  }
  def visitDefBlackBox(defBlackBox: DefBlackBox)(implicit visitor: Visitor): Unit = {
    // TODO: Call C-API Here

    defBlackBox.ports.foreach { port =>
      visitPort(port)
    }
  }
  def visitDefModule(defModule: DefModule)(implicit visitor: Visitor): Unit = {
    // TODO: Call C-API Here

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
  def visitAltBegin(altBegin: AltBegin)(implicit visitor: Visitor): Unit = {
    // TODO: Call C-API Here
  }
  def visitAttach(attach: Attach)(implicit visitor: Visitor): Unit = {
    // TODO: Call C-API Here
  }
  def visitConnect(connect: Connect)(implicit visitor: Visitor): Unit = {
    // TODO: Call C-API Here
  }
  def visitConnectInit(connectInit: ConnectInit)(implicit visitor: Visitor): Unit = {
    // TODO: Call C-API here
  }
  def visitDefInvalid(defInvalid: DefInvalid)(implicit visitor: Visitor): Unit = {
    // TODO: Call C-API here
  }
  def visitOtherwiseEnd(otherwiseEnd: OtherwiseEnd)(implicit visitor: Visitor): Unit = {
    // TODO: Call C-API here
  }
  def visitPartialConnect(partialConnect: PartialConnect)(implicit visitor: Visitor): Unit = {
    // TODO: Call C-API here
  }
  def visitWhenBegin(whenBegin: WhenBegin)(implicit visitor: Visitor): Unit = {
    // TODO: Call C-API here
  }
  def visitWhenEnd(whenEnd: WhenEnd)(implicit visitor: Visitor): Unit = {
    // TODO: Call C-API here
  }
  def visitDefInstance(defInstance: DefInstance)(implicit visitor: Visitor): Unit = {
    // TODO: Call C-API Here

    defInstance.ports.foreach { port =>
      visitPort(port)
    }
  }
  def visitPort(port: Port)(implicit visitor: Visitor): Unit = {
    // TODO: Call C-API here
  }
  def visitDefMemPort(defMemPort: DefMemPort[_])(implicit visitor: Visitor): Unit = {
    // TODO: Call C-API here
  }
  def visitDefMemory(defMemory: DefMemory)(implicit visitor: Visitor): Unit = {
    // TODO: Call C-API here
  }
  def visitDefPrim(defPrim: DefPrim[_])(implicit visitor: Visitor): Unit = {
    // TODO: Call C-API here
  }
  def visitDefReg(defReg: DefReg)(implicit visitor: Visitor): Unit = {
    // TODO: Call C-API here
  }
  def visitDefRegInit(defRegInit: DefRegInit)(implicit visitor: Visitor): Unit = {
    // TODO: Call C-API here
  }
  def visitDefSeqMemory(defSeqMemory: DefSeqMemory)(implicit visitor: Visitor): Unit = {
    // TODO: Call C-API here
  }
  def visitDefWire(defWire: DefWire)(implicit visitor: Visitor): Unit = {
    // TODO: Call C-API here
  }
  def visitPrintf(printf: Printf)(implicit visitor: Visitor): Unit = {
    // TODO: Call C-API here
  }
  def visitStop(stop: Stop)(implicit visitor: Visitor): Unit = {
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
