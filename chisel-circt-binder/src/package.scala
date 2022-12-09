// SPDX-License-Identifier: Apache-2.0

package chisel3.circt

import chisel3.internal.firrtl._

private[chisel3] object converter {
  // Some initialize code when JVM start.

  def convert(circuit: Circuit): ConverterContext = {
    implicit val ctx = new ConverterContext
    visitCircuit(circuit)
    ctx
  }
  // Context for storing a MLIR Builder
  class ConverterContext {
    // TODO
    def verilog: String = ""

    // TODO
    def firrtl: String = ""

    private[converter] def visitCircuit(name: String): Unit = {
      // TODO: Call C-API Here
    }

    private[converter] def visitDefModule(name: String): Unit = {
      // TODO: Call C-API Here
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
    // TODO: Call C-API Here
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
  def visitPort(port: Port)(implicit ctx: ConverterContext): Unit = {
    // TODO: Call C-API here
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
