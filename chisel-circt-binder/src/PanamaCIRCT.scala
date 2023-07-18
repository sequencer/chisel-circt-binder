// SPDX-License-Identifier: Apache-2.0

package chisel3.internal.panama.circt

import java.lang.foreign._
import java.lang.foreign.MemorySegment.NULL
import java.lang.foreign.ValueLayout._

import org.llvm.circt
import org.llvm.circt.c_api // TODO: rename it to CAPI

// Wrapper for CIRCT APIs with Panama framework
class PanamaCIRCT {
  // Open an arena for memory management of MLIR API calling in this context instance

  /* TODO: private */
  val arena = Arena.openConfined()

  // Create MLIR context and register dialects we need
  /* TODO: private */
  val mlirCtx = {
    val mlirCtx = c_api.mlirContextCreate(arena)

    // Register dialects
    Seq(
      c_api.mlirGetDialectHandle__firrtl__(arena),
      c_api.mlirGetDialectHandle__chirrtl__(arena)
    ).foreach(c_api.mlirDialectHandleLoadDialect(arena, _, mlirCtx))

    mlirCtx
  }

  // Public items for outside

  // Constants for this instance
  // TODO: use unknown location for now, but we should use the location from chisel
  // In the future, we may be able to pass entire JVM stack to CIRCT in the “debug mode”
  // and Chisel SourceInfo may not be useful anymore, since we can directly access SourceInfo with reflection.
  // some ideas: use com-lihaoyi/sourcecode to reduce the maintaining burden.
  val unkLoc = mlirLocationUnknownGet()
  val emptyArrayAttr = mlirArrayAttrGet(Seq.empty)

  //////////////////////////////////////////////////
  // Helpers
  //

  // def newString(string: String): MlirStringRef = mlirStringRefCreateFromCString(string)

  /* TODO: private */
  def newString(string: String): MlirStringRef = {
    val bytes = string.getBytes()
    val buffer = MemorySegment.allocateNative(bytes.length + 1, arena.scope())
    buffer.copyFrom(MemorySegment.ofArray(bytes))
    MlirStringRef(c_api.mlirStringRefCreateFromCString(arena, buffer))
  }

  // TODO: dedup us

  private def seqToArrayMemorySegment(xs: Seq[ForeignType[MemorySegment]]): (MemorySegment, Int) = {
    if (xs.nonEmpty) {
      // TODO: We should access the `sizeof` in the way of `T.sizeof`
      val sizeOfT = xs(0).sizeof

      val buffer = MemorySegment.allocateNative(sizeOfT * xs.length, arena.scope())
      xs.zipWithIndex.foreach {
        case (x, i) => buffer.asSlice(sizeOfT * i, sizeOfT).copyFrom(x.get)
      }
      (buffer, xs.length)
    } else {
      (NULL, 0)
    }
  }

  private def seqToArrayInt(xs: Seq[ForeignType[Int]]): (MemorySegment, Int) = {
    if (xs.nonEmpty) {
      // TODO: We should access the `sizeof` in the way of `T.sizeof`
      val sizeOfT = xs(0).sizeof

      val buffer = MemorySegment.allocateNative(sizeOfT * xs.length, arena.scope())
      xs.zipWithIndex.foreach {
        case (x, i) => buffer.setAtIndex(c_api.C_INT, i, x.get)
      }
      (buffer, xs.length)
    } else {
      (NULL, 0)
    }
  }

  //////////////////////////////////////////////////
  // CIRCT APIs
  //

  def mlirModuleCreateEmpty(location: MlirLocation) = MlirModule(c_api.mlirModuleCreateEmpty(arena, location.get))

  def mlirModuleGetBody(module: MlirModule) = MlirBlock(c_api.mlirModuleGetBody(arena, module.get))

  def mlirModuleGetOperation(module: MlirModule) = MlirOperation(c_api.mlirModuleGetOperation(arena, module.get))

  def mlirOperationStateGet(name: String, loc: MlirLocation) = MlirOperationState(
    c_api.mlirOperationStateGet(arena, newString(name).get, loc.get)
  )

  def mlirOperationStateAddAttributes(state: MlirOperationState, attrs: Seq[MlirNamedAttribute]) = {
    if (attrs.nonEmpty) {
      val (ptr, length) = seqToArrayMemorySegment(attrs)
      c_api.mlirOperationStateAddAttributes(state.get, length, ptr)
    }
  }

  def mlirOperationStateAddOperands(state: MlirOperationState, operands: Seq[MlirValue]) = {
    if (operands.nonEmpty) {
      val (ptr, length) = seqToArrayMemorySegment(operands)
      c_api.mlirOperationStateAddOperands(state.get, length, ptr)
    }
  }

  def mlirOperationStateAddResults(state: MlirOperationState, results: Seq[MlirType]) = {
    if (results.nonEmpty) {
      val (ptr, length) = seqToArrayMemorySegment(results)
      c_api.mlirOperationStateAddResults(state.get, length, ptr)
    }
  }

  def mlirOperationStateAddOwnedRegions(state: MlirOperationState, regions: Seq[MlirRegion]) = {
    if (regions.nonEmpty) {
      val (ptr, length) = seqToArrayMemorySegment(regions)
      c_api.mlirOperationStateAddOwnedRegions(state.get, length, ptr)
    }
  }

  def mlirOperationCreate(state: MlirOperationState) = MlirOperation(c_api.mlirOperationCreate(arena, state.get))

  def mlirOperationGetResult(operation: MlirOperation, pos: Int) = MlirValue(
    c_api.mlirOperationGetResult(arena, operation.get, pos)
  )

  def mlirBlockCreate(args: Seq[MlirType], locs: Seq[MlirLocation]): MlirBlock = {
    assert(args.length == locs.length)
    val length = args.length
    MlirBlock(c_api.mlirBlockCreate(arena, length, seqToArrayMemorySegment(args)._1, seqToArrayMemorySegment(locs)._1))
  }

  def mlirBlockGetArgument(block: MlirBlock, pos: Int) = MlirValue(c_api.mlirBlockGetArgument(arena, block.get, pos))

  def mlirBlockAppendOwnedOperation(block: MlirBlock, operation: MlirOperation) = {
    c_api.mlirBlockAppendOwnedOperation(block.get, operation.get)
  }

  def mlirRegionCreate() = MlirRegion(c_api.mlirRegionCreate(arena))

  def mlirRegionAppendOwnedBlock(region: MlirRegion, block: MlirBlock) = {
    c_api.mlirRegionAppendOwnedBlock(region.get, block.get)
  }

  def mlirLocationUnknownGet() = MlirLocation(c_api.mlirLocationUnknownGet(arena, mlirCtx))

  def mlirIdentifierGet(string: String) = MlirIdentifier(c_api.mlirIdentifierGet(arena, mlirCtx, newString(string).get))

  def mlirNamedAttributeGet(name: String, attr: MlirAttribute) = MlirNamedAttribute(
    c_api.mlirNamedAttributeGet(arena, mlirIdentifierGet(name).get, attr.get)
  )

  def mlirArrayAttrGet(elements: Seq[MlirAttribute]): MlirAttribute = {
    val (ptr, length) = seqToArrayMemorySegment(elements)
    MlirAttribute(c_api.mlirArrayAttrGet(arena, mlirCtx, length, ptr))
  }

  def mlirTypeAttrGet(tpe: MlirType) = MlirAttribute(c_api.mlirTypeAttrGet(arena, tpe.get))

  def mlirStringAttrGet(string: String) = MlirAttribute(c_api.mlirStringAttrGet(arena, mlirCtx, newString(string).get))

  def mlirIntegerAttrGet(tpe: MlirType, value: Int) = MlirAttribute(c_api.mlirIntegerAttrGet(arena, tpe.get, value))

  def mlirIntegerTypeGet(bitwidth: Int) = MlirType(c_api.mlirIntegerTypeGet(arena, mlirCtx, bitwidth))

  def mlirIntegerTypeUnsignedGet(bitwidth: Int) = MlirType(c_api.mlirIntegerTypeUnsignedGet(arena, mlirCtx, bitwidth))

  def mlirIntegerTypeSignedGet(bitwidth: Int) = MlirType(c_api.mlirIntegerTypeSignedGet(arena, mlirCtx, bitwidth))

  def mlirOperationDump(op: MlirOperation) = {
    c_api.mlirOperationDump(op.get)
  }

  def mlirExportFIRRTL(module: MlirModule, callback: String => Unit) = {
    val cb = new circt.MlirStringCallback {
      def apply(message: MemorySegment, userData: MemorySegment) = {
        callback(MlirStringRef(message).toString)
      }
    }
    val stub = circt.MlirStringCallback.allocate(cb, arena.scope())
    c_api.mlirExportFIRRTL(arena, module.get, stub, NULL)
  }

  def firrtlTypeGetUInt(width: Int) = MlirType(c_api.firrtlTypeGetUInt(arena, mlirCtx, width))

  def firrtlTypeGetSInt(width: Int) = MlirType(c_api.firrtlTypeGetSInt(arena, mlirCtx, width))

  def firrtlTypeGetClock() = MlirType(c_api.firrtlTypeGetClock(arena, mlirCtx))

  def firrtlTypeGetReset() = MlirType(c_api.firrtlTypeGetReset(arena, mlirCtx))

  def firrtlTypeGetAsyncReset() = MlirType(c_api.firrtlTypeGetAsyncReset(arena, mlirCtx))

  def firrtlTypeGetAnalog(width: Int) = MlirType(c_api.firrtlTypeGetAnalog(arena, mlirCtx, width))

  def firrtlTypeGetVector(element: MlirType, count: Int) = MlirType(
    c_api.firrtlTypeGetVector(arena, mlirCtx, element.get, count)
  )

  def firrtlTypeGetBundle(fields: Seq[FIRRTLBundleField]): MlirType = {
    val buffer = circt.FIRRTLBundleField.allocateArray(fields.length, arena)
    fields.zipWithIndex.foreach {
      case (field, i) =>
        val fieldBuffer = buffer.asSlice(circt.FIRRTLBundleField.sizeof() * i, circt.FIRRTLBundleField.sizeof())
        circt.FIRRTLBundleField.name$slice(fieldBuffer).copyFrom(mlirIdentifierGet(field.name).get)
        circt.FIRRTLBundleField.isFlip$set(fieldBuffer, field.isFlip)
        circt.FIRRTLBundleField.type$slice(fieldBuffer).copyFrom(field.tpe.get)
    }
    MlirType(c_api.firrtlTypeGetBundle(arena, mlirCtx, fields.length, buffer))
  }

  def firrtlAttrGetPortDirs(dirs: Seq[FIRRTLPortDir]) = {
    val (ptr, length) = seqToArrayInt(dirs)
    MlirAttribute(c_api.firrtlAttrGetPortDirs(arena, mlirCtx, length, ptr))
  }

  def firrtlAttrGetNameKind(nameKind: FIRRTLNameKind) = MlirAttribute(
    c_api.firrtlAttrGetNameKind(arena, mlirCtx, nameKind.value)
  )

  def firrtlAttrGetRUW(ruw: firrtlAttrGetRUW) = MlirAttribute(c_api.firrtlAttrGetRUW(arena, mlirCtx, ruw.value))

  def firrtlAttrGetMemDir(dir: FIRRTLMemDir) = MlirAttribute(c_api.firrtlAttrGetMemDir(arena, mlirCtx, dir.value))

  def chirrtlTypeGetCMemory(elementType: MlirType, numElements: Int) = MlirType(
    c_api.chirrtlTypeGetCMemory(arena, mlirCtx, elementType.get, numElements)
  )

  def chirrtlTypeGetCMemoryPort() = MlirType(c_api.chirrtlTypeGetCMemoryPort(arena, mlirCtx))
}

//
// MLIR & CIRCT Types
//
// Since all structs returned from Panama framework are `MemorySegment`, which is like a `void *` in C.
// We create these type wrappers to wrap it, make them type-safe.
//

trait ForeignType[T] {
  private[circt] def get: T
  private[circt] val sizeof: Int
}

// TODO: Make the `ctor` and `ptr` private to outside!

case class MlirAttribute(ptr: MemorySegment) extends ForeignType[MemorySegment] {
  private[circt] def get = ptr
  private[circt] val sizeof = circt.MlirAttribute.sizeof().toInt
}

case class MlirNamedAttribute(ptr: MemorySegment) extends ForeignType[MemorySegment] {
  private[circt] def get = ptr
  private[circt] val sizeof = circt.MlirNamedAttribute.sizeof().toInt
}

case class MlirBlock(ptr: MemorySegment) extends ForeignType[MemorySegment] {
  private[circt] def get = ptr
  private[circt] val sizeof = circt.MlirBlock.sizeof().toInt
}

case class MlirRegion(ptr: MemorySegment) extends ForeignType[MemorySegment] {
  private[circt] def get = ptr
  private[circt] val sizeof = circt.MlirRegion.sizeof().toInt
}

case class MlirIdentifier(ptr: MemorySegment) extends ForeignType[MemorySegment] {
  private[circt] def get = ptr
  private[circt] val sizeof = circt.MlirIdentifier.sizeof().toInt
}

case class MlirLocation(ptr: MemorySegment) extends ForeignType[MemorySegment] {
  private[circt] def get = ptr
  private[circt] val sizeof = circt.MlirLocation.sizeof().toInt
}

case class MlirModule(ptr: MemorySegment) extends ForeignType[MemorySegment] {
  private[circt] def get = ptr
  private[circt] val sizeof = circt.MlirModule.sizeof().toInt
}

case class MlirOperation(ptr: MemorySegment) extends ForeignType[MemorySegment] {
  private[circt] def get = ptr
  private[circt] val sizeof = circt.MlirOperation.sizeof().toInt
}

case class MlirOperationState(ptr: MemorySegment) extends ForeignType[MemorySegment] {
  private[circt] def get = ptr
  private[circt] val sizeof = circt.MlirOperationState.sizeof().toInt
}

case class MlirType(ptr: MemorySegment) extends ForeignType[MemorySegment] {
  private[circt] def get = ptr
  private[circt] val sizeof = circt.MlirType.sizeof().toInt
}

case class MlirValue(ptr: MemorySegment) extends ForeignType[MemorySegment] {
  private[circt] def get = ptr
  private[circt] val sizeof = circt.MlirValue.sizeof().toInt
}

case class MlirStringRef(ptr: MemorySegment) extends ForeignType[MemorySegment] {
  private[circt] def get = ptr
  private[circt] val sizeof = circt.MlirStringRef.sizeof().toInt

  override def toString: String = {
    var slice = circt.MlirStringRef.data$get(ptr).asSlice(0, circt.MlirStringRef.length$get(ptr))
    new String(slice.toArray(JAVA_BYTE))
  }
}

class FIRRTLBundleField(
  val name:   String,
  val isFlip: Boolean,
  val tpe:    MlirType)

//
// MLIR & CIRCT Enums
//

sealed abstract class FIRRTLNameKind(val value: Int) extends ForeignType[Int] {
  private[circt] def get = value
  private[circt] val sizeof = 4 // FIXME: jextract doesn't export type for C enum
}
object FIRRTLNameKind {
  final case object DroppableName extends FIRRTLNameKind(value = c_api.FIRRTL_NAME_KIND_DROPPABLE_NAME())
  final case object InterestingName extends FIRRTLNameKind(value = c_api.FIRRTL_NAME_KIND_INTERESTING_NAME())
}

sealed abstract class FIRRTLPortDir(val value: Int) extends ForeignType[Int] {
  private[circt] def get = value
  private[circt] val sizeof = 4 // FIXME: jextract doesn't export type for C enum
}
object FIRRTLPortDir {
  final case object Input extends FIRRTLPortDir(value = c_api.FIRRTL_PORT_DIR_INPUT())
  final case object Output extends FIRRTLPortDir(value = c_api.FIRRTL_PORT_DIR_OUTPUT())
}

sealed abstract class firrtlAttrGetRUW(val value: Int) extends ForeignType[Int] {
  private[circt] def get = value
  private[circt] val sizeof = 4 // FIXME: jextract doesn't export type for C enum
}
object firrtlAttrGetRUW {
  final case object Undefined extends firrtlAttrGetRUW(value = c_api.FIRRTL_RUW_UNDEFINED())
  final case object Old extends firrtlAttrGetRUW(value = c_api.FIRRTL_RUW_OLD())
  final case object New extends firrtlAttrGetRUW(value = c_api.FIRRTL_RUW_NEW())
}

sealed abstract class FIRRTLMemDir(val value: Int) extends ForeignType[Int] {
  private[circt] def get = value
  private[circt] val sizeof = 4 // FIXME: jextract doesn't export type for C enum
}
object FIRRTLMemDir {
  final case object Infer extends FIRRTLMemDir(value = c_api.FIRRTL_MEM_DIR_INFER())
  final case object Read extends FIRRTLMemDir(value = c_api.FIRRTL_MEM_DIR_READ())
  final case object Write extends FIRRTLMemDir(value = c_api.FIRRTL_MEM_DIR_WRITE())
  final case object ReadWrite extends FIRRTLMemDir(value = c_api.FIRRTL_MEM_DIR_READ_WRITE())
}
