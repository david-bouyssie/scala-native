// format: off

// BEWARE: This file is generated - direct edits will be lost.
// Do not edit this it directly other than to remove
// personally identifiable information in sourceLocation lines.
// All direct edits to this file will be lost the next time it
// is generated.
//
// See nativelib runtime/Arrays.scala.gyb for details.

package scala.scalanative
package unsafe

import scala.language.implicitConversions
import scala.scalanative.annotation.alwaysinline
import scala.scalanative.runtime.Boxes._
import scala.scalanative.runtime.{RawPtr, intrinsic}

sealed abstract class CFuncPtr private[unsafe] (private[scalanative] val rawptr: RawPtr)

object CFuncPtr {
  @alwaysinline def fromPtr[F <: CFuncPtr](ptr: Ptr[Byte])(implicit tag: Tag.CFuncPtrTag[F]): F =
    tag.fromRawPtr(ptr.rawptr)

  @alwaysinline def toPtr(ptr: CFuncPtr): Ptr[Byte] = {
    boxToPtr[Byte](ptr.rawptr)
  }
}

final class CFuncPtr0[R] private (rawptr: RawPtr) extends CFuncPtr(rawptr) {
  def apply()(implicit evRet: Tag[R]): R = intrinsic
}

object CFuncPtr0 {
  implicit def fromScalaFunction[R](fn: Function0[R])(implicit evRet: Tag[R]): CFuncPtr0[R] = intrinsic

  private[scalanative] def fromRawPtr[R](ptr: RawPtr): CFuncPtr0[R] = {
    new CFuncPtr0[R](ptr)
  }
}

final class CFuncPtr1[T1, R] private (rawptr: RawPtr) extends CFuncPtr(rawptr) {
  def apply(arg1: T1)(implicit ev1: Tag[T1], evRet: Tag[R]): R = intrinsic
}

object CFuncPtr1 {
  implicit def fromScalaFunction[T1, R](fn: Function1[T1, R])(implicit ev1: Tag[T1], evRet: Tag[R]): CFuncPtr1[T1, R] = intrinsic

  private[scalanative] def fromRawPtr[T1, R](ptr: RawPtr): CFuncPtr1[T1, R] = {
    new CFuncPtr1[T1, R](ptr)
  }
}

final class CFuncPtr2[T1, T2, R] private (rawptr: RawPtr) extends CFuncPtr(rawptr) {
  def apply(arg1: T1, arg2: T2)(implicit ev1: Tag[T1], ev2: Tag[T2], evRet: Tag[R]): R = intrinsic
}

object CFuncPtr2 {
  implicit def fromScalaFunction[T1, T2, R](fn: Function2[T1, T2, R])(implicit ev1: Tag[T1], ev2: Tag[T2], evRet: Tag[R]): CFuncPtr2[T1, T2, R] = intrinsic

  private[scalanative] def fromRawPtr[T1, T2, R](ptr: RawPtr): CFuncPtr2[T1, T2, R] = {
    new CFuncPtr2[T1, T2, R](ptr)
  }
}

final class CFuncPtr3[T1, T2, T3, R] private (rawptr: RawPtr) extends CFuncPtr(rawptr) {
  def apply(arg1: T1, arg2: T2, arg3: T3)(implicit ev1: Tag[T1], ev2: Tag[T2], ev3: Tag[T3], evRet: Tag[R]): R = intrinsic
}

object CFuncPtr3 {
  implicit def fromScalaFunction[T1, T2, T3, R](fn: Function3[T1, T2, T3, R])(implicit ev1: Tag[T1], ev2: Tag[T2], ev3: Tag[T3], evRet: Tag[R]): CFuncPtr3[T1, T2, T3, R] = intrinsic

  private[scalanative] def fromRawPtr[T1, T2, T3, R](ptr: RawPtr): CFuncPtr3[T1, T2, T3, R] = {
    new CFuncPtr3[T1, T2, T3, R](ptr)
  }
}

final class CFuncPtr4[T1, T2, T3, T4, R] private (rawptr: RawPtr) extends CFuncPtr(rawptr) {
  def apply(arg1: T1, arg2: T2, arg3: T3, arg4: T4)(implicit ev1: Tag[T1], ev2: Tag[T2], ev3: Tag[T3], ev4: Tag[T4], evRet: Tag[R]): R = intrinsic
}

object CFuncPtr4 {
  implicit def fromScalaFunction[T1, T2, T3, T4, R](fn: Function4[T1, T2, T3, T4, R])(implicit ev1: Tag[T1], ev2: Tag[T2], ev3: Tag[T3], ev4: Tag[T4], evRet: Tag[R]): CFuncPtr4[T1, T2, T3, T4, R] = intrinsic

  private[scalanative] def fromRawPtr[T1, T2, T3, T4, R](ptr: RawPtr): CFuncPtr4[T1, T2, T3, T4, R] = {
    new CFuncPtr4[T1, T2, T3, T4, R](ptr)
  }
}

final class CFuncPtr5[T1, T2, T3, T4, T5, R] private (rawptr: RawPtr) extends CFuncPtr(rawptr) {
  def apply(arg1: T1, arg2: T2, arg3: T3, arg4: T4, arg5: T5)(implicit ev1: Tag[T1], ev2: Tag[T2], ev3: Tag[T3], ev4: Tag[T4], ev5: Tag[T5], evRet: Tag[R]): R = intrinsic
}

object CFuncPtr5 {
  implicit def fromScalaFunction[T1, T2, T3, T4, T5, R](fn: Function5[T1, T2, T3, T4, T5, R])(implicit ev1: Tag[T1], ev2: Tag[T2], ev3: Tag[T3], ev4: Tag[T4], ev5: Tag[T5], evRet: Tag[R]): CFuncPtr5[T1, T2, T3, T4, T5, R] = intrinsic

  private[scalanative] def fromRawPtr[T1, T2, T3, T4, T5, R](ptr: RawPtr): CFuncPtr5[T1, T2, T3, T4, T5, R] = {
    new CFuncPtr5[T1, T2, T3, T4, T5, R](ptr)
  }
}

final class CFuncPtr6[T1, T2, T3, T4, T5, T6, R] private (rawptr: RawPtr) extends CFuncPtr(rawptr) {
  def apply(arg1: T1, arg2: T2, arg3: T3, arg4: T4, arg5: T5, arg6: T6)(implicit ev1: Tag[T1], ev2: Tag[T2], ev3: Tag[T3], ev4: Tag[T4], ev5: Tag[T5], ev6: Tag[T6], evRet: Tag[R]): R = intrinsic
}

object CFuncPtr6 {
  implicit def fromScalaFunction[T1, T2, T3, T4, T5, T6, R](fn: Function6[T1, T2, T3, T4, T5, T6, R])(implicit ev1: Tag[T1], ev2: Tag[T2], ev3: Tag[T3], ev4: Tag[T4], ev5: Tag[T5], ev6: Tag[T6], evRet: Tag[R]): CFuncPtr6[T1, T2, T3, T4, T5, T6, R] = intrinsic

  private[scalanative] def fromRawPtr[T1, T2, T3, T4, T5, T6, R](ptr: RawPtr): CFuncPtr6[T1, T2, T3, T4, T5, T6, R] = {
    new CFuncPtr6[T1, T2, T3, T4, T5, T6, R](ptr)
  }
}

final class CFuncPtr7[T1, T2, T3, T4, T5, T6, T7, R] private (rawptr: RawPtr) extends CFuncPtr(rawptr) {
  def apply(arg1: T1, arg2: T2, arg3: T3, arg4: T4, arg5: T5, arg6: T6, arg7: T7)(implicit ev1: Tag[T1], ev2: Tag[T2], ev3: Tag[T3], ev4: Tag[T4], ev5: Tag[T5], ev6: Tag[T6], ev7: Tag[T7], evRet: Tag[R]): R = intrinsic
}

object CFuncPtr7 {
  implicit def fromScalaFunction[T1, T2, T3, T4, T5, T6, T7, R](fn: Function7[T1, T2, T3, T4, T5, T6, T7, R])(implicit ev1: Tag[T1], ev2: Tag[T2], ev3: Tag[T3], ev4: Tag[T4], ev5: Tag[T5], ev6: Tag[T6], ev7: Tag[T7], evRet: Tag[R]): CFuncPtr7[T1, T2, T3, T4, T5, T6, T7, R] = intrinsic

  private[scalanative] def fromRawPtr[T1, T2, T3, T4, T5, T6, T7, R](ptr: RawPtr): CFuncPtr7[T1, T2, T3, T4, T5, T6, T7, R] = {
    new CFuncPtr7[T1, T2, T3, T4, T5, T6, T7, R](ptr)
  }
}

final class CFuncPtr8[T1, T2, T3, T4, T5, T6, T7, T8, R] private (rawptr: RawPtr) extends CFuncPtr(rawptr) {
  def apply(arg1: T1, arg2: T2, arg3: T3, arg4: T4, arg5: T5, arg6: T6, arg7: T7, arg8: T8)(implicit ev1: Tag[T1], ev2: Tag[T2], ev3: Tag[T3], ev4: Tag[T4], ev5: Tag[T5], ev6: Tag[T6], ev7: Tag[T7], ev8: Tag[T8], evRet: Tag[R]): R = intrinsic
}

object CFuncPtr8 {
  implicit def fromScalaFunction[T1, T2, T3, T4, T5, T6, T7, T8, R](fn: Function8[T1, T2, T3, T4, T5, T6, T7, T8, R])(implicit ev1: Tag[T1], ev2: Tag[T2], ev3: Tag[T3], ev4: Tag[T4], ev5: Tag[T5], ev6: Tag[T6], ev7: Tag[T7], ev8: Tag[T8], evRet: Tag[R]): CFuncPtr8[T1, T2, T3, T4, T5, T6, T7, T8, R] = intrinsic

  private[scalanative] def fromRawPtr[T1, T2, T3, T4, T5, T6, T7, T8, R](ptr: RawPtr): CFuncPtr8[T1, T2, T3, T4, T5, T6, T7, T8, R] = {
    new CFuncPtr8[T1, T2, T3, T4, T5, T6, T7, T8, R](ptr)
  }
}

final class CFuncPtr9[T1, T2, T3, T4, T5, T6, T7, T8, T9, R] private (rawptr: RawPtr) extends CFuncPtr(rawptr) {
  def apply(arg1: T1, arg2: T2, arg3: T3, arg4: T4, arg5: T5, arg6: T6, arg7: T7, arg8: T8, arg9: T9)(implicit ev1: Tag[T1], ev2: Tag[T2], ev3: Tag[T3], ev4: Tag[T4], ev5: Tag[T5], ev6: Tag[T6], ev7: Tag[T7], ev8: Tag[T8], ev9: Tag[T9], evRet: Tag[R]): R = intrinsic
}

object CFuncPtr9 {
  implicit def fromScalaFunction[T1, T2, T3, T4, T5, T6, T7, T8, T9, R](fn: Function9[T1, T2, T3, T4, T5, T6, T7, T8, T9, R])(implicit ev1: Tag[T1], ev2: Tag[T2], ev3: Tag[T3], ev4: Tag[T4], ev5: Tag[T5], ev6: Tag[T6], ev7: Tag[T7], ev8: Tag[T8], ev9: Tag[T9], evRet: Tag[R]): CFuncPtr9[T1, T2, T3, T4, T5, T6, T7, T8, T9, R] = intrinsic

  private[scalanative] def fromRawPtr[T1, T2, T3, T4, T5, T6, T7, T8, T9, R](ptr: RawPtr): CFuncPtr9[T1, T2, T3, T4, T5, T6, T7, T8, T9, R] = {
    new CFuncPtr9[T1, T2, T3, T4, T5, T6, T7, T8, T9, R](ptr)
  }
}

final class CFuncPtr10[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, R] private (rawptr: RawPtr) extends CFuncPtr(rawptr) {
  def apply(arg1: T1, arg2: T2, arg3: T3, arg4: T4, arg5: T5, arg6: T6, arg7: T7, arg8: T8, arg9: T9, arg10: T10)(implicit ev1: Tag[T1], ev2: Tag[T2], ev3: Tag[T3], ev4: Tag[T4], ev5: Tag[T5], ev6: Tag[T6], ev7: Tag[T7], ev8: Tag[T8], ev9: Tag[T9], ev10: Tag[T10], evRet: Tag[R]): R = intrinsic
}

object CFuncPtr10 {
  implicit def fromScalaFunction[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, R](fn: Function10[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, R])(implicit ev1: Tag[T1], ev2: Tag[T2], ev3: Tag[T3], ev4: Tag[T4], ev5: Tag[T5], ev6: Tag[T6], ev7: Tag[T7], ev8: Tag[T8], ev9: Tag[T9], ev10: Tag[T10], evRet: Tag[R]): CFuncPtr10[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, R] = intrinsic

  private[scalanative] def fromRawPtr[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, R](ptr: RawPtr): CFuncPtr10[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, R] = {
    new CFuncPtr10[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, R](ptr)
  }
}

final class CFuncPtr11[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, R] private (rawptr: RawPtr) extends CFuncPtr(rawptr) {
  def apply(arg1: T1, arg2: T2, arg3: T3, arg4: T4, arg5: T5, arg6: T6, arg7: T7, arg8: T8, arg9: T9, arg10: T10, arg11: T11)(implicit ev1: Tag[T1], ev2: Tag[T2], ev3: Tag[T3], ev4: Tag[T4], ev5: Tag[T5], ev6: Tag[T6], ev7: Tag[T7], ev8: Tag[T8], ev9: Tag[T9], ev10: Tag[T10], ev11: Tag[T11], evRet: Tag[R]): R = intrinsic
}

object CFuncPtr11 {
  implicit def fromScalaFunction[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, R](fn: Function11[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, R])(implicit ev1: Tag[T1], ev2: Tag[T2], ev3: Tag[T3], ev4: Tag[T4], ev5: Tag[T5], ev6: Tag[T6], ev7: Tag[T7], ev8: Tag[T8], ev9: Tag[T9], ev10: Tag[T10], ev11: Tag[T11], evRet: Tag[R]): CFuncPtr11[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, R] = intrinsic

  private[scalanative] def fromRawPtr[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, R](ptr: RawPtr): CFuncPtr11[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, R] = {
    new CFuncPtr11[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, R](ptr)
  }
}

final class CFuncPtr12[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, R] private (rawptr: RawPtr) extends CFuncPtr(rawptr) {
  def apply(arg1: T1, arg2: T2, arg3: T3, arg4: T4, arg5: T5, arg6: T6, arg7: T7, arg8: T8, arg9: T9, arg10: T10, arg11: T11, arg12: T12)(implicit ev1: Tag[T1], ev2: Tag[T2], ev3: Tag[T3], ev4: Tag[T4], ev5: Tag[T5], ev6: Tag[T6], ev7: Tag[T7], ev8: Tag[T8], ev9: Tag[T9], ev10: Tag[T10], ev11: Tag[T11], ev12: Tag[T12], evRet: Tag[R]): R = intrinsic
}

object CFuncPtr12 {
  implicit def fromScalaFunction[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, R](fn: Function12[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, R])(implicit ev1: Tag[T1], ev2: Tag[T2], ev3: Tag[T3], ev4: Tag[T4], ev5: Tag[T5], ev6: Tag[T6], ev7: Tag[T7], ev8: Tag[T8], ev9: Tag[T9], ev10: Tag[T10], ev11: Tag[T11], ev12: Tag[T12], evRet: Tag[R]): CFuncPtr12[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, R] = intrinsic

  private[scalanative] def fromRawPtr[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, R](ptr: RawPtr): CFuncPtr12[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, R] = {
    new CFuncPtr12[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, R](ptr)
  }
}

final class CFuncPtr13[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, R] private (rawptr: RawPtr) extends CFuncPtr(rawptr) {
  def apply(arg1: T1, arg2: T2, arg3: T3, arg4: T4, arg5: T5, arg6: T6, arg7: T7, arg8: T8, arg9: T9, arg10: T10, arg11: T11, arg12: T12, arg13: T13)(implicit ev1: Tag[T1], ev2: Tag[T2], ev3: Tag[T3], ev4: Tag[T4], ev5: Tag[T5], ev6: Tag[T6], ev7: Tag[T7], ev8: Tag[T8], ev9: Tag[T9], ev10: Tag[T10], ev11: Tag[T11], ev12: Tag[T12], ev13: Tag[T13], evRet: Tag[R]): R = intrinsic
}

object CFuncPtr13 {
  implicit def fromScalaFunction[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, R](fn: Function13[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, R])(implicit ev1: Tag[T1], ev2: Tag[T2], ev3: Tag[T3], ev4: Tag[T4], ev5: Tag[T5], ev6: Tag[T6], ev7: Tag[T7], ev8: Tag[T8], ev9: Tag[T9], ev10: Tag[T10], ev11: Tag[T11], ev12: Tag[T12], ev13: Tag[T13], evRet: Tag[R]): CFuncPtr13[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, R] = intrinsic

  private[scalanative] def fromRawPtr[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, R](ptr: RawPtr): CFuncPtr13[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, R] = {
    new CFuncPtr13[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, R](ptr)
  }
}

final class CFuncPtr14[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, R] private (rawptr: RawPtr) extends CFuncPtr(rawptr) {
  def apply(arg1: T1, arg2: T2, arg3: T3, arg4: T4, arg5: T5, arg6: T6, arg7: T7, arg8: T8, arg9: T9, arg10: T10, arg11: T11, arg12: T12, arg13: T13, arg14: T14)(implicit ev1: Tag[T1], ev2: Tag[T2], ev3: Tag[T3], ev4: Tag[T4], ev5: Tag[T5], ev6: Tag[T6], ev7: Tag[T7], ev8: Tag[T8], ev9: Tag[T9], ev10: Tag[T10], ev11: Tag[T11], ev12: Tag[T12], ev13: Tag[T13], ev14: Tag[T14], evRet: Tag[R]): R = intrinsic
}

object CFuncPtr14 {
  implicit def fromScalaFunction[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, R](fn: Function14[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, R])(implicit ev1: Tag[T1], ev2: Tag[T2], ev3: Tag[T3], ev4: Tag[T4], ev5: Tag[T5], ev6: Tag[T6], ev7: Tag[T7], ev8: Tag[T8], ev9: Tag[T9], ev10: Tag[T10], ev11: Tag[T11], ev12: Tag[T12], ev13: Tag[T13], ev14: Tag[T14], evRet: Tag[R]): CFuncPtr14[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, R] = intrinsic

  private[scalanative] def fromRawPtr[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, R](ptr: RawPtr): CFuncPtr14[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, R] = {
    new CFuncPtr14[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, R](ptr)
  }
}

final class CFuncPtr15[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, R] private (rawptr: RawPtr) extends CFuncPtr(rawptr) {
  def apply(arg1: T1, arg2: T2, arg3: T3, arg4: T4, arg5: T5, arg6: T6, arg7: T7, arg8: T8, arg9: T9, arg10: T10, arg11: T11, arg12: T12, arg13: T13, arg14: T14, arg15: T15)(implicit ev1: Tag[T1], ev2: Tag[T2], ev3: Tag[T3], ev4: Tag[T4], ev5: Tag[T5], ev6: Tag[T6], ev7: Tag[T7], ev8: Tag[T8], ev9: Tag[T9], ev10: Tag[T10], ev11: Tag[T11], ev12: Tag[T12], ev13: Tag[T13], ev14: Tag[T14], ev15: Tag[T15], evRet: Tag[R]): R = intrinsic
}

object CFuncPtr15 {
  implicit def fromScalaFunction[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, R](fn: Function15[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, R])(implicit ev1: Tag[T1], ev2: Tag[T2], ev3: Tag[T3], ev4: Tag[T4], ev5: Tag[T5], ev6: Tag[T6], ev7: Tag[T7], ev8: Tag[T8], ev9: Tag[T9], ev10: Tag[T10], ev11: Tag[T11], ev12: Tag[T12], ev13: Tag[T13], ev14: Tag[T14], ev15: Tag[T15], evRet: Tag[R]): CFuncPtr15[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, R] = intrinsic

  private[scalanative] def fromRawPtr[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, R](ptr: RawPtr): CFuncPtr15[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, R] = {
    new CFuncPtr15[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, R](ptr)
  }
}

final class CFuncPtr16[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, R] private (rawptr: RawPtr) extends CFuncPtr(rawptr) {
  def apply(arg1: T1, arg2: T2, arg3: T3, arg4: T4, arg5: T5, arg6: T6, arg7: T7, arg8: T8, arg9: T9, arg10: T10, arg11: T11, arg12: T12, arg13: T13, arg14: T14, arg15: T15, arg16: T16)(implicit ev1: Tag[T1], ev2: Tag[T2], ev3: Tag[T3], ev4: Tag[T4], ev5: Tag[T5], ev6: Tag[T6], ev7: Tag[T7], ev8: Tag[T8], ev9: Tag[T9], ev10: Tag[T10], ev11: Tag[T11], ev12: Tag[T12], ev13: Tag[T13], ev14: Tag[T14], ev15: Tag[T15], ev16: Tag[T16], evRet: Tag[R]): R = intrinsic
}

object CFuncPtr16 {
  implicit def fromScalaFunction[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, R](fn: Function16[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, R])(implicit ev1: Tag[T1], ev2: Tag[T2], ev3: Tag[T3], ev4: Tag[T4], ev5: Tag[T5], ev6: Tag[T6], ev7: Tag[T7], ev8: Tag[T8], ev9: Tag[T9], ev10: Tag[T10], ev11: Tag[T11], ev12: Tag[T12], ev13: Tag[T13], ev14: Tag[T14], ev15: Tag[T15], ev16: Tag[T16], evRet: Tag[R]): CFuncPtr16[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, R] = intrinsic

  private[scalanative] def fromRawPtr[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, R](ptr: RawPtr): CFuncPtr16[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, R] = {
    new CFuncPtr16[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, R](ptr)
  }
}

final class CFuncPtr17[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, R] private (rawptr: RawPtr) extends CFuncPtr(rawptr) {
  def apply(arg1: T1, arg2: T2, arg3: T3, arg4: T4, arg5: T5, arg6: T6, arg7: T7, arg8: T8, arg9: T9, arg10: T10, arg11: T11, arg12: T12, arg13: T13, arg14: T14, arg15: T15, arg16: T16, arg17: T17)(implicit ev1: Tag[T1], ev2: Tag[T2], ev3: Tag[T3], ev4: Tag[T4], ev5: Tag[T5], ev6: Tag[T6], ev7: Tag[T7], ev8: Tag[T8], ev9: Tag[T9], ev10: Tag[T10], ev11: Tag[T11], ev12: Tag[T12], ev13: Tag[T13], ev14: Tag[T14], ev15: Tag[T15], ev16: Tag[T16], ev17: Tag[T17], evRet: Tag[R]): R = intrinsic
}

object CFuncPtr17 {
  implicit def fromScalaFunction[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, R](fn: Function17[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, R])(implicit ev1: Tag[T1], ev2: Tag[T2], ev3: Tag[T3], ev4: Tag[T4], ev5: Tag[T5], ev6: Tag[T6], ev7: Tag[T7], ev8: Tag[T8], ev9: Tag[T9], ev10: Tag[T10], ev11: Tag[T11], ev12: Tag[T12], ev13: Tag[T13], ev14: Tag[T14], ev15: Tag[T15], ev16: Tag[T16], ev17: Tag[T17], evRet: Tag[R]): CFuncPtr17[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, R] = intrinsic

  private[scalanative] def fromRawPtr[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, R](ptr: RawPtr): CFuncPtr17[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, R] = {
    new CFuncPtr17[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, R](ptr)
  }
}

final class CFuncPtr18[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, R] private (rawptr: RawPtr) extends CFuncPtr(rawptr) {
  def apply(arg1: T1, arg2: T2, arg3: T3, arg4: T4, arg5: T5, arg6: T6, arg7: T7, arg8: T8, arg9: T9, arg10: T10, arg11: T11, arg12: T12, arg13: T13, arg14: T14, arg15: T15, arg16: T16, arg17: T17, arg18: T18)(implicit ev1: Tag[T1], ev2: Tag[T2], ev3: Tag[T3], ev4: Tag[T4], ev5: Tag[T5], ev6: Tag[T6], ev7: Tag[T7], ev8: Tag[T8], ev9: Tag[T9], ev10: Tag[T10], ev11: Tag[T11], ev12: Tag[T12], ev13: Tag[T13], ev14: Tag[T14], ev15: Tag[T15], ev16: Tag[T16], ev17: Tag[T17], ev18: Tag[T18], evRet: Tag[R]): R = intrinsic
}

object CFuncPtr18 {
  implicit def fromScalaFunction[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, R](fn: Function18[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, R])(implicit ev1: Tag[T1], ev2: Tag[T2], ev3: Tag[T3], ev4: Tag[T4], ev5: Tag[T5], ev6: Tag[T6], ev7: Tag[T7], ev8: Tag[T8], ev9: Tag[T9], ev10: Tag[T10], ev11: Tag[T11], ev12: Tag[T12], ev13: Tag[T13], ev14: Tag[T14], ev15: Tag[T15], ev16: Tag[T16], ev17: Tag[T17], ev18: Tag[T18], evRet: Tag[R]): CFuncPtr18[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, R] = intrinsic

  private[scalanative] def fromRawPtr[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, R](ptr: RawPtr): CFuncPtr18[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, R] = {
    new CFuncPtr18[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, R](ptr)
  }
}

final class CFuncPtr19[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, R] private (rawptr: RawPtr) extends CFuncPtr(rawptr) {
  def apply(arg1: T1, arg2: T2, arg3: T3, arg4: T4, arg5: T5, arg6: T6, arg7: T7, arg8: T8, arg9: T9, arg10: T10, arg11: T11, arg12: T12, arg13: T13, arg14: T14, arg15: T15, arg16: T16, arg17: T17, arg18: T18, arg19: T19)(implicit ev1: Tag[T1], ev2: Tag[T2], ev3: Tag[T3], ev4: Tag[T4], ev5: Tag[T5], ev6: Tag[T6], ev7: Tag[T7], ev8: Tag[T8], ev9: Tag[T9], ev10: Tag[T10], ev11: Tag[T11], ev12: Tag[T12], ev13: Tag[T13], ev14: Tag[T14], ev15: Tag[T15], ev16: Tag[T16], ev17: Tag[T17], ev18: Tag[T18], ev19: Tag[T19], evRet: Tag[R]): R = intrinsic
}

object CFuncPtr19 {
  implicit def fromScalaFunction[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, R](fn: Function19[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, R])(implicit ev1: Tag[T1], ev2: Tag[T2], ev3: Tag[T3], ev4: Tag[T4], ev5: Tag[T5], ev6: Tag[T6], ev7: Tag[T7], ev8: Tag[T8], ev9: Tag[T9], ev10: Tag[T10], ev11: Tag[T11], ev12: Tag[T12], ev13: Tag[T13], ev14: Tag[T14], ev15: Tag[T15], ev16: Tag[T16], ev17: Tag[T17], ev18: Tag[T18], ev19: Tag[T19], evRet: Tag[R]): CFuncPtr19[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, R] = intrinsic

  private[scalanative] def fromRawPtr[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, R](ptr: RawPtr): CFuncPtr19[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, R] = {
    new CFuncPtr19[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, R](ptr)
  }
}

final class CFuncPtr20[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, R] private (rawptr: RawPtr) extends CFuncPtr(rawptr) {
  def apply(arg1: T1, arg2: T2, arg3: T3, arg4: T4, arg5: T5, arg6: T6, arg7: T7, arg8: T8, arg9: T9, arg10: T10, arg11: T11, arg12: T12, arg13: T13, arg14: T14, arg15: T15, arg16: T16, arg17: T17, arg18: T18, arg19: T19, arg20: T20)(implicit ev1: Tag[T1], ev2: Tag[T2], ev3: Tag[T3], ev4: Tag[T4], ev5: Tag[T5], ev6: Tag[T6], ev7: Tag[T7], ev8: Tag[T8], ev9: Tag[T9], ev10: Tag[T10], ev11: Tag[T11], ev12: Tag[T12], ev13: Tag[T13], ev14: Tag[T14], ev15: Tag[T15], ev16: Tag[T16], ev17: Tag[T17], ev18: Tag[T18], ev19: Tag[T19], ev20: Tag[T20], evRet: Tag[R]): R = intrinsic
}

object CFuncPtr20 {
  implicit def fromScalaFunction[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, R](fn: Function20[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, R])(implicit ev1: Tag[T1], ev2: Tag[T2], ev3: Tag[T3], ev4: Tag[T4], ev5: Tag[T5], ev6: Tag[T6], ev7: Tag[T7], ev8: Tag[T8], ev9: Tag[T9], ev10: Tag[T10], ev11: Tag[T11], ev12: Tag[T12], ev13: Tag[T13], ev14: Tag[T14], ev15: Tag[T15], ev16: Tag[T16], ev17: Tag[T17], ev18: Tag[T18], ev19: Tag[T19], ev20: Tag[T20], evRet: Tag[R]): CFuncPtr20[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, R] = intrinsic

  private[scalanative] def fromRawPtr[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, R](ptr: RawPtr): CFuncPtr20[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, R] = {
    new CFuncPtr20[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, R](ptr)
  }
}

final class CFuncPtr21[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, R] private (rawptr: RawPtr) extends CFuncPtr(rawptr) {
  def apply(arg1: T1, arg2: T2, arg3: T3, arg4: T4, arg5: T5, arg6: T6, arg7: T7, arg8: T8, arg9: T9, arg10: T10, arg11: T11, arg12: T12, arg13: T13, arg14: T14, arg15: T15, arg16: T16, arg17: T17, arg18: T18, arg19: T19, arg20: T20, arg21: T21)(implicit ev1: Tag[T1], ev2: Tag[T2], ev3: Tag[T3], ev4: Tag[T4], ev5: Tag[T5], ev6: Tag[T6], ev7: Tag[T7], ev8: Tag[T8], ev9: Tag[T9], ev10: Tag[T10], ev11: Tag[T11], ev12: Tag[T12], ev13: Tag[T13], ev14: Tag[T14], ev15: Tag[T15], ev16: Tag[T16], ev17: Tag[T17], ev18: Tag[T18], ev19: Tag[T19], ev20: Tag[T20], ev21: Tag[T21], evRet: Tag[R]): R = intrinsic
}

object CFuncPtr21 {
  implicit def fromScalaFunction[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, R](fn: Function21[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, R])(implicit ev1: Tag[T1], ev2: Tag[T2], ev3: Tag[T3], ev4: Tag[T4], ev5: Tag[T5], ev6: Tag[T6], ev7: Tag[T7], ev8: Tag[T8], ev9: Tag[T9], ev10: Tag[T10], ev11: Tag[T11], ev12: Tag[T12], ev13: Tag[T13], ev14: Tag[T14], ev15: Tag[T15], ev16: Tag[T16], ev17: Tag[T17], ev18: Tag[T18], ev19: Tag[T19], ev20: Tag[T20], ev21: Tag[T21], evRet: Tag[R]): CFuncPtr21[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, R] = intrinsic

  private[scalanative] def fromRawPtr[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, R](ptr: RawPtr): CFuncPtr21[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, R] = {
    new CFuncPtr21[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, R](ptr)
  }
}

final class CFuncPtr22[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22, R] private (rawptr: RawPtr) extends CFuncPtr(rawptr) {
  def apply(arg1: T1, arg2: T2, arg3: T3, arg4: T4, arg5: T5, arg6: T6, arg7: T7, arg8: T8, arg9: T9, arg10: T10, arg11: T11, arg12: T12, arg13: T13, arg14: T14, arg15: T15, arg16: T16, arg17: T17, arg18: T18, arg19: T19, arg20: T20, arg21: T21, arg22: T22)(implicit ev1: Tag[T1], ev2: Tag[T2], ev3: Tag[T3], ev4: Tag[T4], ev5: Tag[T5], ev6: Tag[T6], ev7: Tag[T7], ev8: Tag[T8], ev9: Tag[T9], ev10: Tag[T10], ev11: Tag[T11], ev12: Tag[T12], ev13: Tag[T13], ev14: Tag[T14], ev15: Tag[T15], ev16: Tag[T16], ev17: Tag[T17], ev18: Tag[T18], ev19: Tag[T19], ev20: Tag[T20], ev21: Tag[T21], ev22: Tag[T22], evRet: Tag[R]): R = intrinsic
}

object CFuncPtr22 {
  implicit def fromScalaFunction[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22, R](fn: Function22[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22, R])(implicit ev1: Tag[T1], ev2: Tag[T2], ev3: Tag[T3], ev4: Tag[T4], ev5: Tag[T5], ev6: Tag[T6], ev7: Tag[T7], ev8: Tag[T8], ev9: Tag[T9], ev10: Tag[T10], ev11: Tag[T11], ev12: Tag[T12], ev13: Tag[T13], ev14: Tag[T14], ev15: Tag[T15], ev16: Tag[T16], ev17: Tag[T17], ev18: Tag[T18], ev19: Tag[T19], ev20: Tag[T20], ev21: Tag[T21], ev22: Tag[T22], evRet: Tag[R]): CFuncPtr22[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22, R] = intrinsic

  private[scalanative] def fromRawPtr[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22, R](ptr: RawPtr): CFuncPtr22[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22, R] = {
    new CFuncPtr22[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22, R](ptr)
  }
}
