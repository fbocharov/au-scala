sealed trait Numeral

case object Zero extends Numeral
type Zero = Zero.type

case class Succ[P <: Numeral](prev: P) extends Numeral


// HList = HNil | Cons(T, HList)

sealed trait HList {
  def ::[H](h: H): HCons[H, this.type] = HCons(h, this)
}

object HList {
  type ::[+H, +T <: HList] = HCons[H, T]
  type HNil = HNil.type

  trait Splittable[S <: HList, N <: Numeral, L <: HList, R <: HList] {
    def apply(source: S, at: N): (L, R)
  }

  object Splittable {
    implicit def base[R <: HList]: Splittable[R, Zero, HNil, R] = new Splittable[R, Zero, HNil, R] {
      override def apply(source: R, at: Zero): (HNil, R) = (HNil, source)
    }

    implicit def step[HS, TS <: HList, N <: Numeral, L <: HList, R <: HList]
        (implicit splittable: Splittable[TS, N, L, R]): Splittable[HS :: TS, Succ[N], HS :: L, R] =
          new Splittable[HS :: TS, Succ[N], HS :: L, R] {
            override def apply(source: HS :: TS, at: Succ[N]): (HS :: L, R) = {
              val (l, r) = splittable(source.tail, at.prev)
              (source.head :: l, r)
            }
          }
  }

  def splitAt[S <: HList, N <: Numeral, L <: HList, R <: HList](source: S, at: N)(implicit splittable: Splittable[S, N, L, R]): (L, R) = {
    splittable(source, at)
  }
}

case object HNil extends HList

case class HCons[+H, +T <: HList](head: H, tail: T) extends HList

import HList._

val zero: Zero = Zero
val one = Succ(zero)
val two = Succ(one)
val three = Succ(two)
val four = Succ(three)
val five = Succ(four)
val six = Succ(five)
val seven = Succ(six)

val list1: String :: Boolean :: Int :: Double :: HNil = "hi" :: true :: 5 :: 5.0 :: HNil
splitAt(list1, two)
// splitAt(list1, six) // FAILS TO COMPILE!!!