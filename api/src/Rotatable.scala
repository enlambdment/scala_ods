package api

import scala.language.implicitConversions

/**
 * Trait representing the ability to "rotate" a sequence a,
 * so that a(i) moves to a((i + r) mod a.length), for all 
 * i in { 0, ..., a.length - 1 }.
 *
 * @tparam A  Type of contents in the sequence.
 * @tparam S  Type of the sequence.
 */
trait Rotatable[A, S[_]] {
  val rotator: S[A]
  
  def rotate(r: Int): Unit
}

object Rotatable {
  /**
   * Why an implicit conversion? The purpose of introducing the trait `Rotatable`
   * is to make abstract sequence traits such as `api.List` extend it, indicating
   * that it is always possible (albeit inefficiently) to rotate a class implementing
   * `api.List` by means of nothing but the interface methods. To do so, it should
   * suffice to call the `rotate` method with the index of rotation, rather than
   * having to explicitly pass a parameter for the sequence. This is why the method
   * signature differs from that given for `rotate` in ODS, and also why an implicit
   * conversion is defined here: to spare the user from having to construct a new
   * object, in order to perform the rotation.
   * 
   * @param xs  Array of values to be rotated.
   * @tparam A  Type of the elements in the array.
   * @return
   */
  implicit def arrayRotatable[A](xs: Array[A]): Rotatable[A, Array] = new Rotatable[A, Array] {
    override val rotator: Array[A] = xs
    
    override def rotate(r: Int): Unit = {
      val l = rotator.length
      val rPos = Math.floorMod(r, l)
      val (front, back) = rotator.splitAt(l - rPos)
      Array.copy(back, 0, rotator, 0, back.length)
      Array.copy(front, 0, rotator, rPos, front.length)
    }
  }

  implicit def listRotatable[A](xs: api.List[A]): Rotatable[A, api.List] = new Rotatable[A, api.List] {
    override val rotator: api.List[A] = xs

    override def rotate(r: Int): Unit = {
      /*
      1. Handle negative rotation offset by converting to equivalent positive offset
      2. Determine d := greatest common divisor of rotation offset and l := array length
      3. Carry out (l / d) many "sub-rotations" to rotate the array in-place.
       */
      val l = rotator.size()
      val rPos = Math.floorMod(r, l)
      val d = MathHelper.gcd(l, rPos)
      var (i, j) = (0, 0)
      for (i0 <- Range(0, d)) {
        i = i0
        j = Math.floorMod(i0 + rPos, l)
        var x, y = rotator.get(i)
        for (_ <- Range(0, l / d)) {
          y = rotator.get(j)
          val temp = x // swap vars
          x = y
          y = temp // done swapping vars
          rotator.set(j, y)
          i = Math.floorMod(i + rPos, l)
          j = Math.floorMod(j + rPos, l)
        }
      }
    }
  }
}