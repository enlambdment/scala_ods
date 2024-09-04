package api

/**
 * Trait representing the ability to "rotate" a sequence
 * so that a(i) moves to a((i + r) mod a.length),
 * for all i in { 0, ..., a.length - 1 }.
 *
 *
 * @tparam A  Type of contents in the sequence.
 * @tparam S  Type of the sequence.
 */
trait Rotatable[A, S[_]] {
  def rotate(xs: S[A], r: Int): Unit
}

object Rotatable {
  implicit def arrayRotatable[A]: Rotatable[A, Array] = new Rotatable[A, Array] {
    override def rotate(xs: Array[A], r: Int): Unit = {
      /*
      1. Handle negative rotation offset by converting to equivalent positive offset
      2. Determine d := greatest common divisor of rotation offset and l := array length
      3. Carry out (l / d) many "sub-rotations" to rotate the array in-place.
       */
      val l = xs.length
      val rPos = Math.floorMod(r, l)
      val d = MathHelper.gcd(l, rPos)
      var (i, j) = (0, 0)
      for (i0 <- Range(0, d)) {
        i = i0
        j = Math.floorMod(i0 + rPos, l)
        var x, y = xs(i)
        for (_ <- Range(0, l / d)) {
          y = xs(j)
          val temp = x  // swap vars
          x = y
          y = temp      // done swapping vars
          xs(j) = y
          i = Math.floorMod(i + rPos, l)
          j = Math.floorMod(j + rPos, l)
        }
      }
    }
  }
}