package api

import scala.annotation.tailrec

/**
 * Object for assorted mathematical helper functions.
 */
object MathHelper {
  /**
   * Get the greatest common divisor of two positive Ints.
   *
   * @param m One integer
   * @param n Another integer
   * @return Greatest common divisor of m, n.
   */
  @tailrec
  def gcd(m: Int, n: Int): Int = {
    if (m < 0 || n < 0) {
      throw new Exception("Positive integers required!")
    }
    val remainder = Math.floorMod(m, n)
    if (remainder == 0) {
      n
    } else {
      gcd(n, remainder)
    }
  }
}
