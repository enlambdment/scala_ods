package api

/**
 * In Scala, values of almost any type are hashable by virtue of their
 * `hashCode` method, which is fundamentally due to the Scala type AnyRef
 * (Java type: Object). The purpose of introducing this trait is to make
 * the dependence on hashing, for certain techniques to implement data
 * structures, explicit as a type bound on the type parameter of collection
 * contents.
 *
 * To boot, the instance `anyHashable[T]`, (almost) always available for
 * any `T`, may be "overridden" in specific implementations by an alternative
 * instance based upon other means of hashing with certain desirable properties,
 * at the discretion of the implementer.
 *
 * @tparam T  Type of hashable value.
 */
trait Hashable[T] {
  def hash(x: T): Int
}

object Hashable {
  implicit def anyHashable[T <: AnyRef]: Hashable[T] = new Hashable[T] {
    override def hash(x: T) = x.hashCode()
  }
}
