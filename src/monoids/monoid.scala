package monoids

trait Monoid[T] {
  def neutral: T
  def op (a: T, b: T) : T
}
