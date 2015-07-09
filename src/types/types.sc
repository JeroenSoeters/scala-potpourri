class Outer {
  trait Inner
  def y = new Inner { }
  def foo(x : this.Inner) = null
  def bar(x : x#Inner) = null
}
