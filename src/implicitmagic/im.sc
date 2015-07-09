import java.security.{AccessController, PrivilegedAction}

import scala.collection.mutable.ArrayBuffer

trait Foo
trait Bar
object Foo {
  implicit def fooToBar(foo: Foo) : Bar = new Bar { }
}

def bar(x: Bar) = println("Bar")

val x = new Foo { }
bar(x)

object ScalaSecurityImplicits {
  implicit def functionToPrivilegedAction[A](func: Function0[A]) =
    new PrivilegedAction[A] {
      override def run() = func()
    }
}

import ScalaSecurityImplicits._

AccessController.doPrivileged(() => println("This is privileged"))

class Matrix(private val repr : Array[Array[Double]]) {
  def row(idx: Int) : Seq[Double] = repr(idx)

  def col(idx: Int) : Seq[Double] = {
    repr.foldLeft(ArrayBuffer[Double]()) {
      (buffer, currentRow) => buffer.append(currentRow(idx))
        buffer
    } toArray
  }

  lazy val rowRank = repr.size
  lazy val colRank = if (rowRank > 0) repr(0).size else 0

  override toString = "Matrix" + repr.foldLeft("") {
    (msg, row) => msg + row.mkString("\n|", " | ", "|")
  }
}