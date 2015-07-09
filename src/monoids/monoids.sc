import java.io.File

import scala.concurrent.{Await, Future}

case class Colour(r: Int, g: Int, b: Int)

def addColour(c1: Colour, c2: Colour): Colour =
  new Colour(
    c1.r + c2.r,
    c1.g + c2.g,
    c1.b + c2.b
  )

def addColours(cs: List[Colour]) = {
  var res = new Colour(0, 0, 0)
  for (c <- cs) res = addColour(res, c)
  res
}
val c1 = new Colour(255, 0, 0)
val c2 = new Colour(0, 255, 0)
val c3 = new Colour(0, 0, 255)

addColours(List(c1, c2, c3))

trait Monoid[T] {
  def neutral: T
  def op (a: T, b: T) : T
}

def black = new Colour(0, 0, 0)

class colourAdd extends Monoid[Colour] {
  def neutral = black
  def op(a: Colour, b: Colour) : Colour = addColour(a, b)
}

List(c1, c2, c3, black) reduce addColour

def division(a: Int, b: Int, c: Int, d: Int) : Option[Double] =
  b match {
    case 0 => None
    case _ => {
      c match {
        case 0 => None
        case _ => {
          d match {
            case 0 => None
            case _ => Some(((a/b)/c)/d)
          }
        }
      }
    }
  }

def divide(a: Int, b: Int): Option[Int] =
  b match {
    case 0 => None
    case _ => Some(a/b)
  }

def division_fm(a: Int, b: Int, c: Int, d: Int) =
  divide(a, b) flatMap(divide(_, c)) flatMap (divide(_, d))


class OptionBuilder[T] {
  def flatMap(value: Option[T], function: T => Option[T]): Option[T] =
    value match {
      case Some(value) => function(value)
      case None => None
    }

  def unit(value: T) : Option[T] = Some(value)
}
def option = new OptionBuilder[Int]

divide(6, 2) flatMap(divide(_, 1)) get

for {
  a <- divide(6, 2)
  b <- divide(a, 1)
} yield b

def divisionM(a: Int, b: Int, c: Int, d: Int) = {
  for {
    x <- divide(a, b)
    y <- divide(x, c)
    z <- divide(y, d)
  } yield z
}

sealed trait Maybe[+A] {

  // >>=
  def flatMap[B](f: A => Maybe[B]): Maybe[B]
}

case class Just[+A](a: A) extends Maybe[A] {
  override def flatMap[B](f: A => Maybe[B]) = f(a)
}

// Nothing in the Haskel example
case object MaybeNot extends Maybe[Nothing] {
  override def flatMap[B](f: Nothing => Maybe[B]) = MaybeNot
}

def divideM(a: Int, b: Int) =
  b match {
    case 0 => MaybeNot
    case _ => Just(a/b)
  }

def divideFm(a: Int, b: Int, c: Int, d: Int) =
  divideM(a, b) flatMap (divideM(_, c)) flatMap(divideM(_, d))

def divideForComprehension(a: Int, b: Int, c: Int, d: Int) =
  for {
    x <- divideM(a, b)
    y <- divideM(x, c)
    z <- divideM(y, d)
  } yield z

def divM(a: Int, b: Int, c: Int, d: Int) =


