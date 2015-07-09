import monoids.{ColorAdd, Colour}
import org.scalacheck.{Gen, Properties}
import org.scalacheck.Prop.forAll

object ColorAddProperties extends Properties("ColorAdd") {

  val M = new ColorAdd()
  type T = Colour
  val Z = M.neutral

  val colors: Gen[Colour] =
    for {
      r <- Gen.choose(0, 255)
      g <- Gen.choose(0, 255)
      b <- Gen.choose(0, 255)
    } yield Colour(r, g, b)

  property("Z is the neutral operator") = forAll(colors) { (v: T) =>
    Z ++ v == v && v ++ Z == v
  }

  property("op is associative") = forAll(colors, colors, colors) { (a: T, b: T, c: T) =>
    a ++ (b ++ c) == (a ++ b) ++ c
  }
}
