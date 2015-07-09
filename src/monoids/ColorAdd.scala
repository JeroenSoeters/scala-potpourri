package monoids

class ColorAdd extends Monoid[Colour] {

  override def neutral: Colour = Colour(0, 0, 0)

  override def op(a: Colour, b: Colour): Colour = Colour(
    a.r + b.r,
    a.g + b.g,
    a.b + b.b
  )
}
