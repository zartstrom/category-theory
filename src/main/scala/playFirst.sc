

type Object

case class Arrow(dom: Object, cod: Object) {
  def o(that: Arrow): Arrow = {
    Arrow(that.dom, this.cod)
  }
  def *(that: Arrow): Arrow = {
    Arrow(that.dom, this.cod)
  }
}


case class Category(objects: Set[Object], arrows: Set[Arrow]) {
  def arrowsFromTo(a: Object, b: Object): Set[Arrow] = {
    arrows.filter(f => f.dom == a && f.cod == b)
  }
}


def isEpic(f: Arrow, C: Category): Boolean = {
  (for {
    c <- C.objects
    g1 <- C.arrowsFromTo(f.cod, c)
    g2 <- C.arrowsFromTo(f.cod, c)
    if g1 * f == g2 * f
  } yield {
    g1 == g2
  }).forall(_ == true)
}
