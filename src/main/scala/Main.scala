object Main {
  def map[A, B](in: List[A], f: A => B): List[B] = in match {
    case Nil => List()
    case a :: rest => map(rest, f).+:(f(a))
  }

  def number_encoder[A](f: A => A => A)(in: List[A]): List[A] =
    map[A, A](in, x => f(x)(x))

  def main(args: Array[String]): Unit = {
    val int_encoder: List[Int] => List[Int] = number_encoder[Int] {
      case a if a % 3 == 2 => x => x * 7
      case a if a % 3 == 1 => x => x * 8
      case _ => x => x
    }
    println(int_encoder(List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)))
  }
}
