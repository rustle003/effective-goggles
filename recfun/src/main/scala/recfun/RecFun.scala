package recfun

object RecFun extends RecFunInterface:

  def main(args: Array[String]): Unit =
    println("Pascal's Triangle")
    for row <- 0 to 10 do
      for col <- 0 to row do
        print(s"${pascal(col, row)} ")
      println()

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int =
    import scala.annotation.tailrec

    val start = 1
    val startL = List[Int](start)

    @tailrec def _pascal(pns: List[Int], nns: List[Int] = List[Int]()): List[Int] = (pns, nns) match
      case (_, List())  => _pascal(pns, nns :+ pns.head)
      case (List(h), _) => nns :+ h
      case _            => _pascal(pns.tail, nns :+ (pns.head + pns.tail.head))
    
    @tailrec def findPasc(init: List[Int], row: Int): Int = row match
      case 0 => init(c)
      case n => findPasc(_pascal(init), row - 1)
    
    findPasc(startL, r)
  end pascal

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean =
    import scala.annotation.tailrec
    
    @tailrec def open(cs: List[Char], pcount: Int = 0): Boolean = cs match
      case List()        => true
      case List(')', _*) => false
      case List('(', _*) => close(cs.tail, pcount)
      case _             => open(cs.tail, pcount)

    @tailrec def close(cs: List[Char], pcount: Int): Boolean = cs match
      case List() => false
      case List(')', _*) => if pcount == 0 then open(cs.tail) else close(cs.tail, pcount - 1)
      case List('(', _*) => close(cs.tail, pcount + 1)
      case _             => close(cs.tail, pcount)

    open(chars)
  end balance

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int =
    money
