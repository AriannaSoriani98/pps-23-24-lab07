package ex3

object Solitaire:
  type Cell =(Int,Int)
  type Solution = Iterable[Cell]
  type IterableFactory = Solution => Iterable[Solution]
  given IterableFactory = LazyList(_)

  val w=6
  val h=6

  def isValid(curr: Cell, next: Cell): Boolean =
    val x = (curr._1 - next._1).abs
    val y = (curr._2 - next._2).abs
    (x == 0 && y == 3) || (x == 6 && y == 3) || (x == 3 && y == 0) || (x == 3 && y == 6) || (x == 1 && y == 1) || (x == 5 && y == 5)
    || (x == 1 && y == 5) || (x == 5 && y == 1)

  def isSafe(cell: Cell, solution: Iterable[Cell]): Boolean =
    !solution.exists(p => p == cell) & isValid(solution.last, cell)

  def placeMarks(n: Int = w * h)(using factory: IterableFactory): Iterable[Solution] = n match
    case 1 => factory(List((w / 2, h / 2)))
    case _ =>
      for
        sol <- placeMarks(n - 1)
        x <- 0 until w
        y <- 0 until h
        if isSafe((x, y), sol)
      yield sol.++(List((x, y)))

  def render(solution: Seq[(Int, Int)], width: Int, height: Int): String =
    val reversed = solution.reverse
    val rows =
      for y <- 0 until height
          row = for x <- 0 until width
          number = reversed.indexOf((x, y)) + 1
          yield if number > 0 then "%-2d ".format(number) else "X  "
      yield row.mkString
    rows.mkString("\n")

  println(render(solution = Seq((0, 0), (2, 1)), width = 3, height = 3))

@main def TestSolitaire(): Unit =
  import Solitaire.*
  import Solitaire.given_IterableFactory
  placeMarks().zipWithIndex.foreach({solution => println("Solution " + solution._2); println(render(solution._1.toList, w, h))})