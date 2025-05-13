package ex3

object Solitaire extends App:
  def render(solution: Seq[(Int, Int)], width: Int, height: Int): String =
    val reversed = solution.reverse
    val rows =
      for y <- 0 until height
          row = for x <- 0 until width
          number = reversed.indexOf((x, y)) + 1
          yield if number > 0 then "%-2d ".format(number) else "X  "
      yield row.mkString
    rows.mkString("\n")


  val moves = Seq((1, 0), (-1, 0), (2, 0), (-2, 0), (0, 1), (0, -1), (0, 2), (0, -2), (1, 1), (1, -1), (-1, 1), (-1, -1)) // possible moves

  def placeMarks(width: Int, height: Int): Unit =
    val start = (width / 2, height / 2)

    def inBounds(x: Int, y: Int): Boolean =
      x >= 0 && x < width && y >= 0 && y < height

    val total = width * height

    def explore(path: Seq[(Int, Int)], visited: Set[(Int, Int)]): Unit =
      if path.length == total then
        println(render(path, width, height) + "\n")
      else
        val (x, y) = path.head
        for (dirx, diry) <- moves
          nx = x + dirx
          ny = y + diry
          next = (nx, ny)
          if inBounds(nx, ny) && !visited.contains(next) do
            explore(next +: path, visited + next)

    explore(Seq(start), Set(start))


  //println(render(solution = Seq((0, 0), (2, 1)), width = 3, height = 3))
  placeMarks(3, 3)