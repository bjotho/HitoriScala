import java.io.{File, PrintWriter}
import scala.util;

object HitoriSolver
{
  def main(args: Array[String]): Unit =
  {

    val inputPath = args(0);
    val outputPath = args(1);
    println(inputPath);
    println(outputPath);

    val puzzleFile = new File(inputPath);

    solvePuzzle(puzzleFile);

    def solvePuzzle(f:File):Unit = {
      val lines = scala.io.Source.fromFile(f).mkString.split("\n");
      //println(lines);
      //lines.foreach(print);
      
      //Solve Puzzle here:
      
      val p = new Puzzle(lines);
      p.fillPuzzle(lines);
      println(p.getSquareList().foreach(_.v) + "v");
      println(p.getSquareList()(3).v);

      // Solve puzzle and output to file, like so:
      var outputFile = new PrintWriter( new File(outputPath) , "UTF-8");

      outputFile.println("B W W W W");
      outputFile.println("W B W B W");
      outputFile.println("W W W W B");
      outputFile.println("W W B W W");
      outputFile.println("B W W W B");

      outputFile.close();
    }
  }
  
  class Puzzle(lines:Array[String])
  {
    var allSquares = List[Square]();
    def fillPuzzle(lines:Array[String]) =
    {
      var i = 0;
      while(i < lines.length)
      {
        val n2 = lines(i).mkString.split(" ");
        val n2Int = n2.map(_.toInt);
        val s = new Square(1, 1, n2(i));
        allSquares = allSquares :+ s;
        i += 1;
      }
    }
    
    def getSquareList():List[Square] =
    {
      return allSquares;
    }
  }
  
  
  class Square(xPos:Int, yPos:Int, value:String, possibleColors:List[Char] = List[Char]('B', 'W'), solved:Boolean = false)
  {
    val x = xPos;
    val y = yPos;
    val v = value;
    var s = solved;
    var left = possibleColors;
  }
}
