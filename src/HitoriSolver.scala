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
      lines.foreach(print);
      
      //Solve Puzzle here:
      
      val p = new Puzzle(lines);
      val ps = new PuzzleSolver();
      p.fillPuzzle(lines);
      ps.iterate(p);
      
			
      // Solve puzzle and output to file, like so:
      var outputFile = new PrintWriter( new File(outputPath) , "UTF-8");
      /*
      outputFile.println("B W W W W");
      outputFile.println("W B W B W");
      outputFile.println("W W W W B");
      outputFile.println("W W B W W");
      outputFile.println("B W W W B");
      */
      
      for(i <- 0 to p.SIZE-1)
      {
        for(j <- 0 to p.SIZE-1)
        {
          val s = p.getSquareList()((i*p.SIZE)+j);
          if(s.s)
            outputFile.print(s.pc(0) + " ");
          else
            outputFile.print(s.v + " ");
        }
        outputFile.println("");
      }

      outputFile.close();
    }
  }
  
  class Puzzle(lines:Array[String])
  {
    var allSquares = List[Square]();
    val SIZE = lines.length;
    
    def getSquareList():List[Square] = allSquares;
    
    def getRowY(y:Int):List[Int] =
    {
      var xList = List[Int]();
      for(i <- getSquareList())
      {
        if(i.y == y)
          xList = xList :+ i.v;
      }
      return xList;
    }
    
    def getColumnX(x:Int):List[Int] =
    {
      var yList = List[Int]();
      for(i <- getSquareList())
      {
        if(i.x == x)
          yList = yList :+ i.v;
      }
      return yList;
    }
    
    def fillPuzzle(lines:Array[String]) =
    {
      var i = 0;
      while(i < lines.length)
      {
        val n = lines(i).mkString.split(" ");
        var j = 0;
        while(j < n.length)
        {
          val c = n(j).toCharArray();
          val c2 = c(0).toString().toInt;
          val s = new Square(j, i, c2);
          allSquares = allSquares :+ s;
          j += 1;
        }
        i += 1;
      }
    }
  }
  
  class PuzzleSolver()
  {
    def iterate(p:Puzzle) =
    {
      
      for(i <- 0 to p.SIZE-1)
      {
        //val row = p.getRowY(p.getSquareList()(i*p.SIZE).y);
        for(j <- 0 to p.SIZE-1)
        {
          val s = p.getSquareList()((i*p.SIZE)+j);
          if(!duplicate(p, s))
            s.setSolution('W');
          //val column = p.getColumnX(p.getSquareList()(j).x);
          //println("\nRow " + (i+1) + ": " + row + "\nColumn " + (j+1) + ": " + column);
        }
      }
    }
    
    def duplicate(p:Puzzle, s:Square):Boolean =
    {
      val row = p.getRowY(s.y).take(s.x-1) ++ p.getRowY(s.y).drop(s.x);
      val column = p.getColumnX(s.x).take(s.y-1) ++ p.getColumnX(s.x).drop(s.y);
      var duplicates = 0;
      for(i <- row)
      {
        if(s.v == i)
          duplicates += 1;
      }
      for(i <- column)
      {
        if(s.v == i)
          duplicates += 1;
      }
      return (duplicates > 0);
    }
  }
  
  
  class Square(xPos:Int, yPos:Int, value:Int, possibleColors:List[Char] = List[Char]('B', 'W'), solved:Boolean = false)
  {
    val x = xPos;
    val y = yPos;
    val v = value;
    var s = solved;
    var pc = possibleColors;
    
    def getSolution():Char = this.pc(0);
    def setSolution(c:Char) =
    {
      this.s = true;
      this.pc = List[Char](c);
    }
    
    def getSolved():Boolean = s;
    def setSolved(b:Boolean) = { this.s = b; }
  }
}
