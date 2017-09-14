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
      for(i <- p.getSquareList())
      {
        ps.patternMatchTest(p, i);
      }
      
      
			
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
    
    def getSquareXY(x:Int, y:Int):Square =
    {
      return getSquareList().filter(_.x == x).filter(_.y == y)(0);
    }
    
    def getRowY(y:Int):List[Square] =
    {
      var xList = List[Square]();
      for(i <- getSquareList())
      {
        if(i.y == y)
          xList = xList :+ i;
      }
      return xList;
    }
    
    def getColumnX(x:Int):List[Square] =
    {
      var yList = List[Square]();
      for(i <- getSquareList())
      {
        if(i.x == x)
          yList = yList :+ i;
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
        for(j <- 0 to p.SIZE-1)
        {
          val s = p.getSquareList()((i*p.SIZE)+j);
          //if(duplicates(p, s) <= 0)
            //s.setSolution('W');
          //else
          //{
            /*
            spaceCheck(p, p.getColumnX(s.x));
            spaceCheck(p, p.getRowY(s.y));
            */
          //}
        }
      }
    }
    
    def spaceCheck(p:Puzzle, l:List[Square]) =
    {
      var vList = List[Int]();
      for(i <- l)
      {
        vList = vList :+ i.v;
      }
      for(i <- 0 to (l.length-3))
      {
        if(vList(i) == vList(i+2))
          l(i+1).setSolution('W');
      }
    }
    
    def patternMatchTest(p:Puzzle, s:Square) =
    {
      val row = p.getRowY(s.y);
      val col = p.getColumnX(s.x);

      matchList(p, s, row, true);
      matchList(p, s, col, false);
    }
    
    def matchList(p:Puzzle, s:Square, l:List[Square], rowCheck:Boolean) =
    {
      for(i <- 0 to (l.length-3))
      {
        var vList = List[Int]();
        for(j <- i to (i+2))
        {
          vList = vList :+ l(j).v;
        }
        vList match
        {
          case List(s.v, _, s.v) => 
            if(rowCheck)
            {
              //println("p.getSquareList()((((s.x-1)*p.SIZE)+(s.y-1))).v = " + s.x);
              if(p.getSquareXY(s.x, s.y).v == p.getSquareXY((s.x+2), s.y).v)
              {
                p.getSquareXY(s.x, s.y).setSolution('H');
              }
              /*else
              {
                p.getSquareList()((((s.x)*p.SIZE)+(s.y-1))).setSolution('H');
              }*/
            }
          case _ => print("");
        }
      }
    }
    
    def duplicates(p:Puzzle, s:Square):Int =
    {
      val row = p.getRowY(s.y);
      val column = p.getColumnX(s.x);
      var duplicates = -2;
      for(i <- row)
      {
        if(s.v == i.v)
        {
          duplicates += 1;
          if(i.pc(0) == 'W' && !s.getSolved())
            s.setSolution('B');
        }
      }
      for(i <- column)
      {
        if(s.v == i.v)
        {
          duplicates += 1;
          if(i.pc(0) == 'W' && !s.getSolved())
            s.setSolution('B');
        }
      }
      return duplicates;
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
