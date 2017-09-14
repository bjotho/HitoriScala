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
      print("\n");
      
      val p = new Puzzle(lines);
      val ps = new PuzzleSolver();
      p.fillPuzzle(lines);
      ps.iterate(p);
			
      // Solve puzzle and output to file, like so:
      var outputFile = new PrintWriter( new File(outputPath) , "UTF-8");
      
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
    val SIZE = lines.length;
    var allSquares = List[Square]();
    var unsolvedSquares = List[Square]();
    var solved = false;
    
    def getSquareList():List[Square] = allSquares;
    def getUnsolvedSquares():List[Square] = unsolvedSquares;
    def setUnsolvedSquares(s:Square) =
    {
      this.unsolvedSquares = this.unsolvedSquares.filter(_.i  != s.i);
    }
    
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
      for(i <- 0 to lines.length-1)
      {
        val n = lines(i).mkString.split(" ");
        for(j <- 0 to n.length-1)
        {
          val c = n(j).toCharArray()(0).toString().toInt;
          val s = new Square(j, i, (i*SIZE)+j, c);
          allSquares = allSquares :+ s;
          unsolvedSquares = unsolvedSquares :+ s;
        }
      }
    }
  }
  
  class PuzzleSolver()
  {
    def iterate(p:Puzzle) =
    {
      //Evig løkke som går helt til brettet er løst (til alle squares har fått angitt en farge)
      //while(!p.solved)
      //{
         for(i <- p.getUnsolvedSquares())
         {
           if(!i.isCorner(p))
           {
             if(!i.isEdge(p))
             {
               checkBetweenSame(p, i, true);
               checkBetweenSame(p, i, false);
             }
             else
             {
               (i.x, i.y) match
               {
                 case (0, _) => checkBetweenSame(p, i, false);
                 case (_, 0) => checkBetweenSame(p, i, true);
                 case (x, y) =>
                 {
                   if(x == p.SIZE-1)
                     checkBetweenSame(p, i, false);
                   if(y == p.SIZE-1)
                     checkBetweenSame(p, i, true);
                 }
               }
             }
             if(duplicates(p, i) <= 0)
               i.setSolution('W', p);
           }
         }
         if(p.getUnsolvedSquares().isEmpty)
           p.solved = true;
      //}
    }
    
    def checkBetweenSame(p:Puzzle, s:Square, horizontal:Boolean) =
    {
      var prev = getAdjacentSquare(p, s, true, horizontal);
      var next = getAdjacentSquare(p, s, false, horizontal);
      
      
      if(prev.v == next.v)
      {
        println("prev.v = " + prev.v + ", prev.i = " + prev.i + ", next.v = " + next.v + ", next.i = " + next.i + ", square number (" + (s.x+1) + ", " + (s.y+1) + "), horizontal = " + horizontal);
        s.setSolution('W', p);
      }
    }
    
    def getAdjacentSquare(p:Puzzle, s:Square, prev:Boolean, horizontal:Boolean):Square =
    {
      return (prev, horizontal) match
      {
        case (true, true) => p.getSquareList()(s.i-1);
        case (true, false) => p.getSquareList()(s.i-p.SIZE);
        case (false, true) => p.getSquareList()(s.i+1);
        case (false, false) => p.getSquareList()(s.i+p.SIZE);
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
            s.setSolution('B', p);
        }
      }
      for(i <- column)
      {
        if(s.v == i.v)
        {
          duplicates += 1;
          if(i.pc(0) == 'W' && !s.getSolved())
            s.setSolution('B', p);
        }
      }
      return duplicates;
    }
  }
  
  
  class Square(xPos:Int, yPos:Int, index:Int, value:Int, possibleColors:List[Char] = List[Char]('B', 'W'), solved:Boolean = false)
  {
    val x = xPos;
    val y = yPos;
    val i = index;
    val v = value;
    var s = solved;
    var pc = possibleColors;
    
    def getSolved():Boolean = s;
    def getSolution():Char = this.pc(0);
    def setSolution(c:Char, p:Puzzle) =
    {
      this.s = true;
      this.pc = List[Char](c);
      p.setUnsolvedSquares(this);
    }
    
    def isEdge(p:Puzzle):Boolean = (x == 0 || y == 0 || x == p.SIZE-1 || y == p.SIZE-1);
    def isCorner(p:Puzzle):Boolean =
    {
      return (x, y) match
      {
        case (0, 0) => true;
        case (0, y) => (y == p.SIZE-1);
        case (x, 0) => (x == p.SIZE-1);
        case (x, y) => (x == y && x == p.SIZE-1);
        case _ => false;
      }
    }
  }
}
