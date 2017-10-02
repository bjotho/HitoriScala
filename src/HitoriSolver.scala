import java.io.{File, PrintWriter}
import scala.util;
import scala.util.control.Breaks._;

object HitoriSolver
{
  def main(args: Array[String]): Unit =
  {

    val inputPath = args(0);
    val outputPath = args(1);
    //println(inputPath);
    //println(outputPath);
    //print("\n");
    
    val puzzleFile = new File(inputPath);

    solvePuzzle(puzzleFile);

    def solvePuzzle(f:File):Unit = {
      val lines = scala.io.Source.fromFile(f).mkString.split("\n");
      //lines.foreach(println);
      //print("\n");
      
      val p = new Puzzle(lines);
      val ps = new PuzzleSolver();
      p.fillPuzzle(lines);
      
      println("Start");
      ps.iterate(p);
      println("Done");
			
      var outputFile = new PrintWriter( new File(outputPath) , "UTF-8");
      
      for(i <- 0 to p.SIZE-1)
      {
        for(j <- 0 to p.SIZE-1)
        {
          val s = p.getSquareList()((i*p.SIZE)+j);
          if(s.s)
            outputFile.print(s.sol + " ");
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
    var prevBoard = List[Square]();
    var runOneTime = true;
    var solved = false;
    
    def prevBoardEqual():Boolean = (prevBoard.length == unsolvedSquares.length);
    
    def getSquareList():List[Square] = allSquares;
    def getUnsolvedSquares():List[Square] = unsolvedSquares;
    def setUnsolvedSquares(s:Square) =
    {
      this.unsolvedSquares = this.unsolvedSquares.filter(_.i  != s.i);
    }
    def resetSquareList(l:List[Square]) =
    {
      this.allSquares = l;
    }
    def resetUnsolvedSquares(l:List[Square]) =
    {
      this.unsolvedSquares = l;
    }
    
    def getSquareXY(x:Int, y:Int):Square =
    {
      return getSquareList().filter(_.x == x).filter(_.y == y)(0);
    }
    
    def getSquareIndex(i:Int):Square =
    {
      return getSquareList.filter(_.i == i)(0);
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
          val c = n(j).replaceAll("[^0-9]", "").toInt;
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
      //Loop iterating until the board is solved (when all squares have received a color)
      //var c = 0;
      //val LIMIT = 10;
      while(/*c < LIMIT*/!p.solved)
      {    
        /*if(p.runOneTime)
        {
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
            }
            else
              cornerCase(p, i);
             
            if(duplicatesRow(p, i) >= 2)
            {
              twoPlusOnePattern(p, i, p.getRowY(i.y).filter(_.v == i.v));
            }
            if(duplicatesCol(p, i) >= 2)
            {
              twoPlusOnePattern(p, i, p.getColumnX(i.x).filter(_.v == i.v));
            }
          }
        }
        
        for(i <- p.getUnsolvedSquares())
        {
          if(duplicates(p, i) == 0)
            i.setSolution('W', p);
        }*/
         
        if(p.prevBoardEqual())
        {
          /*for(i <- p.getUnsolvedSquares())
          {
            if(whiteIsolationCheck(p, i))
            {
              //println("Isolation if (" + (i.x+1) + ", " + (i.y+1) + ") is black");
              i.setSolution('W', p);
            }
          }
          if(p.prevBoardEqual())
          {*/
              println("Brute force initiate");
              //bruteForceBuild(p, 0, false);
              bruteForce(p);
          //}
        }
         
        if(p.getUnsolvedSquares().isEmpty)
          p.solved = true;
         
        p.prevBoard = p.unsolvedSquares;
        p.runOneTime = false;
         
        //c += 1;
      }
    }
    
    def bruteForce(p:Puzzle):Unit =
    {
      var cachedBoard = p.getSquareList();
      var cachedUnsolved = p.getUnsolvedSquares();
      var cachedPrevBoard = p.prevBoard;
      var checkPercentage = 0;
      var checks = 0;
      val remainingSquaresCombinations = ((Math.pow(2, p.getUnsolvedSquares().length))-1).toInt
      val remainingSquaresCombinationsPercent = remainingSquaresCombinations/100.toInt;
      
      println("Percentage of possible combinations checked:");
      
      for(i <- 0 to remainingSquaresCombinations)
      {
        var colorList = Array[Char]();
        val binary = Integer.toBinaryString(i);
        var binaryString = binary.toString();
        
        for(j <- binary.length to (p.getUnsolvedSquares().length-1))
        {
          binaryString = "0" + binaryString;
        }
        
        val binaryCharArray = binaryString.toCharArray();
        
        for(j <- 0 to binaryString.length-1)
        {
          if(binaryCharArray(j).asDigit == 0)
            colorList = colorList :+ 'W';
          else
            colorList = colorList :+ 'B';
        }
        var n = 0;
        for(j <- p.getUnsolvedSquares())
        {
          j.setSolution(colorList(n), p, surroundBlack, true);
          n += 1;
        }
        if(valid(p))
        {
          println("Solution found after checking " + (checks+1) + " combinations!");
          p.solved = true;
          return;
        }
        else
          resetToCached();
        
        checks += 1;
        if(checks % remainingSquaresCombinationsPercent == 0)
        {
          checkPercentage += 1;
          println(checkPercentage + "%");
        }
      }
      
      def resetToCached() =
      {
        p.resetSquareList(cachedBoard);
        p.resetUnsolvedSquares(cachedUnsolved);
        p.prevBoard = cachedPrevBoard;
      }
    }
    
    def valid(p:Puzzle):Boolean =
    {
      //Check if any white square has another white square with same value on same row/column
      for(i <- p.getSquareList().filter(_.sol == 'W'))
      {
        if(duplicates(p, i, true) > 0)
        {
          return false;
        }
      }
      
      //Check if any black square has an adjacent black square
      for(i <- p.getSquareList().filter(_.sol == 'B'))
      {
        if(getAdjacentSquares(p, i).exists(_.sol == 'B'))
        {
          //println("Adjacent black squares at (" + (i.x+1) + ", " + (i.y+1) + ")");
          return false;
        }
      }
      
      //Check if any sections of white are isolated
      for(i <- p.getSquareList())
      {
        if(whiteIsolationCheck(p, i))
        {
          //println("White isolation if (" + (i.x+1) + ", " + (i.y+1) + ") is black");
          return false;
        }
      }
      
      return true;
    }
    
    /*def bruteForceBuild(p:Puzzle, n:Int, changeColour:Boolean):Unit =
    {
      var cachedBoard = p.getSquareList();
      var cachedUnsolved = p.getUnsolvedSquares();
      var cachedPrevBoard = p.prevBoard;
      p.prevBoard = List[Square]();
      
      if(!changeColour)
        p.getUnsolvedSquares()(n).setSolution('B', p, surroundBlack);
      else
        p.getUnsolvedSquares()(n).setSolution('W', p);
      
      while(!p.solved && valid(p))
      {
        if(p.prevBoardEqual())
        {
          resetToCached();
          if(changeColour)
            bruteForceBuild(p, (n+1), !changeColour);
          else
            bruteForceBuild(p, n, !changeColour);
          
          return;
        }
        for(i <- p.getUnsolvedSquares())
        {
          if(duplicates(p, i) <= 0)
            i.setSolution('W', p);
       
          if(p.getUnsolvedSquares().isEmpty)
            p.solved = true;
          
          if(!valid(p))
          {
            break;
          }
        }
        if(p.getUnsolvedSquares().isEmpty)
           p.solved = true;
        
        p.prevBoard = p.unsolvedSquares;
      }
      
      if(!valid(p))
      {
        resetToCached();
        if(changeColour)
          p.getUnsolvedSquares()(n).setSolution('B', p, surroundBlack);
        else
          p.getUnsolvedSquares()(n).setSolution('W', p);
      }
      
      def resetToCached() =
      {
        p.resetSquareList(cachedBoard);
        p.resetUnsolvedSquares(cachedUnsolved);
        p.prevBoard = cachedPrevBoard;
      }
    }*/
    
    def checkBetweenSame(p:Puzzle, s:Square, horizontal:Boolean) =
    {
      var adjSquares = List[Square]();
      if(horizontal)
        adjSquares = getAdjacentSquares(p, s, List[String]("left", "right"));
      else
        adjSquares = getAdjacentSquares(p, s, List[String]("above", "below"));
      
      if(adjSquares.length == 2)
      {
        if(adjSquares(0).v == adjSquares(1).v)
          s.setSolution('W', p);
      }
    }
    
    def getAdjacentSquare(p:Puzzle, s:Square, location:String):Square =
    {
      return (location) match
      {
        case ("above") => p.getSquareList()(s.i-p.SIZE);
        case ("left") =>
          {
            if(s.i % p.SIZE != 0)
              return p.getSquareList()(s.i-1)
            else
              return p.getSquareList()(-1);
          }
        case ("right") =>
          {
            if(s.i % p.SIZE != p.SIZE-1)
              return p.getSquareList()(s.i+1);
            else
              return p.getSquareList()(-1);
          }
        case ("below") => p.getSquareList()(s.i+p.SIZE);
      }
    }
    
    def getAdjacentSquares(p:Puzzle, s:Square, l:List[String] = List[String]("above", "left", "right", "below")):List[Square] =
    {
      var adjacentSquares = List[Square]();
      val dir = l;
      
      for(i <- dir)
      {
        squareExists(i);
      }
      
      def squareExists(str:String) =
      {
        try
        {
          setAdjSqrList(str);
        }
        catch
        {
          case e: Exception => {/*println(e);*/}
        }
      }
      
      def setAdjSqrList(str:String) =
        {
          adjacentSquares = adjacentSquares :+ getAdjacentSquare(p, s, str);
        }
      return adjacentSquares;
    }
    
    def duplicates(p:Puzzle, s:Square, bruteForce:Boolean = false):Int =
    {
      return duplicatesRow(p, s, bruteForce) + duplicatesCol(p, s, bruteForce);
    }
    
    def duplicatesRow(p:Puzzle, s:Square, bruteForce:Boolean = false):Int =
    {
      val duplicatesInRow = p.getRowY(s.y).filter(_.i != s.i).filter(_.v == s.v);
      
      if(!s.getSolved() && !bruteForce)
      {
        if(duplicatesInRow.exists(_.getSolution() == 'W'))
        {
          s.setSolution('B', p, surroundBlack);
        }
      }
      if(!bruteForce)
        return duplicatesInRow.length;
      else
        return duplicatesInRow.filter(_.getSolution() != 'B').length;
    }
    
    def duplicatesCol(p:Puzzle, s:Square, bruteForce:Boolean = false):Int =
    {
      val duplicatesInCol = p.getColumnX(s.x).filter(_.i != s.i).filter(_.v == s.v);
      
      if(!s.getSolved() && !bruteForce)
      {
        if(duplicatesInCol.exists(_.getSolution() == 'W'))
        {
          s.setSolution('B', p, surroundBlack);
        }
      }
      if(!bruteForce)
        return duplicatesInCol.length;
      else
        return duplicatesInCol.filter(_.getSolution() != 'B').length;
    }
    
    def twoPlusOnePattern(p:Puzzle, s:Square, l:List[Square]) =
    {
      //println(l.length + " duplicates at square (" + (s.x+1) + ", " + (s.y+1) + ")");
      for(i <- 0 to l.length-2)
      {
        if(Math.abs(l(i).i - l(i+1).i) == 1)
        {
          var restList = l.filter(_.i != l(i).i).filter(_.i != l(i+1).i);
          for(j <- 0 to l.length-3)
          {
            if(Math.abs(restList(j).i - l(i).i) >= 2 || Math.abs(restList(j).i - l(i+1).i) >= 2)
            {
              restList(j).setSolution('B', p, surroundBlack);
            }
          }
        }
      }
    }
    
    val surroundBlack = (p:Puzzle, s:Square) =>
    {
      //println("Square (" + (s.x+1) + ", " + (s.y+1) + ") is black");
      val adj = getAdjacentSquares(p, s);
      
      for(i <- adj)
      {
        if(!i.getSolved())
          i.setSolution('W', p);
      }
    }
    
    def whiteIsolationCheck(p:Puzzle, s:Square):Boolean =
    {
      if(s.getSolution() == 'W')
        return false;
      
      var allWhiteAndUnsolved = 0;
      var checkedIndexes = List[Int](s.i);
      var section = 0;
      
      def setCheckedIndexes(s:Square) =
      {
        checkedIndexes = checkedIndexes :+ s.i;
      }
      def getCheckedIndexes():List[Int] = checkedIndexes;
      
      for(i <- p.getSquareList())
      {
        if(i.getSolution() != 'B')
        {
          allWhiteAndUnsolved += 1;
        }
      }
      //println("allWhiteAndUnsolved = " + allWhiteAndUnsolved);
      
      countSection(p, getAdjacentSquares(p, s)(0), s);
      
      def countSection(p:Puzzle, s:Square, startSquare:Square):Unit =
      {
        if(!getCheckedIndexes().contains(s.i) && s.getSolution() != 'B')
        {
          section += 1;
          setCheckedIndexes(s);
          /*
          println("checkedIndexes: ");
          getCheckedIndexes().foreach(print);
          print("\n");
          */
          var adj = getAdjacentSquares(p, s).filter(_.i != startSquare.i);
          
          for(i <- 0 to adj.length-1)
          {
            countSection(p, adj(i), startSquare);
          }
        }
      }
      return (allWhiteAndUnsolved != section);
    }
    
    
    
    //To better understand how this method works, try un-commenting all the println commands and run
    def cornerCase(p:Puzzle, s:Square) =
    {
      val adj = getAdjacentSquares(p, s);
      if(adj.exists(_.v == s.v))
      {
        //println("Square (" + (s.x+1) + ", " + (s.y+1) + ") is equal to either square (" + (adj(0).x+1) + ", " + (adj(0).y+1) + "), or square (" + (adj(1).x+1) + ", " + (adj(1).y+1) + ")");
        var adjSqrLists = List[List[Square]]();
        var c = 0;
        for(i <- adj)
        {
          adjSqrLists = adjSqrLists :+ getAdjacentSquares(p, i);
          
          /*print("Square adjacent to (" + (i.x+1) + ", " + (i.y+1) + "): ");
          for(j <- 0 to adj.length)
          {
            print("(" + (getAdjacentSquares(p, i)(j).x+1) + ", " + (getAdjacentSquares(p, i)(j).y+1) + ") ");
          }
          print("\n");*/
          c += 1;
        }
        
        for(i <- 0 to adjSqrLists(0).length-1)
        {
          if((adjSqrLists(0).exists(_.i == adjSqrLists(1)(i).i)) && adjSqrLists(1)(i).i != s.i)
          {
            //println("Match! adjSqrsLists(1)(" + i + ") = (" + (adjSqrLists(1)(i).x+1) + ", " + (adjSqrLists(1)(i).y+1) + "), value = " + adjSqrLists(1)(i).v);
            var cornerList = List[Square](s);
            for(j <- adj)
            {
              cornerList = cornerList :+ j;
            }
            cornerList = cornerList :+ adjSqrLists(1)(i);
            for(j <- 0 to cornerList.length-1)
            {
              if(cornerList(0).v != cornerList(j).v || cornerList.filter(_.v != cornerList(0).v).isEmpty)
              {
                if(cornerList(j).i != cornerList(3).i && cornerList(j).v == cornerList(3).v || cornerList.filter(_.v != cornerList(0).v).isEmpty)
                {
                  val diagSquare = cornerList(3);
                  //println("Success. diagSquare = (" + (diagSquare.x+1) + ", " + (diagSquare.y+1) + "), value = " + diagSquare.v);
                  for(j <- adj)
                  {
                    if(diagSquare.v == j.v)
                    {
                      if(!s.getSolved())
                        s.setSolution('B', p, surroundBlack);
                      
                      if(!diagSquare.getSolved())
                      {
                        diagSquare.setSolution('B', p, surroundBlack);
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
  }
  
  
  class Square(xPos:Int, yPos:Int, index:Int, value:Int, solution:Char = 'G', solved:Boolean = false)
  {
    val x = xPos;
    val y = yPos;
    val i = index;
    val v = value;
    var s = solved;
    var sol = solution;
    
    def getSolved():Boolean = this.s;
    def getSolution():Char = this.sol;
    def setSolution(c:Char, p:Puzzle, surroundBlack:(Puzzle, Square) => Unit = (Puzzle, Square) => Unit, bruteForce:Boolean = false) =
    {
      this.s = true;
      this.sol = c;
      p.setUnsolvedSquares(this);
      if(c == 'B' && !bruteForce)
        surroundBlack(p, this);
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
