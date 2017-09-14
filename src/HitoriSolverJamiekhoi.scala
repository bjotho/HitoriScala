import java.io.{File, PrintWriter}
import java.util.regex.Pattern

object HitoriSolver {
  def main(args: Array[String]){

    //val inputPath = args(0)
    //val outputPath = args(1)
    val inputPath = "src/validation_puzzle.txt" //args(0)
    val outputPath = "src/result.txt" //args(1)
    println(inputPath)
    println(outputPath)

    val puzzleFile = new File(inputPath)

    solvePuzzle(puzzleFile)
    
    
    class Box(xp:Int, yp:Int, num:Int, ind:Int, g:String="G"){
      val x = xp;
      val index = ind;
      val y = yp;
      val value = num;
      var choice = g;
      
      def changeColor(s:String):Box = {
        return new Box(x, y, value, index, s);
      }
      
      def isWhite():Boolean = {
        if(choice == "W") true else false
      }
      def isGray():Boolean = {
        if(choice == "G") true else false
      }
      def isBlack():Boolean = {
        if(choice == "B") true else false
      }
    }  

    def solvePuzzle(f:File):Unit = {
      val lines = scala.io.Source.fromFile(f).mkString.split("\n")
      
      // Create the hitori board
      var hitoriBoard = loadBoard(lines);
      var boardsize = Math.sqrt(hitoriBoard.size).toInt
      
      // Initial finding for black and white squares
      for(i <- 1 to boardsize){
        // Check for triplets
        var row = getRow(hitoriBoard, i)
        var col = getColumn(hitoriBoard, i)
        var rowchange = checkTriplets(row);
        var colchange = checkTriplets(col);
        
        for(b<-rowchange){
          row = row.updated(b, row(b).changeColor("W"))
        }
        for(b<-colchange){
          col = col.updated(b, col(b).changeColor("W"))
        }        
        // Check pair induction
        rowchange = checkPairInduction(row)
        colchange = checkPairInduction(col);
        for(b<-rowchange){
          row = row.updated(b, row(b).changeColor("B"))
        }
        for(b<-colchange){
          col = col.updated(b, col(b).changeColor("B"))
        }
        hitoriBoard = multiUpdate(hitoriBoard,row);
        hitoriBoard = multiUpdate(hitoriBoard,col);
      }
      
      //printBoard(hitoriBoard)
      printAnswers(hitoriBoard)
      
      // Solve puzzle and output to file, like so:
      var outputFile = new PrintWriter( new File(outputPath) , "UTF-8")
      
      for(i<-0 until boardsize){
        for(ii <-0 until boardsize){
          outputFile.print(hitoriBoard(i*boardsize+ii).choice + " ")
        }
        outputFile.println()
      }
      /*
      outputFile.println("b w w w w")
      outputFile.println("W B W B W")
      outputFile.println("W W W W B")
      outputFile.println("W W B W W")
      outputFile.println("B W W W B")
*/
      outputFile.close()
    }
    
    def checkTriplets(row:List[Box]):List[Int] = {
      var s = ""
      row.foreach((box:Box)=> s += box.value)
      return tripletsJava(s)
    }
    
    def checkPairInduction(row:List[Box]):List[Int] = {
      var s = ""
      row.foreach((box:Box)=> s += box.value)
      return pairInductionJava(s)
    }
    
    def tripletsJava(row:String):List[Int] = {
      var pattern = Pattern.compile("(.).\\1");
      var matcher = pattern.matcher(row);
      var l = List[Int]()
      while(matcher.find()){
        //println(matcher group(0))
        //println(matcher.start())
        l = l :+ matcher.start() + 1;
      }
      //println(l)
      return l
    }
    
    def pairInductionJava(row:String):List[Int] = {
      var pattern = Pattern.compile("(.).\\1\\1");
      var pattern2 = Pattern.compile("(.)\\1.\\1");
      var matcher = pattern.matcher(row);
      var l = List[Int]()
      while(matcher.find()){
        l = l :+ matcher.start() + 0;
      }
      matcher = pattern2.matcher(row);
      while(matcher.find()){
        l = l :+ matcher.end - 2;
      }
      return l
    }
    
    def blacken(board:List[Box], box:Box):List[Box] = {
      var newBoard = board.updated(box.index, box.changeColor("B"))
      var sides = List[Box]();
      sides = sides ::: board.filter((b:Box)=>b.x == box.x+1 && b.y == box.y)
      sides = sides ::: board.filter((b:Box)=>b.x == box.x-1 && b.y == box.y)
      sides = sides ::: board.filter((b:Box)=>b.x == box.x && b.y+1 == box.y)
      sides = sides ::: board.filter((b:Box)=>b.x == box.x && b.y-1 == box.y)
      for(s<-sides){
        newBoard = whiten(newBoard, s)
      }
      return newBoard
    }
    
    def whiten(board:List[Box], box:Box):List[Box] = {
      var newBoard = board.updated(box.index, box.changeColor("W"))
      return newBoard
    }
    
    def getRow(l:List[Box], row:Int):List[Box] = {
      return l.filter(_.y == row)
    }
    
    def getColumn(l:List[Box], column:Int):List[Box] = {
      return l.filter(_.x == column)
    }
    
    def loadBoard(lines: Array[String]):List[Box] = {
      var hitoriBoard = List[Box]()
      for(i<-1 to lines.length){
        val line = lines(i-1).replaceAll("\\s", "")
        for(ii<-1 to lines.length){
          val b = new Box(ii, i, line(ii-1).asDigit, ((i-1)*5+ii-1));
          hitoriBoard = hitoriBoard :+ b;
        }
      }
      return hitoriBoard;
    }
    
    def printBoard(board:List[Box]){
      for(i<-0 until Math.sqrt(board.size).toInt){
        for(ii <-0 until Math.sqrt(board.size).toInt){
          print(board(i*Math.sqrt(board.size).toInt+ii).value + " ")
        }
        println()
      }
    }
    
    def printAnswers(board:List[Box]){
      for(i<-0 until Math.sqrt(board.size).toInt){
        for(ii <-0 until Math.sqrt(board.size).toInt){
          print(board(i*Math.sqrt(board.size).toInt+ii).choice + " ")
        }
        println()
      }
    }
    
    def multiUpdate(board:List[Box], update:List[Box]):List[Box] = {
      var newBoard = board
      //update.foreach((b:Box)=> newBoard = newBoard.updated(b.index, b))
      for(b<-update){
        if(b.isBlack()){
          newBoard = blacken(newBoard, b)
        }else if(b.isWhite()){
          newBoard = whiten(newBoard, b)
        }else{
        }
      }
      return newBoard
    }
    
    def setAllBlack(row:List[Box]):List[Box] = {
      var a = row;
      for(i<-0 until a.length){
        a = a.updated(i, a(i).changeColor("B"))
      }
      return a
    }
    
  }
}