package context

import value._
import scala.io._
import util.control.Breaks._
/*
 * Notes:
 * console is Jedi's user interface
 * parsers and global environment are created here
 * console.main launches repl or executes a Jedi file
 */
object console {
   val parsers = new jediParsers // for now
   val globalEnv = new Environment
   var verbose = false

   def execute(cmmd: String): String = {
      val tree = parsers.parseAll(parsers.expression, cmmd)
      tree match {
         case tree: parsers.Failure => throw new SyntaxException(tree)
         case _ => {
            val exp = tree.get  // get the expression from the tree
            //println("translation: " + exp.emit)
            val result = exp.execute(globalEnv)  // execute the expression
            result.toString  // return string representation of result
         }
      }
   }
   
   private def executeFile(fileName: String) {

     var more = true
     var multi_lines = ""
     var joining = false
     var startJoining = false
    
     for (line <- Source.fromFile(fileName).getLines if more) {
       try {
         breakable{
           if(line.matches("""\s*""")) break
           if(line.charAt(line.length -1) == '{') { joining = true; startJoining = true; println("-> " + line); multi_lines += line; break}
           if(!joining) {
             println("-> " + line)
             println(execute(line))
           } else {
             println(line)
             if(startJoining) { startJoining = false; multi_lines += line } 
             else if (line.length == 1 && line.charAt(0) == '}') multi_lines += line
             else multi_lines += "; " + line
           }
           if(joining && multi_lines.charAt(multi_lines.length -1) == '}') { joining = false; println(execute(multi_lines)); multi_lines = ""; break}
         }
       } catch {

            case e: SyntaxException => {
               println(e)
               println(e.result.msg)
               println("line # = " + e.result.next.pos.line)
               println("column # = " + e.result.next.pos.column)
               println("token = " + e.result.next.first)
            }
            case e: UndefinedException => {
              println(e)
              if (verbose) e.printStackTrace()
            }
            case e: TypeException => {
              println(e)
              if (verbose) e.printStackTrace()
            }
            case e: JediException => { 
              println(e)
              if (verbose) e.printStackTrace()
            }
            
            case e: Exception => {
              println(e)
              more = false
            }
         } // catch
      } // for
      println("bye")
   }
   
 
   
   // read-execute-print loop
    def repl {
      var more = true
      var cmmd = ""
      var multi_lines = ""
      var joining = false
      var startJoining = false
      
      while(more) {
         try {
            if(!joining) print("-> ")
            cmmd = StdIn.readLine
            breakable{
             if(cmmd.length > 0 && cmmd.charAt(cmmd.length -1) == '{') 
               { joining = true; startJoining = true; multi_lines += cmmd; break}
             if(joining) {
               if(cmmd.matches("""\s*""")) break
               if(startJoining) { startJoining = false; multi_lines += cmmd }
               else if (cmmd.length == 1 && cmmd.charAt(0) == '}') multi_lines += cmmd
               else multi_lines += "; " + cmmd
             } else if (cmmd == "quit") more = false
             else println(execute(cmmd))
             if(joining && multi_lines.charAt(multi_lines.length -1) == '}') 
               { joining = false; println(multi_lines); println(execute(multi_lines)); multi_lines = ""; break}
            }
         } 
         catch {
            case e: SyntaxException => {
               println(e)
               println(e.result.msg)
               println("line # = " + e.result.next.pos.line)
               println("column # = " + e.result.next.pos.column)
               println("token = " + e.result.next.first)
               joining = false
               multi_lines = ""
            }
            case e: UndefinedException => {
              println(e.gripe)
              if (verbose) e.printStackTrace()
              joining = false
              multi_lines = ""
            }
            case e: TypeException => {
              println(e.gripe)
              if (verbose) e.printStackTrace()
              joining = false
              multi_lines = ""
            }
            case e: JediException => { 
              println(e.gripe)
              if (verbose) e.printStackTrace()
              joining = false
              multi_lines = ""
            }
            case e: Exception => {
              println(e.getMessage)
              more = false
            }
         } finally {
            Console.flush
         }
      }
      println("bye")
   }
    
   def main(args: Array[String]): Unit = { 
     if (args.length == 0) 
       repl
     else
       try {
         executeFile(args(0))
       } catch  {
         case e: Exception => {
              println(e)
            }
       }
   }
}