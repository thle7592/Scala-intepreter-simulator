package context

import scala.util.parsing.combinator._
import expression._
import value._

/*
 * Notes:
 * disjunction reduces to conjunction reduces to equality ... reduces to term
 * if A reduces to B, then B will have higher precedence than A
 * Example: sum reduces to product, so a + b * c = a + (b * c)
 * Had to make some big corrections to numeral regex
 * This could probably have been a singleton
 */

class jediParsers extends RegexParsers
{
   def expression: Parser[Expression] = declaration | conditional | iteration | disjunction | failure("Invalid expression")
   
   // assignment parser
   //assignment ::= identifier ~ "=" ~ expression
   def assignment: Parser[Expression] = identifier ~ "=" ~ expression ^^ {
     case id~"="~exp => Assignment(id, exp)
   }
   
   // iteration parser
   // iteration ::= "while" ~ "(" ~ expression ~ ")" ~ expression
   def iteration : Parser[Expression] = "while" ~ "(" ~ expression ~ ")" ~ expression ^^ {
     case "while"~"("~cond~")"~exp => Iteration(cond, exp)
   }
   
   // dereference parsers
   // dereference ::= "[" ~ expression ~ "]"
   def dereference : Parser[Expression] = "[" ~> expression <~ "]" ^^ {
     case exp => FunCall(Identifier("dereference"), exp::Nil)
   }
   
   // block parser
   // a block is one or more semi-colon separated expressions bracketed by curly braces:
   //block ::= "{" ~ expression ~ (";" ~ expression)* ~ "}"
   def block: Parser[Expression] = "{" ~> expression ~ rep(";" ~> expression ) <~ "}" ^^ {
     case exp~more => Block(exp::more)
   }
   
   // params parser
   // a parameter list is zero or more comma-separated identifiers bracketed by parentheses:
   // params ::= "(" ~ (identifier ~ ("," ~ identifier)*)? ~ ")"
   def params: Parser[List[Identifier]] = "(" ~> opt(identifier ~ rep("," ~> identifier)) <~ ")" ^^ {
    case None => Nil
    case Some(iden ~ Nil) => List(iden)
    case Some(iden ~ more) => iden::more
   }
   
   // lambda parser
   // lambda ::= "lambda" ~ params ~ expression
   def lambda: Parser[Expression] = "lambda" ~ params ~ expression ^^ {
    case lam ~ par ~ exp => Lambda(par, exp)
   }
   
   // thunk parser
   // freeze(exp|block)
   def thunk: Parser[Expression] = "freeze" ~ "(" ~ rep("{") ~> expression ~ rep(";" ~> expression ) <~ rep("}") ~ ")" ^^ {
     case exp~Nil =>  MakeThunk(exp)
     case exp~more =>  MakeThunk(Block(exp::more))
   }
   
   // text parser
   // text(exp|block)
   def text: Parser[Expression] = "delay" ~ "(" ~ rep("{") ~> expression ~ rep(";" ~> expression ) <~ rep("}") ~ ")" ^^ {
     case exp~Nil =>  MakeText(exp)
     case exp~more =>  MakeText(Block(exp::more))
   }
 
   def declaration: Parser[Declaration] = "def" ~ identifier ~ "=" ~ expression ^^ {
     case "def" ~ id ~ "=" ~ exp => Declaration(id, exp)
   }
   
   def conditional: Parser[Conditional] = "if" ~ "(" ~ expression ~ ")" ~ expression ~ opt("else" ~ expression) ^^ {
     case "if"~"("~cond~")"~cons~None => Conditional(cond, cons)
     case "if"~"("~cond~")"~cons~Some("else"~alt) => Conditional(cond, cons, alt)
   }
   
   def disjunction: Parser[Expression] = conjunction ~ rep("||" ~> conjunction) ^^ {
     case con ~ Nil => con
     case con ~ more => Disjunction(con::more)
   }
       
   // conjunction ::= equality ~ ("&&" ~ equality)*
   def conjunction: Parser[Expression] = equality ~ rep("&&" ~> equality) ^^ {
     case eq ~ Nil => eq
     case eq ~ more => Conjunction(eq::more)
   }
   
   // equality ::= inequality ~ ("==" ~ inequality)*
   def equality: Parser[Expression] = inequality ~ rep("==" ~> inequality) ^^ {
     case in ~ Nil => in
     case in ~ more => FunCall(Identifier("equals"), in::more)
   }
   
   // inequality ::= sum ~ (("<" | ">" | "!=") ~ sum)?
   def inequality: Parser[Expression] = sum ~ opt(("<" | ">" | "!=") ~ sum) ^^ {
     case s ~ None => s
     case s ~ Some("<" ~ s1) => FunCall(Identifier("less"), List(s, s1))
     case s ~ Some(">" ~ s1) => FunCall(Identifier("more"), List(s, s1))
     case s ~ Some("!=" ~ s1) => FunCall(Identifier("unequals"), List(s, s1))
   }

   // negate(exp) = 0 - exp
   private def negate(exp: Expression): Expression = {
     val sub = Identifier("sub")
     val zero = Integer(0)
     FunCall(sub, List(zero, exp))
   }
      
   // sum ::= product ~ ("+" | "-") ~ product)*  
   def sum: Parser[Expression] = product ~ rep(("+"|"-") ~ product ^^ {
     case "+"~s=>s
     case "-"~s=> negate(s)
     })^^{
     case p~Nil=> p
     case p~rest=>FunCall(Identifier("add"), p::rest)
   }
      
   // product ::= term ~ (("*" | "/") ~ term)*
   def product: Parser[Expression] = term ~ rep(("*"|"/") ~ term) ^^ {
     case (t ~ blah) => parseProduct(t, blah)
   }
    
   // generates left-to-right calls to mul and div:
   private def parseProduct(t: Expression, terms: List[~[String, Expression]]): Expression = {
     terms match {
       case Nil => t
       case ~("*", t1)::more => parseProduct(FunCall(Identifier("mul"), List(t, t1)), more)
       case ~("/", t1)::more => parseProduct(FunCall(Identifier("div"), List(t, t1)), more)
     }
   }

   def term: Parser[Expression]  = lambda | thunk | text | funCall | block | assignment | dereference | literal | "("~>expression<~")" // last term is closure
   
   def literal = boole | real | integer | chars | identifier
   
   // chars ::= any characters bracketed by quotes
   def chars: Parser[Chars] = """\"[^"]+\"""".r ^^ {
       case characters => Chars(characters.substring(1, characters.length - 1))
   }
   
   // integer ::= 0|(\+|-)?[1-9][0-9]*
   def integer: Parser[Integer] = """0|(\+|-)?[1-9][0-9]*""".r ^^ {
     case digitString => Integer(digitString.toInt)
   }
   
   // real ::= (\+|-)?[0-9]+\.[0-9]+
   def real: Parser[Real] = """(\+|-)?[0-9]+\.[0-9]+""".r ^^ {
     case number => Real(number.toDouble)
   }
   
   // boole ::= true | false
   def boole: Parser[Boole] = """(true|false)""".r ^^ {
     case flag => if(flag == "true") Boole(true) else Boole(false)
   }
  
   // identifier ::= [a-zA-Z][a-zA-Z0-9]*
   def identifier: Parser[Identifier] = """[a-zA-Z][a-zA-Z0-9]*""".r ^^ {
     case alphaNum => Identifier(alphaNum)
   }
   
   // funCall ::= identifier ~ operands
   def funCall: Parser[FunCall] = identifier ~ operands ^^ {
     case id ~ ops => FunCall(id, ops)
   }
   
   // operands ::= "(" ~ (expression ~ ("," ~ expression)*)? ~ ")"
   def operands: Parser[List[Expression]] = "(" ~> opt(expression ~ rep("," ~> expression)) <~ ")" ^^ {
     case None => Nil
     case Some(x ~ Nil) => List(x)
     case Some(x ~ exp) => x::exp
   }
}
