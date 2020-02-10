
import scala.io.Source
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.Stack 
import scala.util.parsing.combinator._
import scala.util.parsing.combinator.RegexParsers
import scala.util.parsing.input.{Position, Positional}
import scala.collection.immutable.HashSet
import scala.io.StdIn
import scala.util.matching.Regex
import scala.util.Failure
import org.parboiled2._

object Words {

var listTokens = ListBuffer[String]()
var sTokens = ""

    def main(args:Array[String]) {
      //tabla de ids implementar
    var b = false
    var input = Source.fromFile("Code.txt").getLines.mkString
      try {
        if (input != null) {
          val tokens = Lexico(input)
          for(t<-tokens){
            var temp = t.toString()
            if(t.toString() == "ParIzq(()"){
              temp = "ParIzq"
            }
            else if(t.toString() == "ParDer())"){
              temp = "ParDer"
            }
            else{
              temp = """\(([^\)]+)\)""".r.replaceAllIn(temp,"")
            }
            println(t)
            sTokens = sTokens + temp + " " 
          }
        }
      }
      catch {
        case e: LexError =>
          b=true
          println(s"Error lexico: ${e.msg} en la linea ${e.pos.line}, columna ${e.pos.column}")
          if (e.pos.line > 0) {
            println(input.lines.toList(e.pos.line - 1))
            println("-" * (e.pos.column - 1) + "^")
          }
      }
      val p = new Sintactico(sTokens)
      if (p.insMain.run().toString() == "Success(())"){
        println("Compilación exitosa")
      }
      else{
        val Failure(err: ParseError) = p.insMain.run()
        if (!b){
          var rep = p.formatError(err).toString()
          var uei = """Unexpected end of input""".r; rep = uei.replaceFirstIn(rep, "Final de entrada inesperado")
          uei = """expected""".r; rep = uei.replaceFirstIn(rep, "se esperaba:")
          uei = """Invalid input""".r; rep = uei.replaceFirstIn(rep, "Entrada invalida")
          uei = """line""".r; rep = uei.replaceFirstIn(rep, "linea")
          uei = """column""".r; rep = uei.replaceFirstIn(rep, "columna")
          println(rep)
        }
        
      }
    }
  }
    

object Lexico extends RegexParsers{

  override def skipWhitespace = false

  def char: Parser[Char] = ("""[^"\\]""".r | '\\' ~> ".".r) ^^ { _.head }
  def cadena: Parser[Token] = "\"" ~> rep(char) <~ "\"" ^^ (chars => Cadena(chars.mkString))
  def operasign: Parser[Token] = """=""".r ^^ OperAsign
  def numero: Parser[Token] = """\d+(\.\d+)?""".r ^^ (s => Numero(s.toDouble))
  def tipodato: Parser[Token] = """int | float|string|uint|decimal|double|short|sbyte|bool|char|long|ulong|
                                tinyInt|const|date|volatile""".r ^^ TipoDato
  def progmain: Parser[Token] = """main""".r ^^ ProgMain
  def operarit: Parser[Token] = """[\+ | \- | \* | / | \% | \^]""".r ^^ OperArit
  def llaveizq: Parser[Token] = """\{""".r ^^ LlaveIzq 
  def llaveder: Parser[Token] = """\}""".r ^^ LlaveDer
  def parizq: Parser[Token] = """\(""".r ^^ ParIzq
  def parder: Parser[Token] = """\)""".r ^^ ParDer
  def corizq: Parser[Token] = """\[""".r ^^ CorIzq
  def corder: Parser[Token] = """\]""".r ^^ CorDer
  def puntuador: Parser[Token] = """[; | , | \. | \:]""".r ^^ Puntuador
  def operunario: Parser[Token] = ("""(\+\+)""".r | """\-\-""".r) ^^ OperUnario
  def operrelacional: Parser[Token] = ("=<".r|">=".r|"<".r | ">".r|"==".r|"!=".r|"!!".r|"<>".r) ^^ OperRelacional
  def operlambda: Parser[Token] = ("""<=|=>""").r ^^ OperLambda
  def operio: Parser[Token] = "<<|>>".r ^^ OperIO
  def operlogico: Parser[Token] = ("""\! | \& | \|""").r ^^ OperLogico
  def condicion: Parser[Token] = """\?|switch""".r ^^ Condicion
  def opcion: Parser[Token] = """else|case|finally|unless""".r ^^ Opcion
  def estdatos: Parser[Token] = """list|stack|queue|tree""".r ^^ EstDatos
  def imprimir: Parser[Token] = "cout".r ^^ Imprimir
  def ciclo: Parser[Token] = "do|while|foreach|for|until".r ^^ Ciclo
  def modacceso: Parser[Token] = "public|private|static|internal|protected|global|super|unsp".r ^^ ModAcceso
  def booleano: Parser[Token] = "true|false".r ^^ Booleano
  def clase: Parser[Token] = "Class|Struct|Math|Date|Exception|Thread|Timer|Object".r ^^ Clase
  def manerror: Parser[Token] = "throw|try|catch|finally|lock".r ^^ ManError
  def excepcion: Parser[Token] = """OverflowException|OutOfRangeException|DivideByZeroException|IndexOutOfRangeException
                                NullReferenceException|InvalidOperationException|ArgumentNullException|
                                ArgumentOutOfRangeException""".r ^^ Excepcion
  def metmath: Parser[Token] = "Round|Sqrt|Row|Ceil|Floor|Sin|Cos|Max|Min|Tan|Truncate|Log|Pi|Arcot|Cosec|Cotan|Arsec".r ^^ MetMath
  def optipo: Parser[Token] = "typeof".r ^^ OpTipo
  def metcheck: Parser[Token] = "checked|unchecked".r ^^ MetCheck
  def propdate: Parser[Token] = "Now|Year|Month|Day|Hour|Minute".r ^^ PropDate
  def leer: Parser[Token] = "cin".r ^^ Leer
  def spagg: Parser[Token] = "goto".r ^^ Spagg
  def valor: Parser[Token] = "null|void|value".r ^^ Valor
  def ovmet: Parser[Token] = "override".r ^^ OvMet
  def nuevo: Parser[Token] = "Nuevo".r ^^ Nuevo
  def referenciador: Parser[Token] = "ref|using|this|from".r ^^ Referenciador
  def metconversor: Parser[Token] = "toInt16|toInt32|toInt64|toString|toDecimal|toDouble|toBoolean|toDate".r ^^ MetConversor
  def metorden: Parser[Token] = "quickSort|bubbleSort".r ^^ MetOrden
  def funcion: Parser[Token] = "reserved|step".r ^^ Funcion
  def mettipo: Parser[Token] = """isAlphaNumeric|isAlpha|isDigit|isAscii|isControl|isGraph|isHexadecimal|isLowerCase|
                                isPrintable|isPunct|isSpace|isString|isUpperCase|isWhiteSpace|isNull|isInt|
                                isChar|virtual""".r ^^ MetTipo
  def metbyte: Parser[Token] = """bitClear|bitReal|bitSet|bitWrite|bit|highByte|lowByte""".r ^^ MetByte
  def metarreglo: Parser[Token] = "reverse|unshift|shift|sortSplice|concat|slice|ndim|asType|view|containsValue".r ^^ MetArreglo
  def metgrafico: Parser[Token] = """setColor|getColor|fill|drawString|getFontMetrics|setFont|getFont|color|
                                  paint|polygon|point|drawLine|drawRectangle|fillRectangle|drawRoundRect|
                                  fillRoundRect|drawOval|fillOval|drawPolygon|moveTo|lineTo|closePath|
                                  updateGraph|rePaint|rotate|translate|move""".r ^^ MetGrafico
  def metfuente: Parser[Token] = "getStyle|getName|getFamily|isPlain|isBold|isItalic".r ^^ MetFuente
  def importador: Parser[Token] = "import".r ^^ Importador
  def metlista: Parser[Token] = "add|first|last|dropRight|drop|item|lenght|key".r ^^ MetLista
  def metformato: Parser[Token] = "format".r ^^ MetFormato
  def conjunto:Parser[Token] = "unionAll|union|intersect|except".r ^^ Conjunto
  def metrol: Parser[Token] = "grant|revoke".r ^^ MetRol
  def momento: Parser[Token] = "before|after".r ^^ Momento
  def llamar: Parser[Token] = "call".r ^^ Llamar
  def consulta: Parser[Token] = "select|from|where|orderby|on|delete|by|ascending|descending".r ^^ Consulta

  val keywords = HashSet("int", "float" ,"string", "uint", "decimal","double","short","sbyte","bool","char","long","ulong",
  "tinyInt","const","date","volatile")

  def id: Parser[Token] = """[a-zA-Z][a-zA-Z0-9_]*""".r ^^ {
    case s if keywords.contains(s) => TipoDato(s)
    case s => ID(s)
  }

  def salta: Parser[Unit] = rep(whiteSpace | comment) ^^^ ()
  def comment: Parser[Unit] = comentario | multiComment

  def token: Parser[Token] = positioned(cadena | operlambda | operio| operrelacional | operlogico |metconversor|imprimir
  |metorden|funcion|mettipo|metbyte|metarreglo|metgrafico|metfuente| operasign | numero | llaveizq | 
  llaveder | parizq | parder | corizq | corder|tipodato | progmain | importador|metformato|conjunto|metrol|momento|llamar|consulta|
  puntuador| operunario | operarit | excepcion|condicion|opcion|estdatos|modacceso|ciclo|
  booleano|clase|manerror|metmath|optipo|propdate|leer|spagg|metlista|valor|ovmet|nuevo|referenciador| id)

  def program: Parser[List[Token]] = salta ~> rep(token <~ salta) <~ eof
  def eof: Parser[String] = "\\z".r | failure("caracter inesperado")

  def comentario: Parser[Unit] = "//" ~ rep(not("\n") ~ ".".r) ^^^ ()
  def multiComment: Parser[Unit] = "/*" ~ rep(not("*/") ~ "(?s).".r) ~ "*/" ^^^ ()
 
  def apply(input: String): List[Token] = parseAll(program, input) match {
    case Success(result, _) => result
    case NoSuccess(msg, next) => throw LexError(msg, next.pos)
  }
}

class Sintactico(val input: ParserInput) extends Parser{
  
  def insMain = rule {atomic("ProgMain") ~ WS ~ atomic("LlaveIzq") ~ WS ~ insBloque.named("Instrucciones de Main") ~ WS ~ atomic("LlaveDer") ~ WS ~ EOI.named("Fin de programa")}
  
  def insBloque:Rule0  = rule {zeroOrMore(insIf | insSwitch | insAsignacion | insUnario|insImprimir|insLeer|insFuncion)}

  def insBloqueFun:Rule0  = rule {zeroOrMore(insIf | insSwitch | insAsignacion | insUnario|insImprimir|insLeer)}
  
  def insIf = rule{atomic("ParIzq") ~ WS ~ insCondicion ~ WS ~ atomic("ParDer") ~ WS ~ atomic("Condicion") ~ WS ~ insBloque ~ WS ~ atomic("LlaveDer") ~ WS ~ insElse}
  
  def insCondicion = rule{insValComp~ WS ~ atomic("OperRelacional") ~ WS ~ insValComp}
  
  def insValComp = rule{atomic("ID")|atomic("Numero")|atomic("Cadena")}
  
  def insSwitch = rule{atomic("ParIzq") ~ WS ~ atomic("ID") ~ WS ~ atomic("ParDer") ~ WS ~ atomic("Condicion").named("switch") ~ WS ~ oneOrMore(atomic("Opcion") ~ WS ~ atomic("OperLambda") 
                  ~ WS ~ insBloque) ~ WS ~ atomic("LlaveDer")}
  
  def insOperAr:Rule0 = rule{((atomic("ID") | atomic("Numero")) ~ WS ~ atomic("OperArit") ~ WS ~ (atomic("ID") | atomic("Numero"))) ~ WS ~ zeroOrMore(atomic("OperArit") ~ WS ~ insOperAr)}
  
  def insAsignacion = rule{ (atomic("TipoDato") ~ WS ~ atomic("ID") | atomic("ID")) ~ WS ~ atomic("OperAsign") ~ WS ~ (insOperAr | insUnario |insConcat| atomic("ID") | atomic("Numero") | atomic("Cadena"))}
  
  def insLeer = rule{ atomic("Leer") ~ WS ~ atomic("OperIO") ~ WS ~ atomic("ID")}
  
  def insImprimir = rule {atomic("Imprimir") ~ WS ~ atomic("OperIO") ~ WS ~ (atomic("Numero") | atomic("Cadena") | atomic("ID"))}
  
  def insElse = rule {zeroOrMore(atomic("Opcion") ~ WS ~ insBloque ~ WS ~ atomic("LlaveDer"))}
  
  def insUnario = rule { ((atomic("Numero") | atomic("ID")) ~ WS ~ atomic("OperUnario"))}
  
  def insConcat:Rule0 = rule{(atomic("Cadena") | atomic("ID")) ~ WS ~ atomic("OperArit") ~ WS ~ (atomic("Cadena") | atomic("ID")) ~ WS ~ zeroOrMore(atomic("OperArit") ~ WS ~ insConcat)}

  def insImport = rule {atomic("Importador") ~ WS ~ atomic("ID")}

  def insFuncion = rule {atomic("ModAcceso") ~ WS ~ atomic("TipoDato") ~ WS ~ atomic("ID") ~ WS ~ atomic("CorIzq") ~ WS ~ insParametros ~ WS ~ atomic("CorDer") ~ WS ~ insBloqueFun ~ WS ~ atomic("LlaveDer") }

  def insParametros:Rule0 = rule { zeroOrMore(atomic("TipoDato") ~ WS ~ atomic("ID") ~ WS ~ zeroOrMore(insMasParametros))}

  def insMasParametros = rule { atomic("Puntuador") ~ WS ~ insParametros}

  def WS = rule { quiet(zeroOrMore(anyOf(" \t \n"))) }

}

  abstract sealed class Token extends Positional
  case class Cadena(value: String) extends Token
  case class OperAsign(value:String) extends Token
  case class Numero(value: Double) extends Token
  case class ID(id: String) extends Token
  case class TipoDato(value: String)  extends Token
  case object Comentario extends Token
  case class ProgMain(value:String) extends Token
  case class OperArit(value:String) extends Token
  case class LlaveIzq(value:String) extends Token
  case class LlaveDer(value:String) extends Token
  case class ParIzq(value:String) extends Token
  case class ParDer(value:String) extends Token
  case class CorIzq(value:String) extends Token
  case class CorDer(value:String) extends Token
  case class Puntuador(value:String) extends Token
  case class OperUnario(value:String) extends Token
  case class OperIO(value:String) extends Token
  case class OperLogico(value:String) extends Token
  case class OperLambda(value:String) extends Token
  case class OperRelacional(value:String) extends Token
  case class Condicion(value:String) extends Token
  case class Opcion(value:String) extends Token
  case class EstDatos(value:String) extends Token
  case class Imprimir(value:String) extends Token
  case class Ciclo(value:String) extends Token
  case class ModAcceso(value:String) extends Token
  case class Booleano(value:String) extends Token
  case class Clase(value:String) extends Token
  case class ManError(value:String) extends Token
  case class Excepcion(value:String) extends Token
  case class MetMath(value:String) extends Token
  case class OpTipo(value:String) extends Token
  case class MetCheck(value:String) extends Token
  case class PropDate(value:String) extends Token
  case class Leer(value:String) extends Token
  case class Spagg(value:String) extends Token
  case class Valor(value:String) extends Token
  case class OvMet(value:String) extends Token
  case class Nuevo(value:String) extends Token
  case class Referenciador(value:String) extends Token
  case class MetConversor(value:String) extends Token
  case class MetOrden(value:String) extends Token
  case class Funcion(value:String) extends Token
  case class MetTipo(value:String) extends Token
  case class MetByte(value:String) extends Token
  case class MetArreglo(value:String) extends Token
  case class MetGrafico(value:String) extends Token
  case class MetFuente(value:String) extends Token
  case class MetLista(value:String) extends Token
  case class Importador(value:String) extends Token
  case class MetFormato(value:String) extends Token
  case class Conjunto(value:String) extends Token
  case class Momento(value:String) extends Token
  case class Llamar(value:String) extends Token
  case class Consulta(value:String) extends Token
  case class MetRol(value:String) extends Token

  case class LexError(msg: String, pos: Position) extends Exception(msg)