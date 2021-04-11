package fitnesse.idea.lexer

import java.util
import com.intellij.lexer.LexerBase
import com.intellij.psi.tree.IElementType
import fitnesse.wikitext._
import fitnesse.wikitext.parser._
import fitnesse.idea.lexer.FitnesseLexer._

import java.util.Optional
import scala.collection.JavaConversions._

class FitnesseLexer extends LexerBase {

  var buffer: CharSequence = null
  var startOffset: Int = 0
  var endOffset: Int = 0
  var state = 0

  var specification: ParseSpecification = null
  var scanner: Scanner = null
  var parser: Parser = null

  var symbolList: List[Symbol] = Nil

  override def start(buffer: CharSequence, startOffset: Int, endOffset: Int, initialState: Int): Unit = {
    this.buffer = buffer
    this.startOffset = startOffset
    this.endOffset = endOffset
    this.state = initialState

    val input: CharSequence = buffer.subSequence(startOffset, endOffset)
    val currentPage: ParsingPage = new LexerParsingPage

    specification = new ParseSpecification().provider(SymbolProvider.wikiParsingProvider)
    scanner = new Scanner(new TextMaker(currentPage, currentPage.getNamedPage), input)
    parser = new Parser(null, currentPage, scanner, specification)
    symbolList = Nil

    advance()
  }

  override def advance(): Unit = {
    state += 1
    symbolList = fetchNextSymbols()

    // if TABLE_START, ROW_START -> reduce to TABLE_START, fix offsets
    // if COLLAPSIBLE_START, title ->  fix offsets
    // drop all CELL_START | ROW_START -> advance
    // CELL_END, ROW_END -> advance to ROW_END
    // ROW_END, TABLE_END -> advance to TABLE_END

    symbolList match {
      case symbol :: _ if !symbol.hasOffset => advance()
      case LexerSymbol(FitnesseTokenType.ROW_END, _, _) :: LexerSymbol(FitnesseTokenType.TABLE_END, _, _) :: _ =>
        advance()
      case LexerSymbol(FitnesseTokenType.CELL_END, _, _) :: LexerSymbol(FitnesseTokenType.ROW_END, _, _) :: _ =>
        advance()
      case symbol :: _ => symbol.getType match {
        case SymbolType.SymbolList   => advance()
        case Table.tableRow          => advance()
        case Table.tableCell         => advance()
        case table: Table            =>
          // Fetch new simples to ensure the table contents are properly parsed.
          symbolList = LexerSymbol(FitnesseTokenType.TABLE_START, symbol.getStartOffset, symbol.getChildren.get(0).getStartOffset) :: fetchNextSymbols()
        case _: Collapsible            =>
          // Fetch new simples to ensure the collapsible contents are properly parsed.
          val title :: tail = fetchNextSymbols()
          val titleEnd = title.getChildren.last.getEndOffset
          val startAfter = previousEnd(symbol, titleEnd, title.getEndOffset)
          symbolList = LexerSymbol(FitnesseTokenType.COLLAPSIBLE_START, symbol.getStartOffset, title.getStartOffset) :: title :: LexerSymbol(FitnesseTokenType.LINE_TERMINATOR, startAfter, startAfter + 1) :: tail
        case _ =>
      }
      case _ =>
    }
  }

  // Lazily evaluate the
  private def fetchNextSymbols(): List[Symbol] = {
    def parseSymbol: List[Symbol] = {
      specification.parseSymbol(parser, scanner) match {
        case parsedSymbol if parsedSymbol.isNothing =>
          Nil
        case parsedSymbol =>
          parsedSymbol.getValue :: Nil
      }
    }

    symbolList match {
      case symbol :: tail if symbol.getType eq SymbolType.SymbolList =>
        symbol.getChildren.toList ::: tail
      case symbol :: tail =>
        FitnesseLexer.terminatorFor(symbol) match {
          case Some(endSymbol) => symbol.getChildren.toList ::: endSymbol :: tail
          case None =>
            tail match {
              case Nil =>
                parseSymbol
              case _ =>
                tail
            }
        }
      case Nil =>
        parseSymbol
    }
  }

  override def getTokenType: IElementType = {
    symbolList match {
      case Nil => null
      case LexerSymbol(elementType, _, _) :: _ => elementType
      case symbol :: _ => symbol.getType match {
        case _: WikiWord           => FitnesseTokenType.WIKI_WORD
        case _: Collapsible        => FitnesseTokenType.COLLAPSIBLE_START
        case SymbolType.Bold       => FitnesseTokenType.BOLD
        case SymbolType.Italic     => FitnesseTokenType.ITALIC
        case SymbolType.Whitespace => FitnesseTokenType.WHITE_SPACE
        case SymbolType.Newline    => FitnesseTokenType.LINE_TERMINATOR
        case _: Table              => FitnesseTokenType.TABLE_START
        case Table.tableRow        => FitnesseTokenType.ROW_START
        case Table.tableCell       => FitnesseTokenType.CELL_START
        case SymbolType.Colon      => FitnesseTokenType.COLON
        case _                     => FitnesseTokenType.WORD
      }
    }
  }

  override def getTokenStart: Int = symbolList match {
    case symbol :: _ => symbol.getStartOffset
    case Nil => throw new IllegalStateException("case Nil should have been covered by getTokenType()")
  }

  override def getTokenEnd: Int = symbolList match {
    case symbol :: _ => symbol.getEndOffset
    case Nil => throw new IllegalStateException("case Nil should have been covered by getTokenType()")
  }

  override def getState: Int = state

  override def getBufferEnd: Int = buffer.length

  override def getBufferSequence: CharSequence = buffer
}

object FitnesseLexer {

  case class LexerSymbol(elementType: IElementType, start: Int, end: Int) extends Symbol(new SymbolType(elementType.toString), "", start, end)

  def terminatorFor(symbol: Symbol): Option[Symbol] = {
    val lastNestedChild : Symbol = lastChild(symbol)
    val symbolEndOffset = symbol.getEndOffset
    val startOffset = if (lastNestedChild.getEndOffset == symbolEndOffset) lastNestedChild.getStartOffset else lastNestedChild.getEndOffset
    val previousEndOffset = previousEnd(symbol, startOffset, symbolEndOffset)

    symbol.getType match {
      case _ : Table                 => Some(LexerSymbol(FitnesseTokenType.TABLE_END, previousEndOffset, symbolEndOffset))
      case _ : Collapsible           => {
        val childPreviousEndOffset = previousEnd(symbol.getChildren.last, startOffset, symbol.getChildren.last.getEndOffset)
        val collEndStart = if (symbolEndOffset - childPreviousEndOffset > 1) childPreviousEndOffset else lastNestedChild.getEndOffset
        Some(LexerSymbol(FitnesseTokenType.COLLAPSIBLE_END, collEndStart, symbolEndOffset))
      }
      case s if s eq Table.tableRow  => Some(LexerSymbol(FitnesseTokenType.ROW_END, previousEndOffset, symbolEndOffset))
      case s if s eq Table.tableCell => Some(LexerSymbol(FitnesseTokenType.CELL_END, previousEndOffset, symbolEndOffset))
      case _ => None
    }
  }

  private def lastChild(symbol: Symbol): Symbol = {
    val children = symbol.getChildren
    if (children.isEmpty || children.exists(_.getStartOffset == -1)) symbol else lastChild(children.last)
  }

  private def previousEnd(symbol: Symbol, previousMax: Int, endOffset: Int): Int = {
    val children = symbol.getChildren
    val symbolEnd = symbol.getEndOffset
    val currentEnd = if ((symbolEnd < endOffset) && (symbolEnd > previousMax)) symbolEnd else previousMax
    if (children.isEmpty || children.exists(_.getStartOffset == -1)) currentEnd else previousEnd(children.last, currentEnd, endOffset)
  }
}

class LexerParsingPage extends ParsingPage(new LexerSourcePage) {

  override def copyForNamedPage(namedPage: SourcePage): ParsingPage = {
    throw new IllegalStateException("FitNesse plugin: method LexerParsingPage.copyForNamedPage() has not been implemented")
  }

  override def putVariable(name: String, value: String): Unit = {
    super.putVariable(name, value)
  }

  override def findVariable(name: String): Optional[String] = {
    super.findVariable(name)
  }
}


class LexerSourcePage extends SourcePage {
  override def getName: String = {
    "NAME"
  }

  override def getFullName: String = {
    "FULL_NAME"
  }

  override def getPath: String = {
    // TODO: We need to know where the FitNesseRoot is, so we can determine a page path (wiki page name)
    "PATH_PLACEHOLDER"
     //page.getPageCrawler.getFullPath.parentPath.toString
  }

  override def getFullPath: String = {
    // page.getPageCrawler.getFullPath.toString
    throw new IllegalStateException("FitNesse plugin: method LexerParsingPage.getFullPath() has not been implemented")
  }

  override def getContent: String = {
    throw new IllegalStateException("FitNesse plugin: method LexerParsingPage.getContent() has not been implemented")
  }

  override def targetExists(wikiWordPath: String): Boolean = {
    false
  }

  override def makeFullPathOfTarget(wikiWordPath: String): String = {
    null
  }

  override def findParentPath(targetName: String): String = {
    null
  }

  override def findIncludedPage(pageName: String): Maybe[SourcePage] = {
    Maybe.nothingBecause("not in this context")
  }

  override def getChildren = {
    List.empty[SourcePage]
  }

  override def hasProperty(propertyKey: String): Boolean = {
    false
  }

  override def getProperty(propertyKey: String): String = {
    throw new IllegalStateException("FitNesse plugin: method LexerParsingPage.getProperty() has not been implemented")
  }

  override def compareTo(o: SourcePage): Int = {
    throw new IllegalStateException("FitNesse plugin: method LexerParsingPage.compareTo() has not been implemented")
  }

}
