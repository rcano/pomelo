package pomelo

import org.fxmisc.richtext.CodeArea
import org.fxmisc.richtext.model.NavigationActions.SelectionPolicy

object LiveEditionHeuristics {

  type Heuristic = CodeArea => PartialFunction[Char, Unit]
  
  def All: Seq[Heuristic] = Seq(
    closingPair,
    indentation)
  
  private object ClosingPair {
    def unapply(c: Char) = c match {
      case '{' => Some("}")
      case '(' => Some(")")
      case '[' => Some("]")
      case '<' => Some(">")
      case '"' => Some("\"")
      case '\'' => Some("'")
      case '`' => Some("`")
      case _ => None
    }
  }
  val closingPair: Heuristic = {
    var entered = false //need to detect reentrancy to avoid SO on chars like " or '
    ca => { 
      case ClosingPair(c) if !entered => 
        entered = true
        val pos = ca.getCaretPosition
        ca.insertText(pos + 1, c)
        ca.moveTo(pos)
        entered = false
    }
  }
  
  val indentation: Heuristic = { 
    var entered = false //need to detect reentrancy to avoid SO additional newlines
    ca => {
      case '\n' if !entered =>
        entered = true
        val currentLine = ca.getParagraph(ca.getCurrentParagraph).getText
        val indent = currentLine.iterator.takeWhile(' '.==).size
        val indentText = " " * indent
        if (currentLine.charAt(ca.getCaretColumn - 1) == '{') {
          ca.insertText(ca.getCaretPosition + 1, indentText + "  \n" + indentText)
        } else {
          ca.insertText(ca.getCaretPosition + 1, indentText)
        }
        javafx.application.Platform.runLater { () => ca.lineEnd(SelectionPolicy.CLEAR) } //needs to be delayed for the natural enter and cursor positioning to happen
        entered = false
    }
  }
}
