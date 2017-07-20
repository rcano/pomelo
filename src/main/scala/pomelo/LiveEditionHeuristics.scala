package pomelo

import java.util.function.{Consumer, Predicate}
import javafx.scene.input.{KeyCode, KeyEvent, KeyCombination}
import org.fxmisc.richtext.CodeArea
import org.fxmisc.richtext.model.NavigationActions.SelectionPolicy
import org.fxmisc.wellbehaved.event.InputMap, InputMap.consume
import org.fxmisc.wellbehaved.event.EventPattern._

object LiveEditionHeuristics {

  type Heuristic = CodeArea => InputMap[KeyEvent]
  
  def All: Seq[Heuristic] = Seq(
    closingPair,
    indentation
  )
  
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
    ca => consume(
      keyTyped({ 
          case "{"|"("|"["|"<"|"\""|"'"|"`" => true
          case other => false
        }: Predicate[String], KeyCombination.SHIFT_ANY, KeyCombination.ALT_ANY,KeyCombination.SHORTCUT_ANY),
      { evt =>
        val ClosingPair(c) = evt.getCharacter.charAt(0)
        val pos = ca.getCaretPosition
        ca.insertText(pos, evt.getCharacter + c)
        ca.moveTo(pos + 1)
      }: Consumer[KeyEvent])
  }
  
  val indentation: Heuristic = { 
    ca => consume(
      keyPressed(KeyCode.ENTER),
      { evt =>
        val currentLine = ca.getParagraph(ca.getCurrentParagraph).getText
        if (currentLine.isEmpty) {
          ca.insertText(ca.getCaretPosition, "\n")
        } else {
          val indent = currentLine.iterator.takeWhile(' '.==).size
          val indentText = " " * indent
          val sb = new StringBuilder().append("\n").append(indentText)
          val shouldIncreaseIdent = currentLine.charAt(ca.getCaretColumn - 1) == '{'
          if (shouldIncreaseIdent) sb.append("  \n").append(indentText)
          ca.insertText(ca.getCaretPosition, sb.toString)
          if (shouldIncreaseIdent) {
            ca.lineStart(SelectionPolicy.CLEAR)
            ca.previousChar(SelectionPolicy.CLEAR)
          }
        }
      }: Consumer[KeyEvent])
  }
}
