package pomelo

import java.util.function.{Consumer, Predicate}
import javafx.scene.input.{KeyCode, KeyEvent, KeyCombination}
import org.fxmisc.richtext.CodeArea
import org.fxmisc.richtext.model.NavigationActions.SelectionPolicy
import org.fxmisc.wellbehaved.event.InputHandler
import org.fxmisc.wellbehaved.event.InputMap, InputMap._
import org.fxmisc.wellbehaved.event.EventPattern._

object LiveEditionHeuristics {

  type Heuristic = CodeArea => InputMap[KeyEvent]
  
  def All: Seq[Heuristic] = Seq(
    closingPair,
    indentation,
    ignoreClosingPair,
    deleteClosingPair
  )
  
  val closingPair: Heuristic = ca => consume(
    keyTyped({ 
        case "{"|"("|"["|"<"|"\""|"'"|"`" => true
        case other => false
      }: Predicate[String], KeyCombination.SHIFT_ANY, KeyCombination.ALT_ANY, KeyCombination.SHORTCUT_ANY),
    { evt =>
      val c = evt.getCharacter.charAt(0) match {
        case '{' => '}'
        case '(' => ')'
        case '[' => ']'
        case '<' => '>'
        case '"' => '"'
        case '\'' => '\''
        case '`' => '`'
      }
      val pos = ca.getCaretPosition
      ca.insertText(pos, evt.getCharacter + c)
      ca.moveTo(pos + 1)
    }: Consumer[KeyEvent])
  
  val indentation: Heuristic = ca => consume(
    keyPressed(KeyCode.ENTER),
    { evt =>
      val currentLine = ca.getParagraph(ca.getCurrentParagraph).getText
      if (currentLine.isEmpty) {
        ca.insertText(ca.getCaretPosition, "\n")
      } else {
        val indent = currentLine.iterator.takeWhile(' '.==).size
        val indentText = " " * indent
        val sb = new StringBuilder().append("\n").append(indentText)
        val shouldIncreaseIdent = ca.getCaretColumn > 0 && currentLine.charAt(ca.getCaretColumn - 1) == '{'
        if (shouldIncreaseIdent) sb.append("  \n").append(indentText)
        ca.insertText(ca.getCaretPosition, sb.toString)
        if (shouldIncreaseIdent) {
          ca.lineStart(SelectionPolicy.CLEAR)
          ca.previousChar(SelectionPolicy.CLEAR)
        }
      }
    }: Consumer[KeyEvent])
  
  val ignoreClosingPair: Heuristic = ca => process(
    keyTyped({ 
        case "}"|")"|"]"|">"|"\""|"'"|"`" => true
        case other => false
      }: Predicate[String], KeyCombination.SHIFT_ANY, KeyCombination.ALT_ANY, KeyCombination.SHORTCUT_ANY),
    { evt =>
      val currentLine = ca.getParagraph(ca.getCurrentParagraph).getText
      if (currentLine.length != 0 && currentLine.length > ca.getCaretColumn && 
          currentLine.charAt(ca.getCaretColumn) == evt.getCharacter.charAt(0)) {
        ca.nextChar(SelectionPolicy.CLEAR) //just move the cursor, instead of adding a character
        InputHandler.Result.CONSUME
      } else {
        InputHandler.Result.PROCEED
      }
    }: java.util.function.Function[KeyEvent, InputHandler.Result])
  
  val deleteClosingPair: Heuristic = ca => process(
    keyPressed(KeyCode.BACK_SPACE),
    { evt => 
      val currentLine = ca.getParagraph(ca.getCurrentParagraph).getText
      if (currentLine.length != 0 && ca.getCaretColumn > 0 && currentLine.length > ca.getCaretColumn) {
        (currentLine.charAt(ca.getCaretColumn - 1), currentLine.charAt(ca.getCaretColumn)) match {
          case ('{', '}') |
            ('(', ')') |
            ('[', ']') |
            ('<', '>') |
            ('"', '"') |
            ('\'', '\'') |
            ('`', '`') => ca.deleteNextChar()
          case _ =>
        }
      }
      InputHandler.Result.PROCEED
    }: java.util.function.Function[KeyEvent, InputHandler.Result])
}
