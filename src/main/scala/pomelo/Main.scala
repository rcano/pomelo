package pomelo

import better.files._
import java.util.Collection
import java.util.concurrent.Executors
import javafx.application.{Application, Platform}
import javafx.concurrent.Task
import javafx.event.EventHandler
import javafx.geometry.{Orientation, Pos}
import javafx.scene.Scene
import javafx.scene.control._
import javafx.scene.layout._
import javafx.stage.{FileChooser, Popup}
import org.fxmisc.richtext.{CodeArea, LineNumberFactory, MouseOverTextEvent}
import org.fxmisc.flowless.VirtualizedScrollPane
import org.fxmisc.richtext.model.{StyleSpansBuilder}
import org.reactfx.EventStreams
import scala.meta._
import org.scalafmt.Scalafmt
import org.scalafmt.config.{ScalafmtConfig, ScalafmtRunner}

class Main extends Application {
  override def start(stage) = {
    stage.setTitle("Pomelo")
    
    val centerPane = new BorderPane
    
    // setup menu bar
    val menuBar = new MenuBar
    val fileMenu = new Menu("File")
    val editMenu = new Menu("Edit")
    menuBar.getMenus.addAll(fileMenu, editMenu)
    centerPane.setTop(menuBar)
    
    val openFile = new MenuItem("Open")
    val saveFile = new MenuItem("Save")
    val saveAsFile = new MenuItem("Save As...")
    val settings = new MenuItem("Settings")
    val exit = new MenuItem("Exit")
    fileMenu.getItems.addAll(openFile,
                             new SeparatorMenuItem(),
                             settings,
                             new SeparatorMenuItem(),
                             exit)
    
    val formatCode = new MenuItem("Format Code")
    editMenu.getItems.add(formatCode)
    
    //setup bottom status bar
    val statusBar = new HBox(4)
    statusBar.setAlignment(Pos.BOTTOM_RIGHT)
    val dialectComboBox = new ComboBox[Dialect]()
    dialectComboBox.getItems.addAll(Seq(dialects.Scala212, dialects.Scala211, dialects.Scala210, dialects.Dotty, dialects.Sbt0137, dialects.Sbt0136):_*)
    dialectComboBox.getSelectionModel.select(Dialect.current)
    val caretPositionLabel = new Label("0 - 1:0")
    caretPositionLabel.setPrefWidth(150)
    statusBar.getChildren.addAll(dialectComboBox, new Separator(Orientation.VERTICAL), caretPositionLabel)
    centerPane.setBottom(statusBar)
    
    //setup main area
    val codeArea = new CodeArea()
    codeArea.setParagraphGraphicFactory(LineNumberFactory.get(codeArea))
    codeArea.setMouseOverTextDelay(java.time.Duration.ofSeconds(1))
    centerPane.setCenter(new VirtualizedScrollPane(codeArea))

    
    //setup reaction to changes
    val dialectChanges = EventStreams.changesOf(dialectComboBox.getSelectionModel.selectedItemProperty)
    val stylesCalculator = Executors.newFixedThreadPool(1)
    val textChanges = codeArea.plainTextChanges.filter(ch => !(ch.getInserted() == ch.getRemoved())).successionEnds(java.time.Duration.ofMillis(150))
    EventStreams.merge(dialectChanges, textChanges).supplyTask (() => {
        val task = new Task[CodeArea => Unit] {
          override def call() = calculateStyleSpans(codeArea.getText(), dialectComboBox.getSelectionModel.getSelectedItem)
        }
        stylesCalculator submit task
        task
      }
    ).
    awaitLatest(codeArea.richChanges()).filterMap(_.toOptional).
    subscribe(_(codeArea))
    
    
    openFile.setOnAction { _ => 
      val fileChooser = new FileChooser()
      fileChooser.setTitle("Open File")
      fileChooser.getExtensionFilters().addAll(
        new FileChooser.ExtensionFilter("Scala files", "*.scala", "*.sbt"))
      val selectedFile = fileChooser.showOpenDialog(stage)
      if (selectedFile != null) try {
        selectedFile.toScala.extension.get match {
          case ".sbt" => dialectComboBox.getSelectionModel.select(dialects.Sbt0137)
          case other => dialectComboBox.getSelectionModel.select(dialects.Scala212)
        }
        codeArea.replaceText(0, codeArea.getText.length, selectedFile.toScala.contentAsString) 
      } catch { case e: Exception => notifyError(s"Failed reading file $selectedFile: $e")}
    }
    
    formatCode.setOnAction { _ =>
      val conf = ScalafmtConfig.default.copy(runner = ScalafmtRunner.default.copy(dialect = dialectComboBox.getSelectionModel.getSelectedItem))
      Scalafmt.format(codeArea.getText, conf).toEither.fold(ex => ex.printStackTrace, res => codeArea.replaceText(res))
    }

    val boundHeuristics = LiveEditionHeuristics.All.map(_(codeArea))
    codeArea.plainTextChanges.filter(_ != null).subscribe { evt =>
      evt.getInserted match {
        case null => //ignore
        case text if text.length == 1 =>
          val char = text.head
          boundHeuristics.foreach(h => if (h isDefinedAt char) h(char))
        case _ =>
      }
    }
//    codeArea.setOnKeyTyped { evt =>
//      val char = evt.getCharacter.head
//      boundHeuristics.foreach(h => if (h isDefinedAt char) h(char))
//    }
    codeArea.caretPositionProperty.addListener { (prop, o, n) =>
      caretPositionLabel.setText(s"${n} - ${codeArea.getCurrentParagraph + 1}:${codeArea.getCaretColumn}")
    }
    
    val scene = new Scene(centerPane, 800, 500, true)
    scene.getStylesheets().add(getClass.getResource("scala-syntax.css").toExternalForm())
    stage.setScene(scene)
    Platform.setImplicitExit(true)
    stage.show()
    
  }
  
  def notifyError(msg: String): Unit = {
    val dialog = new Alert(Alert.AlertType.ERROR, msg)
    dialog.showAndWait()
  }
  
  private var previousPopupHandler: EventHandler[MouseOverTextEvent] = _
  private val codeAreaPopup = new Popup()
  /**
   * calculates the style spans for the given text.
   * This method processes the style spans in the calling thread, but returns a function of CodeArea => Unit that will modify a given CodeArea
   * with the calculated styleSpans. This is compatible with CodeArea async styles calculation.
   */
  def calculateStyleSpans(text: String, dialect: Dialect): CodeArea => Unit = {
    text.tokenize match {
      case Tokenized.Success(tokens) =>
        val spans = tokens2StyleSpans(tokens)
        //now terms parsing
        val sourceTokens = tokens.parse[Source](implicitly, implicitly, dialect)
        codeArea => {
          codeArea.setStyleSpans(0, spans)
          sourceTokens match {
            case Parsed.Success(tree) =>
            case Parsed.Error(pos, msg, details) =>
              codeArea.setStyleClass(pos.start.offset, pos.end.offset, "error")
              val errorInputRange = pos.start.offset until pos.end.offset
              installErrorHandler(codeArea, msg, errorInputRange)
          }
        }
          
      case Tokenized.Error(pos, mesg, e) =>
        //if we found an error, colour everything up to the point where the error is,
        //then add the error
        val tokens = text.take(pos.start.offset - 1).tokenize.get
        val spans = tokens2StyleSpans(tokens)
          
        val lineLength = text.lines.drop(pos.end.line).take(1).next.length
        val startPos = pos.start.offset
        val endPos = pos.start.offset + (lineLength - pos.start.column)
        ca => {
          ca.setStyleSpans(0, spans)
          ca.setStyleClass(startPos, endPos, "error")
          installErrorHandler(ca, mesg, startPos until endPos)
        }
    }
  }
  
  private def installErrorHandler(codeArea: CodeArea, msg: String, charRange: Range): Unit = {
    if (previousPopupHandler != null) codeArea.removeEventHandler(MouseOverTextEvent.ANY, previousPopupHandler)
    previousPopupHandler = evt => {
      evt.getEventType match {
        case MouseOverTextEvent.MOUSE_OVER_TEXT_BEGIN if charRange.contains(evt.getCharacterIndex) =>
          println("displaying popup!")
          val l = new Label()
          l.getStyleClass add "code-area-popup"
          l setText msg
          codeAreaPopup.getContent add l
          codeAreaPopup.show(codeArea, evt.getScreenPosition.getX, evt.getScreenPosition().getY + 10)
        case MouseOverTextEvent.MOUSE_OVER_TEXT_END =>
          codeAreaPopup.getContent.clear()
          codeAreaPopup.hide()
                    
        case _ => //ignore other positions
      }
    }
    codeArea.addEventHandler(MouseOverTextEvent.ANY, previousPopupHandler)
  }
  private def tokens2StyleSpans(tokens: Tokens) = {
    val spans = new StyleSpansBuilder[Collection[String]]()
    tokens foreach (t => spans.add(ScalaToken2StyleClass(t), t.end - t.start))
    spans.create()
  }
}
