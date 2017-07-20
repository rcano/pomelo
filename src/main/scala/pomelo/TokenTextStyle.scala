package pomelo

import javafx.scene.paint.Color

case class TokenTextStyle(font: String,
                          color: Color = Color.DIMGRAY.darker,
                          backgroundColor: Color = Color.WHITE,
                          fontSize: Float = 1.0f,
                          bold: Boolean = false,
                          italic: Boolean = false,
                          strikethrough: Boolean = false,
                          underlined: Boolean = false) {

  def toCss = {
    def colorCss(c: Color) = s"rgba(${c.getRed * 100}%,${c.getGreen * 100}%,${c.getBlue * 100}%,${c.getOpacity})"
    val res = new StringBuilder
    res.append("-fx-font-family: " + font).append(";")
    res.append("-fx-font-weight: " + (if (bold) "bold" else "normal")).append(";")
    res.append("-fx-font-style: " + (if (italic) "italic" else "normal")).append(";")
    res.append("-fx-font-size: " + fontSize).append("em;")
    res.append(s"-rtfx-background-color: ${colorCss(backgroundColor)}").append(";")
    res.append(s"-fx-fill: ${colorCss(color)}").append(";")
    res.append("-fx-strikethrough: " + strikethrough).append(";")
    res.append("-fx-underline: " + underlined).append(";")
    res.toString()
  }
}
