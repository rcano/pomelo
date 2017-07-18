package pomelo

import scala.collection.JavaConverters._
import scala.meta.tokens.Token, Token._

object ScalaToken2StyleClass {
  def apply(token: Token): java.util.List[String] = {
    val tokenName = token match {
      case _: Constant.Char | _: Constant.Double | _: Constant.Float | _: Constant.Int
        | _: Constant.Long | _: Constant.String | _: Constant.Symbol => "constant-" + token.productPrefix
        
      case _: Interpolation.End | _: Interpolation.Id | _: Interpolation.Part | _: Interpolation.SpliceEnd | _: Interpolation.SpliceStart |
         _: Interpolation.Start => "interpolation-" + token.productPrefix
         
      case _ => token.productPrefix
    }
    (Seq("scala-syntax-" + tokenName) ++ Some("scala-syntax-keyword").filter(_ => tokenName startsWith "Kw")).asJava
  }
}
