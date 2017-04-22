/**
  * Created by jgaylard on 21/04/17.
  */
class Bob {
  def hey(s: String) : String = s match {
    case s if (s == s.toUpperCase()) && hasAlpha(s)=> "Whoa, chill out!"
    case s if s.endsWith("?") => "Sure."
    case s if s.trim() == "" => "Fine. Be that way!"
    case _ => "Whatever."
  }

  private[this] def hasAlpha(s: String) : Boolean = {
    "[a-zA-Z]".r.findFirstIn(s).isDefined
  }
}
