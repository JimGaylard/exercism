/**
  * Created by jgaylard on 21/04/17.
  */
class Bob {
  def hey(s: String) : String = s match {
    case s if (s == s.toUpperCase())
      && s.exists(_.isLetter)=> "Whoa, chill out!"
    case s if s.endsWith("?") => "Sure."
    case s if s.trim.isEmpty => "Fine. Be that way!"
    case _ => "Whatever."
  }
}
