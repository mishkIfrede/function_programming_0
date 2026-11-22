package cli
import scala.io.StdIn

trait UserInterface {
    def printLine(line: String): Unit

    def readString(): String

    def readInt(): Int

    def readString(prompt: String): String = {
      printLine(prompt)
      readString()
    }

    def readInt(prompt: String): Option[Int] =
      readString(prompt).trim.toIntOption

//    def readInt(prompt: String, from: Int, to: Int): Either[AppError, Int] =
//    val raw = ui.readString(prompt).trim
//    raw.toIntOption match
//    case Some(n) if n >= from && n <= to => Right(n)
//    case Some(n)                         => Left(ParseError.OutOfRange(n, from, to))
//    case None                            => Left(ParseError.NotANumber(raw))
}

case object CLI extends UserInterface {
  override def printLine(line: String): Unit = println(line)

  override def readString(): String = StdIn.readLine()

  override def readInt(): Int = StdIn.readInt()
}
