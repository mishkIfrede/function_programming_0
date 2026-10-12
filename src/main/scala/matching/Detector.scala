package function_programming_0.matching
import function_programming_0.model.SearchMethod

// Детектор персональных данных в тексте
object Detector {
  // регулярные выражения для различных видов данных (примерные)
  private val emailRe = """[A-Za-z0-9._%+\-]+@[A-Za-z0-9.\-]+\.[A-Za-z]{2,}""".r
  private val phoneRe = """(?:\+?\d[\d\-\s()]{7,}\d)""".r
  private val ipv4Re = """\b(?:(?:25[0-5]|2[0-4]\d|[01]?\d?\d)\.){3}(?:25[0-5]|2[0-4]\d|[01]?\d?\d)\b""".r
  private val urlRe = """https?://[^\s<>"']+""".r
  // ключ: password / pwd / pass / secret / token / api_key / apikey / access_key
  private val pwdRe = """(?i)(?:password|passwd|pwd|secret)\s*[:=]\s*["']?([^\s"';,]+)["']?""".r
  private val tokenRe = """(?i)(?:token|api[_-]?key|access[_-]?key|secret[_-]?key)\s*[:=]\s*["']?([A-Za-z0-9_\-\.]{8,})["']?""".r

  def find(text: String, search: SearchMethod): List[(Int, Int, String)] = search match {
    case SearchMethod.Exact(v) if v.nonEmpty =>
      findAllExact(text, v)

    case SearchMethod.Regex(pat, flags) =>
      val caseInsensitive = flags.contains("i")
      val re = if (caseInsensitive) ("(?i)" + pat).r else pat.r
      re.findAllMatchIn(text).map(m => (m.start, m.end, m.matched)).toList

    case SearchMethod.Dictionary(values) =>
      values.filter(_.nonEmpty).flatMap(v => findAllExact(text, v)).sortBy(_._1)

    case SearchMethod.Builtin(kind) =>
      kind.toLowerCase match { // нормализуем регистр строки
        case "email"    => regexMatches(text, emailRe)
        case "phone"    => regexMatches(text, phoneRe).filter { case (_, _, v) => v.count(_.isDigit) >= 7 } // отсечение строк вида "1-2-3-4-5-6-7"
        case "ipv4"     => regexMatches(text, ipv4Re)
        case "url"      => regexMatches(text, urlRe)
        case "password" => captureGroupMatches(text, pwdRe)
        case "token"    => captureGroupMatches(text, tokenRe)
        case other      => Nil
      }
  }
  // находим все вхождения подстроки вручную
  // функция возвращает список кортежей вида (индекс вхождения, индекс+длина строки, искомое значение)
  private def findAllExact(text: String, value: String): List[(Int, Int, String)] = {
    val res = scala.collection.mutable.ArrayBuffer.empty[(Int, Int, String)]
    var idx = text.indexOf(value)
    while (idx >= 0) {
      res += ((idx, idx + value.length, value))
      idx = text.indexOf(value, idx + value.length)
    }
    res.toList
  }
  // находим все вхождения подстроки через Regex, преобразуем в необходимый формат
  private def regexMatches(text: String, re: scala.util.matching.Regex): List[(Int, Int, String)] =
    re.findAllMatchIn(text).map(m => (m.start, m.end, m.matched)).toList

  // находим все вхождения подстроки для паролей и токенов (capture group = 1)
  private def captureGroupMatches(text: String, re: scala.util.matching.Regex): List[(Int, Int, String)] =
    re.findAllMatchIn(text).flatMap { m =>
      if (m.groupCount >= 1 && m.group(1) != null) Some((m.start(1), m.end(1), m.group(1)))
      else None
    }.toList
}
