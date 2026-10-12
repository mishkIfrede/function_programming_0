package function_programming_0.matching
import function_programming_0.model.Rule

// Найденные значения полей (и их совпадения)
case class Match(rule: Rule, start: Int, end: Int, value: String)

object Matcher {
  // находждение всех совпадений для всех правил в исходном тексте
  def findAll(text: String, rules: List[Rule]): List[Match] = {
    // для каждого правила применяется ф-ция, где вызывается детектор, возвращает кортеж...
    // ... к кортежу применяется ф-ция, в результате которой возвращается объект Match.
    // итоговый результат - объект Match
    rules.flatMap { r =>
      Detector.find(text, r.search).map { case (s, e, v) => Match(r, s, e, v) }
    }
  }
}
