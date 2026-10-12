package function_programming_0.matching
import function_programming_0.report.Logger
// "Разрешатель" конфликтов совпадений правил для полей текста
// объект конфликта
case class Conflict(start: Int, end: Int, matches: List[Match])
// результат решения конфликта
case class ResolutionResult(
                             accepted: List[Match], // "победившие" совпадения
                             conflicts: List[Conflict] // нерешённые конфликты
                           )

object ConflictResolver {
  // решение конфликта
  def resolve(all: List[Match]): ResolutionResult = {
    val sorted = all.sortBy(m => (m.start, -m.end, -m.rule.priority, m.rule.id)) // сортируем массив совпадений для детерминизма
    val accepted = scala.collection.mutable.ArrayBuffer.empty[Match] // "победившие" совпадения
    val conflicts = scala.collection.mutable.ArrayBuffer.empty[Conflict] // нерешённые конфликты
    var i = 0
    while (i < sorted.length) {
      val current = sorted(i)
      // собираем группу совпадений
      val group = scala.collection.mutable.ArrayBuffer(current)
      var j = i + 1
      var groupEnd = current.end
      while (j < sorted.length && sorted(j).start < groupEnd) {
        group += sorted(j)
        groupEnd = math.max(groupEnd, sorted(j).end)
        j += 1
      }
      if (group.size == 1) { // если совпадение единично
        accepted += current
      } else { // иначе - выбор "победителя"
        val maxPriority = group.map(_.rule.priority).max // сортировка по приоритету
        val topByPriority = group.filter(_.rule.priority == maxPriority)
        val maxLen = topByPriority.map(m => m.end - m.start).max // сортировка по длине
        val topByLen = topByPriority.filter(m => (m.end - m.start) == maxLen)
        if (topByLen.size == 1) {
          accepted += topByLen.head
        } else {
          // Конфликт: совпадают приоритет и длина
          val distinctRules = topByLen.map(_.rule.id).distinct
          if (distinctRules.size == 1) { // если одно и то же правило нашло одинаковый участок, то берём первый
            accepted += topByLen.head
          } else { // иначе конфликт нерешаемый
            val s = group.map(_.start).min
            val e = group.map(_.end).max
            conflicts += Conflict(s, e, group.toList)
            Logger.warn(s"Конфликт совпадений в диапазоне [$s,$e), правила: ${distinctRules.mkString(", ")}")
          }
        }
      }
      i = j
    }
    ResolutionResult(accepted.sortBy(_.start).toList, conflicts.toList)
  }
}