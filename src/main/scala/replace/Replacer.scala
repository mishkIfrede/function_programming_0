package function_programming_0.replace
import function_programming_0.matching.Match
import function_programming_0.model.Replacement
import scala.util.Random
// Исполнитель замен персональных данных на требующуяся форму
object Replacer {

  def apply(m: Match, registry: AliasRegistry, listIndices: scala.collection.mutable.Map[String, Int]): String = {
    m.rule.replacement match { // заменяем поле в зависимости от указанного способа
      case Replacement.Constant(v) => v
      case Replacement.Delete      => ""
      case Replacement.Mask(keepPrefix, keepSuffix, ch) =>
        val v = m.value
        if (v.length <= keepPrefix + keepSuffix) ch.toString * v.length // если длина поля короче нескрываемыхх символов, то маскируем всё поле
        else {
          val prefix = v.take(keepPrefix)
          val suffix = if (keepSuffix > 0) v.takeRight(keepSuffix) else ""
          val middle = ch.toString * (v.length - keepPrefix - keepSuffix)
          prefix + middle + suffix // собираем замену-маску
        }
      case Replacement.Alias(prefix) =>
        registry.aliasFor(prefix, m.value) // вычислем псевдоним
      case Replacement.FromList(values) if values.nonEmpty =>
        val idx = listIndices.getOrElseUpdate(m.value, listIndices.size % values.size) // если индекс есть в списке, берём из него, если нет - берём из mod
        values(idx)
      case Replacement.FromList(_) => ""
    }
  }
}
