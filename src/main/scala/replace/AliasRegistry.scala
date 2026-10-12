package function_programming_0.replace
import scala.collection.mutable

// реестр псевдонимов в рамках одной операции
class AliasRegistry {
  private val map = mutable.LinkedHashMap.empty[String, String] // коллекция вида ключ-псевдоним
  private val counters = mutable.Map.empty[String, Int].withDefaultValue(0) // счётчики префиксов псевдонимов
  // создание псевдонима
  def aliasFor(prefix: String, value: String): String = {
    val key = prefix + "\u0000" + value // создаём ключ, 0 нужен для того, чтобы не возникли коллизии
    map.getOrElseUpdate(key, { // если такой ключ уже есть, то возвращаем его, если нет - создаём и возвращаем
      counters(prefix) += 1
      s"$prefix-${counters(prefix)}"
    })
  }

  // возвращение карты псевдонимов
  def snapshot: Map[String, String] = map.toMap.map { case (k, v) =>
    val idx = k.indexOf('\u0000')
    k.substring(idx + 1) -> v
  }
}
