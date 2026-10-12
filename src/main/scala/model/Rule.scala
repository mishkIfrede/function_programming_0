package function_programming_0.model
import play.api.libs.json._

// Способы поиска данных
sealed trait SearchMethod
object SearchMethod {
  case class Exact(value: String) extends SearchMethod // точное значение
  case class Regex(pattern: String, flags: String = "") extends SearchMethod // регулярное выражение
  case class Dictionary(values: List[String]) extends SearchMethod // значение из словаря
  case class Builtin(kind: String) extends SearchMethod // встроенный тип

  implicit val fmt: Format[SearchMethod] = new Format[SearchMethod] {
    // чтение из JsonValue в SearchMethod
    def reads(j: JsValue): JsResult[SearchMethod] = (j \ "type").as[String] match {
      case "exact"      => JsSuccess(Exact((j \ "value").as[String]))
      case "regex"      => JsSuccess(Regex((j \ "pattern").as[String], (j \ "flags").asOpt[String].getOrElse("")))
      case "dictionary" => JsSuccess(Dictionary((j \ "values").as[List[String]]))
      case "builtin"    => JsSuccess(Builtin((j \ "kind").as[String]))
      case other        => JsError(s"Unknown search type: $other")
    }
    // запись из SearchMethod в JsonValue
    def writes(sm: SearchMethod): JsValue = sm match {
      case Exact(v)      => Json.obj("type" -> "exact", "value" -> v)
      case Regex(p, f)   => Json.obj("type" -> "regex", "pattern" -> p, "flags" -> f)
      case Dictionary(v) => Json.obj("type" -> "dictionary", "values" -> v)
      case Builtin(k)    => Json.obj("type" -> "builtin", "kind" -> k)
    }
  }
}

// Спобобы замены полей
sealed trait Replacement
object Replacement {
  case class Constant(value: String) extends Replacement // константа
  case object Delete extends Replacement // полное удаление
  case class Mask(keepPrefix: Int, keepSuffix: Int, maskChar: Char = '*') extends Replacement // подстановка маски
  case class Alias(prefix: String) extends Replacement // псевдоним
  case class FromList(values: List[String]) extends Replacement // значение из списка

  implicit val fmt: Format[Replacement] = new Format[Replacement] {
    // чтение из JsonValue в Replacement
    def reads(j: JsValue): JsResult[Replacement] = (j \ "type").as[String] match {
      case "constant" => JsSuccess(Constant((j \ "value").as[String]))
      case "delete"   => JsSuccess(Delete)
      case "mask"     => JsSuccess(Mask(
        (j \ "keepPrefix").asOpt[Int].getOrElse(0),
        (j \ "keepSuffix").asOpt[Int].getOrElse(0),
        (j \ "maskChar").asOpt[String].flatMap(_.headOption).getOrElse('*')
      ))
      case "alias"    => JsSuccess(Alias((j \ "prefix").asOpt[String].getOrElse("ANON")))
      case "fromList" => JsSuccess(FromList((j \ "values").as[List[String]]))
      case other      => JsError(s"Unknown replacement type: $other")
    }
    // запись из Replacement в JsonValue
    def writes(r: Replacement): JsValue = r match {
      case Constant(v)        => Json.obj("type" -> "constant", "value" -> v)
      case Delete             => Json.obj("type" -> "delete")
      case Mask(p, s, c)      => Json.obj("type" -> "mask", "keepPrefix" -> p, "keepSuffix" -> s, "maskChar" -> c.toString)
      case Alias(p)           => Json.obj("type" -> "alias", "prefix" -> p)
      case FromList(v)        => Json.obj("type" -> "fromList", "values" -> v)
    }
  }
}

// Правила анонимизации
case class Rule(
                 id: String, // id правила
                 name: String, // название
                 description: String, // описание
                 search: SearchMethod, // способ поиска значений
                 replacement: Replacement, // способ замены значений
                 priority: Int, // приоритет
                 active: Boolean // флаг активации
               )

object Rule {
  implicit val fmt: Format[Rule] = Json.format[Rule] // преобразование в json
}