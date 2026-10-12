package function_programming_0.model
import play.api.libs.json._

// Политика - или набор правил, или сборник других политик
case class Policy(
                   name: String, // название
                   description: String, // описание
                   rules: List[Rule] = Nil, // список правил
                   includes: List[String] = Nil // список включённых политик
                 )

object Policy {
  implicit val fmt: Format[Policy] = Json.format[Policy] // преобразование в json
}