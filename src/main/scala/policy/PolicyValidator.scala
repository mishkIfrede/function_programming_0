package function_programming_0.policy
import function_programming_0.model._
object PolicyValidator {
// Валидатор - полностью проверяет политики на ошибки
  def validate(cfg: AppConfig): List[String] = {
    val errors = scala.collection.mutable.ArrayBuffer.empty[String] // буфер ошибок политик
    // проверка на циклы (см. PolicyResolver.scala)
    cfg.policies.foreach { case (name, _) =>
      PolicyResolver.resolve(name, cfg.policies) match {
        case Left(e)  => errors += s"Политика '$name': $e"
        case Right(_) =>
      }
    }

    // проверка на дубли правил внутри одной политики
    cfg.policies.foreach { case (name, p) =>
      val dup = p.rules.groupBy(_.id).collect { case (id, rs) if rs.size > 1 => id }
      dup.foreach(id => errors += s"Политика '$name': дублирующийся id правила '$id'")
    }

    // проверка на пустые id или названия правил
    cfg.policies.foreach { case (name, p) =>
      p.rules.foreach { r =>
        if (r.id.trim.isEmpty) errors += s"Политика '$name': правило с пустым id"
        if (r.name.trim.isEmpty) errors += s"Политика '$name': правило '${r.id}' с пустым именем"
        r.search match {
          case SearchMethod.Regex(pat, _) => // проверка на корректность регулярного выражения
            try pat.r
            catch { case e: Throwable => errors += s"Правило '${r.id}': некорректное регулярное выражение: ${e.getMessage}" }
          case _ => // остальные методы поиска не нуждаются в проверке корректности
        }
      }
    }

    errors.toList
  }
}
