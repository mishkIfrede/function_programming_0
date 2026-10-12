package function_programming_0.policy
import function_programming_0.model._
import function_programming_0.report.Logger
// Перебор политик - раскрывает все вложенные политики, обнаруживает циклические зависимости, переопределяет правила (если необходимо)
case class ResolvedPolicy(
                           rootName: String, // имя политики
                           includedPolicies: List[String], // список всех включенных политик
                           rules: List[Rule], // список правил политики
                           overrides: List[String] // переопределимые правила
                         )

object PolicyResolver {
  def resolve(root: String, all: Map[String, Policy]): Either[String, ResolvedPolicy] = {
    all.get(root) match { // проверяем наличие "корневой" политики - возвращает Option[Policy]
      case None => Left(s"Политика не найдена: $root")
      case Some(_) =>
        val visited = scala.collection.mutable.LinkedHashSet.empty[String] // коллекция полностью "пройденных" политик
        val inStack = scala.collection.mutable.Set.empty[String] // политики, наодящиеся в процессе проверки
        val order = scala.collection.mutable.ArrayBuffer.empty[String] // буфер с политиками в правильном порядке обхода
        // рекурсивный обод в глубину
        def visit(name: String, stack: List[String]): Either[String, Unit] = {
          if (inStack.contains(name)) { // обработка наличия цикла
            val cycle = (stack :+ name).mkString(" -> ")
            Logger.error(s"Обнаружена циклическая зависимость: $cycle")
            return Left(s"Циклическая зависимость: $cycle") // сообщение об ошибке
          }
          if (visited.contains(name)) return Right(()) // проверка на то, проверялась ли политика
          all.get(name) match {
            case None => Left(s"Политика не найдена: $name (включена из ${stack.mkString(" -> ")})")
            case Some(p) =>
              inStack += name // добавляем политику в обработку
              p.includes.foreach { inc => // обрабатываем все includ'ы политики
                visit(inc, stack :+ name) match { // добавляем каждый include в обработку
                  case Left(e)  => return Left(e)
                  case Right(_) =>
                }
              }
              inStack -= name // конец обработки политики
              visited += name
              order += name
              Right(())
          }
        }
        // получаем результат посещений
        visit(root, Nil).map { _ =>
          // Применяем правила в порядке появления политик (сначала включённые, потом включающие).
          val rulesById = scala.collection.mutable.LinkedHashMap.empty[String, Rule] // карта правил вида id - правило
          val overrides = scala.collection.mutable.ArrayBuffer.empty[String] // буфер переопределений правил
          order.foreach { pName => // перебор всего порядка политик
            all(pName).rules.foreach { r => // перебор всех правил политики
              if (rulesById.contains(r.id)) { // если правило попалось повторно - переопределяем его
                val prev = rulesById(r.id)
                overrides += s"Правило '${r.id}' из политики '$pName' переопределяет правило из предыдущей политики"
                Logger.warn(s"Переопределение правила: ${r.id} (политика: $pName)")
              }
              rulesById(r.id) = r
            }
          }
          // формируем итоговую "пройденную" политику
          ResolvedPolicy(
            rootName = root,
            includedPolicies = order.toList,
            rules = rulesById.values.filter(_.active).toList,
            overrides = overrides.toList
          )
        }
    }
  }
}
