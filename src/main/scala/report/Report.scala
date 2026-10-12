package function_programming_0.report
import function_programming_0.io.FileResult
import function_programming_0.policy.ResolvedPolicy
import java.nio.file.Path
import java.time.LocalDateTime
import java.time.format.DateTimeFormatter
// модель итогового отчёта
case class OperationReport(
                            policyName: String,
                            includedPolicies: List[String],
                            startTime: LocalDateTime, // время начала операции
                            endTime: LocalDateTime, // время окончания операции
                            processed: Int,
                            skipped: Int,
                            byRule: Map[String, Int], // замены по правилам (id правила - кол-во замен)
                            conflicts: Int,
                            errors: List[(Path, String)], // кол-во ошибок - путь к файлу и сообщение
                            outputDir: Path
                          ) {
  private val fmt = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss")

  def render: String = {
    val sb = new StringBuilder // буфер для сборки результата
    sb.append("========== ОТЧЁТ ==========\n")
    sb.append(s"Политика: $policyName\n")
    sb.append(s"Включённые политики: ${includedPolicies.mkString(", ")}\n")
    sb.append(s"Время начала:   ${startTime.format(fmt)}\n")
    sb.append(s"Время окончания:${endTime.format(fmt)}\n")
    sb.append(s"Обработано файлов: $processed\n")
    sb.append(s"Пропущено файлов:  $skipped\n")
    sb.append(s"Всего конфликтов:  $conflicts\n")
    sb.append("Замены по правилам:\n")
    if (byRule.isEmpty) sb.append("  (нет)\n")
    else byRule.toList.sortBy(_._1).foreach { case (id, n) => sb.append(s"  $id: $n\n") } //сортировка по id правила, далее разбираем пару значений и выводим
    sb.append("Ошибки:\n")
    if (errors.isEmpty) sb.append("  (нет)\n")
    else errors.foreach { case (p, e) => sb.append(s"  ${p.getFileName}: $e\n") }
    sb.append(s"Выходной каталог: $outputDir\n")
    sb.append("============================\n")
    sb.toString
  }
}
// итоговый отчёт
object Report {
  def build( // возвращает готовый неизменяемый объект - отчёт
             resolved: ResolvedPolicy,
             startTime: LocalDateTime,
             endTime: LocalDateTime,
             results: List[FileResult], // результаты обработки всех файлов
             skipped: Int, // кол-во пропущенных файлов
             outputDir: Path
           ): OperationReport = {
    val byRule = scala.collection.mutable.Map.empty[String, Int].withDefaultValue(0) // коллекция вида (id правила - счётчик)
    results.foreach { r =>
      r.matchesByRule.foreach { case (k, v) => byRule(k) += v } // создаём общие счётчики для каждого файла
    }
    // формирование отчёта
    OperationReport(
      policyName = resolved.rootName,
      includedPolicies = resolved.includedPolicies,
      startTime = startTime,
      endTime = endTime,
      processed = results.count(_.error.isEmpty), // файлы, прошедшие анализ
      skipped = skipped,
      byRule = byRule.toMap, // превращаем в иммутабельную копию
      conflicts = results.map(_.conflicts).sum, // сумма всех конфликтов по файлам
      errors = results.collect { case r if r.error.isDefined => r.path -> r.error.get }, // все ошибки
      outputDir = outputDir
    )
  }
}
