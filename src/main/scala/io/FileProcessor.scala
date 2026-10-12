package function_programming_0.io
import function_programming_0.matching.{Conflict, Match, Matcher, ConflictResolver}
import function_programming_0.model.Rule
import function_programming_0.replace.{AliasRegistry, Replacer}
import function_programming_0.report.Logger
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}
import scala.collection.mutable
// Обработчик файла - производит операцию чтения, замены и записи в новый файл
// "снимок" анализа файла
case class FilePreview(
                        path: Path, // исходный путь
                        matches: List[Match], // принятые совпадения
                        conflicts: List[Conflict], // нерешённые конфликты
                        previewText: String // исходная версия текста
                      )
// результат обработки
case class FileResult(
                       path: Path,
                       outputPath: Option[Path], // выходной каталог
                       matchesByRule: Map[String, Int], // количество замен
                       conflicts: Int, // количество конфликтов
                       error: Option[String] // возможное сообщение об ошибке
                     )

object FileProcessor {
  // анализ текста
  def analyze(path: Path, rules: List[Rule]): Either[String, FilePreview] = {
    try { // при успешном чтении файла находим все замены
      val text = new String(Files.readAllBytes(path), StandardCharsets.UTF_8)
      val all = Matcher.findAll(text, rules)
      val res = ConflictResolver.resolve(all)
      Right(FilePreview(path, res.accepted, res.conflicts, text))
    } catch { // иначе - сообщение об ошибке
      case e: Throwable =>
        Logger.error(s"Ошибка чтения файла ${path.getFileName}: ${e.getMessage}")
        Left(s"Ошибка чтения ${path.getFileName}: ${e.getMessage}")
    }
  }

  def process(
               preview: FilePreview,
               outputDir: Path,
               baseDir: Option[Path],
               preserveStructure: Boolean, // флаг сохранения структуры каталогов
               keepAliases: Boolean // флаг сохранения карты псевдонимов
             ): FileResult = {
    try {
      val text = preview.previewText
      val registry = new AliasRegistry
      val listIndices = mutable.Map.empty[String, Int] // коллекция исходное значение-индекс в списке

      val sorted = preview.matches.sortBy(_.start) // сортируем совпадения по позиции
      val sb = new StringBuilder // буфер для сборки результата
      var pos = 0 // позиция курсора
      val perRule = mutable.Map.empty[String, Int].withDefaultValue(0) // счётчик замен по правилам

      sorted.foreach { m =>
        if (m.start >= pos) {
          sb.append(text.substring(pos, m.start)) // копируем весь текст до замены
          sb.append(Replacer(m, registry, listIndices)) // вносим замену вместо исодного значени
          perRule(m.rule.id) += 1
          pos = m.end // двигаем позицию в конец совпадения
        }
      }
      if (pos < text.length) sb.append(text.substring(pos))

      val outPath: Path = baseDir match { // определяем выходной путь
        case Some(base) if preserveStructure =>
          val rel = base.toAbsolutePath.normalize().relativize(preview.path.toAbsolutePath.normalize()) // сохраняем исходную структуру каталога
          outputDir.resolve(rel)
        case _ =>
          outputDir.resolve(preview.path.getFileName.toString) // оставляем плоский каталог
      }
      Files.createDirectories(outPath.getParent) // создаём выходную папку
      Files.write(outPath, sb.toString.getBytes(StandardCharsets.UTF_8)) // записываем текст в файл
      Logger.info(s"Файл обработан: ${preview.path.getFileName} -> ${outPath.getFileName}")

      if (keepAliases) { // если хотим сохранить карту псевдонимов - создаём отдельный json-файл
        val aliasFile = outPath.resolveSibling(outPath.getFileName.toString + ".aliases.json")
        val json = registry.snapshot.map { case (k, v) => s"${escapeJson(k)}:${escapeJson(v)}" }.mkString("{", ",", "}")
        Files.write(aliasFile, json.getBytes(StandardCharsets.UTF_8))
      }
      // возвращаем результат операции
      FileResult(preview.path, Some(outPath), perRule.toMap, preview.conflicts.size, None)
    } catch {
      case e: Throwable =>
        Logger.error(s"Ошибка записи ${preview.path.getFileName}: ${e.getMessage}")
        FileResult(preview.path, None, Map.empty, 0, Some(s"Ошибка записи: ${e.getMessage}"))
    }
  }
  // json-эскейпер - обрабатывает особые символы
  private def escapeJson(s: String): String =
    "\"" + s.replace("\\", "\\\\").replace("\"", "\\\"").replace("\n", "\\n") + "\""
}
