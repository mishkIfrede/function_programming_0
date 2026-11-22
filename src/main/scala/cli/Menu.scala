package cli
import config.ConfigLoader
import function_programming_0.io.{FileProcessor, FileScanner, Selection}
import function_programming_0.model._
import function_programming_0.policy.{PolicyResolver, PolicyValidator}
import function_programming_0.report.{Logger, Report}
import function_programming_0.policy._

import java.nio.file.{Files, Path, Paths}
import java.time.LocalDateTime
import scala.io.StdIn
import scala.util.Try

sealed trait AppError {
  def message: String
}

  // --- ошибки ввода/парсинга ---
sealed trait ParseError extends AppError

object ParseError {
  case object EmptyInput extends ParseError {
    val message = "Пустой ввод"
  }

  final case class NotANumber(raw: String) extends ParseError {
    val message = s"Ожидалось число, получено: '$raw'"
  }

  final case class OutOfRange(n: Int, from: Int, to: Int) extends ParseError {
    val message = s"Число $n вне диапазона [$from..$to]"
  }

  case object Canceled extends ParseError { //!
    val message = "Отменено пользователем"
  }
}

// --- ошибки домена ---
sealed trait DomainError extends AppError

object DomainError {
  final case class ResolveFailed(name: String) extends DomainError {
    val message = s"Ошибка переопределения: '$name'"
  }

  final case class PolicyNotFound(name: String) extends DomainError {
    val message = s"Политика '$name' не найдена"
  }

  final case class PolicyExists(name: String) extends DomainError {
    val message = s"Политика '$name' уже существует"
  }

  final case class RuleExists(id: String) extends DomainError {
    val message = s"Правило с id '$id' уже существует"
  }

  final case class RuleNotFound(id: String) extends DomainError {
    val message = s"Правило с id '$id' не найдено"
  }

  final case class IncludeNotFound(name: String) extends DomainError {
    val message = s"Включаемая политика '$name' не найдена"
  }

  final case class ValidationFailed(errors: Seq[String]) extends DomainError {
    val message = s"Ошибки валидации:\n" + errors.map(" - " + _).mkString("\n")
  }
}

// --- ошибки ввода-вывода/файлов ---
sealed trait IOError extends AppError

object IOError {
  final case class FileNotFound(path: String) extends IOError {
    val message = s"Файл не найден: $path"
  }

  final case class ReadFailed(path: String, cause: String) extends IOError {
    val message = s"Не удалось прочитать '$path': $cause"
  }

  final case class WriteFailed(path: String, cause: String) extends IOError {
    val message = s"Не удалось записать '$path': $cause"
  }

  final case class ConfigLoadFailed(path: String, cause: String) extends IOError {
    val message = s"Не удалось загрузить конфиг '$path': $cause"
  }

  final case class ConfigSaveFailed(path: String, cause: String) extends IOError {
    val message = s"Не удалось сохранить конфиг '$path': $cause"
  }

  case object NoFilesFound extends IOError { //!
    val message = "Не найдено подходящих файлов"
  }
}


class AppState(var cfg: AppConfig, val configPath: Path)

case class MenuItem[T](name: String, description: String, action: () => Either[AppError, T])

case class Menues[T](title: String, items:Seq[MenuItem[T]], userInterface: UserInterface) {
  def printMenu(): Unit = {
    userInterface.printLine(s"=== ${title} ===")
    items.zipWithIndex.foreach { case (it, i) =>
      userInterface.printLine(s"${i + 1}. ${it.name} — ${it.description}")
    }
    userInterface.printLine(" q. Выход")
    userInterface.printLine("====================================================")
  }

  /** Показать меню и выполнить выбор.
   * Left — ошибка/отмена, Right — результат выбранного действия. */
  def runOnce(): Either[AppError, T] = {
    printMenu()
    userInterface.readString("> ").trim match {
      case "q" | "0" => Left(ParseError.Canceled)
      case s => s.toIntOption match {
        case None => Left(ParseError.NotANumber(s))
        case Some(n)
          if n < 1 || n > items.length =>
          Left(ParseError.OutOfRange(n, 1, items.length))
        case Some(n) => items(n - 1).action()
      }
    }
  }
}
// Модель меню - реализует пользовательский интерфейс программы
class Menu(initialConfigPath: Path, ui:UserInterface) {

  private val state = new AppState(
    ConfigLoader.load(initialConfigPath).getOrElse(ConfigLoader.defaultConfig),
    initialConfigPath
  )

  private val items: Seq[MenuItem[Unit]] = Seq(
    MenuItem("Показать список политик",        "список всех политик",           () => Right(listPolicies())),
    MenuItem("Показать политику и правила",    "детали одной политики",         () => Right(showPolicy())),
    MenuItem("Создать новую политику",         "добавить политику",             () => Right(createPolicy())),
    MenuItem("Изменить существующую политику", "редактировать политику",        () => Right(editPolicy())),
    MenuItem("Удалить политику",               "удалить политику",              () => Right(deletePolicy())),
    MenuItem("Составить политику из существующих", "композиция политик",        () => Right(composePolicy())),
    MenuItem("Проверить конфигурацию системы", "валидация",                     () => Right(validateConfig())),
    MenuItem("Обработать файлы",               "применить политику к файлам",   () => Right(processFiles())),
    MenuItem("Предварительный просмотр",       "анализ без записи",             () => Right(previewMatches())),
    MenuItem("Перезагрузить конфигурацию",     "reload",                        () => Right(reload())),
    MenuItem("Сохранить конфигурацию",         "save",                          () => Right(save()))
  )

  val MainMenu = new Menues("Главное меню програмы", items, CLI)

//  private var configPath: Path = initialConfigPath
//  private var cfg: AppConfig = ConfigLoader.load(configPath).getOrElse(ConfigLoader.defaultConfig) // используем пользовательский конфиг, если его нет - конфиг по умолчанию

  def run(): Unit = {
    Logger.init(Paths.get(state.cfg.settings.logFile))
    var loop = true
    while (loop) {
      MainMenu.printMenu()
      ui.readString("> ").trim match {
        case "q" => loop = false
        case s =>
          s.toIntOption
            .filter(n => n >= 1 && n <= items.length)
            .map(n => items(n - 1).action())
            .getOrElse(Left(ParseError.NotANumber(s))) match {
            case Right(_) => () // успех
            case Left(err) => report(err) // единая обработка
          }
      }
    }
  }
  Logger.close()

  private def report(err: AppError): Unit = {
    err match {
      case ParseError.Canceled => ui.printLine("Отменено.")
      case ParseError.EmptyInput => ui.printLine("Пустой ввод.")
      case ParseError.NotANumber(raw) => ui.printLine(s"Не число: '$raw'.")
      case ParseError.OutOfRange(n, a, b) => ui.printLine(s"$n вне [$a..$b].")
      case DomainError.PolicyNotFound(n) => ui.printLine(s"Политика '$n' не найдена.")
      case DomainError.ValidationFailed(es) =>
        ui.printLine("Ошибки валидации:")
        es.foreach(e => ui.printLine(" - " + e))
      case IOError.FileNotFound(p) => ui.printLine(s"Файл не найден: $p")
      case e: IOError => ui.printLine(s"Ошибка I/O: ${e.message}")
      case e: DomainError => ui.printLine(s"Ошибка: ${e.message}")
    }
  }

  // сохранение файла конфигурации
  private def save(): Either[AppError, Unit] = {
    ConfigLoader.save(state.configPath, state.cfg) match {
      case Right(_) =>
        Logger.info(s"Конфигурация сохранена: ${state.configPath}.")
        Right(())
      case Left(err) =>
        Logger.error(err.message)
        Left(err)
    }
  }

  // перезагрузка конфигурации (при её редактировании пользователем)
  private def reload(): Unit = {
    ConfigLoader.load(state.configPath) match {
      case Right(c) => state.cfg = c; Logger.info("Конфигурация перезагружена.")
      case Left(e) => Logger.error(e.message); ui.printLine(e.message)
    }
  }

  // Далее - разбор каждой из опций
  // вывод списка существующих политик
  private def listPolicies(): Either[AppError, Unit] = {
    println("Список всех политик:")
    state.cfg.policies.toList.sortBy(_._1).foreach { case (name, p) => // политики сортируем по id
      ui.printLine(s" - $name: ${p.description} (правила=${p.rules.size}, включения=${p.includes.mkString(",")})")
    }
    Right()
  }
  // обзор конкретной политики
  private def showPolicy(): Either[AppError, Unit] = {
    val name = ui.readString("Введите имя политики: ").trim
    state.cfg.policies.get(name) match { // ищем политику по имени
      case None => Left(DomainError.PolicyNotFound(name))
      case Some(p) =>
        ui.printLine(s"Политика '$name': ${p.description}")
        if (p.includes.nonEmpty) ui.printLine(s"Включает: ${p.includes.mkString(", ")}")
        ui.printLine("Собственные правила:")
        p.rules.foreach { r =>
          ui.printLine(s"  [${if (r.active) "x" else " "}] id=${r.id} name=${r.name} prio=${r.priority}")
          ui.printLine(s"      desc: ${r.description}")
          ui.printLine(s"      search: ${r.search}")
          ui.printLine(s"      replace: ${r.replacement}")
        }
        Right()
    }
  }
  // создание политики
  private def createPolicy(): Either[AppError, Unit] = {
    val name = ui.readString("Введите имя новой политики: ").trim
    if (name.isEmpty) {
      Left(DomainError.PolicyNotFound(name))
    } else {
      if (state.cfg.policies.contains(name)) {
        Left(DomainError.PolicyExists(name))
      } else {
        val desc = ui.readString("Введите описание политики: ").trim
        state.cfg = state.cfg.copy(policies = state.cfg.policies + (name -> Policy(name, desc))) // добавляем новую политику к существующим
        Logger.info(s"Создана политика: $name.")
        ui.printLine("Для добавления правил или включения других политик перейдите в режим 'Изменить политику'.")
        save()
        Right()
      }
    }
  }

  // редактирование существующей политики
  private def editPolicy(): Either[AppError, Unit] = {
    val name = ui.readString("Введите имя политики для изменения: ").trim
    state.cfg.policies.get(name) match {
      case None => Left(DomainError.PolicyNotFound(name))
      case Some(p) =>
        def editDescription(name: String): Either[AppError, Unit] = {
          val d = ui.readString("Введите новое описание: ").trim
          state.cfg = state.cfg.copy(policies = state.cfg.policies + (name -> p.copy(description = d)))
          ui.printLine("Описание политики успешно изменено.")
          Right()
        }

        def addRule(name: String): Either[AppError, Unit] = {
          readRule() match { // сборка правила из ввода пользователя
            case Some(r) =>
              if (p.rules.exists(_.id == r.id)) {
                Left(DomainError.PolicyExists(name))
              }
              else {
                state.cfg = state.cfg.copy(policies = state.cfg.policies + (name -> p.copy(rules = p.rules :+ r))) // новое правило добавляем в конец
                Logger.info(s"Добавлено правило ${r.id} в политику $name/")
                Right()
              }
            case None => Left(DomainError.PolicyNotFound(name)) // политика не добавлена
          }
        }

        def deleteRule(name: String): Either[AppError, Unit] = {
          val id = ui.readInt("Введите id правила: ")
          state.cfg = state.cfg.copy(policies = state.cfg.policies + (name -> p.copy(rules = p.rules.filterNot(_.id == id)))) // в конфиг заносим все правила, кроме этого
          Logger.info(s"Удалено правило $id из политики $name.")
          Right()
        }

        def changeActive(name: String): Either[AppError, Unit] = {
          val id = ui.readInt("Введите id правила: ")
          state.cfg = state.cfg.copy(policies = state.cfg.policies + (name -> p.copy(rules = p.rules.map(r => if (r.id == id) r.copy(active = !r.active) else r))))
          Logger.info(s"Переключена активность правила $id в $name.")
          Right()
        }

        def addInclude(name: String): Either[AppError, Unit] = {
          val inc = ui.readString("Введите имя включаемой политики: ").trim
          state.cfg.policies.get(inc) match {
            case None => Left(DomainError.PolicyNotFound(name))
            case Some(p) =>
              if (inc.nonEmpty && !p.includes.contains(inc))
                state.cfg = state.cfg.copy(policies = state.cfg.policies + (name -> p.copy(includes = p.includes :+ inc)))
              Logger.info(s"Политика с имененм $inc включена в политику с именем $name.")
              Right()
          }
        }

        def deleteInclude(name: String): Either[AppError, Unit] = {
          val inc = ui.readString("Введите имя исключаемой политики: ").trim
          state.cfg.policies.get(inc) match {
            case None => Left(DomainError.PolicyNotFound(name))
            case Some(p) =>
              state.cfg = state.cfg.copy(policies = state.cfg.policies + (name -> p.copy(includes = p.includes.filterNot(_ == inc))))
              Logger.info(s"Политика с имененм $inc исключена из политики с имененм $name.")
              Right()
          }
        }

        val editItems: Seq[MenuItem[Either[AppError, Unit]]] = Seq(
          MenuItem("Изменить описание", "---", () => Right(editDescription(name))),
          MenuItem("Добавить правило", "---", () => Right(addRule(name))),
          MenuItem("Удалить правило", "---", () => Right(deleteRule(name))),
          MenuItem("Переключить активность правила", "---", () => Right(changeActive(name))),
          MenuItem("Добавить включение (include)", "---", () => Right(addInclude(name))),
          MenuItem("Удалить включение (include)", "---", () => Right(deleteInclude(name))),
        )

        val sub = new Menues(s"Редактирование политики '$name'", editItems, ui)
        var loop = true
        while (loop) {
          sub.printMenu()
          ui.readString("> ").trim match {
            case "q" => loop = false
            case s =>
              s.toIntOption
                .filter(n => n >= 1 && n <= editItems.length)
                .fold(ui.printLine("Неверный выбор"))(n => editItems(n - 1).action())
          }
        }
        save()
    }
  }
  // удаление политики
  private def deletePolicy(): Either[AppError, Unit] = {
    val name = ui.readString("Введите имя политики для удаления: ").trim
    if (state.cfg.policies.contains(name)) {
      state.cfg = state.cfg.copy(policies = state.cfg.policies - name)
      Logger.info(s"Удалена политика: $name.")
      save()
      Right()
    } else Left(DomainError.PolicyNotFound(name))
  }
  // создание составной политики из существующих
  private def composePolicy(): Either[AppError, Unit] = {
    val name = ui.readString("Введите имя новой составной политики: ").trim
    if (name.isEmpty || state.cfg.policies.contains(name)) {
      Left(DomainError.PolicyNotFound(name))
    } else {
      ui.printLine("Доступные для включения политики: " + state.cfg.policies.keys.mkString(", "))
      val inc = ui.readString("Введите имена включаемых политик через запятую: ").split(",").map(_.trim).filter(_.nonEmpty).toList
      val missing = inc.filterNot(state.cfg.policies.contains)
      if (missing.nonEmpty) {
        ui.printLine(s"Не найдены политики, вследствие чего не будут добавлены: ${missing.mkString(", ")}.")
      }
      val desc = ui.readString("Введите описание составляемой политики: ").trim
      state.cfg = state.cfg.copy(policies = state.cfg.policies + (name -> Policy(name, desc, Nil, inc)))
      Logger.info(s"Создана составная политика: $name (включения=${inc.mkString(",")}).")
      save()
      Right()
    }
  }
  // создание нового правила (используется в createPolicy())
  private def readRule(): Option[Rule] = {
    ui.printLine("Создание нового правила")
    val id = ui.readString("Введите id правила (уникальный): ")
    if (id.isEmpty) return None
    val name = ui.readString("Введите имя правила: ").trim
    val desc = ui.readString("Введите описание правила: ").trim

    println("Укажите тип поиска(введите цифру с желаемой опцией):")
    println("1 - Точное значение")
    println("2 - Регулярное выражение")
    println("3 - Словарь значений")
    println("4 - Встроенный тип данных")
    val search: SearchMethod = StdIn.readLine("> ").trim match {
      case "1" => SearchMethod.Exact(StdIn.readLine("Введите значение: ").trim)
      case "2" => SearchMethod.Regex(StdIn.readLine("Введите выражение: ").trim, StdIn.readLine("Флаг (напр. i для регистронезависимости): ").trim) // например, \bhello\b
      case "3" => SearchMethod.Dictionary(StdIn.readLine("Введите значения через | : ").split("\\|").map(_.trim).filter(_.nonEmpty).toList)
      case "4" =>
        println("Имеющиеся строенные типы: email, phone, ipv4, url, password, token")
        SearchMethod.Builtin(StdIn.readLine("Введите необходымый тип: ").trim)
      case _ => return None
    }
    println("Введите необходимый тип замены (выберите нужную цифру):")
    println("1 - Замена константой")
    println("2 - Удаление поля")
    println("3 - Маскирование")
    println("4 - Замена псевдонимом")
    println("5 - Замена значением из списка")
    val repl: Replacement = StdIn.readLine("> ").trim match {
      case "1" => Replacement.Constant(StdIn.readLine("Введите значение: ").trim)
      case "2" => Replacement.Delete
      case "3" =>
        val p = Try(StdIn.readLine("Введите кол-во символов, которое не будет маскироваться в начале: ").trim.toInt).getOrElse(0)
        val s = Try(StdIn.readLine("Введите кол-во символов, которое не будет маскироваться в конце: ").trim.toInt).getOrElse(0)
        Replacement.Mask(p, s)
      case "4" => Replacement.Alias(StdIn.readLine("Введите начало псевдонима: ").trim)
      case "5" => Replacement.FromList(StdIn.readLine("Введите значения через | : ").split("\\|").map(_.trim).filter(_.nonEmpty).toList)
      case _   => return None
    }
    val prio = Try(ui.readString("Введите приоритет правила ив виде числа (по умолчанию 0): ").toInt).getOrElse(0)
    val active = ui.readString("Ваше правило активно на данный момент? (y/n): ").trim.toLowerCase.startsWith("y")
    Some(Rule(id, name, desc, search, repl, prio, active))
  }
  // проверка конфигурации
  private def validateConfig(): Either[AppError, Unit] = {
    val errs = PolicyValidator.validate(state.cfg)
    if (errs.isEmpty) ui.printLine("Конфигурация корректна.")
    else {
      ui.printLine("Обнаружены проблемы:")
      errs.foreach(e => ui.printLine(" - " + e))
    }
    Logger.info(s"Проверка конфигурации: ошибок=${errs.size}.")
    Right()
  }

  // выбор предмета обработки - файл(ы) или каталог (используется в previewMatches() и processFiles())
  private def chooseSelection(): Option[(Selection, Option[Path])] = {

    println("Выберите источник (введите подходящую цифру):")
    println("1 - Один файл")
    println("2 - Несколько файлов (вводятся через ;)")
    println("3 - Каталог (без вложенний)")
    println("4 - Каталог (с вложеними)")
    StdIn.readLine("> ").trim match {
      case "1" =>
        val p = Paths.get(StdIn.readLine("Введите путь: ").trim)
        Some(Selection.SingleFile(p) -> None)
      case "2" =>
        val ps = StdIn.readLine("Введите пути через ; : ").split(";").map(_.trim).filter(_.nonEmpty).map(Paths.get(_)).toList
        Some(Selection.MultipleFiles(ps) -> None)
      case "3" =>
        val p = Paths.get(StdIn.readLine("Введите каталог: ").trim)
        Some(Selection.Directory(p, recursive = false) -> Some(p))
      case "4" =>
        val p = Paths.get(StdIn.readLine("Введите каталог: ").trim)
        Some(Selection.Directory(p, recursive = true) -> Some(p))
      case _ => None
    }
  }
  // ввод имени политики (используется в previewMatches() и processFiles())
  private def selectPolicy(): Option[String] = {
    println("Доступные политики: " + state.cfg.policies.keys.mkString(", "))
    val name = StdIn.readLine("Введите имя политики: ").trim
    if (state.cfg.policies.contains(name)) Some(name) else { println("Политика не найдена."); None }
  }
  // переопределение правила (используется в previewMatches() и processFiles())
  private def resolveOrReport(name: String): Option[function_programming_0.policy.ResolvedPolicy] = {
    PolicyResolver.resolve(name, state.cfg.policies) match {
      case Right(r) =>
        r.overrides.foreach(o => println("Переопределение: " + o))
        Some(r)
      case Left(e) => println("Ошибка: " + e); None
    }
  }

  // анализ всех файлов и предпоказ совпадений
  private def previewMatches(): Unit = {
    val pol = selectPolicy().getOrElse(return)
    val resolved = resolveOrReport(pol).getOrElse(return)
    val (sel, _) = chooseSelection().getOrElse(return) // получаем файл(ы) для обработки и возможный каталог
    val files = FileScanner.scan(sel, state.cfg.settings.allowedExtensions, Paths.get(state.cfg.settings.outputDir))
    ui.printLine(s"Найдено файлов: ${files.size}. Анализ...")
    files.foreach { f =>
      FileProcessor.analyze(f, resolved.rules) match {
        case Right(prev) =>
          ui.printLine(s"\n- Файл: ${f.getFileName}.")
          ui.printLine(s"  Найдено совпадений: ${prev.matches.size}, конфликтов: ${prev.conflicts.size}.")
          prev.matches.take(20).foreach { m => // показываем первые 20 совпадений
            // НЕ выводим само значение, только тип/правило/позицию
            ui.printLine(s"    [${m.rule.id}] позиция ${m.start}-${m.end} (len=${m.end - m.start}).")
          }
          if (prev.matches.size > 20) ui.printLine(s"    ... и ещё ${prev.matches.size - 20}.")
        case Left(e) => ui.printLine(s"- Файл ${f.getFileName}: $e.")
      }
    }
  }

//  private def previewMatches(): Either[AppError, Unit] = {
//    for {
//      pol <- selectPolicy()
//      resolved <- resolveOrReport(pol)
//      selection <- chooseSelection()
//      files: <- FileScanner.scan(selection._1, state.cfg.settings.allowedExtensions, Paths.get(state.cfg.settings.outputDir))
//      _ <- if (files.isEmpty) { Left(IOError.NoFilesFound) } else Right(())
//      _ <- previewFiles(files, resolved)}
//    yield ()
//  }
//
//  private def previewFiles(files: Seq[Path], resolved: ResolvedPolicy): Either[AppError, Unit] = {
//    ui.printLine(s"Найдено файлов: ${files.size}. Анализ...")
//    files.foreach { f =>
//      FileProcessor.analyze(f, resolved.rules) match {
//        case Right(prev) =>
//          ui.printLine(s"\n- Файл: ${f.getFileName}.")
//          ui.printLine(s"  Найдено совпадений: ${prev.matches.size}, конфликтов: ${prev.conflicts.size}.")
//          prev.matches.take(20).foreach { m =>
//            ui.printLine(s"    [${m.rule.id}] позиция ${m.start}-${m.end} (len=${m.end - m.start}).")
//          }
//          if (prev.matches.size > 20) {
//            ui.printLine(s"    ... и ещё ${prev.matches.size - 20}.")
//          }
//        case Left(e) => ui.printLine(s"- Файл ${f.getFileName}: $e.")
//      }
//    }
//    Right(())
//  }

  // прогон файлов - анализ, совпадения, конфликты...
  private def processFiles(): Unit = {
    val pol = selectPolicy().getOrElse(return)
    val resolved = resolveOrReport(pol).getOrElse(return)
    val (sel, baseDir) = chooseSelection().getOrElse(return)
    val outDir = Paths.get(state.cfg.settings.outputDir).toAbsolutePath.normalize()

    val files = FileScanner.scan(sel, state.cfg.settings.allowedExtensions, outDir)
    if (files.isEmpty) { ui.printLine("Нет подходящих файлов."); return }

    // предварительный анализ
    ui.printLine(s"Найдено файлов: ${files.size}. Анализ...")
    val previews = files.map(f => f -> FileProcessor.analyze(f, resolved.rules))
    val ok = previews.collect { case (f, Right(p)) => f -> p } // успешно проанализированные файлы
    val bad = previews.collect { case (f, Left(e)) => f -> e } // файлы с ошибками

    val totalMatches = ok.map(_._2.matches.size).sum // суммируем все совпадения по успешным файлам
    val totalConflicts = ok.map(_._2.conflicts.size).sum // суммируем все конфликты по успешным файлам
    ui.printLine(s"Всего совпадений: $totalMatches, конфликтов: $totalConflicts.")
    ui.printLine("Показать детали по файлу? Введите номер файла или нажмите Enter для пропуска: ")
    ok.zipWithIndex.foreach { case ((f, p), i) =>
      ui.printLine(f"  ${i + 1}%3d) ${f.getFileName} совпадения=${p.matches.size} конфликты=${p.conflicts.size}")
    }
    val idx = ui.readString("Введите номер или нажмите Enter для пропуска: ")
    if (idx.nonEmpty) {
      Try(idx.toInt).toOption.filter(i => i >= 1 && i <= ok.size).foreach { i =>
        val (f, p) = ok(i - 1)
        ui.printLine(s"Файл: $f.")
        p.matches.foreach(m => ui.printLine(s"  [${m.rule.id}] позиция=${m.start}-${m.end} длина=${m.end - m.start}")) // выводим подробности по каждому совпадению
        p.conflicts.foreach(c => ui.printLine(s"  конфликт позиция=${c.start}-${c.end}: ${c.matches.map(_.rule.id).mkString(",")}"))
      }
    }

    // подтверждение операции
    val ans = ui.readString("Применить политику и создать копии? (y/n): ").trim.toLowerCase
    if (!ans.startsWith("y")) { ui.printLine("Операция отменена."); return }

    Files.createDirectories(outDir)
    val startTime = LocalDateTime.now()
    Logger.info(s"Начало обработки политики '$pol', файлов=${ok.size}.")

    val results = ok.map { case (f, p) =>
      FileProcessor.process(p, outDir, baseDir, state.cfg.settings.preserveStructure, state.cfg.settings.keepAliasMapping) // обработка файлов
    }
    val endTime = LocalDateTime.now()
    Logger.info(s"Обработка завершена: успешно=${results.count(_.error.isEmpty)}.")

    val report = Report.build(resolved, startTime, endTime, results, bad.size, outDir) // создаём отчёт
    ui.printLine(report.render)
    val reportPath = outDir.resolve("report.txt")
    Files.write(reportPath, report.render.getBytes("UTF-8"))
    ui.printLine(s"Отчёт об операции сохранён в папке: $reportPath.")
  }
}