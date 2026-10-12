package function_programming_0.cli
import function_programming_0.config.ConfigLoader
import function_programming_0.io.{FileProcessor, FileScanner, Selection}
import function_programming_0.model._
import function_programming_0.policy.{PolicyResolver, PolicyValidator}
import function_programming_0.report.{Logger, Report}
import java.nio.file.{Files, Path, Paths}
import java.time.LocalDateTime
import scala.io.StdIn
import scala.util.Try
// Модель меню - реализует пользовательский интерфейс программы
class Menu(initialConfigPath: Path) {
  private var configPath: Path = initialConfigPath
  private var cfg: AppConfig = ConfigLoader.load(configPath).getOrElse(ConfigLoader.defaultConfig) // используем пользовательский конфиг, если его нет - конфиг по умолчанию
  // сохранение файла конфигурации
  private def save(): Unit = {
    ConfigLoader.save(configPath, cfg) match {
      case Right(_)  => Logger.info(s"Конфигурация сохранена в папку: $configPath.")
      case Left(err) => Logger.error(err); println(err) // при ошибке в сохраняемом конфиге
    }
  }
  // перезагрузка конфигурации (при её редактировании пользователем)
  private def reload(): Unit = {
    ConfigLoader.load(configPath) match {
      case Right(c) => cfg = c; Logger.info("Конфигурация перезагружена.")
      case Left(e)  => Logger.error(e); println(e) // при ошибке в загружаемом конфиге
    }
  }
  // инициализация
  def run(): Unit = {
    Logger.init(Paths.get(cfg.settings.logFile)) // запускаем журнал
    Logger.info(s"Загружена конфигурация: $configPath.")
    var loop = true
    while (loop) { // главный цикл программы
      printMainMenu() // выводим на экран все опции
      val choice = StdIn.readLine("> ").trim
      choice match {
        case "1"  => listPolicies()
        case "2"  => showPolicy()
        case "3"  => createPolicy()
        case "4"  => editPolicy()
        case "5"  => deletePolicy()
        case "6"  => composePolicy()
        case "7"  => validateConfig()
        case "8"  => processFiles()
        case "9"  => previewMatches()
        case "10" => reload()
        case "11" => save()
        case "0"  => loop = false
        case other =>
          println(s"Неизвестная команда: $other.")
          Logger.warn(s"Неизвестная команда (ввод не логируется).")
      }
    }
    Logger.close() // закрываем журнал при выходе из цикла - конец программы
  }
  // вывод на экран возможных опций
  private def printMainMenu(): Unit = {
    println(
      """
        |=========== ПРОГРАММА - АНОНИМИЗАТОР ПЕРСОНАЛЬНЫХ ДАННЫХ ===========
        | Введите одну из цифр для выбора соответствущей опции:
        |1 -  Показать список политик
        |2 -  Показать политику и правила
        |3 -  Создать новую политику
        |4 -  Изменить существующую политику
        |5 -  Удалить политику
        |6 -  Составить политику из существующих
        |7 -  Проверить конфигурацию системы
        |8 -  Обработать файлы
        |9 -  Предварительный просмотр совпадений
        |10 - Перезагрузить конфигурацию
        |11 - Сохранить конфигурацию
        |0 -  Выход из программы
        |====================================
        |""".stripMargin)
  }

  // Далее - разбор каждой из опций
  // вывод списка существующих политик
  private def listPolicies(): Unit = {
    println("Список всех политик:")
    cfg.policies.toList.sortBy(_._1).foreach { case (name, p) => // политики сортируем по id
      println(s" - $name: ${p.description} (правила=${p.rules.size}, включения=${p.includes.mkString(",")})")
    }
  }
  // обзор конкретной политики
  private def showPolicy(): Unit = {
    val name = StdIn.readLine("Введите имя политики: ").trim
    cfg.policies.get(name) match { // ищем политику по имени
      case None => println(s"Политика с именем $name не найдена.")
      case Some(p) =>
        println(s"Политика '$name': ${p.description}")
        if (p.includes.nonEmpty) println(s"Включает: ${p.includes.mkString(", ")}")
        println("Собственные правила:")
        p.rules.foreach { r =>
          println(s"  [${if (r.active) "x" else " "}] id=${r.id} name=${r.name} prio=${r.priority}")
          println(s"      desc: ${r.description}")
          println(s"      search: ${r.search}")
          println(s"      replace: ${r.replacement}")
        }
    }
  }
  // создание политики
  private def createPolicy(): Unit = {
    val name = StdIn.readLine("Введите имя новой политики: ").trim
    if (name.isEmpty) { println("Имя для политики не введено."); return }
    if (cfg.policies.contains(name)) { println("Политика с таким именем уже существует."); return }
    val desc = StdIn.readLine("Введите описание политики: ").trim
    cfg = cfg.copy(policies = cfg.policies + (name -> Policy(name, desc))) // добавляем новую политику к существующим
    Logger.info(s"Создана политика: $name.")
    println("Для добавления правил или включения других политик перейдите в режим 'Изменить политику'.")
    save()
  }
  // редактирование существующей политики
  private def editPolicy(): Unit = {
    val name = StdIn.readLine("Введите имя политики для изменения: ").trim
    cfg.policies.get(name) match {
      case None => println(s"Политика с именем $name не найдена.")
      case Some(p) =>
        println("Введите одну из цифр для выбора соответствущей опции:")
        println("1 - Изменить описание")
        println("2 - Добавить правило")
        println("3 - Удалить правило")
        println("4 - Переключить активность правила")
        println("5 - Добавить включение (include)")
        println("6 - Удалить включение (include)")
        StdIn.readLine("> ").trim match {
          case "1" =>
            val d = StdIn.readLine("Введите новое описание: ").trim
            cfg = cfg.copy(policies = cfg.policies + (name -> p.copy(description = d)))
            println("Описание политики успешно изменено.")
          case "2" =>
            readRule() match { // сборка правила из ввода пользователя
              case Some(r) =>
                if (p.rules.exists(_.id == r.id)) println(s"Правило с id ${r.id} уже есть.")
                else {
                  cfg = cfg.copy(policies = cfg.policies + (name -> p.copy(rules = p.rules :+ r))) // новое правило добавляем в конец
                  Logger.info(s"Добавлено правило ${r.id} в политику $name/")
                }
              case None => println("Правило не добавлено.")
            }
          case "3" =>
            val id = StdIn.readLine("Введите id правила: ").trim
            cfg = cfg.copy(policies = cfg.policies + (name -> p.copy(rules = p.rules.filterNot(_.id == id)))) // в конфиг заносим все правила, кроме этого
            Logger.info(s"Удалено правило $id из политики $name.")
          case "4" =>
            val id = StdIn.readLine("Введите id правила: ").trim
            cfg = cfg.copy(policies = cfg.policies + (name -> p.copy(rules = p.rules.map(r => if (r.id == id) r.copy(active = !r.active) else r))))
            Logger.info(s"Переключена активность правила $id в $name.")
          case "5" =>
            val inc = StdIn.readLine("Введите имя включаемой политики: ").trim
            cfg.policies.get(inc) match {
              case None => println(s"Политика с именем $name не найдена.")
              case Some(p) =>
                if (inc.nonEmpty && !p.includes.contains(inc))
                  cfg = cfg.copy(policies = cfg.policies + (name -> p.copy(includes = p.includes :+ inc)))
                  Logger.info(s"Политика с имененм $inc включена в политику с имененм $name.")}
          case "6" =>
            val inc = StdIn.readLine("Введите имя исключаемой политики: ").trim
            cfg.policies.get(inc) match {
              case None => println(s"Политика с именем $name не найдена.")
              case Some(p) =>
                cfg = cfg.copy(policies = cfg.policies + (name -> p.copy(includes = p.includes.filterNot(_ == inc))))
                Logger.info(s"Политика с имененм $inc исключена из политики с имененм $name.")}
          case _ => println("Неизвестный режим.")
        }
        save() // сохраняем изменение
    }
  }
  // удаление политики
  private def deletePolicy(): Unit = {
    val name = StdIn.readLine("Введите имя политики для удаления: ").trim
    if (cfg.policies.contains(name)) {
      cfg = cfg.copy(policies = cfg.policies - name)
      Logger.info(s"Удалена политика: $name.")
      save()
    } else println(s"Политика с именем $name не найдена.")
  }
  // создание составной политики из существующих
  private def composePolicy(): Unit = {
    val name = StdIn.readLine("Введите имя новой составной политики: ").trim
    if (name.isEmpty || cfg.policies.contains(name)) { println("Некорректное или уже существующее имя."); return }
    println("Доступные для включения политики: " + cfg.policies.keys.mkString(", "))
    val inc = StdIn.readLine("Введите имена включаемых политик через запятую: ").split(",").map(_.trim).filter(_.nonEmpty).toList
    val missing = inc.filterNot(cfg.policies.contains)
    if (missing.nonEmpty) { println(s"Не найдены политики, вследствие чего не будут добавлены: ${missing.mkString(", ")}."); return }
    val desc = StdIn.readLine("Введите описание составляемой политики: ").trim
    cfg = cfg.copy(policies = cfg.policies + (name -> Policy(name, desc, Nil, inc)))
    Logger.info(s"Создана составная политика: $name (включения=${inc.mkString(",")}).")
    save()
  }
  // создание нового правила (используется в createPolicy())
  private def readRule(): Option[Rule] = {
    println("Создание нового правила")
    val id = StdIn.readLine("Введите id правила (уникальный): ").trim
    if (id.isEmpty) return None
    val name = StdIn.readLine("Введите имя правила: ").trim
    val desc = StdIn.readLine("Введите описание правила: ").trim
    println("Укажите тип поиска(введите цифру с желаемой опцией):")
    println("1 - Точное значение")
    println("1 - Регулярное выражение")
    println("1 - Словарь значений")
    println("1 - Встроенный тип данных")
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
    println("1 - Маскирование")
    println("1 - Замена псевдонимом")
    println("1 - Замена значением из списка")
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
    val prio = Try(StdIn.readLine("Введите приоритет правила ив виде числа (по умолчанию 0): ").trim.toInt).getOrElse(0)
    val active = StdIn.readLine("Ваше правило активно на данный момент? (y/n): ").trim.toLowerCase.startsWith("y")
    Some(Rule(id, name, desc, search, repl, prio, active))
  }
  // проверка конфигурации
  private def validateConfig(): Unit = {
    val errs = PolicyValidator.validate(cfg)
    if (errs.isEmpty) println("Конфигурация корректна.")
    else {
      println("Обнаружены проблемы:")
      errs.foreach(e => println(" - " + e))
    }
    Logger.info(s"Проверка конфигурации: ошибок=${errs.size}.")
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
    println("Доступные политики: " + cfg.policies.keys.mkString(", "))
    val name = StdIn.readLine("Введите имя политики: ").trim
    if (cfg.policies.contains(name)) Some(name) else { println("Политика не найдена."); None }
  }
  // переопределение правила (используется в previewMatches() и processFiles())
  private def resolveOrReport(name: String): Option[function_programming_0.policy.ResolvedPolicy] = {
    PolicyResolver.resolve(name, cfg.policies) match {
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
    val files = FileScanner.scan(sel, cfg.settings.allowedExtensions, Paths.get(cfg.settings.outputDir))
    println(s"Найдено файлов: ${files.size}. Анализ...")
    files.foreach { f =>
      FileProcessor.analyze(f, resolved.rules) match {
        case Right(prev) =>
          println(s"\n- Файл: ${f.getFileName}.")
          println(s"  Найдено совпадений: ${prev.matches.size}, конфликтов: ${prev.conflicts.size}.")
          prev.matches.take(20).foreach { m => // показываем первые 20 совпадений
            // НЕ выводим само значение, только тип/правило/позицию
            println(s"    [${m.rule.id}] позиция ${m.start}-${m.end} (len=${m.end - m.start}).")
          }
          if (prev.matches.size > 20) println(s"    ... и ещё ${prev.matches.size - 20}.")
        case Left(e) => println(s"- Файл ${f.getFileName}: $e.")
      }
    }
  }
  // прогон файлов - анализ, совпадения, конфликты...
  private def processFiles(): Unit = {
    val pol = selectPolicy().getOrElse(return)
    val resolved = resolveOrReport(pol).getOrElse(return)
    val (sel, baseDir) = chooseSelection().getOrElse(return)
    val outDir = Paths.get(cfg.settings.outputDir).toAbsolutePath.normalize()

    val files = FileScanner.scan(sel, cfg.settings.allowedExtensions, outDir)
    if (files.isEmpty) { println("Нет подходящих файлов."); return }

    // предварительный анализ
    println(s"Найдено файлов: ${files.size}. Анализ...")
    val previews = files.map(f => f -> FileProcessor.analyze(f, resolved.rules))
    val ok = previews.collect { case (f, Right(p)) => f -> p } // успешно проанализированные файлы
    val bad = previews.collect { case (f, Left(e)) => f -> e } // файлы с ошибками

    val totalMatches = ok.map(_._2.matches.size).sum // суммируем все совпадения по успешным файлам
    val totalConflicts = ok.map(_._2.conflicts.size).sum // суммируем все конфликты по успешным файлам
    println(s"Всего совпадений: $totalMatches, конфликтов: $totalConflicts.")
    println("Показать детали по файлу? Введите номер файла или нажмите Enter для пропуска: ")
    ok.zipWithIndex.foreach { case ((f, p), i) =>
      println(f"  ${i + 1}%3d) ${f.getFileName} совпадения=${p.matches.size} конфликты=${p.conflicts.size}")
    }
    val idx = StdIn.readLine("Введите номер или нажмите Enter для пропуска: ").trim
    if (idx.nonEmpty) {
      Try(idx.toInt).toOption.filter(i => i >= 1 && i <= ok.size).foreach { i =>
        val (f, p) = ok(i - 1)
        println(s"Файл: $f.")
        p.matches.foreach(m => println(s"  [${m.rule.id}] позиция=${m.start}-${m.end} длина=${m.end - m.start}")) // выводим подробности по каждому совпадению
        p.conflicts.foreach(c => println(s"  конфликт позиция=${c.start}-${c.end}: ${c.matches.map(_.rule.id).mkString(",")}"))
      }
    }

    // подтверждение операции
    val ans = StdIn.readLine("Применить политику и создать копии? (y/n): ").trim.toLowerCase
    if (!ans.startsWith("y")) { println("Операция отменена."); return }

    Files.createDirectories(outDir)
    val startTime = LocalDateTime.now()
    Logger.info(s"Начало обработки политики '$pol', файлов=${ok.size}.")

    val results = ok.map { case (f, p) =>
      FileProcessor.process(p, outDir, baseDir, cfg.settings.preserveStructure, cfg.settings.keepAliasMapping) // обработка файлов
    }
    val endTime = LocalDateTime.now()
    Logger.info(s"Обработка завершена: успешно=${results.count(_.error.isEmpty)}.")

    val report = Report.build(resolved, startTime, endTime, results, bad.size, outDir) // создаём отчёт
    println(report.render)
    val reportPath = outDir.resolve("report.txt")
    Files.write(reportPath, report.render.getBytes("UTF-8"))
    println(s"Отчёт об операции сохранён в папке: $reportPath.")
  }
}