package function_programming_0.report
import java.io.{FileWriter, PrintWriter}
import java.nio.file.{Files, Path, Paths}
import java.time.LocalDateTime
import java.time.format.DateTimeFormatter

// Логгер - журнал работы программы
object Logger {
  private val fmt = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss") // строка дата-время
  @volatile private var writer: Option[PrintWriter] = None // поток записи в файл (None, если logger ещё не инициализирован)
  @volatile private var logPath: Path = Paths.get("anonymizer.log") // путь к файлу журнала
  // инициализация журнала
  def init(path: Path): Unit = synchronized {
    logPath = path
    Option(path.getParent).foreach(p => if (!Files.exists(p)) Files.createDirectories(p)) // вычисляем родительские директории для каждого файла
    val fw = new FileWriter(path.toFile, true) // инициализируем поток записи
    writer = Some(new PrintWriter(fw, true))
    info("=== Запуск приложения ===")
  }
  // вызов конца работы программы
  def close(): Unit = synchronized {
    info("=== Завершение приложения ===")
    writer.foreach(_.close()) // закрываем запись
    writer = None
  }
  // запись в журнал - категория записи (уровень) и само сообщение
  private def log(level: String, msg: String): Unit = synchronized {
    val line = s"[${LocalDateTime.now().format(fmt)}] [$level] $msg"
    writer match {
      case Some(w) => w.println(line)
      case None    => println(line)
    }
  }
  // уровни записи - информирующий, предупреждение, ошибка
  def info(msg: String): Unit  = log("INFO", msg)
  def warn(msg: String): Unit  = log("WARN", msg)
  def error(msg: String): Unit = log("ERROR", msg)
}
