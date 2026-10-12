package function_programming_0
import function_programming_0.cli.Commands
import function_programming_0.config.ConfigLoader
import function_programming_0.report.Logger
import java.nio.file.{Files, Paths}
// Главный модуль программы
object Main {
  def main(args: Array[String]): Unit = {
    val configPath = Paths.get(if (args.nonEmpty) args(0) else "config.json") // выбираем папку для конфиг-файла

    // создаём дефолтную конфигурацию, если файла нет
    if (!Files.exists(configPath)) {
      println(s"Конфигурация не найдена, будет использована конфигурация по умолчанию: $configPath")
      ConfigLoader.save(configPath, ConfigLoader.defaultConfig) match {
        case Right(_)  => Logger.info("Создана конфигурация по умолчанию.")
        case Left(err) => Logger.error(err)
      }
    }

    try {
      Commands.run(configPath) // запускаем меню
    } catch {
      case e: Throwable =>
        Logger.error(s"Внутренняя ошибка: ${e.getClass.getName}: ${e.getMessage}.")
        System.err.println(s"Внутренняя ошибка: ${e.getMessage}.")
        sys.exit(1)
    }
  }
}