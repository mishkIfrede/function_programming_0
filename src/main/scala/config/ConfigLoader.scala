package function_programming_0.config
import function_programming_0.model._
import function_programming_0.report.Logger
import play.api.libs.json._
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, Paths}
import scala.util.Try
// Обработчик конфигурационного файла
object ConfigLoader {
  // загрузка конфиг-файла
  def load(path: Path): Either[String, AppConfig] = {
    if (!Files.exists(path)) {
      Logger.warn(s"Конфигурационный файл не найден: $path")
      return Left(s"Файл не найден: $path")
    }
    Try {
      val text = new String(Files.readAllBytes(path), StandardCharsets.UTF_8) // прочтение файла
      Json.parse(text).as[AppConfig] // превращение json файла в AppConfig
    }.toEither.left.map(e => s"Ошибка чтения конфигурации: ${e.getMessage}") // иначе - ошибка
  }
  // сохранение в json
  def save(path: Path, cfg: AppConfig): Either[String, Path] = { // Unit
    Try {
      val json = Json.prettyPrint(Json.toJson(cfg)) // превращение конфигурации в читаемый (для человека) текст
      Files.write(path, json.getBytes(StandardCharsets.UTF_8)) // запись с кодировкой UTF-8
    }.toEither.left.map(e => s"Ошибка записи конфигурации: ${e.getMessage}")
  }
  // Создание дефолтного конфиг-файла (если файла нет)
  def defaultConfig: AppConfig = {
    val emailRule = Rule( // правило изменения почты
      id = "builtin.email",
      name = "Email",
      description = "Адрес электронной почты",
      search = SearchMethod.Builtin("email"), // поиск по встроенному типу
      replacement = Replacement.Alias("EMAIL"), // замена псевдонимом
      priority = 10,
      active = true
    )
    val phoneRule = Rule( // правило изменения номера телефона
      id = "builtin.phone",
      name = "Телефон",
      description = "Номер телефона",
      search = SearchMethod.Builtin("phone"), // поиск по встроенному типу
      replacement = Replacement.Alias("PHONE"), // замена псевдонимом
      priority = 10,
      active = true
    )
    val ipRule = Rule( // правило изменения IP-адреса
      id = "builtin.ipv4",
      name = "IPv4",
      description = "IPv4-адрес",
      search = SearchMethod.Builtin("ipv4"), // поиск по встроенному типу
      replacement = Replacement.Alias("IP"), // замена псевдонимом
      priority = 10,
      active = true
    )
    val urlRule = Rule( // правило изменения URL
      id = "builtin.url",
      name = "URL",
      description = "URL-адрес",
      search = SearchMethod.Builtin("url"), // поиск по встроенному типу
      replacement = Replacement.Alias("URL"), // замена псевдонимом
      priority = 10,
      active = true
    )
    val pwdRule = Rule( // правило изменения паролей
      id = "builtin.password",
      name = "Пароль",
      description = "Значение параметра, похожего на пароль",
      search = SearchMethod.Builtin("password"), // поиск по встроенному типу
      replacement = Replacement.Constant("***"), // замена константой
      priority = 20,
      active = true
    )
    val tokenRule = Rule( // правила изменения токена
      id = "builtin.token",
      name = "Токен",
      description = "Токен или ключ доступа",
      search = SearchMethod.Builtin("token"), // поиск по встроенному типу
      replacement = Replacement.Alias("TOKEN"), // замена псевдонимом
      priority = 20,
      active = true
    )

    val basePolicy = Policy( // создание базовой политики
      name = "base",
      description = "Базовая политика со встроенными типами",
      rules = List(emailRule, phoneRule, ipRule, urlRule, pwdRule, tokenRule) // встраиваем все вышесозданные правила в политику
    )
    val strictPolicy = Policy( // создание более строгой политики
      name = "strict",
      description = "Строгая политика (base + усиленная маскировка)",
      includes = List("base"), // включение базовой политики
      rules = List(
        Rule( // добавление нового правила для почтового адреса
          id = "custom.maskEmail",
          name = "Маскировать Email",
          description = "Полная маскировка email (переопределяет base)",
          search = SearchMethod.Builtin("email"), // поиск по встроенному типу
          replacement = Replacement.Mask(0, 0, '*'), // замена маской
          priority = 100, // более высокий приоритет, чем у базовой политики
          active = true
        )
      )
    )
    AppConfig( // возвращаем созданный конфиг-файл
      settings = AppSettings(),
      policies = Map("base" -> basePolicy, "strict" -> strictPolicy)
    )
  }
}
