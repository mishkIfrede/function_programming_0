package function_programming_0.io
import java.nio.file.{Files, Path}
import scala.jdk.CollectionConverters._
// Сканер файла(ов), необходимых к замене полей
sealed trait Selection
object Selection { // виды файлов
  case class SingleFile(path: Path) extends Selection
  case class MultipleFiles(paths: List[Path]) extends Selection
  case class Directory(path: Path, recursive: Boolean) extends Selection
}

object FileScanner {
  // сканирование файла(ов)
  def scan(sel: Selection, allowedExtensions: List[String], outputDir: Path): List[Path] = {
    val allowed = allowedExtensions.map(_.toLowerCase).toSet // приводим все файловые расширения к одному регистру и формируем множество
    val absOut = outputDir.toAbsolutePath.normalize() // нормализуем файловый путь
    // проверка имени файла на принадлежность к доступным расширениям
    def isAllowed(p: Path): Boolean = {
      val name = p.getFileName.toString.toLowerCase
      allowed.exists(ext => name.endsWith(ext))
    }
    // проверка, находится ли выходной файл в одном каталоге с входным
    def notInOutput(p: Path): Boolean = {
      val abs = p.toAbsolutePath.normalize()
      !abs.startsWith(absOut)
    }
    // проверка вышенаписанных условий и возврат списка файла(ов)
    sel match {
      case Selection.SingleFile(p) => if (Files.exists(p) && isAllowed(p) && notInOutput(p)) List(p) else Nil
      case Selection.MultipleFiles(ps) =>
        ps.filter(p => Files.exists(p) && isAllowed(p) && notInOutput(p))
      case Selection.Directory(dir, recursive) =>
        if (!Files.isDirectory(dir)) Nil // если объект - это не каталог
        else {
          val stream = Files.walk(dir, if (recursive) Int.MaxValue else 1) // возвращает все пути каталога
          try {
            stream.iterator().asScala // создаём Scala Iterator[Path]
              .filter(p => Files.isRegularFile(p)) // оставляем только файлы
              .filter(isAllowed)
              .filter(notInOutput)
              .toList
              .sortBy(_.toString) // сортируем по строковому представлению пути
          } finally stream.close() // закрываем стрим
        }
    }
  }
}
