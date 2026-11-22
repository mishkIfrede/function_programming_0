package cli
import java.nio.file.Path
// Точка входа в программу
object Commands {
  def run(configPath: Path): Unit = new Menu(configPath, CLI).run()
}
