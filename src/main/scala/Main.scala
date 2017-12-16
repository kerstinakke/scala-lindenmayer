import org.scalajs.dom._

object Main {
  def main(args: Array[String]): Unit = {
    TreeGenerator.renderInto(document.getElementById("dirt"))
  }
}
