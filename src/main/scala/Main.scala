import org.scalajs.dom._

object Main {
  def main(args: Array[String]): Unit = {
    val canvas = document.createElement("canvas").asInstanceOf[html.Canvas]
    document.body.appendChild(canvas)
    TreeGenerator.draw(canvas)
  }
}
