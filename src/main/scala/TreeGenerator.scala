import org.scalajs.dom
import org.scalajs.dom.html


object TreeGenerator {
  val root = "--S" //Starting point: two straights and a split-point
  var memory = List.empty[String]

  def makeNext():Unit = if(memory.isEmpty) {
    memory = root :: memory
  }

  def draw(c: html.Canvas):Unit = {
    type Ctx2D =
      dom.CanvasRenderingContext2D
    val ctx = c.getContext("2d")
      .asInstanceOf[Ctx2D]
    val w = 300
    c.width = w
    c.height = w

    ctx.strokeStyle = "red"
    ctx.lineWidth = 3
    ctx.beginPath()
    ctx.moveTo(w/3, 0)
    ctx.lineTo(w/3, w/3)
    ctx.moveTo(w*2/3, 0)
    ctx.lineTo(w*2/3, w/3)
    ctx.moveTo(w, w/2)
    ctx.arc(w/2, w/2, w/2, 0, 3.14)

    ctx.stroke()
  }
}
