import org.scalajs.dom._
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._

object TreeGenerator {
  val root = "--S" //Starting point: two straights and a split-point

  case class State(memory:List[String],future:List[String])

  class Reactor($: BackendScope[Unit, State]){

    def render(state: State) = {
      if (!state.memory.isEmpty) draw(state.memory.head)
      <.div(
      <.button("Uus",^.onClick==>drawNext),
      <.button("Järgmine",^.disabled:=(state.future.isEmpty))
    )
    }


    def drawNext(e: ReactEventFromInput): CallbackTo[Unit] = {
      $.modState(s=> State(makeNext(if(s.memory.isEmpty) "" else s.memory.head)::s.memory,"-"::Nil))
    }



  }

  private val component = ScalaComponent.builder[Unit]("TreeGenerator")
    .initialState(State(List.empty[String],List.empty[String]))
    .renderBackend[Reactor]
    .build

  def renderInto(c: Element): Unit = {
    component().renderIntoDOM(c)
  }

  def makeNext(current:String):String = {
    def nextStep(acc:String, c:Char):String = c match {
      case 'S' => acc + "-S"
      case other => acc + other
    }
    if(current.isEmpty) root else current.foldLeft("")(nextStep)

  }

  def draw(tree:String):Unit = {
    val canvas = document.getElementById("treeCanvas").asInstanceOf[html.Canvas]
    val ctx = canvas.getContext("2d")
    val w = canvas.width
    val h = canvas.height
    var current = h
    val len = 5

    ctx.strokeStyle = "red"
    ctx.lineWidth = 3
    ctx.beginPath()
    ctx.moveTo(w/2,current)

    tree.foreach( _ match {
      case '-' =>
        current-=len
        ctx.lineTo(w/2,current)
      case _ => ()
    })

    ctx.stroke()
  }
}
