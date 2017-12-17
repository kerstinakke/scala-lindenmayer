import org.scalajs.dom._
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._

import scala.util.Random

object TreeGenerator {
  val random = new Random()
  val root = "..S" //Starting point: two forward and a split-point
  //Rules for each variable should add up to 1
  //I am using capital letters for variables
  // TODO: Write legend somehere
  val rules = Map(
    'S'->Map(
      ".S"->0.29,
      ".[(30).S][(-30).S]"->0.7,
      ".."->0.01
    ).withDefaultValue(0.0)).withDefaultValue(Map.empty[String,Double])

  val canvas = document.getElementById("treeCanvas").asInstanceOf[html.Canvas]
  val ctx = canvas.getContext("2d")

  case class State(memory: List[String], future: List[String])

  class Reactor($: BackendScope[Unit, State]) {

    // TODO: Show current tree as string
    def render(state: State) = {
      if (!state.memory.isEmpty) draw(state.memory.head)
      <.div(
        <.button("Uus", ^.onClick ==> drawNew),
        <.button("Eelmine", ^.disabled := (state.memory.isEmpty || state.memory.size == 1), ^.onClick ==> drawPrev),
        <.button("Järgmine", ^.disabled := (state.future.isEmpty), ^.onClick ==> drawNext)
      )
    }

    def drawNew(e: ReactEventFromInput): CallbackTo[Unit] = {
      $.modState(s => {
        if (s.memory.isEmpty) State(makeNext("") :: "" :: Nil, List.empty[String])
        else State(makeNext(s.memory.head) :: s.memory, List.empty[String])
      })
    }

    def drawPrev(e: ReactEventFromInput): CallbackTo[Unit] = {
      $.modState(s => {
        State(s.memory.tail, s.memory.head :: s.future)
      })
    }

    def drawNext(e: ReactEventFromInput): CallbackTo[Unit] = {
      $.modState(s => {
        State(s.future.head :: s.memory, s.future.tail)
      })
    }

  }

  private val component = ScalaComponent.builder[Unit]("TreeGenerator")
    .initialState(State(List.empty[String], List.empty[String]))
    .renderBackend[Reactor]
    .build

  def renderInto(c: Element): Unit = {
    component().renderIntoDOM(c)
  }

  def applyRule(c:Char): String ={
    val current = random.nextDouble()
    var lim = 0.0

    def wasChosen(value:Double): Boolean = if (current < value+lim) true
      else {
        lim+=value
        false
      }

    rules(c).dropWhile( a => !wasChosen(a._2)).head._1
  }

  def makeNext(current: String): String = {
    def nextStep(acc: String, c: Char): String = c match {
      case 'S' => acc + applyRule(c)
      case other => acc + other
    }

    if (current.isEmpty) root else current.foldLeft("")(nextStep)

  }

  case class DrawingDetails(pos:(Int,Int),direction:(Double,Double),len:Int){
    def getCos(degrees:Int):Double = Math.cos(Math.toRadians(degrees))
    def getSin(degrees:Int):Double = Math.sin(Math.toRadians(degrees))

    def turn(degrees:Int):(Double,Double) = {
      (getCos(degrees)*direction._1-getSin(degrees)*direction._2,direction._1*getSin(degrees)+direction._2*getCos(degrees))
    }
    def newPos(): (Int,Int) = (Math.round(pos._1+len*direction._1).asInstanceOf[Int],Math.round(pos._2+len*direction._2).asInstanceOf[Int])
  }

  def draw(tree: String): Unit = {
    val w = canvas.width
    val h = canvas.height

    var stack = List(DrawingDetails((w/2,h),(0,-1),20))

    ctx.fillStyle = "white"
    ctx.fillRect(0, 0, w, h)
    ctx.strokeStyle = "red"
    ctx.lineWidth = 3
    ctx.beginPath()

    var current = stack.head
    var numToParse = ""
    var waitingNumber = false
    ctx.moveTo(current.pos._1,current.pos._2)

    console.log(tree)
    tree.foreach(_ match {
      case '.' =>
        current = current.copy(pos=current.newPos())
        ctx.lineTo(current.pos._1,current.pos._2)
      case '[' =>
        stack=current::stack
        ctx.strokeStyle = "red"
      case ']' =>
        current=stack.head
        ctx.stroke()
        ctx.beginPath()
        ctx.moveTo(current.pos._1,current.pos._2)
        stack=stack.tail
      case '(' =>
        numToParse=""
        waitingNumber = true
      case ')' =>
        current = current.copy(direction = current.turn(Integer.parseInt(numToParse)))
        waitingNumber = false
      case '-' if waitingNumber =>
        numToParse+='-'
        ctx.strokeStyle = "green"
      case c if waitingNumber => numToParse+=c
      case _ => ()
    })

    ctx.stroke()
  }
}
