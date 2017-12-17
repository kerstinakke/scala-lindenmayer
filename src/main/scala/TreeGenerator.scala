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
      "..S"->0.6,
      ".[(30)..B][(-30)..B]S"->0.2,
      ".[(30)..B]S"->0.1,
      ".[(-30)..B]S"->0.1,
    ),
    'B'->Map(
      "..[(30).B][(-30).B]B"->0.2,
      ".[(30).B]B"->0.2,
      ".[(-30).B]B"->0.2,
      "<.L"->0.4
    ),
    'L'->Map(
      "[(40).*]*[(-40).*]"->0.7,
      "[(40).*][(-40).*]"->0.3
    )

  ).withDefaultValue(Map.empty[String,Double])

  val canvas = document.getElementById("treeCanvas").asInstanceOf[html.Canvas]
  val ctx = canvas.getContext("2d")

  case class State(memory: List[String], future: List[String])

  class Reactor($: BackendScope[Unit, State]) {

    // TODO: Show current tree as string
    def render(state: State) = {
      draw(  if (state.memory.isEmpty) "" else state.memory.head ,state.memory.size+3)
      <.div(
        <.button("Uus", ^.onClick ==> drawNew),
        <.button("Eelmine", ^.disabled := (state.memory.isEmpty ), ^.onClick ==> drawPrev),
        <.button("Järgmine", ^.disabled := (state.future.isEmpty), ^.onClick ==> drawNext),
        <.p(state.memory.size)
      )

    }

    def drawNew(e: ReactEventFromInput): CallbackTo[Unit] = {
      $.modState(s => {
        if (s.memory.isEmpty) State(makeNext("") :: Nil, List.empty[String])
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
      case c if c.isUpper => acc + applyRule(c)
      case other => acc + other
    }

    if (current.isEmpty) root else current.foldLeft("")(nextStep)

  }


  def draw(tree: String,startWidth:Int): Unit = {

    case class DrawingDetails(pos:(Int,Int),direction:(Double,Double),len:Int,width:Double){
      def getCos(degrees:Int):Double = Math.cos(Math.toRadians(degrees))
      def getSin(degrees:Int):Double = Math.sin(Math.toRadians(degrees))

      def turn(degrees:Int):(Double,Double) = {
        (getCos(degrees)*direction._1-getSin(degrees)*direction._2,direction._1*getSin(degrees)+direction._2*getCos(degrees))
      }
      def newPos(): (Int,Int) = (Math.round(pos._1+len*direction._1).asInstanceOf[Int],Math.round(pos._2+len*direction._2).asInstanceOf[Int])
      def draw():DrawingDetails = {
        ctx.beginPath()
        ctx.moveTo(pos._1+0.1,pos._2+0.1)
        ctx.lineWidth = Math.max(width.asInstanceOf[Int],1)
        val newDetails = copy(pos=newPos,width=width-0.4)
        ctx.lineTo(newDetails.pos._1,newDetails.pos._2)
        ctx.stroke()
        newDetails
      }
    }


    val w = canvas.width
    val h = canvas.height

    var stack = List(DrawingDetails((w/2,h),(0,-1),20,startWidth))

    ctx.fillStyle = "white"
    ctx.fillRect(0, 0, w, h)
    ctx.strokeStyle = "brown"

    var current = stack.head
    var numToParse = ""
    var waitingNumber = false

    //console.log(tree)
    tree.foreach(_ match {
      case '.' =>
        current = current.draw()
      case '[' =>
        stack = current::stack
        current = current.copy(width=current.width-1)
      case '<' =>
        current = current.copy(width=current.width-startWidth*0.5)
      case ']' =>
        current = stack.head
        stack=stack.tail
      case '(' =>
        numToParse=""
        waitingNumber = true
      case ')' =>
        current = current.copy(direction = current.turn(Integer.parseInt(numToParse)),len=current.len-1)
        waitingNumber = false
      case c if waitingNumber => numToParse+=c
      case c if c.isUpper || c.equals('*') =>
        ctx.strokeStyle="green"
        current.copy(len=10,width=Math.max(6,current.width*1.5)).draw()
        ctx.strokeStyle="brown"
      case _ => ()
    })

    ctx.stroke()
  }
}
