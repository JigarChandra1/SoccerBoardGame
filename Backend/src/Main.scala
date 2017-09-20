import akka.stream.ActorMaterializer
import akka.actor.ActorSystem
import scala.concurrent.ExecutionContext.Implicits.global
import akka.http.scaladsl.Http
import org.json4s.JsonAST.JArray
import org.json4s.JsonAST.JDouble
import org.json4s.JsonAST.JInt
import org.json4s.JsonAST.JNothing
import org.json4s.JsonAST.JObject
import org.json4s.JsonAST.JString
import org.json4s.JsonAST.JValue
import org.json4s.JsonDSL.int2jvalue
import org.json4s.JsonDSL.jobject2assoc
import org.json4s.JsonDSL.pair2Assoc
import org.json4s.JsonDSL.pair2jvalue
import org.json4s.JsonDSL.string2jvalue
import org.json4s.jvalue2monadic
import org.json4s.native.JsonMethods.compact
import org.json4s.native.JsonMethods.parse
import org.json4s.native.JsonMethods.render
import org.json4s.string2JsonInput
object Main extends App{
  val (player1Coins, player2Coins) = init
  def init = {
    val range = (0 to 4)
    val player1Coins = range.map { x => new Player(1, x) }
    val player2Coins = range.map { x => new Player(2, x) }
    player1Coins(1).moveEntity(0, 5)
    (player1Coins, player2Coins)
    }
    val endpoint = new GameEndpoint(player1Coins, player2Coins)
    val route = endpoint.getEndpoint
    endpoint.setCoins
    
    implicit val system = ActorSystem()
    implicit val materializer = ActorMaterializer()

    val f = Http().bindAndHandle(route, "localhost", 9000)
    while(true) {
      println(s"Running")
      Thread.sleep(20000)
    }
  
}

object diceTester extends App {
  for(i <- 1 to 10) {
    SoccerBall.moveEntity(i, i)
    println(s"soccer ball at: ${SoccerBall.getEntityCoordinates}")
    println(s"for ${(i, i)}")
    println(s"Goal scored: ${goalScored}")
  }
    private def goalScored: (Boolean, Int) = {
    val player1GoalPositions = List((0, 4), (0, 5), (0, 6))
    val player2GoalPositions = List((10, 4), (10, 5), (10, 6))
    if(player1GoalPositions.contains(SoccerBall.getEntityCoordinates)) return (true, 2)
    if(player2GoalPositions.contains(SoccerBall.getEntityCoordinates)) return (true, 1)
    (false, 0)
  }
}