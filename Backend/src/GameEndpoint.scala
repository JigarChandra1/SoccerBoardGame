import java.net.URI

import akka.http.scaladsl.marshalling.ToResponseMarshallable.apply
import akka.http.scaladsl.model.HttpEntity.apply
import akka.http.scaladsl.model.HttpResponse
import akka.http.scaladsl.model.StatusCodes._
import akka.http.scaladsl.server.Directive.addByNameNullaryApply
import akka.http.scaladsl.server.Directive.addDirectiveApply
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.Route
import scala.math.BigInt.int2bigInt

import org.json4s.DefaultFormats
import org.json4s.JField
import org.json4s.JsonAST.JArray
import org.json4s.JsonAST.JInt
import org.json4s.JsonAST.JNull
import org.json4s.JsonAST.JObject
import org.json4s.JsonAST.JString
import org.json4s.JsonAST.JValue
import org.json4s.JsonDSL.jobject2assoc
import org.json4s.JsonDSL.pair2Assoc
import org.json4s.JsonDSL.pair2jvalue
import org.json4s.JsonDSL.seq2jvalue
import org.json4s.jvalue2monadic
import org.json4s._
import org.json4s.JsonDSL._
import org.json4s.native.JsonMethods.compact
import org.json4s.native.JsonMethods.render
import org.json4s.native.JsonMethods.parse
import akka.http.scaladsl.model.HttpHeader
import akka.http.scaladsl.model.headers.RawHeader
import akka.http.scaladsl.model.HttpEntity
import akka.http.scaladsl.model.ContentTypes

class GameEndpoint(player1Coins: IndexedSeq[Player], player2Coins: IndexedSeq[Player]) extends Endpoint {

  val responseHeader = List(RawHeader("Access-Control-Allow-Origin", "*"))
  
  override def getEndpoint: Route = {
    pathPrefix("game") {
      post {
        path("player" / "move") {
          entity(as[String]) { moveJson =>
            val playerMove = parse(moveJson)
            val move = playerMove \ "move"
            val tackleMove = playerMove \ "tackle"
            val pass = playerMove \ "pass"
            movePlayer(move)
            if (!tackleMove.equals(JNothing)) movePlayer(tackleMove, true)
            if (!pass.equals(JNothing)) movePlayer(pass)
            complete(HttpResponse(OK, responseHeader, HttpEntity(ContentTypes.`text/plain(UTF-8)`, "Move Accepted")))
          }
        } ~ path("dice" / "turnRoll") {
          entity(as[String]) { moveJson =>
            val move = parse(moveJson)
            val JInt(playerTeam) = move.\("playerTeam")
            playerTeam.toInt match {
              case 1 => {
                val roll = Dice.getRoll
                println(s"Player 1 roll is: $roll")
                Dice.player1TurnRoll = roll
                val rollJson: JObject = ("roll" -> Dice.player1TurnRoll)
                complete(HttpResponse(OK, responseHeader, HttpEntity(ContentTypes.`application/json`, compact(render(rollJson)))))
              }
              case 2 => {
                val roll = Dice.getRoll
                println(s"Player 2 roll is: $roll")
                Dice.player2TurnRoll = roll
                val rollJson: JObject = ("roll" -> Dice.player2TurnRoll)
                complete(HttpResponse(OK, responseHeader, HttpEntity(ContentTypes.`application/json`, compact(render(rollJson)))))
              }
            }
          }
        } ~ path("dice" / "firstToMove") {
          val playerToMoveFirst = (Dice.player1TurnRoll > Dice.player2TurnRoll) match {
            case true  => 1
            case false => 2
          }
          val rollJson: JObject = ("player" -> playerToMoveFirst)
          complete(HttpResponse(OK, responseHeader, HttpEntity(ContentTypes.`application/json`, compact(render(rollJson)))))
        } ~ path("dice" / "predict") {
          entity(as[String]) { moveJson =>
            val move = parse(moveJson)
            val JString(roll) = move.\("roll")
            val JInt(playerTeam) = move.\("playerTeam")
            val diceRoll = Dice.getRoll
            ((roll.equals("down") && diceRoll < 4) || (roll.equals("up") && diceRoll > 3)) match {
              case true => {
                val rollJson = parse(s"""{ "diceRoll": ${diceRoll}, "predictedSuccessfully" : "true", "playerToMove" : ${playerTeam} }""")
                complete(HttpResponse(OK, responseHeader, HttpEntity(ContentTypes.`application/json`, compact(render(rollJson)))))
              }
              case false => {
                val playerWithPenaltyMove = if (playerTeam == 1) 2 else 1
                val rollJson = parse(s"""{ "diceRoll": ${diceRoll}, "predictedSuccessfully" : "false", "playerWithPenaltyMove" : ${playerWithPenaltyMove} }""")
                complete(HttpResponse(OK, responseHeader, HttpEntity(ContentTypes.`application/json`, compact(render(rollJson)))))
              }
            }
          }
        } ~ path("evaluate") {
          val (goalScoredValue, playerTeam) = goalScored
          (goalScoredValue, playerTeam) match {
            case (true, 1) => {
              GoalsScored.player1Goals += 1
              setCoins
            }
            case (true, 2) => {
              GoalsScored.player2Goals += 1
              setCoins
            }
            case (_, _) =>
          }
          val evaluationJson = parse(s"""{ "goalScored" : "${goalScoredValue}", "player1Goals" : "${GoalsScored.player1Goals}", "player2Goals" : "${GoalsScored.player2Goals}"  }""")
          complete(HttpResponse(OK, responseHeader, HttpEntity(ContentTypes.`application/json`, compact(render(evaluationJson)))))
        } ~ path("positions") {
          // TODO: make this a GET operation
          val player1Positions =
            for (i <- 0 to 4) yield {
              val xPos = ("x", JInt(player1Coins(i).x))
              val yPos = ("y", JInt(player1Coins(i).y))
              val combined = JObject(List(xPos, yPos))
              JObject(List((i.toString, combined)))
            }
          val player2Positions =
            for (i <- 0 to 4) yield {
              val xPos = ("x", JInt(player2Coins(i).x))
              val yPos = ("y", JInt(player2Coins(i).y))
              val combined = JObject(List(xPos, yPos))
              JObject(List((i.toString, combined)))
            }
          val soccerBallXpos = ("x", JInt(SoccerBall.x))
          val soccerBallYpos = ("y", JInt(SoccerBall.y))
          val soccerBallPosition = JObject(List(soccerBallXpos, soccerBallYpos))
          val combPlayers = JArray(List(soccerBallPosition, player1Positions, player2Positions))
          val positions = compact(render(combPlayers))
          complete(HttpResponse(OK, responseHeader, HttpEntity(ContentTypes.`application/json`, positions)))
        }
      }
    }
  }

  private def playerHasBall(player: Player) = {
    player.getEntityCoordinates.equals(SoccerBall.getEntityCoordinates)
  }
  private def movePlayer(move: JValue, isTackled: Boolean = false) = {
    val JInt(playerTeam) = move \ "playerTeam"
    val JInt(playerNumber) = move \ "playerNumber"
    val JInt(x) = move.\("x")
    val JInt(y) = move.\("y")
    playerTeam.toInt match {
      case 0 => {
        SoccerBall.moveEntity(x.toInt, y.toInt)
      }
      case 1 => {
        if (!isTackled && playerHasBall(player1Coins(playerNumber.toInt))) {
          SoccerBall.moveEntity(x.toInt, y.toInt)
        }
        player1Coins(playerNumber.toInt).moveEntity(x.toInt, y.toInt)
      }
      case 2 => {
        if (!isTackled && playerHasBall(player2Coins(playerNumber.toInt))) SoccerBall.moveEntity(x.toInt, y.toInt)
        player2Coins(playerNumber.toInt).moveEntity(x.toInt, y.toInt)
      }
    }
  }

  def setCoins = {
    SoccerBall.moveEntity(5, 5)
    player1Coins(0).moveEntity(0, 5)
    player2Coins(0).moveEntity(10, 5)
    player1Coins(1).moveEntity(1, 4)
    player2Coins(1).moveEntity(9, 4)
    player1Coins(2).moveEntity(1, 6)
    player2Coins(2).moveEntity(9, 6)
    player1Coins(3).moveEntity(2, 3)
    player2Coins(3).moveEntity(8, 3)
    player1Coins(4).moveEntity(2, 7)
    player2Coins(4).moveEntity(8, 7)
  }

  private def goalScored: (Boolean, Int) = {
    val player1GoalPositions = List((0, 4), (0, 5), (0, 6))
    val player2GoalPositions = List((10, 4), (10, 5), (10, 6))
    if (player1GoalPositions.contains(SoccerBall.getEntityCoordinates)) return (true, 2)
    if (player2GoalPositions.contains(SoccerBall.getEntityCoordinates)) return (true, 1)
    (false, 0)
  }
}
