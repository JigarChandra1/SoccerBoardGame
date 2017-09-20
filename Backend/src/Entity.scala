import scala.util.Random


trait Entity {
  var x: Int = 0
  var y: Int = 0
  def getEntityCoordinates: (Int, Int) = {
    (x, y)
  }
  def moveEntity(newX: Int, newY: Int): Unit = {
    x = newX
    y = newY
  }
}

class Player(playerTeam: Int, playerNumber: Int) extends Entity {
  def getPlayerTeamAndNumber = {
    (playerTeam, playerNumber)
  }
}

object SoccerBall extends Entity{
  
}

object Dice {
  def getRoll = (Random.nextInt(6) + 1)
  var player1TurnRoll = 0
  var player2TurnRoll = 0
}

// e.g position(2)(3) = (0, 0) Soccer ball is at position 2, 3
object Board {
  var positions = Array.ofDim[(Int, Int)](11, 11)
}

object GoalsScored {
  var player1Goals = 0
  var player2Goals = 0
}