import akka.http.scaladsl.server.Route

trait Endpoint {
  def getEndpoint: Route
}
