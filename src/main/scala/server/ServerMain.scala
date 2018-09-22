package server

import java.net.{BindException, ServerSocket}
import java.io._

import common.ServerSettings
import client.Req

object ServerMain {
  def main(args: Array[String]): Unit = {
    val server = new ServerSocket(port)
    println(s"Server running on port : $port...")

    while (true) {
      val s = server.accept()
      val oos = new ObjectOutputStream(new DataOutputStream(s.getOutputStream()))
      val ois = new ObjectInputStream(new DataInputStream(s.getInputStream()))
      val req = ois.readObject().asInstanceOf[Req]
      println(s"Received request object : $req")
      val res = reqToRes(req)
      oos.writeObject(res)
      oos.flush()
      println(s"Sent response object : $res")
      s.close()
    }
  }

  def reqToRes(req: Req): Res = {
    val newValue = defineNewValue(req.currentValue, req.desiredValue)
    new Res(req.currentValue, newValue, req.desiredValue)
  }

  def defineNewValue(currentValue: Double, desiredValue: Double): Double = {
    var newValue = currentValue
    currentValue - desiredValue match {
      case 0 => newValue = math.rint((currentValue + (math.random()-0.5)*1)*100)/100
      case diff if diff < 0 => {
        diff match {
          case x if x > -5 => newValue = math.rint((currentValue + math.random()*1)*100)/100
          case x if x > -10 => newValue = math.rint((currentValue + math.random()*2)*100)/100
          case _ => newValue = math.rint((currentValue + math.random()*3)*100)/100
        }
      }
      case diff if diff > 0 => {
        diff match {
          case x if x > -5 => newValue = math.rint((currentValue - math.random()*1)*100)/100
          case x if x > -10 => newValue = math.rint((currentValue - math.random()*2)*100)/100
          case _ => newValue = math.rint((currentValue - math.random()*3)*100)/100
        }
      }
    }
    newValue
  }

  private val port = ServerSettings.port
}
