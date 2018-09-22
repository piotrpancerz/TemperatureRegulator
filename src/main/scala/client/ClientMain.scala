package client

import java.net.{InetAddress, Socket}
import java.io._
import scala.io._

import common.ServerSettings
import server.Res

object ClientMain {

  def main(args: Array[String]): Unit = {
    greet()
    val initial = getInitialValue()
    val lowerRange = getLowerRangeValue()
    val upperRange = getUpperRangeValue(lowerRange)
    val timeResolution = getTimeResolution()
    println
    simulate(initial, lowerRange, upperRange, timeResolution)
  }

  def greet(): Unit = {
    println("Welcome to \"Remote controlled air temperature controller\"")
  }

  def getInitialValue(): Double = {
    println(s"Please provide initial value of temperature (Float in range from -50 to 150)")
    var initial: Double = 0
    while(parseDouble(StdIn.readLine) match {
      case None => {
        println("Provided line is not a float value. Please try again.")
        true
      }
      case x => {
        (x.get < -50, x.get > 150.0) match {
          case (false,false) => {
            initial = x.get
            println(s"Initial value = $initial $celsiusDegree")
            false
          }
          case (true, false) => {
            println(s"Provided value is below the limit of -50 $celsiusDegree. Please try again.")
            true
          }
          case (_,_) => {
            println(s"Provided value is above the limit of 150 $celsiusDegree. Please try again.")
            true
          }
        }
      }
    }) initial
    initial
  }

  def parseDouble(x: String): Option[Double] = {
    try {
      Some(x.toDouble)
    } catch {
      case ex : Exception => None
    }
  }

  def getLowerRangeValue(): Double = {
    println("Please provide lower limit of acceptable temperature (Float in range from -50 to 150)")
    var lower: Double = 0
    while(parseDouble(StdIn.readLine) match {
      case None => {
        println("Provided line is not a float value. Please try again.")
        true
      }
      case x => {
        (x.get < -50.0, x.get > 150.0) match {
          case (false,false) => {
            lower = x.get
            println(s"Lower limit value = $lower $celsiusDegree")
            false
          }
          case (true, false) => {
            println(s"Provided value is below the limit of -50 $celsiusDegree. Please try again.")
            true
          }
          case (_,_) => {
            println(s"Provided value is above the limit of 150 $celsiusDegree. Please try again.")
            true
          }
        }
      }
    }) lower
    lower
  }

  def getUpperRangeValue(lower: Double): Double = {
    println(s"Please provide upper limit of acceptable temperature (Float in range from $lower to 150)")
    var upper: Double = 0
    while(parseDouble(StdIn.readLine) match {
      case None => {
        println("Provided line is not a float value. Please try again.")
        true
      }
      case x => {
        (x.get < lower, x.get > 150.0) match {
          case (false,false) => {
            upper = x.get
            println(s"Upper limit value = $upper $celsiusDegree")
            false
          }
          case (true, false) => {
            println(s"Provided value is below the limit of $lower $celsiusDegree. Please try again.")
            true
          }
          case (_,_) => {
            println(s"Provided value is above the limit of 150 $celsiusDegree. Please try again.")
            true
          }
        }
      }
    }) upper
    upper
  }

  def getTimeResolution(): Int = {
    println(s"Please choose time resolution in seconds (5/10/20/30/60)")
    var timeResolution = 5;
    while(parseInt(StdIn.readLine) match {
      case None => {
        println("Provided value is not supported. Please try again.")
        true
      }
      case Some(x) => {
        x == 5 || x == 10 || x == 20 || x == 30 || x == 60 match {
          case true => {
            timeResolution = x
            println(s"Time Resolution = ${timeResolution}s")
            false
          }
          case _ => {
            println("Provided value is not supported. Please try again.")
            true
          }
        }
      }
    }) timeResolution
    timeResolution
  }

  def parseInt(x: String): Option[Int] = {
    try {
      Some(x.toInt)
    } catch {
      case ex : Exception => None
    }
  }

  def simulate(_currentValue: Double, _lowerRange: Double, _upperRange: Double, _timeResolution: Int): Unit = {
    var currentValue = _currentValue
    var lowerRange = _lowerRange
    var upperRange = _upperRange
    var timeResolution = _timeResolution
    try {
      println("Start of simulation... \n")
      while(true) {
        val s = new Socket(InetAddress.getByName(host), port)
        val oos = new ObjectOutputStream(new DataOutputStream(s.getOutputStream()))
        val ois = new ObjectInputStream(new DataInputStream(s.getInputStream()))
        val req = new Req(currentValue, lowerRange, upperRange)
        oos.writeObject(req)
        oos.flush()
        val res = ois.readObject().asInstanceOf[Res]
        println(res.message)
        currentValue = res.currentValue
        Thread.sleep(1000L * timeResolution)
        s.close()
      }
    } catch {
      case ex: java.net.ConnectException => println("In order to simulate, you have to run the ServerMain object.")
    }
  }

  private val celsiusDegree = "\u2103"
  private val port = ServerSettings.port
  private val host = ServerSettings.host
}
