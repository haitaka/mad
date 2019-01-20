package me.haitaka.mad

import scala.collection.mutable
import scala.io.Source
import scala.util.Random
import java.lang.Math._

object Zad5 {

  val DATA_FILENAME = "/home/haitaka/IdeaProjects/mad/res/agaricus-lepiota.data"

  val PROP_N = 22

  case class Entry(cl: String, props: Seq[String])

  object Entry {
    def scan(s: String): Entry = {
      val parts = s.split(",")
      assert(parts.length == PROP_N + 1)
      Entry(parts(0), parts drop 1)
    }
  }

  val data = Random.shuffle((Source.fromFile(DATA_FILENAME).getLines map Entry.scan).toList)

  val trainingData: Seq[Entry] = data take (data.size / 3 * 2)
  val controlData: Seq[Entry] = data drop trainingData.length

  val (edibleStats, poisonousStats) = { // Ne, Np
    val eStats = (0 until PROP_N) map (_ => mutable.HashMap.empty[String, Int])
    val pStats = (0 until PROP_N) map (_ => mutable.HashMap.empty[String, Int])

    for (e <- trainingData) {
      for (propId <- e.props.indices) {
        val stat = e.cl match {
          case "e" => eStats
          case "p" => pStats
          case _ => assert(false); null
        }
        val map = stat(propId)
        val propVal = e.props(propId)
        map.getOrElseUpdate(propVal, 0)
        map(propVal) += 1
      }
    }
    (eStats, pStats)
  }

  val N = trainingData.length
  val Ne = trainingData count (e => e.cl.equals("e"))
  val Np = N - Ne

  val Pe = Ne.toDouble / N
  val Pp = Np.toDouble / N

  def Pexj(j: Int, x: String) = edibleStats(j).getOrElse(x, 0).toDouble / Ne
  def Ppxj(j: Int, x: String) = poisonousStats(j).getOrElse(x, 0).toDouble / Np

  def l0(props: Seq[String]): Double = {
    val a = (props.zipWithIndex map {case (x, j) => Pexj(j, x)} map log).sum
    val b = (props.zipWithIndex map {case (x, j) => Ppxj(j, x)} map log).sum
    a - b + log(Pe / Pp)
  }

  def check(): Unit = {
    var TP = 0
    var FP = 0
    var TN = 0
    var FN = 0
    for (e <- controlData) {
      val pred = if (l0(e.props) >= 0) "e" else "p"
      (e.cl, pred) match {
        case ("e", "e") => TP += 1
        case ("e", "p") => FN += 1
        case ("p", "e") => FP += 1
        case ("p", "p") => TN += 1
        case _ => assert(false); null
      }
    }
    val P = TP + FP
    val N = TN + FN

    val accuracy = (TP + TN).toDouble / (P + N)
    val recall = TP.toDouble / P
    val precision = TP.toDouble / (TP + FP)

    println(s"accuracy: $accuracy")
    println(s"recall: $recall")
    println(s"precision: $precision")
  }

  def main(args: Array[String]): Unit = {
    check()
  }

}
