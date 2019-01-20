package me.haitaka.mad

import java.awt.Color
import java.lang.Math._

import breeze.linalg._
import breeze.plot._
import me.haitaka.mad.Global._

object Zad2 {

  val X =  Seq(70 + V, 160 - V, 80 + V, 75 + V, 400 - V, 140 + V, 230 - V, 300 + V, 200 + V, 180 - V) map (_.toDouble)
  val LEN1 = 4

  val P1 = LEN1.toDouble / X.length
  val P2 = 1 - P1

  val mu1 = mean(X take LEN1)
  val mu2 = mean(X drop LEN1)

  val sigm1 = sqrt(mean(X take LEN1 map (_ - mu1) map sqr))
  val sigm2 = sqrt(mean(X drop LEN1 map (_ - mu2) map sqr))

  def left(x: Double) = sqr(x - mu1) / sqr(sigm1) + log(P2 / P1 * sigm1 / sigm2)
  def right(x: Double) = sqr(x - mu2) / sqr(sigm2) - log(P2 / P1 * sigm1 / sigm2)

  def main(args: Array[String]): Unit = {
    val f = Figure()
    val p = f.subplot(0)

    p += scatter(X take LEN1, (X take LEN1) map (_ => 1.0), _ => 2, _ => Color.blue, x => x.toString)
    p += scatter(X drop LEN1, (X drop LEN1) map (_ => 1.0), _ => 2, _ => Color.red, x => (x + LEN1).toString)

    //p += scatter(Seq(mu1), Seq(1.0), _ => 4, _ => Color.blue, _ => "mu1")
    //p += scatter(Seq(mu2), Seq(2.0), _ => 4, _ => Color.red, _ => "mu2")

    val x = linspace(0.0, 400.0)
    p += plot(x, x map left)
    p += plot(x, x map right)

    f.saveas(s"${Zad2.getClass.getSimpleName}.png")

    println("fin")
  }

}
