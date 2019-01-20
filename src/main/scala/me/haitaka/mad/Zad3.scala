package me.haitaka.mad

import java.awt.Color
import java.lang.Math.log

import Global._
import breeze.plot._
import breeze.linalg._

object Zad3 {

  val X1 =  Seq(40 + V, 100 - V, 30 + V, 25 + V, V,       100 + V, /**/ 230 - V, 110 + V, 120 + V, 180 - V) map (_.toDouble)
  val X2 =  Seq(10 + V, 110 - V, 20 + V, 15 + V, 105 - V, 120 + V, /**/ 220 - V, 90 + V,  200 - V, 160 - V) map (_.toDouble)
  val LEN1 = 6
  val LEN2 = 4

  val P1 = LEN1.toDouble / (LEN1 + LEN2)
  val P2 = LEN2.toDouble  / (LEN1 + LEN2)

  val mu1 = mean(X1 take LEN1)
  val mu2 = mean(X2 take LEN1)

  val v1 = mean(X1 takeRight LEN2)
  val v2 = mean(X2 takeRight LEN2)

  val sigm11 = mean(X1 take LEN1 map (_ - mu1) map sqr)
  val sigm22 = mean(X2 take LEN1 map (_ - mu2) map sqr)

  val lambd11 = mean(X1 takeRight LEN2 map (_ - v1) map sqr)
  val lambd22 = mean(X2 takeRight LEN2 map (_ - v2) map sqr)

  val midSigm = mean(Seq(sigm11, sigm22, lambd11, lambd22))

  def a = (mu1 - v1) / (v2 - mu2)
  def b(s: Double) = (sqr(v1) + sqr(v2) - sqr(mu1) - sqr(mu2)) / 2 / (v2 - mu2) - s / (v2 - mu2) * log(P2 / P1)

  def main(args: Array[String]): Unit = {
    val f = Figure()
    val p = f.subplot(0)

    p += scatter(X1 take LEN1, X2 take LEN1, _ => 1, _ => Color.red, _.toString)
    p += scatter(X1 takeRight LEN2, X2 takeRight LEN2, _ => 1, _ => Color.blue, x => (x + LEN1).toString)

    p += scatter(Seq(mu1), Seq(mu2), _ => 2, _ => Color.red, _ => "mu")
    p += scatter(Seq(v1), Seq(v2), _ => 2, _ => Color.blue, _ => "v")

    val x = linspace(0.0, 300.0)
    p += plot(x, x * a + b(midSigm))

    f.saveas(s"${Zad3.getClass.getSimpleName}.png")

    println("fin")
  }

}
