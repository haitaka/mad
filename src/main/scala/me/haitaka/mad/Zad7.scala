package me.haitaka.mad

import java.awt.Color

import java.lang.Math._
import Global._
import breeze.linalg.linspace
import breeze.plot.{Figure, plot, scatter}

object Zad7 {

  val X = Seq(V + 13, V + 16, V + 19, V + 23, V + 26, V + 30, V + 42)
  val Y = Seq(V + 3,  V + 5,  V + 4,  V + 6,  V + 6,  V + 9,  V + 8)

  val N = X.length

  val mX = mean(X)
  val mY = mean(Y)

  val b1 = (((X zip Y) map {case (x, y) => x * y}).sum - N * mY * mX) / ((X map sqr).sum - N * sqr(mX))
  val b0 = mY - b1 * mX

  def prediction(x: Double) = b0 + x * b1

  val e = (X zip Y) map {case (x, y) => y - prediction(x)}

  val RSS = (e map sqr).sum
  val s2 = RSS / (N - 1 - 1)
  val TSS = (Y map (_ - mY) map sqr).sum
  val ESS = (X map (prediction(_) - mY) map sqr).sum

  val R2 = ESS / TSS

  val r = ((X zip Y) map {case (x, y) => (x - mX) * (y - mY)}).sum / sqrt((X map (x => sqr(x - mX))).sum) / sqrt((Y map (y => sqr(y - mY))).sum)

  def main(args: Array[String]): Unit = {
    val f = Figure()
    val p = f.subplot(0)

    p += scatter(X, Y, _ => 1, _ => Color.red, _.toString)
    p += scatter(Seq(mX), Seq(mY), _ => 2, _ => Color.red, _ => "mid")

    val x = linspace(60.0, 105.0)
    p += plot(x, x map prediction)

    f.saveas(s"${Zad7.getClass.getSimpleName}.png")

    println(s"Y(X) = ${b0} + ${b1} * X + e")
    println(s"cor: ${r} ??")
    println(s"det: ${R2}")
    println(s"mid err: ${mean(e map abs)}")
    println(s"err s2: ${s2}")
    println(s"pred(10): ${prediction(10)}")
    println("e")
    e foreach println

    println("fin")
  }

}
