package me.haitaka.mad

import Zad3._
import breeze.linalg.DenseVector

import java.awt.Color
import java.lang.Math._

import Global._
import breeze.plot._
import breeze.linalg._

object Zad4 {

  def mu = DenseVector(mu1, mu2)
  def nu = DenseVector(v1, v2)

  def sigmFun(i: Int, j: Int) = {
    val (xI, muI) = if (i == 1) (X1, mu1) else (X2, mu2)
    val (xJ, muJ) = if (j == 1) (X1, mu1) else (X2, mu2)

    mean((0 until LEN1) map (_i => (xI(_i) - muI) * (xJ(_i) - muJ)))
  }

  def lambdFun(i: Int, j: Int) = {
    val (xI, muI) = if (i == 1) (X1, v1) else (X2, v2)
    val (xJ, muJ) = if (j == 1) (X1, v1) else (X2, v2)

    mean((LEN1 until LEN1 + LEN2) map (_i => (xI(_i) - muI) * (xJ(_i) - muJ)))
  }

  val sigm = DenseMatrix((sigmFun(1, 1), sigmFun(1, 2)), (sigmFun(2, 1), sigmFun(2, 2)))
  val lambd = DenseMatrix((lambdFun(1, 1), lambdFun(1, 2)), (lambdFun(2, 1), lambdFun(2, 2)))

  val midSigm = (sigm + lambd) / 2.0

  def s(l: Int, j: Int, k: Int): Double = {
    val (data, mid1, mid2) = {
      val pairs = X1 zip X2
      if (k == 1) {
        (pairs take LEN1, mu1, mu2)
      } else {
        assert(k == 2)
        (pairs drop LEN1, v1, v2)
      }
    }
    def f(x: Double, y: Double) = {
      val v1 = if (l == 1) x - mid1 else y - mid2
      val v2 = if (j == 1) x - mid1 else y - mid2
      v1 * v2
    }
    (data map {case (x, y) => f(x, y)}).sum
  }

  val Sw = DenseMatrix(
    (s(1, 1, 1) + s(1, 1, 2), s(1, 2, 1) + s(1, 2, 2)),
    (s(2, 1, 1) + s(2, 1, 2), s(2, 2, 1) + s(2, 2, 2))
  )

  var w = inv(Sw) * (mu - nu)
  w = w / norm(w)

  val proj = X1 zip X2 map {case (x, y) => (w*w.t) / (w.t * w) * DenseVector(x, y)} map (v => (v(0), v(1)))

  val ap = w(1) / w(0)
  val bp = {
    val (x0, y0) = proj(0)
    y0 - ap * x0
  }
  def yp(x: Double) = x * ap + bp

  def f0(x: Double, y: Double) = {
    val m1 = mean(X1)
    val m2 = mean(X2)
    w(0) * x + w(1) * y - w(0) * m1 - w(1) * m2
  }

  val ad = - w(0) / w(1)
  val bd = {
    val m1 = mean(X1)
    val m2 = mean(X2)
    val x0 = (w(0) * m1 + w(1) * m2 - w(1) * bp) / (w(0) + ap * w(1))
    val y0 = yp(x0)
    y0 - ad * x0
  }
  def yd(x: Double) = x * ad + bd

  def main(args: Array[String]): Unit = {
    val fig = Figure()
    val p = fig.subplot(0)

    p += scatter(X1 take LEN1, X2 take LEN1, _ => 1, _ => Color.red, _.toString)
    p += scatter(X1 takeRight LEN2, X2 takeRight LEN2, _ => 1, _ => Color.blue, x => (x + LEN1).toString)
    p += scatter(proj map (_._1), proj map (_._2), _ => 1, _ => Color.green, x => s"$x (${f0(proj(x)._1, proj(x)._2) match {
        case v if v > 0.0 => "+"
        case _ => "-"
      }})")
    println(proj)

    p += scatter(Seq(mu1), Seq(mu2), _ => 2, _ => Color.red, _ => "mu")
    p += scatter(Seq(v1), Seq(v2), _ => 2, _ => Color.blue, _ => "nu")

    val x = linspace(0.0, 190.0)
    p += plot(x, x map yp)
    p += plot(x, x map yd)

    fig.saveas(s"${Zad4.getClass.getSimpleName}.png")

    println("fin")
  }

}
