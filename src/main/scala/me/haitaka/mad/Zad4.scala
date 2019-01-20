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

  val w = inv(midSigm) * (mu - nu)

  val tmp = (log(P1) - log(P2)) / (mu - nu).t * inv(midSigm) * (mu - nu)

  val x0 = 0.5 * (mu + nu) - DenseVector(tmp(0), tmp(0))

  def y(x: Double) = ((w dot x0) - w(0) * x) / w(1)

  def main(args: Array[String]): Unit = {
    val f = Figure()
    val p = f.subplot(0)

    p += scatter(X1 take LEN1, X2 take LEN1, _ => 1, _ => Color.red, _.toString)
    p += scatter(X1 takeRight LEN2, X2 takeRight LEN2, _ => 1, _ => Color.blue, x => (x + LEN1).toString)

    p += scatter(Seq(mu1), Seq(mu2), _ => 2, _ => Color.red, _ => "mu")
    p += scatter(Seq(v1), Seq(v2), _ => 2, _ => Color.blue, _ => "v")

    val x = linspace(0.0, 300.0)
    p += plot(x, x map y)

    f.saveas(s"${Zad4.getClass.getSimpleName}.png")

    println("fin")
  }

}
