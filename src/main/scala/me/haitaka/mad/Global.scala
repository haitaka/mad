package me.haitaka.mad

object Global {

  final val V = 60.0

  def mean(s: Seq[Double]) = s.sum / s.length

  def sqr(x: Double) = x * x

  def abs(x: Double) = if (x >= 0) x else -x

  def min(x: Double, y: Double) = if (x >= y) y else x
  def max(x: Double, y: Double) = if (x >= y) x else y

}
