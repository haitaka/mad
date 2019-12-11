package me.haitaka.mgla

import breeze.linalg._
import breeze.numerics._
import org.apache.commons.math3.linear.{Array2DRowRealMatrix, ArrayRealVector, QRDecomposition}

import collection.JavaConverters._
import scala.collection.immutable.NumericRange
import scala.collection.mutable

object Zad2 {

  val rand = scala.util.Random

  val lBound = -1d
  val rBound = 1d

  case class Sample(n1: Int, n2: Int, m: Int, delta: Double) {

    val exact: Seq[Double] =
      Seq(1d, 0d, -pow(constants.Pi, 2)/2, 0d, pow(constants.Pi, 4)/24, 0d)

    val x: Seq[Double] = {
      val right = (1 to n1) map (_.toDouble / n1)
      val left = (1 to n2) map (-_.toDouble / n2)
      (left ++ Seq(0D) ++ right).sorted
    }

    val pointsN = (n1 + 1 + n2) ensuring (_ == x.length)

    def f(x: Double): Double = cos(constants.Pi * x)

    def fTilda(x: Double) = f(x) + rand.between(-delta, delta)

    def P(x: Double, k: Int): Double = pow(x, k.toDouble)

    val A: DenseMatrix[Double] = {
      val a = DenseMatrix.zeros[Double](pointsN, m + 1)
      a.foreachKey{ case (i, j) => a(i, j) = P(x(i), j) }
      a
    }

    def pFun(x: Double, p: Vector[Double]) = (p.iterator map {case (i, v) => v * P(x, i)}).sum

    def solve() = {
      val c = cond(A)
      val (p, r) = leastResidual(A, DenseVector(x map fTilda: _*))
      val diff = (x map (x => f(x) - pFun(x, p)) map(abs(_))).max
      val rn = maxNorm(r)
      val check = delta + rn - diff

//      println(s"N1: $n1, N2: $n2, M: $m, detla: $delta")
//      println(f"cond $c%1.4f")
//      println(f"r ${rn}%1.4f")
//      println(f"diff $diff%1.4f")
//      println()

      (c, rn, diff, check)
    }
  }

  def maxNorm(t: Tensor[Int, Double]): Double = t.valuesIterator.map(abs(_)).max

  def leastResidual(A: DenseMatrix[Double], b: DenseVector[Double]) = {
    val apacheA = new Array2DRowRealMatrix(A.rows, A.cols)
    A foreachPair {
      case ((i, j), v) => apacheA.setEntry(i, j, v)
    }
    val apacheB = new ArrayRealVector((b.length))
    b foreachPair {
      case (i, v) => apacheB.setEntry(i, v)
    }
    val qr = new QRDecomposition(apacheA)
    val apacheX = qr.getSolver.solve(apacheB)
    val x = DenseVector(apacheX.iterator().asScala.map(_.getValue).toSeq: _*)
    val r = A * x - b
    (x, r)
  }

  def range(from: Double, to: Double, step: Double) = {
    val top = round((to - from) / step).toInt
    (0 to top) map (lBound + _ * step)
  }

  def main(args: Array[String]): Unit = {
    val m = 6
    val n2 = 20

    val cMap = mutable.HashMap.empty[(Int, Int), Double]
    val rMap = mutable.HashMap.empty[(Int, Int), Double]
    val dMap = mutable.HashMap.empty[(Int, Int), Double]
    val chMap = mutable.HashMap.empty[(Int, Int), Double]

    for (n1 <- Seq(10, 20, 30)) {
      for (deltaPow <- Seq(-2, -4, -6)) {
        val delta = pow(10D, deltaPow)
        val (cond, r, diff, check) = Sample(n1, n2, m, delta).solve()

        cMap((n1, deltaPow)) = cond
        rMap((n1, deltaPow)) = r
        dMap((n1, deltaPow)) = diff
        chMap((n1, deltaPow)) = check
      }
    }


    for (deltaPow <- Seq(-2, -4, -6)) {
      print(s" & $$\\delta=10^{${deltaPow}}$$")
      for (n1 <- Seq(10, 20, 30)) {
        print(f" & ${cMap((n1, deltaPow))}%1.4f")
      }
      print(" \\\\\n")
    }
    println()

    for (deltaPow <- Seq(-2, -4, -6)) {
      print(s" & $$\\delta=10^{${deltaPow}}$$")
      for (n1 <- Seq(10, 20, 30)) {
        print(f" & $$${rMap((n1, deltaPow))}%1.4f$$")
      }
      print(" \\\\\n")
    }
    println()

    for (deltaPow <- Seq(-2, -4, -6)) {
      print(s" & $$\\delta=10^{${deltaPow}}$$")
      for (n1 <- Seq(10, 20, 30)) {
        print(f" & $$${dMap((n1, deltaPow))}%1.4f$$")
      }
      print(" \\\\\n")
    }
    println()

    for (deltaPow <- Seq(-2, -4, -6)) {
      print(s" & $$\\delta=10^{${deltaPow}}$$")
      for (n1 <- Seq(10, 20, 30)) {
        print(f" & $$${chMap((n1, deltaPow))}%1.6f$$")
      }
      print(" \\\\\n")
    }
    println()

  }

}
