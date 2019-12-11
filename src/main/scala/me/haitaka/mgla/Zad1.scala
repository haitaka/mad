package me.haitaka.mgla

import java.io.{File, PrintWriter}

import breeze.linalg._
import breeze.linalg.svd.Svd_DM_Impl
import breeze.math._
import breeze.numerics._

object Zad1 {

  def maxNorm(m: Matrix[Double]) = m.valuesIterator.max

  def solve(N: Integer, d: Double, b: Double): Unit = {
    val M: DenseMatrix[Double] = Tridiag(N, d, b)

    val dec = svd(M)(Svd_DM_Impl)

    val res = maxNorm(M - (dec.U * diag(dec.S) * dec.Vt))

    val uRes = maxNorm(dec.U * dec.U.t - DenseMatrix.eye[Double](N))
    val vRes = maxNorm(dec.Vt * dec.Vt.t - DenseMatrix.eye[Double](N))

    println(s"res: $res, uRes: $uRes, vRes: $vRes")
  }

  def Tridiag(N: Integer, d: Double, b: Double) = {
    val M = DenseMatrix.zeros[Double](N, N)
    for (i <- 0 until N) {
      for (j <- 0 until N) {
        if (i == j) {
          M(i, j) = d
        } else if (i == j - 1 || i == j + 1) {
          M(i, j) = d
        }
      }
    }
    M
  }

  def main(args: Array[String]): Unit = {
    val d = 2D
    val b = 4D

    solve(10, d, b)
    solve(20, d, b)
    solve(30, d, b)
  }
}
