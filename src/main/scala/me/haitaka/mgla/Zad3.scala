package me.haitaka.mgla

import breeze.linalg.svd.{Impl, SDenseSVD, SVD, doSVD_Float}
import breeze.linalg.{CompleteSVD, DenseMatrix, DenseVector, Matrix, svd}
import breeze.math.Complex

object Zad3 {

  val q = 1

  val a = DenseMatrix.tabulate(9, 9) {
    case (i, j) if i == j =>
      val diag = Seq(6d, 5d, 4d, 3d, 2d, 1d/2, 1d/3, 1d/4, 1d/5)
      diag(i)
    case (i, j) if j == i + 1 => q
    case _ => 0d
  }

//  def spectr(m: DenseMatrix[Double]) = {
//    val svd.SVD(_, s, _) = svd(m)
//    s.valuesIterator.toSeq
//  }
//
//  def spectralPortrait(m: DenseMatrix[Complex], lambda: Complex) = {
//    val d = m - lambda
//    d.repr
//    spectr(m - lambda).min
//  }

  def main(args: Array[String]): Unit = {
  }
}
