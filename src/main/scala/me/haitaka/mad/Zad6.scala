package me.haitaka.mad

import Zad3._
import breeze.linalg.DenseVector

import java.awt.Color
import java.lang.Math._

import Global._
import breeze.plot._
import breeze.linalg._


object Zad6 {

  val TPR = Seq(0.6,   0.75,  0.8, 0.9,  0.95)  // TP / (TP + FN)
  val FPR = Seq(0.005, 0.008, 0.1, 0.15, 0.25)  // FP / (FP + TN)

  val income = 10 * V
  val lose =  30 * V

  val badRate = V / 100.0

  val Pm = 1 - badRate
  val Pp = badRate

  val Lpm = income
  val Lmp = lose

  //def R(i: Int) = Lmp * Pp * (1 - TPR(i)) + Lpm * Pm * FPR(i)
  def R(i: Int) = Lpm * Pm * (1 - FPR(i)) - Lmp * Pp * (1- TPR(i))


  def main(args: Array[String]): Unit = {
    println(s"R: ${(0 until TPR.length) map R}")
    println(s"reject all: ${income * Pp}")
    println(s"accept all: ${lose * Pm}")
  }

}
