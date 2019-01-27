package me.haitaka.mad


import Global._
import Zad3._
import breeze.numerics.sqrt

object Zad8 {

  var counter = 0

  trait Tree {
    val id = counter
    counter += 1
    def elems: Seq[Leaf]

    def print: Unit
  }

  class Group(val l: Tree, val r: Tree, val d: Metric) extends Tree {
    def elems = l.elems ++ r.elems

    override def print: Unit = {
      val q = "\""
      val dist = d(l, r)
      println(s"n$id [shape=square label=$q$dist$q];")
      println(s"n$id -> n${l.id}")
      l.print
      println(s"n$id -> n${r.id}")
      r.print
    }
  }

  class Leaf(val x: Double, val y: Double) extends Tree {
    def elems = Seq(this)

    override def print: Unit = {
      val q = "\""
      println(s"n$id [label=$q$id: ($x $y)$q];")
    }
  }

  type Metric = (Tree, Tree) => Double


  def d0(l1: Leaf, l2: Leaf): Double = sqrt(sqr(l1.x - l2.x) + sqr(l1.y - l2.y))


  def dMin(t1: Tree, t2: Tree): Double = {
    t1.elems map (e1 => t2.elems map (e2 => d0(e1, e2)) reduce min) reduce min
  }

  def dMax(t1: Tree, t2: Tree): Double = {
    t1.elems map (e1 => t2.elems map (e2 => d0(e1, e2)) reduce max) reduce max
  }

  val leafs = X1 zip X2 map {case (x, y) => new Leaf(x, y)}

  def closest(s: Seq[Tree], d: Metric): (Tree, Tree) = {
    val UNDEF = -1
    val closestTo = (s map (_ => UNDEF)).toArray
    for (i <- s.indices) {
      for (j <- s.indices) {
        if (i != j) {
          if (closestTo(i) == UNDEF || d(s(i), s(j)) < d(s(i), s(closestTo(i)))) {
            closestTo(i) = j
          }
        }
      }
    }
    var wantedI = 0
    for (i <- s.indices) {
      if (d(s(i), s(closestTo(i))) < d(s(wantedI), s(closestTo(wantedI)))) {
        wantedI = i
      }
    }
    (s(wantedI), s(closestTo(wantedI)))
  }

  def reduce(s: Seq[Tree], d: Metric): Tree = s match {
    case Nil => assert(false); null
    case Seq(t) => t
    case _ =>
      val cl = closest(s, d)
      reduce((s diff cl.productIterator.toList) ++ Seq(new Group(cl._1, cl._2, d)), d)
  }

  def main(args: Array[String]): Unit = {

    val tMax = reduce(leafs, dMax)
    val tMin = reduce(leafs, dMin)

    tMax.print

    println("fin")
  }

}
