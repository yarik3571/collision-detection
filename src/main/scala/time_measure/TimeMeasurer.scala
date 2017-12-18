package com.time_measure

object TimeMeasurer {
  def timed[A](name: String, f: => A): A = {
    val start = System.currentTimeMillis()
    val res = f
    val end = System.currentTimeMillis()
    val duration = end - start
    println(s"$name took $duration milis")
    res
  }
}
