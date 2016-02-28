package fos

import Parser._
import scala.util.parsing.input._

object Launcher {
  def main(args: Array[String]) {
    val stdin = new java.io.BufferedReader(new java.io.InputStreamReader(System.in))
    val tokens = new lexical.Scanner(stdin.readLine())
    phrase(Term)(tokens) match {
      case Success(term, _) =>
        try {
          val (tpe, c) = Infer.collect(Nil, term)
          val sub = Infer.unify(c)
          println("typed: " + sub(tpe))
        } catch {
          case tperror: Exception => println("type error: " + tperror.getMessage)
        }
      case e =>
        println(e)
    }
  }
}
