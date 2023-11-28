package org.deusaquilus.part2.pattern2_strategy

import scala.io.AnsiColor.*
import scala.reflect.internal.TypeDebugging.AnsiColor

object StrategyPatternReg {

object StrategyPattern {
sealed trait ColorStrategy
object ColorStrategy {
  case object Red extends ColorStrategy
  case object Green extends ColorStrategy
  case object Blue extends ColorStrategy
}

  def showTitle(title: String, color: ColorStrategy) = {
    val text = color match {
      case ColorStrategy.Red   => s"${AnsiColor.RED}${title}${RESET}"
      case ColorStrategy.Green => s"${AnsiColor.GREEN}${title}${RESET}"
      case ColorStrategy.Blue  => s"${AnsiColor.BLUE}${title}${RESET}"
    }
    s"======= ${text} ======="
  }

  def main(args: Array[String]): Unit = {
    println(showTitle("Hello", ColorStrategy.Blue))
  }
}


object StrategyPatternLambda {
  def showTitle(title: String, color: (AnsiColor.type) => String) =
    s"======= ${color(AnsiColor)}${title}${RESET} ======="

  def main(args: Array[String]): Unit = {
    println(showTitle("Hello", _.BLUE))
  }
}

}