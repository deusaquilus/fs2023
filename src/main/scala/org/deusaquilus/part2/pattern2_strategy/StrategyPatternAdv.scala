package org.deusaquilus.part2.pattern2_strategy

import scala.io.AnsiColor.*
import scala.reflect.internal.TypeDebugging.AnsiColor

object StrategyPatternAdv {

object StrategyPattern {
sealed trait ColorStrategy
object ColorStrategy {
  case object Red extends ColorStrategy
  case object Green extends ColorStrategy
  case object Blue extends ColorStrategy
  case class Custom(colorFunc: (AnsiColor.type) => String) extends ColorStrategy
}

def showTitle(title: String, color: ColorStrategy): String = {
  val text = color match {
    case ColorStrategy.Red                => showTitle(title, ColorStrategy.Custom(_.RED))
    case ColorStrategy.Green              => showTitle(title, ColorStrategy.Custom(_.GREEN))
    case ColorStrategy.Blue               => showTitle(title, ColorStrategy.Custom(_.BLUE))
    case ColorStrategy.Custom(colorFunc)  => s"${AnsiColor.BLUE}${colorFunc(AnsiColor)}${RESET}"
  }
  s"======= ${text} ======="
}

  def main(args: Array[String]): Unit = {
    println(
      showTitle("Hello", ColorStrategy.Blue) +
      showTitle("How are you", ColorStrategy.Custom(_.MAGENTA))
    )
  }
}

}