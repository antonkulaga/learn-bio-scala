package org.Bionic.Lesson1

class SimplePatternCount (f:String) {
  import java.io.File

import scala.io.Source

  val data = Source.fromFile(new File(f)) //(d:/hello.txt)
  val newData = data.getLines.toArray

  def patternCount (text : String, pattern : String) = {
    text.sliding(pattern.length).foldLeft(0)((acc,el) =>
      if (el == pattern) acc + 1 else acc)
  }
  def result = patternCount(newData(0), newData(1))
}
