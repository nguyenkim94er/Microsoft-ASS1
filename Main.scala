// Project name MP
// Create by: nhphung
// Create date: Aug 27, 2013
// Language: Scala
// Description: This file is about driver for the recognizer for MP language, see MP language specification for more information

import scala.io.Source

object Main {

  def main(args: Array[String]) {
    
	if (args.length > 1) {
		val option = args(0).drop(1)
	    val inputFile = args(1)
	 
		option match {
		  case "help" => printHelp
		  case "testlexer" => {
			  val lines = Source.fromFile(inputFile).getLines
			  val input = if (!lines.isEmpty) lines.reduceLeft[String](_ + '\n' + _) else ""
			  val lexical = new BKOOLLexer
		      val scanner = new lexical.Scanner(input)
	
			  runAll(scanner)
		
			  def clean(token: lexical.Token): String = {
				val t = token.chars
				if (t.indexOf("expected but") != -1) {
					val from = t.indexOf("but")
					val to = t.indexOf("found")
					"ErrorToken " + t.subSequence(from + 4, to - 1).toString
				} else token.toString
			  }
	
			  def runAll(scan: lexical.Scanner): Any = if (!scan.atEnd) {
				  println(clean(scan.first))
				  runAll(scan.rest)
			  }
		  }
		  case "testrecogniser" => {
			val lines = Source.fromFile(inputFile).getLines
			val input = if (!lines.isEmpty) lines.reduceLeft[String](_ + '\n' + _) else ""
			val parser = new BKOOLRecognizer
			val result = parser.parse(input)
			
			println(parser.show(result))
		  }
		  case _ => printHelp
		}
	} else printHelp
  }
  def printHelp = {
    println("Usage: scala Main -option inputFile")
    println("where option is")
    println("help: print this help")
    println("testlexer: test lexer")
    println("testrecogniser: test recogniser")
  }
}