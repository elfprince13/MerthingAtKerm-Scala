package net.cemetech.merthese

import javax.script.SimpleScriptContext
import java.io.OutputStreamWriter
import java.io.InputStreamReader
import scala.io.Source
import javax.script.ScriptContext

object RunInterpreter {
	val usage = "java net.cemetech.merthese.RunInterpreter [-s NNNNNNN] [-k] [-h] [infile]"

	def main(args:Array[String]):Unit = {
		type OptionMap = Map[Symbol, Any]
		val arglist = args.toList
		def nextOption(map : OptionMap, list: List[String]) : OptionMap = {
			def isSwitch(s : String) = (s(0) == '-')
			list match {
				case Nil => map
				case "-s" :: value :: tail =>
					nextOption(map ++ Map('seed -> value.toLong), tail)
				case "-k" :: tail =>
					nextOption(map ++ Map('kerm -> true), tail)
				case "-h" :: tail => println(usage)
					exit(0)
				case string :: opt2 :: tail if isSwitch(opt2) => 
					nextOption(map ++ Map('infile -> string), list.tail)
				case string :: Nil => nextOption(map ++ Map('infile -> string), list.tail)
				case option :: tail => 
					println("Unknown option "+option)
					println(usage)
					exit(1) 
			}
		}
		val options = nextOption(Map('seed -> System.currentTimeMillis(), 'infile -> 'stdin, 'kerm -> false),arglist)
		val engine = new MertheseEngine(options('kerm).asInstanceOf[Boolean],options('seed).asInstanceOf[Long])
		val engineContext = new SimpleScriptContext()
		engineContext.setWriter(new OutputStreamWriter(Console.out))
		engineContext.setErrorWriter(new OutputStreamWriter(Console.err))
		engineContext.setReader(options('infile) match {
			case 'stdin => Console.in
			case filename => Source.fromFile(filename.asInstanceOf[String]).bufferedReader
		})
		engineContext.setBindings(engine.createBindings, ScriptContext.ENGINE_SCOPE)
		val outString = engine.eval(engineContext.getReader(),engineContext).toString
		engineContext.getWriter().write(outString)
		engineContext.getWriter().write("\n")
		engineContext.getWriter().flush()
	}
}