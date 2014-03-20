package net.cemetech.merthese

import javax.script._
import java.io.Reader
import scala.util.Random
import scala.collection.immutable.List
import scala.collection.mutable.Map
import scala.collection.JavaConverters._

object MertheseFactory extends ScriptEngineFactory{
	def getName:String = getLanguageVersion
	def getEngineName:String = "Elfprince's Scala Merthing@Kerm Interpreter"
	def getEngineVersion:String = "v1.0"
	/** As seen from object MertheseFactory, the missing signatures are as follows. 
	 * For convenience, these are usable as stub implementations. */ 
	def getExtensions(): java.util.List[String] = List[String]("mak","merth").asJava
	def getLanguageName():String = "Merthese" 
	def getLanguageVersion():String = "Merthing@Kerm" 
	def getMethodCallSyntax(obj:String,method:String,args:String*): String = throw new UnsupportedOperationException("Merthese cannot invoke Java code")
	def getMimeTypes() = List[String]("text/merthese","text/merthing-at-kerm").asJava 
	def getNames() = List[String]("merthese","merthing@kerm").asJava 
	def getOutputStatement(toDisplay: String): String = throw new UnsupportedOperationException("Merthese cannot display fixed strings")
	def getParameter(param: String): Object = param match {
		case ScriptEngine.ENGINE => getEngineName
		case ScriptEngine.ENGINE_VERSION => getEngineVersion
		case ScriptEngine.NAME => getName
		case ScriptEngine.LANGUAGE => getLanguageName
		case ScriptEngine.LANGUAGE_VERSION => getLanguageVersion
		case "THREADING" => null
	}
	def getProgram(stmts:String*) = stmts.mkString("")
	def getScriptEngine() = new MertheseEngine(true, System.currentTimeMillis())
}

class MertheseEngine(val atKerm:Boolean, val seed:Long) extends AbstractScriptEngine {
	val random = new Random(seed)
	
	def createBindings(): Bindings = {
		new SimpleBindings(Map[String,AnyRef]("accumulator" -> ('\0':java.lang.Character),"seeking" -> (false:java.lang.Boolean)).asJava)
	}

	def eval(srcReader: java.io.Reader,context: ScriptContext): Object = {
		Stream.continually(srcReader.read()).takeWhile(_ != -1).map(i => exec(i.toChar,context)).mkString("")
	} 
	
	def eval(srcString: String,context: ScriptContext): Object = {
		Stream.from(0).takeWhile(_ < srcString.length).map(i => exec(srcString.charAt(i),context)).mkString("")
	}
	
	def exec(char: Char, context: ScriptContext):String = {
		val state = context.getBindings(ScriptContext.ENGINE_SCOPE)
		if (state.get("seeking").asInstanceOf[java.lang.Boolean]){
			state.put("seeking",(false:java.lang.Boolean));""
		} else {
			val shouldKerm = atKerm && random.nextBoolean
			char match {
				case 'm' => if(shouldKerm){
						state.get("accumulator").asInstanceOf[java.lang.Character].charValue.toInt.toString
					} else{"merth"}
				case 'e' => if(shouldKerm){
					val c = state.get("accumulator").asInstanceOf[java.lang.Character].charValue()
					state.put("accumulator",
						(if (Character.UnicodeBlock.of(c+1) == Character.UnicodeBlock.BASIC_LATIN){
							c + 1
						} else {
							'\0'
						}:java.lang.Character)
					);"" 
				} else {"\n"}
				case 'r' => if(shouldKerm){
						state.put("accumulator",('\0':java.lang.Character));""
					} else {" "}
				case 't' => 
					random.alphanumeric.filter(_.isLower).take((random.nextDouble*13.4).toInt).mkString("")
				case 'h' => state.put("seeking",(true:java.lang.Boolean));""
				case 'k' =>  if(atKerm){
						state.get("accumulator").asInstanceOf[java.lang.Character].toString
					} else {""}
				case _ => ""
			}
		}
	}
	
	def getFactory() = MertheseFactory 
}
