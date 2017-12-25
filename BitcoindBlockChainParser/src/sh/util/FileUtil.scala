package sh.util

import java.io.File
import java.io.FileWriter
import org.apache.commons.io.FileUtils

object FileUtil {
  def getAllFiles(dir:String, extensions:Array[String], recursive:Boolean) = try {
    FileUtils.listFiles(new File(dir), extensions, recursive).toArray.map(_.toString)
  } catch {
    case _ : Throwable => Array[String]()
  }
  def using[A <: {def close(): Unit}, B](param: A)(f: A => B): B =
  try { f(param) } finally { param.close() }

  def writeToTextFile(fileName:String, data:String) = using (new FileWriter(fileName)) {
    fileWriter => {
      fileWriter.write(data)
    }
  }

}
