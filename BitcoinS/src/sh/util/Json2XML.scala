
package sh.util

import org.json.JSONObject

object Json2XML {
  def jsonStringToXML(s:String) = try scala.xml.XML.loadString("<JSON>"+org.json.XML.toString(new JSONObject(s))+"</JSON>") catch {
    case e:Any => <error>{e.getMessage}</error>
  }
}
