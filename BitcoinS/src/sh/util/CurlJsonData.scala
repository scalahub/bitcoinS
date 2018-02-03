//
//package sh.util
//
//import java.io.BufferedReader
//import java.io.InputStreamReader
//import java.io.StringWriter
//import java.net.HttpURLConnection
//import java.net.URL
//import Json2XML._
//import org.apache.commons.io.IOUtils
//import org.apache.http.client.methods.HttpPost
//import org.apache.http.entity.ContentType
//import org.apache.http.entity.StringEntity
//import org.apache.http.impl.client.HttpClients
//import CommonUtil._
//import org.apache.http.util.EntityUtils
//
//object CurlJsonData {
//  // https://stackoverflow.com/questions/12059278/how-to-post-json-request-using-apache-httpclient
//  // https://stackoverflow.com/questions/13743205/how-to-add-set-and-get-header-in-request-of-httpclient
//  def curlXML(url:String, jsonEncodedString:String) = jsonStringToXML(curl(url, jsonEncodedString))
//  def curl(url:String, jsonEncodedString:String) = {
//    val http = new HttpPost(url)
//    http.setEntity(new StringEntity(jsonEncodedString, ContentType.APPLICATION_JSON))
//
//    using(HttpClients.createDefault()){httpclient => 
//      using (httpclient.execute(http)) {resp =>
//        val entity = resp.getEntity
//        val answer = using(new InputStreamReader(entity.getContent)){streamReader =>
//          using(new BufferedReader(streamReader)){reader =>
//            var line = ""
//            var str = ""
//            while({line = reader.readLine; line != null}) str = str + line
//            str
//          }
//        }
//        EntityUtils.consume(entity)
//        answer
//      }
//    }
//  }
//}
//
//object CurlJsonDataAlt { // makes curl request with JSON input (and possibly output)
//  def curlXML(url:String, jsonEncodedString:String) = jsonStringToXML(curl(url, jsonEncodedString))
//
//  /**
//   * Used for closing DB connections implicitly.
//   * Also used for writing / reading to files
//   * @author From the book "Beginning Scala"
//   */
//  def using[A <: {def close(): Unit}, B](param: A)(f: A => B): B =
//  try { f(param) } finally { param.close() }
//  
//  def curl(url:String, jsonEncodedString:String) = {
//    val httpcon = new URL(url).openConnection.asInstanceOf[HttpURLConnection]
//    httpcon.setDoOutput(true);
//    httpcon.setRequestProperty("Content-Type", "application/json");
//    httpcon.setRequestProperty("Accept", "application/json");
//    httpcon.setRequestMethod("POST");
//    httpcon.connect;
//    
//    val outputBytes = jsonEncodedString.getBytes("UTF-8");
//    using(httpcon.getOutputStream){os =>
//      os.write(outputBytes)
//    }
//    //https://stackoverflow.com/questions/309424/read-convert-an-inputstream-to-a-string
//    val code = httpcon.getResponseCode
//    val isError = code >= 400 && code <= 500
//    val resp = using{
//      // using method from here: https://stackoverflow.com/a/5218279/243233
//      if (isError) httpcon.getErrorStream else httpcon.getInputStream
//    }{is =>
//      val writer = new StringWriter;
//      IOUtils.copy(is, writer, "UTF-8");
//      writer.toString;
//    }
//    httpcon.disconnect
//    if (isError) throw new Exception(s"Resp code $code. Error: ${resp.take(200)}") else resp
//  }
//}
