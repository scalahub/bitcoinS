
package sh.util

import akka.actor.ActorSystem
import akka.actor.Cancellable
import java.util.concurrent.atomic.AtomicLong
import scala.collection.mutable.{Map => MMap}
import scala.concurrent.duration._

object AkkaUtil {
  val ctr = new AtomicLong(0L)
  val actorSystem = ActorSystem()
  val scheduler = actorSystem.scheduler
  val scheduledTasks = MMap[Long, Cancellable]()

  val OneSec = 1000L // all in millis
  val OneMin = OneSec*60
  val FiveMins = OneMin*5
  val TenMins = FiveMins*2
  val FifteenMins = FiveMins*3
  val ThirtyMins = FifteenMins*2
  val OneHour = ThirtyMins*2
  val OneDay = OneHour*24
  val OneMonth = OneDay*30
  val OneYear = OneDay*365
  val OneWeek = OneDay*7

  implicit val executor = actorSystem.dispatcher
  import scala.collection.mutable.{Map => MMap}
  
  def doHourly(f: => Unit) = doRegularly(f, 1000L*60*60)  // every hour
  def doEvery30Mins(f: => Unit) = doRegularly(f, 1000L*60*30)  // every 1/2 hr
  def doEvery15Mins(f: => Unit) = doRegularly(f, 1000L*60*15)  // every 15 mins
  def doEvery10Mins(f: => Unit) = doRegularly(f, 1000L*60*10)  // every 10 mins
  def doEvery5Mins(f: => Unit) = doRegularly(f, 1000L*60*5)  // every 5 mins
  def doEveryMin(f: => Unit) = doRegularly(f, 1000L*60)  // every 1 min

  def doOnceLater(fn: => Unit, periodMillis:Long) = {
    val id = ctr.getAndIncrement
    val cancellable = scheduler.scheduleOnce(periodMillis milliseconds){
      scheduledTasks.synchronized{scheduledTasks -= id}
      fn
    }
    scheduledTasks += id -> cancellable
    cancellable
  }
  def doRegularly(fn: => Unit, periodMillis:Long) = {
    val cancellable = scheduler.schedule(0 seconds, periodMillis milliseconds)(fn)
    scheduledTasks += ctr.getAndIncrement -> cancellable
    cancellable
  }
  
  def doWhile(fn: => Unit, whileFn: => Boolean, periodMillis:Long) {
    if (whileFn) {
      fn
      doOnceLater(doWhile(fn, whileFn, periodMillis), periodMillis)
    }
  } 
  
  
  sys.addShutdownHook{
    scheduledTasks.foreach{
      case (id, task) => if (!task.isCancelled) try task.cancel catch {
        case a:Throwable => 
          println(s"[ShutdownHook] Error cancelling task with id: $id. StackTrace below")
          a.printStackTrace
      }
    } 
  }
}
