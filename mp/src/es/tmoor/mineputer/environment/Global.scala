package es.tmoor.mineputer.environment

import scala.util.Random
import Environment._

private[environment] object Global {
  class MessageBox {
    private[Global] var value: RTVal = null
    var ready = false
    def getValue = value
    def reply(rtVal: RTVal): Unit = {
      value = rtVal
      ready = true
    }
  }
  private val names = collection.mutable.Map[String, Environment]()
  private def namesInversed = names.map {
    case x->y => y->x
  }
  def getName(env: Environment) = namesInversed(env)
  def register(env: Environment, as: String = Random.alphanumeric.take(32).mkString): String = {
    names += as -> env
    env.init()
    as
  }
  def deregister(name: String, as: Environment): Unit = {
    if (names.get(name) == Some(as)) names.remove(name)
  }
  def sendWithResponse(msg: RTVal, name: String): RTVal = {
    val ct = Thread.currentThread()
    val msgBox = new RTMessageBox()
    val newRTVal = RTObj(msg::msgBox::Nil)
    names.get(name).foreach(e => {
      val t = new Thread(() => {
        e.handle(newRTVal)
      })
      t.start()
    })
    while (!msgBox.ready) Thread.sleep(0, 50)
    msgBox.value
  }
  def send(msg: RTVal, name: String): Unit = {
    names.get(name).foreach(e => {
      val t = new Thread(() => e.handle(msg))
      t.start()
    })
  }
}