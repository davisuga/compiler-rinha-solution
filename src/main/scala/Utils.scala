package graalinterpreter.utils

import scala.collection.mutable
import scala.collection.mutable.HashMap

def memoize[I, O](f: I => O): I => O = new HashMap[I, O]():
  override def apply(key: I) = getOrElseUpdate(key, f(key))

class LRUCache[K, V](capacity: Int):
  val cache: mutable.LinkedHashMap[K, V] = mutable.LinkedHashMap.empty[K, V]

  def get(key: K): Option[V] =
    val value = cache.remove(key)
    value.foreach(v => cache.put(key, v))
    value

  def put(key: K, value: V): Unit =
    if (cache.size >= capacity)
      cache.remove(cache.head._1)
    cache.put(key, value)
