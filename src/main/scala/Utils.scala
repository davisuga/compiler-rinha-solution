package graalinterpreter.utils

import scala.collection.mutable.*

def memoize[I, O](f: I => O): I => O = new HashMap[I, O]():
  override def apply(key: I) = getOrElseUpdate(key, f(key))

class LRUCache[K, V](capacity: Int):
  val cache: LinkedHashMap[K, V] = LinkedHashMap.empty[K, V]
  def get(key: K) =
    val value = cache.remove(key)
    value.foreach(cache.put(key, _))
    value
  def put(key: K, value: V) =
    if (cache.size >= capacity)
      cache.remove(cache.head._1)
    cache.put(key, value)
