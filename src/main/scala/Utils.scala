package graalinterpreter.utils

import scala.collection.mutable.HashMap

def memoize[I, O](f: I => O): I => O = new HashMap[I, O]():
  override def apply(key: I) = getOrElseUpdate(key, f(key))
