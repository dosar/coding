package arrayandstrings

/*
* errors:
* 1) copies of same value on update
* 2) remove fail if removing non existent key
* 3) didn't ensure capacity on remove
* */
class Hashtable[Key, Value] {

  def length: Int = size

  def update(key: Key, value: Value): Unit = {
    assert(key != null)
    ensureCapacity()
    val (sizeInc, slotInc) = internalUpdate(array, key, value)
    size += sizeInc
    occupiedSlots += slotInc
  }

  def remove(key: Key): Unit = {
    assert(key != null)
    var holder = array(key.hashCode() % array.length)
    if(holder == null) return
    if(holder.key == key) {
      array(key.hashCode() % array.length) = holder.next
      if(holder.next == null) occupiedSlots -= 1
    }
    else {
      while(holder.next != null) {
        val next = holder.next
        if(next.key == key) {
          holder.next = next.next
          return
        }
        holder = next
      }
    }
    size -= 1
    ensureCapacity()
  }

  def add(key: Key, value: Value): Unit = this(key) = value

  def apply(key: Key): Value = {
    assert(key != null)
    var holder = array(key.hashCode() % array.length)
    while(holder != null) {
      if(holder.key == key) return holder.value
      holder = holder.next
    }
    throw new KeyNotFoundException(key.toString)
  }

  private def ensureCapacity() = {
    if(occupiedSlots == array.length)
      array = redistribute(array.length * 2)
    else if (occupiedSlots >= 5 && occupiedSlots <= array.length / 4)
      array = redistribute(array.length / 2)
  }

  private def redistribute(newLength: Int): KeyValueArray = {
    val newArray = new KeyValueArray(newLength)
    occupiedSlots = 0
    size = 0

    for(keyValueHolder <- array) {
      var holder = keyValueHolder
      while(holder != null) {
        val (sizeInc, slotInc) = internalUpdate(newArray, holder.key, holder.value)
        size += sizeInc
        occupiedSlots += slotInc
        holder = holder.next
      }
    }
    newArray
  }

  private def internalUpdate(array: KeyValueArray, k: Key, v: Value): (SizeIncrement, SlotIncrement) = {
    val index = k.hashCode() % array.length
    val noCollision = array(index) == null
    if(noCollision) {
      array(index) = KeyValueHolder(k, v)
      (1.toByte, 1.toByte)
    }
    else {
      var holder = array(index)
      var parent: KeyValueHolder[Key, Value] = null
      do {
        if(holder.key == k) {
          holder.value = v
          return (0.toByte, 0.toByte)
        }
        parent = holder
        holder = holder.next
      } while (holder != null)
      parent.next = KeyValueHolder(k, v)
      (1.toByte, 0.toByte)
    }
  }

  type SizeIncrement = Byte; type SlotIncrement = Byte
  type KeyValueArray = Array[KeyValueHolder[Key, Value]]
  private[arrayandstrings] var array: KeyValueArray = new KeyValueArray(10)
  private[arrayandstrings] var occupiedSlots = 0
  private var size = 0
}

object KeyValueHolder {
  def apply[Key, Value](key: Key, value: Value): KeyValueHolder[Key, Value] =
    KeyValueHolder(key, value, null)
}

case class KeyValueHolder[Key, Value](key: Key, var value: Value, var next: KeyValueHolder[Key, Value])

class KeyNotFoundException(msg: String) extends Exception