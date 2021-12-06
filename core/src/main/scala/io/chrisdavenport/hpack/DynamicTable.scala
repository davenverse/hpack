package io.chrisdavenport.hpack

import HeaderField.{overhead => HEADER_ENTRY_OVERHEAD}

class DynamicTable(
  var headerFields: Array[HeaderField],
  private var head: Int,
  private var tail: Int,
  private var isize: Int,
  private var icapacity: Int = -1,
){

  def size: Int = isize

  def capacity: Int = icapacity

  /**
   * Return the number of header fields in the dynamic table.
   */
  def length = {
    if (head < tail) {
      headerFields.length - tail + head
    } else {
      head - tail
    }
  }

    /**
   * Return the header field at the given index.
   * The first and newest entry is always at index 1,
   * and the oldest entry is at the index length().
   */
  def getEntry(index: Int): HeaderField = {
    if (index <= 0 || index > length) {
      throw new IndexOutOfBoundsException();
    } else {
      val i = head - index
      if (i < 0) {
        headerFields(i + headerFields.length)
      } else {
        headerFields(i)
      }
    }
  }

    /**
   * Remove all entries from the dynamic table.
   */
  def clear() = {
    while (tail != head) {
      tail = tail + 1
      headerFields(tail) = null
      if (tail == headerFields.length) {
        tail = 0
      }
    }
    head = 0
    tail = 0
    isize = 0
  }

    /**
   * Remove and return the oldest header field from the dynamic table.
   */
  def remove(): HeaderField = {
    val removed = headerFields(tail)
    if (removed == null) {
      null
    } else {
      isize = isize - removed.size
      tail = tail + 1
      headerFields(tail) = null
      if (tail == headerFields.length) {
        tail = 0;
      }
      return removed;
    }
  }

  /**
   * Add the header field to the dynamic table.
   * Entries are evicted from the dynamic table until the size of the table
   * and the new header field is less than or equal to the table's capacity.
   * If the size of the new entry is larger than the table's capacity,
   * the dynamic table will be cleared.
   */
  def add(header: HeaderField): Unit = {
    val headerSize = header.size
    if (headerSize > icapacity) {
      clear()
    } else {
      while (isize + headerSize > icapacity) {
        remove();
      }
      head = head + 1
      headerFields(head) = header
      isize += header.size
      if (head == headerFields.length) {
        head = 0
      }
    }
  }

    /**
   * Set the maximum size of the dynamic table.
   * Entries are evicted from the dynamic table until the size of the table
   * is less than or equal to the maximum size.
   */
  def setCapacity(capacity: Int): Unit = {
    if (capacity < 0) {
      throw new IllegalArgumentException("Illegal Capacity: "+ capacity);
    }

    // initially capacity will be -1 so init won't return here
    if (this.icapacity == capacity) {
      ()
    } else {
      this.icapacity = capacity

      if (capacity == 0) {
        clear()
      } else {
        // initially size will be 0 so remove won't be called
        while (isize > capacity) {
          remove()
        }
      }

      var maxEntries = capacity / HEADER_ENTRY_OVERHEAD
      if (capacity % HEADER_ENTRY_OVERHEAD != 0) {
        maxEntries = maxEntries + 1
      }

      // check if capacity change requires us to reallocate the array
      if (headerFields != null && headerFields.length == maxEntries) {
        return;
      }

      val tmp = new Array[HeaderField](maxEntries)

      // initially length will be 0 so there will be no copy
      val len = length
      var cursor = tail
      (0 until len).foreach{ i =>
        cursor = cursor + 1
        val entry = headerFields(cursor)
        tmp(i) = entry
        if (cursor == headerFields.length) {
          cursor = 0
        }
      }

      this.tail = 0;
      this.head = tail + len;
      this.headerFields = tmp;
    }
  }

}

object DynamicTable {
  def apply(capacity: Int): DynamicTable = {
    val t = new DynamicTable(null, null.asInstanceOf[Int], null.asInstanceOf[Int], null.asInstanceOf[Int])
    t.setCapacity(capacity)
    t
  }
}

object Immutable {

  import cats.syntax.all._
  import cats.data.Chain

  abstract case class DynamicTable private (
    chain: Chain[HeaderField],
    capacity: Int,
    size: Int
  ){
    def length = chain.length
    // lazy val size = chain.foldMap(_.size)
    def getEntry(idx: Int) = chain.get(idx.toLong)

    /**
     * Remove and return the oldest header field from the dynamic table.
     */
    def remove: (DynamicTable, Option[HeaderField]) = 
      chain.initLast.fold((this, Option.empty[HeaderField])){ case (rest, last) => 
        (new DynamicTable(rest, capacity, size - last.size){}, last.some)
      }


    def empty: DynamicTable = new DynamicTable(Chain.empty, capacity, 0){}

    def add(headerField: HeaderField): DynamicTable = {
      val headerSize = headerField.size
      if (headerSize > capacity){
        empty
      } else {
        def recurse(table: DynamicTable): DynamicTable = {
          if (table.size + headerSize > capacity) recurse(table.remove._1)
          else if (table.size <= 0) empty
          else table
        }
        val out = recurse(this)
        new DynamicTable(out.chain.prepend(headerField), out.capacity, out.size + headerSize){}
      }
    }

    def setCapacity(capacity: Int): DynamicTable = {
      require(capacity >= 0, "Capacity must by zero or more")
      if (this.capacity == capacity) this
      else if (capacity == 0) empty
      else {
        def recurse(table: DynamicTable): DynamicTable = {
          if (table.size > capacity) recurse(table.remove._1)
          else if (table.size <= 0) empty
          else table
        }
        val out = recurse(this)
        new DynamicTable(out.chain, capacity, out.size){}
      }
    }
  }

  object DynamicTable {
    // def empty = 
  }
}