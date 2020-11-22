package memtable.mutable

class MemTable[K, V] {

  def get: Option[V] = ???

  def insert(key: K, value: V): this.type = ???

}

object MemTable {

  def empty[K, V]: MemTable[K, V] = ???
}
