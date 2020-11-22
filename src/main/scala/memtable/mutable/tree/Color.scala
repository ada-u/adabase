package memtable.mutable.tree

sealed trait Color

case object Red extends Color {
  override def toString: String = "red"
}
case object Black extends Color {
  override def toString: String = "black"

}
