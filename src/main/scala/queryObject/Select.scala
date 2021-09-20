package queryObject

import dbObject.Field

case class Select(columns: Seq[Field]) {
  private val columnsString = columns.map(_.toString).mkString(",\n\t")

  override def toString: String = s"SELECT\n\t$columnsString"
}
