package dbObject

trait Field {
  def relevantTables: Seq[Field]
}

case class Column(table: Table, columnName: String) extends Field with Entity {
  override def name: SqlObject = sqlObject(columnName)
  override def toString: String = this match {
    case Column(table, column) => s"${table.name}.$column"
    case t: Transformation => t.transform
  }

  override def relevantTables: Seq[Field] = Seq(this)
}

trait Transformation extends Field {
  def transform: String
}

case class Coalesce(fields: Seq[Field]) extends Transformation {
  override def relevantTables: Seq[Field] = fields
  override def transform: String = s"COALESCE(${fields.mkString(", ")})"
}

case class Upper(field: Field) extends Transformation {
  override def relevantTables: Seq[Field] = Seq(field)
  override def transform: String = s"UPPER($field)"
}

case class Lower(field: Field) extends Transformation {
  override def relevantTables: Seq[Field] = Seq(field)
  override def transform: String = s"LOWER($field)"
}

case class Custom(customSql: String, includedFields: Option[Seq[Field]] = None) extends Transformation {
  override def relevantTables: Seq[Field] = includedFields match {
    case None => Seq.empty
    case Some(fields) => fields
  }
  override def transform: String = customSql
}
