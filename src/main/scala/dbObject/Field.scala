package dbObject

trait Field {
  def relevantTables: Seq[Field]

  override def toString: String = this match {
    case Column(table, column) => s"${table.name}.$column"
    case t: Transformation => t.transform
  }
}

case class Column(table: DbTable, columnName: String) extends Field with Entity {
  override def name: SqlObject = sqlObject(columnName)

  override def relevantTables: Seq[Field] = Seq(this)
}

trait Transformation extends Field { // should transformations be a function or does that lose safety?
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

trait AggregateTransformation extends Transformation

case class Max(field: Field) extends  AggregateTransformation {
  override def relevantTables: Seq[Field] = Seq.empty

  override def transform: String = s"MAX($field)"
}

trait RankFunction {
  def functionName: String
}

case object RowNumber extends RankFunction {
  override def functionName: String = "ROW_NUMBER"
}

case object Rank extends RankFunction {
  override def functionName: String = "RANK"
}

case class Window(partitionByCols: Seq[Field], orderByCols: Seq[Field], rankFunction: RankFunction) extends Transformation {
  override def transform: String = {
    s"""
       |${rankFunction.functionName} OVER (
       |  PARTITION BY ${partitionByCols.mkString(", ")}
       |  ORDER BY ${orderByCols.mkString(", ")}
       |)  AS rn
       |""".stripMargin
  }

  override def relevantTables: Seq[Field] = Seq.empty
}

case class Custom(customSql: String, includedFields: Option[Seq[Field]] = None) extends Transformation {
  override def relevantTables: Seq[Field] = includedFields match {
    case None => Seq.empty
    case Some(fields) => fields
  }

  override def transform: String = customSql
}
