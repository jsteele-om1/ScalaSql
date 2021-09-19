package query

import dbObject.{Column, Database, DbTable, Field, SqlObject, Table, Transformation}
import queryObject.{Expressions, From, Join, Select, Where}

class Query(columns: Seq[Field],
            table: Table,
            joins: Seq[Join],
            where: Where) extends Table {

  override def isEmpty: Boolean = this.write == Query.builder.build.write

  private def unpackTable(table: Table) = {
    table match {
      case dbTable: DbTable => dbTable
      case query: Query => s"(${query.write})"
    }
  }

  private def isValid = table.isEmpty && columns.nonEmpty

  private val validColumns = if (isValid) columns else Seq.empty
  private val validTable = if (isValid) table else DbTable("", "", "") // there's a better way to do this since it's part of the check

  private val select = Select(validColumns)
  private val from = From(Some(validTable))

  def write: String =
    s"""
       |$select
       |$from
       |${joins.mkString("\n")}
       |$where
       |""".stripMargin

  override def toString: String = this.write
  override def name: SqlObject = sqlObject(this.toString)
}

object Query {
  def builder: QueryBuilder = QueryBuilder.apply
}

case class QueryBuilder(columns: Seq[Field],
                        maybeTable: Option[DbTable],
                        joins: Seq[Join],
                        where: Where) { // todo change overall where to seq of filters

  val table = maybeTable match {
    case None => DbTable("", "", "")
    case Some(table: DbTable) => table
    case _ => throw new Exception("UH OH")
  }

  def withSelectColumn(newColumn: Field): QueryBuilder = this.copy(columns ++ Seq(newColumn))

  def withSelectColumns(newColumns: Seq[Field]): QueryBuilder = this.copy(columns ++ newColumns)

  def withFrom(table: DbTable): QueryBuilder = this.copy(maybeTable = Some(table))

  def withJoin(newJoin: Join): QueryBuilder = this.copy(joins = joins ++ Seq(newJoin))

  def withJoins(newJoins: Seq[Join]): QueryBuilder = this.copy(joins = joins ++ newJoins)

  def build: Query = new Query(columns, table, joins, where)

}

object QueryBuilder {

  def apply: QueryBuilder = {
    QueryBuilder(
      columns = Seq.empty,
      maybeTable = None,
      joins = Seq.empty,
      where = Where(Expressions.empty)
    )
  }

  def apply(column: Field, table: DbTable): QueryBuilder =
    QueryBuilder(
      columns = Seq(column),
      maybeTable = Some(table),
      joins = Seq.empty,
      where = Where(Expressions.empty)
    )

  def apply(columns: Seq[Field], table: DbTable): QueryBuilder =
    QueryBuilder(
      columns = columns,
      maybeTable = Some(table),
      joins = Seq.empty,
      where = Where(Expressions.empty)
    )

  def apply(column: Field, table: DbTable, join: Join): QueryBuilder =
    QueryBuilder(
      columns = Seq(column),
      maybeTable = Some(table),
      joins = Seq(join),
      where = Where(Expressions.empty)
    )

  def apply(columns: Seq[Field], table: DbTable, join: Join): QueryBuilder =
    QueryBuilder(
      columns = columns,
      maybeTable = Some(table),
      joins = Seq(join),
      where = Where(Expressions.empty)
    )

  def apply(column: Field, table: DbTable, joins: Seq[Join]): QueryBuilder =
    QueryBuilder(
      columns = Seq(column),
      maybeTable = Some(table),
      joins = joins,
      where = Where(Expressions.empty)
    )

  def apply(columns: Seq[Field], table: DbTable, joins: Seq[Join]): QueryBuilder =
    QueryBuilder(
      columns = columns,
      maybeTable = Some(table),
      joins = joins,
      where = Where(Expressions.empty)
    )

}
