package query

import dbObject.{Column, Database, DbTable, Field, SqlObject, Table, Transformation}
import queryObject.{Expressions, From, Join, Select, Where}

case class Query(columns: Seq[Field],
            table: Table,
            joins: Seq[Join],
            where: Where) extends Table { // should be optional

  override def isEmpty: Boolean = this.write == Query.builder.build.write

//  private def unpackTable(table: Table) = { // can't implement yet
//    table match {
//      case dbTable: DbTable => dbTable
//      case query: Query => s"(${query.write})"
//    }
//  }

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

case class QueryBuilder(private val wip: Query = Query(Seq.empty, null, Seq.empty, null)) { // table needs to be optional
  def withSelectColumn(newColumn: Field): QueryBuilder = {
//    this.copy(columns ++ Seq(newColumn))
    this.copy(wip = this.wip.copy(columns = this.wip.columns ++ Seq(newColumn)))
  }

  def build: Query = {
    require(wip.columns.nonEmpty)
    require(wip.table != null)
    // check columns are from tables in from or joins
    wip
  }
} // communicating to devs more clearly the reason for the wip, it's not

object testObj {
  val q: QueryBuilder = QueryBuilder(Query(Seq.empty, null, Seq.empty, null))
}

object Query {
  def builder: QueryBuilder = QueryBuilder.apply // why do I need to call apply here?
}

case class QueryBuilder2(columns: Seq[Field],
                        maybeTable: Option[DbTable],
                        joins: Seq[Join],
                        where: Where) { // todo change overall where to seq of filters

  val table = maybeTable match {
    case None => DbTable("", "", "") // antipattern - data model has to support not populating it
    case Some(table: DbTable) => table
    case _ => throw new Exception("UH OH")
  }

  def withSelectColumn(newColumn: Field): QueryBuilder2 = this.copy(columns ++ Seq(newColumn))

  def withSelectColumns(newColumns: Seq[Field]): QueryBuilder2 = this.copy(columns ++ newColumns)

  def withFrom(table: DbTable): QueryBuilder2 = this.copy(maybeTable = Some(table))

  def withJoin(newJoin: Join): QueryBuilder2 = this.copy(joins = joins ++ Seq(newJoin))

  def withJoins(newJoins: Seq[Join]): QueryBuilder2 = this.copy(joins = joins ++ newJoins)

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
