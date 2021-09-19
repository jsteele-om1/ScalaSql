package query

import dbObject.{Column, Database, Field, Table, Transformation}
import queryObject.{Expressions, From, Join, Select, Where}

class Query(columns: Seq[Field],
            table: Table,
            joins: Seq[Join],
            where: Where)

object Query {
  def builder: QueryBuilder = QueryBuilder
}

case class QueryBuilder(columns: Seq[Field],
                        table: Table,
                        joins: Seq[Join],
                        where: Where) { // todo change overall where to seq of filters

//  val select: Select = columns match {
//    case Some(columns) => Select(columns)
//    case None => Select(Seq.empty)
//  }
//
//  val from: From = table match {
//    case Some(table) => From(table)
//    case None => Table.empty
//  }

  val select = Select(columns)
  val from = From(table)

  override def toString: String =
    s"""
       |$select
       |$from
       |${joins.mkString("\n")}
       |$where
       |""".stripMargin

//  def isValidQuery: Boolean = {
//    val tables = columns.map {
//      case column: Column => Seq(column)
//      case t: Transformation => t.relevantTables
//    }.flatten
//      .map(_.table)

//    val a = tables
//
//    true
//  }

  def withSelectColumn(newColumn: Field): QueryBuilder = this.copy(columns ++ Seq(newColumn))

  def withSelectColumns(newColumns: Seq[Field]): QueryBuilder = this.copy(columns ++ newColumns)

  def withJoin(newJoin: Join): QueryBuilder = this.copy(joins = joins ++ Seq(newJoin))

  def withJoins(newJoins: Seq[Join]): QueryBuilder = this.copy(joins = joins ++ newJoins)

  def build: QueryBuilder = new QueryBuilder(columns, table, joins, where)

}

object QueryBuilder {

  def apply: QueryBuilder = {
    QueryBuilder(
      columns = Seq.empty,
      table = From,
      joins = Seq.empty
      where = Where(Expressions.empty)
    )
  }

  def apply(column: Field, table: Table): QueryBuilder =
    QueryBuilder(
      columns = Seq(column),
      table = table,
      joins = Seq.empty,
      where = Where(Expressions.empty)
    )

  def apply(columns: Seq[Field], table: Table): QueryBuilder =
    QueryBuilder(
      columns = columns,
      table = table,
      joins = Seq.empty,
      where = Where(Expressions.empty)
    )

  def apply(column: Field, table: Table, join: Join): QueryBuilder =
    QueryBuilder(
      columns = Seq(column),
      table = table,
      joins = Seq(join),
      where = Where(Expressions.empty)
    )

  def apply(columns: Seq[Field], table: Table, join: Join): QueryBuilder =
    QueryBuilder(
      columns = columns,
      table = table,
      joins = Seq(join),
      where = Where(Expressions.empty)
    )

  def apply(column: Field, table: Table, joins: Seq[Join]): QueryBuilder =
    QueryBuilder(
      columns = Seq(column),
      table = table,
      joins = joins,
      where = Where(Expressions.empty)
    )

  def apply(columns: Seq[Field], table: Table, joins: Seq[Join]): QueryBuilder =
    QueryBuilder(
      columns = columns,
      table = table,
      joins = joins,
      where = Where(Expressions.empty)
    )

}
