package query

import dbObject.{Column, Database, DbTable, EmptyTable, Field, SqlObject, Table, Transformation}
import queryObject.{Expression, Expressions, From, Join, Select, Where}

case class Query(columns: Seq[Field],
                 table: Option[Table],
                 joins: Seq[Join],
                 where: Seq[Expression],
                 groupBy: Seq[Field],
                 orderBy: Seq[Field]) extends Table {

  override def isEmpty: Boolean = this.write == Query.builder.build.write

  private val unpackTable = table match {
    case Some(t: Table) => t
    case None => EmptyTable // different pattern here based on how I do the from?
  }

  //  private def isValid = unpackTable == EmptyTable && columns.nonEmpty

  private val select = Select(columns)
  private val from = From(unpackTable)
  private val whereClause = Where(where)
  private val groupByClause = s"GROUP BY \n\t${groupBy.mkString(",\n\t")}"
  private val orderByClause = s"ORDER BY \n\t${orderBy.mkString(",\n\t")}"

  def write: String =
    s"""
       |$select
       |$from
       |${joins.mkString("\n")}
       |$whereClause
       |$groupBy
       |$orderByClause
       |""".stripMargin

  override def toString: String = this.write

  override def name: SqlObject = sqlObject(this.toString)
}

object Query {
  def builder: QueryBuilder = QueryBuilder() // why do I need to call apply here?
}

case class QueryBuilder(private val wip: Query = Query(Seq.empty, None, Seq.empty, Seq.empty, Seq.empty, Seq.empty)) { // table needs to be optional
  def withSelectColumn(newColumn: Field): QueryBuilder = {
    this.copy(wip = this.wip.copy(columns = this.wip.columns ++ Seq(newColumn)))
  }

  def withSelectColumns(newColumns: Seq[Field]): QueryBuilder = {
    this.copy(wip = this.wip.copy(columns = this.wip.columns ++ newColumns))
  }

  // this method will return a new Querybuilder with a REPLACED table
  def withFrom(table: DbTable): QueryBuilder = {
    this.copy(wip = this.wip.copy(table = Some(table)))
  }

  def withJoin(newJoin: Join): QueryBuilder = {
    this.copy(wip = this.wip.copy(joins = this.wip.joins ++ Seq(newJoin)))
  }

  def withJoins(newJoins: Seq[Join]): QueryBuilder = {
    this.copy(wip = this.wip.copy(joins = this.wip.joins ++ newJoins))
  }

  def withCondition(newCondition: Expression): QueryBuilder = {
    this.copy(wip = this.wip.copy(where = this.wip.where ++ Seq(newCondition)))
  }

  def withGroupByColumns(columns: Seq[Field]): QueryBuilder = {
    this.copy(wip = this.wip.copy(groupBy = columns))
  }

  def withOrderByCol(newColumn: Field): QueryBuilder = {
    this.copy(wip = this.wip.copy(orderBy = this.wip.orderBy ++ Seq(newColumn)))
  }

  def withOrderByCols(newColumns: Seq[Field]): QueryBuilder = {
    this.copy(wip = this.wip.copy(orderBy = this.wip.orderBy ++ newColumns))
  }

  def build: Query = {
    require(wip.columns.nonEmpty, s"Query requires at least 1 column in select")
    require(wip.table != None, s"Query Type requires a valid table in from clause")
    // check columns are from tables in from or joins
    wip
  }
} // communicating to devs more clearly the reason for the wip, it's not
