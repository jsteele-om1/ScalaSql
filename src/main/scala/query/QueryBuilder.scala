package query

import dbObject.{Column, Database, DbTable, EmptyTable, Field, SqlObject, Table, Transformation}
import queryObject.{Expressions, From, Join, Select, Where}

case class Query(columns: Seq[Field],
            table: Option[Table],
            joins: Seq[Join],
            where: Option[Where]) extends Table { // should be optional

  override def isEmpty: Boolean = this.write == Query.builder.build.write

//  private def unpackTable(table: Table) = { // can't implement yet
//    table match {
//      case dbTable: DbTable => dbTable
//      case query: Query => s"(${query.write})"
//    }
//  }

  val unpackTable = table match {
    case Some(t: Table) => t
    case None => EmptyTable // different pattern here based on how I do the from?
  }

//  private def isValid = unpackTable == EmptyTable && columns.nonEmpty

  private val select = Select(columns)
  private val from = From(unpackTable)

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
  def builder: QueryBuilder = QueryBuilder() // why do I need to call apply here?
}

case class QueryBuilder(private val wip: Query = Query(Seq.empty, null, Seq.empty, null)) { // table needs to be optional
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

  def build: Query = {
    require(wip.columns.nonEmpty)
    require(wip.table != null)
    // check columns are from tables in from or joins
    wip
  }
} // communicating to devs more clearly the reason for the wip, it's not
