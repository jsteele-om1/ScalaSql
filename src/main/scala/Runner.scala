import dbObject.{Coalesce, Column, Database, Schema, SqlObject, Table}
import query.QueryBuilder
import queryObject.{From, InnerJoin, Select}

object Runner extends App {

  val database = Database(SqlObject("deiddev"))
  val schema = Schema(database, SqlObject("profile_store"))
  val patientTable = Table(schema, SqlObject("patient"))
  val encounterTable = Table(schema, SqlObject("encounter"))

  val patientId = Column(patientTable, SqlObject("patient_id"))
  val encounterId = Column(encounterTable, SqlObject("encounter_id"))
  val encounterPatientId = Column(encounterTable, SqlObject("patient_id"))

  val coalescePatientId = Coalesce(Seq(patientId, encounterPatientId))
  val selectColumns = Seq(coalescePatientId, encounterId)

//  val select = Select(selectColumns)
//  val from = From(patientTable)
//  val join = InnerJoin(patientId, encounterTable, patientId)
//
//  val sql =
//    s"""
//      |$select
//      |$from
//      |$join
//      |""".stripMargin

  val sql = QueryBuilder(patientId, patientTable)
    .withSelectColumn(encounterPatientId)
    .withJoin(InnerJoin(patientId, encounterTable, encounterPatientId))
    .build

  println(sql)

  val t = Seq(
    Seq("a"),
    Seq("b")
  )

  val z = t.flatMap(i => i)
  println(z)



}
