import dbObject.{Coalesce, Column, Database, Schema, SqlObject, DbTable}
import query.{Query, QueryBuilder}
import queryObject.{From, InnerJoin, Select}

object Runner extends App {

  val database = Database("deiddev")
  val schema = Schema(database, "profile_store")
  val patientTable = DbTable(schema, "patient")
  val encounterTable = DbTable(schema, "encounter")

  val patientId = Column(patientTable, "patient_id")
  val encounterId = Column(encounterTable, "encounter_id")
  val encounterPatientId = Column(encounterTable, "patient_id")

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

//  val sql = QueryBuilder(patientId, patientTable)
//    .withSelectColumn(encounterPatientId)
//    .withJoin(InnerJoin(patientId, encounterTable, encounterPatientId))
//    .build

  val query = Query.builder
    .withSelectColumn(patientId)
    .withFrom(patientTable)
    .build

  println(query.write)
//
//  val t = Seq(
//    Seq("a"),
//    Seq("b")
//  )
//
//  val z = t.flatMap(i => i)
//  println(z)



}
