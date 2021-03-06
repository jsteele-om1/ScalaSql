import dbObject.{Coalesce, Column, Database, DbTable, Max, RowNumber, Schema, SqlObject, Window}
import query.{Query, QueryBuilder}
import queryObject.{Expressions, From, InnerJoin, Select}

object Runner extends App {

  val database = Database("deiddev")
  val schema = Schema(database, "profile_store")
  val patientTable = DbTable(schema, "patient")
  val encounterTable = DbTable(schema, "encounter")

  val patientId = Column(patientTable, "patient_id")
  val encounterId = Column(encounterTable, "encounter_id")
  val maxEncounterId = Max(encounterId)
  val encounterPatientId = Column(encounterTable, "patient_id")

  val coalescePatientId = Coalesce(Seq(patientId, encounterPatientId))
  val selectColumns = Seq(coalescePatientId, encounterId)

  val partition = Window(selectColumns, Seq(patientId), RowNumber) //

  val query: Query = Query.builder
    .withSelectColumns(Seq(patientId, maxEncounterId))
    .withFrom(patientTable)
    .withCondition(Expressions.isIn(patientId, "VARCHAR", Seq("1", "4", "98")))
    .withOrderByCol(patientId)
    .withGroupByColumns(Seq(patientId))
    .build

  println(query.write)

//  val brokenQuery = Query.builder
//    .withFrom(patientTable)
//    .withSelectColumn(patientId)
//    .build
//
//  println(brokenQuery)
}
