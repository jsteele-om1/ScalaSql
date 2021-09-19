package dbObject

trait Entity {
  def sqlObject(entity: String): SqlObject = SqlObject(entity)

  def name: SqlObject
}

case class SqlObject(objectName: String) {
  override def toString: String = objectName.toLowerCase
}

case class Database(databaseName: String) extends Entity {
  override def name: SqlObject = sqlObject(databaseName)

  override def toString: String = name.toString
}

object Database {
  def apply: Database = Database("")

  def apply(databaseName: String): Database = Database(databaseName)
}

case class Schema(database: Database, schemaName: String) extends Entity {
  override def name: SqlObject = sqlObject(schemaName)

  override def toString: String = s"${database.toString}.$name"
}

object Schema {
  def apply: Schema = Schema("", "") // why is this bad??

  //  def apply(schemaName: String): Schema = Schema(Database, schemaName) // why is this bad??
  def apply(databaseName: String, schemaName: String): Schema = Schema(Database(databaseName), schemaName)

  def apply(database: Database, schemaName: String): Schema = Schema(database, schemaName)
}

case class Table(schema: Schema, tableName: String) extends Entity {
  override def name: SqlObject = sqlObject(tableName)

  override def toString: String = s"${schema.toString}.$name"
}

object Table {
  //  def apply(tableName: String): Table = new Table(Schema, tableName)
  //  def apply(schemaName: String, tableName: String): Table = Table(Schema(Database, schemaName), tableName)
  def apply(databaseName: String, schemaName: String, tableName: String): Table = {
    val database = Database(databaseName)
    val schema = Schema(database, schemaName)
    Table(schema, tableName)
  }
}