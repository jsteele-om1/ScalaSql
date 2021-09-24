package dbObject

trait Entity {
  def sqlObject(entity: String): SqlObject = SqlObject(entity)

  def name: SqlObject // could condense this to one method
}

case class SqlObject(objectName: String) {
  override def toString: String = objectName.toLowerCase
}

case class Database(databaseName: String) extends Entity {
  override def name: SqlObject = sqlObject(databaseName)

  override def toString: String = name.toString
}

case class Schema(database: Database, schemaName: String) extends Entity {
  override def name: SqlObject = sqlObject(schemaName)

  override def toString: String = s"${database.toString}.$name"
}

trait Table extends Entity {
  def write: String // todo can drop this

  def isEmpty: Boolean // todo can probably drop this
}

object EmptyEntity {
  val emptyDatabase: Database = Database("")
  val emptySchema: Schema = Schema(emptyDatabase, "")
}

case object EmptyTable extends Table {
  def write = ""

  override def name: SqlObject = SqlObject("Empty") // probably want to rethink this inheretence
  override def isEmpty: Boolean = true
}

case class DbTable(schema: Schema, tableName: String) extends Table {
  override def name: SqlObject = sqlObject(tableName)

  override def toString: String = s"${schema.toString}.$name"

  override def write: String = this.toString

  def isEmpty: Boolean = this == DbTable(EmptyEntity.emptySchema, "")
}