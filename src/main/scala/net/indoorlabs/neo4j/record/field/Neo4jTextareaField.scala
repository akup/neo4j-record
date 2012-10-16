package net.indoorlabs.neo4j
package record
package field

import net.liftweb.record.field.TextareaTypedField

class Neo4jTextareaField[OwnerType <: Neo4jRecord[OwnerType]](rec: OwnerType, maxLength: Int)
  extends Neo4jStringField(rec, maxLength) with TextareaTypedField

class Neo4jOptionalTextareaField[OwnerType <: Neo4jRecord[OwnerType]](rec: OwnerType, maxLength: Int)
  extends Neo4jOptionalStringField(rec, maxLength) with TextareaTypedField