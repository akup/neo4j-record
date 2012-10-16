package net.indoorlabs.neo4j
package record
package field

import net.liftweb.record.field._

class Neo4jCountryField[OwnerType <: Neo4jRecord[OwnerType]](rec: OwnerType)
  extends Neo4jEnumField(rec, Countries) {

}

class Neo4jOptionalCountryField[OwnerType <: Neo4jRecord[OwnerType]](rec: OwnerType)
  extends Neo4jOptionalEnumField(rec, Countries) {

}