package rapture.json


sealed abstract class DataGetException(msg: String) extends RuntimeException(msg)

case class TypeMismatchException(found: DataTypes.DataType, expected: DataTypes.DataType)
  extends DataGetException(s"type mismatch: Expected ${expected.name} but found ${found.name}")

case class MissingValueException(name: String = "")
  extends DataGetException(
    if (name.isEmpty)
      "missing value"
    else s"missing value: $name")

