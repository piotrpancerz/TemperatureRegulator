package server

@SerialVersionUID(15L)
class Res(val _previousValue: Double, val _currentValue: Double, val _desiredValue: Double) extends Serializable {
  def previousValue = _previousValue
  def currentValue =  _currentValue
  def change = math.rint(math.abs(_previousValue - _currentValue)*100)/100
  def desiredValue = _desiredValue
  def dist = math.rint(math.abs(_currentValue - _desiredValue)*100)/100

  def message: String =
    s"""|Current value = ${_currentValue}$celsiusDegree
      |Previous value = ${_previousValue}$celsiusDegree
      |Change = ${change}$celsiusDegree
      |Distance to center of desired range = ${dist}$celsiusDegree
    """.stripMargin

  private val celsiusDegree = "\u2103"
}
