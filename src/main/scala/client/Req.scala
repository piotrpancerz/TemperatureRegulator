package client

@SerialVersionUID(15L)
class Req(val _currentValue: Double, val _lowerRange: Double, val _upperRange: Double) extends Serializable {
  def currentValue = _currentValue;
  def lowerRange = _lowerRange
  def upperRange = _upperRange
  def desiredValue = math.rint((math.abs(_upperRange + _lowerRange)/2)*100)/100
}
