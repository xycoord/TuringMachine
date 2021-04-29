trait TapeMachine {
  def read() : Char
  def P(symbol : Char) : TapeMachine
  def E() : TapeMachine
  def L() : TapeMachine
  def R() : TapeMachine
  def getComputedNumberString() : String
  def getComputedNumber() : Double
}

class Tape extends TapeMachine{
  private var head = Tape.Cell(Tape.BLANK, null, null)
  private var first = head

  def read(): Char = head.symbol

  def P(symbol: Char): TapeMachine = {
    head.symbol = symbol
    this
  }

  def E(): TapeMachine = {
    head.symbol = Tape.BLANK
    this
  }

  def L(): TapeMachine = {
    if(head.left == null){
      head.left = Tape.Cell(Tape.BLANK, null, head)
      first = head.left
    }
    head = head.left
    this
  }

  def R(): TapeMachine = {
    if(head.right == null) 
      head.right = Tape.Cell(Tape.BLANK, head, null)
    head = head.right
    this
  }

  override def toString(): String = {
    var cell = first
    var string = if (cell == head) "[" else ""
    while (cell != null){
      string += cell.symbol
      if (cell == head) string += "]"
      else if(cell.right != null)
        if(cell.right == head) string += "["
        else string += " "
      cell=cell.right
    }
    string
  }
  def getComputedNumberString(): String = {
    var cell = first
    while(cell != null && cell.symbol != '0' && cell.symbol != '1')
      cell = cell.right
    var string = ""
    while (cell != null){
      if(cell.symbol == '0' || cell.symbol == '1')
        string += cell.symbol
      if(cell.right != null)
        cell = cell.right.right
      else cell = null
    }
    string
  }

  def getComputedNumber() : Double = {
    var cell = first
    while(cell != null && cell.symbol != '0' && cell.symbol != '1')
      cell = cell.right
    var long : Long = 0
    var processedbits = 0
    while (cell != null && processedbits < 63){
      if(cell.symbol == '0' || cell.symbol == '1'){
        long = long << 1
        processedbits += 1
      }
      if(cell.symbol == '1')
        long += 1
      if(cell.right != null)
        cell = cell.right.right
      else cell = null
    }
    long = long << 63-processedbits
    (long.toDouble/((Long.MaxValue/2)+1))/2
  }
}
object Tape {
  val BLANK = '_'
  case class Cell(var symbol: Char, var left: Cell, var right: Cell)
}
