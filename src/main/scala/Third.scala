object Third {
  type mconfig = TapeMachine => MConfiguration
  def next(config: mconfig): MConfiguration = new MConfiguration(config)

  val begin: mconfig = tape => 
    tape.read match {
      case Tape.BLANK => tape.P('0');     next(begin)
      case '0'        => tape.R.R.P('1'); next(begin)
      case '1'        => tape.R.R.P('0'); next(begin)
    } 

  def begin2(tape: TapeMachine, c: Char) : MConfiguration = 
    tape.read match {
      case Tape.BLANK => tape.P(c);       next(begin2(_, '0'))
      case '0'        => tape.R.R.P('1'); next(begin2(_, '0'))
      case '1'        => tape.R.R.P('0'); next(begin2(_, '0'))
    }  

  def begin3(tape: TapeMachine, c1: Char, c2:Char) : MConfiguration = 
    tape.read match {
      case Tape.BLANK => tape.P(c1);     next(begin3(_, c1, c2))
      case `c1`       => tape.R.R.P(c2); next(begin3(_, c1, c2))
      case `c2`       => tape.R.R.P(c1); next(begin3(_, c1, c2))
    } 
}
