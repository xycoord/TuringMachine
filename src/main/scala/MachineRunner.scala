object MachineRunner{
  type mconfig = TapeMachine => MConfiguration

  def runMachine(begin: mconfig, cycles: Int) : Tape = {
    var mconfiguration = new MConfiguration(begin)
    val tape = new Tape
    var i = 0
    while(i<cycles){
      mconfiguration = mconfiguration.apply(tape)
      i+=1
    }
    tape
  }
}
class MConfiguration(config: TapeMachine => MConfiguration){
  def apply(tape: TapeMachine): MConfiguration =
    config(tape)
}
