object MachineRunner{
  type mconfig = TapeMachine => MConfiguration

  def runMachine(begin: mconfig, cycles: Int) : TapeMachine = {
    val completeConfig = new CompleteConfiguration(new Tape, begin)

    for(i <- 0 until cycles) 
      completeConfig.next
    
    return completeConfig.tape
  }
}

class MConfiguration(config: TapeMachine => MConfiguration){
  def apply(tape: TapeMachine): MConfiguration =
    config(tape)
}

class CompleteConfiguration(private val tape_ : TapeMachine, private val init: TapeMachine => MConfiguration){
  private var mconfiguration_ = new MConfiguration(init)
  
  def tape: TapeMachine = tape_
  def mconfiguration: MConfiguration = mconfiguration_
  def next: Unit = {
      mconfiguration_ = mconfiguration_.apply(tape_)
  }
}
