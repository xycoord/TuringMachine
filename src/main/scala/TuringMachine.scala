object TuringMachine {
    def main(args: Array[String]): Unit = {
      val tape = MachineRunner.runMachine(Third.begin, 20) 
      println(tape.getComputedNumberString)
      println(tape.getComputedNumber)
    }
}
