class TapeMachineTest extends org.scalatest.funsuite.AnyFunSuite {
  test("Read") {
    val tape = new Tape
    assert(tape.read == Tape.BLANK)
  }
  test("Print") {
    val tape = new Tape
    tape.P('1')
    assert(tape.read == '1')
  }
  test("Erase") {
    val tape = new Tape
    tape.P('1')
    assert(tape.read == '1')
    tape.E
    assert(tape.read == Tape.BLANK)
  }
  test("Left") {
    val tape = new Tape
    tape.P('1')
    tape.L
    assert(tape.read==Tape.BLANK)
  }
  test("Right") {
    val tape = new Tape
    tape.P('1')
    tape.R
    assert(tape.read==Tape.BLANK)
  }
  test("Left-Right-Left") {
    val tape = new Tape
    tape.P('1')
    tape.L
    tape.P('0')
    tape.R
    assert(tape.read == '1')
    tape.L
    assert(tape.read == '0')
  }
  test("Right-Left-Right") {
    val tape = new Tape
    tape.P('1')
    tape.R
    tape.P('0')
    tape.L
    assert(tape.read == '1')
    tape.R
    assert(tape.read == '0')
  }
  test("toString") {
    val tape = new Tape
    tape.P('1')
    tape.R
    tape.P('0')
    tape.R
    tape.P('1')
    tape.R
    tape.P('0')
    tape.L
    assert(tape.toString == "1 0[1]0")
  }

  

  test("f if there is something to find") {
    type mconfig = TapeMachine => MConfiguration
    def next(config: mconfig): MConfiguration = new MConfiguration(config)
    val NON = Tape.BLANK

    def testConfig(tape: TapeMachine, Sym: Char): MConfiguration = { 
      tape.read match {
        case 'ə' | Sym => tape.P(Sym); next(testConfig(_,Sym))
        case  _        => tape.L;      next(testConfig(_,Sym))
      }
    }
    val C = testConfig(_, 'C')
    val B = testConfig(_, 'B')
    def begin: mconfig = tape => {
      tape.P('ə').R.P('1').R.R.P('a')
      next(Skeleton.f(_,C,B,'a'))
    }

    val tape = MachineRunner.runMachine(begin, 1000)
    assert(tape.read == 'C')
  }
  test("f if there is nothing to find") {
    type mconfig = TapeMachine => MConfiguration
    def next(config: mconfig): MConfiguration = new MConfiguration(config)
    val NON = Tape.BLANK

    def testConfig(tape: TapeMachine, Sym: Char): MConfiguration = { 
      tape.read match {
        case 'ə' | Sym => tape.P(Sym); next(testConfig(_,Sym))
        case  _        => tape.L;      next(testConfig(_,Sym))
      }
    }
    val C = testConfig(_, 'C')
    val B = testConfig(_, 'B')
    def begin: mconfig = tape => {
      tape.P('ə').R.P('1').R.R.P('d')
      next(Skeleton.f(_,C,B,'a'))
    }

    val tape = MachineRunner.runMachine(begin, 1000)
    assert(tape.read == 'B')
  }
}
