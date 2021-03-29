object Skeleton {
  type mconfig = TapeMachine => MConfiguration
  def next(config: mconfig): MConfiguration = new MConfiguration(config)
  val NON = Tape.BLANK

  //find
  def f(tape: TapeMachine, C: mconfig, B:mconfig, a: Char): MConfiguration = {
    tape.read match {
      case 'ə' => tape.L; next(f1(_,C,B,a)) 
      case  _  => tape.L; next(f(_,C,B,a)) 
    }
  }
  def f1(tape: TapeMachine, C: mconfig, B:mconfig, a: Char): MConfiguration = {
    tape.read match {
      case `a` =>         next(C) 
      case NON => tape.R; next(f2(_,C,B,a)) 
      case  _  => tape.R; next(f1(_,C,B,a))
    }
  }
  def f2(tape: TapeMachine, C: mconfig, B:mconfig, a: Char): MConfiguration = {
    tape.read match {
      case `a` =>         next(C) 
      case NON => tape.R; next(B) 
      case  _  => tape.R; next(f1(_,C,B,a))
    }
  }

  //erase
  def e(tape: TapeMachine, C: mconfig, B:mconfig, a: Char): MConfiguration = 
    next(f(_,e1(_,C),B,a))
  def e1(tape: TapeMachine, C: mconfig, B:mconfig, a: Char): MConfiguration = {
    tape.E
    next(C)
  }
  def e(tape: TapeMachine, B: mconfig, a: Char): MConfiguration =
    next(e(_, e(_,B,a), B, a))

  //print at the end
  def pe(tape: TapeMachine, C: mconfig, b: Char): MConfiguration =
    next(f(_, pe1(_,C,b), C, 'ə'))

  def pe1(tape: TapeMachine, C: mconfig, b: Char): MConfiguration = {
    tape.read match {
      case NON => tape.P('b'); next(C) 
      case  _  => tape.R.R;    next(pe1(_,C,b))
    }
  }

  //left & right
  def l(tape: TapeMachine, C: mconfig): MConfiguration = {
    tape.L
    next(C)
  }
  def r(tape: TapeMachine, C: mconfig): MConfiguration = {
    tape.R
    next(C)
  }

  //find & move
  def fl(tape: TapeMachine, C: mconfig, B: mconfig, a: Char): MConfiguration =
    next(f(_, l(_,C), B, a))
  def fr(tape: TapeMachine, C: mconfig, B: mconfig, a: Char): MConfiguration =
    next(f(_, r(_,C), B, a))

  //copy
  def c(tape: TapeMachine, C: mconfig, B: mconfig, a: Char): MConfiguration =
    next(fl(_, c1(_,C), B, a))
  def c1(tape: TapeMachine, C: mconfig): MConfiguration = {
    next(pe(_, C, tape.read)) 
  }

  //copy & erase
  def ce(tape: TapeMachine, C: mconfig, B: mconfig, a: Char): MConfiguration =
    next(c(_, e(_,C,B,a), B, a))

  def ce(tape: TapeMachine, B: mconfig, a: Char): MConfiguration =
    next(ce(_, ce(_,B,a), B, a))

  //replace
  def re(tape: TapeMachine, C: mconfig, B: mconfig, a: Char, b: Char): MConfiguration =
    next(f(_, re1(_, C, B, a, b), B, a))
  def re1(tape: TapeMachine, C: mconfig, B: mconfig, a: Char, b: Char): MConfiguration = {
    tape.E.P(b)
    next(C)
  }
  def re(tape: TapeMachine, B: mconfig, a: Char, b: Char): MConfiguration =
    next(re(_, re(_, B, a, b), B, a, b))
  
  //copy & replace
  def cr(tape: TapeMachine, C: mconfig, B: mconfig, a: Char): MConfiguration =
    next(c(_, re(_,C,B,a,a), B, a))
  def cr(tape: TapeMachine, B: mconfig, a: Char): MConfiguration =
    next(cr(_, cr(_,B,a), re(_,B,a,a), a))

  //compare
  def cp(tape: TapeMachine, C: mconfig, A: mconfig, E: mconfig, a: Char, b: Char): MConfiguration =
    next(fl(_, cp1(_,C,A,b), f(_,A,E,b), a))
  def cp1(tape: TapeMachine, C: mconfig, A: mconfig, b: Char): MConfiguration = {
    val y = tape.read
    next(fl(_, cp2(_,C,A,y), A, b))
  }
  def cp2(tape: TapeMachine, C: mconfig, A: mconfig, y: Char): MConfiguration = {
    tape.read match {
      case `y` => next(C)
      case  _  => next(A)
    }
  }

  //compare & erase
  def cpe(tape: TapeMachine, C: mconfig, A: mconfig, E: mconfig, a: Char, b: Char): MConfiguration =
    next(cp(_, e(_, e(_,C,C,b), C, a), A, E, a, b))
  def cpe(tape: TapeMachine, A: mconfig, E: mconfig, a: Char, b: Char): MConfiguration =
    next(cpe(_, cpe(_, A, E, a, b), A, E, a, b))

  //find end (first double blank)
  def g(tape: TapeMachine, C: mconfig): MConfiguration = {
    tape.read match {
      case NON => tape.R; next(g1(_, C)) 
      case  _  => tape.R; next(g(_, C)) 
    }
  }
  def g1(tape: TapeMachine, C: mconfig): MConfiguration = {
    tape.read match {
      case NON =>         next(C) 
      case  _  => tape.R; next(g(_, C)) 
    }
  }
  //find last
  def g(tape: TapeMachine, C: mconfig, a: Char): MConfiguration =
    next(g(_, g1(_,C,a)))
  def g1(tape: TapeMachine, C: mconfig, a: Char): MConfiguration = {
    tape.read match {
      case `a` =>         next(C) 
      case  _  => tape.L; next(g1(_, C, a)) 
    }
  }

  //print a b at the end
  def pe2(tape: TapeMachine, C: mconfig, a: Char, b: Char): MConfiguration =
    next(pe(_, pe(_,C,b), a))
  
  //copy more than one character to the end
  def ce2(tape: TapeMachine, B: mconfig, a: Char, b: Char): MConfiguration =
    next(ce(_, ce(_,B,b), a))
  def ce3(tape: TapeMachine, B: mconfig, a: Char, b: Char, y: Char): MConfiguration =
    next(ce(_, ce2(_,B,b,y), a))
  def ce4(tape: TapeMachine, B: mconfig, a: Char, b: Char, y: Char, d: Char): MConfiguration =
    next(ce(_, ce3(_,B,b,y,d), a))
  def ce5(tape: TapeMachine, B: mconfig, a: Char, b: Char, y: Char, d: Char, e: Char): MConfiguration =
    next(ce(_, ce4(_,B,b,y,d,e), a))
  
  //erase all markers
  def e(tape: TapeMachine, C: mconfig): MConfiguration = {
    tape.read match {
      case 'ə' => tape.R; next(e1(_,C)) 
      case  _  => tape.L; next(e(_, C)) 
    }
  }
  def e1(tape: TapeMachine, C: mconfig): MConfiguration = {
    tape.read match {
      case NON =>             next(C) 
      case  _  => tape.R.E.R; next(e1(_, C)) 
    }
  }
}
