object Sqrt2 {
  type mconfig = TapeMachine => MConfiguration
  def next(config: mconfig): MConfiguration = new MConfiguration(config)
  val NON = Tape.BLANK

  val begin: mconfig = tape => 
    tape.read match {
      case NON => tape.P('@').R.P('1'); next(newDigit)
    }  

  val newDigit: mconfig = tape => 
    tape.read match {
      case '@' => tape.R; next(markDigits)
      case  _  => tape.L; next(newDigit)
    }

  val markDigits: mconfig = tape => 
    tape.read match {
      case '0' | '1' => tape.R.P('x').R;          next(markDigits)
      case    NON    => tape.R.P('z').R.R.P('r'); next(findX)
    } 

  val findX: mconfig = tape => 
    tape.read match {
      case 'x' => tape.E;   next(firstR)
      case '@' =>           next(findDigits)
      case  _  => tape.L.L; next(findX)
    }

  val firstR: mconfig = tape => 
    tape.read match {
      case 'r' => tape.R.R; next(lastR)
      case  _  => tape.R.R; next(firstR)
    }

  val lastR: mconfig = tape => 
    tape.read match {
      case 'r' => tape.R.R;               next(lastR)
      case NON => tape.P('r').R.R.P('r'); next(findX)
    }

  val findDigits: mconfig = tape => 
    tape.read match {
      case '@' => tape.R.R; next(find1stDigit)
      case  _  => tape.L.L; next(findDigits)
    }

  val find1stDigit: mconfig = tape => 
    tape.read match {
      case 'x' | 'y' => tape.L;   next(found1stDigit)
      case    'z'    => tape.L;   next(found2ndDigit)
      case    NON    => tape.R.R; next(find1stDigit)
    }

  val found1stDigit: mconfig = tape => 
    tape.read match {
      case '0' => tape.R;     next(addZero)
      case '1' => tape.R.R.R; next(find2ndDigit)
    }

  val find2ndDigit: mconfig = tape => 
    tape.read match {
      case 'x' | 'y'  => tape.L;   next(found2ndDigit)
      case    NON     => tape.R.R; next(find2ndDigit)
    }

  val found2ndDigit: mconfig = tape => 
    tape.read match {
      case '0'       => tape.R; next(addZero)
      case '1' | NON => tape.R; next(addOne)
    }
 
  val addZero: mconfig = tape => 
    tape.read match {
      case 'r' => tape.P('s'); next(addFinished)
      case 'u' => tape.P('v'); next(addFinished)
      case  _  => tape.R.R;    next(addZero)
    }

  val addOne: mconfig = tape => 
    tape.read match {
      case 'r' => tape.P('v');     next(addFinished)
      case 'u' => tape.P('s').R.R; next(carry)
      case  _  => tape.R.R;        next(addOne)
    }

  val carry: mconfig = tape => 
    tape.read match {
      case 'r' => tape.P('u');     next(addFinished)
      case NON => tape.P('u');     next(newDigitIsZero)
      case 'u' => tape.P('r').R.R; next(carry)
    }

  val addFinished: mconfig = tape => 
    tape.read match {
      case '@' => tape.R.R; next(eraseOldX)
      case  _  => tape.L.L; next(addFinished)
    }

  val eraseOldX: mconfig = tape => 
    tape.read match {
      case 'x' => tape.E.L.L;      next(printNewX)
      case 'z' => tape.P('y').L.L; next(printNewX)
      case  _  => tape.R.R;        next(eraseOldX)
    }

  val printNewX: mconfig = tape => 
    tape.read match {
      case '@' => tape.R.R;    next(eraseOldY)
      case 'y' => tape.P('z'); next(findDigits)
      case NON => tape.P('x'); next(findDigits)
    }

  val eraseOldY: mconfig = tape => 
    tape.read match {
      case 'y' => tape.E.L.L; next(printNewY)
      case  _  => tape.R.R;   next(eraseOldY)
    }

  val printNewY: mconfig = tape => 
    tape.read match {
      case '@' => tape.R;        next(newDigitIsOne)
      case  _  => tape.P('y').R; next(resetNewX)
    }

  val resetNewX: mconfig = tape => 
    tape.read match {
      case NON => tape.R.P('x'); next(flagResultDigits)
      case  _  => tape.R.R;      next(resetNewX)
    }

  val flagResultDigits: mconfig = tape => 
    tape.read match {
      case 's' => tape.P('t').R.R; next(unflagResultDigits)
      case 'v' => tape.P('w').R.R; next(unflagResultDigits)
      case  _  => tape.R.R;        next(flagResultDigits)
    }

  val unflagResultDigits: mconfig = tape => 
    tape.read match {
      case 's' => tape.P('r').R.R; next(unflagResultDigits)
      case 'v' => tape.P('u').R.R; next(unflagResultDigits)
      case  _  =>                  next(findDigits)
    }

  val newDigitIsZero: mconfig = tape => 
    tape.read match {
      case '@' => tape.R; next(printZeroDigit)
      case  _  => tape.L; next(newDigitIsZero)
    }

  val printZeroDigit:mconfig = tape => 
    tape.read match {
      case '0' | '1' => tape.R.E.R;        next(printZeroDigit)
      case    NON    => tape.P('0').R.R.R; next(cleanup)
    }

  val newDigitIsOne: mconfig = tape => 
    tape.read match {
      case '@' => tape.R; next(printOneDigit)
      case  _  => tape.L; next(newDigitIsOne)
    }

  val printOneDigit: mconfig = tape => 
    tape.read match {
      case '0' | '1' => tape.R.E.R;        next(printOneDigit)
      case    NON    => tape.P('1').R.R.R; next(cleanup)
    }

  val cleanup: mconfig = tape => 
    tape.read match {
      case NON =>             next(newDigit)
      case  _  => tape.E.R.R; next(cleanup)
    }
}


object Sqrt2b {
  type mconfig = TapeMachine => MConfiguration
  def next(config: mconfig): MConfiguration = new MConfiguration(config)
  val NON = Tape.BLANK

  val begin: mconfig = tape => 
    tape.read match {
      case NON => tape.P('@').R.P('1'); next(gotoSentinel(_,markDigits))
    }  

  val markDigits: mconfig = tape => 
    tape.read match {
      case '0' | '1' => tape.R.P('x').R;          next(markDigits)
      case    NON    => tape.R.P('z').R.R.P('r'); next(findX)
    } 

  val findX: mconfig = tape => 
    tape.read match {
      case 'x' => tape.E;   next(firstR)
      case '@' =>           next(findDigits)
      case  _  => tape.L.L; next(findX)
    }

  val firstR: mconfig = tape => 
    tape.read match {
      case 'r' => tape.R.R; next(lastR)
      case  _  => tape.R.R; next(firstR)
    }

  val lastR: mconfig = tape => 
    tape.read match {
      case 'r' => tape.R.R;               next(lastR)
      case NON => tape.P('r').R.R.P('r'); next(findX)
    }

  val findDigits: mconfig = tape => 
    tape.read match {
      case '@' => tape.R.R; next(find1stDigit)
      case  _  => tape.L.L; next(findDigits)
    }

  val find1stDigit: mconfig = tape => 
    tape.read match {
      case 'x' | 'y' => tape.L;   next(found1stDigit)
      case    'z'    => tape.L;   next(found2ndDigit)
      case    NON    => tape.R.R; next(find1stDigit)
    }

  val found1stDigit: mconfig = tape => 
    tape.read match {
      case '0' => tape.R;     next(addZero)
      case '1' => tape.R.R.R; next(find2ndDigit)
    }

  val find2ndDigit: mconfig = tape => 
    tape.read match {
      case 'x' | 'y'  => tape.L;   next(found2ndDigit)
      case    NON     => tape.R.R; next(find2ndDigit)
    }

  val found2ndDigit: mconfig = tape => 
    tape.read match {
      case '0'       => tape.R; next(addZero)
      case '1' | NON => tape.R; next(addOne)
    }
 
  val addZero: mconfig = tape => 
    tape.read match {
      case 'r' => tape.P('s'); next(addFinished)
      case 'u' => tape.P('v'); next(addFinished)
      case  _  => tape.R.R;    next(addZero)
    }

  val addOne: mconfig = tape => 
    tape.read match {
      case 'r' => tape.P('v');     next(addFinished)
      case 'u' => tape.P('s').R.R; next(carry)
      case  _  => tape.R.R;        next(addOne)
    }

  val carry: mconfig = tape => 
    tape.read match {
      case 'r' => tape.P('u');     next(addFinished)
      case NON => tape.P('u');     next(gotoSentinel(_,printDigit(_,0)))
      case 'u' => tape.P('r').R.R; next(carry)
    }

  val addFinished: mconfig = tape => 
    tape.read match {
      case '@' => tape.R.R; next(eraseOldX)
      case  _  => tape.L.L; next(addFinished)
    }

  val eraseOldX: mconfig = tape => 
    tape.read match {
      case 'x' => tape.E.L.L;      next(printNewX)
      case 'z' => tape.P('y').L.L; next(printNewX)
      case  _  => tape.R.R;        next(eraseOldX)
    }

  val printNewX: mconfig = tape => 
    tape.read match {
      case '@' => tape.R.R;    next(eraseOldY)
      case 'y' => tape.P('z'); next(findDigits)
      case NON => tape.P('x'); next(findDigits)
    }

  val eraseOldY: mconfig = tape => 
    tape.read match {
      case 'y' => tape.E.L.L; next(printNewY)
      case  _  => tape.R.R;   next(eraseOldY)
    }

  val printNewY: mconfig = tape => 
    tape.read match {
      case '@' => tape.R;        next(gotoSentinel(_,printDigit(_,'1')))
      case  _  => tape.P('y').R; next(resetNewX)
    }

  val resetNewX: mconfig = tape => 
    tape.read match {
      case NON => tape.R.P('x'); next(flagResultDigits)
      case  _  => tape.R.R;      next(resetNewX)
    }

  val flagResultDigits: mconfig = tape => 
    tape.read match {
      case 's' => tape.P('t').R.R; next(unflagResultDigits)
      case 'v' => tape.P('w').R.R; next(unflagResultDigits)
      case  _  => tape.R.R;        next(flagResultDigits)
    }

  val unflagResultDigits: mconfig = tape => 
    tape.read match {
      case 's' => tape.P('r').R.R; next(unflagResultDigits)
      case 'v' => tape.P('u').R.R; next(unflagResultDigits)
      case  _  =>                  next(findDigits)
    }

  def gotoSentinel(tape: TapeMachine, A: mconfig): MConfiguration = {
    tape.read match {
      case '@' => tape.R; next(A)
      case  _  => tape.L; next(gotoSentinel(_,A))
    }  
  }

  val newDigit: mconfig = tape => 
    tape.read match {
      case '@' => tape.R; next(markDigits)
      case  _  => tape.L; next(newDigit)
    }

  def printDigit(tape: TapeMachine, digit: Char): MConfiguration ={
    tape.read match {
      case '0' | '1' => tape.R.E.R;        next(printDigit(_,digit))
      case    NON    => tape.P('0').R.R.R; next(cleanup)
    }
  }
  
  val cleanup: mconfig = tape => 
    tape.read match {
      case NON =>             next(newDigit)
      case  _  => tape.E.R.R; next(cleanup)
    }
}
