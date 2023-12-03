import scala.annotation.targetName

@main
def main(): Unit = {
  val input = funcErr_GetInput()
  //BEGIN Implementacja z zadania 2
  input match {
    case Result.Failure(errMsg) => println(errMsg)
    case Result.Success(x) => {
      val add = funcErr_Add(x)
      add match {
        case Result.Failure(errMsg) => println(errMsg)
        case Result.Success(y) => {
          val div = funcErr_Div(y)
          div match {
            case Result.Failure(errMsg) => println(errMsg)
            case Result.Success(z) => println("functions completed successfully")
          }
        }
      }
    }
  }
  // END implementacja z zadania 2

  // Implementacja z zadania 3:
  val z = executeOnSuccess(input, funcErr_Add)
  println(executeOnSuccess(z, funcErr_Div))

  // implementacja operatorem >>= z zadania 4:
  println(input >>= funcErr_Add >>= funcErr_Div)
}


type X = Long
sealed trait Result {
  @targetName(".>>=")
  def >>=(f: X => Result): Result = executeOnSuccess(this, f)
}
object Result {
  case class Success(X: X) extends Result
  case class Failure(errMsg: String) extends Result

}
// Zadanie 2:
// possible failure functions:
//function 1: gets user input, could fail because of non numeric input
//function 2: adds 100 to user input number, could fail because of overflow
//function 3: divides 100 by addition result, could fail because of division by zero
private def funcErr_GetInput(): Result = {
  try { Result.Success(scala.io.StdIn.readLine().toLong) }
  catch {
    case err: Exception => Result.Failure(err.getMessage)
  }
}
private def funcErr_Add(a: Long): Result = {
  try { Result.Success(Math.addExact(a,100)) }
  catch {
    case err: Exception => Result.Failure(err.getMessage)
  }
}
private def funcErr_Div(a: Long): Result = {
  try { Result.Success(Math.floorDiv(100,a)) }
  catch {
    case err: Exception => Result.Failure(err.getMessage)
  }
}

// Zadanie 3:
private def executeOnSuccess(inputValue: Result, f: X => Result): Result = {
  inputValue match {
    case Result.Failure(errMsg) => Result.Failure(errMsg)
    case Result.Success(x) => f(x)
  }
}


