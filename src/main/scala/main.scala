@main
def main(): Unit = {
  val input = funcErr_GetInput()

}

type X = Long
enum Result:
  case Success(X: X)
  case Failure(errMsg: String)

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
private def funcErr_Div(a: Long,b: Long): Result = {
  try { Result.Success(Math.floorDiv(100,a)) }
  catch {
    case err: Exception => Result.Failure(err.getMessage)
  }
}

