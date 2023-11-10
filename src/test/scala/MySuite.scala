import munit._
import graalinterpreter._
import ast._
import Value._
class InterpreterSuite extends FunSuite {

  test("evalTerm should evaluate Integer") {
    val env = new Env()
    val term = Integer(5, None)
    val result = evalTerm(env, term)
    assertEquals(result, IntV(5))
  }

  test("evalTerm should evaluate Bool") {
    val env = new Env()
    val term = Bool(true, None)
    val result = evalTerm(env, term)
    assertEquals(result, BooleanV(true))
  }

  test("evalTerm should evaluate Str") {
    val env = new Env()
    val term = Str("hello", None)
    val result = evalTerm(env, term)
    assertEquals(result, StringV("hello"))
  }

  test("evalTerm should evaluate Var") {
    val env = new Env()
    env += ("x" -> IntV(10))
    val term = Var("x", None)
    val result = evalTerm(env, term)
    assertEquals(result, IntV(10))
  }

  test("evalTerm should evaluate Function") {
    val env = new Env()
    val term = Function(List(Parameter("x")), Integer(5, None), None)
    val result = evalTerm(env, term)
    assert(result.isInstanceOf[ClosureV])
  }

  test("evalTerm should evaluate Binary") {
    val env = new Env()
    val term = Binary(Integer(5, None), BinaryOp.Add, Integer(3, None), None)
    val result = evalTerm(env, term)
    assertEquals(result, IntV(8))
  }

  test("evalTerm should evaluate Let") {
    val env = new Env()
    val term = Let(Var("x"), Integer(5, None), Var("x", None), None)
    val result = evalTerm(env, term)
    assertEquals(result, IntV(5))
  }

  test("evalTerm should evaluate If") {
    val env = new Env()
    val term = If(Bool(true, None), Integer(5, None), Integer(3, None), None)
    val result = evalTerm(env, term)
    assertEquals(result, IntV(5))
  }

  test("evalTerm should evaluate Tuple") {
    val env = new Env()
    val term = Tuple(Integer(5, None), Integer(3, None), None)
    val result = evalTerm(env, term)
    assertEquals(result, TupleV(IntV(5), IntV(3)))
  }

  test("evalTerm should evaluate First") {
    val env = new Env()
    val term = First(Tuple(Integer(5, None), Integer(3, None), None), None)
    val result = evalTerm(env, term)
    assertEquals(result, IntV(5))
  }

  test("evalTerm should evaluate Second") {
    val env = new Env()
    val term = Second(Tuple(Integer(5, None), Integer(3, None), None), None)
    val result = evalTerm(env, term)
    assertEquals(result, IntV(3))
  }
  test("let _ = print(1); print(2) should print 1 and 2") {
    val env = new Env()
    val term = Let(
      Var("_"),
      Print(Integer(1, None), None),
      Print(Integer(2, None), None),
      None
    )
    val result = evalTerm(env, term)
    assertEquals(result, IntV(2))
  }

  test("f(print(1), print(2), print(3)) should print 1, 2 and 3") {
    val env = new Env()
    val term = Call(
      Function(
        List(Parameter("x"), Parameter("y"), Parameter("z")),
        Integer(0, None),
        None
      ),
      List(
        Print(Integer(1, None), None),
        Print(Integer(2, None), None),
        Print(Integer(3, None), None)
      ),
      None
    )
    val result = evalTerm(env, term)
    assertEquals(result, IntV(0))
  }

  test(
    "let tuple = (print(1), print(2)); print(tuple) should print 1, 2 and (1, 2)"
  ) {
    val env = new Env()
    val term = Let(
      Var("tuple"),
      Tuple(Print(Integer(1, None), None), Print(Integer(2, None), None), None),
      Print(Var("tuple", None), None),
      None
    )
    val result = evalTerm(env, term)
    assertEquals(result, TupleV(IntV(1), IntV(2)))
  }

  test("print(print(1) + print(2)) should print 1, 2 and 3") {
    val env = new Env()
    val term = Print(
      Binary(
        Print(Integer(1, None), None),
        BinaryOp.Add,
        Print(Integer(2, None), None),
        None
      ),
      None
    )
    val result = evalTerm(env, term)
    assertEquals(result, IntV(3))
  }

  test("3 + 5 should equal 8") {
    val env = new Env()
    val term = Binary(Integer(3, None), BinaryOp.Add, Integer(5, None), None)
    val result = evalTerm(env, term)
    assertEquals(result, IntV(8))
  }

  test("\"a\" + 2 should equal \"a2\"") {
    val env = new Env()
    val term = Binary(Str("a", None), BinaryOp.Add, Integer(2, None), None)
    val result = evalTerm(env, term)
    assertEquals(result, StringV("a2"))
  }

  test("2 + \"a\" should equal \"2a\"") {
    val env = new Env()
    val term = Binary(Integer(2, None), BinaryOp.Add, Str("a", None), None)
    val result = evalTerm(env, term)
    assertEquals(result, StringV("2a"))
  }

  test("\"a\" + \"b\" should equal \"ab\"") {
    val env = new Env()
    val term = Binary(Str("a", None), BinaryOp.Add, Str("b", None), None)
    val result = evalTerm(env, term)
    assertEquals(result, StringV("ab"))
  }
  test("Combination") {
    readProgram("src/test/resources/combination.rinha.json") map:
      case (Program(_name, term, _)) =>
        assertEquals(evalTerm(new Env(), term), IntV(499500))
  }
  test("Fib") {
    readProgram("src/test/resources/fib.rinha.json") map:
      case (Program(_name, term, _)) =>
        assertEquals(evalTerm(new Env(), term), IntV(102334155))
  }
  test("Operations") {
    try
      readProgram("src/test/resources/operations.rinha.json") map:
        case (Program(_name, term, _)) =>
          assertEquals(evalTerm(new Env(), term), StringV("OK"))
    catch case e: StackOverflowError => fail("Stack overflow")
  }
  test("Flood") {
    try
      readProgram("src/test/resources/flood.rinha.json") map:
        case (Program(_name, term, _)) =>
          assertEquals(evalTerm(new Env(), term), IntV(0))
    catch case e: StackOverflowError => fail("Stack overflow")
  }
  test("Fact") {

    readProgram("src/test/resources/fact.rinha.json") map:
      case (Program(_name, term, _)) =>
        val now = System.currentTimeMillis()
        val StringV(result) = (evalTerm(new Env(), term): @unchecked)
        val elapsed = System.currentTimeMillis() - now
        println(s"Fact took $elapsed ms")
        assert(!result.isEmpty())
    // catch
    //   case e: StackOverflowError =>
    //     println(e.getStackTrace().apply(0))
    //     fail("Stack overflow")
  }
}
