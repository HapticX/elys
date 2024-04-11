import
  ../src/elys,
  unittest


suite "Elys":

  test "function declaration":
    exec("""
    fn testFunc(a, b, c, d = 10, e = "testFunc", f = {"x": 10}) {
      print a, b, c, d, e, f
    }

    print testFunc

    testFunc(1, 2, 3, d = 1, e = 2, f = 3)
    testFunc(1, 2, 3, 4, 5, 6)
    testFunc(1, 2, 3)
    testFunc(1, 2, 3, f = 1012030)

    fn factorial(n) {
      if n != 1 {
        n * factorial(n-1)
      } else {
        1
      }
    }

    print factorial(5)
    print (5) factorial
    print 5.factorial()


    print {
      var x = 0
      for i in 0..<5 {
        x++
      }
      x
    }.factorial()

    fn test(s) {
      s*2;
    }

    # calling functions with one string argument
    print test"hello"
    """)
