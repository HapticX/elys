import
  ../src/elys,
  ../src/elyspkg/lexer,
  unittest,
  benchy


suite "Elys":

  test "usage test":
    exec"""
    var x = [0 1 2 3 4 5 6 7]
    print x
    print x
    print x
    """

  test "while cycles (10 000 iterations)":
    timeIt "pure Nim":
      var x = 0
      while x < 10_000:
        inc x
    timeIt "pure Elys":
      exec"""
      var x = 0
      while x < 10000 {
        x++
      }
      """

  test "for cycles (10 000 iterations)":
    timeIt "pure Nim":
      var x = 0
      for i in 0..<10_000:
        inc x
    timeIt "pure Elys":
      exec"""
      var x = 0
      for i in 0..<10000 {
        x++
      }
      """
