import
  ../src/elys,
  unittest


suite "Elys":
  test "Variables":
    exec("""
    # Here we just declare variables
    var x = 10
    var y = 20
    """)
  test "Basic math":
    exec("""
    print -5 + 10
    print 2 + 2 * 2 / -10 * .1
    """)
  test "increment / decrement":
    exec("""
    var x = 10
    print x
    print x++
    print ++x
    """)
  test "Basic bool operations":
    exec("""
    var x = true
    var y = false
    x = true or false
    print x
    print x and y, x or y
    print 2 + 2 * 2 == 6, (2 + 2) * 2 == 8
    """)
