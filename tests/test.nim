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
  test "if-elif-else bool operations":
    exec("""
    if 0 {
      print "0!"
    } elif (false) {
      print "oops", false
    } elif (false) {
      print "oops", false
    } else {
      print "yeap"
    }

    if 2 + 2 * 2 == 6 {
      print "yeap, 2 + 2 * 2 is 6"
    }

    print if false {
      var x = 10
      x
    } else {
      var y = 20
      y
    }

    var x = if ("") {0} elif (true) {"hello"} else {.0}
    print x
    """)
