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
  
  test "embedded statements":
    exec("""
    var x = {
      10
    }
    print x
    """)
  
  test "while statement":
    exec("""
    var y = 0
    print {
      var x = 0
      while x < 10 {
        x++
        if x % 2 == 0 {
          y++
          continue
        }
        if x == 7 {
          break
        }
      }
      x
    }
    print y
    """)
  
  test "test arrays":
    exec("""
    var x = [1, 2, 3, 4, 5]
    var y = [
      1  # you can create arrays also without comma
      # it's possible to use embedded statements here
      {var x=0; while (x < 5) {x++}; x;}
    ]
    print x, y
    print x[0], x[1], y[1]
    var mt = [
      [1 2 3]
      [4 5 6]
      [7 8 9]
    ]
    print mt
    print mt[1][1]
    print {
      [2 3 4]
    }[1]
    print "Hello, world!"[10]
    print "Hello, world!"[-2]
    print "Hello, world!"[2..10]
    print "Hello, world!"[..3]
    print "Hello, world!"[..-3]
    print "Hello, world!"[3..]
    print "'" + "Hello, world!"[3..5] + "'" + " is not " + "'" + "Hello, world"[3..<5] + "'"

    # and ... 💀
    print [
      [1 2 3]
      [4 5 6]
      {
        var x = [0 0 0]
        x[0] = 7
        x[1] = 8
        x[2] = 9
        x
      }
    ][{
      var x = 0
      while x < 2 {x++}
      x
    }..]
    """)
  
  test "swap":
    exec("""
    var x = [1 2 3 4 5]
    var y = [6 7 8 9 10]

    print x, y
    x, y = y, x
    print x, y
    x[0], y[0] = 100, 50
    print x, y
    """)
  
  test "for statement":
    exec("""
    var x = [10 11 12 13 14]
    for i, v in x {
      print i, v
    }
    for i in x {
      print i
    }
    for i in 0..<10 {
      print i
    }
    for i, v in "hello" {
      print i, v
    }

    # and ... 💀
    for v in {
      var y = if (false) {
        {var x = 0 x}..{var x = 10 x}
      } else {
        {var x = 0 x}..<{var x = 10 x}
      }
      y
    } {
      print v
    }
    """)
  
  test "dictionaries":
    exec("""
    var x = {
      "hello": "world",
      123: true,
      false: on,
      12039: {
        "0": [
          {
            # and ... 💀
            "x": {
              var x = 0;
              x++;
              x
            }
          } {
            [1 2 3]: true
          }
        ]
      }
    }
    print x
    x[false] = [1 2 3 4 5]
    x[off] = 'it\'s like false :)'
    print x
    print x[12039]['0'][0]["x"]
    """)
  
  test "generators":
    exec"""
    # this is simple generator
    print {
      for i in 0..10 {
        if i % 2 {
          i
        }
      }
    }
    # here python-like generators
    print [i for i in 0..10]
    print [i for i in 0..10 if i % 2]
    print [i*2 for i in 0..10 if i % 2]
    """
