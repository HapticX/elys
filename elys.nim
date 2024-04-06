import
  ./modules/lexer,
  ./modules/parser,
  ./modules/ast,
  options


var tokens = parseForTokens("""
# Hi there
var x = 10
var y = 20
print(x + y, 2 + 2 * 2 / 10 * .1)

x *= -2
print x

print 7 / 3
print 7 // 3

const xy = x + y
print(xy)
# xy += 2 # this will cause error
# print(xy)
""")

var
  parsed = tokens.elysParser()
  env = newEnv()
if parsed.isSome:
  discard parsed.get.ast.eval(env)
