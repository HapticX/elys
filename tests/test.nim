import ../src/elys


exec("""
# Here we just declare variables
var x = 10
var y = 20

# Unary operator in action
x = -x + 5
print x

print x + y
print -x + y
""")
