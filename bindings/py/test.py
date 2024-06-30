import elys

elys.execute("""
var x = 0
for i in 0..<1000000 {
  x++
}
print x
""")

x = 0
for i in range(1_000_000):
  x += 1
print(x)