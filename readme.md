<div align="center">

# elys
### scripting language

[![GitHub Actions Workflow Status](https://img.shields.io/github/actions/workflow/status/HapticX/elys/artifacts.yml?style=for-the-badge&label=Build%20CLI)](https://github.com/HapticX/elys/actions/workflows/artifacts.yml)
[![GitHub Actions Workflow Status](https://img.shields.io/github/actions/workflow/status/HapticX/elys/test.yml?style=for-the-badge&label=Tests)](https://github.com/HapticX/elys/actions/workflows/test.yml)


</div>


## Why Elys? 🤔
**Elys** is a lightweight scripting language written in Nim, designed to seamlessly integrate into various applications.

Thanks to its compact implementation, **Elys** can be embedded into Minecraft mods, web applications, native apps, mobile applications, and even devices like smart refrigerators.

With its simple syntax, **Elys** is an excellent choice for rapid prototyping and building extensible systems.

It provides easy interaction with the host application and straightforward handling of tasks such as data management, interface control, and process automation.


## API

If you want to use **elys** in your project, then you just need to install it using `nimble` and use it as follows:
```sh
nimble install https://github.com/HapticX/elys
```

```nim
import elys

discard elys.exec("print(1)")
```


## Hello, world! 👋

Here's hello world program written in **Elys**:
```elys
print('Hello, world!')
```


## Variables 🧩

**Elys** supports two types of variables - `var` and `const`.

`var` is mutable variable. `const` is immutable variable

### Example

```elys
var x = 10
const y = 20

x = 20  # success
y = 10  # error
```

## If Statements ❓

```elys
if (2 + 2 * 2 == 8) {  # Order of operations as in math
  print('nope, 2 + 2 * 2 is 6 :(')
} elif (2 + 2 * 2 == 6) {
  print('yeap, 2 + 2 * 2 is 6!')
} else {
  print('???')
}
```

You can also use if statements as an expression to obtain a result for a variable:

```elys
var x = if (false) {
  0
} elif (false) {
  1
} else {
  2
}
print x  # will be 2
```

## Cycles 🔄

Elys provides `for` and `while` cycles:

```elys
for i in 0..<10 {
  print i  # will shows numbers from 0 to 9
}

var x = 0
while x < 10 {
  x++
}
print x  # will be 10
```
