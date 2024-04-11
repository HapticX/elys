<div align="center">

# elys
### scripting language

</div>


## Why Elys? 🤔
**Elys** is a lightweight scripting language written in Nim, designed to seamlessly integrate into various applications.

Thanks to its compact implementation, **Elys** can be embedded into Minecraft mods, web applications, native apps, mobile applications, and even devices like smart refrigerators.

With its simple syntax, **Elys** is an excellent choice for rapid prototyping and building extensible systems.

It provides easy interaction with the host application and straightforward handling of tasks such as data management, interface control, and process automation.


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

## If Statements

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
print x
```
