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


## Variables

**Elys** supports two types of variables - `var` and `const`.

`var` is mutable variable. `const` is immutable variable

### Example

```elys
var x = 10
const y = 20

x = 20  # success
y = 10  # error
```
