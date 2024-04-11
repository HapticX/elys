import
  strutils

when not defined(js):
  import terminal


func getLineFromCode(source: ptr string, line: int): string =
  let code = source[].split("\n")
  return code[line-1]


func exception*(msg, exception: string, line, col: int, source: ptr string) =
  {.cast(noSideEffect).}:
    when not defined(js):
      styledEcho fgMagenta, exception, fgRed,
                 " at ", fgYellow, "$file(", $line, ", ", $col, ")"
      echo getLineFromCode(source, line)
      echo " ".repeat(col), "^"
      styledEcho fgRed, msg
    else:
      echo (
        "Error [", exception, "] ",
        "at $file(", $line, ", ", $col, ")"
      )
      echo getLineFromCode(source, line)
      echo " ".repeat(col), "^"
      echo msg
    quit(0)


func elysError*(msg: string, line, col: int, source: ptr string) =
  exception(msg, "ElysError", line, col, source)
func zeroDivisionError*(msg: string, line, col: int, source: ptr string) =
  exception(msg, "ZeroDivisionError", line, col, source)
func syntaxError*(msg: string, line, col: int, source: ptr string) =
  exception(msg, "SyntaxError", line, col, source)
func assignedBefore*(msg: string, line, col: int, source: ptr string) =
  exception(msg, "AssignedBefore", line, col, source)
func usedBeforeAssign*(msg: string, line, col: int, source: ptr string) =
  exception(msg, "UsedBeforeAssign", line, col, source)
func valueError*(msg: string, line, col: int, source: ptr string) =
  exception(msg, "ValueError", line, col, source)
