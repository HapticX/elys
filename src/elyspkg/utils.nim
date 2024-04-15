import
  strutils

when not defined(js):
  import terminal


func getLineFromCode(source: ptr string, line: int): string =
  let code = source[].split("\n")
  return code[line-1]


func exception*(msg, exception: string, line, col: int, source, filepath: ptr string) =
  {.cast(noSideEffect).}:
    when not defined(js):
      styledEcho fgMagenta, exception, fgRed,
                 " at ", fgYellow, filepath[], "(", $line, ", ", $col, ")"
      echo getLineFromCode(source, line)
      echo " ".repeat(col), "^"
      styledEcho fgRed, msg
    else:
      echo (
        "Error [", exception, "] ",
        "at ", filepath[], "(", $line, ", ", $col, ")"
      )
      echo getLineFromCode(source, line)
      echo " ".repeat(col), "^"
      echo msg
    quit(0)


func elysError*(msg: string, line, col: int, source, filepath: ptr string) =
  exception(msg, "ElysError", line, col, source, filepath)
func zeroDivisionError*(msg: string, line, col: int, source, filepath: ptr string) =
  exception(msg, "ZeroDivisionError", line, col, source, filepath)
func syntaxError*(msg: string, line, col: int, source, filepath: ptr string) =
  exception(msg, "SyntaxError", line, col, source, filepath)
func assignedBefore*(msg: string, line, col: int, source, filepath: ptr string) =
  exception(msg, "AssignedBefore", line, col, source, filepath)
func usedBeforeAssign*(msg: string, line, col: int, source, filepath: ptr string) =
  exception(msg, "UsedBeforeAssign", line, col, source, filepath)
func valueError*(msg: string, line, col: int, source, filepath: ptr string) =
  exception(msg, "ValueError", line, col, source, filepath)
