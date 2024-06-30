import
  std/strutils


proc isInteger(source: string, start: int): int =
  result = 0
  var i = start
  while i < source.len:
    if source[i].isDigit:
      inc result
    else:
      break
    inc i
  return result


proc isFloat(source: string, start: int): int =
  result = 0
  var i = start
  var hasDot = false
  var hasDigit = false
  while i < source.len:
    if source[i].isDigit:
      inc result
      hasDigit = true
    elif not hasDot and source[i] == '.':
      inc result
      hasDot = true
    else:
      break
    inc i
  if not hasDigit:
    return 0
  if source[i] == '.':
    return 0
  return result


proc isIdentifier(source: string, start: int): int =
  result = 0
  var i = start
  var hasDot = false
  while i < source.len:
    if source[i] in {'a'..'z', 'A'..'Z'} and start == i:
      inc result
    elif source[i] in {'0'..'9', 'a'..'z', 'A'..'Z', '_'}:
      inc result
    else:
      break
    inc i
  return result


proc testFor(source: string, start: int, val: string): int =
  result = 0
  var
    i = start
    j = 0
  if source.len - start < val.len:
    return 0
  while i < source.len and j < val.len:
    if source[i] == val[j]:
      inc result
    else:
      break
    inc i
    inc j
  if result == val.len:
    return result
  return 0


proc testFor(source: string, start: int, values: openarray[string]): int =
  for val in values:
    if (let length = testFor(source, start, val); length) > 0:
      return length
  return 0


type
  TokenKind* {.size: sizeof(int8).} = enum
    tkEof,
    tkInt,  # 1, 512, 1293
    tkFloat,  # .2, 1.0, 1f,
    tkBool,  # true, false, off, on
    tkString,  # "hello", 'hello', `multiline`
    tkId,  # variable, source, destination
    tkOp,  # +, -, /, *, ^, %, =
    tkKeyword,  # if, elif, else, while, for
    tkSep,  # ; and \n
    tkComment,
    tkUnknown  # otherwise
  Token* = object
    kind*: TokenKind
    value*: string
    pos*: int
    line*, col*: int
  LexState = enum
    None,
    String,
    DoubleString,
    Backtick,
    Comment


func `$`*(tk: Token): string =
  if tk.kind == tkSep:
    "(\\n, " & $tk.kind & ")"
  else:
    "(" & tk.value & ", " & $tk.kind & ")"


func parseForTokens*(source: string): seq[Token] =
  result = @[]

  var
    i: int = 0
    line: int = 1
    col: int = 1
    current = Token()
    src = source
    state = LexState.None

  while i < src.len:
    let chr = src[i]
    var prev: char
    if i > 0:
      prev = src[i-1]
    # new lines
    if chr == '\n':
      if state == LexState.Comment:
        state = LexState.None
      line += 1
      col = 1
    else:
      col += 1
    # One-line comments
    if chr == '#' and state == LexState.None:
      state = LexState.Comment
      inc i
      continue
    elif state == LexState.Comment:
      inc i
      continue
    # Strings
    elif chr == '\'' and state == LexState.None:
      state = LexState.String
      current = Token(kind: tkString, pos: i, line: line, col: col, value: $chr)
      inc i
    elif chr == '\'' and state == LexState.String and prev != '\\':
      state = LexState.None
      current.value &= $chr
      inc current.pos
      inc i
      result.add(current)
    elif chr == '"' and state == LexState.None:
      state = LexState.DoubleString
      current = Token(kind: tkString, pos: i, line: line, col: col, value: $chr)
      inc i
    elif chr == '"' and state == LexState.DoubleString and prev != '\\':
      state = LexState.None
      current.value &= $chr
      inc current.pos
      inc i
      result.add(current)
    elif chr == '`' and state == LexState.None:
      state = LexState.Backtick
      current = Token(kind: tkString, pos: i, line: line, col: col, value: $chr)
      inc i
    elif chr == '`' and state == LexState.Backtick and prev != '\\':
      state = LexState.None
      current.value &= $chr
      inc current.pos
      inc i
      result.add(current)
    elif state in {LexState.String, LexState.Backtick, LexState.DoubleString}:
      current.value &= $chr
      inc current.pos
      inc i
    # Integer
    elif (let length = isInteger(src, i); length) > 0:
      result.add Token(kind: tkInt, pos: i + length, col: col, line: line, value: src[i..<i+length])
      inc i, length
    # Floats
    elif (let length = isFloat(src, i); length) > 0:
      var value = src[i..<i+length]
      if value[0] == '.':
        value = "0" & value
      result.add Token(kind: tkFloat, pos: i + length, col: col, line: line, value: value)
      inc i, length
    # Boolean
    elif (let length = testFor(src, i, ["true", "false", "on", "off"]); length) > 0:
      result.add Token(kind: tkBool, pos: i + length, col: col, line: line, value: src[i..<i+length])
      inc i, length
    # Operators
    elif (let length = testFor(src, i, [">=", "==", "<=", "!=", "&&", "||", "++", "--", "//"]); length) > 0:
      result.add Token(kind: tkOp, pos: i + length, col: col, line: line, value: src[i..<i+length])
      inc i, length
    elif (let length = testFor(src, i, ["+=", "-=", "/=", "*=", "&=", "@=", "$=", "^=", "?=", "..<", ".."]); length) > 0:
      result.add Token(kind: tkOp, pos: i + length, col: col, line: line, value: src[i..<i+length])
      inc i, length
    elif (let length = testFor(src, i, ["not", "or", "and", "in", "of"]); length) > 0:
      result.add Token(kind: tkOp, pos: i + length, col: col, line: line, value: src[i..<i+length])
      inc i, length
    elif (let length = testFor(src, i, [".", "+", "-", "/", "\\", "[", "]", "(", ")", "{", "}", "~", "!", "@", "#", "$", "%", "^", "|", "&", "?", ":", ";", ",", "*", "=", ">", "<"]); length) > 0:
      result.add Token(kind: tkOp, pos: i + length, col: col, line: line, value: src[i..<i+length])
      inc i, length
    # Keywords
    elif (let length = testFor(src, i, ["fn", "if", "elif", "else", "while", "for", "case", "var", "const", "continue", "break", "return"]); length) > 0:
      # echo src[i..length], ", ", length
      result.add Token(kind: tkKeyword, pos: i + length, col: col, line: line, value: src[i..<i+length])
      inc i, length
    elif (let length = testFor(src, i, ["print", "null"]); length) > 0:
      result.add Token(kind: tkKeyword, pos: i + length, col: col, line: line, value: src[i..<i+length])
      inc i, length
    elif (let length = isIdentifier(src, i); length) > 0:
      result.add Token(kind: tkId, pos: i + length, col: col, line: line, value: src[i..<i+length])
      inc i, length
    else:
      inc i
  result.add Token(value: "\0", kind: tkEof, line: line, col: col)
  # {.noSideEffect.}:
  #   echo result
