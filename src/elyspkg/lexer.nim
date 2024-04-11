when defined(js):
  import regex
  
  template findRe(s: string, p: Regex2, m: var RegexMatch2): bool =
    s.find(p, m)
else:
  import re

  template re2(x: varargs[untyped]): untyped =
    re(`x`)
  
  template findRe(s: string, p: Regex, m: var seq[string]): bool =
    s.find(p, m) != -1


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


func `$`*(tk: Token): string =
  if tk.kind == tkSep:
    "(\\n, " & $tk.kind & ")"
  else:
    "(" & tk.value & ", " & $tk.kind & ")"


when defined(js):
  func tkn(src: string, m: var RegexMatch2, i: var int, kind: TokenKind, line, col: int): Token =
    let val = src[m.boundaries]
    result = Token(value: val, line: line, col: col, kind: kind )
    inc i, val.len
else:
  func tkn(src: string, m: var seq[string], i: var int, kind: TokenKind, line, col: int): Token =
    let val = m[0]
    result = Token(value: val, line: line, col: col, kind: kind)
    inc i, val.len


func parseString(source: string, symbol: char, line, col: var int): tuple[t: Token, i: int] =
  var
    i = 0
    src = source[1..^1]
    r = Token(value: $symbol, kind: tkString, line: line, col: col)
  for s in src:
    if s == '\n':
      line += 1
      col = 1
    else:
      col += 1
    if s == symbol and i > 0 and src[i-1] == '\\':
      r.value = r.value[0..^2] & s
    elif s == symbol:
      r.value &= s
      inc i
      break
    else:
      r.value &= s
    inc i
  (t: r, i: i+1)


func parseForTokens*(source: string): seq[Token] =
  result = @[]

  var
    i: int = 0
    line: int = 1
    col: int = 1
    src = source
  when defined(js):
    var m: RegexMatch2
  else:
    var m = newSeq[string](2)

  while src.len > 0:
    i = 0
    # separators
    if src.findRe(re2"^([\n])", m):
      line += 1
      col = 1
    else:
      col += 1
    # One-line comments
    if src.findRe(re2"^(#[^\n]+)", m):
      discard tkn(src, m, i, tkComment, line, col)
    # Strings
    elif src[0] == '\'':
      let str = parseString(src, '\'', line, col)
      result.add str.t
      inc i, str.i
    elif src[0] == '"':
      let str = parseString(src, '"', line, col)
      result.add str.t
      inc i, str.i
    elif src[0] == '`':
      let str = parseString(src, '`', line, col)
      result.add str.t
      inc i, str.i
    # Floats
    elif src.findRe(re2"^(\d+\.\d+)", m):
      result.add tkn(src, m, i, tkFloat, line, col)
    elif src.findRe(re2"^(\.\d+)", m):
      var token = tkn(src, m, i, tkFloat, line, col)
      token.value = "0" & token.value
      result.add token
    # Integers
    elif src.findRe(re2"^(\d+)", m):
      result.add tkn(src, m, i, tkInt, line, col)
    # Operators
    elif src.findRe(re2"^(>=|==|<=|!=|&&|\|\||\+\+|\-\-|//)", m):
      result.add tkn(src, m, i, tkOp, line, col)
    elif src.findRe(re2"^(\+=|\-=|//=|/=|\*=|&=|@=|\$=|\^=|\?=|%=|\.\.<|\.\.)", m):
      result.add tkn(src, m, i, tkOp, line, col)
    elif src.findRe(re2"^(\b(not|or|and|in|of)\b)", m):
      result.add tkn(src, m, i, tkOp, line, col)
    elif src.findRe(re2"^([\.\+\-/\\,;:\[\]\(\)\{\}~!@#$%^|&?*=><])", m):
      result.add tkn(src, m, i, tkOp, line, col)
    # Boolean
    elif src.findRe(re2"^(\b(true|false|on|off)\b)", m):
      result.add tkn(src, m, i, tkBool, line, col)
    # Keywords
    elif src.findRe(re2"^(\b(fn|if|elif|else|while|for|case|var|const|continue|break)\b)", m):
      result.add tkn(src, m, i, tkKeyword, line, col)
    elif src.findRe(re2"^(\b(print|null)\b)", m):
      result.add tkn(src, m, i, tkKeyword, line, col)
    # identifiers
    elif src.findRe(re2"^(\b[a-zA-Z][a-zA-Z0-9_]*\b)", m):
      result.add tkn(src, m, i, tkId, line, col)
    # Whitespaces
    elif src.findRe(re2"^(\s)", m):
      inc i
    else:
      inc i
    src = src[i..^1]
  result.add Token(value: "\0", kind: tkEof, line: line, col: col)
