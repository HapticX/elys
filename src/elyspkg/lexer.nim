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
    tkUnknown  # otherwise
  Token* = object
    kind*: TokenKind
    value*: string


func `$`*(tk: Token): string =
  if tk.kind == tkSep:
    "(\\n, " & $tk.kind & ")"
  else:
    "(" & tk.value & ", " & $tk.kind & ")"


when defined(js):
  func tkn(src: string, m: var RegexMatch2, i: var int, kind: TokenKind): Token =
    let val = src[m.boundaries]
    result = Token(value: val, kind: kind)
    inc i, val.len
else:
  func tkn(src: string, m: var seq[string], i: var int, kind: TokenKind): Token =
    let val = m[0]
    result = Token(value: val, kind: kind)
    inc i, val.len


func parseString(source: string, symbol: char): Token =
  result = Token(value: $symbol, kind: tkString)
  var
    i = 0
    src = source[1..^1]
  for s in src:
    if s == symbol and i > 0 and src[i-1] == '\\':
      result.value &= s
    elif s == symbol:
      result.value &= s
      break
    else:
      result.value &= s
    inc i


func parseForTokens*(source: string): seq[Token] =
  result = @[]

  var
    i: int = 0
    src = source
  when defined(js):
    var m: RegexMatch2
  else:
    var m = newSeq[string](2)

  while src.len > 0:
    i = 0
    # One-line comments
    if src.findRe(re2"^(#[^\n]+)", m):
      let token = tkn(src, m, i, tkFloat)
    # Strings
    elif src[0] == '\'':
      result.add parseString(src, '\'')
      inc i, result[^1].value.len
    elif src[0] == '"':
      result.add parseString(src, '"')
      inc i, result[^1].value.len
    elif src[0] == '`':
      result.add parseString(src, '`')
      inc i, result[^1].value.len
    # Floats
    elif src.findRe(re2"^(\d+\.\d+)", m):
      result.add tkn(src, m, i, tkFloat)
    elif src.findRe(re2"^(\.\d+)", m):
      var token = tkn(src, m, i, tkFloat)
      token.value = "0" & token.value
      result.add token
    # Integers
    elif src.findRe(re2"^(\d+)", m):
      result.add tkn(src, m, i, tkInt)
    # Operators
    elif src.findRe(re2"^(>=|==|<=|!=|&&|\|\||\+\+|\-\-|//)", m):
      result.add tkn(src, m, i, tkOp)
    elif src.findRe(re2"^(\+=|\-=|//=|/=|\*=|&=|@=|\$=|\^=|\?=|%=)", m):
      result.add tkn(src, m, i, tkOp)
    elif src.findRe(re2"^(\b(not|or|and|in)\b)", m):
      result.add tkn(src, m, i, tkOp)
    elif src.findRe(re2"^([\.\+\-/\\,;:\[\]\(\)\{\}~!@#$%^|&?*=><])", m):
      result.add tkn(src, m, i, tkOp)
    # Boolean
    elif src.findRe(re2"^(\b(true|false|on|off)\b)", m):
      result.add tkn(src, m, i, tkBool)
    # Keywords
    elif src.findRe(re2"^(\b(if|elif|else|while|for|case|of|var|const|continue|break)\b)", m):
      result.add tkn(src, m, i, tkKeyword)
    elif src.findRe(re2"^(\b(print|null)\b)", m):
      result.add tkn(src, m, i, tkKeyword)
    # identifiers
    elif src.findRe(re2"^(\b[a-zA-Z][a-zA-Z0-9_]*\b)", m):
      result.add tkn(src, m, i, tkId)
    # separators
    # elif src.findRe(re2"^[\n;]", m):
    #   result.add tkn(src, m, i, tkSep)
    # Whitespaces
    elif src.findRe(re2"^(\s)", m):
      inc i
    else:
      inc i
    src = src[i..^1]
  result.add Token(value: "\0", kind: tkEof)
