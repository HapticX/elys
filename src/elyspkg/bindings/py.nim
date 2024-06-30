import
  ../tools,
  nimpy


pyExportModule(
  "elys",
  doc="Other module docstring"
)

proc compileCode(code: string): void {.exportpy: "execute".} =
  discard compile(code, "$python")

proc compileFile(code, file: string): void {.exportpy: "compile_file".} =
  discard compile(code, file)
