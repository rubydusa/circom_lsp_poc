## Curcial
- [ ] Adjust hover char index because circom prerocesses (SEEMS TO BE WORKING JUST FINE WITHOUT, STRANGE BUT NOT COMPLAINING)

## Features
- [x] resolve TemplateOrFunction
- [x] take into account main component
- [x] work also outside of blocks
- [x] Add support for `access` (if a token is signal array, show it)
- [x] in case of template or function, generate docs
- [x] prettier hover messages

- [x] reimplement go to defintion (find decleration instead if symbol not a definition)
- [x] defintions should show params
- [x] signals should show tag information and signal type
- [ ] component types should show what component are they assigned to

## Bugs
- [x] resolve bug where things are shown as variable despite being signal
- [ ] resolve bug where if function / template is not included in final archive it is not reachable by `find_token`
- [ ] hover not showing in return statements
- [ ] hover not showing in accesses
- [x] definition gets docs of definition above it if it is single line comments
- [x] access signature of symbol should be derived only from its declaration, not any expression/ statement that includes access information
- [x] token type not foundable if template/function variable because there is no declaration

## Miscellaneous
- [x] rename StatementOrExpression to something more appropriate

## Backburner
- [ ] test tag support
- [ ] neovim lsp client shows extra spaces when escaping characters in result (hypothesis: the server under the hood returns the suspected amount of characters the message should take and server doesn't account for escapes)
