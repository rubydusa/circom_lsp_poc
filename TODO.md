## Curcial
- [ ] Adjust hover char index because circom prerocesses (SEEMS TO BE WORKING JUST FINE WITHOUT, STRANGE BUT NOT COMPLAINING)

## Features
- [x] resolve TemplateOrFunction
- [x] take into account main component
- [x] work also outside of blocks
- [x] Add support for `access` (if a token is signal array, show it)
- [x] in case of template or function, generate docs
- [x] prettier hover messages

## Bugs
- [x] resolve bug where things are shown as variable despite being signal
- [ ] resolve bug where if function / template is not included in final archive it is not reachable by `find_token`
- [ ] hover not showing in return statements
- [ ] hover not showing in accesses
- [x] definition gets docs of definition above it if it is single line comments

## Miscellaneous
- [x] rename StatementOrExpression to something more appropriate

## Backburner
- [ ] test tag support
- [ ] defintions should show params, templates show input and output signals
- [ ] neovim lsp client shows extra spaces when escaping characters in result (hypothesis: the server under the hood returns the suspected amount of characters the message should take and server doesn't account for escapes)
