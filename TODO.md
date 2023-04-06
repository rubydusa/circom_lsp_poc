## Curcial
- [ ] Adjust hover char index because circom prerocesses (SEEMS TO BE WORKING JUST FINE WITHOUT, STRANGE BUT NOT COMPLAINING)

## Features
- [x] resolve TemplateOrFunction
- [x] take into account main component
- [x] work also outside of blocks
- [x] Add support for `access` (if a token is signal array, show it)
- [ ] in case of template or function, generate docs
- [ ] prettier hover messages

- [ ] defintions should show params, templates show input and output signals

## Bugs
- [x] resolve bug where things are shown as variable despite being signal
- [ ] resolve bug where if function / template is not included in final archive it is not reachable by `find_token`
- [ ] hover not showing in return statements
- [ ] hover not showing in accesses

## Miscellaneous
- [x] rename StatementOrExpression to something more appropriate

## Backburner
- [ ] test tag support
