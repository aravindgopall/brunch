# Approach Doc

## This tool is a compiler wrapper on top of `purescript` whose main idea is to save the compile time
## by compiling all the dependencies if the type of function changed in the file otherwise compile only the file that's changed


## Approach:
* Create Map with key as functionName and value as hash of function signature
* When there is a change in a file we recompute and check for hash mismatch for any function
  if there is no mismatch then compile only the current file
  else if there is a mismatch in function f then compile current file with all the dependencies with f
  else compile all the dependencies
* There is no optimization for first time compile as of now

