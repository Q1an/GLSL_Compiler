#GLSL Compiler Project
 
###Lexical Analysis
I mainly changed the codes in scanner.I and scanner.h to realize the lexical analysis.

The First part is to return Boolconstant, Intconstant and Floatconstant. Here we treat `-` as a unary operator so we only consider the positive number and zero. We also allow number start with zero. Then we consider the white space and depending on different white space our yycolumn and yylineno will change. Then we deal with punctuation, single-char operators, two-character operators and keywords. We use `C_COMMENT` to decide whether we are in a multi-line comments and report errors if any. Then we get the identifiers and report the error if it is too long. Lastly, we report whether there is any invalid characters.
