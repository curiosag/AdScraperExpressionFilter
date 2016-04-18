grammar AdFilterExpression;

K_AND
:
	'&'
;

K_OR
:
	'|'
;

K_NOT
:
	'!'
;

K_EQUAL
:
	'=='
;

K_MATCHES
:
	M A T C H E S
;

K_PASSES
:
    P A S S E S
;

adFilterStmt
:
	expr
;

expr
:	
    
	| basicExpr
	| '(' expr ')'
	| logicalBinOperator
	| logicalBinExpr
;

logicalBinExpr
:
	basicExpr logicalBinExpr_
;


logicalBinExpr_
:
	logicalBinOperator expr
;

basicExpr
:
	 K_NOT expr
	| binOp
	| ternOp
	| K_PASSES'(' identifier ',' identifier ')'
;

binOp
:
	identifier binOperator numeric_literal
;

ternOp
:
	 numeric_literal binOperator identifier binOperator numeric_literal
;


logicalBinOperator
:
	K_AND
	| K_OR
;

binOperator
:
	LT
	| LT_EQ
	| GT
	| GT_EQ
	| EQ
	| K_MATCHES
;

literal
:
	numeric_literal
	| string_literal
;

identifier
:
	string_literal
;

numeric_literal
:
	NUMERIC_LITERAL
;

string_literal
:
	STRING_LITERAL
;

LT_EQ
:
	'<='
;

GT_EQ
:
	'>='
;

GT
:
	'>'
;

EQ
:
	'='
;

LT
:
	'<'
;

LPAR
:
	'('
;

RPAR
:
	')'
;

NUMERIC_LITERAL
:
	DIGIT+
	(
		'.' DIGIT*
	)?
	|
	(
		'+'
		| '-'
	) NUMERIC_LITERAL
;

STRING_LITERAL
:
	STRING
	| QUOTED_STRING
;

STRING
:
	(
		[a-zA-Z_0-9]
		| '-'
	)+ // TODO unicode support

;

QUOTED_STRING
:
	'\''
	(
		~'\''
		| '\'\''
	)* '\''
;

WHITESPACE
:
	[ \u000B\t\r\n] -> channel ( WHITESPACE )
;

fragment
DIGIT
:
	[0-9]
;

fragment
A
:
	[aA]
;

fragment
B
:
	[bB]
;

fragment
C
:
	[cC]
;

fragment
D
:
	[dD]
;

fragment
E
:
	[eE]
;

fragment
F
:
	[fF]
;

fragment
G
:
	[gG]
;

fragment
H
:
	[hH]
;

fragment
I
:
	[iI]
;

fragment
J
:
	[jJ]
;

fragment
K
:
	[kK]
;

fragment
L
:
	[lL]
;

fragment
M
:
	[mM]
;

fragment
N
:
	[nN]
;

fragment
O
:
	[oO]
;

fragment
P
:
	[pP]
;

fragment
Q
:
	[qQ]
;

fragment
R
:
	[rR]
;

fragment
S
:
	[sS]
;

fragment
T
:
	[tT]
;

fragment
U
:
	[uU]
;

fragment
V
:
	[vV]
;

fragment
W
:
	[wW]
;

fragment
X
:
	[xX]
;

fragment
Y
:
	[yY]
;

fragment
Z
:
	[zZ]
;

