type ident = string

(* définition des types Java *)

type java_type = 
	| PrimitiveType of primitiveType
	| ReferenceType of referenceType

and
primitiveType =
	| NumericType of numericType
	| BOOL

and
numericType = 
	| IntegralType of integralType
	| FloatingPointType of floatingPointType

and
integralType = BYTE | SHORT | INT | LONG | CHAR

and
floatingPointType = FLOAT | DOUBLE


and
referenceType =
	| ClassOrInterfaceType of classOrInterfaceType
	| TypeVariable of typeVariable
	| ArrayType of arrayType


and
arrayType = java_type (*TODO brac*)

and
typeVariable = string

and
classOrInterfaceType = ClassType of classType | InterfaceType of interfaceType
and
classType = string
and
interfaceType = string

(* problème de compréhension de ce qu'est un classOrInterfaceType *)


type fieldModifier = STATIC | FINAL | TRANSIENT | VOLATILE
type variableInitializer = string (* more later maybe *)
type variableDeclarator = ModTypeId of fieldModifier list*java_type*ident | ModTypeIdInit of fieldModifier list*java_type * ident * variableInitializer

type visibility = Public | Protected | Private


type normalClassDeclaration = {
	visibilityModifier: visibility; (*public by default*)
	classIdentifier: ident;
	typeParameters: java_type list;
	super: classType; (*normal_class;*) (* extends *)
	interfaces: interfaceType list; (*interface list;*) 
	classBody: classBodyDeclaration list }
and
classBodyDeclaration = 
    | ClassMemberDeclaration of classMemberDeclaration
    (*| InstanceInitializer of instanceInitializer
    | StaticInitializer of staticInitializer
    | ConstructorDeclaration of constructorDeclaration*)
and
classMemberDeclaration =
    | FieldDeclaration of variableDeclarator
    (*| MethodDeclaration of methodDeclaration
    | ClassDeclaration of normal_class
    | InterfaceDeclaration of interface*)
    


type classOrInterfaceDeclaration = normalClassDeclaration

type import = ident list

type package = Package of ident list | NoPackage

type program = {
	packageName: package;	
	importsList: import list;
	classOrInterface: classOrInterfaceDeclaration}

type literal =
    | IntegerLiteral of string
    | RealLiteral of string
    | BoolLiteral of bool
    | StringLiteral of string
    | CharLiteral of string


(* pb de récursion : expression a besoin de primaryExpression qui a besoin de dims qui a besoin de expression ; d'où le "and" *)
type expression = 
    AssignmentExpression of assignmentExpression
and
primaryExpression =
    | Literal of literal
    | ClassLiteral of java_type
    | This
    | ParExpr of expression
    | ClassInstanceCreation of java_type * expression list
    | ArrayInstanceCreation of java_type * dim list
    | FieldAccess of fieldAccess
    | MethodInvocation of methodInvocation
    | ArrayAccess of arrayAccess
and
fieldAccess =
    | ExprAcc of primaryExpression * ident
    | SuperAcc of ident
    | ClassSuperAcc of java_type * ident
and
arrayAccess = expression * expression
and
methodInvocation =
    | Invoc of ident * expression list
    | ExprInvoc of primaryExpression * ident * expression list
    | SuperInvoc of ident * expression list
    | ClassSuperInvoc of java_type * ident * expression list
    | TypeInvoc of java_type * ident * expression list
and
dim =
    | Dim
    | DimExpr of expression
and
postfixExpression =
    | ExpressionName of expressionName
    | PostfixPrimaryExpression of primaryExpression
    | PostIncExpr of postfixExpression
    | PostDecExpr of postfixExpression
and
expressionName = ident
and
unaryExpression =
    | PreIncExpr of unaryExpression
    | PreDecExpr of unaryExpression
    | PlusExpr of unaryExpression
    | MinusExpr of unaryExpression
    | TildeExpr of unaryExpression
    | ExclamationExpr of unaryExpression
    | UnaryPostfixExpression of postfixExpression
and
multiplicativeExpression =
    | MultiplicativeUnaryExpression of unaryExpression
    | Mul of multiplicativeExpression * unaryExpression
    | Div of multiplicativeExpression * unaryExpression
    | Mod of multiplicativeExpression * unaryExpression
and
additiveExpression = 
    | AdditiveMultiplicativeExpression of multiplicativeExpression
    | Plus of additiveExpression * multiplicativeExpression
    | Minus of additiveExpression * multiplicativeExpression
and
shiftExpression =
    | ShiftAdditiveExpression of additiveExpression
    | ShiftTwoLeft of shiftExpression * additiveExpression
    | ShiftTwoRight of shiftExpression * additiveExpression
    | ShiftThreeLeft of shiftExpression * additiveExpression
    | ShiftThreeRight of shiftExpression * additiveExpression
and
relationalExpression =
    | RelationalShiftExpression of shiftExpression
    | Smaller of relationalExpression * shiftExpression
    | Greater of relationalExpression * shiftExpression
    | SmallerOrEqual of relationalExpression * shiftExpression
    | GreaterOrEqual of relationalExpression * shiftExpression
    | InstanceOf of relationalExpression * java_type
and
equalityExpression =
    | EqualityRelationalExpression of relationalExpression
    | Equal of equalityExpression * relationalExpression
    | NotEqual of equalityExpression * relationalExpression
and
logicalExpression =
    | LogicalEqualityExpression of equalityExpression
    | And of logicalExpression * logicalExpression
    | ExcOr of logicalExpression * logicalExpression
    | IncOr of logicalExpression * logicalExpression
    | DoubleAnd of logicalExpression * logicalExpression
    | DoubleOr of logicalExpression * logicalExpression
    | Conditional of logicalExpression * expression * expression
and
assignmentExpression =
    | AssignmentLogicalExpression of logicalExpression
    | Assignment of assignmentLeftHand * assignmentOperator * assignmentExpression
and
assignmentLeftHand = 
    | LeftHandExpressionName of expressionName
    | LeftHandFieldAccess of fieldAccess
    | LeftHandArrayAccess of arrayAccess
and
assignmentOperator =
    | EQUAL
    | MULEQUAL
    | DIVEQUAL
    | MODEQUAL
    | PLUSEQUAL
    | MINUSEQUAL
    | DOUBLECHEVRONLEFTEQUAL
    | TRIPLECHEVRONLEFTEQUAL
    | DOUBLECHEVRONRIGHTEQUAL
    | TRIPLECHEVRONRIGHTEQUAL

type statement =
    | EmptyStatement
    | LocalVariableDeclaration of variableDeclarator
    | Expression of expression
    | IfThen of expression * statement
    | IfThenElse of expression * statement * statement
    | Assert of expression
    | Switch of expression * statement list
    | While of expression * statement
    | Do of statement * expression
    | For of variableDeclarator * expression * expression * statement
    | Break
    | Continue
    | ReturnVoid
    | Return of expression
    | Block of statement list




