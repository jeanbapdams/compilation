type ident = string

(* définition des types Java *)

type javaType = 
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
arrayType = javaType (*TODO brac*)

and
typeVariable = string

and
classOrInterfaceType = ClassType of classType | InterfaceType of interfaceType
and
classType = string
and
interfaceType = string

(* problème de compréhension de ce qu'est un classOrInterfaceType *)

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
    | ClassLiteral of javaType
    | This
    | ParExpr of expression
    | ClassInstanceCreation of javaType * expression list
    | ArrayInstanceCreation of javaType * dim list
    | FieldAccess of fieldAccess
    | MethodInvocation of methodInvocation
    | ArrayAccess of arrayAccess
and
fieldAccess =
    | ExprAcc of primaryExpression * ident
    | SuperAcc of ident
    | ClassSuperAcc of javaType * ident
and
arrayAccess = expression * expression
and
methodInvocation =
    | Invoc of ident * expression list
    | ExprInvoc of primaryExpression * ident * expression list
    | SuperInvoc of ident * expression list
    | ClassSuperInvoc of javaType * ident * expression list
    | TypeInvoc of javaType * ident * expression list
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
    | InstanceOf of relationalExpression * javaType
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

and fieldModifier = STATIC | FINAL | TRANSIENT | VOLATILE
and variableInitializer = expression
and variableDeclarator = 
    | ModTypeId of fieldModifier list*javaType *ident 
    | ModTypeIdInit of fieldModifier list*javaType * ident * variableInitializer

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


type methodDeclaration = {
	methodModifiers : methodModifier list;
	(*typeParameters : javaType list;*) (* cause des conflits shift/reduce avec resultType*)
	resultType : resultType;
	methodDeclarator : methodDeclarator;
	methodBody : methodBody
	}
and 
methodModifier = PUBLIC | PROTECTED | PRIVATE | ABSTRACT | STATIC | FINAL | SYNCHRONIZED | NATIVE | STRICTFP

and
resultType = TYPE of javaType | VOID

and 
methodDeclarator = ident * ((javaType * ident) list)

and 
methodBody = statement

	


type visibility = Public | Protected | Private


type normalClassDeclaration = {
	visibilityModifier: visibility; (*public by default*)
	classIdentifier: ident;
	typeParameters: javaType list;
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
    | MethodDeclaration of methodDeclaration
    (* | ClassDeclaration of normal_class
    | InterfaceDeclaration of interface*)
   

type classOrInterfaceDeclaration = normalClassDeclaration

type import = ident list

type package = Package of ident list | NoPackage

type program = {
	packageName: package;	
	importsList: import list;
	classOrInterface: classOrInterfaceDeclaration}





