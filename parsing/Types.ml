type ident = string
type fieldType = string (* more later maybe *)

type fieldModifier = STATIC | FINAL | TRANSIENT | VOLATILE
type variableInitializer = string (* more later maybe *)
type variableDeclarator = ModTypeId of fieldModifier list*fieldType*ident | ModTypeIdInit of fieldModifier list*fieldType * ident * variableInitializer

type visibility = Public | Protected | Private

type normal_class = {
	visibilityModifier: visibility; (*public by default*)
	classIdentifier: string;
	(*typeParameters: typeParameter list; *)
	super: string; (*normal_class;*) (* extends *)
	interfaces: string list; (*interface list;*) 
	classBody: string (*list*) }



type class_or_interface = normal_class

type import = string list

type package = Package of string list | NoPackage

type program = {
	package_name: package;	
	imports_list: import list;
	class_or_interface: class_or_interface}

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
    | ClassLiteral of string
    | This
    | ParExpr of expression
    | ClassInstanceCreation of ident * expression list
    | ArrayInstanceCreation of ident * dim list
    | FieldAccess of fieldAccess
    | MethodInvocation of methodInvocation
    | ArrayAccess of arrayAccess
and
fieldAccess =
    | ExprAcc of primaryExpression * ident
    | SuperAcc of ident
    | ClassSuperAcc of ident * ident
and
arrayAccess = expression * expression
and
methodInvocation =
    | Invoc of ident * expression list
    | ExprInvoc of primaryExpression * ident * expression list
    | SuperInvoc of ident * expression list
    | ClassSuperInvoc of ident * ident * expression list
    | TypeInvoc of ident * ident * expression list
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
    | InstanceOf of relationalExpression * ident
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
    | LeftHandFieldAccess of LeftHandfieldAccess
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
    | Expression of expression
    | IfThen of expression * statement
    | IfThenElse of expression * statement * statement
    | Assert of expression
    | Switch of expression * statement list
    | While of expression * statement
    | Do of statement * expression
(*  | FOR of ... *)
    | Break
    | Continue
    | Block of statement list
