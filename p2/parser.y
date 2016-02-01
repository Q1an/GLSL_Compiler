/* File: parser.y
 * --------------
 * Bison input file to generate the parser for the compiler.
 *
 * pp2: your job is to write a parser that will construct the parse tree
 *      and if no parse errors were found, print it.  The parser should 
 *      accept the language as described in specification, and as augmented 
 *      in the pp2 handout.
 */ 

%{

/* Just like lex, the text within this first region delimited by %{ and %}
 * is assumed to be C/C++ code and will be copied verbatim to the y.tab.c
 * file ahead of the definitions of the yyparse() function. Add other header
 * file inclusions or C++ variable declarations/prototypes that are needed
 * by your code here.
 */
#include "scanner.h" // for yylex
#include "parser.h"
#include "errors.h"

void yyerror(const char *msg); // standard error-handling routine





// struct Statement_list_type
// {
//   List<VarDecl*> *declarations;
//   List<Stmt*> *statements;
// };

%}
/*
%code requires {
}*/
//%error-verbose
/* The section before the first %% is the Definitions section of the yacc
 * input file. Here is where you declare tokens and types, add precedence
 * and associativity options, and so on.
 */
 
/* yylval 
 * ------
 * Here we define the type of the yylval global variable that is used by
 * the scanner to store attibute information about the token just scanned
 * and thus communicate that information to the parser. 
 *
 * pp2: You will need to add new fields to this union as you add different 
 *      attributes to your non-terminal symbols.
 */
%code requires {
struct Function_header_type
{
  Identifier *name;
  Type *type;
};

struct Function_header_with_parameters_type
{
  struct Function_header_type header;
  List<VarDecl*> *parameters;
};

struct Selection_rest_statement_type
{
  Stmt *body;
  Stmt *elseBody;
};

struct SwitchBody_type {
  List<Case*> *cases;  
  Default *def;
};

struct ForRest {
  Expr *test;
  Expr *step;
};


struct StmtList {
  List<VarDecl*> *decls;
  List<Stmt*> *stmts;
};
}


%union {
    int integerConstant;
    bool boolConstant;
    float floatConstant;
    char *stringConstant;
    double doubleConstant;
    char identifier[MaxIdentLen+1]; // +1 for terminating null
    char t_field_selection[MaxIdentLen+1];
    Identifier *identify;
    Decl *decl;
    VarDecl *vardecl;
    FnDecl *fndecl;
    List<Decl*> *declList;
    List<Type*> *typelist;
    //List<VarDecl*> *vardecl;
    IntConstant *intConst;
    Program *program;
    Expr *expr;
    Operator *opt;
    Type *type;
    Stmt *stmt;
    StmtBlock *stmtBlock;
    IfStmt *ifStmt;
    SwitchStmt *switchStmt;
    LoopStmt *loopStmt;
    ForStmt *forStmt;
    WhileStmt *whileStmt;
    AssignExpr *assignExpr;
    Case *switchcase;
    Default *switchdefault;
    List<Case*> *caseList;
    struct Function_header_type function_header_type;
    struct Function_header_with_parameters_type function_header_with_parameters_type;
    struct Selection_rest_statement_type selection_rest_statement_type;
    struct SwitchBody_type switchBody_type;
    struct ForRest forRest;
    struct StmtList stmtList;
    //struct Statement_list_type *statement_list_type
}


/* Tokens
 * ------
 * Here we tell yacc about all the token types that we are using.
 * Bison will assign unique numbers to these and export the #define
 * in the generated y.tab.h header file.
 */

%token   T_Void T_Bool T_Int T_Float 
%token   T_Vec2 T_Vec3 T_Vec4 T_Mat2 T_Mat3 T_Mat4
%token   T_LessEqual T_GreaterEqual T_Equal T_NotEqual T_Dims
%token   T_And T_Or 
%token   T_While T_For T_If T_Else T_Return T_Break T_Continue
%token   T_Inc T_Dec T_Switch T_Case T_Default T_Do
%token   T_Mul_Assign T_Div_Assign T_Add_Assign T_Sub_Assign
%token   <identifier> T_Identifier 
%token   <integerConstant> T_IntConstant
%token   <floatConstant> T_FloatConstant
%token   <boolConstant> T_BoolConstant
%token   T_Dot
%token   <identifier> T_Field_Selection 
%token   T_Mul T_Div T_Add T_Sub
%token   T_Left_Brace T_Right_Brace
%token   T_Colon T_Equal_Op T_Semicolon T_Dash T_Plus T_Star T_Slash
/* Non-terminal types
 * ------------------
 * In order for yacc to assign/access the correct field of $$, $1, we
 * must to declare which field is appropriate for the non-terminal.
 * As an example, this first type declaration establishes that the DeclList
 * non-terminal uses the field named "declList" in the yylval union. This
 * means that when we are setting $$ for a reduction for DeclList ore reading
 * $n which corresponds to a DeclList nonterminal we are accessing the field
 * of the union named "declList" which is of type List<Decl*>.
 * pp2: You'll need to add many of these of your own.
 */
//%type <declList>  DeclList 
%type <program>   Program
%type <identify>  variable_identifier
%type <expr>      primary_expression
%type <expr>      postfix_expression
%type <expr>      expression
//%type <expr>      integer_expression
%type <expr>      unary_expression
%type <opt>       unary_operator
%type <expr>      multiplicative_expression
%type <expr>      additive_expression
%type <expr>      shift_expression
%type <expr>      relational_expression
%type <expr>      equality_expression
%type <expr>      and_expression
%type <expr>      exclusive_or_expression
%type <expr>      inclusive_or_expression
%type <expr>      logical_and_expression
%type <expr>      logical_xor_expression
%type <expr>      logical_or_expression
%type <expr>      conditional_expression
%type <expr>      assignment_expression   
%type <expr>      initializer   
%type <opt>       assignment_operator
//%type <expr>      constant_expression
%type <decl>      declaration
%type <fndecl>    function_prototype
%type <fndecl>    function_declarator
%type <function_header_type>    function_header
%type <function_header_with_parameters_type>    function_header_with_parameters
%type <type>      type_specifier_nonarray
%type <type>      parameter_type_specifier
%type <vardecl>   single_declaration
%type <vardecl>   init_declarator_list
%type <vardecl>   parameter_declaration
%type <vardecl>   parameter_declarator
//%type <decl>      declaration_statement
%type <stmt>      statement
%type <stmt>      compound_statement_with_scope
%type <stmt>      simple_statement
%type <stmt>      statement_with_scope
%type <stmt>      statement_no_new_scope
%type <stmt>      compound_statement_no_new_scope
%type <expr>      expression_statement 
%type <stmtList>  statement_list
%type <ifStmt>    selection_statement
%type <selection_rest_statement_type>   selection_rest_statement
%type <expr> condition
%type <expr> conditionopt
%type <switchStmt> switch_statement
%type <stmtList>  switch_statement_list
%type <intConst> case_label
%type <switchcase> case_statement
%type <switchdefault> default_statement
%type <caseList> case_statement_list
%type <switchBody_type> switch_statement_body

%type <loopStmt> iteration_statement
%type <expr> for_init_statement
%type <forRest> for_rest_statement
%type <declList> translation_unit
%type <decl> external_declaration
%type <fndecl> function_definition


%nonassoc IF
%nonassoc T_Else

//%type <typelist>  type_specifier
//%type <typelist>  fully_specified_type


%%
/* Rules
 * -----
 * All productions and actions should be placed between the start and stop
 * %% markers which delimit the Rules section.
	 
 */
Program   :    translation_unit     { 
                                      @1; 
                                      /* pp2: The @1 is needed to convince 
                                       * yacc to set up yylloc. You can remove 
                                       * it once you have other uses of @n*/
                                      Program *program = new Program($1);
                                      // if no errors, advance to next phase
                                      if (ReportError::NumErrors() == 0) 
                                          program->Print(0);
                                    }
          ;
/*
DeclList  :    DeclList Decl        { ($$=$1)->Append($2); }
          |    Decl                 { ($$ = new List<Decl*>)->Append($1); }
          ;
*/

     
variable_identifier : T_Identifier { $$ = new Identifier(@1,$1); }
                    ;

primary_expression  : variable_identifier { $$ = new VarExpr(@1, $1); }
                    | T_IntConstant { $$ = new IntConstant(@1, $1); }
                    | T_FloatConstant { $$ = new FloatConstant(@1, $1); }
                    | T_BoolConstant { $$ = new BoolConstant(@1, $1); }
                    | '(' expression ')' { $$ = $2; }
                    ;
//VarExpr(yyltype loc, Identifier *ident);
postfix_expression  : primary_expression { $$=$1; }
                    | postfix_expression T_Dot T_Field_Selection { $$ = new FieldAccess($1, new Identifier(@3, $3)) ;} //maybe error
                    //| postfix_expression '.' T_Field_Selection { $$ = new VarExpr(@1, new Identifier(@3, $3)) ;}
                    | postfix_expression T_Inc {$$ = new PostfixExpr($1, new Operator(@2, "++"));}
                    | postfix_expression T_Dec {$$ = new PostfixExpr($1, new Operator(@2, "--"));}
                    ;

//integer_expression  : expression { $$ = $1; }
 //                   ;

//function_identifier : type_specifier {  ;}
//                    | postfix_expression {  ;}
//                    ;
// ArithmeticExpr(Operator *op, Expr *rhs)
unary_expression    : postfix_expression { $$ = $1; }
                    | T_Inc unary_expression { $$ = new ArithmeticExpr(new Operator(@1, "++"), $2);}
                    | T_Dec unary_expression { $$ = new ArithmeticExpr(new Operator(@1, "--"), $2);}
                    | unary_operator unary_expression { $$ = new ArithmeticExpr($1, $2);}
                    ;

unary_operator      : '+' { $$ = new Operator(@1,"+"); }
                    | '-' { $$ = new Operator(@1,"-"); }
                    ;
//ArithmeticExpr(Expr *lhs, Operator *op, Expr *rhs)
multiplicative_expression : unary_expression { $$ = $1 ;}
                          | multiplicative_expression '*' unary_expression { $$ = new ArithmeticExpr($1, new Operator(@2, "*"), $3) ;}    
                          | multiplicative_expression '/' unary_expression { $$ = new ArithmeticExpr($1, new Operator(@2, "/"), $3) ;}
                          ;

additive_expression       : multiplicative_expression { $$ = $1 ;}
                          | additive_expression '+' multiplicative_expression { $$ = new ArithmeticExpr($1, new Operator(@2, "+"), $3) ;}
                          | additive_expression '-' multiplicative_expression { $$ = new ArithmeticExpr($1, new Operator(@2, "-"), $3) ;}
                          ;

shift_expression          : additive_expression { $$ = $1 ;}
                          ;

relational_expression     : shift_expression { $$ = $1 ;}
                          | relational_expression '<' shift_expression { $$ = new RelationalExpr($1, new Operator(@2, "<"), $3); }
                          | relational_expression '>' shift_expression { $$ = new RelationalExpr($1, new Operator(@2, ">"), $3); }
                          | relational_expression T_LessEqual shift_expression { $$ = new RelationalExpr($1, new Operator(@2, "<="), $3); }
                          | relational_expression T_GreaterEqual shift_expression { $$ = new RelationalExpr($1, new Operator(@2, ">="), $3); }
                          ;

equality_expression       : relational_expression { $$ = $1; }
                          | equality_expression T_Equal relational_expression { $$ = new EqualityExpr($1,new Operator(@2,"=="),$3); }
                          | equality_expression T_NotEqual relational_expression { $$ = new EqualityExpr($1,new Operator(@2,"!="),$3); }
                          ;

and_expression : equality_expression { $$ = $1; }
               ;

exclusive_or_expression : and_expression { $$ = $1; }
                        ;

inclusive_or_expression : exclusive_or_expression { $$ = $1; }
                        ;

logical_and_expression : inclusive_or_expression { $$ = $1; }
                       | logical_and_expression T_And inclusive_or_expression { $$ = new LogicalExpr($1,new Operator(@2,"&&"),$3); }
                       ;

logical_xor_expression : logical_and_expression { $$ = $1; }
                       ;

logical_or_expression : logical_xor_expression  { $$ = $1; }
                      | logical_or_expression T_Or logical_xor_expression { $$ = new LogicalExpr($1,new Operator(@2,"||"),$3); }
                      ;

conditional_expression : logical_or_expression { $$ = $1; }
                       ;
//Call(yyltype loc, Expr *base, Identifier *field, List<Expr*> *args);
//Assign 
assignment_expression : conditional_expression { $$ = $1; }
                      | unary_expression assignment_operator conditional_expression { $$ = new AssignExpr($1,$2,$3); } //Identifier(yyltype loc, const char *name);
                      | unary_expression assignment_operator type_specifier_nonarray '(' conditional_expression ')' { 
  List<Expr*> *temp = new List<Expr*>();
  temp->Append($5);
$$ = new AssignExpr($1,$2, new Call(@5, NULL, new Identifier(@3, $3->returnName()) , temp)); }
                      ;

assignment_operator : '=' { $$ = new Operator(@1,"="); }
                    | T_Mul_Assign { $$ = new Operator(@1,"*="); }
                    | T_Div_Assign { $$ = new Operator(@1,"/="); }
                    | T_Add_Assign { $$ = new Operator(@1,"+="); }
                    | T_Sub_Assign { $$ = new Operator(@1,"-="); }
                    ;

expression : assignment_expression { $$ = $1; }
           ;

//constant_expression : conditional_expression { $$ = $1; }
  //                  ;

declaration : function_prototype ';' { $$ = $1; }
            | init_declarator_list { $$ = $1; }
            ;

function_prototype : function_declarator ')' { $$ = $1; }
                   ;

function_declarator : function_header { $$ = new FnDecl($1.name, $1.type, new List<VarDecl*>()); }
                    | function_header_with_parameters { $$ = new FnDecl($1.header.name, $1.header.type, $1.parameters); }
                    ;
//FnDecl(Identifier *name, Type *returnType, List<VarDecl*> *formals);
function_header_with_parameters : function_header parameter_declaration { $$.header = $1; ($$.parameters = new List<VarDecl*>())->Append($2); }
                                | function_header_with_parameters ',' parameter_declaration { $$ = $1; $$.parameters->Append($3); }
                                ;
//FnDecl(Identifier *name, Type *returnType, List<VarDecl*> *formals);
function_header : type_specifier_nonarray T_Identifier '(' { $$.name = new Identifier(@2, $2); $$.type = $1;}
                ;

parameter_declarator : type_specifier_nonarray T_Identifier {$$ = new VarDecl(new Identifier(@2, $2), $1);}
                     ;

parameter_declaration : parameter_declarator { $$ = $1; }
                      | parameter_type_specifier { $$ = new VarDecl(new Identifier(@1, ""), $1); }
                      ;

parameter_type_specifier : type_specifier_nonarray { $$ = $1; }
                         ;
 
init_declarator_list : single_declaration ';' { $$ = $1; }
                     | single_declaration '=' initializer ';'{ $$ = $1; }
                     ;

single_declaration : type_specifier_nonarray T_Identifier  { $$ = new VarDecl(new Identifier(@2, $2), $1); }
                   ;

/*fully_specified_type : type_specifier {$$ = $1}
                     ;

type_specifier : type_specifier_nonarray {($$ = new List<Type*>)->Append($1);}
               | type_specifier_nonarray array_specifier {($$=$1)->Append($2);}        
               ;
*/
type_specifier_nonarray : T_Void { $$ = Type::voidType; }
                        | T_Float{ $$ = Type::floatType;}
                        | T_Int  { $$ = Type::intType;  }
                        | T_Bool { $$ = Type::boolType; }
                        | T_Vec2 { $$ = Type::vec2Type; }
                        | T_Vec3 { $$ = Type::vec3Type; }
                        | T_Vec4 { $$ = Type::vec4Type; }
                        | T_Mat2 { $$ = Type::mat2Type; }
                        | T_Mat3 { $$ = Type::mat3Type; }
                        | T_Mat4 { $$ = Type::mat4Type; }
                        ;                                                              

initializer : assignment_expression { $$ = $1; }
            | '=' type_specifier_nonarray '(' assignment_expression ')' {$$ = $4;}
            ;
/*
declaration_statement : declaration { $$ = $1; } //maybe wrong
                      ;
*/
statement   : compound_statement_with_scope { $$ = $1; }
            | simple_statement { $$ = $1; }
            ;

statement_no_new_scope : compound_statement_no_new_scope { $$ = $1; }
                       | simple_statement { $$ = $1; }
                       ; 

statement_with_scope   : compound_statement_no_new_scope { $$ = $1; }
                       | simple_statement { $$ = $1; }
                       ; 

simple_statement : expression_statement { $$ = $1; }
                 | selection_statement { $$ = $1; }
                 | switch_statement { $$ = $1; }
                 //| case_label {  $$ = $1;  }
                 | iteration_statement { $$ = $1; }
                 ;

compound_statement_with_scope  : '{' '}' {$$ = new StmtBlock(new List<VarDecl*>(), new List<Stmt*>());} //$$ = new StmtBlock(new List<VarDecl*>(), new List<Stmt*>());
                               | '{' statement_list '}' { $$ = new StmtBlock($2.decls, $2.stmts); }
                               ;

compound_statement_no_new_scope  : '{' '}' { $$ = new StmtBlock(new List<VarDecl*>(), new List<Stmt*>());} //$$ = new StmtBlock(new List<VarDecl*>(), new List<Stmt*>());
                               | '{' statement_list '}' { $$ = new StmtBlock($2.decls, $2.stmts); }
                               ;

statement_list : statement { $$.decls = new List<VarDecl*>(); ($$.stmts = new List<Stmt*>())->Append($1); }
               | init_declarator_list { $$.stmts = new List<Stmt*>(); ($$.decls = new List<VarDecl*>())->Append($1); }
               | statement_list statement { $$ = $1; $$.stmts->Append($2); }
               | statement_list init_declarator_list { $$ = $1; $$.decls->Append($2); }
               ;

expression_statement   : ';' { $$ = new EmptyExpr(); }  
                       | expression ';' { $$ = $1; }    
                       ;

selection_statement    : T_If '(' expression ')' selection_rest_statement { $$ = new IfStmt($3, $5.body, $5.elseBody); }
                       ;



selection_rest_statement : statement_with_scope T_Else statement_with_scope { $$.body = $1; $$.elseBody = $3; } 
                         | statement_with_scope %prec IF { $$.body = $1; $$.elseBody = NULL; }                                                   
                         ;

condition    : expression { $$ = $1; }
             | type_specifier_nonarray T_Identifier T_Equal initializer { $$ = new AssignExpr(new VarExpr(@2, new Identifier(@2, $2)), new Operator(@3, "=") , $4); }
             ;

switch_statement : T_Switch '(' expression ')' '{' switch_statement_body '}' { $$ = new SwitchStmt($3, $6.cases, $6.def); }
                 ;

switch_statement_list : statement_list {$$ = $1;}
                      ;

case_label  : T_Case T_IntConstant ':' { $$ = new IntConstant(@2, $2); }
            ;

default_label : T_Default ':' {}
              ;

case_statement : case_label switch_statement_list { $$ = new Case($1, $2.stmts); }
               ;

default_statement : default_label switch_statement_list { $$ = new Default($2.stmts); }
                  ;

case_statement_list : case_statement { ($$ = new List<Case*>())->Append($1); }
                    | case_statement_list case_statement { ($$ = $1)->Append($2); }
                    ;

switch_statement_body : case_statement_list { $$.cases = $1; $$.def = NULL; }
                      | case_statement_list default_statement { $$.cases = $1; $$.def = $2; }
                      ;  


iteration_statement : T_While '(' condition ')' statement_no_new_scope { $$ = new WhileStmt($3, $5); }
                    | T_For '(' for_init_statement for_rest_statement ')' statement_no_new_scope { $$ = new ForStmt($3, $4.test, $4.step, $6); }
                    ;

for_init_statement : expression_statement {  $$ = $1;  }
                   //| declaration {  $$ = $1;  }
                   ;

conditionopt : condition {$$ = $1;}
             ;

for_rest_statement : conditionopt ';' { $$.test = $1; $$.step = NULL; }
                   | conditionopt ';' expression { $$.test = $1; $$.step = $3; }
                   ;

translation_unit :  translation_unit external_declaration { ($$ = $1)->Append($2); }
                 |  external_declaration { ($$ = new List<Decl*>)->Append($1); }
                 ;

external_declaration : function_definition { $$ = $1; }
                     | declaration { $$ = $1; }
                     ;

function_definition : function_prototype compound_statement_no_new_scope { ($$ = $1)->SetFunctionBody($2);  }
                    ;                                      



%%

/* The closing %% above marks the end of the Rules section and the beginning
 * of the User Subroutines section. All text from here to the end of the
 * file is copied verbatim to the end of the generated y.tab.c file.
 * This section is where you put definitions of helper functions.
 */

/* Function: InitParser
 * --------------------
 * This function will be called before any calls to yyparse().  It is designed
 * to give you an opportunity to do anything that must be done to initialize
 * the parser (set global variables, configure starting state, etc.). One
 * thing it already does for you is assign the value of the global variable
 * yydebug that controls whether yacc prints debugging information about
 * parser actions (shift/reduce) and contents of state stack during parser.
 * If set to false, no information is printed. Setting it to true will give
 * you a running trail that might be helpful when debugging your parser.
 * Please be sure the variable is set to false when submitting your final
 * version.
 */
void InitParser()
{
   PrintDebug("parser", "Initializing parser");
   yydebug = false;
}
