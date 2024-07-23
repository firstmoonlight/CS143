//
// The following include files must come first.

#ifndef COOL_TREE_HANDCODE_H
#define COOL_TREE_HANDCODE_H

#include <iostream>
#include "tree.h"
#include "cool.h"
#include "stringtab.h"
#define yylineno curr_lineno;
extern int yylineno;

inline Boolean copy_Boolean(Boolean b) {return b; }
inline void assert_Boolean(Boolean) {}
inline void dump_Boolean(ostream& stream, int padding, Boolean b)
	{ stream << pad(padding) << (int) b << "\n"; }

void dump_Symbol(ostream& stream, int padding, Symbol b);
void assert_Symbol(Symbol b);
Symbol copy_Symbol(Symbol b);

class Program_class;
typedef Program_class *Program;
class Class__class;
typedef Class__class *Class_;
class Feature_class;
typedef Feature_class *Feature;
class Formal_class;
typedef Formal_class *Formal;
class Expression_class;
typedef Expression_class *Expression;
class Case_class;
typedef Case_class *Case;

typedef list_node<Class_> Classes_class;
typedef Classes_class *Classes;
typedef list_node<Feature> Features_class;
typedef Features_class *Features;
typedef list_node<Formal> Formals_class;
typedef Formals_class *Formals;
typedef list_node<Expression> Expressions_class;
typedef Expressions_class *Expressions;
typedef list_node<Case> Cases_class;
typedef Cases_class *Cases;

class Visitor;

#define Program_EXTRAS                          \
virtual void semant() = 0;			\
virtual void dump_with_types(ostream&, int) = 0;    \
virtual Symbol accept(Visitor& visitor) = 0;  \
virtual Classes get_classes() = 0; \



#define program_EXTRAS                          \
void semant();     				\
void dump_with_types(ostream&, int);    \
Symbol accept(Visitor& visitor) override;    \
Classes get_classes() { return classes; }

#define Class__EXTRAS                   \
virtual Symbol get_filename() = 0;      \
virtual void dump_with_types(ostream&,int) = 0; \
virtual Symbol accept(Visitor& visitor) = 0;      \
virtual Symbol get_name() const = 0;      \
virtual Symbol get_parent() const = 0;    \
virtual Features get_features() const = 0;    \


#define class__EXTRAS                                 \
Symbol get_filename() { return filename; }             \
void dump_with_types(ostream&,int);                     \
Symbol get_name() const override { return name; }               \
Symbol get_parent() const override { return parent; }           \
Features get_features() const override { return features; }    \
Symbol accept(Visitor& visitor) override;


#define Feature_EXTRAS                                        \
virtual void dump_with_types(ostream&,int) = 0;     \
virtual Symbol accept(Visitor& visitor) = 0;          \
virtual bool is_Method() = 0;           \
virtual bool is_Attr() = 0;


#define Feature_SHARED_EXTRAS                                       \
void dump_with_types(ostream&,int);    

#define method_EXTRAS           \
Symbol accept(Visitor& visitor) override;                 \
Symbol get_name() { return name; }             \
Symbol get_returntype() { return return_type; }      \
Formals get_formals() { return formals; }           \
Expression get_expression() { return expr; }  \
bool is_Method() override { return true; }              \
bool is_Attr() override { return false; }   


#define attr_EXTRAS             \
Symbol accept(Visitor& visitor) override;                 \
Symbol get_name() { return name; }             \
Symbol get_type() { return type_decl; }        \
Expression get_init() { return init; }         \
bool is_Method() override { return false; }             \
bool is_Attr() override { return true; }

#define Formal_EXTRAS                              \
virtual void dump_with_types(ostream&,int) = 0; \
virtual Symbol accept(Visitor& visitor) = 0;    \
virtual Symbol get_name() = 0;              \
virtual Symbol get_typedecl() = 0;  


#define formal_EXTRAS                           \
void dump_with_types(ostream&,int);     \
Symbol accept(Visitor& visitor) override;   \
Symbol get_name() override { return name; } \
Symbol get_typedecl() override { return type_decl; }


#define Case_EXTRAS                             \
virtual void dump_with_types(ostream& ,int) = 0;    \
virtual Symbol accept(Visitor& visitor) = 0;    \
virtual Symbol get_typedecl() = 0;


#define branch_EXTRAS                                   \
void dump_with_types(ostream& ,int);        \
Symbol accept(Visitor& visitor) override;   \
Symbol get_typedecl() { return type_decl; } \
Symbol get_name() { return name; }  \
Expression get_expr() { return expr; }


#define Expression_EXTRAS                    \
Symbol type;                                 \
Symbol get_type() { return type; }           \
Expression set_type(Symbol s) { type = s; return this; } \
virtual void dump_with_types(ostream&,int) = 0;  \
void dump_type(ostream&, int);               \
Expression_class() { type = (Symbol) NULL; }    \
virtual Symbol accept(Visitor& visitor) = 0;

#define Expression_SHARED_EXTRAS           \
void dump_with_types(ostream&,int); 

#define assign_EXTRAS       \
Symbol accept(Visitor& visitor) override;       \
Symbol get_name() { return name; }      \
Expression get_expression() { return expr; }
#define static_dispatch_EXTRAS      \
Symbol accept(Visitor& visitor) override;   \
Expression get_expression() { return expr; }    \
Symbol get_typename() { return type_name; }     \
Symbol get_name() { return name; }          \
Expressions get_actual() { return actual; }

#define dispatch_EXTRAS     \
Symbol accept(Visitor& visitor) override;   \
Expression get_expression() { return expr; }    \
Symbol get_name() { return name; }          \
Expressions get_actual() { return actual; }
#define cond_EXTRAS     \
Symbol accept(Visitor& visitor) override;   \
Expression get_pred() { return pred; }      \
Expression get_thenexpr() { return then_exp; }     \
Expression get_elseexpr() { return else_exp; }
#define loop_EXTRAS     \
Symbol accept(Visitor& visitor) override;   \
Expression get_pred() { return pred; }  \
Expression get_body() { return body; }
#define typcase_EXTRAS      \
Symbol accept(Visitor& visitor) override;   \
Expression get_expr() { return expr; }  \
Cases get_cases() { return cases; }
#define block_EXTRAS        \
Symbol accept(Visitor& visitor) override;   \
Expressions get_body() { return body; }
#define let_EXTRAS      \
Symbol accept(Visitor& visitor) override;   \
Symbol get_identifier() { return identifier; } \
Symbol get_typedecl() { return type_decl; } \
Expression get_init() { return init; }  \
Expression get_body() { return body; }
#define plus_EXTRAS     \
Symbol accept(Visitor& visitor) override;   \
Expression get_lhs() { return e1; }     \
Expression get_rhs() { return e2; }
#define sub_EXTRAS      \
Symbol accept(Visitor& visitor) override;   \
Expression get_lhs() { return e1; }     \
Expression get_rhs() { return e2; }
#define mul_EXTRAS      \
Symbol accept(Visitor& visitor) override;   \
Expression get_lhs() { return e1; }     \
Expression get_rhs() { return e2; }
#define divide_EXTRAS       \
Symbol accept(Visitor& visitor) override;   \
Expression get_lhs() { return e1; }     \
Expression get_rhs() { return e2; }
#define neg_EXTRAS      \
Symbol accept(Visitor& visitor) override;   \
Expression get_expr() { return e1; }
#define lt_EXTRAS       \
Symbol accept(Visitor& visitor) override;   \
Expression get_lhs() { return e1; }     \
Expression get_rhs() { return e2; }
#define eq_EXTRAS       \
Symbol accept(Visitor& visitor) override;   \
Expression get_lhs() { return e1; }     \
Expression get_rhs() { return e2; }
#define leq_EXTRAS      \
Symbol accept(Visitor& visitor) override;   \
Expression get_lhs() { return e1; }     \
Expression get_rhs() { return e2; }
#define comp_EXTRAS     \
Symbol accept(Visitor& visitor) override;   \
Expression get_expr() { return e1; }
#define int_const_EXTRAS        \
Symbol accept(Visitor& visitor) override;
#define bool_const_EXTRAS       \
Symbol accept(Visitor& visitor) override;
#define string_const_EXTRAS     \
Symbol accept(Visitor& visitor) override;
#define new__EXTRAS     \
Symbol accept(Visitor& visitor) override;   \
Symbol get_typename() { return type_name; }
#define isvoid_EXTRAS       \
Symbol accept(Visitor& visitor) override;   \
Expression get_expr() { return e1; }
#define no_expr_EXTRAS      \
Symbol accept(Visitor& visitor) override;
#define object_EXTRAS       \
Symbol accept(Visitor& visitor) override;\
Symbol get_name() { return name;}

#endif
