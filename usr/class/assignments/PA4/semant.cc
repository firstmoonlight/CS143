

#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>
#include <unordered_set>
#include <functional>
#include "semant.h"
#include "utilities.h"


extern int semant_debug;
extern char *curr_filename;

//////////////////////////////////////////////////////////////////////
//
// Symbols
//
// For convenience, a large number of symbols are predefined here.
// These symbols include the primitive type and method names, as well
// as fixed names used by the runtime system.
//
//////////////////////////////////////////////////////////////////////
static Symbol 
    arg,
    arg2,
    Bool,
    concat,
    cool_abort,
    copy,
    Int,
    in_int,
    in_string,
    IO,
    length,
    Main,
    main_meth,
    No_class,
    No_type,
    Object,
    out_int,
    out_string,
    prim_slot,
    self,
    SELF_TYPE,
    Str,
    str_field,
    substr,
    type_name,
    val;
//
// Initializing the predefined symbols.
//
static void initialize_constants(void)
{
    arg         = idtable.add_string("arg");
    arg2        = idtable.add_string("arg2");
    Bool        = idtable.add_string("Bool");
    concat      = idtable.add_string("concat");
    cool_abort  = idtable.add_string("abort");
    copy        = idtable.add_string("copy");
    Int         = idtable.add_string("Int");
    in_int      = idtable.add_string("in_int");
    in_string   = idtable.add_string("in_string");
    IO          = idtable.add_string("IO");
    length      = idtable.add_string("length");
    Main        = idtable.add_string("Main");
    main_meth   = idtable.add_string("main");
    //   _no_class is a symbol that can't be the name of any 
    //   user-defined class.
    No_class    = idtable.add_string("_no_class");
    No_type     = idtable.add_string("_no_type");
    Object      = idtable.add_string("Object");
    out_int     = idtable.add_string("out_int");
    out_string  = idtable.add_string("out_string");
    prim_slot   = idtable.add_string("_prim_slot");
    self        = idtable.add_string("self");
    SELF_TYPE   = idtable.add_string("SELF_TYPE");
    Str         = idtable.add_string("String");
    str_field   = idtable.add_string("_str_field");
    substr      = idtable.add_string("substr");
    type_name   = idtable.add_string("type_name");
    val         = idtable.add_string("_val");
}



ClassTable::ClassTable(Classes classes) : semant_errors(0) , error_stream(cerr) {

    // create and insert the built-in class, like "Object" etc
    install_basic_classes(classes);

    // class can only be defined in program, so there is no nested class
    for(int i = classes->first(); classes->more(i); i = classes->next(i)) {
        add_to_class_table(classes->nth(i));
    }

    /* check whether Main class is defined. */
    if (class_table.find(Main) == class_table.end()) {
        semant_error() << "Class Main is not defined." << std::endl;
    }
}

void ClassTable::install_basic_classes(Classes classes) {

    // The tree package uses these globals to annotate the classes built below.
   // curr_lineno  = 0;
    Symbol filename = stringtable.add_string("<basic class>");
    
    // The following demonstrates how to create dummy parse trees to
    // refer to basic Cool classes.  There's no need for method
    // bodies -- these are already built into the runtime system.
    
    // IMPORTANT: The results of the following expressions are
    // stored in local variables.  You will want to do something
    // with those variables at the end of this method to make this
    // code meaningful.

    // 
    // The Object class has no parent class. Its methods are
    //        abort() : Object    aborts the program
    //        type_name() : Str   returns a string representation of class name
    //        copy() : SELF_TYPE  returns a copy of the object
    //
    // There is no need for method bodies in the basic classes---these
    // are already built in to the runtime system.

    Class_ Object_class =
	class_(Object, 
	       No_class,
	       append_Features(
			       append_Features(
					       single_Features(method(cool_abort, nil_Formals(), Object, no_expr())),
					       single_Features(method(type_name, nil_Formals(), Str, no_expr()))),
			       single_Features(method(copy, nil_Formals(), SELF_TYPE, no_expr()))),
	       filename);

    // 
    // The IO class inherits from Object. Its methods are
    //        out_string(Str) : SELF_TYPE       writes a string to the output
    //        out_int(Int) : SELF_TYPE            "    an int    "  "     "
    //        in_string() : Str                 reads a string from the input
    //        in_int() : Int                      "   an int     "  "     "
    //
    Class_ IO_class = 
	class_(IO, 
	       Object,
	       append_Features(
			       append_Features(
					       append_Features(
							       single_Features(method(out_string, single_Formals(formal(arg, Str)),
										      SELF_TYPE, no_expr())),
							       single_Features(method(out_int, single_Formals(formal(arg, Int)),
										      SELF_TYPE, no_expr()))),
					       single_Features(method(in_string, nil_Formals(), Str, no_expr()))),
			       single_Features(method(in_int, nil_Formals(), Int, no_expr()))),
	       filename);  

    //
    // The Int class has no methods and only a single attribute, the
    // "val" for the integer. 
    //
    Class_ Int_class =
	class_(Int, 
	       Object,
	       single_Features(attr(val, prim_slot, no_expr())),
	       filename);

    //
    // Bool also has only the "val" slot.
    //
    Class_ Bool_class =
	class_(Bool, Object, single_Features(attr(val, prim_slot, no_expr())),filename);

    //
    // The class Str has a number of slots and operations:
    //       val                                  the length of the string
    //       str_field                            the string itself
    //       length() : Int                       returns length of the string
    //       concat(arg: Str) : Str               performs string concatenation
    //       substr(arg: Int, arg2: Int): Str     substring selection
    //       
    Class_ Str_class =
	class_(Str, 
	       Object,
	       append_Features(
			       append_Features(
					       append_Features(
							       append_Features(
									       single_Features(attr(val, Int, no_expr())),
									       single_Features(attr(str_field, prim_slot, no_expr()))),
							       single_Features(method(length, nil_Formals(), Int, no_expr()))),
					       single_Features(method(concat, 
								      single_Formals(formal(arg, Str)),
								      Str, 
								      no_expr()))),
			       single_Features(method(substr, 
						      append_Formals(single_Formals(formal(arg, Int)), 
								     single_Formals(formal(arg2, Int))),
						      Str, 
						      no_expr()))),
	       filename);

    add_to_class_table(Object_class);
    add_to_class_table(IO_class);
    add_to_class_table(Int_class);
    add_to_class_table(Bool_class);
    add_to_class_table(Str_class);
}

////////////////////////////////////////////////////////////////////
//
// semant_error is an overloaded function for reporting errors
// during semantic analysis.  There are three versions:
//
//    ostream& ClassTable::semant_error()                
//
//    ostream& ClassTable::semant_error(Class_ c)
//       print line number and filename for `c'
//
//    ostream& ClassTable::semant_error(Symbol filename, tree_node *t)  
//       print a line number and filename
//
///////////////////////////////////////////////////////////////////

ostream& ClassTable::semant_error(Class_ c)
{                                                             
    return semant_error(c->get_filename(),c);
}    

ostream& ClassTable::semant_error(Symbol filename, tree_node *t)
{
    error_stream << filename << ":" << t->get_line_number() << ": ";
    return semant_error();
}

ostream& ClassTable::semant_error()                  
{                                                 
    semant_errors++;                            
    return error_stream;
} 

// start
// add class to classtable
void ClassTable::add_to_class_table(Class_ c) {
    Symbol name = c->get_name();
    Symbol parent = c->get_parent();
    if ((parent == Bool) || (parent == Str) || (parent == SELF_TYPE)){
        semant_error(c) << name << "Can't inherent from " << parent << "!" << endl; 
    }else if (name == SELF_TYPE){
        semant_error(c) << "Can't define SELF_TYPE!" << endl;
    }else if ((class_table.find(name) != class_table.end())){
        semant_error(c) << "Can't be defined multiple times!" << endl;
    }else{
        class_table[name] = c;
        inherit_graph[parent].emplace_back(name);
        der_2_base[name] = parent;
    }
}

bool ClassTable::has_parent_defined(Classes classes) {
    // check whether parent has been defined
    for (int i = classes->first(); classes->more(i); i = classes->next(i)) {
        Symbol parent = classes->nth(i)->get_parent();
        if (class_table.find(parent) == class_table.end()) {
            // parent has not been defined
            semant_error(classes->nth(i)) << "Parent class " << parent->get_string() << " is not defined" << endl;
            return false;
        }
    }
    return true;
}

bool ClassTable::has_cycle() {
    // check cycle, from the Object
    std::unordered_set<Symbol> visited;
    std::function<bool(Symbol)> dfs = [&](Symbol cls) -> bool {
        for (auto child : inherit_graph[cls]) {
            if (visited.find(child) != visited.end()) {
                return true;
            }
            visited.insert(child);
            dfs(child);
        }

        return false;
    };

    visited.insert(Object);
    return dfs(Object);
}

bool ClassTable::is_sub_class(Symbol s1, Symbol s2) {
    if (s2 == Object){
        return true;
    }

    while (der_2_base.find(s1) != der_2_base.end()){
        if (s1 == s2){
            return true;
        }
        s1 = der_2_base[s1];
    }

    return false;
}

Symbol ClassTable::find_lowest_common_ancestor(Symbol root, std::unordered_set<Symbol>& childs) {
    if (childs.find(root) != childs.end()) return root;

    Symbol ans = nullptr;
    for(auto* child : inherit_graph[root]) {
        Symbol t = find_lowest_common_ancestor(child, childs);
        if (ans != nullptr && t != nullptr) {
            return root;
        }
        if (t != nullptr) {
            ans = t;
        }
    }

    return ans;
}

#define VISIT_VISITOR return visitor.visit(*this)
Symbol program_class::accept(Visitor& visitor) {
    VISIT_VISITOR;
}
Symbol class__class::accept(Visitor& visitor) {
    VISIT_VISITOR;
}		
Symbol method_class::accept(Visitor& visitor) {
    VISIT_VISITOR;
}
Symbol attr_class::accept(Visitor& visitor) {
    VISIT_VISITOR;
}
Symbol formal_class::accept(Visitor& visitor) {
    VISIT_VISITOR;
}
Symbol branch_class::accept(Visitor& visitor) {
    VISIT_VISITOR;
}
Symbol assign_class::accept(Visitor& visitor) {
    VISIT_VISITOR;
}
Symbol static_dispatch_class::accept(Visitor& visitor) {
    VISIT_VISITOR;
}
Symbol dispatch_class::accept(Visitor& visitor) {
    VISIT_VISITOR;
}
Symbol cond_class::accept(Visitor& visitor) {
    VISIT_VISITOR;
}
Symbol loop_class::accept(Visitor& visitor) {
    VISIT_VISITOR;
}
Symbol typcase_class::accept(Visitor& visitor) {
    VISIT_VISITOR;
}
Symbol block_class::accept(Visitor& visitor) {
    VISIT_VISITOR;
}
Symbol let_class::accept(Visitor& visitor) {
    VISIT_VISITOR;
}
Symbol plus_class::accept(Visitor& visitor) {
    VISIT_VISITOR;
}
Symbol sub_class::accept(Visitor& visitor) {
    VISIT_VISITOR;
}
Symbol mul_class::accept(Visitor& visitor) {
    VISIT_VISITOR;
}
Symbol divide_class::accept(Visitor& visitor) {
    VISIT_VISITOR;
}
Symbol neg_class::accept(Visitor& visitor) {
    VISIT_VISITOR;
}
Symbol lt_class::accept(Visitor& visitor) {
    VISIT_VISITOR;
}
Symbol eq_class::accept(Visitor& visitor) {
    VISIT_VISITOR;
}
Symbol leq_class::accept(Visitor& visitor) {
    VISIT_VISITOR;
}
Symbol comp_class::accept(Visitor& visitor) {
    VISIT_VISITOR;
}
Symbol int_const_class::accept(Visitor& visitor) {
    VISIT_VISITOR;
}
Symbol bool_const_class::accept(Visitor& visitor) {
    VISIT_VISITOR;
}
Symbol string_const_class::accept(Visitor& visitor) {
    VISIT_VISITOR;
}
Symbol new__class::accept(Visitor& visitor) {
    VISIT_VISITOR;
}
Symbol isvoid_class::accept(Visitor& visitor) {
    VISIT_VISITOR;
}
Symbol no_expr_class::accept(Visitor& visitor) {
    VISIT_VISITOR;
}
Symbol object_class::accept(Visitor& visitor) {
    VISIT_VISITOR;
}

void TypeCheckVisitor::gather_attribute(Class_ cls) {
    if (cls == NULL) return;
    Class_ parentCls = cls_table.get_class(cls->get_parent());
    gather_attribute(parentCls);
    const Features fs = cls->get_features();
    for (int i = fs->first(); fs->more(i); i = fs->next(i)) {
        if (fs->nth(i)->is_Method()) continue;

        attr_class* attr = static_cast<attr_class*>(fs->nth(i));
        if (attr->get_name() == self) {
            cls_table.semant_error(cls) << "'self' cannot be the name of an attribute in class " << cls->get_name() << endl;
        }

        Symbol fname = attr->get_name();
        Symbol ftype = attr->get_type();
        // check whether fname has been defined in current scope
        if (sym_table->probe(fname) == NULL) {
            sym_table->addid(fname, new Symbol(ftype));
        } else {
            cls_table.semant_error(cls) << fname << " Can't defined multiple times!" << endl;
        }
    }
}

method_class* TypeCheckVisitor::get_closest_method(Class_ cls, Symbol method_name) {
    while (cls) {
        method_class* method = method_tables[cls->get_name()].probe(method_name);
        // get the closest parent method, which has the same name with current method
        if (method != NULL) {
            return method;
        }
        cls = cls_table.get_class(cls->get_parent());
    }

    return NULL;
}

void TypeCheckVisitor::gather_method(Class_ cls) {
    if (cls == NULL) return;
    if (method_tables.find(cls->get_name()) != method_tables.end()) return;
    Class_ parentCls = cls_table.get_class(cls->get_parent());
    gather_method(parentCls);
    method_tables[cls->get_name()].enterscope();
    const Features fs = cls->get_features();
    for (int i = fs->first(); fs->more(i); i = fs->next(i)) {
        if (fs->nth(i)->is_Attr()) continue;

        method_class* mc = static_cast<method_class*>(fs->nth(i));
        Symbol fname = mc->get_name();
        method_tables[cls->get_name()].addid(fname, mc);

        // check for return type
        Symbol return_type = mc->get_returntype();
        if (cls_table.get_class(return_type) == NULL && return_type != SELF_TYPE) {
            cls_table.semant_error(cls->get_filename(), mc) << "Error! return type " << return_type << " doesn't exist." << endl;
        }

        // check for illegal method overriding
        method_class* method = get_closest_method(parentCls, mc->get_name());

        if (method != NULL) {
            // a method is found，then check foraml parameters and return type
            Formals fms1 = mc->get_formals();
            Formals fms2 = method->get_formals();
            // check formal parameter length
            if (fms1->len() != fms2->len()) {
                cls_table.semant_error(cls->get_filename(), mc) << "Method override error: length of formals not match." << endl;
                continue;
            }

            int k1 = fms1->first(), k2 = fms2->first();
            for (; fms1->more(k1) && fms2->more(k2); k1 = fms1->next(k1), k2 = fms2->next(k2)) {
                if ((fms1->nth(k1)->get_typedecl() != fms2->nth(k2)->get_typedecl())) {
                    cls_table.semant_error(cls->get_filename(), mc) << "Method override error: formal type not match." << endl;
                }
            }
        }

    }
}

///////////////////////////////////////////////////////////////////
//type_check start
//////////////////////////////////////////////////////////////////
Symbol TypeCheckVisitor::visit(program_class& cls) {
    Classes classes = cls.get_classes();
    // because method can only be declared in classed, and can be referenced any where
    // so there is only one method scope for each class, and the scope will not be exit utill the program is end
    for(int i = classes->first(); classes->more(i); i = classes->next(i)) {
        gather_method(classes->nth(i));
    }
    for(int i = classes->first(); classes->more(i); i = classes->next(i)) {
        sym_table->enterscope();
        curr_class = classes->nth(i);
        gather_attribute(classes->nth(i));
        Symbol cur_class_name = curr_class->get_name();
        sym_table->addid(self, new Symbol(cur_class_name));
        classes->nth(i)->accept(*this);
        sym_table->exitscope();
    }

    return NULL;
}

Symbol TypeCheckVisitor::visit(class__class& cls) {
    Features fs = cls.get_features();
    for (int i = fs->first(); fs->more(i); i = fs->next(i)) {
        fs->nth(i)->accept(*this);
    }
    return NULL;
}

Symbol TypeCheckVisitor::visit(attr_class& cls) {
    // because we have add all attr to scope by gather_attribute, so we just check for init
    Symbol init_type = cls.get_init()->accept(*this);
    if (init_type != No_type) {
        if (init_type == SELF_TYPE) init_type = curr_class->get_name();
        if(!cls_table.is_sub_class(init_type, cls.get_type())) {
            cls_table.semant_error(curr_class->get_filename(), &cls) << "Error! decl type is not ancestor of init type." << endl;
        }
    }
    return NULL;
}

Symbol TypeCheckVisitor::visit(method_class& cls) {
    // just check for expression
    sym_table->enterscope();
    Formals fs = cls.get_formals();
    for (int i = fs->first(); fs->more(i); i = fs->next(i)) {
        Symbol type_decl = fs->nth(i)->get_typedecl();
        sym_table->addid(fs->nth(i)->get_name(), new Symbol(type_decl));
    }
    Symbol s = cls.get_expression()->accept(*this);
    if (!cls_table.is_sub_class(s, cls.get_returntype())) {
        cls_table.semant_error(curr_class->get_filename(), &cls) << "Error! return type is not ancestor of method body." << endl;
    }
    sym_table->exitscope();

    return NULL;
}

Symbol TypeCheckVisitor::visit(formal_class& cls) {
    // check for duplicated formal parameters and formal parameter type
    Symbol formal_name = cls.get_name();
    Symbol formal_type = cls.get_typedecl();
    if (cls_table.get_class(formal_type) == NULL) {
        cls_table.semant_error(curr_class->get_filename(), &cls) << "Error! formal type " << formal_type << " can not be found." << endl;
    } else if (sym_table->probe(formal_name) != NULL) {
        cls_table.semant_error(curr_class->get_filename(), &cls) << "Error! formal name " << formal_name << " duplicated." << endl;
    } else if (formal_name == self) {
        cls_table.semant_error(curr_class->get_filename(), &cls) << "Error! formal name should not be self." << endl;
    } else {
        sym_table->addid(formal_name, new Symbol(formal_type));
    }

    return NULL;
}

// check the type of OBJECTID
Symbol TypeCheckVisitor::visit(object_class& cls) {
    Symbol name = cls.get_name();
    Symbol type;

    Symbol* find_type = sym_table->lookup(name);
    if (NULL == find_type) {
        cls_table.semant_error(curr_class->get_filename(), &cls) << "Use undefined identifier " << name << endl;
        type = Object;
    } else {
        
        type = *find_type;
    }
    

    return cls.set_type(type)->get_type();
}

Symbol TypeCheckVisitor::visit(no_expr_class& cls) {
    return No_type;
}

Symbol TypeCheckVisitor::visit(isvoid_class& cls) {
    cls.get_expr()->accept(*this);
    return cls.set_type(Bool)->get_type();
}

Symbol TypeCheckVisitor::visit(new__class& cls) {
    Symbol type_name = cls.get_typename();
    Symbol type = type_name;
    if (type_name != SELF_TYPE && NULL == cls_table.get_class(type_name)) {
        cls_table.semant_error(curr_class->get_filename(), &cls) << "Cannot find object " << type_name << endl;
        type = Object;
    }
    return cls.set_type(type)->get_type();
}

Symbol TypeCheckVisitor::visit(string_const_class& cls) {
    return cls.set_type(Str)->get_type();
}

Symbol TypeCheckVisitor::visit(int_const_class& cls) {
    return cls.set_type(Int)->get_type();
}

Symbol TypeCheckVisitor::visit(bool_const_class& cls) {
    return cls.set_type(Bool)->get_type();
}

Symbol TypeCheckVisitor::visit(comp_class& cls) {
    Expression e = cls.get_expr();
    if (e->accept(*this) != Bool) {
        cls_table.semant_error(curr_class->get_filename(), &cls) << "Error! 'not' meets non-Bool value." << endl;
    }

    return cls.set_type(Bool)->get_type();
}

Symbol TypeCheckVisitor::visit(leq_class& cls) {
    Symbol lhs_type = cls.get_lhs()->accept(*this);
    Symbol rhs_type = cls.get_rhs()->accept(*this);
    if (lhs_type != Int || rhs_type != Int) {
        cls_table.semant_error(curr_class->get_filename(), &cls) << "Error! '<=' meets non-Int value." << endl;
    }

    return cls.set_type(Bool)->get_type();;
}

Symbol TypeCheckVisitor::visit(eq_class& cls) {
    Symbol lhs_type = cls.get_lhs()->accept(*this);
    Symbol rhs_type = cls.get_rhs()->accept(*this);
    if (lhs_type == Int || lhs_type == Bool || lhs_type == Str || rhs_type == Int || rhs_type == Bool || rhs_type == Str) {
        cls_table.semant_error(curr_class->get_filename(), &cls) << "Error! '=' meets Int or Bool or Str value." << endl;
    }

    return cls.set_type(Bool)->get_type();;
}

Symbol TypeCheckVisitor::visit(lt_class& cls) {
    Symbol lhs_type = cls.get_lhs()->accept(*this);
    Symbol rhs_type = cls.get_rhs()->accept(*this);
    if (lhs_type != Int || rhs_type != Int) {
        cls_table.semant_error(curr_class->get_filename(), &cls) << "Error! '<' meets non-Int value." << endl;
    }

    return cls.set_type(Bool)->get_type();
}

Symbol TypeCheckVisitor::visit(neg_class& cls) {
    Symbol lhs_type = cls.get_expr()->accept(*this);
    if (lhs_type != Int) {
        cls_table.semant_error(curr_class->get_filename(), &cls) << "Error! '~' meets non-Int value." << endl;
    }

    return cls.set_type(Int)->get_type();
}

Symbol TypeCheckVisitor::visit(plus_class& cls) {
    Symbol lhs_type = cls.get_lhs()->accept(*this);
    Symbol rhs_type = cls.get_rhs()->accept(*this);
    if (lhs_type != Int || rhs_type != Int) {
        cls_table.semant_error(curr_class->get_filename(), &cls) << "Error! '+' meets non-Int value." << endl;
    }

    return cls.set_type(Int)->get_type();
}

Symbol TypeCheckVisitor::visit(sub_class& cls) {
    Symbol lhs_type = cls.get_lhs()->accept(*this);
    Symbol rhs_type = cls.get_rhs()->accept(*this);
    if (lhs_type != Int || rhs_type != Int) {
        cls_table.semant_error(curr_class->get_filename(), &cls) << "Error! '-' meets non-Int value." << endl;
    }

    return cls.set_type(Bool)->get_type();
}

Symbol TypeCheckVisitor::visit(mul_class& cls) {
    Symbol lhs_type = cls.get_lhs()->accept(*this);
    Symbol rhs_type = cls.get_rhs()->accept(*this);
    if (lhs_type != Int || rhs_type != Int) {
        cls_table.semant_error(curr_class->get_filename(), &cls) << "Error! '*' meets non-Int value." << endl;
    }

    return cls.set_type(Bool)->get_type();
}

Symbol TypeCheckVisitor::visit(divide_class& cls) {
    Symbol lhs_type = cls.get_lhs()->accept(*this);
    Symbol rhs_type = cls.get_rhs()->accept(*this);
    if (lhs_type != Int || rhs_type != Int) {
        cls_table.semant_error(curr_class->get_filename(), &cls) << "Error! '/' meets non-Int value." << endl;
    }

    return cls.set_type(Bool)->get_type();
}

Symbol TypeCheckVisitor::visit(let_class& cls) {
    Symbol identifier = cls.get_identifier();
    if (identifier == self) {
        cls_table.semant_error(curr_class->get_filename(), &cls) << "Error! self in let binding." << std::endl;
    }

    Symbol init_type = cls.get_init()->accept(*this);
    Symbol type_decl = cls.get_typedecl();
    if (init_type != No_type && !cls_table.is_sub_class(init_type, type_decl)) {
        cls_table.semant_error(curr_class->get_filename(), &cls) << "Error! init type '" << init_type 
                            << "' is not a derived class of type_decl '" << type_decl << "'." << std::endl;
    }

    sym_table->enterscope();
    sym_table->addid(identifier, new Symbol(type_decl));
    Symbol type = cls.get_body()->accept(*this);
    sym_table->exitscope();

    return cls.set_type(type)->get_type();
}

Symbol TypeCheckVisitor::visit(block_class& cls) {
    Expressions exprs = cls.get_body();
    Symbol type = NULL;
    for (int i = exprs->first(); exprs->more(i); i = exprs->next(i)) {
        type = exprs->nth(i)->accept(*this);
    }

    return cls.set_type(type)->get_type();
}

Symbol TypeCheckVisitor::visit(typcase_class& cls) {
    // check expression
    cls.get_expr()->accept(*this);

    std::unordered_set<Symbol> types;
    Cases cases = cls.get_cases();
    for (int i = cases->first(); cases->more(i); i = cases->next(i)) {
        Symbol type_decl = cases->nth(i)->get_typedecl();
        if (types.find(type_decl) != types.end()) {
            cls_table.semant_error(curr_class->get_filename(), cases->nth(i)) <<
                                 "Error! type '" << type_decl << "' duplicated." << std::endl;
        }
        Symbol type = cases->nth(i)->accept(*this);
        types.insert(type);
    }

    Symbol ancestorType = cls_table.find_lowest_common_ancestor(Object, types);
    return cls.set_type(ancestorType)->get_type();
}

Symbol TypeCheckVisitor::visit(branch_class& cls) {
    sym_table->enterscope();
    Symbol name = cls.get_name(), typedecl = cls.get_typedecl();
    sym_table->addid(name, new Symbol(typedecl));
    Symbol type = cls.get_expr()->accept(*this);
    sym_table->exitscope();

    return type;
}

Symbol TypeCheckVisitor::visit(loop_class& cls) {
    Symbol pred_type = cls.get_pred()->accept(*this);
    if (pred_type != Bool) {
        cls_table.semant_error(curr_class->get_filename(), &cls) <<
                        "Error! In 'Loop', the pred type should be Bool '" << endl;
    }
    cls.get_body()->accept(*this);

    return cls.set_type(Object)->get_type();
}

Symbol TypeCheckVisitor::visit(cond_class& cls) {
    Symbol pred_type = cls.get_pred()->accept(*this);
    if (pred_type != Bool) {
        cls_table.semant_error(curr_class->get_filename(), &cls) <<
                        "Error! In 'If', the pred type should be Bool '" << endl;
    }

    // no need to check for no_type, because there is always else
    Symbol t1 = cls.get_thenexpr()->accept(*this);
    Symbol t2 = cls.get_elseexpr()->accept(*this);
    if (t1 == SELF_TYPE) {
        t1 = curr_class->get_name();
    }
    if (t2 == SELF_TYPE) {
        t2 = curr_class->get_name();
    }
    std::unordered_set<Symbol> types = {t1, t2};
    Symbol ancestor = cls_table.find_lowest_common_ancestor(Object, types);

    return cls.set_type(ancestor)->get_type();
}

Symbol TypeCheckVisitor::visit(dispatch_class& cls) {
    Symbol expr_type = cls.get_expression()->accept(*this);

    if (expr_type == SELF_TYPE) {
        expr_type = curr_class->get_name();
    }

    auto it = method_tables.find(expr_type);
    if (it == method_tables.end()) {
        cls_table.semant_error(curr_class->get_filename(), &cls) << "Error! there is no class named " << expr_type << "." << endl;
        return cls.set_type(Object)->get_type();
    }

    method_class* method = get_closest_method(cls_table.get_class(expr_type), cls.get_name());
    if (method == NULL) {
        cls_table.semant_error(curr_class->get_filename(), &cls) << "Error! there is no method " << cls.get_name() << " in class " 
        << expr_type << "." << endl;
        return cls.set_type(Object)->get_type();
    }

    // check for actual parameter with formal parameters
    Expressions exprs = cls.get_actual();
    Formals formals = method->get_formals();
    if (exprs->len() != formals->len()) {
        cls_table.semant_error(curr_class->get_filename(), &cls) << "Error! the length of actual parameter is not match with formal parameters" << endl;
        return cls.set_type(Object)->get_type();
    }

    for (int i = exprs->first(); exprs->more(i); i = exprs->next(i)) {
        Symbol expr_type = exprs->nth(i)->accept(*this);
        if (!cls_table.is_sub_class(expr_type, formals->nth(i)->get_typedecl())) {
            cls_table.semant_error(curr_class->get_filename(), &cls) << "Error! " << i << "'th actual parameter is not match with formal parameters" << endl;
            return cls.set_type(Object)->get_type();
        }
    }

    Symbol type = method->get_returntype();
    if (type == SELF_TYPE) {
        type = expr_type;
    }

    return cls.set_type(type)->get_type();
}

Symbol TypeCheckVisitor::visit(static_dispatch_class& cls) {
  Symbol expr_type = cls.get_expression()->accept(*this);

    if (expr_type == SELF_TYPE) {
        expr_type = curr_class->get_name();
    }

    if (!cls_table.is_sub_class(expr_type, cls.get_typename())) {
        cls_table.semant_error(curr_class->get_filename(), &cls) << "Error! static dispatch class is not an ancestor." << endl;
        return cls.set_type(Object)->get_type();
    }

    auto it = method_tables.find(cls.get_typename());
    if (it == method_tables.end()) {
        cls_table.semant_error(curr_class->get_filename(), &cls) << "Error! there is no class named " << expr_type << "." << endl;
        return cls.set_type(Object)->get_type();
    }

    method_class* method = get_closest_method(cls_table.get_class(expr_type), cls.get_name());
    if (method == NULL) {
        cls_table.semant_error(curr_class->get_filename(), &cls) << "Error! there is no method " << cls.get_name() << " in class " 
        << expr_type << "." << endl;
        return cls.set_type(Object)->get_type();
    }

    // check for actual parameter with formal parameters
    Expressions exprs = cls.get_actual();
    Formals formals = method->get_formals();
    if (exprs->len() != formals->len()) {
        cls_table.semant_error(curr_class->get_filename(), &cls) << "Error! the length of actual parameter is not match with formal parameters" << endl;
        return cls.set_type(Object)->get_type();
    }

    for (int i = exprs->first(); exprs->more(i); i = exprs->next(i)) {
        Symbol expr_type = exprs->nth(i)->accept(*this);
        if (!cls_table.is_sub_class(expr_type, formals->nth(i)->get_typedecl())) {
            cls_table.semant_error(curr_class->get_filename(), &cls) << "Error! " << i << "'th actual parameter is not match with formal parameters" << endl;
            return cls.set_type(Object)->get_type();
        }
    }

    Symbol type = method->get_returntype();
    if (type == SELF_TYPE) {
        type = expr_type;
    }

    return cls.set_type(type)->get_type();
}

Symbol TypeCheckVisitor::visit(assign_class& cls) {
    Symbol* type = sym_table->lookup(cls.get_name());
    if (type == NULL) {
        cls_table.semant_error(curr_class->get_filename(), &cls) << "Error! " << cls.get_name() << " is undefined." << endl;
        return cls.set_type(Object)->get_type();
    }

    Symbol expr_type = cls.get_expression()->accept(*this);
    if (!cls_table.is_sub_class(expr_type, *type)) {
        cls_table.semant_error(curr_class->get_filename(), &cls) << "Error! " << *type << " is not the ancestor of the rhs expression." << endl;
        return cls.set_type(Object)->get_type();
    }

    return cls.set_type(expr_type)->get_type();
}

/*   This is the entry point to the semantic checker.

     Your checker should do the following two things:

     1) Check that the program is semantically correct
     2) Decorate the abstract syntax tree with type information
        by setting the `type' field in each Expression node.
        (see `tree.h')

     You are free to first do 1), make sure you catch all semantic
     errors. Part 2) can be done in a second stage, when you want
     to build mycoolc.
 */
void program_class::semant()
{
    initialize_constants();

    /* ClassTable constructor may do some semantic analysis */
    ClassTable *classtable = new ClassTable(classes);

    if (!classtable->errors() && !classtable->has_cycle()) {
        /* some semantic analysis code may go here */
        TypeCheckVisitor tc_visitor(*classtable);
        accept(tc_visitor);
    }

    if (classtable->errors()) {
	cerr << "Compilation halted due to static semantic errors." << endl;
	exit(1);
    }
}


