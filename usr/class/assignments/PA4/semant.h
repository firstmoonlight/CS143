#ifndef SEMANT_H_
#define SEMANT_H_

#include <assert.h>
#include <iostream> 
#include <unordered_map>
#include <vector> 
#include "cool-tree.h"
#include "stringtab.h"
#include "symtab.h"
#include "list.h"

#define TRUE 1
#define FALSE 0

class ClassTable;
typedef ClassTable *ClassTableP;

// This is a structure that may be used to contain the semantic
// information such as the inheritance graph.  You may use it or not as
// you like: it is only here to provide a container for the supplied
// methods.

class ClassTable {
private:
  int semant_errors;
  void install_basic_classes(Classes classes);
  ostream& error_stream;
  // map<parent, vector<child>>
  std::unordered_map<Symbol, std::vector<Symbol>> inherit_graph;
  // map<derived class, base class>
  std::unordered_map<Symbol, Symbol> der_2_base;
  std::unordered_map<Symbol, Class_> class_table;

public:
  ClassTable(Classes);
  int errors() { return semant_errors; }
  ostream& semant_error();
  ostream& semant_error(Class_ c);
  ostream& semant_error(Symbol filename, tree_node *t);

  Class_ get_class(Symbol s) {
    auto iter = class_table.find(s);
    if (iter == class_table.end()) {
        return NULL;
    }
    return iter->second;
  }

  void add_to_class_table(Class_ c);
  bool has_parent_defined(Classes classes);
  bool has_cycle();
  bool is_sub_class(Symbol s1, Symbol s2);
  Symbol find_lowest_common_ancestor(Symbol root, std::unordered_set<Symbol>& childs);
};

class Visitor {
public:
    virtual Symbol visit(program_class& cls) = 0;
    virtual Symbol visit(class__class& cls) = 0;
    virtual Symbol visit(method_class& cls) = 0;
    virtual Symbol visit(attr_class& cls) = 0;
    virtual Symbol visit(formal_class& cls) = 0;
    virtual Symbol visit(branch_class& cls) = 0;
    virtual Symbol visit(assign_class& cls) = 0;
    virtual Symbol visit(static_dispatch_class& cls) = 0;
    virtual Symbol visit(dispatch_class& cls) = 0;
    virtual Symbol visit(cond_class& cls) = 0;
    virtual Symbol visit(loop_class& cls) = 0;
    virtual Symbol visit(typcase_class& cls) = 0;
    virtual Symbol visit(block_class& cls) = 0;
    virtual Symbol visit(let_class& cls) = 0;
    virtual Symbol visit(plus_class& cls) = 0;
    virtual Symbol visit(sub_class& cls) = 0;
    virtual Symbol visit(mul_class& cls) = 0;
    virtual Symbol visit(divide_class& cls) = 0;
    virtual Symbol visit(neg_class& cls) = 0;
    virtual Symbol visit(lt_class& cls) = 0;
    virtual Symbol visit(eq_class& cls) = 0;
    virtual Symbol visit(leq_class& cls) = 0;
    virtual Symbol visit(comp_class& cls) = 0;
    virtual Symbol visit(int_const_class& cls) = 0;
    virtual Symbol visit(bool_const_class& cls) = 0;
    virtual Symbol visit(string_const_class& cls) = 0;
    virtual Symbol visit(new__class& cls) = 0;
    virtual Symbol visit(isvoid_class& cls) = 0;
    virtual Symbol visit(no_expr_class& cls) = 0;
    virtual Symbol visit(object_class& cls) = 0;
};

class TypeCheckVisitor : public Visitor {
 private:
    SymbolTable<Symbol, Symbol>* sym_table;
    using MethodTable = SymbolTable<Symbol, method_class>;
    std::unordered_map<Symbol, MethodTable> method_tables;
    ClassTable& cls_table;
    Class_ curr_class;

 public:
    TypeCheckVisitor(ClassTable& ct) : sym_table(new SymbolTable<Symbol, Symbol>()), cls_table(ct), curr_class(nullptr) {}
    ~TypeCheckVisitor() { delete sym_table; }
    method_class* get_closest_method(Class_ cls, Symbol method_name);
    void gather_attribute(Class_ cls);
    void gather_method(Class_ cls);

    Symbol visit(program_class& cls);
    Symbol visit(class__class& cls);
    Symbol visit(method_class& cls);
    Symbol visit(attr_class& cls);
    Symbol visit(formal_class& cls);
    Symbol visit(branch_class& cls);
    Symbol visit(assign_class& cls);
    Symbol visit(static_dispatch_class& cls);
    Symbol visit(dispatch_class& cls);
    Symbol visit(cond_class& cls);
    Symbol visit(loop_class& cls);
    Symbol visit(typcase_class& cls);
    Symbol visit(block_class& cls);
    Symbol visit(let_class& cls);
    Symbol visit(plus_class& cls);
    Symbol visit(sub_class& cls);
    Symbol visit(mul_class& cls);
    Symbol visit(divide_class& cls);
    Symbol visit(neg_class& cls);
    Symbol visit(lt_class& cls);
    Symbol visit(eq_class& cls);
    Symbol visit(leq_class& cls);
    Symbol visit(comp_class& cls);
    Symbol visit(int_const_class& cls);
    Symbol visit(bool_const_class& cls);
    Symbol visit(string_const_class& cls);
    Symbol visit(new__class& cls);
    Symbol visit(isvoid_class& cls);
    Symbol visit(no_expr_class& cls);
    Symbol visit(object_class& cls);
};
#endif

