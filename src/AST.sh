#!/bin/sh

abstract() {
  class "$1" "$2" "abstract"
}

class() {  
  # The classname is the string before the : on the first line
  classname=`echo "$1" | sed -n '1 s/ *:.*$//p'`
  # The parent's class name is the string after the : on the first line
  parent=`echo "$1" | sed -n '1 s/^.*: *//p'` ; # String after :
  # The fields come from the second line through the first empty line
  # Each is the identifier just before the semicolon
  # Lines with "typedef" are skipped
  fields=`echo "$1" | sed '/typedef/d' | sed -n '2,/^\$/ s/^.*[^a-zA-Z0-9_]\([a-zA-Z0-9_]*\);.*/\1/p'`
  # The body for the header file starts at the second line
  hppbody=`echo "$1" | sed -n '2,$p'`

  # Any additional methods are defined in the second argument

  #echo "[$classname]"
  #echo "[$parent]"
  #echo "[$fields]"
  #echo "[$hppbody]"

  forwarddefs="$forwarddefs
  class $classname;"

  # Define a default (zero-argument) constructor if one isn't already
  # defined in the body
  if (echo $hppbody | grep -q "$classname()"); then
    defaultconstructor=
  else
    defaultconstructor="$classname() {}
"    
  fi

  if test -z "$3"; then
      visitorclassdefs="$visitorclassdefs
  virtual Status visit($classname& n) { assert(0); return Status(); }"
      welcome="
    IRCLASSDEFS;
  public:
    Status welcome(Visitor&);"
      welcomedef="
  IRCLASS($classname);
  Status $classname::welcome(Visitor &v) { return v.visit(*this); }"    
  else
    welcome="public:"
    welcomedef=
  fi

  classdefs="$classdefs

  class $classname : public $parent {
    $welcome
    $copyme
    void read(XMListream &);
    void write(XMLostream &) const;
    $defaultconstructor
$hppbody
  };
"

  if test -n "$fields"; then
    writefields=`echo $fields | sed "s/ / << /g"`;
    writefields="
    w << $writefields;"
    readfields=`echo $fields | sed "s/ / >> /g"`;
    readfields="
    r >> $readfields;"
  else
    readfields=
    writefields=
  fi
  
  methoddefs="$methoddefs

  void $classname::read(XMListream &r) {
    $parent::read(r); $readfields
  }

  void $classname::write(XMLostream &w) const {
    $parent::write(w); $writefields
  }
$welcomedef
  $2
  "
}

abstract "ASTNode : Node


  virtual Status welcome(Visitor&) = 0;
"
abstract "Symbol : ASTNode
  string name;

  Symbol(string s) : name(s) {}"
class "ModuleSymbol : Symbol
  Module *module;

  ModuleSymbol(string s) : Symbol(s), module(0) {}"
abstract "ValuedSymbol : Symbol
  TypeSymbol *type;
  Expression *initializer;

  ValuedSymbol(string n, TypeSymbol *t, Expression *e)
    : Symbol(n), type(t), initializer(e) {}"
class "VariableSymbol : ValuedSymbol

  VariableSymbol(string n, TypeSymbol *t, Expression *e)
    : ValuedSymbol(n, t, e) {}"

class "ConstantSymbol : VariableSymbol

  ConstantSymbol(string n, TypeSymbol *t, Expression *i)
     : VariableSymbol(n, t, i) {}"

class "BuiltinConstantSymbol : ConstantSymbol

  BuiltinConstantSymbol(string n, TypeSymbol *t, Expression *i)
     : ConstantSymbol(n, t, i) {}"
class "SignalSymbol : ValuedSymbol
  typedef enum { Input,Output,Inputoutput,Sensor,Return,Local,Trap,Unknown } kinds;
  int kind;
  FunctionSymbol *combine; // combining function, if any
  SignalSymbol *reincarnation; // the signal this is a reincarnation of, if any

  SignalSymbol(string n, TypeSymbol *t, kinds k, FunctionSymbol *f,
               Expression *e, SignalSymbol *r)
    : ValuedSymbol(n, t, e), kind(k), combine(f), reincarnation(r) {}
"
class "BuiltinSignalSymbol : SignalSymbol

  BuiltinSignalSymbol(string n, TypeSymbol *t, kinds k, FunctionSymbol *f)
    : SignalSymbol(n, t, k, f, NULL, NULL) {}"
class "TypeSymbol : Symbol

   TypeSymbol(string s) : Symbol(s) {}"
class "BuiltinTypeSymbol : TypeSymbol

   BuiltinTypeSymbol(string s) : TypeSymbol(s) {}"
class "FunctionSymbol : TypeSymbol
  vector<TypeSymbol*> arguments;
  TypeSymbol *result;

  FunctionSymbol(string s) : TypeSymbol(s), result(NULL) {}"
class "BuiltinFunctionSymbol : FunctionSymbol

  BuiltinFunctionSymbol(string s) : FunctionSymbol(s) {}"
class "ProcedureSymbol : TypeSymbol
  vector<TypeSymbol*> reference_arguments;
  vector<TypeSymbol*> value_arguments;

  ProcedureSymbol(string s) : TypeSymbol(s) {}"
class "TaskSymbol : ProcedureSymbol

  TaskSymbol(string s) : ProcedureSymbol(s) {}"
class "SymbolTable : ASTNode
  SymbolTable *parent;
  typedef vector<Symbol*> stvec;
  stvec symbols;

  SymbolTable() : parent(NULL) {}

  class const_iterator {
    stvec::const_iterator i;
  public:
    const_iterator(stvec::const_iterator ii) : i(ii) {}
    void operator ++(int) { i++; } // int argument denotes postfix
    void operator ++() { ++i; } // int argument denotes postfix
    bool operator !=(const const_iterator &ii) { return i != ii.i; }
    Symbol *operator *() { return *i; }
  };

  const_iterator begin() const { return const_iterator(symbols.begin()); }
  const_iterator end() const { return const_iterator(symbols.end()); }
  size_t size() const { return symbols.size(); }
  void clear() { symbols.clear(); }

  bool local_contains(const string) const;
  bool contains(const string) const;
  void enter(Symbol *);
  Symbol* get(const string);
" "
bool SymbolTable::local_contains(const string s) const {
  for ( stvec::const_iterator i = symbols.begin() ; i != symbols.end() ; i++ ) {
    assert(*i);
    if ( (*i)->name == s) return true;
  }
  return false;
}

bool SymbolTable::contains(const string s) const {
  for ( const SymbolTable *st = this ; st ; st = st->parent )
    if (st->local_contains(s)) return true;
  return false;
}

void SymbolTable::enter(Symbol *sym) {
  assert(sym);
  assert(!local_contains(sym->name));
  symbols.push_back( sym );
}

Symbol* SymbolTable::get(const string s) {
  for ( SymbolTable *st = this; st ; st = st->parent ) {
    for ( const_iterator i = st->begin() ; i != st->end() ; i++ )
      if ( (*i)->name == s) return *i;
  }
  assert(0); // get should not be called unless contains returned true
}
"
abstract "Expression : ASTNode
  TypeSymbol *type;

  Expression(TypeSymbol *t) : type(t) {}"

class "Literal : Expression
  string value;

  Literal(string v, TypeSymbol *t) : Expression(t), value(v) {}"
class "LoadVariableExpression : Expression
  VariableSymbol *variable;

  LoadVariableExpression(VariableSymbol *v)
    : Expression(v->type), variable(v) {}"
class "LoadSignalExpression : Expression
  SignalSymbol *signal;

  LoadSignalExpression(TypeSymbol *t, SignalSymbol *s)
    : Expression(t), signal(s) {}"
class "LoadSignalValueExpression : Expression
  SignalSymbol *signal;
  
  LoadSignalValueExpression(SignalSymbol *s)
    : Expression(s->type), signal(s) {}"

class "UnaryOp : Expression
  string op;
  Expression *source;

  UnaryOp(TypeSymbol *t, string s, Expression *e)
    : Expression(t), op(s), source(e) {}"
class "BinaryOp : Expression
  string op;
  Expression *source1;
  Expression *source2;

  BinaryOp(TypeSymbol *t, string s, Expression *e1, Expression *e2)
    : Expression(t), op(s), source1(e1), source2(e2) {}"  
class "FunctionCall : Expression
  FunctionSymbol *callee;
  vector<Expression*> arguments;

  FunctionCall(FunctionSymbol *s)
    : Expression(s->result), callee(s) {}"
class "Delay : Expression
  Expression *predicate;
  Expression *count;
  bool is_immediate;
  Counter *counter;

  Delay(TypeSymbol *t, Expression *e1, Expression *e2,
         bool i, Counter *c)
   : Expression(t), predicate(e1), count(e2), is_immediate(i), counter(c) {}"
class "CheckCounter : Expression
  Counter *counter;
  Expression *predicate;

  CheckCounter(TypeSymbol *t, Counter *c, Expression *p )
    : Expression(t), counter(c), predicate(p) {}
"
class "Module : ASTNode
  ModuleSymbol *symbol;
  SymbolTable *types;
  SymbolTable *constants;
  SymbolTable *functions;
  SymbolTable *procedures;
  SymbolTable *tasks;
  SymbolTable *signals;
  SymbolTable *variables;
  vector<Counter*> counters;
  vector<InputRelation*> relations;
  ASTNode *body;
  
  Module() {}
  Module(ModuleSymbol *);
  ~Module();    
" "
Module::Module(ModuleSymbol *s) : symbol(s), body(NULL) {
  signals = new SymbolTable();
  constants = new SymbolTable();
  types = new SymbolTable();
  functions = new SymbolTable();
  procedures = new SymbolTable();
  tasks = new SymbolTable();
  variables = new SymbolTable();
}

Module::~Module() {
   delete signals;
   delete types;
   delete constants;
   delete functions;
   delete procedures;
   delete tasks;
   delete body;
   delete variables;
}"
abstract "InputRelation : ASTNode"

class "Exclusion : InputRelation
  vector<SignalSymbol *> signals;"

class "Implication : InputRelation
  SignalSymbol *predicate;
  SignalSymbol *implication;

  Implication(SignalSymbol *ss1, SignalSymbol*ss2)
    : predicate(ss1), implication(ss2) {}"
class "Counter : ASTNode"
class "Modules : ASTNode
  SymbolTable module_symbols;
  vector<Module*> modules;

  void add(Module*);
" "
void Modules::add(Module* m) {
  assert(m);
  assert(m->symbol);
  assert(!module_symbols.contains(m->symbol->name));
  modules.push_back(m);
  module_symbols.enter(m->symbol);
}"
abstract "Statement : ASTNode"
abstract "BodyStatement : Statement
  Statement *body;

  BodyStatement(Statement *s) : body(s) {}"
class "PredicatedStatement : BodyStatement
  Expression *predicate;

  PredicatedStatement(Statement *s, Expression *e)
    : BodyStatement(s), predicate(e) {}"
abstract "CaseStatement : Statement
  vector<PredicatedStatement *> cases;
  Statement *default_stmt;

  CaseStatement() : default_stmt(0) {}
  PredicatedStatement *newCase(Statement *s, Expression *e) {
    PredicatedStatement *ps = new PredicatedStatement(s, e);
    cases.push_back(ps);
    return ps;
  }"
class "StatementList : Statement
  vector<Statement *> statements;

  StatementList& operator <<(Statement *s) {
    assert(s);
    statements.push_back(s);
    return *this;
  }"
class "ParallelStatementList : Statement 
  vector<Statement *> threads;"
class "Nothing : Statement"
class "Pause : Statement"
class "Halt : Statement"

class "Emit : Statement
  SignalSymbol *signal;
  Expression *value;
  bool unknown;

  Emit(SignalSymbol *s, Expression *e)
    : signal(s), value(e), unknown(false) {}"

class "Exit : Statement
  SignalSymbol *trap;
  Expression *value;

  Exit(SignalSymbol *t, Expression *e) : trap(t), value(e) {}"

class "Sustain : Emit

  Sustain(SignalSymbol *s, Expression *e) : Emit(s, e) {}"

class "Assign : Statement
  VariableSymbol *variable;
  Expression *value;

  Assign(VariableSymbol *v, Expression *e) : variable(v), value(e) {}"
class "ProcedureCall : Statement
  ProcedureSymbol *procedure;
  vector<VariableSymbol*> reference_args;
  vector<Expression*> value_args;

  ProcedureCall(ProcedureSymbol *ps) : procedure(ps) {}"
class "Present : CaseStatement"
class "If : CaseStatement"
class "Loop : BodyStatement

  Loop(Statement *s) : BodyStatement(s) {}"

class "Repeat : Loop
  Expression *count;
  bool is_positive;
  Counter *counter;

  Repeat(Statement *s, Expression *e, bool p, Counter *c)
    : Loop(s), count(e), is_positive(p), counter(c) {}"
class "Abort : CaseStatement
  Statement *body;
  bool is_weak;

  Abort(Statement *s, bool i) : body(s), is_weak(i) {}
  Abort(Statement *s, Expression *e, Statement *s1)
    : body(s), is_weak(false) {
    newCase(s1, e);
  }"

class "Await : CaseStatement"

class "LoopEach : PredicatedStatement

  LoopEach(Statement *s, Expression *e) : PredicatedStatement(s, e) {}"

class "Every : PredicatedStatement

  Every(Statement *s, Expression *e) : PredicatedStatement(s, e) {}"

class "Suspend : PredicatedStatement

  Suspend(Statement *s, Expression *e) : PredicatedStatement(s, e) {}"

class "DoWatching : PredicatedStatement
  Statement *timeout;

  DoWatching(Statement *s1, Expression *e, Statement *s2)
    : PredicatedStatement(s1, e), timeout(s2) {}"

class "DoUpto : PredicatedStatement

  DoUpto(Statement *s, Expression *e) : PredicatedStatement(s, e) {}"
class "TaskCall : ProcedureCall
  SignalSymbol *signal;
  Statement *body;

  TaskCall(TaskSymbol *ts) : ProcedureCall(ts), signal(0), body(0) {}
"

class "Exec : Statement
  vector <TaskCall *> calls;"
abstract "ScopeStatement : BodyStatement
   SymbolTable *symbols;"
class "Trap : ScopeStatement
  vector<PredicatedStatement *> handlers;

  PredicatedStatement* newHandler(Expression *e, Statement *s) {
    PredicatedStatement *ps = new PredicatedStatement(s, e);
    handlers.push_back(ps);
    return ps;
  }"
class "Signal : ScopeStatement"
class "Var : ScopeStatement"
class "StartCounter : Statement
  Counter *counter;
  Expression *count;

  StartCounter(Counter *c, Expression *i): counter(c), count(i) {}"
abstract "Renaming : ASTNode
  string old_name;

Renaming(string s) : old_name(s) {}"

class "TypeRenaming : Renaming
  TypeSymbol *new_type;

  TypeRenaming(string s, TypeSymbol *t) : Renaming(s), new_type(t) {}"

class "ConstantRenaming : Renaming
  Expression *new_value;

  ConstantRenaming(string s, Expression *e) : Renaming(s), new_value(e) {}"

class "FunctionRenaming : Renaming
  FunctionSymbol *new_func;

  FunctionRenaming(string s, FunctionSymbol *f) : Renaming(s), new_func(f) {}"

class "ProcedureRenaming : Renaming
  ProcedureSymbol *new_proc;

  ProcedureRenaming(string s, ProcedureSymbol *p)
    : Renaming(s), new_proc(p) {}"

class "SignalRenaming : Renaming
  SignalSymbol *new_sig;

  SignalRenaming(string s, SignalSymbol *ss) : Renaming(s), new_sig(ss) {}"
class "Run : Statement
  string old_name;
  string new_name;
  vector<TypeRenaming *> types;
  vector<ConstantRenaming *> constants;
  vector<FunctionRenaming *> functions;
  vector<ProcedureRenaming *> procedures;
  vector<ProcedureRenaming *> tasks;
  vector<SignalRenaming *> signals;
  SymbolTable *signalScope;

  Run(string s, SymbolTable *ss) : old_name(s), new_name(s), signalScope(ss)
    {}"
class "IfThenElse : Statement
  Expression *predicate;
  Statement *then_part;
  Statement *else_part;

  IfThenElse(Expression *e) : predicate(e) , then_part(0), else_part(0) {}
  IfThenElse(Expression *e, Statement *s1, Statement *s2)
    : predicate(e) , then_part(s1), else_part(s2) {}"
abstract "GRCNode : ASTNode
  vector<GRCNode*> predecessors;
  vector<GRCNode*> successors;
  vector<GRCNode*> dataPredecessors;
  vector<GRCNode*> dataSuccessors;

  virtual Status welcome(Visitor&) = 0;

  GRCNode& operator >>(GRCNode*);
  GRCNode& operator <<(GRCNode*);
  typedef map<GRCNode *, int> NumMap;
  int enumerate(NumMap &, std::set<GRCNode *> &, int);
" "
  GRCNode& GRCNode::operator >>(GRCNode *s) {
    successors.push_back(s);
    if (s) s->predecessors.push_back(this);
    return *this;
  }

  GRCNode& GRCNode::operator <<(GRCNode *p) {
    assert(p);
    dataPredecessors.push_back(p);
    p->dataSuccessors.push_back(this);
    return *this;
  }

  int GRCNode::enumerate(NumMap &number, std::set<GRCNode *> &visited, int next) {
    
    if (visited.find(this) != visited.end()) return next;
    visited.insert(this);
    if (number.find(this) == number.end() || number[this] == 0) {
      number[this] = next++;
    }
    for (vector<GRCNode*>::const_iterator i = successors.begin();
          i != successors.end() ; i++)
                if (*i) next = (*i)->enumerate(number, visited, next);
    for (vector<GRCNode*>::const_iterator i = predecessors.begin();
           i != predecessors.end() ; i++)
                if(*i) next = (*i)->enumerate(number, visited, next);
    for (vector<GRCNode*>::const_iterator i = dataSuccessors.begin();
           i != dataSuccessors.end() ; i++)
                if(*i) next = (*i)->enumerate(number, visited, next);
    for (vector<GRCNode*>::const_iterator i = dataPredecessors.begin();
           i != dataPredecessors.end() ; i++)
                if(*i) next = (*i)->enumerate(number, visited, next);      
    return next;
  }
"
abstract "GRCSTNode : GRCNode
  STNode *st;

  GRCSTNode(STNode *s) : st(s) {}
"
class "EnterGRC : GRCNode"
class "ExitGRC : GRCNode"
class "Nop : GRCNode
  int type;
  int code;
  string body;

  Nop(): type(0), code(0) {}

  int isflowin() { return type == 1;}
  void setflowin() { type = 1;}
  // a shorcut Nop gives "up" flow to child 0
  int isshortcut() { return type == 2;}
  void setshortcut() { type = 2;}
"
class "DefineSignal : GRCNode
  SignalSymbol *signal;
  bool is_surface;

  DefineSignal(SignalSymbol *s, bool ss) : signal(s), is_surface(ss) {}
"
class "Switch : GRCSTNode

  Switch(STNode *s) : GRCSTNode(s) {}
"
class "Test : GRCSTNode
  Expression *predicate;
  
  Test(STNode *s, Expression *e) : GRCSTNode(s), predicate(e) {}
"
class "STSuspend : GRCSTNode

  STSuspend(STNode *s) : GRCSTNode(s) {}
"
class "Fork : GRCNode
  Sync* sync;

  Fork() : sync(0) {}
  Fork(Sync* sync) : sync(sync) {}
"
class "Sync : GRCSTNode
 
  Sync(STNode *s) : GRCSTNode(s) {}
"
class "Terminate : GRCNode
  int code;
  int index;

  Terminate(int c, int i) : code(c), index(i) {}
"
class "Action : GRCNode
  Statement *body;

  Action(Statement *s) : body(s) {}
"
class "Enter : GRCSTNode

  Enter(STNode *s) : GRCSTNode(s) {}
"
abstract "STNode : ASTNode
  STNode *parent;
  vector<STNode*> children;

  STNode() : parent(0) {}
  virtual Status welcome(Visitor&) = 0;

  STNode& operator >>(STNode*);
  typedef map<STNode *, int> NumMap;
  int enumerate(NumMap &, std::set<STNode*> &visited, int);
" "
  STNode& STNode::operator >>(STNode *s) {
//    assert(s);
    children.push_back(s);
    if(s) s->parent = this;
    return *this;
  }

  int STNode::enumerate(NumMap &number, std::set<STNode*> &visited, int next) {
    if(visited.find(this) != visited.end()) return next;
    visited.insert(this);

    if(number.find(this) == number.end() || number[this] == 0){
        number[this] = next++;
    }
    for (vector<STNode*>::const_iterator i = children.begin() ;
         i != children.end() ; i++) if(*i)
      next = (*i)->enumerate(number, visited,  next);
    return next;
  }
"
class "STexcl : STNode"
class "STpar : STNode"
class "STref : STNode
  int type;

  STref(): type(0) {}

  int isabort() { return type == 1;}
  void setabort() { type = 1;}
  int issuspend() { return type == 2;}
  void setsuspend() { type = 2;}
"
class "STleaf : STNode
  int type;

  STleaf(): type(0) {}
 
  int isfinal() { return type == 1;}
  void setfinal() { type = 1;}
"
class "GRCgraph : ASTNode
  STNode *selection_tree;
  GRCNode *control_flow_graph;

  GRCgraph(STNode *st, GRCNode *cfg)
     : selection_tree(st), control_flow_graph(cfg) {}

  int enumerate(GRCNode::NumMap &, STNode::NumMap &, int max = 1);
" "
 int GRCgraph::enumerate(GRCNode::NumMap &cfgmap, STNode::NumMap &stmap, int max)
{
  std::set<GRCNode*> cfg_visited;
  std::set<STNode*>   st_visited;

  assert(selection_tree);
  assert(control_flow_graph);

  max = selection_tree->enumerate(stmap, st_visited, max);
  max = control_flow_graph->enumerate(cfgmap, cfg_visited, max);
  return max;
}

"

######################################################################

echo "#ifndef _AST_HPP
#  define _AST_HPP

/* Automatically generated by AST.sh -- do not edit */

#  include \"IR.hpp\"
#  include <string>
#  include <vector>
#  include <map>
#  include <cassert>
#  include <set>

namespace AST {
  using IR::Node;
  using IR::XMListream;
  using IR::XMLostream;
  using std::string;
  using std::vector;
  using std::map;

  class Visitor;
$forwarddefs

  union Status {
    int i;
    ASTNode *n;
    Status() {}
    Status(int ii) : i(ii) {}
    Status(ASTNode *nn) : n(nn) {}
  };

$classdefs

  class Visitor {
  public:
  virtual ~Visitor() {}
$visitorclassdefs
  };

}

#endif
" > AST.hpp

echo "/* Automatically generated by AST.sh -- do not edit */
#include \"AST.hpp\"
namespace AST {

$methoddefs

}
" > AST.cpp
