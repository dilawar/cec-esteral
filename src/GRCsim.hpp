#ifndef _GRCSIM_HPP
#  define _GRCSIM_HPP

#  include "IR.hpp"
#  include "AST.hpp"
#  include <iostream>
#  include <stdlib.h>
#  include <stdio.h>
#  include <set>
#  include <map>

namespace AST {

using std::set;
using std::map;

class GRCsim: public Visitor {
  
  int debug;
  static const int debugDFS = 1 << 1;
  static const int debugSync = 1 << 2;
  static const int debugVectors = 1 << 3;
  int useold;
  int globalcc; //a hack
  
  GRCgraph *top;
  EnterGRC *entergrc;
  SymbolTable *sigs;
  STNode *stroot;
  STleaf  *boot;
  GRCNode *grcroot;

  Module *module;
  
  GRCNode::NumMap &cfgmap;
  STNode::NumMap &stmap;

  bool ternary;
  map<STNode*, int > state;
  map<STNode*, int > nextstate; 
  // state & next state information for selection tree nodes

  vector<GRCNode*> topo;
  map<GRCNode*, int> sched;
  set<GRCNode*> dfs_notwhite, dfs_black;
  map<SignalSymbol*, int> signals; // Status of each signal
                                   // 0,1,2(unknown)
  map<SignalSymbol*, int> signals_v;
  map<Counter*, int> counters; // Value of each counter
  map<VariableSymbol*, int> var; // Value of each variable
  
  std::ostream &outf;
  bool ISFinal;
  bool jump;
  
  public:
  GRCsim(GRCgraph *top, Module *m, GRCNode::NumMap &cm,
	 STNode::NumMap &sm, int db, std::ostream &outf)
    : debug(db), top(top), sigs(m->signals),
      module(m), cfgmap(cm), stmap(sm), outf(outf) {}
  
  virtual ~GRCsim() {}
  
  void visit (GRCNode *);
    
  Status visit(EnterGRC &) { return Status(); }
  Status visit(ExitGRC &) { return Status(); }
  Status visit(Switch &);

  Status visit(Test &);
  Status visit(LoadSignalExpression &);
  Status visit(CheckCounter &);
  Status visit(BinaryOp &);
  Status visit(UnaryOp &);
  Status visit(LoadSignalValueExpression &);
  Status visit(FunctionCall &);
  Status visit(LoadVariableExpression &);
  Status visit(Literal &);
  
  Status visit(STexcl &) { return Status(); }
  Status visit(STref &) { return Status(); }
  Status visit(STpar &) { return Status(); }
  Status visit(STleaf &) { return Status(); }

  void dfs(GRCNode *n);
  void init();

  void schedule_child(GRCNode *,int);
  void schedule(GRCNode *,int);
  void clear_inputs();
  void dotick();
  void execute_max(int);
  void execute_vectors(std::istream &);
  int doswitch(STNode *n);  

  Status visit(Fork &);
  Status visit(Terminate &);
  Status visit(Sync &);
  void setState(STNode *n, bool isInit);
  Status visit(Enter &);
  Status visit(STSuspend &);
  Status visit(Action &);
  Status visit(Emit &);
  Status visit(Exit &);
  Status visit(Assign &);
  Status visit(StartCounter &);
  Status visit(Nop &);
  Status visit(DefineSignal &);
  int intVal(ASTNode *n)
  {
    assert(n);
    Status s = n->welcome(*this);
    return s.i; // Integer-valued result
  }
};

}
#endif
