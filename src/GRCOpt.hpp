#include "IR.hpp"
#include "AST.hpp"
#include <set>
#include <vector>

namespace GRCOpt {

  using namespace AST;

  using std::set;
  using std::vector;

  template <class T> typename vector<T*>::iterator find(vector<T*> &v, T *e)
  {
    for (typename vector<T*>::iterator i = v.begin() ; i != v.end() ; i++)
      if ( (*i) == e ) return i;
    return v.end();
  }
  template <class T> void erase(vector<T*> &v, T *e) {
    v.erase(find(v, e));
  }
  template <class T> bool contains(set<T> &s, T o) {
    return s.find(o) != s.end();
  }
  template <class T, class U> bool contains(map<T, U> &m, T o) {
    return m.find(o) != m.end();
  }
  void delete_node(GRCNode *);
  void delete_node(STNode *);
  void bypass(GRCNode *);
  void bypass(STNode *);
  class Simulator : public Visitor {
    GRCgraph &g;

    // Reached nodes in the control-flow graph and selection tree
    std::set<GRCNode *> cfg;
    std::set<STNode *> st;

    // Switch statement in the CFG for each exclusive node in the ST
    std::map<STexcl *, Switch *> switch_for_excl;

    // The set of known-reachable children of each sync node
    std::map<Sync *, set<int> > sync_levels;

    // Nodes scheduled to be analyzed
    std::set<GRCNode *> pending;

    GRCNode *entergrc;

    // All cfg and ST nodes
    std::set<GRCNode *> allcfg;
    std::set<STNode *> allst;

  public:
    Simulator(GRCgraph &);
    Status visit(Switch &);
    Status visit(Enter &);
    Status visit(Terminate &);
    Status visit(Sync &);
    Status visit(Fork &);
    Status visit(Test &);
    Status visit(DefineSignal &);
    Status visit(Action &);
    Status visit(Nop &);
    Status visit(STSuspend &);
    Status visit(ExitGRC &) { return Status(); }
    void all_dfs(GRCNode *);
    void st_walk(STNode *);
    virtual ~Simulator() {}
  };
  class Pass : public Visitor {
    Status visit(Switch &) { return Status(); }
    Status visit(Test &) { return Status(); }
    Status visit(Terminate &) { return Status(); }
    Status visit(Sync &) { return Status(); }
    Status visit(Fork &) { return Status(); }
    Status visit(Action &) { return Status(); }
    Status visit(Enter &) { return Status(); }
    Status visit(STSuspend &) { return Status(); }
    Status visit(DefineSignal &) { return Status(); }
    Status visit(Nop &) { return Status(); }
    
    std::vector<GRCNode*> topolist;
    std::set<GRCNode*> reachable_nodes;
    std::set<GRCNode*> all_nodes;
    bool forward;
    GRCNode *entergrc;
    GRCNode *exitgrc;
    GRCNode *grcroot;
    STNode *stroot;

    void forward_dfs(GRCNode *);
    void all_dfs(GRCNode *);
    
  public:
    Pass(GRCgraph* g, bool f) : forward(f) {
      assert(g);
      stroot   = g->selection_tree;
      assert(stroot);
      entergrc = g->control_flow_graph;
      assert(entergrc);
      assert(entergrc->successors.size() == 2);
      exitgrc  = entergrc->successors[0];
      grcroot  = entergrc->successors[1];
    }
    virtual ~Pass(){}
    
    void transform();
  };
  class DanglingST : public Pass {
      std::set<STNode*> &stkept;
      Status visit(Enter &);
      Status visit(STSuspend &);
    public:
      DanglingST(GRCgraph* g, std::set<STNode*> &stkept) :
  	 Pass(g, true), stkept(stkept) {}
  };
  class PruneSW : public Pass {
      std::set<STNode*> &stkept;
      Status visit(Switch &);
    public:
      PruneSW(GRCgraph* g, std::set<STNode*> &stkept) :
  	 Pass(g, false), stkept(stkept) {}
  };
  class MergeSW : public Pass {
    std::set<STNode*> &stkept;
    Status visit(Switch &);
  public:
    MergeSW(GRCgraph* g, std::set<STNode*> &stkept) :
      Pass(g, false), stkept(stkept) {}
  };
  class STSimplify {
    GRCgraph *g;
    std::set<STNode*> &stkept;

    STNode *check_st(STNode *, STNode *realpar);
  public:
    STSimplify(GRCgraph *g, std::set<STNode*> &stkept) 
      : g(g), stkept(stkept) { assert(g); }
    void simplify() { g->selection_tree = check_st(g->selection_tree, NULL); }
  };
  class RemoveNops : public Pass {
      Status visit(Nop &);
    public:
      RemoveNops(GRCgraph *g) : Pass(g, true) {};
  };
  class RedundantEnters : public Pass {
    Status visit(Enter &);
  public:
    RedundantEnters(GRCgraph *g) : Pass(g, false) {}
  };
  class UnobservedEmits : public Pass {
    set<SignalSymbol*> observed;
    Status visit(Action &);
    Status visit(DefineSignal &);
  public:
    UnobservedEmits(Module *, GRCgraph *);
  };
}
