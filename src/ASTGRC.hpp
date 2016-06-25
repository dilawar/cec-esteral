#ifndef _ASTGRC_HPP
#  define _ASTGRC_HPP

#  include "AST.hpp"
#  include <assert.h>
#  include <stack>
#  include <map>
#  include <set>
#  include <stdio.h>

namespace ASTGRC {
  using namespace IR;
  using namespace AST;
  using std::map;
  using std::set;

  class GrcSynth;

  class CompletionCodes : public Visitor {
    int maxOverModule; // Maximum code for this
    map<Abort*, int> codeOfAbort; // Code for each weak abort
    map<SignalSymbol*, int> codeOfTrapSymbol; // Code for each trap symbol
    map<Trap*, int> codeOfTrap; // Code for each trap statement
  public:

    void alsoMax(AST::ASTNode *n, int &m) {
       int max = recurse(n);
       if (max > m) m = max;
    }

    int recurse(AST::ASTNode *n) {
       if (n) return n->welcome(*this).i;
       else return 0;
    }

    CompletionCodes(Module *m)
    {
      assert(m);
      assert(m->body);
      maxOverModule = recurse(m->body);
      if (maxOverModule <= 1) maxOverModule = 1;
    }

    virtual ~CompletionCodes() {}
      int max() const { return maxOverModule; }

      int operator [] (Abort *a) {
        assert(codeOfAbort.find(a) != codeOfAbort.end());
        return codeOfAbort[a];
      }

      int operator [] (SignalSymbol *ts) {
        assert(codeOfTrapSymbol.find(ts) != codeOfTrapSymbol.end());
        return codeOfTrapSymbol[ts];
      }

      int operator [] (Trap *t) {
        assert(codeOfTrap.find(t) != codeOfTrap.end());
        return codeOfTrap[t];
      }
    Status visit(Signal &s) { return recurse(s.body); }
    Status visit(Var &s) { return recurse(s.body); }
    Status visit(Loop &s) { return recurse(s.body); }
    Status visit(Repeat &s) { return recurse(s.body); }
    Status visit(Every &s) { return recurse(s.body); }
    Status visit(Suspend &s) { return recurse(s.body); }
    Status visit(PredicatedStatement &s) { return recurse(s.body); }
    Status visit(StatementList &l) {
      int max = 1;
      for (vector<Statement*>::iterator i = l.statements.begin() ;
           i != l.statements.end() ; i++ ) alsoMax(*i, max);
      return max;
    }
    Status visit(ParallelStatementList &l) {
      int max = 1;
      for (vector<Statement*>::iterator i = l.threads.begin() ;
           i != l.threads.end() ; i++ ) alsoMax(*i, max);
      return max;
    }
    Status visit(IfThenElse& n) {
      int max = 1;
      alsoMax(n.then_part, max);
      alsoMax(n.else_part, max);
      return max;
    }
    Status visit(Emit&) { return Status(0); }
    Status visit(Assign&) { return Status(0); }
    Status visit(ProcedureCall&) { return Status(0); }
    Status visit(TaskCall&) { return Status(0); }
    Status visit(Exec&) { return Status(0); }
    Status visit(Exit&) { return Status(0); }
    Status visit(Run&) { return Status(0); }
    Status visit(Pause&) { return Status(0); }
    Status visit(Abort &s) {
      int max = 1;
      alsoMax(s.body, max);
      for (vector<PredicatedStatement*>::iterator i = s.cases.begin() ;
           i != s.cases.end() ; i++ ) alsoMax(*i, max);
      if (s.is_weak) {
        int code = max + 1;
        codeOfAbort[&s] = code;
        assert(code >= 2);
        max += 1 + s.cases.size();
      }
      return max;
    }
    Status visit(Trap &s) {
      int max = 1;
      alsoMax(s.body, max);

      // FIXME: is this the right order?  Should the predicates be
      // considered before or after the code is assigned?
     
      for (vector<PredicatedStatement*>::iterator i = s.handlers.begin() ;
           i != s.handlers.end() ; i++ ) alsoMax(*i, max);

      max++; // Allocate an exit level for this trap statement

      codeOfTrap[&s] = max;

      assert(s.symbols);
      for (SymbolTable::const_iterator i = s.symbols->begin() ; i !=
           s.symbols->end() ; i++) {
        SignalSymbol *ts = dynamic_cast<SignalSymbol*>(*i);
        assert(ts);
        assert(ts->kind == SignalSymbol::Trap);
        codeOfTrapSymbol[ts] = max;
      }

      return max;
    }
  };

  class Cloner : public Visitor {
  public:
    template <class T> T* operator() (T* n) {
      if (!n) return NULL;
      T* result = dynamic_cast<T*>(n->welcome(*this).n);
      assert(result);
      return result;
    }

    SignalSymbol *cloneLocalSignal(SignalSymbol *s, SymbolTable *st) {
      assert(s);
      assert(newsig.find(s) == newsig.end()); // Should not already be there
      assert(st);

      string name = s->name;
      int next = 1;
      while (st->contains(name)) {
        char buf[10];
        sprintf(buf, "%d", next++);
        name = s->name + '_' + buf;
      }
      SignalSymbol::kinds kind =
        (s->kind == SignalSymbol::Trap) ? SignalSymbol::Trap : SignalSymbol::Local;
      SignalSymbol *reincarnation = 0;
      if (master_signal.find(s) != master_signal.end()) {
        reincarnation = master_signal[s];
        assert(reincarnation);
      }
      SignalSymbol *result =
        new SignalSymbol(name, s->type, kind, clone(s->combine),
                         clone(s->initializer), reincarnation);
      if (!reincarnation)
        master_signal[s] = result;
      assert(master_signal.find(s) != master_signal.end());
      // std::cerr << "cloning " << s->name << std::endl;
      st->enter(result);
      newsig[s] = result;
      return result;
    }
    void sameSig(SignalSymbol *s) {
      assert(s);
      assert(newsig.find(s) == newsig.end());
      newsig[s] = s;
    }
    void clearSig(SignalSymbol *orig) {
      assert(orig);
      map<SignalSymbol*, SignalSymbol*>::iterator i = newsig.find(orig);
      assert(i != newsig.end());
      newsig.erase(i);
    }
    map<VariableSymbol*, VariableSymbol*> newvar;
    VariableSymbol *hoistLocalVariable(VariableSymbol *s, SymbolTable *st) {
      assert(s);
      assert(newvar.find(s) == newvar.end()); // should not be remapped yet
      assert(st);

      // Add a suffix to the name to make it unique, if necessary

      string name = s->name;
      int next = 1;
      while (st->contains(name)) {
        char buf[10];
        sprintf(buf, "%d", next++);
        name = s->name + '_' + buf;
      }

      VariableSymbol *result =
        new VariableSymbol(name, s->type, clone(s->initializer));
      st->enter(result);
      newvar[s] = result;
      return result;
    }
    void sameVar(VariableSymbol *s) {
      assert(s);
      assert(newvar.find(s) == newvar.end());
      newvar[s] = s;
    }

    virtual ~Cloner() {}

  protected:
    template <class T> T* clone(T* n) { return (*this)(n); }

    /* For each signal symbol in the AST's symbol tables,
       the master signal symbol in the expanded graph.  Used to
       set up the reincarnation field of the cloned signals. */
    map<SignalSymbol *, SignalSymbol *> master_signal;

    Status visit(Emit &s) {
      return new Emit(clone(s.signal), clone(s.value));
    }

    Status visit(Exit &s) {
      return new Exit(clone(s.trap), clone(s.value));
    }

    Status visit(Assign &s) {
      return new Assign(clone(s.variable), clone(s.value));
    }
    Status visit(Literal &s) { return new Literal(s.value, s.type); }

    Status visit(LoadVariableExpression &s) {
      return new LoadVariableExpression(clone(s.variable));
    }

    Status visit(LoadSignalExpression &s) {
      return new LoadSignalExpression(s.type, clone(s.signal));
    }

    Status visit(LoadSignalValueExpression &s) {
      return new LoadSignalValueExpression(clone(s.signal));
    }
    Status visit(UnaryOp &s) {
      return new UnaryOp(s.type, s.op, clone(s.source));
    }

    Status visit(BinaryOp &s) {
      return new BinaryOp(s.type, s.op, clone(s.source1), clone(s.source2));
    }
    Status visit(FunctionCall &s) {
      FunctionCall *c = new FunctionCall(clone(s.callee));
      for (vector<Expression*>::const_iterator i = s.arguments.begin() ;
           i != s.arguments.end() ; i++) {
        assert(*i);
        c->arguments.push_back(clone(*i));
      }
      return c;
    }
    Status visit(ProcedureCall &s) {
      ProcedureCall *c = new ProcedureCall(clone(s.procedure));
      for (vector<VariableSymbol*>::const_iterator i = s.reference_args.begin() ;
           i != s.reference_args.end() ; i++) {
        assert(*i);
        c->reference_args.push_back(clone(*i));
      }
      for (vector<Expression*>::const_iterator i = s.value_args.begin() ;
           i != s.value_args.end() ; i++) {
        assert(*i);
        c->value_args.push_back(clone(*i));
      }

      return c;
    }
    Status visit(CheckCounter &s) {
      return new CheckCounter(s.type, s.counter, clone(s.predicate));
    }
    Status visit(ConstantSymbol &s) { return &s; }
    Status visit(BuiltinConstantSymbol &s) { return &s; }
    Status visit(BuiltinSignalSymbol &s) { return &s; }
    Status visit(FunctionSymbol &s) { return &s; }
    Status visit(ProcedureSymbol &s) { return &s; }
    Status visit(BuiltinFunctionSymbol &s) { return &s; }
    Status visit(Counter &s) { return &s; }
    map<SignalSymbol*, SignalSymbol*> newsig;
    Status visit(SignalSymbol &s) {
      assert(newsig.find(&s) != newsig.end()); // should be there
      return newsig[&s];
    }
    Status visit(VariableSymbol &s) {
      assert(newvar.find(&s) != newvar.end()); // should be there
      return newvar[&s];
    }
  };

  struct Context {
    int size;
    std::stack<GRCNode**> continuations;

    Context(int sz) : size(sz) {
      assert(sz >= 2); // Must at least have termination at levels 0 and 1
      continuations.push(new GRCNode*[size]);
      for (int i = 0 ; i < size ; i++ ) continuations.top()[i] = 0;
    }
    ~Context() {}

    void push(Context &c) {
      GRCNode **parent = c.continuations.top();
      continuations.push(new GRCNode*[size]);
      GRCNode **child = continuations.top();
      for ( int i = 0 ; i < size ; i++ ) child[i] = parent[i];
    }

    void push() { push(*this); }

    void pop() {
      delete [] continuations.top();
      continuations.pop();
    }

    GRCNode *& operator ()(int k) {
      assert(k >= 0);
      assert(k < size);
      return continuations.top()[k];
    }
  };
  class GrcWalker : public Visitor {
  protected:
    Context &context;
    GrcSynth &environment;
    Cloner &clone;
  public:
    GrcWalker(Context &, GrcSynth &);

    GRCNode *synthesize(ASTNode *n) {
      assert(n);
      n->welcome(*this);
      assert(context(0));
      return context(0);
    }

    GRCNode *recurse(ASTNode *n) {
      context.push();
      GRCNode *nn = synthesize(n);
      context.pop();
      return nn;
    }

    static GRCNode* push_onto(GRCNode *& b, GRCNode* n) {
      *n >> b;
      b = n;
      return b;
    }

    STNode *stnode(const ASTNode &);
  };
  class Surface : public GrcWalker {
  public:
    Surface(Context &c, GrcSynth &e) : GrcWalker(c, e) {}
    Status visit(Pause &);
    Status visit(Exit &);
    Status visit(Emit &s) {
      push_onto(context(0), new Action(clone(&s)));
      return Status();
    }
    Status visit(Assign &s) {
      Action *a = new Action(clone(&s));
      *a >> context(0);
      context(0) = a;
      return Status();
    }
    Status visit(IfThenElse &);
    Status visit(StatementList &);
    Status visit(Loop &);
    Status visit(Every &);
    Status visit(Repeat &);
    Status visit(Suspend &);
    Status visit(Abort &);
    Status visit(ParallelStatementList &);
    Status visit(Trap &);
    Status visit(Signal &);
    Status visit(Var &);
    Status visit(Exec &) { return Status(); }
    Status visit(ProcedureCall &s) {
      push_onto(context(0), new Action(clone(&s)));
      return Status();
    }
  };
  class Depth : public GrcWalker {
  public:
    Depth(Context &c, GrcSynth &e) : GrcWalker(c, e) {}
    Status visit(Pause &);
    Status visit(Exit &) { return Status(); }
    Status visit(Emit &) { return Status(); }
    Status visit(Assign &) { return Status(); }
    Status visit(IfThenElse &);
    Status visit(StatementList &);
    Status visit(Loop &);
    Status visit(Every &);
    Status visit(Repeat &);
    Status visit(Suspend &);
    Status visit(Abort &);
    Status visit(ParallelStatementList &);
    Status visit(Trap &);
    Status visit(Signal &);
    Status visit(Var &);
    Status visit(Exec &) { return Status(); }
    Status visit(ProcedureCall &) { return Status(); }
  };
  class SelTree : public Visitor {
  protected:
    GrcSynth &environment;
  public:
    SelTree(GrcSynth &e): environment(e) {}

    STNode *synthesize(ASTNode *n) {
      assert(n);
      STNode *result = dynamic_cast<STNode*>(n->welcome(*this).n);
      assert(result);
      return result;
    }

    void setNode(const ASTNode &, STNode *);
      
    Status visit(Pause &);
    Status visit(Exit &s) {
      return Status(new STref());
    }
    Status visit(Emit &) {
      return Status(new STref());
    }
    Status visit(Assign &s) {
      return Status(new STref());
    }
    Status visit(IfThenElse &);
    Status visit(StatementList &s);
    Status visit(Loop &s);
    Status visit(Every &);
    Status visit(Repeat &);
    Status visit(Suspend &);
    Status visit(Abort &);
    Status visit(ParallelStatementList &);
    Status visit(Trap &);
    Status visit(Signal &);
    Status visit(Var &);
    Status visit(Exec &) { return Status(new STref()); }
    Status visit(ProcedureCall &) { return Status(new STref()); }
  };
  struct GrcSynth {
    Module *module;
    CompletionCodes &code;

    Cloner clone;

    Context surface_context;
    Context depth_context;

    Surface surface;
    Depth depth;
    SelTree seltree;

    map<const ASTNode*, STNode*> ast2st;

    BuiltinTypeSymbol *integer_type;
    BuiltinTypeSymbol *boolean_type;
    BuiltinConstantSymbol *true_symbol;

    GrcSynth(Module *, CompletionCodes &);
    GRCgraph *synthesize()
    {
      assert(module->body);

      // Set up initial and terminal states in the selection tree

      STexcl *stroot = new STexcl();
      STleaf *boot = new STleaf();
      STleaf *finished = new STleaf();
      finished->setfinal();

      // Set up the root of the GRC

      EnterGRC *engrc = new EnterGRC();
      ExitGRC *exgrc = new ExitGRC();

      Enter *enfinished = new Enter(finished);
      Switch *top_switch = new Switch(stroot);

      *engrc >> exgrc >> top_switch;
      *enfinished >> exgrc;

      enfinished->st = finished;

      Terminate *term0 = new Terminate(0, 0);
      *term0 >> enfinished;
      Terminate *term1 = new Terminate(1, 0);
      *term1 >> exgrc;

      // Set up the context for the surface and the depth: point to term0 and 1

      surface_context(0) = depth_context(0) = term0;
      surface_context(1) = depth_context(1) = term1;

      // Build the selection tree and create the selection tree root

      STNode *synt_seltree = seltree.synthesize(module->body);
      *stroot >> finished >> synt_seltree >> boot;

      // Build the surface and the depth

      GRCNode *synt_surface = surface.synthesize(module->body);
      GRCNode *synt_depth = depth.synthesize(module->body);

      // Add DefineSignal statements for every output signal.  This clears
      // their presence and initializes their values if an initializer was given

      for (SymbolTable::const_iterator i = module->signals->begin() ;
           i != module->signals->end() ; i++) {
        SignalSymbol *ss = dynamic_cast<SignalSymbol*>(*i);
        assert(ss);
        if (ss->kind == SignalSymbol::Output) {
          DefineSignal *ds = new DefineSignal(ss, true);
          *ds >> synt_surface;
          synt_surface = ds;
        }
      }

      *top_switch >> enfinished >> synt_depth >> synt_surface;

      GRCgraph *result = new GRCgraph(stroot, engrc);

      return result;
    }
  };
  class RecursiveSynth : public Visitor {
  public:
    Module *module;
    CompletionCodes &code;

    Cloner clone;

    Context context;

    BuiltinTypeSymbol *boolean_type;
    BuiltinConstantSymbol *true_symbol;

    // The visitors set these pointers when they return

    STNode *stnode;
    GRCNode *surface;
    GRCNode *depth;

    void synthesize(ASTNode *n) {
      assert(n);

      stnode = NULL; // Assignments not strictly necessary: for safety
      surface = depth = NULL;

      n->welcome(*this);
      assert(stnode);
      assert(surface);
      assert(depth);
    }

    // Run n then b, replacing b
    static void run_before(GRCNode *& b, GRCNode *n) { *n >> b; b = n; }

    RecursiveSynth(Module *, CompletionCodes &);
    GRCgraph *synthesize();
    map<GRCNode *, bool> visiting;
    void visit(GRCNode *);
    Status visit(Pause &);
    Status visit(Exit &);
    Status visit(Emit &);
    Status visit(Assign &);
    Status visit(IfThenElse &);
    Status visit(StatementList &);
    Status visit(Loop &);
    Status visit(Every &);
    Status visit(Repeat &);
    Status visit(Suspend &);
    Status visit(Abort &);
    Status visit(ParallelStatementList &);
    Status visit(Trap &);
    Status visit(Signal &);
    Status visit(Var &);
    Status visit(ProcedureCall &);
    Status visit(Exec &);
    virtual ~RecursiveSynth() {}
  };

  class Dependencies : public Visitor {
  protected:
    set<GRCNode *> visited;
    GRCNode *current;

  public:
    struct SignalNodes {
      set<GRCNode *> writers;
      set<GRCNode *> readers;
    };

    map<SignalSymbol *, SignalNodes> dependencies;

    void dfs(GRCNode *);
    Status visit(Action &);
    Status visit(Emit &e) {
      dependencies[e.signal].writers.insert(current);
      if (e.value) e.value->welcome(*this);
      return Status();
    }
    Status visit(Exit &e) {
      dependencies[e.trap].writers.insert(current);
      if (e.value) e.value->welcome(*this);
      return Status();
    }
    Status visit(Assign &a) {
      a.value->welcome(*this);
      return Status();
    }
    Status visit(ProcedureCall &c) {
      for ( vector<Expression*>::const_iterator i = c.value_args.begin() ;
            i != c.value_args.end() ; i++ )
        (*i)->welcome(*this);
      return Status();
    }
    Status visit(Pause &) { return Status(); }
    Status visit(StartCounter &) { return Status(); }
    Status visit(DefineSignal &d) {
      dependencies[d.signal].writers.insert(current);
      return Status();
    }
    Status visit(Test &t) { t.predicate->welcome(*this); return Status(); }
    Status visit(LoadSignalExpression &e) {
      dependencies[e.signal].readers.insert(current);
      return Status();
    }

    Status visit(LoadSignalValueExpression &e) {
      dependencies[e.signal].readers.insert(current);
      return Status();
    }

    Status visit(BinaryOp &e) {
      e.source1->welcome(*this);
      e.source2->welcome(*this);
      return Status();
    }

    Status visit(UnaryOp &e) {
      e.source->welcome(*this);
      return Status();
    }

    Status visit(CheckCounter &e) {
      e.predicate->welcome(*this);
      return Status();
    }

    Status visit(Delay &d) {
      d.predicate->welcome(*this);
      return Status();
    }

    Status visit(FunctionCall &c) {
        for ( vector<Expression*>::const_iterator i = c.arguments.begin() ;
            i != c.arguments.end() ; i++ )
        (*i)->welcome(*this);
      return Status();
    }
    Status visit(Literal &) { return Status(); }
    Status visit(LoadVariableExpression &) { return Status(); }
    Status visit(Sync &);
    Status visit(EnterGRC &) { return Status(); }
    Status visit(ExitGRC &) { return Status(); }
    Status visit(Nop &) { return Status(); }
    Status visit(Switch &) { return Status(); }
    Status visit(STSuspend &) { return Status(); }
    Status visit(Fork &) { return Status(); }
    Status visit(Terminate &) { return Status(); }
    Status visit(Enter &) { return Status(); }
    Dependencies() {}
    virtual ~Dependencies() {}

    static void compute(GRCNode *);
  };
}
#endif
