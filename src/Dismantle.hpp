#ifndef _DISMANTLE_HPP
#  define _DISMANTLE_HPP

#  include "AST.hpp"
#  include <assert.h>
#  include <sstream>
#  include <set>
#  include <stdio.h>

namespace Dismantle {
  using namespace IR;
  using namespace AST;

  class Rewriter : public Visitor {
  protected:
    Module *module;
  public:
    template <class T> T* transform(T* n) {
      T* result = n ? dynamic_cast<T*>(n->welcome(*this).n) : 0;
      assert(result || !n);
      return result;
    }

    template <class T> void rewrite(T* &n) { n = transform(n); }

    StatementList& sl() { return *(new StatementList()); }

    Rewriter() : module(0) {}

    Status visit(Modules &m) {
      for (vector<Module*>::iterator i = m.modules.begin() ;
           i != m.modules.end() ; i++ ) {
        rewrite(*i);
        assert(*i);
      }
      return &m;
    }
    Status visit(Module &m) {
      module = &m;
      rewrite(m.body);
      assert(m.body);
      return &m;
    }
    Status visit(StatementList &l) {
      for (vector<Statement*>::iterator i = l.statements.begin() ;
           i != l.statements.end() ; i++ ) {
        rewrite(*i);
        assert(*i);
      }
      return &l;
    }
    Status visit(ParallelStatementList &l) {
      for (vector<Statement*>::iterator i = l.threads.begin() ;
           i != l.threads.end() ; i++ ) {
        rewrite(*i);
        assert(*i);
      }
      return &l;
    }
    Status visit(Loop &s) {
      rewrite(s.body);
      return &s;
    }
    Status visit(Repeat &s) {
      rewrite(s.count);
      rewrite(s.body);
      return &s;
    }
    Status visit(Every &s) {
      rewrite(s.body);
      rewrite(s.predicate);
      return &s;
    }
    Status visit(Suspend &s) {
      rewrite(s.predicate);
      rewrite(s.body);
      return &s;
    }
    Status visit(Abort &s) {
      rewrite(s.body);
      for (vector<PredicatedStatement*>::iterator i = s.cases.begin() ;
           i != s.cases.end() ; i++) {
        assert(*i);
        rewrite(*i);
      }
      return &s;
    }
    Status visit(PredicatedStatement &s) {
      rewrite(s.predicate);
      rewrite(s.body);
      return &s;
    }
    Status visit(Trap &s) {
      rewrite(s.body);
      for (vector<PredicatedStatement*>::iterator i = s.handlers.begin() ;
           i != s.handlers.end() ; i++) {
        assert(*i);
        rewrite((*i)->body);
      }
      return &s;
    }
    Status visit(IfThenElse& n) {
      rewrite(n.predicate);
      rewrite(n.then_part);
      rewrite(n.else_part);
      return &n;
    }
    Status visit(Signal& s) {
      rewrite(s.body);
      return &s;
    }
    Status visit(Var& s) {
      rewrite(s.body);
      return &s;
    }
    Status visit(ProcedureCall& s) {
      for ( vector<Expression*>::iterator i = s.value_args.begin() ;
    	i != s.value_args.end() ; i++ ) {
        assert(*i);
        rewrite(*i);
      }
      return &s;
    }
    Status visit(Emit& s) {
      rewrite(s.value);
      return &s;
    }
    Status visit(Assign& s) {
      rewrite(s.value);
      return &s;
    }
    Status visit(Present& s) {
      for ( vector<PredicatedStatement*>::iterator i = s.cases.begin() ;
    	i != s.cases.end() ; i++ ) {
        assert(*i);
        rewrite(*i);
      }
      if (s.default_stmt) rewrite(s.default_stmt);
      return &s;
    }
    Status visit(If& s) {
      for ( vector<PredicatedStatement*>::iterator i = s.cases.begin() ;
    	i != s.cases.end() ; i++ ) {
        assert(*i);
        rewrite(*i);
      }
      if (s.default_stmt) rewrite(s.default_stmt);
      return &s;
    }
    Status visit(Nothing& n) { return &n; }
    Status visit(Pause& n) { return &n; }
    Status visit(Halt& n) { return &n; }
    Status visit(Sustain& n) { return &n; }
    Status visit(Await& n) { return &n; }
    Status visit(LoopEach& n) { return &n; }
    Status visit(DoWatching& n) { return &n; }
    Status visit(DoUpto& n) { return &n; }
    Status visit(TaskCall& n) { return &n; }
    Status visit(Exec& n) { return &n; }
    Status visit(Exit& n) { return &n; }
    Status visit(Run& n) { return &n; }
    Status visit(LoadVariableExpression &e) { return &e; }
    Status visit(LoadSignalExpression &e) { return &e; }
    Status visit(LoadSignalValueExpression &e) { return &e; }
    Status visit(Literal &e) { return &e; }
    Status visit(UnaryOp &e) {
      rewrite(e.source);
      return &e;
    }
    Status visit(BinaryOp &e) {
      rewrite(e.source1);
      rewrite(e.source2);
      return &e;
    }
    Status visit(FunctionCall &e) {
      for ( vector<Expression*>::iterator i = e.arguments.begin() ;
    	i != e.arguments.end() ; i++ ) {
        assert(*i);
        rewrite(*i);
      }
      return &e;
    }
    Status visit(Delay &e) {
      rewrite(e.predicate);
      rewrite(e.count);
      return &e;
    }
  };
  class Dismantler1 : public Rewriter {
  public:
    IfThenElse *dismantle_case(CaseStatement &c) {
      assert(c.cases.size() > 0);
      IfThenElse *result = 0;
      IfThenElse *lastif = 0;

      for (vector<PredicatedStatement*>::iterator i = c.cases.begin() ;
           i != c.cases.end() ; i++ ) {
        assert(*i);
        assert((*i)->predicate);
        IfThenElse *thisif = new IfThenElse((*i)->predicate);
        thisif->then_part = transform((*i)->body);
        if (result)
          lastif->else_part = thisif;
        else
          result = thisif;
        lastif = thisif;
      }
      assert(lastif);
      lastif->else_part = c.default_stmt;
      assert(result);
      return transform(result);
    }

    virtual Status visit(Present &s) { return dismantle_case(s); }
    virtual Status visit(If &s) { return dismantle_case(s); }
    Status visit(Await &a) {
      Pause *p = new Pause();
      Loop *l = new Loop(p);
      Abort *ab = new Abort(l, false);
      // Copy the predicates
      for ( vector<PredicatedStatement*>::const_iterator i = a.cases.begin();
    	i != a.cases.end() ; i++ )
        ab->cases.push_back(*i);     
      return transform(ab);
    }
    Status visit(Trap &t) {
      assert(t.symbols);
      if (t.handlers.size() > 1 ||
          (t.handlers.size() >= 1 && t.symbols->size() > 1) ) {

       // More than one trap or more than one handler: transform

       ParallelStatementList *psl = new ParallelStatementList();

       BuiltinTypeSymbol *boolean_type = NULL;

       for (vector<PredicatedStatement*>::const_iterator i = t.handlers.begin() ; 
             i != t.handlers.end() ; i++ ) {
         assert(*i);
         assert((*i)->predicate);
         IfThenElse *ite = new IfThenElse((*i)->predicate, (*i)->body, NULL);
         boolean_type = dynamic_cast<BuiltinTypeSymbol*>((*i)->predicate->type);
         psl->threads.push_back(ite);
       }

       assert(boolean_type); // Should have found at least one

       Expression *newExpr = NULL;

       for (SymbolTable::const_iterator i = t.symbols->begin() ;
    	i != t.symbols->end() ; i++) {
         SignalSymbol *ss = dynamic_cast<SignalSymbol*>(*i);
         assert(ss);
         assert(ss->kind == SignalSymbol::Trap);
         LoadSignalExpression *lse = new LoadSignalExpression(boolean_type, ss);
         if (newExpr)
           newExpr = new BinaryOp(boolean_type, "or", newExpr, lse);
         else
           newExpr = lse;                
       }

       assert(newExpr); // Should have found at least one

       t.handlers.clear(); // Old handlers are now unneeded: should garbage collect

       t.newHandler(newExpr, psl);
      }

      assert(t.handlers.size() <= 1);

      return Rewriter::visit(t);
    }
    Trap *newTrap(SignalSymbol *&ts) {
      static unsigned int nextIndex = 0;

      Trap *result = new Trap();
      result->symbols = new SymbolTable();
      // Note: The parent of this symbol table is incorrectly NULL

      char buf[10];
      sprintf(buf, "%d", nextIndex++);
      string name = "weak_trap_" + string(buf);
      ts = new SignalSymbol(name, NULL, SignalSymbol::Trap, NULL, NULL, NULL);
      result->symbols->enter(ts);
      return result;
    }
    Status visit(Abort &a) { 
      if (a.is_weak) {

        SignalSymbol *innerTrap;
        Trap *inner = newTrap(innerTrap);

        StatementList *newbody = new StatementList();
        if (a.body) *newbody << a.body;
        *newbody << new Exit(innerTrap, 0);

        Await *await = new Await();

        ParallelStatementList *psl = new ParallelStatementList();
        psl->threads.push_back(newbody);
        psl->threads.push_back(await);

        inner->body = psl;

        Statement *result = inner;

        BuiltinTypeSymbol *boolean_type =
          dynamic_cast<BuiltinTypeSymbol*>(module->types->get("boolean"));
        assert(boolean_type);

        for (vector<PredicatedStatement*>::reverse_iterator i = a.cases.rbegin() ;
    	 i != a.cases.rend() ; i++) {
          SignalSymbol *trapSymbol;
          Trap *theNewTrap = newTrap(trapSymbol);
          theNewTrap->body = result;
          Statement *body = (*i)->body;
          if (!body) body = new Nothing();
          theNewTrap->newHandler(
    	  new LoadSignalExpression(boolean_type, trapSymbol), body);

          await->cases.insert(await->cases.begin(),
            new PredicatedStatement(new Exit(trapSymbol, NULL), (*i)->predicate));

          result = theNewTrap;
        }
     
        return transform(result);
      } else {
        // A normal Abort: recurse
        return Rewriter::visit(a);
      }
    }
    Status visit(Var &v) {

      bool hasInitializer = false;

      assert(v.symbols);

      for ( SymbolTable::const_iterator i = v.symbols->begin() ;
    	i != v.symbols->end() ; i++ ) {
        VariableSymbol *vs = dynamic_cast<VariableSymbol *>(*i);
        assert(vs);
        if (vs->initializer) {
          hasInitializer = true;
          break;
        }
      }

      if (hasInitializer) {
        StatementList *sl = new StatementList();

        for ( SymbolTable::const_iterator i = v.symbols->begin() ;
    	  i != v.symbols->end() ; i++ ) {
          VariableSymbol *vs = dynamic_cast<VariableSymbol *>(*i);
          assert(vs);
          if (vs->initializer) {
    	*sl << new Assign(vs, vs->initializer);
    	vs->initializer = NULL;
          }
        }
        // Add the body of the var statement to the list
        *sl << v.body;
        v.body = sl;
      }

      rewrite(v.body);
      return &v;
    }
    Status visit(DoWatching &s) {
      return transform(new Abort(s.body, s.predicate, s.timeout));
    }
    Status visit(DoUpto &s) {
      return transform(new Abort(&(sl() << s.body << new Halt()), s.predicate, 0));
    }
    Status visit(LoopEach &s) {
      return transform(new Loop(new Abort(&(sl() << s.body << new Halt()),
    				      s.predicate, 0)));
    }
    Status visit(Halt &s) {
      return transform(new Loop(new Pause()));
    }
    Status visit(Sustain &s) {
      return transform(new Loop(&(sl() <<
    			      new Emit(s.signal, s.value) << new Pause())));
    }
    Status visit(Nothing &) {
      return transform(new StatementList());
    }
  };
}
#endif
