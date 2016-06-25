#ifndef _GRCC2_HPP
#  define _GRCC2_HPP

#  include "AST.hpp"
#  include "CPrinter.hpp"

namespace GRCC2 {
  using namespace AST;
  typedef map<GRCNode *, int> CFGmap;
  typedef map<STNode *, int> STmap;
  void generateC(std::ostream &o, Module &, string, bool);
  void fixGRCNode(GRCNode *);
  void checkAcyclic(GRCNode *);
  class AcyclicChecker {
    std::map<GRCNode*, bool> completed;
    bool visit(GRCNode *);
  public:
    AcyclicChecker(GRCNode* n) {
      if (visit(n)) {
        std::cerr << std::endl;
        throw IR::Error("CFG is cyclic");
      }
    }
  };
  void calculateSchedule(GRCNode *, vector<GRCNode*> &);
  class Scheduler {
    std::set<GRCNode*> visited;
    std::vector<GRCNode*> &schedule;
    void visit(GRCNode*);
  public:
    Scheduler(GRCNode *n, vector<GRCNode*> &schedule) : schedule(schedule) {
      visit(n);
    }
  };
  struct Cluster {
    unsigned int id;
    unsigned int level;
    vector<GRCNode*> nodes;
    std::set<Cluster*> successors;
    vector<GRCNode*> entries;
    Cluster(int id) : id(id) {}
  };
  void cluster(vector<GRCNode*> &, vector<Cluster*> &, map<GRCNode*, Cluster*> &);
  void greedyCluster(GRCNode *, vector<Cluster*> &, map<GRCNode*, Cluster*> &);
  struct Level {
    vector<Cluster *> clusters;
  };
  void levelize(vector<Cluster*> &, vector<Level*> &);
  void split(Cluster *, map<GRCNode*, Cluster*> &, map<GRCNode*, string> &);
}

#endif
