#ifndef _BLIF_HPP
#  define _BLIF_HPP

#  include <string>
#  include <vector>
#  include <assert.h>
#  include <iostream>
#  include <map>
#  include <set>

namespace BLIF {
  using std::vector;
  using std::string;
  using std::set;

  class Input;
  class Gate;
  class Netlist;

  class Input {
    friend class Gate;
    Input(Gate *d, Gate *g, bool i = false)
      : driver(d), gate(g), is_inverting(i) {}
  public:
    Gate *driver;
    Gate *gate;
    bool is_inverting;
  };
  class Gate {
   friend class Netlist;

   Gate(Netlist *nl, unsigned int i, string n, bool inv = false)
     : parent(nl), id(i), name(n), is_inverting(inv),
       is_input(false), is_output(false), is_latch(false) {}
  public:
    Netlist *parent;
    unsigned int id;
    string name;
    vector<Input*> inputs;
    bool is_inverting;
    vector<Input*> outputs;

    bool is_input;
    bool is_output;
    bool is_latch;

    void newInput(Gate *g, bool i = false) {
      assert(g);
      Input *ni = new Input(g, this, i);
      inputs.push_back(ni);
      g->outputs.push_back(ni);
    }

    void newOutput(Gate *g, bool i = false) {
      assert(g);
      g->newInput(this, i);
    }
  };
    class Netlist {
    public:
      string name;
      Netlist(string n) : name(n) {}
      vector<Gate*> gates;
      Gate *newGate(bool = false, string = "");
    };

  void print_dot(std::ostream &, const Netlist &);

  Netlist *read_blif(std::istream &);
  class BlifReader {
    Netlist *netlist;
    std::istream &inf;
    std::map<string, Gate*> namedGate;
    string line;
    vector<string> word;
    unsigned lineNumber;

    struct Row {
      string andplane;
      char orplane;
      Row(string s, char c) : andplane(s), orplane(c) {}
    };

  public:
    BlifReader(std::istream &ii) :  netlist(0), inf(ii), lineNumber(0) {}

    struct Error {
      unsigned lineNumber;
      string error;
      Error(unsigned l, string s) : lineNumber(l), error(s) {}
    };

    Gate *getGate(string = "");
    Netlist *readModel();
    void readLine();
  };

  void print_blif(std::ostream &, const Netlist &);
  class Simulator {
    Netlist &n;

    unsigned int next;
    Gate **topoorder; // Array of gate pointers in topological order

    bool *currentState; // State of latches, indexed by gate number
    bool *nextState; // State of inputs and gates/latches after simulation

    void dfs(Gate*, set<Gate*> &);
  public:
    vector<Gate*> inputs;
    vector<Gate*> outputs;
    vector<Gate*> latches;

    void setInput(Gate* g, bool v) { currentState[g->id] = v; }
    bool getOutput(Gate* g) { return currentState[g->id]; }
    bool getLatch(Gate* g) { return nextState[g->id]; }

    Simulator(Netlist &);
    ~Simulator();

    void reset();
    void simulate();

    static bool debug;
  };

  void print_verilog(std::ostream &, const Netlist &);
}

#endif
