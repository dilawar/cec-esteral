#ifndef _IR_HPP
#  define _IR_HPP

#  include <expat.h>
#  include <string>
#  include <map>
#  include <stack>
#  include <vector>
#  include <iostream>

#  define IRCLASSDEFS \
  public: \
    static IR::Class _; \
    virtual const std::string className() const { return _; }
#  define IRCLASS(s) IR::Class s::_ = IR::Class( #s, IR::constructor<s> )

namespace IR {
  class Node;
  class XMListream;
  class XMLostream;

    struct Error {
      std::string s;
      Error(std::string ss) : s(ss) {}
    };
    // Information for the classes in the IR: a string naming each,
    // and a map giving the constructor for each class
    class Class {
      typedef Node *(*createfunc)();
      typedef std::map<std::string, createfunc> createfuncmap;
      static createfuncmap *classmap;
    public:
      Class(std::string, createfunc);
      static Node *newNodeByName(const std::string);

    private:
      const std::string _className;
    public:
      operator const std::string () { return _className; }
    };
    class Node {
      friend class XMLostream;
      friend class XMListream;
    protected:
      virtual void read(XMListream &) {}
      virtual void write(XMLostream &) const {}
    public:
      static IR::Class _;
      virtual const std::string className() const { return _; }
      virtual ~Node() {}
    };
    class XMLostream {
      typedef std::map<const Node *, unsigned int> idmap;
      idmap ID;

      unsigned int nextID;
      std::ostream &o;
      
    public:
      XMLostream(std::ostream &oo) : nextID(0), o(oo) {}
      XMLostream& operator <<(const Node&);
      XMLostream& operator <<(const Node *);
      XMLostream& operator <<(const std::string);
      XMLostream& operator <<(int);
      XMLostream& operator <<(bool);
          template <class T> XMLostream& operator <<(const std::vector<T*> &v) {
            typename std::vector<T*>::const_iterator i;
            for ( i = v.begin() ; i != v.end() ; i++ ) (*this) << *i;
            o << "<EOV/>" << std::endl;
            return *this;
          }
          template <class K, class V>
           XMLostream& operator <<(const std::map<K, V> &m) {
            typename std::map<K, V>::const_iterator i;
            for ( i = m.begin() ; i != m.end() ; i++ ) 
              (*this) << (*i).first << (*i).second;
            o << "<EOM/>" << std::endl;
            return *this;
          }
    };
    struct XMLNode {
      std::string name;
      std::string body;

      typedef std::map<const std::string, const std::string> attrmap;
      attrmap attrs;
      
      XMLNode *first;
      XMLNode *next;

      XMLNode() : first(0), next(0) {}

      ~XMLNode() { delete first; delete next; }

      void print();
    };
    class XMListream {
      std::stack<XMLNode*> parents;
      XMLNode *lastsibling;
      XMLNode *current;
      XMLNode *root;

      static void startElement(void *, const char *, const char **);
      static void endElement(void *, const char *);
      static void charData(void *, const XML_Char *, int);
      void attachSibling(XMLNode *);

      // Map that tracks Nodes with IDs
      typedef std::map<const std::string, Node *> nodemap;
      nodemap nodeofid;

      Node *getNextNode();

    public:
      XMListream(std::istream &);
      ~XMListream() { delete root; }

          template <class T> XMListream& operator >>(T* &f) {
            Node *n = getNextNode();
            f = (n != NULL) ? dynamic_cast<T*>(n) : NULL;
            if (!f && n) throw Error("Unexpected element " + n->className());
            return *this;
          }
        template <class N> XMListream& operator >>(N &n) {
          if ( current->name != std::string(N::_) ) // Check name of this class
            throw Error("Unexpected element " + current->name);
          parents.push(current);
          current = current->first;
          n.read(*this); // Fill in the object's fields
          if (current != NULL)
            throw Error("excess elements for " + std::string(N::_));
          current = parents.top();
          parents.pop();
          current = current->next;
          return *this;
        }
      XMListream& operator >>(std::string&);
      XMListream& operator >>(int &);
      XMListream& operator >>(bool &);
          template <class T> XMListream& operator >>(std::vector<T> &v) {
            while (current && current->name != "EOV") {
               T value;
               (*this) >> value;
               v.push_back(value);
            }
            if (!current) throw Error("vector ended without EOV");
            current = current->next;  // Skip the EOV Element
            return *this;
          }
          template <class K, class V> XMListream& operator >>(std::map<K, V> &m) {
            while (current && current->name != "EOM") {
               K key;
               V value;
      	 (*this) >> key >> value;
               m.insert( typename std::map<K,V>::value_type(key, value) );
            }
            if (!current) throw Error("map ended without EOM");
            current = current->next;  // Skip the EOM Element
            return *this;
          }
    };
  template<class T> Node* constructor() { return new T(); }
}
#endif
