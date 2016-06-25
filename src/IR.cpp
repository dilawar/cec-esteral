#include "IR.hpp"

#include <expat.h>
#include <iostream>
#include <sstream>
#include <cassert>

namespace IR {
    IRCLASS(Node);

    XMLostream& XMLostream::operator <<(const Node *n)
    {
      if (n == NULL) {
        o << "<NULL/>" << std::endl;
      } else {
        idmap::iterator i = ID.find(n);
        if (i != ID.end()) {
  	// Already written this Node: leave a placeholder
  	o << "<Ref id=\"" << (*i).second << "\"/>" << std::endl;
        } else {
  	o << "<" << n->className() << " id=\"" << nextID << "\">" << std::endl;
  	ID[n] = nextID;
  	nextID++;
  	n->write(*this);  // usually a recursive call
  	o << "</" << n->className() << ">" << std::endl;
        }
      }
      return *this;
    }
    XMLostream& XMLostream::operator <<(const Node &n)
    {
      o << "<" << n.className() << '>' << std::endl;
      n.write(*this);  // usually a recursive call
      o << "</" << n.className() << ">" << std::endl;
      return *this;
    }
    XMLostream& XMLostream::operator <<(const std::string s)
    {
      o << "<S>";
      for (std::string::const_iterator i = s.begin() ; i != s.end() ; i++ )
        switch (*i) {
        case '&': o << "&amp;"; break;
        case '<': o << "&lt;"; break;
        case '>': o << "&gt;"; break;
        case '\'': o << "&apos;"; break;
        case '\"': o << "&quot;"; break;
        default: o << *i; break;
        };
      o << "</S>\n";
      return *this;
    }
    XMLostream& XMLostream::operator <<(int i)
    {
      o << "<Int>" << i << "</Int>\n";
      return *this;
    }
    XMLostream& XMLostream::operator <<(bool b)
    {
      if (b) o << "<BoolTrue/>";
      else o << "<BoolFalse/>";
      return *this;
    }

    void XMLNode::print() {
      std::cout << '<' << name;
      for (attrmap::iterator j = attrs.begin() ; j != attrs.end() ; j++)
        std::cout << ' ' << (*j).first << "=\"" << (*j).second << '\"';
      std::cout << '>';
      std::cout << body;
      if (first) first->print();
      std::cout << "</" << name << ">" << std::endl;
      if (next) next->print();
    }

    XMListream::XMListream(std::istream &i)
    {
      root = lastsibling = NULL;
      XML_Parser p = XML_ParserCreate(NULL);
      if (!p) throw Error("Couldn't create parser");

      XML_SetElementHandler(p, startElement, endElement);
      XML_SetCharacterDataHandler(p, charData);
      XML_SetUserData(p, (void *) this);

      do {
        static const size_t SIZE = 8192;
        char buffer[SIZE];

        i.read(buffer, SIZE);
        if (i.bad()) throw Error("Read error");
        if (!XML_Parse(p, buffer, i.gcount(), i.eof())) {
  	std::ostringstream ost;
  	ost << "XML parsing error at line " << XML_GetCurrentLineNumber(p)
              << ':' << XML_ErrorString(XML_GetErrorCode(p));
  	throw Error(ost.str());
        }
      } while (!i.eof());

      XML_ParserFree(p);
      if (!parents.empty()) throw Error("Non-empty stack."); 

      // root->print(); // For debugging
      current = root;
    }
    XMListream& XMListream::operator >>(std::string &s)
    {
      if (!current) throw Error("Expecting text before end of element");
      if (current->name != "S")
  	throw Error("Expecting text, found " + current->name);
      s = current->body;
      current = current->next;
      return *this;
    }
    XMListream& XMListream::operator >>(int &i)
    {
      if (!current) throw Error("Expecting an Int");
      if (current->name == "Int") {
        std::istringstream iss(current->body);
        iss >> i;
      } else
        throw Error("Expecting Int, found " + current->name);
      current = current->next;
      return *this;
    }
    XMListream& XMListream::operator >>(bool &i)
    {
      if (!current) throw Error("Expecting BoolTrue or BoolFalse");
      if (current->name == "BoolTrue") i = true;
      else if (current->name == "BoolFalse") i = false;
      else throw Error("Expecting BoolTrue or BoolFalse");

      current = current->next;
      return *this;
    }
    Node *XMListream::getNextNode()
    {
      if (!current) throw Error("Expecting an element, found nothing");
      Node *n;

      XMLNode::attrmap::iterator idit = current->attrs.find("id");

      if (current->name == "NULL") {       // NULL pointer

        n = NULL;

      } else if (current->name == "Ref") { // Reference to existing object

        if ( idit == current->attrs.end() )
  	throw Error("Ref node without id attribute");

        nodemap::iterator ni = nodeofid.find((*idit).second);
        if ( ni == nodeofid.end())
  	throw Error("Ref to undefined node id " + (*idit).second);
        n = (*ni).second;

      } else {                            // Normal object

        std::string name = current->name;
        n = Class::newNodeByName(name);
        if (idit != current->attrs.end()) nodeofid[(*idit).second] = n;
        parents.push(current);
        current = current->first;
        n->read(*this);    // Fill in the node's fields from this stream
        if (current != NULL) throw Error("excess elements under " + name);
        current = parents.top();
        parents.pop();
      }
      current = current->next;
      return n;
    }
    void XMListream::attachSibling(XMLNode *n)
    {
      if (root == NULL) root = n;
      if (!lastsibling) {
        if (!parents.empty()) parents.top()->first = n;
      } else
        lastsibling->next = n;
      lastsibling = n;
    }
    void XMListream::startElement(void *rr, const char *name, const char **attrs)
    {
      XMListream *r = static_cast<XMListream*>(rr);
      XMLNode *newNode = new XMLNode();
      newNode->name = name;
      while (*attrs) {
        newNode->attrs.insert( std::make_pair(*attrs,*(attrs+1)) );
        attrs += 2;
      }

      r->attachSibling(newNode);

      r->parents.push(r->lastsibling);
      r->lastsibling = NULL;
    }
    void XMListream::endElement(void *rr, const char *)  
    {
      XMListream *r = static_cast<XMListream*>(rr);
      // discard the topmost sibling; we'll go back to where we were.
      r->lastsibling = r->parents.top();
      r->parents.pop();
    }
  void XMListream::charData(void *rr, const XML_Char *ss, int len)
  {
    XMListream *r = static_cast<XMListream*>(rr);
    std::string s(ss, len);
    if (!(r->parents.empty()))
      r->parents.top()->body += s;
  }

    Class::createfuncmap *Class::classmap = 0;

    Class::Class(const std::string s, Class::createfunc f) : _className(s)
    {
      if (!classmap) classmap = new createfuncmap();
      (*classmap)[s] = f;
    }
    Node * Class::newNodeByName(const std::string s)
    {
      assert(classmap);
      createfuncmap::iterator i = classmap->find(s);
      if (i != classmap->end()) {
        createfunc cf = (*i).second;
        return (*cf)();
      } else {
        throw Error("Unknown class " + s);
      }
    }
}
