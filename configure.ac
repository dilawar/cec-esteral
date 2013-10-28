# Run aclocal to generate aclocal.m4
# then run automake
# then run autoconf

AC_INIT(src/cec-strlxml.cpp)
AM_INIT_AUTOMAKE(cec, 0.4)

AC_PROG_CC
AC_LANG(C)

AC_PROG_CXX
AC_LANG(C++)
AC_PROG_RANLIB

AC_MSG_CHECKING([for C++ standard library headers])
AC_COMPILE_IFELSE([
#include <vector>
#include <set>
#include <map>
#include <sstream>

std::vector<int> myvect;
std::set<std::vector<int> > myset;
std::map<char *, std::set<int> > mymap;
std::ostringstream ost;

using std::vector;
using std::map;

vector<int> anothervect;
map<char*, int> anothermap;

],[AC_MSG_RESULT(yes)],[AC_MSG_RESULT(no: Warning compilation likely to fail)])

# FIXME: Add an option for the user to specify expat library/header paths

AC_CHECK_HEADER(expat.h,,
echo The required expat header file expat.h was not found.
echo Please re-run configure with CPPFLAGS=-Idirectory
exit 1
)
AC_CHECK_LIB(expat, XML_ParserCreate,,
echo The required expat XML parser library was not found.
echo Please re-run configure with LIBS=-Ldirectory
exit 1
)

# These are useful for development, but aren't necessary for building

AC_PATH_PROG(NOTANGLE, notangle)
AC_PATH_PROG(NOWEAVE, noweave)

# Check for the ability to run ANTLR -- necessary for development, but not
# for a build

AC_SUBST(ANTLR)

AC_MSG_CHECKING([for ANTLR])
AC_CACHE_VAL(ac_cv_found_antlr,
[ac_cv_found_antlr=yes ;
cat << EOF > test.g
options { language="Cpp"; }
class TestParser extends Parser;
file : ;
EOF
java antlr.Tool test.g > antlr-version 2>&1 || ac_cv_found_antlr=no
antlr_version=`sed 's/^.*Version //
s/ .*$//' antlr-version`
rm -f TestParser.cpp TestParser.hpp TestParserTokenTypes.hpp TestParserTokenTypes.txt test.g antlr-version
])
if test $ac_cv_found_antlr = yes ; then
  AC_MSG_RESULT($antlr_version)
  ANTLR="java antlr.Tool"
else
  ANTLR=""
  AC_MSG_RESULT(no)
fi

AC_CONFIG_FILES([Makefile src/Makefile tests/Makefile libantlr/Makefile])
AC_CONFIG_FILES([src/cec],[chmod +x src/cec])
AC_OUTPUT
