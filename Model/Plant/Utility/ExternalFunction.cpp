#include "StdPlant.h"

#include "ExternalFunction.h"
using namespace std;

externalFunction::externalFunction() {};
externalFunction::~externalFunction() {};

void externalFunction::read(ScienceAPI& /*scienceAPI*/,
                       const string& xname, const string& xunits, float /* x0*/, float /* x1*/,
                       const string& yname, const string& yunits, float /* y0*/, float /* y1*/)
      {
      xName = string(xname); yName = string(yname);
      xUnits = string(xunits); yUnits = string(yunits);
      }

std::string externalFunction::description(void)
   {
   return string("");
   }
