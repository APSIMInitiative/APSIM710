#include "../StdPlant.h"

#include "ExternalFunction.h"
#include "LookupFunction.h"
using namespace std;

void lookupFunction::read(ScienceAPI& scienceAPI,
                            const string& xname, const string& xunits, float x0, float x1,
                            const string& yname, const string& yunits, float y0, float y1)
   {
   externalFunction::read(scienceAPI, xname, xunits, x0, x1, yname, yunits, y0, y1);
   x.clear();   y.clear();

   scienceAPI.read(xname, x, x0, x1);
   scienceAPI.read(yname, y, y0, y1);

   if (x.size() != y.size())
   	throw std::runtime_error(string("Mismatched vector size in ") + xname + " and " + yname);

   if (x.size() <= 0)
   	throw std::runtime_error(string("Zero length vectors in") + xname + " and " + yname);
   }


std::string lookupFunction::description(void)
   {
   int pad;
   ostringstream text;
   pad = max(0, 27 - (int)xName.size());
   text << "   " << xName << setw(pad) << " " << setw(-1) << "=";
   for (unsigned int i = 0; i != x.size(); i++) {
   	text << " " << setw(7) << x[i] << setw(-1);
   }
   text << " (" << xUnits << ")\n";

   pad = max(0, 27 - (int)yName.size());
   text << "   " << yName << setw(pad) << " " << setw(-1) << "=";
   for (unsigned int i = 0; i != y.size(); i++) {
   	text << " " << setw(7) << y[i] << setw(-1);
   }
   text << " (" << yUnits << ")\n";
   return text.str();
   }

// Return a y value via table lookup
float lookupFunction::value(float v)
   {
   if (x.size() == 0 || y.size() == 0)
       throw std::runtime_error(string("Uninitialised call to lookupFunction:") + xName + " and " + yName);

   // find which sector of the table that v falls in
   unsigned sector;
   for(sector = 0; sector < x.size() && v >= x[sector]; sector++) /*nothing*/ ;

   if(sector == 0) return y[0];
   return y[sector-1];
   }

