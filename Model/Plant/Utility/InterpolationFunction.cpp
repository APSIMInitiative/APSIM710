
#include <stdexcept>
#include "../StdPlant.h"

#include "ExternalFunction.h"
#include "InterpolationFunction.h"
using namespace std;

// Linear Interpolation function setup
void interpolationFunction::read(ScienceAPI& scienceAPI,
                                   const string& xname, const string& xunits, float x0, float x1,
                                   const string& yname, const string& yunits, float y0, float y1)
   {
   externalFunction::read(scienceAPI, xname, xunits, x0, x1, yname, yunits, y0, y1);
   x.clear();   y.clear();

   scienceAPI.read(xname, x, x0, x1);
   scienceAPI.read(yname, y, y0, y1);
   }

bool interpolationFunction::readOptional(ScienceAPI& scienceAPI,
                                   const string& xname, const string& xunits, float x0, float x1,
                                   const string& yname, const string& yunits, float y0, float y1)
   {
   externalFunction::read(scienceAPI, xname, xunits, x0, x1, yname, yunits, y0, y1);
   x.clear();   y.clear();

   if (scienceAPI.readOptional(xname, x, x0, x1))
     return (scienceAPI.readOptional(yname, y, y0, y1));

   return false;
   }

void interpolationFunction::setDefaultValue(float DefaultY)
   {
   x.clear();   y.clear();
   x.push_back(0.0);
   y.push_back(DefaultY);
   }

std::string interpolationFunction::description(void)
   {
   int pad;
   ostringstream text;
   pad = max(0, 27 - (int)xName.size());
   text << "         " << xName << setw(pad) << " " << setw(-1) << "=";
   for (unsigned int i = 0; i != x.size(); i++) {
   	text << " " << setw(7) << x[i] << setw(-1);
   }
   text << " (" << xUnits << ")\n";

   pad = max(0, 27 - (int)yName.size());
   text << "         " << yName << setw(pad) << " " << setw(-1) << "=";
   for (unsigned int i = 0; i != y.size(); i++) {
   	text << " " << setw(7) << y[i] << setw(-1);
   }
   text << " (" << yUnits << ")\n";

   return text.str();
   }

// Return a y value from a linear interpolation function
float interpolationFunction::value(float v)
   {
   if (x.size() == 0 || y.size() == 0)
       throw std::runtime_error(string("Uninitialised call to interpolationFunction:") + xName + " and " + yName);

   if (x.size() != y.size())
       throw std::runtime_error(string("Mismatched vector size in ") + xName + " and " + yName);

   // find which sector of the function that v falls in
   unsigned sector;
   for(sector = 0;sector < x.size();sector++)
      if(v < x[sector] || isEqual(v,x[sector]))break;

   if(sector == 0) return y[0];
   if(sector == x.size())return y[y.size()-1];
   if(isEqual(v,x[sector]))return y[sector]; // prevent roundoff errors
   // y = mx + c
   float slope =  divide(y[sector]-y[sector-1],
                         x[sector]-x[sector-1],
                         y[sector]);

   return y[sector-1] + slope * (v - x[sector - 1]);
   }


// Return an integral for a linear interpolation function
float interpolationFunction::integral(float v1, float v2)
   {
   if (x.size() == 0 || y.size() == 0)
       throw std::runtime_error(string("Uninitialised call to interpolationFunction:") + xName + " and " + yName);

   if (x.size() != y.size())
       throw std::runtime_error(string("Mismatched vector size in ") + xName + " and " + yName);

   // find which sector of the function that v1 falls in
   unsigned sector1;
   for(sector1 = 0;sector1 < x.size();sector1++)
      if(v1 < x[sector1] || isEqual(v1,x[sector1]))break;
   // find which sector of the function that v2 falls in
   unsigned sector2;
   for(sector2 = 0;sector2 < x.size();sector2++)
      if(v2 < x[sector2] || isEqual(v2,x[sector2]))break;

   float I = 0; // Integral value
   unsigned s;
   for (s = sector1;s <= sector2; s++)
      {
      float xstart;
      if (s>0)
         xstart = max(x[s-1],v1);
      else
         xstart = v1;

      float xend;
      if (s>x.size()-1)
         xend = v2;
      else
         xend = min(x[s],v2);

      I = I + (xend-xstart)*(value(xend)+value(xstart))/2.0;
      }
   return I;
   }

