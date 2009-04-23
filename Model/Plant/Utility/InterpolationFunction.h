#ifndef InterpolationFunctionH
#define InterpolationFunctionH

#include <list>
#include <math.h>
#include <string>
#include <algorithm>
#include "Utility/ExternalFunction.h"

using namespace std;
class plantInterface;
class ScienceAPI;

// Implement stick (linear interpolation) functions
class interpolationFunction : public externalFunction
{
 private:
   vector<float> x;
   vector<float> y;
 public:
   float integral(float v1, float v2);
   void read(ScienceAPI& scienceAPI,
             const string& xName, const string&  xunits, float x0, float x1,
             const string& yName, const string& yunits, float y0, float y1);
   void readOptional(ScienceAPI& scienceAPI,
             const string& xName, const string&  xunits, float x0, float x1,
             const string& yName, const string& yunits, float y0, float y1, float DefaultY);

   float value(float v);
   vector<float> xVal()  {
   	return(x);
   };
   vector<float> yVal()  {
   	return(y);
   };
   std::string description(void);
   bool isInitialised(void)
      {
      if (x.size() == 0) return false;
      if (y.size() == 0) return false;
      return true;
      };
   float minYval(){return *min_element(y.begin(),y.end());}
   void setXY(const vector<float>& xValues,
              const vector<float>& yValues)
      {
      x = xValues;
      y = yValues;
      }
};


#endif
