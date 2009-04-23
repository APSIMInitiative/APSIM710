#ifndef LookupFunctionH
#define LookupFunctionH

#include <list>
#include <math.h>
#include <string>
#include <algorithm>

using namespace std;
class plantInterface;
class ScienceAPI;

// Implement table lookup functions
class lookupFunction : public externalFunction
{
 private:
   vector<float> x;
   vector<float> y;
 public:
   void read(ScienceAPI& scienceAPI,
             const string& xName, const string& xunits, float x0, float x1,
             const string& yName, const string& yunits, float y0, float y1);
   float value(float v);
   vector<float> xVal()  {
   	return(x);
   };
   vector<float> yVal()  {
   	return(y);
   };
   std::string description(void);
};

#endif
