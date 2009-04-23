#ifndef ExternalFunctionH
#define ExternalFunctionH

#include <list>
#include <math.h>
#include <string>
#include <algorithm>

using namespace std;
class plantInterface;
class ScienceAPI;

// An "external" function: an abstract class to encapsulate functions defined in .ini files..
class externalFunction {
 protected:
     std::string xName, yName, xUnits, yUnits;
public:
   externalFunction();
   virtual ~externalFunction();

   virtual void read(ScienceAPI& scienceAPI,
                const string& xname, const string& xunits, float x0, float x1,
                const string& yname, const string& yunits, float y0, float y1);

   virtual std::string description(void);
   virtual float value(float v) = 0;
   float operator [] (float arg)  {return value(arg);};
   virtual bool isInitialised(void) {return false;};
};

#endif
