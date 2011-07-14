#include <General/math_functions.h>
#include <General/stl_functions.h>

#include "YPComponent.h"
#include <ComponentInterface/Variant.h>
using namespace std;
using namespace protocol;

// ------------------------------------------------------------------
// Return a blank string when requested to indicate that we
// don't need a wrapper DLL.
// ------------------------------------------------------------------
extern "C" EXPORT void STDCALL wrapperDLL(char* wrapperDll)
   {
   strcpy(wrapperDll, "");
   }
extern "C" void STDCALL getDescriptionInternal(char* initScript,
                                                 char* description);
// ------------------------------------------------------------------
// Return component description info.
// ------------------------------------------------------------------
extern "C" EXPORT void STDCALL getDescription(char* initScript, char* description)
   {
   getDescriptionInternal(initScript, description);
   }
extern "C" void STDCALL getDescriptionLengthInternal(char* initScript,
                                                 int* length);
// ------------------------------------------------------------------
// Return component description length.
// ------------------------------------------------------------------
extern "C" EXPORT void STDCALL getDescriptionLength(char* initScript, int* length)
   {
   getDescriptionLengthInternal(initScript, length);
   }
// ------------------------------------------------------------------
// Create an instance of the YP module
// ------------------------------------------------------------------
Component* createComponent(void)
   {
   return new YPComponent;
   }

// ------------------------------------------------------------------
// constructor
// ------------------------------------------------------------------
YPComponent::YPComponent(void)
   {
   }

// ------------------------------------------------------------------
// initialise the YP component - STAGE 1.
// ------------------------------------------------------------------
void YPComponent::doInit1(const protocol::Init1Data& initData)
   {
   Component::doInit1(initData);
   static const char* realArrayType = "<type kind=\"single\" array=\"T\" />";
   static const char* doubleType = "<type kind=\"double\" />";

   cllID = addRegistration(::respondToGet, 0, "CLLToday", doubleType);
   dulID = addRegistration(::respondToGet, 0, "DULToday", doubleType);
   swID = addRegistration(::respondToGet, 0, "SWToday", doubleType);
   critSwID = addRegistration(::respondToGet, 0, "CritSWToday", doubleType);
   nID = addRegistration(::respondToGet, 0, "NToday", doubleType);
   satID = addRegistration(::respondToGet, 0, "SATToday", doubleType);

   rootDepthID = addRegistration(::get, 0, "root_depth", doubleType);
   lldepID = addRegistration(::get, 0, "ll_dep", realArrayType);
   duldepID = addRegistration(::get, 0, "dul_dep", realArrayType);
   satdepID = addRegistration(::get, 0, "sat_dep", realArrayType);
   swdepID = addRegistration(::get, 0, "sw_dep", realArrayType);
   dlayerID = addRegistration(::get, 0, "dlayer", realArrayType);
   no3ID = addRegistration(::get, 0, "no3", realArrayType);
   nh4ID = addRegistration(::get, 0, "nh4", realArrayType);
   }
// ------------------------------------------------------------------
// return a variable to caller.
// ------------------------------------------------------------------
void YPComponent::respondToGet(unsigned int& fromID, QueryValueData& queryData)
   {
   getStaticVariables();
   if (queryData.ID == cllID)
      sendVariable(queryData, interpFromArray(cll, "cll"));
   else if (queryData.ID == dulID)
      sendVariable(queryData, interpFromArray(dul, "dul"));
   else if (queryData.ID == satID)
      sendVariable(queryData, interpFromArray(sat, "sat"));
   else if (queryData.ID == nID)
      {
      getDoubles(no3ID, no3, false);
      getDoubles(nh4ID, nh4, false);
      no3 = Accum(no3);
      nh4 = Accum(nh4);
      sendVariable(queryData, interpFromArray(no3, "no3") + interpFromArray(nh4, "nh4"));
      }
   else if (queryData.ID == swID)
      {
      std::vector<double> sw;
      getDoubles(swdepID, sw, false);
      sw = Accum(sw);
      sendVariable(queryData, interpFromArray(sw, "sw"));
      }
   else if (queryData.ID == critSwID)
      {
      double llToday = interpFromArray(cll, "cll");
      double dulToday = interpFromArray(dul, "dul");
      double critSWToday = llToday + (dulToday - llToday) * 0.30;
      sendVariable(queryData, critSWToday);
      }
   else
	  {
      protocol::Component::respondToGet(fromID, queryData);
      }
   }

// ------------------------------------------------------------------
// Calculate a cll value
// ------------------------------------------------------------------
double YPComponent::interpFromArray(std::vector<double>& values, const std::string& variableName)
   {
   protocol::Variant* variant;
   bool ok = getVariable(rootDepthID, &variant, true);
   float rootDepth;
   if (ok)
      ok = variant->unpack(rootDepth);
   if (!ok)
      throw runtime_error("Cannot find a root_depth variable belonging to any module");

   if (rootDepth == 0)
      return 0.0;
   else
      {
      bool DidInterp;
      if (values.size() != Depth.size())
         {
         ostringstream o;
         o << "Size of variable: " << variableName << " is wrong. Size = " << values.size();
         error(o.str().c_str(), true);
         }
      else
      return linear_interp_real (rootDepth, Depth, values, DidInterp);
      }
   }

// ------------------------------------------------------------------
// Calculate a dlayer
// ------------------------------------------------------------------
void YPComponent::getStaticVariables()
   {
   protocol::Variant* variant;
   bool ok;
   if (dul.size() == 0)
      {
      getDoubles(duldepID, dul, false);
      getDoubles(dlayerID, Depth, false);

      if (dul.size() != Depth.size())
         throw runtime_error("Number of layers dont match between dul & dlayer");

      dul = Accum(dul);
      Depth = Accum(Depth);
      }
   if (cll.size() == 0)
      {
      getDoubles(lldepID, cll, true);
      if (cll.size() > 0)
         {
         cll = Accum(cll);
         if (cll.size() > 0 && cll.size() != dul.size())
            throw runtime_error("Number of layers dont match between cll and dul.");
         }
      }
   if (sat.size() == 0)
      {
      getDoubles(satdepID, sat, false);
      sat = Accum(sat);
      if (sat.size() != Depth.size())
         throw runtime_error("Number of layers dont match between sat & dlayer");
      }
      
   }

// -----------------------------------
// Accumulate the values in the array.
// -----------------------------------
std::vector<double> YPComponent::Accum(std::vector<double>& values)
   {
   std::vector<double> returnValues;
   if (values.size() > 0)
      {
   returnValues.push_back(0.0);
   double ValueSoFar = 0.0;
   for (unsigned i = 0; i != values.size(); i++)
      {
      ValueSoFar += values[i];
      returnValues.push_back(ValueSoFar);
      }
      }
   return returnValues;
   }
// ------------------------------------------------------------------
// Get a variable from APSIM.
// ------------------------------------------------------------------
void YPComponent::getDoubles(unsigned id, std::vector<double>& values, bool optional)
   {
   values.clear();

   protocol::Variant* variant;
   if (getVariable(id, &variant, optional))
      {
      std::vector<float> singleValues;
      variant->unpack(singleValues);

      for (unsigned i = 0; i != singleValues.size(); i++)
         values.push_back(singleValues[i]);
      }
   }
