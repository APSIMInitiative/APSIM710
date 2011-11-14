#include "ScienceConverterComponent.h"

using namespace std;

//const float kg2g = 1000.0;
//const float ha2sm = 10000.0;
//const float g2kg = (1.0/kg2g);
//const float sm2ha = (1.0/ha2sm);
//const float cmol2mol = (1.0/100.0);
//const float mm2m = (1.0/1000.0);
//


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

extern "C" EXPORT void STDCALL getDescriptionLength(char* initScript, int* length)
//=======================================================================================
// Return component description info.
   {
   char* buffer = new char[500000];
   getDescriptionInternal(initScript, buffer);
   *length = strlen(buffer);
   delete [] buffer;
   }

// ------------------------------------------------------------------
// Create an instance of the science converter module
// ------------------------------------------------------------------
protocol::Component* createComponent(void)
   {
   return new ScienceConverterComponent;
   }
// ------------------------------------------------------------------
// constructor
// ------------------------------------------------------------------
ScienceConverterComponent::ScienceConverterComponent(void)
   {
   }
// ------------------------------------------------------------------
// Destructor
// ------------------------------------------------------------------
ScienceConverterComponent::~ScienceConverterComponent(void)
   {
    if (conversion) delete conversion;
   }
// ------------------------------------------------------------------
// Init1 phase.
// ------------------------------------------------------------------
void ScienceConverterComponent::doInit1(const protocol::Init1Data& initData)
   {
   protocol::Component::doInit1(initData);
   endRunID = addRegistration(::respondToEvent, 0, "end_run", protocol::DDML(""));

    conversion_model = readParameter ("constants", "conversion_model");

    if (conversion_model == "")
       throw std::invalid_argument("The parameter 'conversion_model'\nisn't in your ini file.\n\nGet one.\n");
    else if (conversion_model == "herbage")
       conversion = new HerbageConverter(this);
//    else if (scratch == "surfaceom")
//       conversion = new ResidueHerbage();
    else if (conversion_model == "nonherbage")
       conversion = new NonHerbageConverter(this);
    else
       throw std::invalid_argument("Unknown conversion model '" + conversion_model + "'");

    conversion->doInit1(initData);
   }
// ------------------------------------------------------------------
// Init2 phase.
// ------------------------------------------------------------------
void ScienceConverterComponent::doInit2(void)
   {
      ostringstream msg;
      msg << "Conversion model:- " << conversion_model << endl << ends;
      writeString (msg.str().c_str());

    conversion->doInit2();
   }

// ------------------------------------------------------------------
// Event handler.
// ------------------------------------------------------------------
void ScienceConverterComponent::respondToEvent(unsigned int& fromID, unsigned int& eventID, protocol::Variant& variant)
{
   if (eventID != endRunID)
      conversion->respondToEvent(fromID, eventID, variant);
}

void ScienceConverterComponent::respondToGet(unsigned int& fromID, protocol::QueryValueData& queryData)
{
    conversion->respondToGet(fromID, queryData);
}

