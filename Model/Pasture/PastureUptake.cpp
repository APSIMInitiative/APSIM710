#include "PastureUptake.h"


#pragma package(smart_init)
using namespace std;


#define singleArrayTypeDDML "<type  array=\"T\" kind=\"single\"/>"

// ------------------------------------------------------------------
// constructor
// ------------------------------------------------------------------
//===========================================================================
PastureUptake::PastureUptake(protocol::Component *s)
   {
      system = s;
   }
// ------------------------------------------------------------------
// constructor
// ------------------------------------------------------------------
//===========================================================================
PastureUptake::PastureUptake(protocol::Component *s, string uptakeNam, string deltaNam, string unitNam)
   {
      system = s;
      deltaName = deltaNam;
      uptakeName = uptakeNam;
      unitName = unitNam;
   }
// ------------------------------------------------------------------
// Destructor
// ------------------------------------------------------------------
PastureUptake::~PastureUptake(void)
//===========================================================================
   {
   }
// ------------------------------------------------------------------
// Init1 phase.
// ------------------------------------------------------------------
void PastureUptake::doInit1(const protocol::Init1Data& initData)
//===========================================================================
   {
      // get
   elementUptakeID = system->addRegistration(::get, -1, uptakeName.c_str(), singleArrayTypeDDML);

      // set
   dltElementID = system->addRegistration(::set, -1, deltaName.c_str(), singleArrayTypeDDML);
   }
// ------------------------------------------------------------------
// Init2 phase.
// ------------------------------------------------------------------
void PastureUptake::doInit2(void)
//===========================================================================
   {
   readParameters (); // Read constants
   }

// ------------------------------------------------------------------
void PastureUptake::doUptake(void)
//===========================================================================
{
      std::vector <float> elementUptake;
      system->getVariable(elementUptakeID, elementUptake, -1000.0, 1000.0);

      ostringstream msg;
      if (cDebug == "on")
         msg << endl <<  uptakeName + ":-" << endl;

      float uptakeTotal = 0.0;
      std::vector <float> dltElement;
      for (unsigned int layer = 0; layer != elementUptake.size(); layer++)
      {
         dltElement.push_back(-elementUptake[layer]);
         uptakeTotal +=  elementUptake[layer];

         if (cDebug == "on")
            msg << "   layer " << layer+1 << " = " << elementUptake[layer]  << " " << unitName << endl;
      }

   if (cDebug == "on")
   {
      msg << "   " << uptakeName << " total = " << uptakeTotal << " " << unitName << endl << ends;
      system->writeString (msg.str().c_str());
   }
   system->setVariable(dltElementID, dltElement);
}



void PastureUptake::readParameters ( void )
//===========================================================================
   {
    const char*  section_name = "parameters" ;
    cDebug = system->readParameter (section_name, "debug");
   }


