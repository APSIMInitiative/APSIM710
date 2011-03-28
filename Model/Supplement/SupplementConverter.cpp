#include <General/pch.h>
#include <boost/function.hpp>
#pragma hdrstop

#include <math.h>
#include <string>
#include <strstream>

#include <General/string_functions.h>
#include <General/stl_functions.h>
#include <ApsimShared/FStringExt.h>
#include <ComponentInterface/MessageDataExt.h>
#include <ComponentInterface/ApsimVariant.h>

#include <ComponentInterface/Component.h>
#include <ComponentInterface/DataTypes.h>
#include "SupplementConverter.h"


#pragma package(smart_init)
using namespace std;


#define doubleArrayTypeDDML "<type  array=\"T\" kind=\"double\"/>"
#define singleArrayTypeDDML "<type  array=\"T\" kind=\"single\"/>"
#define singleTypeDDML "<type kind=\"single\"/>"

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
// ------------------------------------------------------------------
// Create an instance of the science converter module
// ------------------------------------------------------------------
protocol::Component* createComponent(void)
//===========================================================================
   {
   return new SupplementConverter;
   }
// ------------------------------------------------------------------
// constructor
// ------------------------------------------------------------------
//===========================================================================
SupplementConverter::SupplementConverter(void)
   {
   }
// ------------------------------------------------------------------
// Destructor
// ------------------------------------------------------------------
SupplementConverter::~SupplementConverter(void)
//===========================================================================
   {
   }
// ------------------------------------------------------------------
// Init1 phase.
// ------------------------------------------------------------------
void SupplementConverter::doInit1(const protocol::Init1Data& initData)
//===========================================================================
   {
   protocol::Component::doInit1(initData);
//   buyID = addRegistration(RegistrationType::event, 0,"buy", supplementbuyTypeDDML);
   buySupplementID = addRegistration(::respondToEvent, 0,"buysupplement", singleTypeDDML);
   feedID = addRegistration(::event, 0, "feed", protocol::DDML(protocol::SupplementFeedType()).c_str());
   feedSupplementID = addRegistration(::respondToEvent, 0, "feedsupplement", singleTypeDDML);
   mixID = addRegistration(::event, 0, "mix", protocol::DDML(protocol::SupplementMixType()).c_str());
   mixSupplementID = addRegistration(::respondToEvent, 0, "mixsupplement", singleTypeDDML);

   }
// ------------------------------------------------------------------
// Init2 phase.
// ------------------------------------------------------------------
void SupplementConverter::doInit2(void)
//===========================================================================
   {
   readParameters (); // Read constants
   }

// ------------------------------------------------------------------
// Event handler.
// ------------------------------------------------------------------
void SupplementConverter::respondToEvent(unsigned int& fromID, unsigned int& eventID, protocol::Variant& variant)
//===========================================================================
{
   if (eventID == buySupplementID)
      dobuySupplement(fromID, eventID, variant);
   else if (eventID == feedSupplementID)
      dofeedSupplement(fromID, eventID, variant);
   else if (eventID == mixSupplementID)
      domixSupplement(fromID, eventID, variant);
   else
   {} //not interested an other events

}
// ------------------------------------------------------------------
void SupplementConverter::dobuySupplement(unsigned int& fromID, unsigned int& eventID, protocol::Variant& variant)
//===========================================================================
{
   protocol::SupplementBuyType SupplementBuy;

    std::string  valuestr;
    double   value;
    ostringstream msg;
    msg << "Supplement buy:" << endl;

    protocol::ApsimVariant incomingApsimVariant(variant);

    if (incomingApsimVariant.get("supplement", valuestr) == true)
    {
         SupplementBuy.supplement = valuestr;

         msg << "   Name = " << valuestr << endl;
    }
    else
         SupplementBuy.supplement = "";

    if (incomingApsimVariant.get("amount", value) == true)
    {
         SupplementBuy.amount = value;

         msg << "   Amount = " << value << " (kg)" << endl;
    }
    else
         SupplementBuy.amount = 0.0;

    msg << ends;
    writeString (msg.str().c_str());

   publish (buyID, SupplementBuy);
}

// ------------------------------------------------------------------
void SupplementConverter::dofeedSupplement(unsigned int& fromID, unsigned int& eventID, protocol::Variant& variant)
//===========================================================================
{
   protocol::SupplementFeedType SupplementFeed;

    std::string  valuestr;
    double   value;
    ostringstream msg;
    msg << "Supplement feed:" << endl;

    protocol::ApsimVariant incomingApsimVariant(variant);

    if (incomingApsimVariant.get("supplement", valuestr) == true)
    {
         SupplementFeed.supplement = valuestr;

         msg << "   Name = " << valuestr << endl;
    }
    else
         SupplementFeed.supplement = "";

    if (incomingApsimVariant.get("amount", value) == true)
    {
         SupplementFeed.amount = value;

         msg << "   Amount = " << value << " (kg)" << endl;
    }
    else
         SupplementFeed.amount = 0.0;

    if (incomingApsimVariant.get("paddock", valuestr) == true)
    {
         SupplementFeed.paddock = valuestr;

         msg << "   Paddock = " << valuestr << endl;
    }
    else
         SupplementFeed.paddock = "";

    msg << ends;
    writeString (msg.str().c_str());

   publish (feedID, SupplementFeed);
}

// ------------------------------------------------------------------
void SupplementConverter::domixSupplement(unsigned int& fromID, unsigned int& eventID, protocol::Variant& variant)
//===========================================================================
{
   protocol::SupplementMixType SupplementMix;

    std::string  valuestr;
    double   value;
    ostringstream msg;
    msg << "Supplement mix:" << endl;

    protocol::ApsimVariant incomingApsimVariant(variant);

    if (incomingApsimVariant.get("src_store", valuestr) == true)
    {
         SupplementMix.src_store = valuestr;

         msg << "   Src_store = " << valuestr << endl;
    }
    else
         SupplementMix.src_store = "";

    if (incomingApsimVariant.get("amount", value) == true)
    {
         SupplementMix.amount = value;

         msg << "   Amount = " << value << " (kg)" << endl;
    }
    else
         SupplementMix.amount = 0.0;

    if (incomingApsimVariant.get("dest_store", valuestr) == true)
    {
         SupplementMix.dest_store = valuestr;

         msg << "   Dest_store = " << valuestr << endl;
    }
    else
         SupplementMix.dest_store = "";

    msg << ends;
    writeString (msg.str().c_str());

   publish (mixID, SupplementMix);
}

// ------------------------------------------------------------------
// return a variable to caller.  Return true if we own variable.
// ------------------------------------------------------------------
void SupplementConverter::respondToGet(unsigned int& fromID, protocol::QueryValueData& queryData)
//===========================================================================
{
//   if (queryData.ID == sandID) sendSand(queryData);
//
//   else
//   {   // don't respond to any other gets.
//   }
}

void SupplementConverter::readParameters ( void )
//===========================================================================
{
   const char*  section_name = "parameters" ;

   writeString (" - reading parameters");

   cDebug = readParameter (section_name, "debug");

   std::string supplementModuleName = readParameter (section_name, "supplement_module");

   ostringstream msg;
   msg << "Debug = " << cDebug << endl;
   msg << "supplement_module = " << supplementModuleName;
   msg << endl << ends;
   writeString (msg.str().c_str());

   int moduleID = 0;
   componentNameToID(supplementModuleName, moduleID);
   buyID = addRegistration(::event, moduleID, "buy", protocol::DDML(protocol::SupplementBuyType()).c_str());
}




