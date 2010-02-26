#include "PastureConverter.h"


#pragma package(smart_init)
using namespace std;


#define doubleArrayTypeDDML "<type  array=\"T\" kind=\"double\"/>"
#define singleArrayTypeDDML "<type  array=\"T\" kind=\"single\"/>"
#define singleTypeDDML "<type  kind=\"single\"/>"

      const float kg2g = 1000.0 ;
      const float ha2sm = 10000.0 ;
      const float g2kg = 1.0/kg2g ;
      const float sm2ha = 1.0/ha2sm ;
      const float cmol2mol = 1.0/100.0 ;
      const float mm2m = 1.0/1000.0;

// ------------------------------------------------------------------
// Return a blank string when requested to indicate that we
// don't need a wrapper DLL.
// ------------------------------------------------------------------
extern "C" _export void __stdcall wrapperDLL(char* wrapperDll)
   {
   strcpy(wrapperDll, "");
   }
extern "C" void __stdcall getDescriptionInternal(char* initScript,
                                                 char* description);
// ------------------------------------------------------------------
// Return component description info.
// ------------------------------------------------------------------
extern "C" _export void __stdcall getDescription(char* initScript, char* description)
   {
   getDescriptionInternal(initScript, description);
   }
// ------------------------------------------------------------------
// Create an instance of the science converter module
// ------------------------------------------------------------------
protocol::Component* createComponent(void)
//===========================================================================
   {
   return new PastureConverter;
   }
// ------------------------------------------------------------------
// constructor
// ------------------------------------------------------------------
//===========================================================================
PastureConverter::PastureConverter(void)
   {
   }
// ------------------------------------------------------------------
// Destructor
// ------------------------------------------------------------------
PastureConverter::~PastureConverter(void)
//===========================================================================
   {
      delete SW;
      delete NO3;
      delete NH4;
   }
// ------------------------------------------------------------------
// Init1 phase.
// ------------------------------------------------------------------
void PastureConverter::doInit1(const protocol::Init1Data& initData)
//===========================================================================
   {
   protocol::Component::doInit1(initData);

      // respondToGet
   sandID =    addRegistration(::respondToGet, -1, "sand", singleArrayTypeDDML);
   vpdID =     addRegistration(::respondToGet, -1, "vpd", singleTypeDDML);
   co2ppmID =     addRegistration(::respondToGet, -1, "co2_ppm", singleTypeDDML);

      // get
   maxtID =       addRegistration(::get, -1, "maxt", singleTypeDDML);
   mintID =       addRegistration(::get, -1, "mint", singleTypeDDML);

      // event
   sowID =            addRegistration(::event, -1, "sow", protocol::DDML(protocol::PastureSowType()).c_str());
   cutID =            addRegistration(::event, -1, "cut", protocol::DDML(protocol::PastureCutType()).c_str());
   cultivateID =      addRegistration(::event, -1, "cultivate", protocol::DDML(protocol::PastureCultivateType()).c_str());
   killID =           addRegistration(::event, -1, "kill", protocol::DDML(protocol::PastureKillType()).c_str());
   burnID =           addRegistration(::event, -1, "burn", protocol::DDML(protocol::PastureBurnType()).c_str());
 spraytopID =       addRegistration(::event,   -1, "spraytop", "");

//   residueAddedID =   addRegistration(RegistrationType::event, "residue_added", residueAddedTypeDDML);
   incorpFOMID =        addRegistration(::event, -1, "incorpfom", "");

      // respondToEvent
   sowPastureID =       addRegistration(::respondToEvent, -1, "sowpasture", singleTypeDDML);
   cutPastureID =       addRegistration(::respondToEvent, -1, "cutpasture", singleTypeDDML);
   cultivatePastureID = addRegistration(::respondToEvent, -1, "cultivatepasture", singleTypeDDML);
   killPastureID =      addRegistration(::respondToEvent, -1, "killpasture", singleTypeDDML);
   burnPastureID =      addRegistration(::respondToEvent, -1, "burnpasture", singleTypeDDML);
   spraytopPastureID =  addRegistration(::respondToEvent, -1, "spraytoppasture", singleTypeDDML);

   prepareID =          addRegistration(::respondToEvent, -1, "prepare", "");
   processID =          addRegistration(::respondToEvent, -1, "process", "");
   postID =             addRegistration(::respondToEvent, -1, "post", "");
   fomAddedID =         addRegistration(::respondToEvent, -1, "fom_added", protocol::DDML(protocol::FomAddedType()).c_str());

   SW = new PastureUptake(this, "sw_uptake", "dlt_sw_dep", "(mm)");
   NO3 = new PastureUptake(this, "uptake_no3", "dlt_no3", "(kg/ha)");
   NH4 = new PastureUptake(this, "uptake_nh4", "dlt_nh4", "(kg/ha)");

   SW->doInit1(initData);
   NO3->doInit1(initData);
   NH4->doInit1(initData);
}
// ------------------------------------------------------------------
// Init2 phase.
// ------------------------------------------------------------------
void PastureConverter::doInit2(void)
//===========================================================================
   {
   readParameters (); // Read constants
   SW->doInit2();
   NO3->doInit2();
   NH4->doInit2();
   }

// ------------------------------------------------------------------
// Event handler.
// ------------------------------------------------------------------
void PastureConverter::respondToEvent(unsigned int& fromID, unsigned int& eventID, protocol::Variant& variant)
//===========================================================================
{
   if (eventID == prepareID)
      doPrepare(fromID, eventID, variant);
   else if (eventID == processID)
      doProcess(fromID, eventID, variant);
   else if (eventID == postID)
      doPost(fromID, eventID, variant);
   else if (eventID == fomAddedID)
      doAddFOM(fromID, eventID, variant);
   else if (eventID == sowPastureID)
      dosowPasture(fromID, eventID, variant);
   else if (eventID == cutPastureID)
      docutPasture(fromID, eventID, variant);
   else if (eventID == cultivatePastureID)
      docultivatePasture(fromID, eventID, variant);
   else if (eventID == killPastureID)
      dokillPasture(fromID, eventID, variant);
   else if (eventID == burnPastureID)
      doburnPasture(fromID, eventID, variant);
   else if (eventID == spraytopPastureID)
      dospraytopPasture(fromID, eventID, variant);
   else
   {} //not interested an other events

}
// ------------------------------------------------------------------
void PastureConverter::doPrepare(unsigned int& fromID, unsigned int& eventID, protocol::Variant& variant)
//===========================================================================
{
}
// ------------------------------------------------------------------
void PastureConverter::doProcess(unsigned int& fromID, unsigned int& eventID, protocol::Variant& variant)
//===========================================================================
{
}
// ------------------------------------------------------------------
void PastureConverter::doPost(unsigned int& fromID, unsigned int& eventID, protocol::Variant& variant)
//===========================================================================
{
      doCropUptake(fromID, eventID, variant);
}

// ------------------------------------------------------------------
double PastureConverter::getVariableValue (protocol::Variant& variant, string eventName, string variableName, string unitName)
//===========================================================================
{
    double   value;
    protocol::ApsimVariant incomingApsimVariant(this);
    incomingApsimVariant.aliasTo(* variant.getMessageData());

    if (incomingApsimVariant.get(variableName.c_str(), protocol::DTdouble, false, value) == true)
    {
         ostringstream msg;
         msg << "Pasture " << eventName << " " << variableName << " = " << value << " " << unitName << ends;
         writeString (msg.str().c_str());
    }
    else
     value = 0.0;

    return value;
}

// ------------------------------------------------------------------
void PastureConverter::dosowPasture(unsigned int& fromID, unsigned int& eventID, protocol::Variant& variant)
//===========================================================================
{
   protocol::PastureSowType pastureSow;
   pastureSow.rate = getVariableValue (variant, "Sow", "rate", "(kg/ha)");

   publish (sowID, pastureSow);
}

// ------------------------------------------------------------------
void PastureConverter::docutPasture(unsigned int& fromID, unsigned int& eventID, protocol::Variant& variant)
//===========================================================================
{
   protocol::PastureCutType pastureCut;

   pastureCut.cut_height = getVariableValue (variant, "Cut", "cut_height", "(mm)");
   pastureCut.gathered =   getVariableValue (variant, "Cut", "gathered", "(-)");
   pastureCut.dmd_loss =   getVariableValue (variant, "Cut", "dmd_loss", "(-)");
   pastureCut.dm_content = getVariableValue (variant, "Cut", "dm_content", "(kg/kg)");

   publish (cutID, pastureCut);
}

// ------------------------------------------------------------------
void PastureConverter::docultivatePasture(unsigned int& fromID, unsigned int& eventID, protocol::Variant& variant)
//===========================================================================
{
   protocol::PastureCultivateType pastureCultivate;

   pastureCultivate.depth =        getVariableValue (variant, "Cultivate", "depth", "(mm)");
   pastureCultivate.propn_incorp = getVariableValue (variant, "Cultivate", "propn_incorp", "(-)");
   pastureCultivate.propn_mixed =  getVariableValue (variant, "Cultivate", "propn_mixed", "(-)");

   publish (cultivateID, pastureCultivate);
}

// ------------------------------------------------------------------
void PastureConverter::dokillPasture(unsigned int& fromID, unsigned int& eventID, protocol::Variant& variant)
//===========================================================================
{
   protocol::PastureKillType pastureKill;
   pastureKill.propn_herbage = getVariableValue (variant, "Kill", "propn_herbage", "(-)");
   pastureKill.propn_seed =    getVariableValue (variant, "Kill", "propn_seed", "(-)");

   publish (killID, pastureKill);
}

// ------------------------------------------------------------------
void PastureConverter::doburnPasture(unsigned int& fromID, unsigned int& eventID, protocol::Variant& variant)
//===========================================================================
{
     protocol::PastureBurnType pastureBurn;
   pastureBurn.kill_plants =   getVariableValue (variant, "Burn", "kill_plants", "(-)");
   pastureBurn.kill_seed =     getVariableValue (variant, "Burn", "kill_seed", "(-)");
   pastureBurn.propn_unburnt = getVariableValue (variant, "Burn", "propn_unburnt", "(-)");

   publish (burnID, pastureBurn);
}

// ------------------------------------------------------------------
void PastureConverter::dospraytopPasture(unsigned int& fromID, unsigned int& eventID, protocol::Variant& variant)
//===========================================================================
{
   char* null = "";
   publish (spraytopID, null);
}

// ------------------------------------------------------------------
void PastureConverter::doCropUptake(unsigned int& fromID, unsigned int& eventID, protocol::Variant& variant)
//===========================================================================
{
   SW->doUptake();
   NO3->doUptake();
   NH4->doUptake();
}

// ------------------------------------------------------------------
void PastureConverter::doAddFOM(unsigned int& fromID, unsigned int& eventID, protocol::Variant& variant)
//===========================================================================
{
   protocol::FomAddedType FOMAdded;

   variant.unpack(FOMAdded);
   int numLayers = FOMAdded.layers.size();
   float weightTotal = 0.0;
   float NTotal = 0.0;
   float PTotal = 0.0;
   float STotal = 0.0;
   float ashAlkTotal = 0.0;

   if (cDebug == "on")
   {
      ostringstream msg;
      msg << "FOM Added:" << endl;
      for (int layer = 0; layer < numLayers; layer++)
      {
         msg << "   layer (" << layer+1 << "): ";
         msg << "weight = " << FOMAdded.fom[layer].weight  << " (kg/ha); ";
         msg << "N = " << FOMAdded.fom[layer].n  << " (kg/ha); ";
         msg << "P = " << FOMAdded.fom[layer].p  << " (kg/ha); ";
         msg << "S = " << FOMAdded.fom[layer].s  << " (kg/ha); ";
         msg << "ash_alk = " << FOMAdded.fom[layer].ash_alk  << " (mol/ha) " << endl;
         weightTotal +=  FOMAdded.fom[layer].weight;
         NTotal +=  FOMAdded.fom[layer].n;
         PTotal +=  FOMAdded.fom[layer].p;
         STotal +=  FOMAdded.fom[layer].s;
         ashAlkTotal +=  FOMAdded.fom[layer].ash_alk;
      }

      msg << endl << "  Totals: ";
      msg << "weight = " << weightTotal << " (kg/ha); ";
      msg << "N = " << NTotal << " (kg/ha); ";
      msg << "P = " << PTotal << " (kg/ha); ";
      msg << "S = " << STotal << " (kg/ha); ";
      msg << "Ash Alk = " << ashAlkTotal << " (mol/ha)" << endl << ends;

      writeString (msg.str().c_str());
   }

   protocol::vector<float> dltDMincorp;
   protocol::vector<float> dltNincorp;
   protocol::vector<float> dltPincorp;

   for (int layer = 0; layer < numLayers; layer++)
   {
      dltDMincorp.push_back(FOMAdded.fom[layer].weight);
      dltNincorp.push_back(FOMAdded.fom[layer].n);
      dltPincorp.push_back(FOMAdded.fom[layer].p);
   }

   protocol::FOMLayerType IncorpFOM;
   IncorpFOM.Type = "pasture";
   for (unsigned i = 0; i != dltDMincorp.size(); i++)
      {
      protocol::FOMLayerLayerType Layer;
      Layer.FOM.amount = dltDMincorp[i];
      Layer.FOM.N = dltNincorp[i];
      Layer.FOM.P = dltPincorp[i];
      IncorpFOM.Layer.push_back(Layer);
      }
   publish (incorpFOMID, IncorpFOM);

}


// ------------------------------------------------------------------
// return a variable to caller.  Return true if we own variable.
// ------------------------------------------------------------------
void PastureConverter::respondToGet(unsigned int& fromID, protocol::QueryValueData& queryData)
//===========================================================================
{
     // sand_layer
   if (queryData.ID == sandID) sendSand(queryData);
   else if (queryData.ID == vpdID) sendVPD(queryData);
   else if (queryData.ID == co2ppmID) sendCO2(queryData);
   else
   {   // don't respond to any other gets.
   }
}

void PastureConverter::readParameters ( void )
//===========================================================================
   {
   const char*  section_name = "parameters" ;

   writeString (" - reading parameters");

   cDebug = readParameter (section_name, "debug");
   readParameter (section_name, "sand",      pSandLayer, 0.0, 1.0);
   readParameter (section_name, "svp_fract", cSVPFract, 0.0, 1.0);
   readParameter (section_name, "co2_ppm",   cCO2ppm, 0.0, 1000.0);

   ostringstream msg;
   msg << "debug = " << cDebug << endl;
   msg << "svp_fract = " << cSVPFract << endl;
   msg << "co2_ppm = " << cCO2ppm << endl;
   msg << "sand (kg/kg) = ";
   for (unsigned int layer = 0; layer < pSandLayer.size(); layer++)
      msg << pSandLayer[layer] << " ";
   msg << endl << ends;

   writeString (msg.str().c_str());

   }

void PastureConverter::sendSand (protocol::QueryValueData& queryData)
//===========================================================================
{
   vector <double> sandLayers;
   for (unsigned int layer = 0; layer != pSandLayer.size(); layer++)
      sandLayers.push_back(pSandLayer[layer]);

   if (cDebug == "on")
   {
      ostringstream msg;
      msg << "send sand (kg/kg) = ";
      for (unsigned int layer = 0; layer < pSandLayer.size(); layer++)
         msg << pSandLayer[layer] << " ";
      msg << endl << ends;
      writeString (msg.str().c_str());
   }

   sendVariable(queryData, sandLayers);
}

void PastureConverter::sendVPD (protocol::QueryValueData& queryData)
//==========================================================================
{
      float maxt;
      getVariable(maxtID, maxt, -100.0, 100.0);
      float mint;
      getVariable(mintID, mint, -100.0, 100.0);
      float VPD = vpd(cSVPFract, maxt, mint);
      sendVariable(queryData, VPD);
}

void PastureConverter::sendCO2 (protocol::QueryValueData& queryData)
//==========================================================================
{
    sendVariable(queryData, cCO2ppm);
}

float PastureConverter::vpd(float cSVPFract, float maxt, float mint) //(INPUT)
//==========================================================================
{
      return (max (cSVPFract * ( svp(maxt) - svp(mint)), 0.01));
}

//==========================================================================
float PastureConverter::svp(float temp) //(INPUT)  fraction of distance between svp at mi
//==========================================================================
/*  Purpose
*
*  Mission Statement
*    function to get saturation vapour pressure for a given temperature in oC (kpa)
*
*  Changes
*       21/5/2003 ad converted to BC++
*
*/
   {
      const double ES0 = 6.1078;            // Teten coefficients -SATURATION VAPOR PRESSURE (MB) OVER WATER AT 0C
      const double TC_B = 17.269388;        // Teten coefficients
      const double TC_C = 237.3;            // Teten coefficients
      const float mb2kpa = 100.0/1000.0;    // convert pressure mbar to kpa 1000 mbar = 100 kpa

   return  (ES0 * exp(TC_B * temp / (TC_C + temp)) * mb2kpa);
   }

