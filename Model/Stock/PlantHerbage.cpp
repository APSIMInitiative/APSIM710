#include "PlantHerbage.h"
#pragma hdrstop
#include "Constants.h"

using namespace std;


#define singleArrayTypeDDML \
   "<type  array=\"T\" kind=\"single\"/>"
#define singleTypeDDML \
   "<type  kind=\"single\"/>"
#define stringTypeDDML \
   "<type  kind=\"string\"/>"

      const int  GREEN = 0 ;
      const int  SENESCED = 1 ;

 //      indices of plant part names
      const int  ROOT = 0 ;
      const int  LEAF = 1 ;
      const int  STEM = 2 ;
      const int  POD  = 3 ;
      const int  MEAL = 4 ; // excludes oil component
      const int  OIL  = 5 ; // seed oil


      inline bool floatsAreEqual(float A, float B, float C) {return(fabs(A-B)<C);}
      float divide (float dividend, float divisor, float default_value);

// ------------------------------------------------------------------
// default constructor
// ------------------------------------------------------------------
PlantHerbage::PlantHerbage(void)
   {
      initialiseData();
   }
// ------------------------------------------------------------------
// constructor
// ------------------------------------------------------------------
PlantHerbage::PlantHerbage(protocol::Component *s) : HerbageBase(s)
   {
////      system = s;
      initialiseData();
   }
// ------------------------------------------------------------------
// destructor
// ------------------------------------------------------------------
PlantHerbage::~PlantHerbage(void)
   {
   }
// ------------------------------------------------------------------
// Init1 phase.
// ------------------------------------------------------------------
void PlantHerbage::doInit1(const protocol::Init1Data&)
   {
//   protocol::Component::doInit1(sdml);

   }

// ------------------------------------------------------------------
// Init2 phase.
// ------------------------------------------------------------------
void PlantHerbage::doInit2(void)
   {
      readParameters (); // Read constants
      readHerbageModuleParameters ();
      doRunTimeReg ();
      cropMatureStageNo = 1000;
   }

void PlantHerbage::doDigestibility(void)
{
      getVariables();

////      calcDmdDecline();

      if (dmVeg.total() > 0.0)
      {

// distribute herbage


      calcDmdDistribution(dmdFractionVeg);

      if (cDebug == "on")
      {
         for (int pool = 0; pool < cNumDmdPoolsVeg; pool++)
         {
            ostringstream msgFraction;
            msgFraction << endl << "Plant Herbage dmd distribution, pool " << (pool+1) << ":-" << endl;
            msgFraction << dmdFractionVeg[pool];
            system->writeString (msgFraction.str().c_str());
         }

         for (int pool = 0; pool < cNumDmdPoolsSeed; pool++)
         {
            ostringstream msgFraction;
            msgFraction << endl << "Plant Seed dmd distribution, pool " << (pool+1) << ":-" << endl;
            msgFraction << dmdFractionSeed[pool];
            system->writeString (msgFraction.str().c_str());
         }
      }
   }
}

void PlantHerbage::doRegister(const int partNo, const string& partName)
   {
   // register interest in plant part parameters.

      dmGreenID[partNo] = system->addRegistration(::get, cHerbageModuleID, string( partName+"GreenWt"), singleTypeDDML);   // parameter crop name=lablab
      nGreenID[partNo] = system->addRegistration(::get, cHerbageModuleID, string( partName+"GreenN"), singleTypeDDML);   // parameter crop name=lablab
      pGreenID[partNo] = system->addRegistration(::get, cHerbageModuleID, string( partName+"GreenP"), singleTypeDDML);   // parameter crop name=lablab
      dmdMaxGreenID[partNo] = system->addRegistration(::get, cHerbageModuleID, string(partName+"GreenDigestibilityMax"), singleTypeDDML);   // parameter crop name=lablab
      dmdAvgGreenID[partNo] = system->addRegistration(::get, cHerbageModuleID, string(partName+"GreenDigestibilityAvg"), singleTypeDDML);   // parameter crop name=lablab
      dmdMinGreenID[partNo] = system->addRegistration(::get, cHerbageModuleID, string(partName+"GreenDigestibilityMin"), singleTypeDDML);   // parameter crop name=lablab

      dmSenescedID[partNo] = system->addRegistration(::get, cHerbageModuleID, string(partName+"SenescedWt"), singleTypeDDML,"");   // parameter crop name=lablab
      nSenescedID[partNo] = system->addRegistration(::get, cHerbageModuleID, string(partName+"SenescedN"), singleTypeDDML,""  );   // parameter crop name=lablab
      pSenescedID[partNo] = system->addRegistration(::get, cHerbageModuleID, string(partName+"SenescedP"), singleTypeDDML,""  );   // parameter crop name=lablab
      dmdMaxSenescedID[partNo] = system->addRegistration(::get, cHerbageModuleID, string(partName+"SenescedDigestibilityMax") , singleTypeDDML,"");   // parameter crop name=lablab
      dmdAvgSenescedID[partNo] = system->addRegistration(::get, cHerbageModuleID, string(partName+"SenescedDigestibilityAvg") , singleTypeDDML,"");   // parameter crop name=lablab
      dmdMinSenescedID[partNo] = system->addRegistration(::get, cHerbageModuleID, string(partName+"SenescedDigestibilityMin") , singleTypeDDML,"");   // parameter crop name=lablab
   }


// ------------------------------------------------------------------
// Runtime Registrations.
// ------------------------------------------------------------------
void PlantHerbage::doRunTimeReg(void)
   {

   doRegister(LEAF, "leaf");
   doRegister(STEM, "stem");
   doRegister(POD, "pod");
   doRegister(MEAL, "meal");
   doRegister(OIL, "oil");

   tramplingID = system->addRegistration(::get, -1, "trampling", singleTypeDDML);
   heightID = system->addRegistration(::get, cHerbageModuleID, "height", singleTypeDDML);
   stageID = system->addRegistration(::get, cHerbageModuleID, "stage", singleTypeDDML);
   stageNameID = system->addRegistration(::get, cHerbageModuleID,"stagename", stringTypeDDML);

    removeCropBiomassID = system->addRegistration(::event, cHerbageModuleID,"remove_crop_biomass", DDML(protocol::RemoveCropDmType()));
    detachRateID = system->addRegistration(::event, cHerbageModuleID, "detach_crop_biomass_rate", "");

//   dmFeedOnOfferID = system->addRegistration(::respondToGet, -1, "dm_feed_on_offer", singleArrayTypeDDML);
     dmPartsRemovedID = system->addRegistration(::respondToGet, -1, "dm_parts_removed", singleArrayTypeDDML);
  }

// ------------------------------------------------------------------
// return a variable to caller.  Return true if we own variable.
// ------------------------------------------------------------------
void PlantHerbage::respondToGet(unsigned int& fromID, protocol::QueryValueData& queryData)
{
   if (queryData.ID == dmPartsRemovedID) sendPartsRemoved(queryData);
   else
   { }  // don't respond to any other gets.
}


void PlantHerbage::sendPartsRemoved(protocol::QueryValueData& queryData)
{
      float dmPartsRemoved[15] = {0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0};
      int num_parts = crop.dm.size() * crop.dm[0].part.size();
      if (num_parts > 0)
      {
         int i = 0;
         for (unsigned int pool=0; pool < crop.dm.size(); pool++)
         {
            for (unsigned int part = 0; part < crop.dm[pool].part.size(); part++)
            {
               i ++;
               dmPartsRemoved[i] =  crop.dm[pool].dlt[part];
            }
         }

      }
      else
      {
         num_parts = 15;
      }

      system->sendVariable(queryData, vector <float> (dmPartsRemoved, dmPartsRemoved+num_parts));
}

// ------------------------------------------------------------------
// Event handler.
// ------------------------------------------------------------------
void PlantHerbage::doGrazed(protocol::RemoveHerbageType &grazed)
{

      doDmdPoolsToHerbageParts(grazed, crop);

      float dmTotal = 0.0;
      for (unsigned int pool=0; pool < crop.dm.size(); pool++)
      {
         for (unsigned int part = 0; part < crop.dm[pool].part.size(); part++)
         {
            dmTotal +=  crop.dm[pool].dlt[part];
         }
      }

      if (cDebug == "on")
      {
         ostringstream msg;
         msg << endl << "Remove herbage plant parts:-" << endl;

         for (unsigned int pool=0; pool < crop.dm.size(); pool++)
         {
            for (unsigned int part = 0; part < crop.dm[pool].part.size(); part++)
            {
               msg << "   dm " << crop.dm[pool].pool << " " << crop.dm[pool].part[part] << " = " << crop.dm[pool].dlt[part] << " (g/m2)" << endl;
            }
         }

         msg << endl << "   dm total = " << dmTotal << " (g/m2)" << ends;

         system->writeString (msg.str().c_str());

      }

      if (dmTotal > 1.0e-6)
      {
         system->publish (removeCropBiomassID, crop);
      }

      if (cDebug == "on")
      {
         if (trampling() > 1.0e-6 && trampling() < 1.0e+6)
         {
            ostringstream msg;
            msg << endl << "Detach herbage plant parts:-" << endl;

            float detachRate = trampling() * c.specificDetachRate;
            msg << "   Detach fraction = " << detachRate << " ()" << endl << ends;
            system->writeString (msg.str().c_str());

            system->publish (detachRateID, detachRate);
         }
      }

}

// ------------------------------------------------------------------
// Event handler.
// ------------------------------------------------------------------
void PlantHerbage::doDmdPoolsToHerbageParts(protocol::RemoveHerbageType &grazed, protocol::RemoveCropDmType &crop)
{
      for (int pool = 0; pool < numDmdPoolsVeg(); pool++)
      {
         PlantPool poolDm = dmVeg * dmdFractionVeg[pool];
         float dmTot = poolDm.total();
         partFractionVeg[pool] = poolDm / dmTot;
      }

      for (int pool = 0; pool < numDmdPoolsSeed(); pool++)
      {
         SeedPool poolDm = dmSeed * dmdFractionSeed[pool];
         float dmTot = poolDm.total();
         partFractionSeed[pool] = poolDm / dmTot;
      }

      protocol::RemoveCropDmdmType dm;
      crop.dm.erase(crop.dm.begin(), crop.dm.end());
      dm.dlt.erase(dm.dlt.begin(), dm.dlt.end());
      dm.part.erase(dm.part.begin(), dm.part.end());

      dm.pool = "green";
      dm.part.push_back("leaf");
      float dmPart = 0.0;
      for (int pool = 0; pool < numDmdPoolsVeg(); pool++) dmPart += grazed.herbage[pool]*partFractionVeg[pool].green.leaf;
      dm.dlt.push_back(dmPart * kg2g / ha2sm);

      dm.part.push_back("stem");
      dmPart = 0.0;
      for (int pool = 0; pool < numDmdPoolsVeg(); pool++) dmPart += grazed.herbage[pool]*partFractionVeg[pool].green.stem;
      dm.dlt.push_back(dmPart * kg2g / ha2sm);

      dm.part.push_back("pod");
      dmPart = 0.0;
      for (int pool = 0; pool < numDmdPoolsVeg(); pool++) dmPart += grazed.herbage[pool]*partFractionVeg[pool].green.pod;
      dm.dlt.push_back(dmPart * kg2g / ha2sm);

      dm.part.push_back("meal");
      dmPart = 0.0;
      for (int pool = 0; pool < numDmdPoolsSeed(); pool++) dmPart += grazed.seed[pool]*partFractionSeed[pool].green.meal;
      dm.dlt.push_back(dmPart * kg2g / ha2sm);

      dm.part.push_back("oil");
      dmPart = 0.0;
      for (int pool = 0; pool < numDmdPoolsSeed(); pool++) dmPart += grazed.seed[pool]*partFractionSeed[pool].green.oil;
      dm.dlt.push_back(dmPart * kg2g / ha2sm);

      crop.dm.push_back(dm);
      dm.dlt.erase(dm.dlt.begin(), dm.dlt.end());
      dm.part.erase(dm.part.begin(), dm.part.end());

      dm.pool = "senesced";
      dm.part.push_back("leaf");
      dmPart = 0.0;
      for (int pool = 0; pool < numDmdPoolsVeg(); pool++) dmPart += grazed.herbage[pool]*partFractionVeg[pool].senesced.leaf;
      dm.dlt.push_back(dmPart * kg2g / ha2sm);

      dm.part.push_back("stem");
      dmPart = 0.0;
      for (int pool = 0; pool < numDmdPoolsVeg(); pool++) dmPart += grazed.herbage[pool]*partFractionVeg[pool].senesced.stem;
      dm.dlt.push_back(dmPart * kg2g / ha2sm);

      dm.part.push_back("pod");
      dmPart = 0.0;
      for (int pool = 0; pool < numDmdPoolsVeg(); pool++) dmPart += grazed.herbage[pool]*partFractionVeg[pool].senesced.pod;
      dm.dlt.push_back(dmPart * kg2g / ha2sm);

      dm.part.push_back("meal");
      dmPart = 0.0;
      for (int pool = 0; pool < numDmdPoolsSeed(); pool++) dmPart += grazed.seed[pool]*partFractionSeed[pool].senesced.meal;
      dm.dlt.push_back(dmPart * kg2g / ha2sm);

      dm.part.push_back("oil");
      dmPart = 0.0;
      for (int pool = 0; pool < numDmdPoolsSeed(); pool++) dmPart += grazed.seed[pool]*partFractionSeed[pool].senesced.oil;
      dm.dlt.push_back(dmPart * kg2g / ha2sm);

      crop.dm.push_back(dm);
      dm.dlt.erase(dm.dlt.begin(), dm.dlt.end());
      dm.part.erase(dm.part.begin(), dm.part.end());
}

float PlantHerbage::getPart(unsigned &partID)
{
      float partValue;
      bool ok = system->getVariable(partID, partValue, -10000.0, 10000.0, true);
      if (ok)
         return partValue;
      throw std::runtime_error("Couldn't get Leaf variable partsID");
}

void PlantHerbage::getParts(PlantPartType &parts, SeedPartType &seedParts, unsigned partsID[])
{
      parts.leaf = getPart(partsID[LEAF]);
      parts.stem = getPart(partsID[STEM]);
      parts.pod = getPart(partsID[POD]);
      seedParts.meal = getPart(partsID[MEAL]);
      seedParts.oil = getPart(partsID[OIL])  ;
}

void PlantHerbage::getPGreen(PlantPartType &pGreen, PlantPool &dm, SeedPartType &pGreenSeed, SeedPool &dmSeed, unsigned partsID[])
{
         float P;
         bool ok = system->getVariable(partsID[STEM], P, 0.0, 10000.0, true);
         if (ok)
         {
               if (P > 0.0)
               {
                  pGreen.leaf = getPart(partsID[LEAF]) * g2kg/sm2ha;
                  pGreen.stem = getPart(partsID[STEM]) * g2kg/sm2ha;
                  pGreen.pod = getPart(partsID[POD]) * g2kg/sm2ha;
                  pGreenSeed.meal = getPart(partsID[MEAL]) * g2kg/sm2ha;
                  pGreenSeed.oil = getPart(partsID[OIL]) * g2kg/sm2ha;

               }
               else
               {
                  pGreen.stem = c.pConcGreenStemDefault * dm.green.stem;  // parameter for default P contents
                  pGreen.leaf = c.pConcGreenLeafDefault * dm.green.leaf;
                  pGreen.pod = c.pConcGreenPodDefault * dm.green.pod;
                  pGreenSeed.meal = c.pConcGreenMealDefault * dmSeed.green.meal;
                  pGreenSeed.oil = c.pConcGreenOilDefault * dmSeed.green.oil;
               }
         }
         else
         {
            throw std::runtime_error("Couldn't get variable pGreenID");
         }
}

void PlantHerbage::getPSenesced(PlantPartType &pSenesced, PlantPool &dm, SeedPartType &pSenescedSeed, SeedPool &dmSeed, unsigned partsID[])
{
         float P;
         bool ok = system->getVariable(partsID[LEAF], P, 0.0, 10000.0, true);
         if (ok)
         {
               if (P > 0.0)
               {
                  pSenesced.leaf = getPart(partsID[LEAF]) * g2kg/sm2ha;
                  pSenesced.stem = getPart(partsID[STEM]) * g2kg/sm2ha;
                  pSenesced.pod = getPart(partsID[POD]) * g2kg/sm2ha;
                  pSenescedSeed.meal = getPart(partsID[MEAL]) * g2kg/sm2ha;
                  pSenescedSeed.oil = getPart(partsID[OIL]) * g2kg/sm2ha;

               }
               else
               {
                  pSenesced.stem = c.pConcSenescedStemDefault * dm.senesced.stem;  // parameter for default P contents
                  pSenesced.leaf = c.pConcSenescedLeafDefault * dm.senesced.leaf;
                  pSenesced.pod = c.pConcSenescedPodDefault * dm.senesced.pod;
                  pSenescedSeed.meal = c.pConcSenescedMealDefault * dmSeed.senesced.meal;
                  pSenescedSeed.oil = c.pConcSenescedOilDefault * dmSeed.senesced.oil;
               }
         }
         else
         {
            throw std::runtime_error("Couldn't get variable pSenescedID");
         }
}

void PlantHerbage::getTrampling(void)
{
      protocol::Variant* variant;
         bool ok = system->getVariable(tramplingID, &variant, true);
         if (ok)
         {
            bool ok = variant->unpack(eTrampling);
            if (ok)
            { // do nothing
            }
            else
            {
               throw std::runtime_error("Couldn't unpack trampling");
            }
         }
         else
         {
            throw std::runtime_error("Couldn't get variable tramplingID");
         }
}

void PlantHerbage::getHeight(float &height)
{
         if (!system->getVariable(heightID, height, 0.0, 10000.0, true))
         {
            throw std::runtime_error("Couldn't get variable heightID");
         }
}

void PlantHerbage::getStage()
{
         if (!system->getVariable(stageID, cropStageNo, 0.0, 1000.0, true))
         {
            throw std::runtime_error("Couldn't get variable stageID");
         }
         protocol::Variant *variant = NULL;
         bool ok = system->getVariable(stageNameID, &variant, true);
         if (ok)
         {
            bool ok = variant->unpack(cropStageName);
            if (ok)
            {  // do nothing
            }
            else
            {
               throw std::runtime_error("Couldn't unpack cropStageName");
            }
         }
         else
         {
            throw std::runtime_error("Couldn't get variable stageNameID");
         }
         if (cropStageName == "maturity")
         {
            cropMatureStageNo = cropStageNo;
         }
         else
         {  // leave as is
         }
}

void PlantHerbage::getVariables(void)
{
         // Get dm GREEN                                  // get part names: crop type (residue) or plant parts (leaf, stem etc).
      PlantPartType dmGreen;                              // get type names: standing,lying or green, senesced, dead
      SeedPartType dmGreenSeed;                              // get type names: standing,lying or green, senesced, dead
      getParts(dmGreen, dmGreenSeed, dmGreenID);                       // get element: dm, N, P, ash alk
                                                         // get dm deltas?
         // Get dm SENESCED
      PlantPartType dmSenesced;
      SeedPartType dmSenescedSeed;
      getParts(dmSenesced, dmSenescedSeed, dmSenescedID);

      dmVeg.setValue(dmGreen, dmSenesced);
      dmSeed.setValue(dmGreenSeed, dmSenescedSeed);
      dmVeg = dmVeg * g2kg/sm2ha;
      dmSeed = dmSeed * g2kg/sm2ha;

      if ((dmVeg.total() + dmSeed.total()) > 0.0)
      {
            // Get N GREEN
         PlantPartType nGreen;
         SeedPartType nGreenSeed;
         getParts(nGreen, nGreenSeed, nGreenID);

         // Get N SENESCED
         PlantPartType nSenesced;
         SeedPartType nSenescedSeed;
         getParts(nSenesced, nSenescedSeed, nSenescedID);

         NVeg.setValue(nGreen, nSenesced);
         NSeed.setValue(nGreenSeed, nSenescedSeed);
         NVeg = NVeg * g2kg/sm2ha;
         NSeed = NSeed * g2kg/sm2ha;

         // Get P GREEN
         PlantPartType pGreen;
         SeedPartType pGreenSeed;
         getPGreen(pGreen, dmVeg, pGreenSeed, dmSeed, pGreenID);

         // Get P SENESCED
         PlantPartType pSenesced;
         SeedPartType pSenescedSeed;
         getPSenesced(pSenesced, dmVeg, pSenescedSeed, dmSeed, pSenescedID);

         PVeg.setValue(pGreen, pSenesced);
         PSeed.setValue(pGreenSeed, pSenescedSeed);
         PVeg = PVeg * g2kg/sm2ha;
         PSeed = PSeed * g2kg/sm2ha;

            // Get dmd max GREEN                                  // get part names: crop type (residue) or plant parts (leaf, stem etc).
         PlantPartType dmdMaxGreen;                              // get type names: standing,lying or green, senesced, dead
         SeedPartType dmdMaxGreenSeed;                              // get type names: standing,lying or green, senesced, dead
         getParts(dmdMaxGreen, dmdMaxGreenSeed, dmdMaxGreenID);                       // get element: dm, N, P, ash alk
                                                             // get dm deltas?
            // Get dmd average GREEN                                  // get part names: crop type (residue) or plant parts (leaf, stem etc).
         PlantPartType dmdAvgGreen;                              // get type names: standing,lying or green, senesced, dead
         SeedPartType dmdAvgGreenSeed;                              // get type names: standing,lying or green, senesced, dead
         getParts(dmdAvgGreen, dmdAvgGreenSeed, dmdAvgGreenID);                       // get element: dm, N, P, ash alk
                                                             // get dm deltas?
            // Get dmd min GREEN                                  // get part names: crop type (residue) or plant parts (leaf, stem etc).
         PlantPartType dmdMinGreen;                              // get type names: standing,lying or green, senesced, dead
         SeedPartType dmdMinGreenSeed;                              // get type names: standing,lying or green, senesced, dead
         getParts(dmdMinGreen, dmdMinGreenSeed, dmdMinGreenID);                       // get element: dm, N, P, ash alk
                                                             // get dm deltas?
            // Get dmd max SENESCED
         PlantPartType dmdMaxSenesced;
         SeedPartType dmdMaxSenescedSeed;
         getParts(dmdMaxSenesced, dmdMaxSenescedSeed, dmdMaxSenescedID);

            // Get dmd average SENESCED
         PlantPartType dmdAvgSenesced;
         SeedPartType dmdAvgSenescedSeed;
         getParts(dmdAvgSenesced, dmdAvgSenescedSeed, dmdAvgSenescedID);

            // Get dmd min SENESCED
         PlantPartType dmdMinSenesced;
         SeedPartType dmdMinSenescedSeed;
         getParts(dmdMinSenesced, dmdMinSenescedSeed, dmdMinSenescedID);

         dmdMaxVeg.setValue(dmdMaxGreen, dmdMaxSenesced);
         dmdAvgVeg.setValue(dmdAvgGreen, dmdAvgSenesced);
         dmdMinVeg.setValue(dmdMinGreen, dmdMinSenesced);
         dmdUnripeSeed.setValue(dmdAvgGreenSeed, dmdAvgSenescedSeed);
         dmdRipeSeed.setValue(dmdAvgGreenSeed, dmdAvgSenescedSeed);
        // Get HEIGHT
//         PlantPool  heightRatio;

         getHeight(height);
        // Get plant stage and seed maturity
         getStage();
         getTrampling();
      }

}


//void PlantHerbage::calcDmdClass(void)
//{
//
//// get GREEN Leaf, stem, pod dmd classes
//      dmdClass (dmdMaxVeg.green.leaf, dmdMinVeg.green.leaf, dmdClassMaxVeg.green.leaf, dmdClassMinVeg.green.leaf);
//      dmdClass (dmdMaxVeg.green.stem, dmdMinVeg.green.stem, dmdClassMaxVeg.green.stem, dmdClassMinVeg.green.stem);
//      dmdClass (dmdMaxVeg.green.pod, dmdMinVeg.green.pod, dmdClassMaxVeg.green.pod, dmdClassMinVeg.green.pod);
//
//// get SENESCED Leaf, stem, pod dmd classes
//      dmdClass (dmdMaxVeg.senesced.leaf, dmdMinVeg.senesced.leaf, dmdClassMaxVeg.senesced.leaf, dmdClassMinVeg.senesced.leaf);
//      dmdClass (dmdMaxVeg.senesced.stem, dmdMinVeg.senesced.stem, dmdClassMaxVeg.senesced.stem, dmdClassMinVeg.senesced.stem);
//      dmdClass (dmdMaxVeg.senesced.pod, dmdMinVeg.senesced.pod, dmdClassMaxVeg.senesced.pod, dmdClassMinVeg.senesced.pod);
//
//// get DEAD Leaf, stem, pod dmd classes
//      dmdClass (dmdMaxVeg.dead.leaf, dmdMinVeg.dead.leaf, dmdClassMaxVeg.dead.leaf, dmdClassMinVeg.dead.leaf);
//      dmdClass (dmdMaxVeg.dead.stem, dmdMinVeg.dead.stem, dmdClassMaxVeg.dead.stem, dmdClassMinVeg.dead.stem);
//      dmdClass (dmdMaxVeg.dead.pod, dmdMinVeg.dead.pod, dmdClassMaxVeg.dead.pod, dmdClassMinVeg.dead.pod);
//}
//
void PlantHerbage::calcDmdDistribution(PlantPool dmdFraction[])
{
////      PlantPool dmdAvgVeg = dmdMaxVeg - dQVeg;

      float fraction[maxDmdPoolsVeg];
      float fractionSeed[maxDmdPoolsSeed];

// get GREEN Leaf dmd fractions
      for (int pool = 0; pool < numDmdPoolsVeg(); pool++) fraction[pool] = 0.0;
      proportion (dmdAvgVeg.green.leaf, dmdMaxVeg.green.leaf, dmdMinVeg.green.leaf, fraction);
      for (int pool = 0; pool < numDmdPoolsVeg(); pool++) dmdFraction[pool].green.leaf = fraction[pool];

// get GREEN stem dmd fractions
      for (int pool = 0; pool < numDmdPoolsVeg(); pool++) fraction[pool] = 0.0;
      proportion (dmdAvgVeg.green.stem, dmdMaxVeg.green.stem, dmdMinVeg.green.stem, fraction);
      for (int pool = 0; pool < numDmdPoolsVeg(); pool++) dmdFraction[pool].green.stem = fraction[pool];

// get GREEN pod dmd fractions
      for (int pool = 0; pool < numDmdPoolsVeg(); pool++) fraction[pool] = 0.0;
      proportion (dmdAvgVeg.green.pod, dmdMaxVeg.green.pod, dmdMinVeg.green.pod, fraction);
      for (int pool = 0; pool < numDmdPoolsVeg(); pool++) dmdFraction[pool].green.pod = fraction[pool];

// get GREEN meal dmd fractions
      for (int pool = 0; pool < numDmdPoolsSeed(); pool++) fractionSeed[pool] = 0.0;
      proportion (dmdUnripeSeed.green.meal, dmdRipeSeed.green.meal, fractionSeed);
      for (int pool = 0; pool < numDmdPoolsSeed(); pool++) dmdFractionSeed[pool].green.meal = fractionSeed[pool];

// get GREEN oil dmd fractions
      for (int pool = 0; pool < numDmdPoolsSeed(); pool++) fractionSeed[pool] = 0.0;
      proportion (dmdUnripeSeed.green.oil, dmdRipeSeed.green.oil, fractionSeed);
      for (int pool = 0; pool < numDmdPoolsSeed(); pool++) dmdFractionSeed[pool].green.oil = fractionSeed[pool];

// get SENESCED Leaf dmd fractions
      for (int pool = 0; pool < numDmdPoolsVeg(); pool++) fraction[pool] = 0.0;
      proportion (dmdAvgVeg.senesced.leaf, dmdMaxVeg.senesced.leaf, dmdMinVeg.senesced.leaf, fraction);
      for (int pool = 0; pool < numDmdPoolsVeg(); pool++) dmdFraction[pool].senesced.leaf = fraction[pool];

// get SENESCED stem dmd fractions
      for (int pool = 0; pool < numDmdPoolsVeg(); pool++) fraction[pool] = 0.0;
      proportion (dmdAvgVeg.senesced.stem, dmdMaxVeg.senesced.stem, dmdMinVeg.senesced.stem, fraction);
      for (int pool = 0; pool < numDmdPoolsVeg(); pool++) dmdFraction[pool].senesced.stem = fraction[pool];

// get SENESCED pod dmd fractions
      for (int pool = 0; pool < numDmdPoolsVeg(); pool++) fraction[pool] = 0.0;
      proportion (dmdAvgVeg.senesced.pod, dmdMaxVeg.senesced.pod, dmdMinVeg.senesced.pod, fraction);
      for (int pool = 0; pool < numDmdPoolsVeg(); pool++) dmdFraction[pool].senesced.pod = fraction[pool];

// get SENESCED meal dmd fractions
      for (int pool = 0; pool < numDmdPoolsSeed(); pool++) fractionSeed[pool] = 0.0;
      proportion (dmdUnripeSeed.senesced.meal, dmdRipeSeed.senesced.meal, fractionSeed);
      for (int pool = 0; pool < numDmdPoolsSeed(); pool++) dmdFractionSeed[pool].senesced.meal = fractionSeed[pool];

// get SENESCED oil dmd fractions
      for (int pool = 0; pool < numDmdPoolsSeed(); pool++) fractionSeed[pool] = 0.0;
      proportion (dmdUnripeSeed.senesced.oil, dmdRipeSeed.senesced.oil, fractionSeed);
      for (int pool = 0; pool < numDmdPoolsSeed(); pool++) dmdFractionSeed[pool].senesced.oil = fractionSeed[pool];
}

void PlantHerbage::initialiseData ( void )
{
      removeHerbageID = 0;

      dmFeedOnOfferID = 0;
      dmFeedRemovedID = 0;
      removeCropBiomassID = 0;
      detachRateID = 0;

      for (int pool = 0; pool < maxDmdPoolsVeg; pool++)
      {
         dmdFractionVeg[pool] = 0.0;
         dmdPoolDmVeg[pool] = 0.0;
         partFractionVeg[pool] = 0.0;
      }
      dmdMaxVeg = 0.0;
      dmdAvgVeg = 0.0;
      dmdMinVeg = 0.0;

      dmdMaxSeed = 0.0;
      dmdAvgSeed = 0.0;
      dmdMinSeed = 0.0;

      dmdClassMaxVeg = 0.0;
      dmdClassMinVeg = 0.0;

      dmVeg = 0.0;
      NVeg = 0.0;
      PVeg = 0.0;
      dmdVeg = 0.0;
      eTrampling = 0.0;
      height = 0.0;
      cropMatureStageNo = 0;
      cropStageNo = 0.0;
      string cropStageName = "";

      for (int pool = 0; pool < maxDmdPoolsSeed; pool++)
      {
         dmdFractionSeed[pool] = 0.0;
         dmdPoolDmSeed[pool] = 0.0;
         partFractionSeed[pool] = 0.0;
      }
      dmdUnripeSeed = 0.0;
      dmdRipeSeed = 0.0;

      dmdClassMaxSeed = 0.0;
      dmdClassMinSeed = 0.0;

      dmSeed = 0.0;
      NSeed = 0.0;
      PSeed = 0.0;
      dmdSeed = 0.0;

      cHerbageModuleID = -1;
      cDebug = "";

      for (int pool = 0; pool < maxDmdPoolsVeg; pool++)
         cDmdValueVeg[pool] = 0.0;
      cNumDmdPoolsVeg = 0.0;

      for (int pool = 0; pool < maxDmdPoolsSeed; pool++)
         cDmdValueSeed[pool] = 0.0;
      cNumDmdPoolsSeed = 0.0;

      for (int part = 0; part < max_part; part++)
      {
         dmGreenID[part] = 0;
         dmSenescedID[part] = 0;
         nGreenID[part] = 0;
         nSenescedID[part] = 0;
         pGreenID[part] = 0;
         pSenescedID[part] = 0;
         dmdMaxGreenID[part] = 0;
         dmdAvgGreenID[part] = 0;
         dmdMinGreenID[part] = 0;
         dmdMaxSenescedID[part] = 0;
         dmdAvgSenescedID[part] = 0;
         dmdMinSenescedID[part] = 0;
      }

      heightID = 0;
      tramplingID = 0;
      stageID = 0;
      stageNameID = 0;
      dmPartsRemovedID = 0;

      c.numDmdPoolsVeg = 0;
      for (int pool = 0; pool < maxDmdPoolsSeed; pool++)
         c.dmdValueSeed[pool] = 0.0;
      c.numDmdPoolsSeed = 0;
      c.specificDetachRate = 0.0;

      c.pConcGreenStemDefault = 0.0;
      c.pConcGreenPodDefault = 0.0;
      c.pConcGreenLeafDefault = 0.0;
      c.pConcGreenMealDefault = 0.0;
      c.pConcGreenOilDefault = 0.0;
      c.pConcSenescedStemDefault = 0.0;
      c.pConcSenescedPodDefault = 0.0;
      c.pConcSenescedLeafDefault = 0.0;
      c.pConcSenescedMealDefault = 0.0;
      c.pConcSenescedOilDefault = 0.0;

      c.AshAlkGreenStemDefault = 0.0;
      c.AshAlkGreenPodDefault = 0.0;
      c.AshAlkGreenLeafDefault = 0.0;
      c.AshAlkGreenMealDefault = 0.0;
      c.AshAlkGreenOilDefault = 0.0;
      c.AshAlkSenescedStemDefault = 0.0;
      c.AshAlkSenescedPodDefault = 0.0;
      c.AshAlkSenescedLeafDefault = 0.0;
      c.AshAlkSenescedMealDefault = 0.0;
      c.AshAlkSenescedOilDefault = 0.0;

      c.NSRatioGreenStemDefault = 0.0;
      c.NSRatioGreenPodDefault = 0.0;
      c.NSRatioGreenLeafDefault = 0.0;
      c.NSRatioGreenMealDefault = 0.0;
      c.NSRatioGreenOilDefault = 0.0;
      c.NSRatioSenescedStemDefault = 0.0;
      c.NSRatioSenescedPodDefault = 0.0;
      c.NSRatioSenescedLeafDefault = 0.0;
      c.NSRatioSenescedMealDefault = 0.0;
      c.NSRatioSenescedOilDefault = 0.0;

      c.NPRatioGreenStemDefault = 0.0;
      c.NPRatioGreenPodDefault = 0.0;
      c.NPRatioGreenLeafDefault = 0.0;
      c.NPRatioGreenMealDefault = 0.0;
      c.NPRatioGreenOilDefault = 0.0;
      c.NPRatioSenescedStemDefault = 0.0;
      c.NPRatioSenescedPodDefault = 0.0;
      c.NPRatioSenescedLeafDefault = 0.0;
      c.NPRatioSenescedMealDefault = 0.0;
      c.NPRatioSenescedOilDefault = 0.0;

      for (int i = 0; i < 3; i++)
      {
         c.dmdGreenLeaf[i] = 0.0;
         c.dmdGreenStem[i] = 0.0;
         c.dmdGreenPod[i] = 0.0;
         c.dmdSenescedLeaf[i] = 0.0;
         c.dmdSenescedStem[i] = 0.0;
         c.dmdSenescedPod[i] = 0.0;
      }
      for (int i = 0; i < 2; i++)
      {
         c.dmdGreenMeal[i] = 0.0;
         c.dmdGreenOil[i] = 0.0;
         c.dmdSenescedMeal[i] = 0.0;
         c.dmdSenescedOil[i] = 0.0;

         c.seedClass[i] = 0;
      }

      c.cpNRatio = 0.0;
      c.proportionLegume = 0.0;

      c.KQ5Leaf = 0.0;
      c.KQ5Stem = 0.0;
      c.KQ4 = 0.0;
}


   // REST
float PlantHerbage::trampling ( void )
{
      return eTrampling;
}

float PlantHerbage::dmTotalVeg ( void )
{
      return dmVeg.total();
}

float PlantHerbage::dmTotalSeed ( void )
{
      return dmSeed.total();
}

float PlantHerbage::dmTotVeg ( int pool )
{
      PlantPool poolDmVeg = dmVeg * dmdFractionVeg[pool];
      float dmTot = poolDmVeg.total();
//         if (dmTot < 0.5) dmTot = 0.0;
      return dmTot;
}

float PlantHerbage::dmTotSeed ( int pool )
{
      SeedPool poolDmSeed = dmSeed * dmdFractionSeed[pool];
      float dmTot = poolDmSeed.total();
//         if (dmTot < 0.5) dmTot = 0.0;
      return dmTot;
}

float PlantHerbage::cpConcVeg ( int pool )
{
        PlantPool poolNVeg = NVeg * dmdFractionVeg[pool];
        float nTot = poolNVeg.total();
        float nConc = divide (nTot, dmTotVeg(pool), 0.0);
      return nConc * c.cpNRatio;
}

float PlantHerbage::cpConcSeed ( int pool )
{
        SeedPool poolNSeed = NSeed * dmdFractionSeed[pool];
        float nTot = poolNSeed.total();
        float nConc = divide (nTot, dmTotSeed(pool), 0.0);
      return nConc * c.cpNRatio;
}

float PlantHerbage::pConcVeg ( int pool )
{
      PlantPool NPRatioVeg(c.NPRatioGreenLeafDefault,  c.NPRatioGreenStemDefault,  c.NPRatioGreenPodDefault,  c.NPRatioSenescedLeafDefault,  c.NPRatioSenescedStemDefault,  c.NPRatioSenescedPodDefault);
//      poolP = P * dmdFractionVeg[pool];
      PlantPool poolPVeg = NVeg/NPRatioVeg * dmdFractionVeg[pool];
      float pTot = poolPVeg.total();
      float pConc = divide (pTot, dmTotVeg(pool), 0.0);
      return pConc;
}

float PlantHerbage::pConcSeed ( int pool )
{
      SeedPool NPRatioSeed(c.NPRatioGreenMealDefault,  c.NPRatioGreenOilDefault,  c.NPRatioSenescedMealDefault,  c.NPRatioSenescedOilDefault);
//      poolP = P * dmdFractionSeed[pool];
      SeedPool poolPSeed = NSeed/NPRatioSeed * dmdFractionSeed[pool];
      float pTot = poolPSeed.total();
      float pConc = divide (pTot, dmTotSeed(pool), 0.0);
      return pConc;
}

float PlantHerbage::ashAlkVeg ( int pool )
{
      PlantPool partAshAlkVeg(c.AshAlkGreenLeafDefault, c.AshAlkGreenStemDefault, c.AshAlkGreenPodDefault, c.AshAlkSenescedLeafDefault, c.AshAlkSenescedStemDefault, c.AshAlkSenescedPodDefault);
      partAshAlkVeg = partAshAlkVeg*cmol2mol;
      PlantPool poolAAVeg    = partAshAlkVeg * dmVeg * dmdFractionVeg[pool];  // ash alk to be got from lablab
      float aaTot = poolAAVeg.total();
      float ashAlk = divide (aaTot, dmTotVeg(pool), 0.0);
      return ashAlk;
}

float PlantHerbage::ashAlkSeed ( int pool )
{
      SeedPool partAshAlkSeed(c.AshAlkGreenMealDefault, c.AshAlkGreenOilDefault, c.AshAlkSenescedMealDefault, c.AshAlkSenescedOilDefault);
      partAshAlkSeed = partAshAlkSeed*cmol2mol;
      SeedPool poolAASeed    = partAshAlkSeed * dmSeed * dmdFractionSeed[pool];  // ash alk to be got from lablab
      float aaTot = poolAASeed.total();
      float ashAlk = divide (aaTot, dmTotSeed(pool), 0.0);
      return ashAlk;
}

float PlantHerbage::sConcVeg ( int pool )
{
      PlantPool NSRatioVeg(c.NSRatioGreenLeafDefault, c.NSRatioGreenStemDefault, c.NSRatioGreenPodDefault, c.NSRatioSenescedLeafDefault, c.NSRatioSenescedStemDefault, c.NSRatioSenescedPodDefault);
      PlantPool poolSVeg = NVeg/NSRatioVeg * dmdFractionVeg[pool];
      float sTot = poolSVeg.total();
      float sConc = divide (sTot, dmTotVeg(pool), 0.0);
      return sConc;
}

float PlantHerbage::sConcSeed ( int pool )
{
      SeedPool NSRatioSeed(c.NSRatioGreenMealDefault, c.NSRatioGreenOilDefault, c.NSRatioSenescedMealDefault, c.NSRatioSenescedOilDefault);
      SeedPool poolSSeed = NSeed/NSRatioSeed * dmdFractionSeed[pool];
      float sTot = poolSSeed.total();
      float sConc = divide (sTot, dmTotSeed(pool), 0.0);
      return sConc;
}

float PlantHerbage::proportionGreen ( void )
{
         float dm_green = dmVeg.green.pod + dmVeg.green.stem + dmVeg.green.leaf;
         float dm_dead = dmVeg.senesced.pod + dmVeg.senesced.stem + dmVeg.senesced.leaf;
         float dm_total = dm_green + dm_dead;
         return divide (dm_green, dm_total, 0.0);
}

float PlantHerbage::proportionLegume ( void )
{
   return c.proportionLegume;       //FIXME - calc legume content
}

float PlantHerbage::selectionFactor ( void )
{
   return 0.0; // ??
}

//===========================================================================
void PlantHerbage::proportion (float dmdAvg, float dmdMax, float dmdMin, float dmdFraction[])
//===========================================================================

//Definition
//Assumptions
//Parameters

{
   //Constant Values

   // Assumes dmd of classes decreases by an constant amount over each class.

   const float ClassWidth = cDmdValueVeg[0] - cDmdValueVeg[1];
   const float MAXDMD = cDmdValueVeg[0] + ClassWidth/2.0;
   const float MINDMD = cDmdValueVeg[cNumDmdPoolsVeg-1] - ClassWidth/2.0;
//   const float MAXDMD = 0.8;
//   const float MINDMD = 0.3;
   const float errorMargin = 1.0e-5;
   const float roundingMargin = 1.0e-2;

   //Local Varialbes

   //Implementation

   // Check that dmds are legal

   if (dmdAvg > dmdMax + errorMargin)
   {  ostringstream msg;
      msg << endl << "Average digestibility > Maximum digestibility:-" << endl
          << "   Average      = " <<  dmdAvg << endl
          << "   Maximum      = " <<  dmdMax << endl  << ends;
      throw std::runtime_error(msg.str().c_str());
   }
   if (dmdAvg < dmdMin - errorMargin)
   {  ostringstream msg;
      msg << endl << "Average digestibility < Minimum digestibility:-" << endl
          << "   Average      = " <<  dmdAvg << endl
          << "   Minimum      = " <<  dmdMin << endl  << ends;
      throw std::runtime_error(msg.str().c_str());
   }
   if (dmdMin > dmdAvg + errorMargin)
   {  ostringstream msg;
      msg << endl << "Minimum digestibility > Average digestibility:-" << endl
          << "   Minimum      = " <<  dmdMin << endl
          << "   Average      = " <<  dmdAvg << endl  << ends;
      throw std::runtime_error(msg.str().c_str());
   }
   if (dmdMin < MINDMD - errorMargin)
   {  ostringstream msg;
      msg << endl << "Minimum digestibility < Lower Limit:-" << endl
          << "   Minimum      = " <<  dmdMin << endl
          << "   Lower Limit  = " <<  MINDMD << endl  << ends;
      throw std::runtime_error(msg.str().c_str());
   }
   if (dmdMax > MAXDMD + errorMargin)
   {  ostringstream msg;
      msg << endl << "Maximum digestibility > Upper Limit:-" << endl
          << "   Maximum      = " <<  dmdMax << endl
          << "   Upper Limit  = " <<  MAXDMD << endl  << ends;
      throw std::runtime_error(msg.str().c_str());
   }
   if (dmdMax < dmdAvg - errorMargin)
   {  ostringstream msg;
      msg << endl << "Maximum digestibility < Average digestibility:-" << endl
          << "   Maximum      = " <<  dmdMax << endl
          << "   Average      = " <<  dmdAvg << endl  << ends;
      throw std::runtime_error(msg.str().c_str());
   }


   float x = (dmdMax == dmdMin) ? 0.0 : (dmdAvg - dmdMin) / (dmdMax - dmdMin);
   int startDmd = (MAXDMD - dmdMax) / ClassWidth + errorMargin;  // index of first (highest) pool to distribute average to
   int endDmd = (MAXDMD - dmdMin) / ClassWidth + errorMargin;    // index of last (lowest) pool to distribute average to
   int numPools = (endDmd - startDmd) + 1;               // number of pools the average will be distributed over

   switch (numPools)
   {
      case 1:
         {
            dmdFraction[startDmd] = 1.0;
         }
         break;
      case 2:
         {
            dmdFraction[startDmd] = x;
            dmdFraction[startDmd+1] = 1.0-x;
         }
         break;
      case 3:
         {
            dmdFraction[startDmd] = pow(x, 2);
            dmdFraction[startDmd+1] = 2.0 * x * (1.0-x);
            dmdFraction[startDmd+2] = pow(1.0-x, 2);
         }
         break;
      case 4:
         {
            dmdFraction[startDmd] = pow(x, 3);
            dmdFraction[startDmd+1] = 3.0 * pow(x, 2) * (1.0-x);
            dmdFraction[startDmd+2] = 3.0 * x * pow(1.0-x, 2);
            dmdFraction[startDmd+3] = pow(1.0-x, 3);
         }
         break;
      case 5:
         {
            dmdFraction[startDmd] = pow(x, 4);
            dmdFraction[startDmd+1] = 4.0 * pow(x, 3) * (1.0-x);
            dmdFraction[startDmd+2] = 6.0 * pow(x, 2) * pow(1.0-x, 2);
            dmdFraction[startDmd+3] = 4.0 * x * pow(1.0-x,3);
            dmdFraction[startDmd+4] = pow(1.0-x, 4);
         }
         break;
      default:
         throw std::runtime_error("Too many digestibility classes");

   }
   for (int pool = 0; pool < numPools; pool ++)
   {
      if (dmdFraction[startDmd + pool] < roundingMargin)
      {
         dmdFraction[startDmd+pool+1] += dmdFraction[startDmd+pool];
         dmdFraction[startDmd+pool] = 0.0;
      }
   }
}

//===========================================================================
void PlantHerbage::proportion (float dmdUnripe, float dmdRipe, float dmdFraction[])
//===========================================================================

//Definition
//Assumptions
//Parameters

{
   //Constant Values

//   const float MAXDMD = cDmdValueSeed[0];
//   const float MINDMD = cDmdValueSeed[cNumDmdPoolsSeed-1];
   const float ClassWidth = cDmdValueSeed[0] - cDmdValueSeed[1];
   const float MAXDMD = cDmdValueSeed[0] + ClassWidth/2.0;
   const float MINDMD = cDmdValueSeed[cNumDmdPoolsSeed-1] - ClassWidth/2.0;
//   const float MAXDMD = 0.8;
//   const float MINDMD = 0.3;
   const float errorMargin = 1.0e-5;

   //Local Varialbes

   //Implementation

   // Check that dmds are legal

   if (dmdRipe < MINDMD - errorMargin)
   {  ostringstream msg;
      msg << endl << "Ripe digestibility < Lower Limit:-" << endl
          << "   Minimum      = " <<  dmdRipe << endl
          << "   Lower Limit  = " <<  MINDMD << endl  << ends;
      throw std::runtime_error(msg.str().c_str());
   }
   if (dmdUnripe > MAXDMD + errorMargin)
   {  ostringstream msg;
      msg << endl << "Unripe digestibility > Upper Limit:-" << endl
          << "   Maximum      = " <<  dmdUnripe << endl
          << "   Upper Limit  = " <<  MAXDMD << endl  << ends;
      throw std::runtime_error(msg.str().c_str());
   }

   int Unripe = 0;
   int ripe = 1;

   if (ripeSeed())
   {
      dmdFraction[Unripe] = 0.0;
      dmdFraction[ripe] = 1.0;
   }
   else
   {
      dmdFraction[Unripe] = 1.0;
      dmdFraction[ripe] = 0.0;
   }
}

////===========================================================================
//void PlantHerbage::dmdClass (float dmdMax, float dmdMin, float &dmdClassMax, float &dmdClassMin)
////===========================================================================
//
////Definition
//   // return the upper and lower dmd class for this dmd Max and Min.
////Assumptions
////Parameters
//
//{
//   //Constant Values
//
//   const float MAXDMD = cDmdValueVeg[0];
//   const float MINDMD = cDmdValueVeg[cNumDmdPoolsVeg-1];
//   const float errorMargin = 1.0e-5;
//
//   //Local Varialbes
//
//   //Implementation
//
//   // Check that dmds are legal
//
//   if (dmdMin < MINDMD - errorMargin)
//   {  ostringstream msg;
//      msg << endl << "Minimum digestibility < Lower Limit:-" << endl
//          << "   Minimum      = " <<  dmdMin << endl
//          << "   Lower Limit  = " <<  MINDMD << endl  << ends;
//      throw std::runtime_error(msg.str().c_str());
//   }
//
//   if (dmdMax > MAXDMD + errorMargin)
//   {  ostringstream msg;
//      msg << endl << "Maximum digestibility > Upper Limit:-" << endl
//          << "   Maximum      = " <<  dmdMax << endl
//          << "   Upper Limit  = " <<  MAXDMD << endl  << ends;
//      throw std::runtime_error(msg.str().c_str());
//   }
//
//   if (dmdMax < dmdMin - errorMargin)
//   {  ostringstream msg;
//      msg << endl << "Minimum digestibility > Maximum digestibility:-" << endl
//          << "   Minimum      = " <<  dmdMin << endl
//          << "   Maximum      = " <<  MINDMD << endl  << ends;
//      throw std::runtime_error(msg.str().c_str());
//   }
//
//   for (int dmdClassNum = 0; dmdClassNum < cNumDmdPoolsVeg; dmdClassNum++)
//   {           // Assume dmdValue in descending order
//      dmdClassMax = dmdClassNum;
//      if (fabs(dmdMax - cDmdValueVeg[dmdClassNum]) < errorMargin)
//      {
//         break;
//      }
//   }
//
//   for (int dmdClassNum = 0; dmdClassNum < cNumDmdPoolsVeg; dmdClassNum++)
//   {           // Assume dmdValue in descending order
//      dmdClassMin = dmdClassNum;
//      if (fabs(dmdMin - cDmdValueVeg[dmdClassNum]) < errorMargin)
//      {
//         break;
//      }
//   }
//}

   // REST
float PlantHerbage::hHeight ( void )
{
      return height;
}

float PlantHerbage::bD ( void )
{
//         float bd = divide(herbage.dm *kg2g/ha2sm, height*mm2m, 0.0);
      float bd = divide(dmVeg.total() *kg2g/ha2sm, hHeight()*mm2m, 0.0);
      return bd;
}

float PlantHerbage::heightRatioVeg (void)
{
      return  divide(100.0, 0.03*bD(), 0.0);
}

float PlantHerbage::heightRatioSeed (void)
{
      return  1.0;
}

float PlantHerbage::dmdValueVeg ( int pool )
{
      return cDmdValueVeg[pool];
}

float PlantHerbage::protDgVeg ( int pool )
{
      return cDmdValueVeg[pool] + 0.1;
}

int PlantHerbage::numDmdPoolsVeg ( void )
{
   return cNumDmdPoolsVeg; // ??
}

float PlantHerbage::dmdValueSeed ( int pool )
{
      return cDmdValueSeed[pool];
}

float PlantHerbage::protDgSeed ( int pool )
{
      return cDmdValueSeed[pool] + 0.1;
}

int PlantHerbage::numDmdPoolsSeed ( void )
{
   return cNumDmdPoolsSeed; // ??
}

string PlantHerbage::debug(void)
   {
      return cDebug;
   }

int PlantHerbage::seedClass(int pool)
{
      return c.seedClass[pool];
}

bool PlantHerbage::ripeSeed(void)
{
      return (seedMaturity() == 1);
}

int PlantHerbage::seedMaturity(void)
{
   if (cropMatureStageNo < 1000)     // maturity has been reached
      return 1;    // seed is ripe
   else
      return 0;    // seed is unripe
}

void PlantHerbage::readParameters ( void )
{

//+  Constant Values
    const char*  section_name = "parameters" ;

//- Implementation Section ----------------------------------

    system->writeString (" - conversion object reading parameters");

    cHerbageModuleName = system->readParameter (section_name, "herbage_module_name");
    system->componentNameToID(cHerbageModuleName, cHerbageModuleID );

    cDebug = system->readParameter (section_name, "debug");
    system->readParameter (section_name, "dmdValue", cDmdValueVeg, cNumDmdPoolsVeg, 0.0, 1.0);

      ostringstream msg;
      msg << "Herbage module name = " << cHerbageModuleName << "(id="<<cHerbageModuleID<<")"<<endl
          << "Debug = " << cDebug << ends;
      system->writeString (msg.str().c_str());
}

void PlantHerbage::readHerbageModuleParameters ( void )
{
//- Implementation Section ----------------------------------
      ostringstream msg;
      msg << " - reading  herbage parameters for module '" << cHerbageModuleName << "'" << endl << ends;
      system->writeString (msg.str().c_str());

    system->readParameter (cHerbageModuleName, "specific_detach_rate", c.specificDetachRate, 0.0, 1.0);

    system->readParameter (cHerbageModuleName, "p_conc_green_leaf_default", c.pConcGreenLeafDefault, 0.0, 1.0);
    system->readParameter (cHerbageModuleName, "p_conc_green_stem_default", c.pConcGreenStemDefault, 0.0, 1.0);
    system->readParameter (cHerbageModuleName, "p_conc_green_pod_default", c.pConcGreenPodDefault, 0.0, 1.0);
    system->readParameter (cHerbageModuleName, "p_conc_green_meal_default", c.pConcGreenMealDefault, 0.0, 1.0);
    system->readParameter (cHerbageModuleName, "p_conc_green_oil_default", c.pConcGreenOilDefault, 0.0, 1.0);
    system->readParameter (cHerbageModuleName, "p_conc_senesced_leaf_default", c.pConcSenescedLeafDefault, 0.0, 1.0);
    system->readParameter (cHerbageModuleName, "p_conc_senesced_stem_default", c.pConcSenescedStemDefault, 0.0, 1.0);
    system->readParameter (cHerbageModuleName, "p_conc_senesced_pod_default", c.pConcSenescedPodDefault, 0.0, 1.0);
    system->readParameter (cHerbageModuleName, "p_conc_senesced_meal_default", c.pConcSenescedMealDefault, 0.0, 1.0);
    system->readParameter (cHerbageModuleName, "p_conc_senesced_oil_default", c.pConcSenescedOilDefault, 0.0, 1.0);

    system->readParameter (cHerbageModuleName, "ash_alk_green_leaf_default", c.AshAlkGreenLeafDefault, 0.0, 500.0);
    system->readParameter (cHerbageModuleName, "ash_alk_green_stem_default", c.AshAlkGreenStemDefault, 0.0, 500.0);
    system->readParameter (cHerbageModuleName, "ash_alk_green_pod_default", c.AshAlkGreenPodDefault, 0.0, 500.0);
    system->readParameter (cHerbageModuleName, "ash_alk_green_meal_default", c.AshAlkGreenMealDefault, 0.0, 500.0);
    system->readParameter (cHerbageModuleName, "ash_alk_green_oil_default", c.AshAlkGreenOilDefault, 0.0, 500.0);
    system->readParameter (cHerbageModuleName, "ash_alk_senesced_leaf_default", c.AshAlkSenescedLeafDefault, 0.0, 500.0);
    system->readParameter (cHerbageModuleName, "ash_alk_senesced_stem_default", c.AshAlkSenescedStemDefault, 0.0, 500.0);
    system->readParameter (cHerbageModuleName, "ash_alk_senesced_pod_default", c.AshAlkSenescedPodDefault, 0.0, 500.0);
    system->readParameter (cHerbageModuleName, "ash_alk_senesced_meal_default", c.AshAlkSenescedMealDefault, 0.0, 500.0);
    system->readParameter (cHerbageModuleName, "ash_alk_senesced_oil_default", c.AshAlkSenescedOilDefault, 0.0, 500.0);

    system->readParameter (cHerbageModuleName, "ns_ratio_green_leaf_default", c.NSRatioGreenLeafDefault, 0.0, 30.0);
    system->readParameter (cHerbageModuleName, "ns_ratio_green_stem_default", c.NSRatioGreenStemDefault, 0.0, 30.0);
    system->readParameter (cHerbageModuleName, "ns_ratio_green_pod_default", c.NSRatioGreenPodDefault, 0.0, 30.0);
    system->readParameter (cHerbageModuleName, "ns_ratio_green_meal_default", c.NSRatioGreenMealDefault, 0.0, 30.0);
    system->readParameter (cHerbageModuleName, "ns_ratio_green_oil_default", c.NSRatioGreenOilDefault, 0.0, 30.0);
    system->readParameter (cHerbageModuleName, "ns_ratio_senesced_leaf_default", c.NSRatioSenescedLeafDefault, 0.0, 30.0);
    system->readParameter (cHerbageModuleName, "ns_ratio_senesced_stem_default", c.NSRatioSenescedStemDefault, 0.0, 30.0);
    system->readParameter (cHerbageModuleName, "ns_ratio_senesced_pod_default", c.NSRatioSenescedPodDefault, 0.0, 30.0);
    system->readParameter (cHerbageModuleName, "ns_ratio_senesced_meal_default", c.NSRatioSenescedMealDefault, 0.0, 30.0);
    system->readParameter (cHerbageModuleName, "ns_ratio_senesced_oil_default", c.NSRatioSenescedOilDefault, 0.0, 30.0);
    system->readParameter (cHerbageModuleName, "np_ratio_green_leaf_default", c.NPRatioGreenLeafDefault, 0.0, 10.0);
    system->readParameter (cHerbageModuleName, "np_ratio_green_stem_default", c.NPRatioGreenStemDefault, 0.0, 10.0);
    system->readParameter (cHerbageModuleName, "np_ratio_green_pod_default", c.NPRatioGreenPodDefault, 0.0, 10.0);
    system->readParameter (cHerbageModuleName, "np_ratio_green_meal_default", c.NPRatioGreenMealDefault, 0.0, 10.0);
    system->readParameter (cHerbageModuleName, "np_ratio_green_oil_default", c.NPRatioGreenOilDefault, 0.0, 10.0);
    system->readParameter (cHerbageModuleName, "np_ratio_senesced_leaf_default", c.NPRatioSenescedLeafDefault, 0.0, 10.0);
    system->readParameter (cHerbageModuleName, "np_ratio_senesced_stem_default", c.NPRatioSenescedStemDefault, 0.0, 10.0);
    system->readParameter (cHerbageModuleName, "np_ratio_senesced_pod_default", c.NPRatioSenescedPodDefault, 0.0, 10.0);
    system->readParameter (cHerbageModuleName, "np_ratio_senesced_meal_default", c.NPRatioSenescedMealDefault, 0.0, 10.0);
    system->readParameter (cHerbageModuleName, "np_ratio_senesced_oil_default", c.NPRatioSenescedOilDefault, 0.0, 10.0);


    int numSeedClasses = 0;
    system->readParameter (cHerbageModuleName, "dmd_seed", cDmdValueSeed, numSeedClasses, 0.0, 1.0);
    system->readParameter (cHerbageModuleName, "cp_n_ratio", c.cpNRatio, 0.0, 10.0);
    system->readParameter (cHerbageModuleName, "proportion_legume", c.proportionLegume, 0.0, 1.0);
    system->readParameter (cHerbageModuleName, "seed_class", c.seedClass, cNumDmdPoolsSeed, 0.0, 6.0);
//   calcDmdClass(dmdClassMaxVeg, dmdClassMinVeg);
}

//===========================================================================
float PlantHerbage::divide (float dividend, float divisor, float default_value)
//===========================================================================

/*Definition
 *   Returns (dividend / divisor) if the division can be done
 *   without overflow or underflow.  If divisor is zero or
 *   overflow would have occurred, a specified default is returned.
 *   If underflow would have occurred, zero is returned.
 *Assumptions
 *   largest/smallest real number is 1.0e+/-30
 *Parameters
 *   dividend:     dividend
 *   divisor:      divisor
 *   defaultValue: default value to return if overflow
 *Calls
 *   reals_are_equal
 */

   {
   //Constant Values
   const float LARGEST = 1.0e30;    //largest acceptable no. for quotient
   const float SMALLEST = 1.0e-30;  //smallest acceptable no. for quotient
   const float nought = 0.0;
   const float one = 1.0;
   const float granularity = 1.0e-6;

   //Local Varialbes
   float quotient;

   //Implementation
   if(floatsAreEqual(dividend, nought, granularity))      //multiplying by 0
      {
      quotient = nought;
      }
   else if(floatsAreEqual(divisor, nought, granularity))  //dividing by 0
      {
      quotient = default_value;
      }
   else if(fabs(divisor) < one)            //possible overflow
      {
      if(fabs(dividend) > fabs(LARGEST * divisor)) //overflow
         {
         quotient = default_value;
         }
      else
         {
         quotient = dividend / divisor;          //ok
         }
      }
   else if(fabs(divisor) > one)             //possible underflow
      {
      if(fabs(dividend) < fabs(SMALLEST * divisor))    //underflow
         {
         quotient = nought;
         }
      else
         {
         quotient = dividend / divisor;                //ok
         }
      }
   else
      {
      quotient = dividend / divisor;                   //ok
      }
   return quotient;
   }



