#include "ResidueHerbage.h"
#pragma hdrstop
#include "Constants.h"

#pragma package(smart_init)
using namespace std;

#define singleArrayTypeDDML \
   "<type  array=\"T\" kind=\"single\"/>"
#define singleTypeDDML \
   "<type  kind=\"single\"/>"

      const int  CARBOHYDRATE = 0 ;
      const int  CELLULOSE = 1 ;
      const int  LIGNIN = 2 ;

      const int  LYING = 0 ;
      const int  STANDING = 1 ;

// number of plant parts
// const int  max_part = 6 ; // NB. implies for (i=0; i < max_part; max_part++) usage

//      const float dmdValue[numDmdPools] = {0.8, 0.7, 0.6, 0.5, 0.4, 0.3};

      inline bool floatsAreEqual(float A, float B, float C) {return(fabs(A-B)<C);}
      float divide (float dividend, float divisor, float default_value);

// ------------------------------------------------------------------
// default constructor
// ------------------------------------------------------------------
ResidueHerbage::ResidueHerbage(void)
   {
   }

// ------------------------------------------------------------------
// constructor
// ------------------------------------------------------------------
ResidueHerbage::ResidueHerbage(protocol::Component *s) : HerbageBase(s)
   {
   }
// ------------------------------------------------------------------
// destructor
// ------------------------------------------------------------------
ResidueHerbage::~ResidueHerbage(void)
   {
   }
// ------------------------------------------------------------------
// Init1 phase.
// ------------------------------------------------------------------
void ResidueHerbage::doInit1(const protocol::Init1Data&)
   {
//   protocol::Component::doInit1(sdml);
//
//   dmFeedOnOfferID = system->addRegistration(RegistrationType::respondToGet, "dm_feed_on_offer", singleArrayTypeDDML);
//   dmFeedRemovedID = system->addRegistration(RegistrationType::respondToGet, "dm_feed_removed", singleArrayTypeDDML);

  }
// ------------------------------------------------------------------
// Init2 phase.
// ------------------------------------------------------------------
void ResidueHerbage::doInit2(void)
   {
      readParameters (); // Read constants
      readHerbageModuleParameters ();
      doRunTimeReg ();
//     zero_variables (); // Zero global states
//     init ();           // Site specific init
//     get_other_variables (); // sw etc..
   }

void ResidueHerbage::readParameters ( void )
{

//+  Constant Values
    const char*  section_name = "parameters" ;

//- Implementation Section ----------------------------------

    system->writeString (" - conversion object reading parameters");

    cHerbageModuleName = system->readParameter (section_name, "herbage_module_name");
    cDebug = system->readParameter (section_name, "debug");
    system->readParameter (section_name, "dmdValue", cDmdValueVeg, cNumDmdPoolsVeg, 0.0, 1.0);

      ostringstream msg;
      msg << "Herbage module name = " << cHerbageModuleName << endl
          << "Debug = " << cDebug << ends;
      system->writeString (msg.str().c_str());
}

// ------------------------------------------------------------------
// Event handler.
// ------------------------------------------------------------------
void ResidueHerbage::doGrazed(protocol::RemoveHerbageType &grazed)
{
      protocol::RemoveCropBiomassType crop;

      doDmdPoolsToHerbageParts(grazed, crop);

      float dmTotal = 0.0;
      for (unsigned int pool=0; pool < crop.dm.size(); pool++)
      {
         for (unsigned int part = 0; part < crop.dm[pool].part.size(); part++)
         {
            dmTotal += (float) crop.dm[pool].dlt[part];
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

         msg << endl << "   dm total = " << dmTotal << " (g/m2)" << endl << ends;

         system->writeString (msg.str().c_str());

      }

      if (dmTotal > 1.0e-6)
      {
         system->publish (removeCropBiomassID, crop);
      }

}

//===========================================================================
void ResidueHerbage::proportion (float dmdAvg, float dmdMax, float dmdMin, float dmdFraction[])
//===========================================================================

//Definition
//Assumptions
//Parameters

{
   //Constant Values

   const float MAXDMD = cDmdValueVeg[0];
   const float MINDMD = cDmdValueVeg[cNumDmdPoolsVeg-1];
//   const float MAXDMD = 0.8;
//   const float MINDMD = 0.3;
   const float errorMargin = 1.0e-5f;
   const float roundingMargin = 1.0e-2f;

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


   float x = (dmdAvg - dmdMin) / (dmdMax - dmdMin);
   int startDmd = (MAXDMD - dmdMax)*10.0 + errorMargin;
   int endDmd = (MAXDMD - dmdMin)*10.0 + errorMargin;
   int numPools = (endDmd - startDmd) + 1;

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
            dmdFraction[startDmd+1] = (float)(1.0-x);
         }
         break;
      case 3:
         {
            dmdFraction[startDmd] = pow(x, 2);
            dmdFraction[startDmd+1] = (float)(2.0 * x * (1.0-x));
            dmdFraction[startDmd+2] = (float)(pow(1.0-x, 2));
         }
         break;
      case 4:
         {
            dmdFraction[startDmd] = pow(x, 3);
            dmdFraction[startDmd+1] = (float)(3.0 * pow(x, 2) * (1.0-x));
            dmdFraction[startDmd+2] = (float)(3.0 * x * pow(1.0-x, 2));
            dmdFraction[startDmd+3] = (float)(pow(1.0-x, 3));
         }
         break;
      case 5:
         {
            dmdFraction[startDmd] = pow(x, 4);
            dmdFraction[startDmd+1] = (float)(4.0 * pow(x, 3) * (1.0-x));
            dmdFraction[startDmd+2] = (float)(6.0 * pow(x, 2) * pow(1.0-x, 2));
            dmdFraction[startDmd+3] = (float)(4.0 * x * pow(1.0-x,3));
            dmdFraction[startDmd+4] = (float)(pow(1.0-x, 4));
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
void ResidueHerbage::dmdClass (float dmdMax, float dmdMin, float &dmdClassMax, float &dmdClassMin)
//===========================================================================

//Definition
//Assumptions
//Parameters

{
   //Constant Values

   const float MAXDMD = cDmdValueVeg[0];
   const float MINDMD = cDmdValueVeg[cNumDmdPoolsVeg-1];
   const float errorMargin = 1.0e-5f;

   //Local Varialbes

   //Implementation

   // Check that dmds are legal

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

   if (dmdMax < dmdMin - errorMargin)
   {  ostringstream msg;
      msg << endl << "Minimum digestibility > Maximum digestibility:-" << endl
          << "   Minimum      = " <<  dmdMin << endl
          << "   Maximum      = " <<  MINDMD << endl  << ends;
      throw std::runtime_error(msg.str().c_str());
   }

   for (int dmdClassNum = 0; dmdClassNum < cNumDmdPoolsVeg; dmdClassNum++)
   {           // Assume dmdValue in descending order
      dmdClassMax = dmdClassNum;
      if (fabs(dmdMax - cDmdValueVeg[dmdClassNum]) < errorMargin)
      {
         break;
      }
   }

   for (int dmdClassNum = 0; dmdClassNum < cNumDmdPoolsVeg; dmdClassNum++)
   {           // Assume dmdValue in descending order
      dmdClassMin = dmdClassNum;
      if (fabs(dmdMin - cDmdValueVeg[dmdClassNum]) < errorMargin)
      {
         break;
      }
   }
}

   // REST
float ResidueHerbage::dmTotalVeg ( void )
{
      return dmVeg.total();
}

float ResidueHerbage::dmTotalSeed ( void )
{
      return dmSeed.total();
}

float ResidueHerbage::hHeight ( void )
{
      return height;
}

float ResidueHerbage::bD ( void )
{
//         float bd = divide(herbage.dm *kg2g/ha2sm, height*mm2m, 0.0);
      float bd = divide(dmVeg.total() *kg2g/ha2sm, hHeight()*mm2m, 0.0);
      return bd;
}

float ResidueHerbage::heightRatioVeg (void)
{
      return  divide(100.0f, (float)(0.03*bD()), 0.0f);
}

float ResidueHerbage::heightRatioSeed (void)
{
      return  1.0f;
}

float ResidueHerbage::dmdValueVeg ( int pool )
{
      return cDmdValueVeg[pool];
}

float ResidueHerbage::dmdValueSeed ( int pool )
{
      return cDmdValueSeed[pool];
}

float ResidueHerbage::protDgVeg ( int pool )
{
      return (float)(cDmdValueVeg[pool] + 0.1);
}

float ResidueHerbage::protDgSeed ( int pool )
{
      return (float)(cDmdValueSeed[pool] + 0.1);
}

int ResidueHerbage::numDmdPoolsVeg ( void )
{
   return cNumDmdPoolsVeg; // ??
}

int ResidueHerbage::numDmdPoolsSeed ( void )
{
   return cNumDmdPoolsSeed; // ??
}

void ResidueHerbage::doDigestibility(void)
{
      getVariables();

      calcDmdAverage();

      if (dmVeg.total() > 0.0)
      {

// distribute herbage


      calcDmdDistribution(dmdFractionVeg, dQVeg);
////      calcDmdDistributionB(dmdFraction, dQ);

      for (int pool = 0; pool < cNumDmdPoolsVeg; pool++)
      {
         if (cDebug == "on")
         {
            ostringstream msgFraction;
            msgFraction << endl << "Residue Herbage dmd distribution, pool " << (pool+1) << ":-" << endl;
            msgFraction << dmdFractionVeg[pool] << ends;
            system->writeString (msgFraction.str().c_str());
         }


      } // end of Pools loop
   }
}

string ResidueHerbage::herbageModuleName(void)
   {
      return cHerbageModuleName;
   }

string ResidueHerbage::debug(void)
   {
      return cDebug;
   }

// ------------------------------------------------------------------
// Runtime Registrations.
// ------------------------------------------------------------------
void ResidueHerbage::doRunTimeReg(void)
   {
   protocol::SurfaceOrganicMatterType dummy;
   surfaceOMID = system->addRegistration(::get, 0 , "surface_organic_matter", protocol::DDML(dummy));   // parameter crop name=lablab

//   heightID = system->addRegistration(RegistrationType::get, "height", singleTypeDDML,"", cHerbageModuleName.c_str());
   }
// ------------------------------------------------------------------
// Event handler.
// ------------------------------------------------------------------
void ResidueHerbage::doDmdPoolsToHerbageParts(protocol::RemoveHerbageType &grazed, protocol::RemoveCropBiomassType &crop)
{
      for (int pool = 0; pool < numDmdPoolsVeg(); pool++)
      {
         ResiduePool poolDm = dmVeg * dmdFractionVeg[pool];
         float dmTot = poolDm.total();
         partFractionVeg[pool] = poolDm / dmTot;
      }

      protocol::RemoveCropBiomassdmType dm;
      crop.dm.erase(crop.dm.begin(), crop.dm.end());
      dm.dlt.erase(dm.dlt.begin(), dm.dlt.end());
      dm.part.erase(dm.part.begin(), dm.part.end());

      dm.pool = "standing";
      dm.part.push_back("carbohydrate");
      float dmPart = 0.0;
      for (int pool = 0; pool < numDmdPoolsVeg(); pool++) dmPart += (float)(grazed.herbage[pool]*partFractionVeg[pool].standing.carbohydrate);
      dm.dlt.push_back(dmPart * kg2g / ha2sm);

      dm.part.push_back("cellulose");
      dmPart = 0.0;
      for (int pool = 0; pool < numDmdPoolsVeg(); pool++) dmPart += (float)(grazed.herbage[pool]*partFractionVeg[pool].standing.cellulose);
      dm.dlt.push_back(dmPart * kg2g / ha2sm);

      dm.part.push_back("lignin");
      dmPart = 0.0;
      for (int pool = 0; pool < numDmdPoolsVeg(); pool++) dmPart += (float)(grazed.herbage[pool]*partFractionVeg[pool].standing.lignin);
      dm.dlt.push_back(dmPart * kg2g / ha2sm);

      crop.dm.push_back(dm);
      dm.dlt.erase(dm.dlt.begin(), dm.dlt.end());
      dm.part.erase(dm.part.begin(), dm.part.end());

      dm.pool = "lying";
      dm.part.push_back("carbohydrate");
      dmPart = 0.0;
      for (int pool = 0; pool < numDmdPoolsVeg(); pool++) dmPart += (float)(grazed.herbage[pool]*partFractionVeg[pool].lying.carbohydrate);
      dm.dlt.push_back(dmPart * kg2g / ha2sm);

      dm.part.push_back("cellulose");
      dmPart = 0.0;
      for (int pool = 0; pool < numDmdPoolsVeg(); pool++) dmPart += (float)(grazed.herbage[pool]*partFractionVeg[pool].lying.cellulose);
      dm.dlt.push_back(dmPart * kg2g / ha2sm);

      dm.part.push_back("lignin");
      dmPart = 0.0;
      for (int pool = 0; pool < numDmdPoolsVeg(); pool++) dmPart += (float)(grazed.herbage[pool]*partFractionVeg[pool].lying.lignin);
      dm.dlt.push_back(dmPart * kg2g / ha2sm);

      crop.dm.push_back(dm);
      dm.dlt.erase(dm.dlt.begin(), dm.dlt.end());
      dm.part.erase(dm.part.begin(), dm.part.end());

}

void ResidueHerbage::getStage(void)
{
}

float ResidueHerbage::trampling(void)
{
   return -1;
}


void ResidueHerbage::getParts(ResiduePartType &parts, unsigned partsID)
{
      vector <float> partsArray;
      bool ok = system->getVariable(partsID, partsArray, 0.0, 10000.0, true);
      if (ok)
      {
         if (partsArray.size() >= 2)
         {
            parts.cellulose = partsArray[CELLULOSE]*g2kg/sm2ha;
            parts.carbohydrate = partsArray[CARBOHYDRATE]*g2kg/sm2ha;
            parts.lignin = partsArray[LIGNIN]*g2kg/sm2ha;
         }
      }
      else
      {
         throw std::runtime_error("Couldn't get variable partsID");
      }
}


////void ResidueHerbage::getPstanding(ResiduePartType &pstanding, ResiduePool &dm)      //FIXME why do we need this?
////{
////      protocol::Variant* variant;
////         bool ok = system->getVariable(pstandingID, variant, true);
////         if (ok)
////         {
////            vector <float> P;
////            bool ok = variant->unpack(P);
////            if (ok && P.size() >= 2)
////            {
////               if (P[CELLULOSE] && P[CARBOHYDRATE] && P[LIGNIN] > 0.0)
////               {
////                  pstanding.cellulose = P[CELLULOSE]*g2kg/sm2ha;
////                  pstanding.carbohydrate = P[CARBOHYDRATE]*g2kg/sm2ha;
////                  pstanding.lignin = P[LIGNIN]*g2kg/sm2ha;
////               }
////               else
////               {
////                  pstanding.cellulose = c.pConcstandingcelluloseDefault * dm.standing.cellulose;  // parameter for default P contents
////                  pstanding.carbohydrate = c.pConcstandingcarbohydrateDefault * dm.standing.carbohydrate;
////                  pstanding.lignin = c.pConcstandingligninDefault * dm.standing.lignin;
////               }
////            }
////            else
////            {
////               throw std::runtime_error("Couldn't unpack pstanding");
////           }
////         }
////         else
////         {
////            throw std::runtime_error("Couldn't get variable pstandingID");
////         }
////}

////void ResidueHerbage::getHeight(float &height)
////{
////      protocol::Variant* variant;
////         bool ok = system->getVariable(heightID, variant, true);
////         if (ok)
////         {
////            bool ok = variant->unpack(height);
////            if (ok)
////            { // do nothing
////            }
////            else
////            {
////               throw std::runtime_error("Couldn't unpack height");
////            }
////         }
////         else
////         {
////            throw std::runtime_error("Couldn't get variable heightID");
////         }
////}
////
////void ResidueHerbage::getThermalTime(float &thermalTime)
////{
////      protocol::Variant* variant;
////         bool ok = system->getVariable(thermalTimeID, variant, true);
////         if (ok)
////         {
////            bool ok = variant->unpack(thermalTime);
////            if (ok)
////            { // do nothing
////            }
////            else
////            {
////               throw std::runtime_error("Couldn't unpack thermalTime");
////            }
////         }
////         else
////         {
////            throw std::runtime_error("Couldn't get variable thermalTimeID");
////         }
////
////         ok = system->getVariable(thermalTimeBGID, variant, true);
////         if (ok)
////         {
////            vector<float> thermalTimeBG;
////            bool ok = variant->unpack(thermalTimeBG);
////            if (ok)
////            {
////               for (unsigned int stage = 0; stage < thermalTimeBG.size(); stage++)
////               {
////                  thermalTime -= thermalTimeBG[stage];
////               }
////               thermalTime = max(0, thermalTime);
////            }
////            else
////            {
////               throw std::runtime_error("Couldn't unpack thermalTimeBG");
////            }
////         }
////         else
////         {
////            throw std::runtime_error("Couldn't get variable thermalTimeBGID");
////         }
////}

void ResidueHerbage::getVariables(void)
{

////   do residue = 1, g%num_surfom
////       SOM(residue)%name = g%SurfOM(residue)%name
////       SOM(residue)%OrganicMatterType = g%SurfOM(residue)%OrganicMatterType
////       SOM(residue)%PotDecompRate = g%SurfOM(residue)%PotDecompRate
////       SOM(residue)%no3 = g%SurfOM(residue)%no3
////       SOM(residue)%nh4 = g%SurfOM(residue)%nh4
////       SOM(residue)%po4 = g%SurfOM(residue)%po4
////
////       do pool = 1, MaxFr
////
////          SOM(residue)%StandingFraction(pool)%amount = g%SurfOM(residue)%Standing(pool)%amount
////          SOM(residue)%StandingFraction(pool)%C = g%SurfOM(residue)%Standing(pool)%C
////          SOM(residue)%StandingFraction(pool)%N = g%SurfOM(residue)%Standing(pool)%N
////          SOM(residue)%StandingFraction(pool)%P = g%SurfOM(residue)%Standing(pool)%P
////          SOM(residue)%StandingFraction(pool)%AshAlk = g%SurfOM(residue)%Standing(pool)%AshAlk
////
////          SOM(residue)%LyingFraction(pool)%amount = g%SurfOM(residue)%Lying(pool)%amount
////          SOM(residue)%LyingFraction(pool)%C = g%SurfOM(residue)%Lying(pool)%C
////          SOM(residue)%LyingFraction(pool)%N = g%SurfOM(residue)%Lying(pool)%N
////          SOM(residue)%LyingFraction(pool)%P = g%SurfOM(residue)%Lying(pool)%P
////          SOM(residue)%LyingFraction(pool)%AshAlk = g%SurfOM(residue)%Lying(pool)%AshAlk
////       end do
////
////   end do

         // Get dm STANDING                  //FIXME to get residue structgure                // get part names: crop type (residue) or plant parts (carbohydrate, cellulose etc).
      protocol::SurfaceOrganicMatterType SOM;
//      getParts(SOM, SOMID);                       // get element: dm, N, P, ash alk
                                                          // get dm deltas?

        // Get HEIGHT
//         ResiduePool  heightRatio;

//         getHeight(height);


}


void ResidueHerbage::calcDmdAverage(void)
{
   // base this on the weighted proportions of carbohydrate, cellulose, lignin
   // using weightings - dmdWeightingCarbohydrate, dmdWeightingCellulose, dmdWeightingLignin


}

void ResidueHerbage::calcDmdClass(ResiduePool &dmdClassMax, ResiduePool &dmdClassMin)
{

// get STANDING carbohydrate & cellulose & lignin dmd classes
      dmdClass (dmdMaxVeg.standing.carbohydrate, dmdMinVeg.standing.carbohydrate, dmdClassMaxVeg.standing.carbohydrate, dmdClassMinVeg.standing.carbohydrate);
      dmdClass (dmdMaxVeg.standing.cellulose, dmdMinVeg.standing.cellulose, dmdClassMaxVeg.standing.cellulose, dmdClassMinVeg.standing.cellulose);
      dmdClass (dmdMaxVeg.standing.lignin, dmdMinVeg.standing.lignin, dmdClassMaxVeg.standing.lignin, dmdClassMinVeg.standing.lignin);

// get LYING carbohydrate & cellulose & lignin dmd classes
      dmdClass (dmdMaxVeg.lying.carbohydrate, dmdMinVeg.lying.carbohydrate, dmdClassMaxVeg.lying.carbohydrate, dmdClassMinVeg.lying.carbohydrate);
      dmdClass (dmdMaxVeg.lying.cellulose, dmdMinVeg.lying.cellulose, dmdClassMaxVeg.lying.cellulose, dmdClassMinVeg.lying.cellulose);
      dmdClass (dmdMaxVeg.lying.lignin, dmdMinVeg.lying.lignin, dmdClassMaxVeg.lying.lignin, dmdClassMinVeg.lying.lignin);

}

void ResidueHerbage::calcDmdDistribution(ResiduePool dmdFraction[], ResiduePool dQVeg)
{
// base average (dmdDeclined) on weighted proportions of carbohydrate, cellulose and lignin.
      ResiduePool dmdDeclined = dmdMaxVeg - dQVeg;

      float fraction[maxDmdPoolsVeg];

// get STANDING dmd fractions
      for (int pool = 0; pool < numDmdPoolsVeg(); pool++)
         fraction[pool] = 0.0;
      proportion (dmdDeclined.standing.carbohydrate, dmdMaxVeg.standing.carbohydrate, dmdMinVeg.standing.carbohydrate, fraction);
      for (int pool = 0; pool < numDmdPoolsVeg(); pool++)
         dmdFraction[pool].standing = fraction[pool];

// get LYING dmd fractions
      for (int pool = 0; pool < numDmdPoolsVeg(); pool++)
         fraction[pool] = 0.0;
      proportion (dmdDeclined.lying.carbohydrate, dmdMaxVeg.lying.carbohydrate, dmdMinVeg.lying.carbohydrate, fraction);
      for (int pool = 0; pool < numDmdPoolsVeg(); pool++)
         dmdFraction[pool].lying = fraction[pool];

////// get STANDING carbohydrate dmd fractions
////      for (int pool = 0; pool < numDmdPools(); pool++) fraction[pool] = 0.0;
////      proportion (dmdDeclined.standing.carbohydrate, dmdMax.standing.carbohydrate, dmdMin.standing.carbohydrate, fraction);
////      for (int pool = 0; pool < numDmdPools(); pool++) dmdFraction[pool].standing.carbohydrate = fraction[pool];
////
////// get STANDING cellulose dmd fractions
////      for (int pool = 0; pool < numDmdPools(); pool++) fraction[pool] = 0.0;
////      proportion (dmdDeclined.standing.cellulose, dmdMax.standing.cellulose, dmdMin.standing.cellulose, fraction);
////      for (int pool = 0; pool < numDmdPools(); pool++) dmdFraction[pool].standing.cellulose = fraction[pool];
////
////// get STANDING lignin dmd fractions
////      for (int pool = 0; pool < numDmdPools(); pool++) fraction[pool] = 0.0;
////      proportion (dmdDeclined.standing.lignin, dmdMax.standing.lignin, dmdMin.standing.lignin, fraction);
////      for (int pool = 0; pool < numDmdPools(); pool++) dmdFraction[pool].standing.lignin = fraction[pool];

////// get LYING carbohydrate dmd fractions
////      for (int pool = 0; pool < numDmdPools(); pool++) fraction[pool] = 0.0;
////      proportion (dmdDeclined.lying.carbohydrate, dmdMax.lying.carbohydrate, dmdMin.lying.carbohydrate, fraction);
////      for (int pool = 0; pool < numDmdPools(); pool++) dmdFraction[pool].lying.carbohydrate = fraction[pool];
////
////// get LYING cellulose dmd fractions
////      for (int pool = 0; pool < numDmdPools(); pool++) fraction[pool] = 0.0;
////      proportion (dmdDeclined.lying.cellulose, dmdMax.lying.cellulose, dmdMin.lying.cellulose, fraction);
////      for (int pool = 0; pool < numDmdPools(); pool++) dmdFraction[pool].lying.cellulose = fraction[pool];
////
////// get LYING lignin dmd fractions
////      for (int pool = 0; pool < numDmdPools(); pool++) fraction[pool] = 0.0;
////      proportion (dmdDeclined.lying.lignin, dmdMax.lying.lignin, dmdMin.lying.lignin, fraction);
////      for (int pool = 0; pool < numDmdPools(); pool++) dmdFraction[pool].lying.lignin = fraction[pool];

}

void ResidueHerbage::calcDmdDistributionB(ResiduePool dmdFraction[], ResiduePool dQVeg)
{
      ResiduePool dmdDeclined = dmdMaxVeg - dQVeg;

// get STANDING carbohydrate dmd fractions
      dmdFraction[0].standing.carbohydrate = 1.0;
      cDmdValueVeg[0] = dmdDeclined.standing.carbohydrate;

// get STANDING cellulose dmd fractions
      dmdFraction[1].standing.cellulose = 1.0;
      cDmdValueVeg[1] = dmdDeclined.standing.cellulose;

// get STANDING lignin dmd fractions
      dmdFraction[1].standing.lignin = 1.0;
      cDmdValueVeg[1] = dmdDeclined.standing.lignin;

// get LYING carbohydrate dmd fractions
      dmdFraction[2].lying.carbohydrate = 1.0;
      cDmdValueVeg[2] = dmdDeclined.lying.carbohydrate;

// get LYING cellulose dmd fractions
      dmdFraction[3].lying.cellulose = 1.0;
      cDmdValueVeg[3] = dmdDeclined.lying.cellulose;

// get LYING lignin dmd fractions
      dmdFraction[3].lying.lignin = 1.0;
      cDmdValueVeg[3] = dmdDeclined.lying.lignin;

}


   // REST
float ResidueHerbage::dmTotVeg ( int pool )
{
      ResiduePool poolDm = dmVeg * dmdFractionVeg[pool];
      float dmTot = poolDm.total();
//         if (dmTot < 0.5) dmTot = 0.0;
      return dmTot;
}

float ResidueHerbage::dmTotSeed ( int pool )
{
      ResiduePool poolDm = dmSeed * dmdFractionSeed[pool];
      float dmTot = poolDm.total();
//         if (dmTot < 0.5) dmTot = 0.0;
      return dmTot;
}

float ResidueHerbage::cpConcVeg ( int pool )
{
        ResiduePool poolN = NVeg * dmdFractionVeg[pool];
        float nTot = poolN.total();
        float nConc = divide (nTot, dmTotVeg(pool), 0.0);
      return nConc * c.cpNRatio;
}

float ResidueHerbage::cpConcSeed ( int pool )
{
        ResiduePool poolN = NSeed * dmdFractionSeed[pool];
        float nTot = poolN.total();
        float nConc = divide (nTot, dmTotSeed(pool), 0.0);
      return nConc * c.cpNRatio;
}

float ResidueHerbage::pConcVeg ( int pool )
{
   ResiduePartType NPRatioStanding(c.NPRatiocarbohydrateDefault,  c.NPRatiocelluloseDefault,  c.NPRatioligninDefault);
   ResiduePartType NPRatioLying(c.NPRatiocarbohydrateDefault,  c.NPRatiocelluloseDefault,  c.NPRatioligninDefault);

      ResiduePool NPRatio(NPRatioStanding, NPRatioLying);
//      poolP = P * dmdFraction[pool];
      ResiduePool poolP = NVeg/NPRatio * dmdFractionVeg[pool];
      float pTot = poolP.total();
      float pConc = divide (pTot, dmTotVeg(pool), 0.0);
      return pConc;
}

float ResidueHerbage::pConcSeed ( int pool )
{
   ResiduePartType NPRatioStanding(c.NPRatiocarbohydrateDefault,  c.NPRatiocelluloseDefault,  c.NPRatioligninDefault);
   ResiduePartType NPRatioLying(c.NPRatiocarbohydrateDefault,  c.NPRatiocelluloseDefault,  c.NPRatioligninDefault);

      ResiduePool NPRatio(NPRatioStanding, NPRatioLying);
//      poolP = P * dmdFraction[pool];
      ResiduePool poolP = NSeed/NPRatio * dmdFractionSeed[pool];
      float pTot = poolP.total();
      float pConc = divide (pTot, dmTotSeed(pool), 0.0);
      return pConc;
}

float ResidueHerbage::ashAlkVeg ( int pool )
{
   ResiduePartType AshAlkStanding(c.AshAlkcarbohydrateDefault,  c.AshAlkcelluloseDefault,  c.AshAlkligninDefault);
   ResiduePartType AshAlkLying(c.AshAlkcarbohydrateDefault, c.AshAlkcelluloseDefault, c.AshAlkligninDefault);

      ResiduePool partAshAlk(AshAlkStanding, AshAlkLying);
      partAshAlk = partAshAlk*cmol2mol;
      ResiduePool poolAA    = partAshAlk * dmVeg * dmdFractionVeg[pool];  // ash alk to be got from lablab
      float aaTot = poolAA.total();
      float ashAlk = divide (aaTot, dmTotVeg(pool), 0.0);
      return ashAlk;
}

float ResidueHerbage::ashAlkSeed ( int pool )
{
   ResiduePartType AshAlkStanding(c.AshAlkcarbohydrateDefault,  c.AshAlkcelluloseDefault,  c.AshAlkligninDefault);
   ResiduePartType AshAlkLying(c.AshAlkcarbohydrateDefault, c.AshAlkcelluloseDefault, c.AshAlkligninDefault);

      ResiduePool partAshAlk(AshAlkStanding, AshAlkLying);
      partAshAlk = partAshAlk*cmol2mol;
      ResiduePool poolAA    = partAshAlk * dmSeed * dmdFractionSeed[pool];  // ash alk to be got from lablab
      float aaTot = poolAA.total();
      float ashAlk = divide (aaTot, dmTotSeed(pool), 0.0);
      return ashAlk;
}

float ResidueHerbage::sConcVeg ( int pool )
{
   ResiduePartType NSRatioStanding(c.NSRatiocarbohydrateDefault,  c.NSRatiocelluloseDefault,  c.NSRatioligninDefault);
   ResiduePartType NSRatioLying(c.NSRatiocarbohydrateDefault,  c.NSRatiocelluloseDefault,  c.NSRatioligninDefault);

      ResiduePool NSRatio(NSRatioStanding, NSRatioLying);
      ResiduePool poolS = NVeg/NSRatio * dmdFractionVeg[pool];
      float sTot = poolS.total();
      float sConc = divide (sTot, dmTotVeg(pool), 0.0);
      return sConc;
}

float ResidueHerbage::sConcSeed ( int pool )
{
   ResiduePartType NSRatioStanding(c.NSRatiocarbohydrateDefault,  c.NSRatiocelluloseDefault,  c.NSRatioligninDefault);
   ResiduePartType NSRatioLying(c.NSRatiocarbohydrateDefault,  c.NSRatiocelluloseDefault,  c.NSRatioligninDefault);

      ResiduePool NSRatio(NSRatioStanding, NSRatioLying);
      ResiduePool poolS = NSeed/NSRatio * dmdFractionSeed[pool];
      float sTot = poolS.total();
      float sConc = divide (sTot, dmTotSeed(pool), 0.0);
      return sConc;
}

float ResidueHerbage::proportionGreen ( void )  //FIXME how to handle proportion Green here?
{
         float dm_standing = dmVeg.standing.total();
         float dm_lying = dmVeg.lying.total();
         float dm_total = dm_standing + dm_lying;
         return divide (dm_standing, dm_total, 0.0);
}

float ResidueHerbage::proportionLegume ( void )
{
   return c.proportionLegume;       //FIXME - calc legume content
}

float ResidueHerbage::selectionFactor ( void )
{
   return 0.0; // ??
}

int ResidueHerbage::seedClass(int pool)
{
      return 0;
}

int ResidueHerbage::seedMaturity(void)
{
      return 0;
}

void ResidueHerbage::readHerbageModuleParameters ( void )
{

//+  Constant Values
//    const char*  section_name = "parameters" ;

//- Implementation Section ----------------------------------
      ostringstream msg;
      msg << " - reading  herbage parameters for module '" << herbageModuleName().c_str() << "'" << endl << ends;
      system->writeString (msg.str().c_str());


    system->readParameter (herbageModuleName().c_str(), "p_conc_cellulose_default", c.pConccelluloseDefault, 0.0, 1.0);
    system->readParameter (herbageModuleName().c_str(), "p_conc_carbohydrate_default", c.pConccarbohydrateDefault, 0.0, 1.0);
    system->readParameter (herbageModuleName().c_str(), "p_conc_lignin_default", c.pConcligninDefault, 0.0, 1.0);

    system->readParameter (herbageModuleName().c_str(), "ash_alk_cellulose_default", c.AshAlkcelluloseDefault, 0.0, 500.0);
    system->readParameter (herbageModuleName().c_str(), "ash_alk_carbohydrate_default", c.AshAlkligninDefault, 0.0, 500.0);
    system->readParameter (herbageModuleName().c_str(), "ash_alk_lignin_default", c.AshAlkcarbohydrateDefault, 0.0, 500.0);

    system->readParameter (herbageModuleName().c_str(), "ns_ratio_cellulose_default", c.NSRatiocelluloseDefault, 0.0, 30.0);
    system->readParameter (herbageModuleName().c_str(), "ns_ratio_carbohydrate_default", c.NSRatiocarbohydrateDefault, 0.0, 30.0);
    system->readParameter (herbageModuleName().c_str(), "ns_ratio_lignin_default", c.NSRatioligninDefault, 0.0, 30.0);

    system->readParameter (herbageModuleName().c_str(), "np_ratio_cellulose_default", c.NPRatiocelluloseDefault, 0.0, 10.0);
    system->readParameter (herbageModuleName().c_str(), "np_ratio_carbohydrate_default", c.NPRatiocarbohydrateDefault, 0.0, 10.0);
    system->readParameter (herbageModuleName().c_str(), "np_ratio_lignin_default", c.NPRatioligninDefault, 0.0, 10.0);

    int numClasses = 3;
    system->readParameter (herbageModuleName().c_str(), "dmd_standing", c.dmdstanding, numClasses, 0.0, 1.0);
    system->readParameter (herbageModuleName().c_str(), "dmd_lying", c.dmdlying, numClasses, 0.0, 1.0);

    system->readParameter (herbageModuleName().c_str(), "dmd_weighting_carbohydrate", c.dmdWeightingCarbohydrate, 0.0, 1.0);
    system->readParameter (herbageModuleName().c_str(), "dmd_weighting_cellulose", c.dmdWeightingCellulose, 0.0, 1.0);
    system->readParameter (herbageModuleName().c_str(), "dmd_weighting_lignin", c.dmdWeightingLignin, 0.0, 1.0);

    system->readParameter (herbageModuleName().c_str(), "cp_n_ratio", c.cpNRatio, 0.0, 10.0);
    system->readParameter (herbageModuleName().c_str(), "proportion_legume", c.proportionLegume, 0.0, 1.0);

   const int MAX = 0;
   const int AVG = 1;
   const int MIN = 2;

   ResiduePartType standing[MIN+1];
   ResiduePartType lying[MIN+1];

   standing[MAX] = c.dmdstanding[MAX];
   lying[MAX] = c.dmdlying[MAX];
   standing[AVG] = c.dmdstanding[AVG];
   lying[AVG] = c.dmdlying[AVG];
   standing[MIN] = c.dmdstanding[MIN];
   lying[MIN] = c.dmdlying[MIN];

   dmdMaxVeg.setValue(standing[MAX], lying[MAX]);
   dmdAvgVeg.setValue(standing[AVG], lying[AVG]);
   dmdMinVeg.setValue(standing[MIN], lying[MIN]);

   calcDmdClass(dmdClassMaxVeg, dmdClassMinVeg);
}

//===========================================================================
float ResidueHerbage::divide (float dividend, float divisor, float default_value)
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
   const float LARGEST = 1.0e30f;    //largest acceptable no. for quotient
   const float SMALLEST = 1.0e-30f;  //smallest acceptable no. for quotient
   const float nought = 0.0f;
   const float one = 1.0f;
   const float granularity = 1.0e-6f;

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



