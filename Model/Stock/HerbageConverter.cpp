
#include "HerbageConverter.h"
#pragma hdrstop
#include "Constants.h"

using namespace std;

static const char*  singleArrayTypeDDML = "<type  array=\"T\" kind=\"single\"/>";
static const char*  singleTypeDDML = "<type kind=\"single\"/>";

// number of plant parts
// const int  max_part = 6 ; // NB. implies for (i=0; i < max_part; max_part++) usage

//      const float dmdValue[numDmdPools] = {0.8, 0.7, 0.6, 0.5, 0.4, 0.3};

      inline bool floatsAreEqual(float A, float B, float C) {return(fabs(A-B)<C);}
      float divide (float dividend, float divisor, float default_value);

// ------------------------------------------------------------------
// default constructor
// ------------------------------------------------------------------
HerbageConverter::HerbageConverter(void)
   {
   }
// ------------------------------------------------------------------
// constructor
// ------------------------------------------------------------------
HerbageConverter::HerbageConverter(protocol::Component *s) : ConverterBase(s)
   {
   }
// ------------------------------------------------------------------
// Destructor
// ------------------------------------------------------------------
HerbageConverter::~HerbageConverter(void)
   {
    if (conversion) delete conversion;
   }
// ------------------------------------------------------------------
// Init1 phase.
// ------------------------------------------------------------------
void HerbageConverter::doInit1(const protocol::Init1Data& initData)
   {
   tramplingID = system->addRegistration(::respondToGet, 0, "herbage_trampling", singleTypeDDML);
   plant2stockID = system->addRegistration(::respondToGet, 0, "plant2stock", protocol::DDML(protocol::Plant2StockType()).c_str());
   removeHerbageID = system->addRegistration(::respondToEvent, 0, "remove_herbage", DDML(protocol::RemoveHerbageType()).c_str());

   dmFeedOnOfferID = system->addRegistration(::respondToGet, 0, "dm_feed_on_offer", singleArrayTypeDDML);
   dmFeedRemovedID = system->addRegistration(::respondToGet, 0, "dm_feed_removed", singleArrayTypeDDML);
   DmdFeedRemovedID = system->addRegistration(::respondToGet, 0, "dm_feed_removed_dmd_class", singleArrayTypeDDML);
   DmdAvgFeedRemovedID = system->addRegistration(::respondToGet, 0, "dmd_avg_feed_removed", singleTypeDDML);

     herbage_model = system->readParameter ("constants", "herbage_model");

    if (herbage_model == "")
       throw std::invalid_argument("The parameter 'herbage_model'\nisn't in your ini file.\n\nGet one.\n");
    else if (herbage_model == "plant")
       conversion = new PlantHerbage(system);
    else if (herbage_model == "surfaceom")
       conversion = new ResidueHerbage(system);
    else
       throw std::invalid_argument("Unknown herbage model '" + herbage_model + "'");

    conversion->doInit1(initData);
   }
// ------------------------------------------------------------------
// Init2 phase.
// ------------------------------------------------------------------
void HerbageConverter::doInit2(void)
   {
         ostringstream msg;
         msg << "Herbage model:- " << herbage_model << endl << ends;
         system->writeString (msg.str().c_str());

      plant2StockSent = false;
      readParameters (); // Read constants
//     zero_variables (); // Zero global states
//     init ();           // Site specific init
//     get_other_variables (); // sw etc..
    conversion->doInit2();
   }

// ------------------------------------------------------------------
// Event handler.
// ------------------------------------------------------------------
void HerbageConverter::respondToEvent(unsigned int& fromID, unsigned int& eventID, protocol::Variant& variant)
{
   if (eventID == removeHerbageID && plant2StockSent == true)
   {
      variant.unpack(grazed);
      if (c.debug == "on")
      {
         ostringstream msg1;
         msg1 << endl << "Remove herbage dmd pools:-" << endl;
         float dmTotal1 = 0.0;

         for (unsigned int pool = 0; pool < feed.herbage.size(); pool++)
         {
            msg1 << "   dm pool " << (pool+1) << " (" << feed.herbage[pool].dmd << ") = " << grazed.herbage[pool] << " (kg/ha)" << endl;
            dmTotal1 +=  grazed.herbage[pool];
         }

         msg1 << endl << "Remove Seed un/ripe pools:-" << endl;
         for (unsigned int pool = 0; pool < feed.seed.size(); pool++)
         {
            msg1 << "   dm pool " << (pool+1) << " (" << feed.seed[pool].dmd << ") = " << grazed.seed[pool] << " (kg/ha)" << endl;
            dmTotal1 +=  grazed.seed[pool];
         }

         msg1 << endl << "   dm total = " << dmTotal1 << " (kg/ha)" << endl << ends;

         system->writeString (msg1.str().c_str());
      }

      for (unsigned int dmdPool = 0; dmdPool < grazed.herbage.size(); dmdPool++)
      {
         if (grazed.herbage[dmdPool] > feed.herbage[dmdPool].dm)
         {
            ostringstream msg;     //FIXME!! Coding to avoid stack being wiped out.
            msg << "              !!!!! FATAL ERROR !!!!!" << endl;
            msg << "Attempting to remove more herbage from dmd pool " << (dmdPool+1) << " (dmd " << feed.herbage[dmdPool].dmd << ")" << " than available:-" << endl;
            msg << "Removing " << grazed.herbage[dmdPool] << " (kg/ha) from " << feed.herbage[dmdPool].dm << " (kg/ha) available." << endl;
            msg << "Stock Science Converter Component Exiting" << endl << ends;
            system->writeString (msg.str().c_str());
            cerr << msg;
            exit(1);
//            throw std::runtime_error (msg.str());
         }
      }

      for (unsigned int dmdPool = 0; dmdPool < grazed.seed.size(); dmdPool++)
      {
         if (grazed.seed[dmdPool] > feed.seed[dmdPool].dm)
         {
            ostringstream msg;     //FIXME!! Coding to avoid stack being wiped out.
            msg << "              !!!!! FATAL ERROR !!!!!" << endl;
            msg << "Attempting to remove more seed from dmd pool " << (dmdPool+1) << " (dmd " << feed.seed[dmdPool].dmd << ")" << " than available:-" << endl;
            msg << "Removing " << grazed.seed[dmdPool] << " (kg/ha) from " << feed.seed[dmdPool].dm << " (kg/ha) available." << endl;
            msg << "Stock Science Converter Component Exiting" << endl << ends;
            system->writeString (msg.str().c_str());
            cerr << msg;
            exit(1);
//            throw std::runtime_error (msg.str());
         }
      }

      conversion->doGrazed(grazed);

   }
   else
   {   // Don't respond to any other events.
   }
}
// ------------------------------------------------------------------
// Event handler.
// ------------------------------------------------------------------

// ------------------------------------------------------------------
// return a variable to caller.  Return true if we own variable.
// ------------------------------------------------------------------
void HerbageConverter::respondToGet(unsigned int& fromID,
                                             protocol::QueryValueData& queryData)
{
   if (queryData.ID == dmFeedOnOfferID) sendFeedOnOffer(queryData);

   //dm feed removed
   else if (queryData.ID == dmFeedRemovedID) sendFeedRemoved(queryData);
   else if (queryData.ID == DmdAvgFeedRemovedID) sendDmdAvgFeedRemoved(queryData);
   else if (queryData.ID == DmdFeedRemovedID) sendDmdFeedRemoved(queryData);

   // plant2stock
   else if (queryData.ID == plant2stockID)  sendPlant2Stock(queryData);

   // trampling
   else if (queryData.ID == tramplingID)  sendTrampling(queryData);

   else
   {   // don't respond to any other gets.
   }
}

void HerbageConverter::sendTrampling(protocol::QueryValueData& queryData)
{
      float trampling = conversion->trampling();
      system->sendVariable(queryData, trampling);
}

void HerbageConverter::sendFeedOnOffer(protocol::QueryValueData& queryData)
{
      float dmFeedOnOffer[8] = {0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0};
      int num_parts = feed.herbage.size();
      if (num_parts > 0)
      {
         for (int i = 0; i != num_parts; i++)
         {
            dmFeedOnOffer[i] = feed.herbage[i].dm;
         }
      }
      else
      {
         num_parts = 6;
      }

      int num_seed_parts = feed.seed.size();
      if (num_seed_parts > 0)
      {
         for (int i = 0; i != num_seed_parts; i++)
         {
            dmFeedOnOffer[num_parts + i] = feed.seed[i].dm;
         }
      }
      else
      {
         num_seed_parts = 2;
      }
      system->sendVariable(queryData, vector <float> (dmFeedOnOffer, dmFeedOnOffer+num_parts+num_seed_parts));
}

void HerbageConverter::sendFeedRemoved(protocol::QueryValueData& queryData)
{
      float dmFeedRemoved[8] = {0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0};
      int num_parts = grazed.herbage.size();
      if (num_parts > 0)
      {
         for (int i = 0; i != num_parts; i++)
         {
            dmFeedRemoved[i] = grazed.herbage[i];
         }
      }
      else
      {
         num_parts = 6;
      }

      int num_seed_parts = grazed.seed.size();
      if (num_seed_parts > 0)
      {
         for (int i = 0; i != num_seed_parts; i++)
         {
            dmFeedRemoved[num_parts + i] = grazed.seed[i];
         }
      }
      else
      {
         num_seed_parts = 2;
      }
      system->sendVariable(queryData, vector <float> (dmFeedRemoved, dmFeedRemoved+num_parts+num_seed_parts));
}

void HerbageConverter::sendDmdFeedRemoved(protocol::QueryValueData& queryData)
{
      float dmdFeedRemoved[8] = {0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0};
      int num_parts = grazed.herbage.size();
      if (num_parts > 0)
      {
         for (int i = 0; i != num_parts; i++)
         {
            dmdFeedRemoved[i] = grazed.herbage[i];
         }
      }
      else
      {
         num_parts = 6;
      }

      int num_seed_parts = grazed.seed.size();
      if (num_seed_parts > 0)
      {
         for (int i = 0; i != num_seed_parts; i++)
         {
            dmdFeedRemoved[num_parts + i] = grazed.seed[i];
         }
      }
      else
      {
         num_seed_parts = 2;
      }
      system->sendVariable(queryData, vector <float> (dmdFeedRemoved, dmdFeedRemoved+num_parts+num_seed_parts));
}

void HerbageConverter::sendDmdAvgFeedRemoved(protocol::QueryValueData& queryData)
{
      float dmdFeedRemoved = 0.0;
      float dmTotal1 = 0.0;
      int num_parts = feed.herbage.size();
      if (num_parts > 0)
      {
         for (int i = 0; i != num_parts; i++)
         {
            dmdFeedRemoved += feed.herbage[i].dmd * grazed.herbage[i];
            dmTotal1 +=  grazed.herbage[i];
         }
      }

      int num_seed_parts = grazed.seed.size();
      if (num_seed_parts > 0)
      {
         for (int i = 0; i != num_seed_parts; i++)
         {
            dmdFeedRemoved += feed.seed[i].dmd * grazed.seed[i];
            dmTotal1 +=  grazed.seed[i];
         }
      }
      if (dmTotal1 > 0)
         dmdFeedRemoved = dmdFeedRemoved / dmTotal1;
      else
         dmdFeedRemoved;

      system->sendVariable(queryData, dmdFeedRemoved);

}

void HerbageConverter::sendPlant2Stock(protocol::QueryValueData& queryData)
{
      conversion->doDigestibility();
//      conversion->getVariables(dm, N, P, height, thermalTime);

      protocol::Plant2StockherbageType herbage;

      if (conversion->dmTotalVeg() > 0.0)
      {

// Now PREPARE herbage
      feed.herbage.erase(feed.herbage.begin(), feed.herbage.end());

// distribute herbage


      for (int pool = 0; pool < conversion->numDmdPoolsVeg(); pool++)
      {
         herbage.dm = conversion->dmTotVeg(pool);
         herbage.dmd = conversion->dmdValueVeg(pool);        // kg/ha  //fixme
         herbage.cp_conc = conversion->cpConcVeg(pool);   // (kg/ha) - parameter cpNRatio = 6.25
         herbage.p_conc = conversion->pConcVeg(pool);
         herbage.s_conc = conversion->sConcVeg(pool);      //parameter NSRatioLeaf = 19.0, NSRatioStem = 11.0;  herbage.s_conc = 0.0023;  kg/ha
         herbage.prot_dg = conversion->protDgVeg(pool);     // parameter ProtDegrade = 0.1;  herbage.prot_dg = 0.7;    // kg/ha
         herbage.ash_alk = conversion->ashAlkVeg(pool);      // herbage.ash_alk = 2.0;    // mol/kg
         herbage.height_ratio = conversion->heightRatioVeg();

         feed.herbage.push_back(herbage);

         if (herbage.dm > 0.0)
         {
            if (c.debug == "on")
            {
               ostringstream msg;
               msg << endl << "Herbage on offer, pool " << (pool+1) << ":-" << endl
                   << "   dm           = " <<              herbage.dm <<      " (kg/ha)" << endl
                   << "   dmd          = " <<              herbage.dmd <<     " (-)" <<     endl
                   << "   cp_conc      = " <<              herbage.cp_conc << " (kg/kg)" << endl
                   << "   p_conc       = " <<              herbage.p_conc <<  " (kg/kg)" << endl
                   << "   s_conc       = " <<              herbage.s_conc <<  " (kg/kg)" << endl
                   << "   ash_alk      = " <<              herbage.ash_alk << " (mol/kg)" << endl
                   << "   prot_dg      = " <<              herbage.prot_dg << " (kg/kg)" << endl
                   << "   bd           = " <<              conversion->bD() << " (g/m^3)" << endl
                   << "   dm total     = " <<              conversion->dmTotalVeg() *kg2g/ha2sm << " (g/m2)" << endl
                   << "   height       = " <<              conversion->hHeight()*mm2m << " (m)" << endl
                   << "   height_ratio = " <<              herbage.height_ratio << " (-)" << ends;
               system->writeString (msg.str().c_str());
            }
         }
      } // end of Pools loop
   // REST
         feed.propn_green = conversion->proportionGreen();
         feed.legume = conversion->proportionLegume();
         feed.select_factor = conversion->selectionFactor(); // ??

         plant2StockSent = true;
      }
      else
      {
         // No dry matter so Clear herbage
         feed.herbage.erase(feed.herbage.begin(), feed.herbage.end());
         herbage.dm = 0.0;         // kg/ha
         herbage.dmd = 0.0;        // kg/ha
         herbage.cp_conc = 0.0;    // (kg/ha)
         herbage.p_conc = 0.0;
         herbage.s_conc = 0.0;      //  kg/ha
         herbage.prot_dg = 0.0;     //  kg/ha
         herbage.ash_alk = 0.0;      //  mol/kg
         herbage.height_ratio = 0.0; //

         for (int pool = 0; pool < conversion->numDmdPoolsVeg(); pool++)
         {
            feed.herbage.push_back(herbage);
         }

   // REST
         feed.propn_green = 0.0;
         feed.legume = 0.0;
         feed.select_factor = 0.0;
      }

      protocol::Plant2StockseedType seed;
//      int seedClass;
// Now PREPARE seed
      feed.seed.erase(feed.seed.begin(), feed.seed.end());

// distribute seed


      for (int pool = 0; pool < conversion->numDmdPoolsSeed(); pool++)
      {
            seed.dm = conversion->dmTotSeed(pool);
            seed.dmd = conversion->dmdValueSeed(pool);        // kg/ha  //fixme
            seed.cp_conc = conversion->cpConcSeed(pool);   // (kg/ha) - parameter cpNRatio = 6.25
            seed.p_conc = conversion->pConcSeed(pool);
            seed.s_conc = conversion->sConcSeed(pool);      //parameter NSRatioLeaf = 19.0, NSRatioStem = 11.0;  seed.s_conc = 0.0023;  kg/ha
            seed.prot_dg = conversion->protDgSeed(pool);     // parameter ProtDegrade = 0.1;  seed.prot_dg = 0.7;    // kg/ha
            seed.ash_alk = conversion->ashAlkSeed(pool);      // seed.ash_alk = 2.0;    // mol/kg
            seed.height_ratio = conversion->heightRatioSeed();

            feed.seed.push_back(seed);
            feed.seed_class.push_back(conversion->seedClass(pool));

            if (seed.dm > 0.0)
            {
               if (c.debug == "on")
               {
                  ostringstream msg;
                  msg << endl << "Seed on offer, pool " << (pool+1) << ":-" << endl
                      << "   dm           = " <<              seed.dm <<      " (kg/ha)" << endl
                      << "   dmd          = " <<              seed.dmd <<     " (-)" <<     endl
                      << "   cp_conc      = " <<              seed.cp_conc << " (kg/kg)" << endl
                      << "   p_conc       = " <<              seed.p_conc <<  " (kg/kg)" << endl
                      << "   s_conc       = " <<              seed.s_conc <<  " (kg/kg)" << endl
                      << "   ash_alk      = " <<              seed.ash_alk << " (mol/kg)" << endl
                      << "   prot_dg      = " <<              seed.prot_dg << " (kg/kg)" << endl
                      << "   dm total     = " <<              conversion->dmTotalSeed() *kg2g/ha2sm << " (g/m2)" << endl
                      << "   height_ratio = " <<              seed.height_ratio << " (-)" << ends;
                  system->writeString (msg.str().c_str());
               }
            }
         plant2StockSent = true;

      } // end of Pools loop

   // Now SEND feed off
   system->sendVariable(queryData, feed);
}


void HerbageConverter::readParameters ( void )
{
//+  Constant Values
    const char*  section_name = "parameters" ;

//- Implementation Section ----------------------------------

    system->writeString (" - herbage converter reading parameters");

    c.debug = system->readParameter (section_name, "debug");

      ostringstream msg;
      msg << "Debug = " << c.debug << ends;
      system->writeString (msg.str().c_str());
}


//===========================================================================
float HerbageConverter::divide (float dividend, float divisor, float default_value)
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





