//------------------------------------------------------------------------------------------------
#include <stdio.h>
#include "Plant.h"
#include "LeafLER.h"
#include "Utilities.h"

using namespace Maize;
//------------------------------------------------------------------------------------------------
//------ LeafLER Constructor
//------------------------------------------------------------------------------------------------
LeafLER::LeafLER(ScienceAPI2 &api, Plant *p) : Leaf(api, p)
   {
   initialize();
   doRegistrations();
   }
//------------------------------------------------------------------------------------------------
//------ Destructor
//------------------------------------------------------------------------------------------------
LeafLER::~LeafLER()
   {
   }
//--------------------------------------------------------------------------------------------------
// Register variables for other modules
//--------------------------------------------------------------------------------------------------
void LeafLER::doRegistrations(void)
   {
   // doubles
   scienceAPI.expose("nInitLeaves"    , "()"      , "Number of initiated leaves"                   ,false  , nInitLeaves        );
   scienceAPI.expose("nLeafTips"      , "()"      , "Number of leaf tips"                          ,false  , nLeafTips          );
   scienceAPI.expose("nLeafExp"       , "()"      , "Number of leaves that have started to expand" ,false  , nLeafExp           );
   scienceAPI.expose("nLeafFullyExp"  , "()"      , "Number of leaves that have fully expanded"    ,false  , nLeafFullyExp      );
   scienceAPI.expose("nLigules"       , "()"      , "Number of ligules"                            ,false  , nLigulesRep        );
   scienceAPI.expose("LER"            , "()"      , "Leaf Elongation Rate"                         ,false  , LER                );
   scienceAPI.expose("psi"            , "()"      , "psi"                                          ,false  , psi                );
   scienceAPI.expose("dltLERlai"      , "()"      , "dltLERlai"                                    ,false  , dltLERlai          );

   // vectors
   scienceAPI.expose("hRadn",        "Mj/m3",  "Hourly Radiation"                   ,true  ,hRadn              );
   //scienceAPI.expose("hPAR",         "",       "Hourly PAR"                         ,true  ,hPAR               );
   scienceAPI.expose("hTAir",        "ooC",    "Hourly Air Temperature"             ,true  ,TAir              );
   scienceAPI.expose("hTSoil",       "oC",     "Hourly Soil Temperature"            ,true  ,TSoil             );
   scienceAPI.expose("hSVP",         "",       "SVP"                                ,true  ,SVP               );
   scienceAPI.expose("hRH",          "%",      "Hourly Relative Humidity"           ,true  ,RH                );
   scienceAPI.expose("hVPDair",      "",       "Hourly Air VPD Air"                 ,true  ,VPDair            );
   scienceAPI.expose("hDemand",      "mm",     "Hourly Water Demand"                ,true  ,demand            );
   scienceAPI.expose("hSupply",      "mm",     "Hourly Water Supply"                ,true  ,supply            );
   //scienceAPI.expose("hRatio",       "",       "Hourly Water Supply/Demand Ratio"   ,true  ,hRatio             );
   scienceAPI.expose("hTLeaf",       "oC",     "Hourly Leaf Temperature"            ,true  ,TLeaf             );
   scienceAPI.expose("hVPDairLeaf",  "",       "Hourly VPD AirLeaf"                 ,true  ,VPDairLeaf        );
   scienceAPI.expose("hVPDeq",       "",       "Hourly VPD eq"                      ,true  ,VPDeq             );
   scienceAPI.expose("hLER",         "oC",     "Hourly Leaf Enlongation Rate"       ,true  ,hLER               );
   //scienceAPI.expose("LeafSize",     "",       "Size of each leaf"                  ,true  ,leafSize           );  // cols
   //scienceAPI.expose("PlantArea",    "",       "Leaf Area of Plant"                 ,false ,plantArea          );  // cols
   //scienceAPI.expose("leafInit",     "oCd",    "Init time of each leaf"             ,true  ,leafInit           );
   //scienceAPI.expose("leafTip",      "oCd",    "Time of each leaf tip appearance"   ,true  ,leafTip            );
   //scienceAPI.expose("startExpTT",   "oCd",    "Time of each leaf start to expand"  ,true  ,leafStartExpTT     );
   //scienceAPI.expose("liguleTT",     "oCd",    "Time of each ligule appearance"     ,true  ,leafLiguleTT       );
   //scienceAPI.expose("fullyExpTT",   "oCd",    "Time of each leaf expanded"         ,true  ,leafExpTT          );
   //scienceAPI.expose("leafLength",   "mm",     "Length of each leaf"                ,true  ,leafLength         );
   //scienceAPI.expose("leafWidth",    "mm",     "Width of each leaf"                 ,true  ,leafWidth          );
   //scienceAPI.expose("leafArea",     "mm2",    "Area of each leaf"                  ,true  ,leafArea           );
   //scienceAPI.expose("expLeafArea",  "mm2",    "Exposed Area of each leaf"          ,true  ,expLeafArea        );

   }
//--------------------------------------------------------------------------------------------------
// Initial variables
//--------------------------------------------------------------------------------------------------
void LeafLER::initialize(void)
   {
   Leaf::initialize();

   nLeafTips = 0.0;
   nLeafExp = 0.0;
   nLeafFullyExp = 0.0;
   nLigules = 0.0;
   nLigulesRep = 0.0;
   LER = 0.0;
   psi = 0;

   nInitLeaves  = 0.0;
   dltLERlai = 0.0;

   plantLeaves.leaf.clear();
   }
//------------------------------------------------------------------------------------------------
//-----------  read leaf slow wilting parameters
//------------------------------------------------------------------------------------------------
void LeafLER::readParams (void)
   {
   Leaf::readParams();

   scienceAPI.read( "leaf_no_init_at_emerg", "", false, leafNoInitEmerg);
   scienceAPI.read( "tt_leaf_init_to_growth", "", false, ttInitToGrowth);
   scienceAPI.read( "tt_endExp_to_ligule", "", false, ttEndExpToLigule);
   scienceAPI.read( "leaf_no_tip_at_emerg", "", false, leaf_no_tip_at_emerg);
   scienceAPI.read( "leaf_no_ligu_at_emerg", "", false, leaf_no_ligu_at_emerg);
   scienceAPI.read( "LIR", "", false, LIR);
   scienceAPI.read( "LTAR" , "", false, LTAR);
   scienceAPI.read( "LLAR1", "", false, LLAR1);
   scienceAPI.read( "LLAR2", "", false, LLAR2);
   scienceAPI.read( "LLAR3", "", false, LLAR3);
   scienceAPI.read( "leaf_no_change1", "", false, leaf_no_change1);
   scienceAPI.read( "leaf_no_change2", "", false, leaf_no_change2);
   scienceAPI.read( "a", "", false, a);
   scienceAPI.read( "b", "", false, b);
   scienceAPI.read( "c", "", false, c);
   scienceAPI.read( "T0", "", false, T0);
   scienceAPI.read( "diam", "", false, diam);
   scienceAPI.read( "hm", "", false, hm);
   scienceAPI.read( "Pc", "", false, Pc);
   scienceAPI.read( "cp", "", false, cp);
   scienceAPI.read( "a_Psi", "", false, a_Psi);
   scienceAPI.read( "b_Psi", "", false, b_Psi);
   scienceAPI.read( "a_gs", "", false, a_gs );
   scienceAPI.read( "b_gs", "", false, b_gs);
   scienceAPI.read( "c_gs", "", false, c_gs);

   // LER rank coef
   scienceAPI.read( "UseLERCoef", "", false, useLERCoef);
   scienceAPI.read( "aScaleLER", "", false, aScaleLER);
   scienceAPI.read( "bScaleLER", "", false, bScaleLER);
   scienceAPI.read( "LeafNoMaxLER", "", false, LeafNoMaxLER);

   // start of leaf expansion
   scienceAPI.read( "leaf_no_begin_exp_at_emerg", "", false, leaf_no_begin_exp_at_emerg);
   scienceAPI.read( "leaf_begin_exp_rate1", "", false, leaf_begin_exp_rate1);
   scienceAPI.read( "leaf_begin_exp_rate2", "", false, leaf_begin_exp_rate2);
   scienceAPI.read( "leaf_no_change_begin_exp", "", false, leaf_no_change_begin_exp);

   // leaf width
   scienceAPI.read( "UseLeafWidthFn", "", false, useLeafWidthFn);

   double largestLeafSize;  scienceAPI.read( "LargestLeafSize", "", false,largestLeafSize);
   int relRank; scienceAPI.read( "RelRank", "", false,relRank);
   int smallNumber; scienceAPI.read( "SLNumber", "", false,smallNumber);
   double smallSize; scienceAPI.read( "SmallLeafSize", "", false,smallSize);

   leafWidths.SetLeafWidthParams(smallNumber, smallSize, relRank, largestLeafSize);
   plantLeaves.SetLeafWidthParams(smallNumber, smallSize, relRank, largestLeafSize);
   scienceAPI.read( "LeafWidth", "", false, LeafWidth);

   LeafTemp.read(scienceAPI, "LeafTempSD","LeafTempDiff");

   readTParams();
   }
//------------------------------------------------------------------------------------------------
//-----------  read leaf slow wilting parameters
//------------------------------------------------------------------------------------------------
void LeafLER::readTParams (void)
   {
   //latitude
   scienceAPI.get( "latitude",  "", 0, latitude, -90.0f, 90.0f);

   // Diurnal Param For Air
   scienceAPI.get( "maxLag",     "", false, maxLag, -10.0f, 10.0f);
   scienceAPI.get( "nightCoef",  "", false, nightCoef, -10.0f, 10.0f);
   scienceAPI.get( "minLag",     "", false, minLag, -10.0f, 10.0f);
   TAirParam.push_back(maxLag);
   TAirParam.push_back(nightCoef);
   TAirParam.push_back(minLag);
   // Diurnal Param For Soil
   scienceAPI.get( "maxLagSoil",     "", false, maxLag, -10.0f, 10.0f);
   scienceAPI.get( "nightCoefSoil",  "", false, nightCoef, -10.0f, 10.0f);
   scienceAPI.get( "minLagSoil",     "", false, minLag, -10.0f, 10.0f);
   TSoilParam.push_back(maxLag);
   TSoilParam.push_back(nightCoef);
   TSoilParam.push_back(minLag);
   }
//------------------------------------------------------------------------------------------------
//Get functions for registration
//------------------------------------------------------------------------------------------------
/*
void LeafLER::getHourRad(protocol::Component *system, protocol::QueryValueData &qd)
   {
   if(!hRadn.size())
      {
      vector<float> empty(24);
      system->sendVariable(qd, protocol::vector<float>(&empty[0], &empty[0] + empty.size()));
      }
   else
      {
      system->sendVariable(qd, protocol::vector<float>(&hRadn[0], &hRadn[0] + hRadn.size()));
      }
   }
//------------------------------------------------------------------------------------------------
void LeafLER::getHourPAR(protocol::Component *system, protocol::QueryValueData &qd)
   {
   if(!hRadn.size())
      {
      vector<float> empty(24);
      system->sendVariable(qd, protocol::vector<float>(&empty[0], &empty[0] + empty.size()));
      }
   else
      {
      vector<float> par(24);
      for(int i=0;i < 24;i++)
         par[i] = (hRadn[i] * 2.02 / 3600 * 1e6);

      system->sendVariable(qd, protocol::vector<float>(&par[0], &par[0] + par.size()));
      }
   }
//------------------------------------------------------------------------------------------------
void LeafLER::getHourTAir(protocol::Component *system, protocol::QueryValueData &qd)
   {
   if(!TAir.size())
      {
      vector<float> empty(24);
      system->sendVariable(qd, protocol::vector<float>(&empty[0], &empty[0] + empty.size()));
      }
   else
      {
      system->sendVariable(qd, protocol::vector<float>(&TAir[0], &TAir[0] + TAir.size()));
      }
   }
//------------------------------------------------------------------------------------------------
void LeafLER::getHourTSoil(protocol::Component *system, protocol::QueryValueData &qd)
   {
   if(!TSoil.size())
      {
      vector<float> empty(24);
      system->sendVariable(qd, protocol::vector<float>(&empty[0], &empty[0] + empty.size()));
      }
   else
      {
      system->sendVariable(qd, protocol::vector<float>(&TSoil[0], &TSoil[0] + TSoil.size()));
      }
   }
//------------------------------------------------------------------------------------------------
void LeafLER::getHourSVP(protocol::Component *system, protocol::QueryValueData &qd)
   {
   if(!SVP.size())
      {
      vector<float> empty(24);
      system->sendVariable(qd, protocol::vector<float>(&empty[0], &empty[0] + empty.size()));
      }
   else
      {
      system->sendVariable(qd, protocol::vector<float>(&SVP[0], &SVP[0] + SVP.size()));
      }
   }
//------------------------------------------------------------------------------------------------
void LeafLER::getHourRH(protocol::Component *system, protocol::QueryValueData &qd)
   {
   if(!RH.size())
      {
      vector<float> empty(24);
      system->sendVariable(qd, protocol::vector<float>(&empty[0], &empty[0] + empty.size()));
      }
   else
      {
      system->sendVariable(qd, protocol::vector<float>(&RH[0], &RH[0] + RH.size()));
      }
   }
//------------------------------------------------------------------------------------------------
void LeafLER::getHourVPDair(protocol::Component *system, protocol::QueryValueData &qd)
   {
   if(!VPDair.size())
      {
      vector<float> empty(24);
      system->sendVariable(qd, protocol::vector<float>(&empty[0], &empty[0] + empty.size()));
      }
   else
      {
      system->sendVariable(qd, protocol::vector<float>(&VPDair[0], &VPDair[0] + VPDair.size()));
      }
   }
//------------------------------------------------------------------------------------------------
void LeafLER::getHourDemand(protocol::Component *system, protocol::QueryValueData &qd)
   {
   if(!demand.size())
      {
      vector<float> empty(24);
      system->sendVariable(qd, protocol::vector<float>(&empty[0], &empty[0] + empty.size()));
      }
   else
      {
      system->sendVariable(qd, protocol::vector<float>(&demand[0], &demand[0] + demand.size()));
      }
   }
//------------------------------------------------------------------------------------------------
void LeafLER::getHourSupply(protocol::Component *system, protocol::QueryValueData &qd)
   {
   if(!supply.size())
      {
      vector<float> empty(24);
      system->sendVariable(qd, protocol::vector<float>(&empty[0], &empty[0] + empty.size()));
      }
   else
      {
      system->sendVariable(qd, protocol::vector<float>(&supply[0], &supply[0] + supply.size()));
      }
   }
//------------------------------------------------------------------------------------------------
void LeafLER::getHourRatio(protocol::Component *system, protocol::QueryValueData &qd)
   {
   if(!supply.size())
      {
      vector<float> empty(24);
      system->sendVariable(qd, protocol::vector<float>(&empty[0], &empty[0] + empty.size()));
      }
   else
      {
      float hSupplyMax = maxVector(supply);

      vector<float> sd(24);
      for(int i=0;i < 24;i++)
         {
         sd[i] = divide(hSupplyMax,Max(demand[i],0.0001));
         }
      //         sd[i] = divide(supply[i],demand[i]);
      system->sendVariable(qd, protocol::vector<float>(&sd[0], &sd[0] + sd.size()));
      }
   }
//------------------------------------------------------------------------------------------------
void LeafLER::getHourTLeaf(protocol::Component *system, protocol::QueryValueData &qd)
   {
   if(!TLeaf.size())
      {
      vector<float> empty(24);
      system->sendVariable(qd, protocol::vector<float>(&empty[0], &empty[0] + empty.size()));
      }
   else
      {
      system->sendVariable(qd, protocol::vector<float>(&TLeaf[0], &TLeaf[0] + TLeaf.size()));
      }
   }
//------------------------------------------------------------------------------------------------
void LeafLER::getHourVPDairLeaf(protocol::Component *system, protocol::QueryValueData &qd)
   {
   if(!VPDairLeaf.size())
      {
      vector<float> empty(24);
      system->sendVariable(qd, protocol::vector<float>(&empty[0], &empty[0] + empty.size()));
      }
   else
      {
      system->sendVariable(qd, protocol::vector<float>(&VPDairLeaf[0], &VPDairLeaf[0] + VPDairLeaf.size()));
      }
   }
//------------------------------------------------------------------------------------------------
void LeafLER::getHourVPDeq(protocol::Component *system, protocol::QueryValueData &qd)
   {
   if(!VPDeq.size())
      {
      vector<float> empty(24);
      system->sendVariable(qd, protocol::vector<float>(&empty[0], &empty[0] + empty.size()));
      }
   else
      {
      system->sendVariable(qd, protocol::vector<float>(&VPDeq[0], &VPDeq[0] + VPDeq.size()));
      }
   }
//------------------------------------------------------------------------------------------------
void LeafLER::getHourLER(protocol::Component *system, protocol::QueryValueData &qd)
   {
   if(!hLER.size())
      {
      vector<float> empty(24);
      system->sendVariable(qd, protocol::vector<float>(&empty[0], &empty[0] + empty.size()));
      }
   else
      {
      system->sendVariable(qd, protocol::vector<float>(&hLER[0], &hLER[0] + hLER.size()));
      }
   }
//------------------------------------------------------------------------------------------------
void LeafLER::getLeafSize(protocol::Component *system, protocol::QueryValueData &qd)
   {
   // col's
   for(int i=leafSize.size();i <= 24;i++)
      leafSize.push_back(0);
   system->sendVariable(qd, protocol::vector<float>(&leafSize[0], &leafSize[0] + leafSize.size()));
   }

//------------------------------------------------------------------------------------------------
void LeafLER::getLeafInit(protocol::Component *system, protocol::QueryValueData &qd)
   {
   // init time of each leaf
   vector<float> leafInit;
   for(int i=0;i <= 24;i++)leafInit.push_back(0);
   for(unsigned i=0;i < plantLeaves.leaf.size();i++)
      leafInit[i] = plantLeaves.leaf[i].initTT;
   system->sendVariable(qd, protocol::vector<float>(&leafInit[0], &leafInit[0] + leafInit.size()));
   }
//------------------------------------------------------------------------------------------------
void LeafLER::getLeafTip(protocol::Component *system, protocol::QueryValueData &qd)
   {
   // init time of each leaf
   vector<float> leafTip;
   for(int i=0;i <= 24;i++)leafTip.push_back(0);
   for(unsigned i=0;i < plantLeaves.leaf.size();i++)
      leafTip[i] = plantLeaves.leaf[i].tipTT;
   system->sendVariable(qd, leafTip);
   }
//------------------------------------------------------------------------------------------------
void LeafLER::getLeafLiguleTT(protocol::Component *system, protocol::QueryValueData &qd)
   {
   // init time of each leaf
   vector<float> leafLig;
   for(int i=0;i <= 24;i++)leafLig.push_back(0);
   for(unsigned i=0;i < plantLeaves.leaf.size();i++)
      leafLig[i] = plantLeaves.leaf[i].liguleTT;
   system->sendVariable(qd, leafLig);
   }

//------------------------------------------------------------------------------------------------
void LeafLER::getLeafLength(protocol::Component *system, protocol::QueryValueData &qd)
   {
   // length of each leaf
   vector<float> leafLength;
   for(int i=0;i <= 24;i++)leafLength.push_back(0);
   for(unsigned i=0;i < plantLeaves.leaf.size();i++)
      leafLength[i] = plantLeaves.leaf[i].length;
   system->sendVariable(qd, leafLength);
   }
//------------------------------------------------------------------------------------------------
void LeafLER::getLeafWidth(protocol::Component *system, protocol::QueryValueData &qd)
   {
   // width of each leaf
   vector<float> leafWidth;
   for(int i=0;i <= 24;i++)leafWidth.push_back(0);
   for(unsigned i=0;i < plantLeaves.leaf.size();i++)
      leafWidth[i] = plantLeaves.leaf[i].width;
   system->sendVariable(qd, protocol::vector<float>(&leafWidth[0], &leafWidth[0] + leafWidth.size()));
   }
//------------------------------------------------------------------------------------------------
void LeafLER::getLeafArea(protocol::Component *system, protocol::QueryValueData &qd)
   {
   // area of each leaf
   vector<float> leafArea;
   for(int i=0;i <= 24;i++)leafArea.push_back(0);
   for(unsigned i=0;i < plantLeaves.leaf.size();i++)
      leafArea[i] = plantLeaves.leaf[i].area;
   system->sendVariable(qd, leafArea);
   }
//------------------------------------------------------------------------------------------------
void LeafLER::getExpLeafArea(protocol::Component *system, protocol::QueryValueData &qd)
   {
   // area of each leaf
   vector<float> leafArea;
   for(int i=0;i <= 24;i++)leafArea.push_back(0);
   for(unsigned i=0;i < plantLeaves.leaf.size();i++)
      leafArea[i] = plantLeaves.leaf[i].exposedArea;
   system->sendVariable(qd, leafArea);
   }

//------------------------------------------------------------------------------------------------
void LeafLER::getLeafExpTT(protocol::Component *system, protocol::QueryValueData &qd)
   {
   // length of each leaf
   vector<float> leafExpTT;
   for(int i=0;i <= 24;i++)leafExpTT.push_back(0);
   for(unsigned i=0;i < plantLeaves.leaf.size();i++)
      leafExpTT[i] = plantLeaves.leaf[i].fullyExpTT;
   system->sendVariable(qd, leafExpTT);
   }

//------------------------------------------------------------------------------------------------
void LeafLER::getLeafStartExpTT(protocol::Component *system, protocol::QueryValueData &qd)
   {
   // length of each leaf
   vector<float> leafExpTT;
   for(int i=0;i <= 24;i++)leafExpTT.push_back(0);
   for(unsigned i=0;i < plantLeaves.leaf.size();i++)
      leafExpTT[i] = plantLeaves.leaf[i].startExpTT;
   system->sendVariable(qd, leafExpTT);
   }

//------------------------------------------------------------------------------------------------

void LeafLER::getPlantArea(protocol::Component *system, protocol::QueryValueData &qd)
   {
   // area of plant
   vector<float> leafArea;
   for(int i=0;i <= 24;i++)leafArea.push_back(0);
   for(unsigned i=0;i < plantLeaves.leaf.size();i++)
      leafArea[i] = plantLeaves.leaf[i].area;
   float plantArea = sumVector(leafArea);

   system->sendVariable(qd, plantArea);
   }
   */
//------------------------------------------------------------------------------------------------
//-----------  replace calculation of leaf number
//------------------------------------------------------------------------------------------------
void LeafLER::calcLeafNo()
	{
   // calculate final leaf number up to initiation
   if(stage >= emergence && stage <= fi)
      {
      calcFinalLeafNo();
      // calculate potential individual leaf areas
      calcLeafSize();
      // LER Model
      // up to initiation, update the leaves (thermal time targets, potential width
      updateLeaves();

      // calculated computed leaf widths dependent on the final leaf number    finalLeafNo
      if(useLeafWidthFn)
         leafWidths.SetWidths(LeafWidth,(int)finalLeafNo);
      
      }
   if(stage >= emergence)
      {
      calcLeafAppearance();
      growLeaves();
      }
	}
//------------------------------------------------------------------------------------------------
//-----------  replace calculation of potential area
//------------------------------------------------------------------------------------------------
void LeafLER::calcPotentialArea()
	{
   dltPotentialLAI = 0.0;
   dltStressedLAI = 0.0;

   if(stage >= emergence && stage <= flag)
      {
      // Use LER instead of Bell Curve
      dltPotentialLAI = dltLERlai;
      dltStressedLAI = calcStressedLeafArea();
      }
	}
//------------------------------------------------------------------------------------------------
//-----------  replace calculation of stressed leaf area
//------------------------------------------------------------------------------------------------
double LeafLER::calcStressedLeafArea()
	{
	return dltPotentialLAI;// * Min(plant->water->getExpansionStress(), plant->nitrogen->getExpansionStress());
	}
//------------------------------------------------------------------------------------------------
//-----------  replace calculation of emergence of flag leaf in TT units
//------------------------------------------------------------------------------------------------
double LeafLER::calcEmergFlagTT()
   {
   // calculated emergence of flag leaf with LER model rather than leaf appearence rates.
   return plantLeaves.leaf[plantLeaves.leaf.size() - 1].liguleTT;
   }
//------------------------------------------------------------------------------------------------
//-----------  update LER leaf model
//------------------------------------------------------------------------------------------------
void LeafLER::updateLeaves()
   {
   // between emergence and initiation, update the leaf parameters
   // if the number of leaves hasn't changed, no update needed
   // first time through, sets up  ceil(finalLeafNo) leaves
   int nLeavesNeeded = (int)ceil(finalLeafNo);
   int nLeavesNow = plantLeaves.nLeaves();

   if (nLeavesNeeded == nLeavesNow)
      {
      return;                                        // warning EARLY FUNCTION EJECTION
      }


   // Thermal time targets for each leaf. easiest to recalculate all
   // calculate tt targets for initiation of the leaves
   // tt from 0
   double ttElapsed = plant->phenology->sumTTtotal(sowing,flag);
   double dltTT = plant->phenology->getDltTT();
   ttElapsed -= dltTT;
   vector<double> initTT(nLeavesNeeded);
   for(int i = 0;i < nLeavesNeeded;i++)
      {
      if(i < leafNoInitEmerg)                // e.g. 4.8929
         {
         if(i < floor(leafNoInitEmerg))
            {
            initTT[i] = ttElapsed;
            }
         else 
            {
            initTT[i] = ttElapsed + (1 - (leafNoInitEmerg - floor(leafNoInitEmerg))) * 1/LIR;
            }
         }
      else  
         {
         initTT[i] = initTT[i-1] + 1/LIR;
         }
      }

   /*   vector<double> initTT(nLeavesNeeded);
   for(int i = 0;i < nLeavesNeeded;i++)
   if(i < leafNoInitEmerg)                // e.g. 4.8929
   if(i < floor(leafNoInitEmerg)) initTT[i] = 0.0;
   else initTT[i] = (leafNoInitEmerg - floor(leafNoInitEmerg)) * 1/LIR;
   else  initTT[i] = initTT[i-1] + 1/LIR;               */

   // calculate tt targets for start expansion of the leaves
   vector<double> startExpTT(nLeavesNeeded);
   for(int i = 0;i < nLeavesNeeded;i++)
      {
      if(i < leaf_no_begin_exp_at_emerg)                // e.g. 2.48
         {
         if(i < floor(leaf_no_begin_exp_at_emerg))
            {
            startExpTT[i] = ttElapsed;
            }
         else 
            {
            startExpTT[i] = 
               ttElapsed + ( 1 - (leaf_no_begin_exp_at_emerg - floor(leaf_no_begin_exp_at_emerg))) * 1/leaf_begin_exp_rate1;
            }
         }
      else
         {
         if(i < leaf_no_change_begin_exp)
            {
            startExpTT[i] = startExpTT[i-1] + 1/leaf_begin_exp_rate1;
            }
         else 
            {
            startExpTT[i] = startExpTT[i-1] + 1/leaf_begin_exp_rate2;
            }
         }
      }

   // calculate tt targets for tips of the leaves
   vector<double> tipTT(nLeavesNeeded);
   for(int i = 0;i < nLeavesNeeded;i++)
      {
      if(i < leaf_no_tip_at_emerg)                // e.g. 2.445
         {
         if(i < floor(leaf_no_tip_at_emerg))
            {
            tipTT[i] = ttElapsed;
            }
         else
            {
            tipTT[i] = ttElapsed + (1 - (leaf_no_tip_at_emerg - floor(leaf_no_tip_at_emerg))) * 1/LTAR;
            }
         }
      else  
         {
         tipTT[i] = tipTT[i-1] + 1/LTAR;
         }
      }

   // calculate tt targets for leaf ligule appearance
   vector<double> liguleTT(nLeavesNeeded);
   for(int i = 0;i < nLeavesNeeded;i++)
      {
      if(i < leaf_no_ligu_at_emerg)                // e.g. 0.6662
         {
         if(i < floor(leaf_no_ligu_at_emerg))
            {
            liguleTT[i] = ttElapsed + (1 - (leaf_no_ligu_at_emerg - floor(leaf_no_ligu_at_emerg))) * 1/LLAR1;
            }
         else
            {
            liguleTT[i] = ttElapsed + (1 - (leaf_no_ligu_at_emerg - floor(leaf_no_ligu_at_emerg))) * 1/LLAR1;
            }
         }
      else
         {
         double deltaTT;
         if(i < nLeavesNeeded - leaf_no_change1) 
            {
            deltaTT = 1/LLAR1;
            }
         else if(i < nLeavesNeeded - leaf_no_change2) 
            {
            deltaTT = 1/LLAR2;
            }
         else if(LLAR3 < 0.00001)
            {
            deltaTT = 0;
            }
         else 
            {
            deltaTT = 1/LLAR3;
            }
         liguleTT[i] = liguleTT[i-1] + deltaTT;
         }
      }


      // add any new leaves needed
      for(int i = nLeavesNow;i < nLeavesNeeded;i++)
         {
         PlantLeaf newLeaf;
         // tt targets
         newLeaf.initTT = initTT[i];
         newLeaf.startExpTT = startExpTT[i];
         newLeaf.tipTT = tipTT[i];
         double ttDiff = Min(ttEndExpToLigule,i * ttEndExpToLigule / 4);
         newLeaf.fullyExpTT = Max(0.0,liguleTT[i] - ttDiff);
         newLeaf.liguleTT = liguleTT[i];

         newLeaf.LERCoef = Max(0.4,aScaleLER * exp(-0.5 * pow(((LeafNoMaxLER - (i + 1))/bScaleLER),2)));

         plantLeaves.leaf.push_back(newLeaf);
         }

      // calculate the potential width of the leaves
      plantLeaves.potentialWidths(nLeavesNeeded);
      ////int leavesNow = plantLeaves.nLeaves();
   }
//------------------------------------------------------------------------------------------------
//-----------  grow LER leaf model
//------------------------------------------------------------------------------------------------
void LeafLER::growLeaves()
   {
   // use the LER approach to calculate leaf size and total area
   // calculate leaf elongation rate
   LER = calcLER();

   // fix the last fraction  - should be done elsewhare
   for(int i = 0;i < finalLeafNo;i++)
      {
      if(i < floor(finalLeafNo))
         {
         plantLeaves.leaf[i].fracPopn = 1.0;
         }
      else 
         {
         plantLeaves.leaf[i].fracPopn = finalLeafNo - floor(finalLeafNo);
         }
      }


      // some variables
      double ttElapsed = plant->phenology->sumTTtotal(sowing,flag);
      double dltTT = plant->phenology->getDltTT();
      // we want ttElapsed to be at the beginning of today
      ttElapsed -= dltTT;  ttElapsed = Max(0.0,ttElapsed);
      // data structure plantLeaves

      // go through the leaves.
      // if in the growing stage (between begin and end expansion)
      //    add length (LER *dltTT)

      // get the current total leaf area so we can calculate the plant growth later
      double plantArea = plantLeaves.exposedArea();           // current total leaf area

      for(unsigned leafNo = 0;leafNo < plantLeaves.leaf.size();leafNo++)
         {
         if (ttElapsed + dltTT > plantLeaves.leaf[leafNo].startExpTT &&
            ttElapsed < plantLeaves.leaf[leafNo].fullyExpTT) //in growing period
            {

            // some mucking around to take part days into account
            double growthTT = Min(dltTT,ttElapsed + dltTT - plantLeaves.leaf[leafNo].startExpTT);
            growthTT = Min(growthTT,plantLeaves.leaf[leafNo].fullyExpTT - ttElapsed);

            double ttLER;
            if(useLERCoef)
               ttLER = divide(LER * plantLeaves.leaf[leafNo].LERCoef,dltTT);      // expansion rate in mm/oCd   (LER in mm/day)
            else
               ttLER = divide(LER,dltTT);      // expansion rate in mm/oCd   (LER in mm/day)

            double dltLength = growthTT * ttLER;      // mm
            plantLeaves.leaf[leafNo].length += dltLength;
            // estimate width - final width * fraction through expansion
            double expFraction = divide((ttElapsed + dltTT - plantLeaves.leaf[leafNo].startExpTT),
               (plantLeaves.leaf[leafNo].fullyExpTT - plantLeaves.leaf[leafNo].startExpTT));

            expFraction = Max(Min(expFraction,1),0);
            plantLeaves.leaf[leafNo].width = expFraction *  plantLeaves.leaf[leafNo].potentialWidth;
            plantLeaves.leaf[leafNo].CalcArea();
            }
         // calculate exposed(green) leaf
         // if before tip appearance then 0
         // else calculate the fraction we are between tip and ligule
         // and use that as a fraction of the current expanded area

         double exposedFraction = divide((ttElapsed + dltTT - plantLeaves.leaf[leafNo].tipTT),
            (plantLeaves.leaf[leafNo].liguleTT - plantLeaves.leaf[leafNo].tipTT));
         exposedFraction = Max(Min(exposedFraction,1),0);
         plantLeaves.leaf[leafNo].exposedArea = exposedFraction * plantLeaves.leaf[leafNo].area;

         }
      double newPlantArea = plantLeaves.exposedArea();
      dltLERlai = (newPlantArea - plantArea) * density / 1000000;
      dltLERlai = Max(dltLERlai,0.0);



      // update reporting variables
      //   nInitLeaves = 0;

      vector<double> vals;


      for(unsigned leafNo = 0;leafNo < plantLeaves.leaf.size();leafNo++)
         {
         vals.push_back(plantLeaves.leaf[leafNo].initTT);
         }
      nInitLeaves = Min(valVector(vals,ttElapsed),finalLeafNo);
      vals.clear();
      for(unsigned leafNo = 0;leafNo < plantLeaves.leaf.size();leafNo++)
         {
         vals.push_back(plantLeaves.leaf[leafNo].startExpTT);
         }
      //   if(ttElapsed < 1)nLeafExp = floor(leaf_no_begin_exp_at_emerg); // at emergence
      //   else
      nLeafExp = Min(valVector(vals,ttElapsed),finalLeafNo);
      vals.clear();
      for(unsigned leafNo = 0;leafNo < plantLeaves.leaf.size();leafNo++)
         {
         vals.push_back(plantLeaves.leaf[leafNo].fullyExpTT);
         }
      nLeafFullyExp = Min(valVector(vals,ttElapsed),finalLeafNo);
      vals.clear();

      for(unsigned leafNo = 0;leafNo < plantLeaves.leaf.size();leafNo++)
         {
         vals.push_back(plantLeaves.leaf[leafNo].tipTT);
         }
      //   if(ttElapsed < 1)nLeafTips = floor(leaf_no_tip_at_emerg); // at emergence
      //  else
      nLeafTips = Min(valVector(vals,ttElapsed),finalLeafNo);
      vals.clear();
      for(unsigned leafNo = 0;leafNo < plantLeaves.leaf.size();leafNo++)
         {
         vals.push_back(plantLeaves.leaf[leafNo].liguleTT);\
         }
      //   if(ttElapsed < 1)nLigules = floor(leaf_no_ligu_at_emerg); // at emergence
      //   else
      nLigules = Min(valVector(vals,ttElapsed),finalLeafNo);
   }
//------------------------------------------------------------------------------------------------
//-----------  calculate Leaf Extension Rate
//------------------------------------------------------------------------------------------------
double LeafLER::calcLER(void)
   {
   //clear LER variables
   hRadn.clear();
   TAir.clear();
   SVP.clear();
   RH.clear();
   VPDair.clear();
   supply.clear();
   VPDairLeaf.clear();
   VPDeq.clear();
   hLER.clear();
   demand.clear();
   supply.clear();
   TLeaf.clear();
   TSoil.clear();

   // daily calculation of elongation rate given environment and genetic factors
   // LER = (Tleaf - T0)(A + b * VPDair-leaf -c * T)
   // need hourly values
   // first air temperature

   int doy = plant->today.doy;
   calcHourlyWeatherVars(latitude, doy, plant->today.radn, plant->today.maxT, plant->today.minT, TAirParam,
						  hRadn, TAir, SVP, RH, VPDair);

   // Do only Temperature for Soils
   double LatR = M_PI /180.0 * latitude;      // convert latitude (degrees) to radians
   HourlyTemperature(plant->today.maxT,plant->today.minT, TSoilParam, doy, LatR, TSoil);

   // hourly demand
   double RUE = plant->getRUECf();
   double TEc = plant->getTranspEffCf();
   CalcTDemand(hRadn, coverGreen, RUE, TEc, VPDair, plant->water->getDemand(), demand);

   // calculate supply
   CalcTSupply(plant->water->getTotalSupply(),demand,supply);
//   double psi;
   if(useProfileFTSW)
      psi = CalcPsi(plant->water->getProfileSwAvailRatio());
   else
      psi = CalcPsi(plant->water->getSwAvailRatio());

   // calculate TLeaf
   TLeaf = TAir;
   CalcTLeaf(supply, demand, &LeafTemp, TLeaf);

   // calculate VPDairLeaf
   CalcVPDairLeaf(TAir, TLeaf,RH, VPDairLeaf);

   // calculate VPDeq
   CalcVPDeq(hRadn, VPDairLeaf, VPDeq);

   // calculate hLER
   CalcHLER(T0, a, b, c, TLeaf, VPDeq, psi, hLER);


   return sumVector(hLER)/24.0;
   }
//------------------------------------------------------------------------------------------------
//-----------  calculate Potential Widths
//------------------------------------------------------------------------------------------------
void PlantLeaves::potentialWidths(int fln)
   {
   // calculate the potential width of the leaves. fn(fln)
   // fln   : final leaf number

   // small leaves first
   for(int i=0;i < SLnumber;i++)
      leaf[i].potentialWidth = SLsize;
   // now big leaves
   for(int i = 0;i < 3;i++)
      leaf[fln - BLrankFLN - 2 + i].potentialWidth = BLsize;

   // get slope of leaf size between smallest and largest
   double deltaY = BLsize - SLsize;
   double deltaX = (fln - BLrankFLN - 1) - SLnumber;
   double slope = divide(deltaY,deltaX);
   // between smallest and largest
   for(int i = SLnumber; i < (fln - BLrankFLN - 1);i++)
      leaf[i].potentialWidth = SLsize + slope * (i + 1 - SLnumber);
   // between largest and last
   for(int i = (fln - BLrankFLN + 1); i < fln;i++)
      leaf[i].potentialWidth = BLsize - slope * (i + 1 - (fln - BLrankFLN + 1));

   };
//------------------------------------------------------------------------------------------------
//-----------  calculate Transpiration Demand
//------------------------------------------------------------------------------------------------
void LeafLER::CalcTDemand(vector <double> hRadn, double cover, double RUE, double TEc, vector<double> VPDair,
                double sw_demand, vector<double> &Td)
   {
   // calculate hourly demand then adjust so that it totals the daily demand
   for(int i=0;i < 24; i++)
      {
      double td = (hRadn[i] * cover * RUE) / ((TEc / VPDair[i]) / 10e-4);
//      Td.push_back((hRadn[i] * cover * RUE) / ((TEc / VPDair[i]) / 10e-4));
      Td.push_back(Max(td,0.00001));
      }
   double demand = sumVector(Td);
   double scale = 1;
   if(demand > 0.0)scale = sw_demand / demand;
   for(unsigned i = 0;i < 24;i++) Td[i] = (Td[i] * scale);
   }
//------------------------------------------------------------------------------------------------
//-----------  calculate Hourly Leaf Extension Rate
//------------------------------------------------------------------------------------------------
void LeafLER::CalcHLER(double T0, double a, double b, double c, vector <double> TLeaf,
         vector <double> VPDairLeaf, double psi, vector <double> &LER)
   {
   for(int i=0;i < 24;i++)
      LER.push_back(Max(0.0,(TLeaf[i] - T0) * (a + b * VPDairLeaf[i] + c * psi)));
   }
//------------------------------------------------------------------------------------------------