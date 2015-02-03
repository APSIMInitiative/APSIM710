//------------------------------------------------------------------------------------------------
#include <stdio.h>

#include "Grain.h"
#include "Plant.h"

using namespace Maize;

//---------------------------------------------------------------------------
//------ Grain Constructor
//------------------------------------------------------------------------------------------------
Grain::Grain(ScienceAPI2 &api, Plant *p) : PlantPart(api)
   {
   plant = p;
   name = "Grain";
   partNo = 4;

   doRegistrations();
   initialize();
   }
//------------------------------------------------------------------------------------------------
//------ Grain Destructor
//------------------------------------------------------------------------------------------------
Grain::~Grain()
   {

   }
//--------------------------------------------------------------------------------------------------
// Register variables for other modules
//--------------------------------------------------------------------------------------------------
void Grain::doRegistrations(void)
   {
   scienceAPI.expose("GrainGreenWt",    "g/m^2",       "Live grain dry weight",    false, dmGreen);
   scienceAPI.expose("GrainNo",         "grains/m^2",  "Grain number",             false, grainNo);
   scienceAPI.expose("GrainSize",       "g/1000grain", "1000 grain weight",        false, grainSize);
   scienceAPI.expose("GrainGreenN",     "g/m^2",       "N in grain",               false, nGreen);
   scienceAPI.expose("GrainGreenNConc", "%",           "N concentration in grain", false, nConc);
   scienceAPI.expose("Yield",           "kg/ha",       "Grain yield",              false, yield);
   scienceAPI.expose("GrainGreenP",     "g/m^2",       "P in live Grain",          false, pGreen);
   scienceAPI.expose("GrainTempFactor", "()",         "Stress on Grain Number",    false, tempFactor);
   scienceAPI.expose("DltDMGrainDemand","g/m^2",       "Delta DM Grain Demand",    false, dltDMGrainDemand);
   scienceAPI.expose("PotGrainFillRate","mg/grain/oCd","Potential Grain Fill Rate",false, potGFRate);

   }
//------------------------------------------------------------------------------------------------
//------- Initialize variables
//------------------------------------------------------------------------------------------------
void Grain::initialize(void)
   {
   grainNo = 0.0;
   finalGrainNo = 0.0;
   grainSize = 0.0;
   dltDMGrainDemand = 0.0;
   yield = 0.0;

   plantDMt0 = 0;
   plantDMt1 = 0;
   nDays = 0;
   pKGR = 0.0;                      // potential kernel growth rate in mg/grain/oC

   tempFactor = 1.0;

   PlantPart::initialize();
   }
//------------------------------------------------------------------------------------------------
//------ read Grain parameters
//------------------------------------------------------------------------------------------------
void Grain::readParams (void)
   {

   scienceAPI.read("grn_water_cont", "", 0, waterContent);
   // nitrogen
   scienceAPI.read("grainNFillRate","", 0, grainNFillRate);
   scienceAPI.read("targetGrainNConc", "", 0, targetNConc);

   // phosphorus
   pMaxTable.read(scienceAPI, "x_p_stage_code","y_p_conc_max_grain");
   pMinTable.read(scienceAPI, "x_p_stage_code","y_p_conc_min_grain");
   pSenTable.read(scienceAPI, "x_p_stage_code","y_p_conc_sen_grain");
   scienceAPI.read("p_conc_init_grain", "", 0, initialPConc);   /* TODO : Remove this */

	// grain number and growth
   scienceAPI.read("PGRt0", "", 0, PGRt0);
   scienceAPI.read("PGRt1", "", 0, PGRt1);
   scienceAPI.read("GNmax", "", 0, GNmax);
   GNmaxProlific = -1.0; scienceAPI.read("GNmaxProlific", "", 1, GNmaxProlific);
   scienceAPI.read("PGRbase", "", 0, PGRbase);
   PGRprolific = -1.0; scienceAPI.read("PGRprolific", "", 1, PGRprolific);
   scienceAPI.read("GNk", "", 0, GNk);
   GNkProlific =  -1.0; scienceAPI.read("GNkProlific", "", 1, GNkProlific);
   scienceAPI.read("potKernelWt", "", 0, potKernelWt);

   scienceAPI.read("GNmaxCoef", "", 0, GNmaxCoef);
	
   GNmax = (int)(GNmaxCoef  / potKernelWt * 1000);


	   // heat effects on grain number
   scienceAPI.read("GrainTempWindow","", 0, grainTempWindow);
   scienceAPI.read("GrainTempOrdinals","", 0, grainTempOrdinals);
   vector<double> y;y.push_back(0.0);y.push_back(1.0);
   grainTempTable.load(grainTempOrdinals,y);

	}

//------------------------------------------------------------------------------------------------
//------ update variables
//------------------------------------------------------------------------------------------------
void Grain::updateVars(void)
   {
   // initialise P - must be better way
   if(dmGreen < 1e-5 && dltDmGreen > 0)
      pGreen = initialPConc * dltDmGreen;

   dmGreen += dltDmGreen;
   dmGreen += dmRetranslocate;
   nGreen += dltNGreen  + dltNRetranslocate;
   nConc = divide(nGreen,dmGreen,0) * 100.0;

   grainSize = divide (dmGreen, grainNo, 0.0) * 1000.0;		// 100 grain wt in grams
   stage = plant->phenology->currentStage();
   yield = dmGreen * 10.0;												// in kg/ha

   // Ramp grain number from 0 at StartGrainFill to finalGrainNo at SGF + 100dd
   double gfTTNow = plant->phenology->sumTTtotal(startGrainFill,maturity);
   grainNo = Min((gfTTNow/100.0 *  finalGrainNo),finalGrainNo) * tempFactor;

   }
//------------------------------------------------------------------------------------------------
//------- react to a phenology event
//------------------------------------------------------------------------------------------------
void Grain::phenologyEvent(int iStage)
   {
   switch (iStage)
      {
   case emergence :
      break;
   case startGrainFill :
      pKGR = divide(potKernelWt,plant->phenology->sumTTtarget(startGrainFill, endGrainFill));

      break;
      }
   }
//------------------------------------------------------------------------------------------------
void Grain::process(void)
   {

   // Calculate grain number in the period flowering -  PGRt0 to flowering + PGRt1
   double ttFlowering = plant->phenology->sumTTtarget(sowing,flowering);
   double ttNow = plant->phenology->sumTTtotal(sowing,maturity);
   if(ttNow > ttFlowering - PGRt0 && ttNow < ttFlowering + PGRt1)
	finalGrainNo = calcGrainNumber();

	   // calculate high temperature effects on grain number
   if(stage >= fi && stage <= flowering)
      {
      tempFactor -= calcTempFactor();
      tempFactor = bound(tempFactor,0.0,1.0);
      }

   // calculate grain biomass demand
   if(stage >= startGrainFill && stage <= endGrainFill)
      {
      calcBiomassDemand();
      }
   }
//------------------------------------------------------------------------------------------------
double Grain::calcGrainNumber(void)
   {
   // calculate PGR in g/plant/day between flowering -  PGRt0 and flowering + PGRt1
   double fgn = 0.0;
   nDays++;
   if(plantDMt0 <  1.0)				// occurs on first day in window
      plantDMt0 = plant->biomass->getAboveGroundBiomass() / 10.0;
   plantDMt1 = plant->biomass->getAboveGroundBiomass() / 10.0;		// todays dm

	double PGR = (plantDMt1 - plantDMt0) / nDays / plant->getPlantDensity();	// growth rate per plant per day
         
	if(PGR > 0.1 && PGRprolific > 0.0 ) 
		fgn = max(max(GNmaxProlific * (1.0 - (exp(-GNkProlific * (PGR - PGRprolific)))),
		              GNmax * (1.0 - exp(-GNk * (PGR - PGRbase)))),
		          0.0);
	else if(PGR > 0.1 && PGR > PGRbase)
		fgn = GNmax * (1.0 - exp(-GNk * (PGR - PGRbase)));
         
	fgn *= plant->getPlantDensity();		// per m2

 	return fgn;
   }
//------------------------------------------------------------------------------------------------
double Grain::calcTempFactor(void)
   {
   // calculate a daily contribution to stress on grain number
   // if we are within the grain stress window (grainTempWindow)calculate stress factor
   // from grainTempTable and this day's contribution to the total stress

   // first see if it is a hot day
   if(grainTempTable.value(plant->today.maxT) < 0.001)return 0.0;

   // then see if we are in the pre flag or post-flag window window
   // if not return 0                                      (grainTempWindow[0] is -ve)
   double targetTT = plant->phenology->sumTTtarget (fi, flag) + grainTempWindow[0];
   double eTT = plant->phenology->sumTTtotal (fi, flag);
   if(eTT < targetTT)return 0.0;
   // see if in the post flag window
   double eTTpostFlag = plant->phenology->sumTTtotal (flag, flowering);
   if(eTTpostFlag > grainTempWindow[1]) return 0.0;

   double dltTT = plant->phenology->getDltTT();
   double ttContrib;
   // check  window
   if(eTTpostFlag > 0.0)  // post flag
      ttContrib = Min(grainTempWindow[1] - eTTpostFlag, dltTT);      // allow for overlap
   else                   // pre flag
      ttContrib = Min(eTT - targetTT, dltTT);      // allow for overlap

   double dayFract = ttContrib / (-grainTempWindow[0] + grainTempWindow[1]);
   return dayFract * grainTempTable.value(plant->today.maxT);
   }
//------------------------------------------------------------------------------------------------

//------------------------------------------------------------------------------------------------
void Grain::calcBiomassDemand(void)
   {
   // biomass demand in g/m2
   // grain demand is calculated as the potential kernel growth rate (pKGR) * grain number (grainNo)

   if(stage >= startGrainFill && stage <= endGrainFill)
      dltDMGrainDemand = pKGR * grainNo * mg2gm * plant->phenology->getDltTT();

   }
//------------------------------------------------------------------------------------------------
//------- calc N demand
//------------------------------------------------------------------------------------------------
//     GRAIN demand to keep grain N filling rate at 0.001mg/grain/dd up to halfway
//       between sgf and maturity where dd is degree days from start_grain_fill
//       then target [N] (1.75%)
//------------------------------------------------------------------------------------------------
double Grain::calcNDemand(void)
   {
   nDemand = 0.0;
   // if not in grain fill, no demand
   if(stage < startGrainFill)return nDemand;

   // for the first half of grainfilling, the demand is calculated on a grain
   // filling rate per grain per oCd
   // rest on target N concentration

   double gfFract = divide(plant->phenology->sumTTtotal(startGrainFill, maturity),
      plant->phenology->sumTTtarget(startGrainFill, maturity));

   if(gfFract < 0.5)
      nDemand = grainNo * plant->phenology->getDltTT() * grainNFillRate / 1000.0;
   else
      nDemand = dltDmGreen * targetNConc;

   nDemand = Max(nDemand,0.0);
   return nDemand;
   }
//------------------------------------------------------------------------------------------------
void Grain::RetranslocateN(double N)
   {
   dltNRetranslocate += N;
   }
//------------------------------------------------------------------------------------------------
double Grain::partitionDM(double dltDM)
   {
   dltDmGreen = Min(dltDMGrainDemand, dltDM);
   return Max(dltDmGreen,0.0);
   }
//------------------------------------------------------------------------------------------------
double Grain::grainDMDifferential(void)
   {
   return dltDMGrainDemand - dltDmGreen;
   }
//------------------------------------------------------------------------------------------------
double Grain::calcPDemand(void)
   {
   // Grain P demand   (demand from soil)
   pDemand = 0.0;
   return pDemand;
   }
//------------------------------------------------------------------------------------------------
double Grain::calcPRetransDemand(void)
   {
   // Grain P demand
   double maxP = pConcMax() * dmGreen;
   return Max(maxP - pGreen,0.0);
   }
//------------------------------------------------------------------------------------------------
void Grain::Summary(void)
   {
   char msg[120];
   sprintf(msg,"Stover (kg/ha)        = %.1f \t Grain yield (kg/ha)     = %.1f\n",
      plant->biomass->getAboveGroundBiomass() - dmGreen * 10.0, dmGreen * 10.0);
   scienceAPI.write(msg);
   sprintf(msg,"Grain %% water content = %.1f \t\t Grain yield wet (kg/ha) = %.1f\n",
      waterContent*100,dmGreen * 10.0 * 100 / (100 - waterContent*100));
   scienceAPI.write(msg);
   sprintf(msg,"Weight 1000 grains(g) = %.1f \t\t Grains/m^2              = %.1f\n",
      grainSize, grainNo);scienceAPI.write(msg);
   sprintf(msg,"Grains/head           = %.1f\n",grainNo / plant->getPlantDensity());
   scienceAPI.write(msg);
   }
//------------------------------------------------------------------------------------------------
void  Grain::Harvest(void)
   {
   // send crop_chopped
   if(dmGreen > 0)
      {
      // Build surface residues by part
      vector<string> part_name;
      vector<float> fraction_to_residue;           // fraction sent to residue (0-1)
      vector<float> dlt_dm_crop;                   // change in dry matter of crop (kg/ha)
      vector<float> dlt_dm_n;                      // N content of changed dry matter (kg/ha)
      vector<float> dlt_dm_p;                      // P content of changed dry matter (kg/ha)

      double fracts[] = {0.0, 0.0, 0.0, 0.0, 0.0};  // No root or grain to residue.

      for (unsigned part = 0; part < plant->PlantParts.size(); part++)
         {
         part_name.push_back(plant->PlantParts[part]->getName());
         if(part < 4)
            {
            dlt_dm_crop.push_back(0.0);
            dlt_dm_n.push_back(0.0);
            dlt_dm_p.push_back(0.0);
            }
         else
            {
            dlt_dm_crop.push_back((float)((plant->PlantParts[part]->getDmGreen() +
               plant->PlantParts[part]->getDmSenesced()) * gm2kg/sm2ha));
            dlt_dm_n.push_back((float)((plant->PlantParts[part]->getNGreen() +
               plant->PlantParts[part]->getNSenesced()) * gm2kg/sm2ha));
            dlt_dm_p.push_back((float)((plant->PlantParts[part]->getPGreen() +
               plant->PlantParts[part]->getPSenesced()) * gm2kg/sm2ha));
            }

         fraction_to_residue.push_back((float)fracts[part]);
         }

      Variant chopped;
      chopped.pack("crop_type",   plant->getCropType());
      chopped.pack("dm_type",     part_name);
      chopped.pack("dlt_crop_dm", dlt_dm_crop);
      chopped.pack("dlt_dm_n",    dlt_dm_n);
      chopped.pack("dlt_dm_p",    dlt_dm_p);
      chopped.pack("fraction_to_residue", fraction_to_residue);


      scienceAPI.publish ("crop_chopped", chopped);
      }
   initialize();
   }

