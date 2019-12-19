#include <stdio.h>
#include <ComponentInterface2/Variant.h> 
#include "Plant.h"
#include "Biomass.h"
#include <General\string_functions.h> //dcaps

using namespace Sorghum;
//---------------------------------------------------------------------------
//------ Biomass Constructor
//------------------------------------------------------------------------------------------------
Biomass::Biomass(ScienceAPI2 &api, Plant *p) : PlantProcess(api)
{
	plant = p;
	initialize();
	doRegistrations();

	//Variables for DCAPS
	useDetailedPSModel = false;
	DCaPSModelInitialised = false;
	DCAPSTriggered = false;
	laiTrigger = -1;
}
//------------------------------------------------------------------------------------------------
//------ Biomass Destructor
//------------------------------------------------------------------------------------------------
Biomass::~Biomass()
   {
   }
//--------------------------------------------------------------------------------------------------
// Register variables for other modules
//--------------------------------------------------------------------------------------------------
void Biomass::doRegistrations(void)
   {
   scienceAPI.expose("DeltaGreenWt", "g/m^2", "Daily biomass production",  false, dltDM);
   scienceAPI.expose("Biomass",      "kg/ha", "Total above-ground biomass",false, aboveGroundBiomass);
   scienceAPI.expose("HI",           "()",    "Harvest index",             false, hi);
   scienceAPI.expose("GreenWt",      "g/m^2", "Live plant dry weight",     false, greenBiomass);


   scienceAPI.exposeFunction("SenescedWt", "g/m^2", "Senesced plant dry weight",
                    FloatFunction(&Biomass::getDMSenesced));
   scienceAPI.exposeFunction("dlt_dm_green", "g/m^2", "Plant biomass growth in each part",
                    FloatFunction(&Biomass::getDltDMGreen));
   scienceAPI.exposeFunction("dlt_dm_detached", "g/m^2", "Plant biomass detached from each part",
                    FloatArrayFunction(&Biomass::getDltDMDetached));
   scienceAPI.exposeFunction("dlt_dm_green_retrans", "g/m^2", "Plant biomass retranslocated from each part",
                    FloatArrayFunction(&Biomass::getDltDMGreenRetrans));

   scienceAPI.exposeFunction("biomass_wt", "g/m2", "Total above-ground biomass",
                    FloatFunction(&Biomass::getBiomass));

   
	//dcaps variables
	scienceAPI.expose("laiTrigger", "", "", true, laiTrigger);
	scienceAPI.expose("DOY", "", "", true, DOY);
	scienceAPI.expose("RootShootRatio", "", "", true, RootShootRatio);

	scienceAPI.expose("SWAvailable", "", "", true, SWAvailable);

}
//------------------------------------------------------------------------------------------------
//------- Initialize variables
//------------------------------------------------------------------------------------------------
void Biomass::initialize(void)
   {
   hi =    0.0;
   stage = 0;
   dltDM = 0.0;
   dltDMPotTE =  0.0;
   dltDMPotRUE = 0.0;
   aboveGroundBiomass = 0.0;
   greenBiomass = 0.0;
	totalBiomass = 0.0;
	aboveGroundGreenBiomass = 0.0;
   //Setup report vectors

   int nParts = plant->PlantParts.size();
   greenDM.assign   (nParts,0.0);
   senescedDM.assign(nParts,0.0);
   dltDMGreen.assign(nParts,0.0);
   dltDMDetachedSen.assign  (nParts,0.0);
   dltDMRetranslocate.assign(nParts,0.0);

   }
	DCAPSTriggered = false;

}
//------------------------------------------------------------------------------------------------
//------ read Biomass parameters
//------------------------------------------------------------------------------------------------
void Biomass::readParams(void)
{
	scienceAPI.read("ratio_root_shoot", "", 0, ratioRootShoot);
	ratioRootShoot.insert(ratioRootShoot.begin(), 0);  // for compatibility with fortran

	scienceAPI.read("frac_stem2flower", "", 0, stem2FlowerFrac);

//------------------------------------------------------------------------------------------------
//------ read Biomass parameters
//------------------------------------------------------------------------------------------------
void Biomass::updateVars(void)
   {
   for(unsigned i = 0; i < plant->PlantParts.size(); i++)
      {
      greenDM[i] = plant->PlantParts[i]->getDmGreen();
      senescedDM[i] = plant->PlantParts[i]->getDmSenesced();
      dltDMGreen[i] = plant->PlantParts[i]->getDltDmGreen();
      dltDMDetachedSen[i] = plant->PlantParts[i]->getDltDetDmSenesced();
      dltDMRetranslocate[i] = plant->PlantParts[i]->getDltDmRetranslocate();
      }
   greenBiomass = sumVector(greenDM);
   totalBiomass = greenBiomass + sumVector(senescedDM);

   aboveGroundGreenBiomass = greenBiomass - plant->roots->getDmGreen();
   double aboveGroundSenescedBiomass = sumVector(senescedDM) - plant->roots->getDmSenesced();
   aboveGroundBiomass = (aboveGroundGreenBiomass + aboveGroundSenescedBiomass) * 10.0;  // in kg/ha

   //Calculate harvest index

   hi = divide(plant->grain->getDmGreen() * 10.0,aboveGroundBiomass, 0.0);

   stage = plant->phenology->currentStage();

   }
}
//------------------------------------------------------------------------------------------------
//------------------- Returns true if dcaps is to be used
//------------------------------------------------------------------------------------------------
bool Biomass::useDCAPS()
{
	double lai = plant->leaf->getLAI();

	//Setup fpr DCaPs
	if (laiTrigger > -1)
	{
		useDetailedPSModel = true;
	}

	return DCAPSTriggered == true || (useDetailedPSModel == true && lai >= laiTrigger);
}
//------------------------------------------------------------------------------------------------
//------------------- calculate biomass production due to water (transpiration)
//------------------------------------------------------------------------------------------------
void Biomass::calcBiomassTE(void)
{
	dltDMPotTE = calcDltDMPotTE();

	//Override for use in the biochem photosynthesis model
	if (useDCAPS() == true)
	{
		dltDMPotTE = dltDMPotRUE;
	}
}
//------------------------------------------------------------------------------------------------
//------------------- calculate biomass production due to light (limited by water and n)
//------------------------------------------------------------------------------------------------
void Biomass::calcBiomassRUE(double rue, double radnIntercepted)
{
	double tempStress = plant->getTempStress();
	double nitrogenStress = plant->nitrogen->getPhotoStress();
	double photoStress = plant->phosphorus->getPhotoStress();
	effectiveRue = rue * Min(Min(tempStress,nitrogenStress),photoStress);
	dltDMPotRUE = effectiveRue * radnIntercepted;
}
//------------------------------------------------------------------------------------------------
//------------------- calculate biomass production from DCAPS
//------------------------------------------------------------------------------------------------

void Biomass::calcBiomassDCAPS()
{

	double lai = plant->leaf->getLAI();

	if (useDCAPS())
	{
		DCAPSTriggered = true;

		DOY = plant->today.doy;
		double latitude = plant->phenology->getLatitude();
		double maxT = plant->today.maxT;
		double minT = plant->today.minT;
		double radn = plant->today.radn;

		int currentPhase = (int)stage;
		RootShootRatio = ratioRootShoot[currentPhase];

		SLN = plant->leaf->getSLN();
		SWAvailable = plant->water->getTotalSupply();

		scienceAPI.publish("dodcaps");

		std::vector<float> DCaPSOut;

		if (!scienceAPI.get("dcaps", "", false, DCaPSOut, -1000.0, 1000.0)) {
			throw std::runtime_error("nothing returned from dcap");
		}

		double swDemand, swUptake, radInt;
		long index = 0;

		dltDMPotRUE = DCaPSOut[0];
		swDemand = DCaPSOut[1];
		swUptake = DCaPSOut[2];
		radInt = DCaPSOut[3];

		plant->setRadInt(radInt);
		plant->water->setSWDemand(swDemand);
	}
}
//------------------------------------------------------------------------------------------------
//-------------------
//------------------------------------------------------------------------------------------------
void Biomass::calcDltBiomass(void)
   {
   dltDM = Min(dltDMPotRUE, dltDMPotTE);
   }
//------------------------------------------------------------------------------------------------
//-------------------  Partitioning
//------------------------------------------------------------------------------------------------
void Biomass::calcPartitioning(void)
   {
   calcBiomassPartitioning();
   }
//------------------------------------------------------------------------------------------------
//-------------------  Partitioning
//------------------------------------------------------------------------------------------------
void Biomass::calcRetranslocation(void)
   {
   calcBiomassRetranslocation();
   }
//------------------------------------------------------------------------------------------------
//-------------------  Scenescence
//------------------------------------------------------------------------------------------------
void Biomass::dmScenescence(void)
   {
   plant->roots->calcSenescence();
   plant->leaf->calcSenescence();
   }

//------------------------------------------------------------------------------------------------
double Biomass::calcDltDMPotTE(void)
   {
   return plant->water->getTotalSupply() * plant->getTranspEff();
   }
//------------------------------------------------------------------------------------------------
void Biomass::calcBiomassPartitioning(void)
   {
   // Roots
   // Root must be satisfied. The roots don't take any of the carbohydrate produced
   //  - that is for tops only.  Here we assume that enough extra was produced to meet demand.
   // Thus the root growth is not removed from the carbo produced by the model.
	stage = plant->phenology->currentStage();
   double biomPool = dltDM;

   int currentPhase = (int)plant->phenology->currentStage();
   plant->roots->partitionDM(ratioRootShoot[currentPhase] * biomPool);
   double dmroot = plant->roots->getDltDmGreen();
   double total = dmroot + biomPool;
   double ftp = total;
   //  leaf and stem to fi then rachis as well to flag
   if(stage >= emergence && stage < flag)
      {
      // leaf first
      biomPool -= plant->leaf->partitionDM(biomPool);

      //if stage > fi give some to rachis
      if(stage >= fi)
         biomPool -= plant->rachis->partitionDM(biomPool * stem2FlowerFrac);

      // rest to stem
      plant->stem->partitionDM(biomPool);

      }
   else if(stage >= flag && stage < flowering)
      {
      // we only have rachis and stem growth here
      biomPool -= plant->rachis->partitionDM(biomPool * stem2FlowerFrac);
      plant->stem->partitionDM(biomPool);
      }
   else if(stage >= flowering && stage < maturity)
      {
      //grain filling starts - stem continues when it can
      biomPool -= plant->grain->partitionDM(biomPool);
      plant->stem->partitionDM(biomPool);
      }
   else
      {
      plant->stem->partitionDM(biomPool);
      }
   }
//------------------------------------------------------------------------------------------------
// Calculate plant dry matter delta's due to retranslocation to grain (g/m^2)
void Biomass::calcBiomassRetranslocation(void)
   {
   double grainDifferential = plant->grain->grainDMDifferential();
   if(grainDifferential > 0)
      {
      // we can translocate stem and leaf carbohydrate to grain if needed

      double stemWtAvail = plant->stem->dmRetransAvailable();
      double stemRetrans = Min(grainDifferential,stemWtAvail);
      grainDifferential -= stemRetrans;
      plant->stem->dmRetrans(-1 * stemRetrans);

      double leafWtAvail = plant->leaf->dmRetransAvailable();
      double leafRetrans = Min(grainDifferential,leafWtAvail);

      plant->leaf->dmRetrans(-1 * leafRetrans);
      plant->grain->dmRetrans(stemRetrans + leafRetrans);
      }
   }
//------------------------------------------------------------------------------------------------
//------- Calculate Dry Matter detachment
//------------------------------------------------------------------------------------------------
void Biomass::detachment(vector<double> senDetachFrac)
   {
   for(unsigned i = 0; i < plant->PlantParts.size(); i++)
      {
      plant->PlantParts[i]->dmDetachment(senDetachFrac);
      }
   }
//------------------------------------------------------------------------------------------------
void Biomass::getDMGreen(float &result)
   {
   result = (float)sumVector(greenDM);
   }
//------------------------------------------------------------------------------------------------
void Biomass::getDMSenesced(float &result)
   {
   result = (float)sumVector(senescedDM);
   }
//------------------------------------------------------------------------------------------------
void Biomass::getDltDMGreen(float &result)
   {
   result = (float)sumVector(dltDMGreen);
   }
//------------------------------------------------------------------------------------------------
void Biomass::getDltDMDetached(vector<float> &result)
   {
   DVecToFVec(result, dltDMDetachedSen);
   }
//------------------------------------------------------------------------------------------------
void Biomass::getDltDMGreenRetrans(vector<float> &result)
   {
   DVecToFVec(result, dltDMRetranslocate);
   }
//------------------------------------------------------------------------------------------------
void Biomass::getBiomass(float &result)
   {
   result = (float)(aboveGroundBiomass / 10.0);
   }
//------------------------------------------------------------------------------------------------
void Biomass::Summary(void)
   {
   char msg[80];
   sprintf(msg,"Total above ground biomass    (kg/ha) = %.1f\n",aboveGroundBiomass); scienceAPI.write(msg);
   sprintf(msg,"Green above ground biomass    (kg/ha) = %.1f\n",aboveGroundGreenBiomass * 10); scienceAPI.write(msg);
   sprintf(msg,"Senesced above ground biomass (kg/ha) = %.1f\n",aboveGroundBiomass - aboveGroundGreenBiomass*10); scienceAPI.write(msg);
   }
//------------------------------------------------------------------------------------------------
void Biomass::incorporateResidue(void)
   {
   //Stover + remaining grain into surface residue     called from plantActions doEndCrop
   if(aboveGroundBiomass > 0.0)
      {
      // Build surface residues by part
      vector<string> part_name;
      vector<float> fraction_to_residue;           // fraction sent to residue (0-1)
      vector<float> dlt_dm_crop;                   // change in dry matter of crop (kg/ha)
      vector<float> dlt_dm_n;                      // N content of changed dry matter (kg/ha)
      vector<float> dlt_dm_p;                      // P content of changed dry matter (kg/ha)

      float fracts[] = {0.0, 1.0, 1.0, 1.0, 0.0};  // No root or grain to residue.

      for (unsigned part = 0; part < plant->PlantParts.size(); part++)
         {
         part_name.push_back(plant->PlantParts[part]->getName());
         dlt_dm_crop.push_back((float)((plant->PlantParts[part]->getDmGreen() +
               plant->PlantParts[part]->getDmSenesced()) * gm2kg/sm2ha));
         dlt_dm_n.push_back((float)((plant->PlantParts[part]->getNGreen() +
               plant->PlantParts[part]->getNSenesced()) * gm2kg/sm2ha));
         dlt_dm_p.push_back((float)((plant->PlantParts[part]->getPGreen() +
               plant->PlantParts[part]->getPSenesced()) * gm2kg/sm2ha));

         fraction_to_residue.push_back(fracts[part]);
         }
      dlt_dm_crop[0] = 0.0;
      dlt_dm_n[0] = 0.0;
      dlt_dm_p[0] = 0.0;
      dlt_dm_crop[4] = 0.0;
      dlt_dm_n[4] = 0.0;
      dlt_dm_p[4] = 0.0;

      Variant chopped;
      chopped.pack("crop_type",   plant->getCropType());
      chopped.pack("dm_type",     part_name);
      chopped.pack("dlt_crop_dm", dlt_dm_crop);
      chopped.pack("dlt_dm_n",    dlt_dm_n);
      chopped.pack("dlt_dm_p",    dlt_dm_p);
      chopped.pack("fraction_to_residue", fraction_to_residue);

      scienceAPI.publish ("crop_chopped", chopped);
      }
   }
//------------------------------------------------------------------------------------------------



