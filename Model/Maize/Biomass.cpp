#include <stdio.h>
#include <ComponentInterface2/Variant.h>

#include "Plant.h"
#include "Biomass.h"

using namespace Maize;
//---------------------------------------------------------------------------
//------ Biomass Constructor
//------------------------------------------------------------------------------------------------
Biomass::Biomass(ScienceAPI2 &api, Plant *p) : PlantProcess(api)
	{
	plant = p;
	initialize();
	doRegistrations();
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
	scienceAPI.expose("Stover",       "kg/ha", "Stover dry weight",        false, stover);


	scienceAPI.exposeFunction("SenescedWt", "g/m^2", "Senesced plant dry weight",
		FloatFunction(&Biomass::getDMSenesced));
	scienceAPI.exposeFunction("dlt_dm_green", "g/m^2", "Plant biomass growth in each part",
		FloatFunction(&Biomass::getDltDMGreen));
	scienceAPI.exposeFunction("dlt_dm_detached", "g/m^2", "Plant biomass detached from each part",
		FloatArrayFunction(&Biomass::getDltDMDetached));
	scienceAPI.exposeFunction("dlt_dm_green_retrans", "g/m^2", "Plant biomass retranslocated from each part",
		FloatArrayFunction(&Biomass::getDltDMGreenRetrans));

	scienceAPI.exposeFunction("biomass_wt", "g/m2", "Total above-ground biomass",FloatFunction(&Biomass::getBiomass));

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
	stover = 0.0;
	//Setup report vectors

	int nParts = plant->PlantParts.size();
	greenDM.assign   (nParts,0.0);
	senescedDM.assign(nParts,0.0);
	dltDMGreen.assign(nParts,0.0);
	dltDMDetachedSen.assign  (nParts,0.0);
	dltDMRetranslocate.assign(nParts,0.0);
	}
//------------------------------------------------------------------------------------------------
//------ read Biomass parameters
//------------------------------------------------------------------------------------------------
void Biomass::readParams (void)
	{
	scienceAPI.read("ratio_root_shoot","", 0, ratioRootShoot);
	ratioRootShoot.insert(ratioRootShoot.begin(),0);  // for compatibility with fortran

	scienceAPI.read("frac_stem2flower", "", 0, stem2FlowerFrac);
	}
//------------------------------------------------------------------------------------------------
void Biomass::process(void)
	{
	calcBiomassTE();
	calcDltBiomass();
	// biomass partitioning
	calcPartitioning();
	// biomass retranslocation
	stage = plant->phenology->currentStage();
	if(stage >= startGrainFill && stage < endGrainFill)
		calcRetranslocation();

	}
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
	stover = aboveGroundBiomass - plant->grain->getDmGreen() * 10.0;

	//Calculate harvest index
	hi = divide(plant->grain->getDmGreen() * 10.0,aboveGroundBiomass, 0.0);

	stage = plant->phenology->currentStage();

	}
//------------------------------------------------------------------------------------------------
//------------------- calculate biomass production due to water (transpiration)
//------------------------------------------------------------------------------------------------
void Biomass::calcBiomassTE(void)
	{
	dltDMPotTE = calcDltDMPotTE();
	}
//------------------------------------------------------------------------------------------------
//------------------- calculate biomass production due to light (limited by water and n)
//------------------------------------------------------------------------------------------------
void Biomass::calcBiomassRUE(double rue, double radnIntercepted)
	{
	effectiveRue = rue * Min(plant->getTempStress(),plant->nitrogen->getPhotoStress());
	dltDMPotRUE =  effectiveRue * radnIntercepted;
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
	double ts = plant->water->getTotalSupply();
	double te = plant->getTranspEff();
	return plant->water->getTotalSupply() * plant->getTranspEff();
	}
//------------------------------------------------------------------------------------------------
//-------------------  Partitioning
//------------------------------------------------------------------------------------------------
void Biomass::calcPartitioning(void)
	{
	// Roots
	// Root must be satisfied. The roots don't take any of the carbohydrate produced
	//  - that is for tops only.  Here we assume that enough extra was produced to meet demand.
	// Thus the root growth is not removed from the carbo produced by the model.
	double biomPool = dltDM;
	stage = plant->phenology->currentStage();
	int currentPhase = (int) stage;
	plant->roots->partitionDM(ratioRootShoot[currentPhase] * biomPool);

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
		biomPool -= plant->stem->partitionDM(biomPool);
		plant->roots->partitionDM(biomPool);
		}
	else
		{
		plant->roots->partitionDM(biomPool);
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
	result = (float)(aboveGroundBiomass / 10);
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
        BiomassRemovedType chopped;
        chopped.crop_type = plant->getCropType();

        float fracts[] = {0.0, 1.0, 1.0, 1.0, 0.0};  // No root or grain to residue.

        // Build surface residues by part
        for (unsigned part = 0; part < plant->PlantParts.size(); part++)
        {
            if (fracts[part] == 0.0)
            {
                chopped.dlt_crop_dm.push_back(0.0);       // change in dry matter of crop (kg/ha)
                chopped.dlt_dm_n.push_back(0.0);          // N content of changed dry matter (kg/ha)
                chopped.dlt_dm_p.push_back(0.0);          // P content of changed dry matter (kg/ha)
            }
            else
            {
                chopped.dlt_crop_dm.push_back((float)((plant->PlantParts[part]->getDmGreen() +
                    plant->PlantParts[part]->getDmSenesced()) * gm2kg/sm2ha));
                chopped.dlt_dm_n.push_back((float)((plant->PlantParts[part]->getNGreen() +
                    plant->PlantParts[part]->getNSenesced()) * gm2kg/sm2ha));
                chopped.dlt_dm_p.push_back((float)((plant->PlantParts[part]->getPGreen() +
                    plant->PlantParts[part]->getPSenesced()) * gm2kg/sm2ha));
            }

            chopped.fraction_to_residue.push_back(fracts[part]);
        }
        scienceAPI.publish("BiomassRemoved", chopped);
		}
	}
//------------------------------------------------------------------------------------------------



