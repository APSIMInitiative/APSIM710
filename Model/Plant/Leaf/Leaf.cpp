#include "StdPlant.h"

#include "../CompositePart.h"
#include "Leaf.h"
#include "GenericLeaf.h"
#include "CohortingLeaf.h"
#include "../Photosynthesis/PhotosynthesisModel.h"
#include "../Photosynthesis/RUEModel.h"
#include "../Photosynthesis/DCaPST.h"
#include "../Photosynthesis/DCaPSTOld.h"
#include "../Phenology/Phenology.h"
#include "../Environment.h"

using namespace std;

Leaf::Leaf(ScienceAPI& scienceAPI, plantInterface* p, const string& name)
	: SimplePart(scienceAPI, p, name)
{
	// Ben Ababaei (2019.10.14): Constructs all available photosynthesis (Ps) models.
	PsModels = constructPhotosynthesisModel(scienceAPI, *p);

	// Ben Ababaei (2019.10.14): Moved from 'PhotosynthesisModel* constructPhotosynthesisModel'.
	// Read 'photosynthesismodel' from input files, or set to 'rue'.
	if (!scienceAPI.readOptional("photosynthesismodel", PsModelName1)) PsModelName1 = "rue";
	scienceAPI.expose("PsModelName1", "", "Photosynthesis model name", PsModelName1);
	onSetPSModel(PsModelName1);

	TEFactor = 1.0;
	DCaPSTLowLAI = false;

	// Ben Ababaei (2019.10.14): Ps model can be changed dynamically during the simulation.
	scienceAPI.exposeWritable("DCaPSTTriggerLAI", "", "Lai to start using DCaPST", FloatSetter(&Leaf::onSetLaiTrigger));
	scienceAPI.exposeWritable("PsModelName", "", "Photosynthesis model name", StringSetter(&Leaf::onSetPSModel));
}

// Ben Ababaei (2019.10.14): The LAI value at which DCaPST takes over.
void Leaf::onSetLaiTrigger(float value)
{
	DCaPSTTriggerLAI = value;
	DCaPSTLowLAI = false;
}

// Ben Ababaei (2019.10.14): This sets the name of the 'current' Ps model and switches to it.
void Leaf::onSetPSModel(string Factor)
{
	PsModelName = Factor;
	ChoosePsModel(PsModelName);

	//printf("      Initial PsModelName: %10s\n", PsModelName1.c_str());
	//printf("      Current PsModelName: %10s\n", PsModelName.c_str());
	//printf("      gLAI: %5.3f\n", getLAI());
	//printf("      SLN: %5.3f\n", getSLN());
}

// Ben Ababaei (2019.10.14): It chooses the PS model.
void Leaf::ChoosePsModel(string Factor) {
	PsModelName = Factor;

	if (PsModelName == "rue") {
		Photosynthesis = PsModels[0];
	}
	else if (PsModelName == "dcapst") {
		Photosynthesis = PsModels[1];
	}
	else if (PsModelName == "dcapstold") {
		Photosynthesis = PsModels[2];
	}
	else if (PsModelName == "sucros") {
		Photosynthesis = PsModels[3];
	}
	else if (PsModelName == "rue_whole_plant_model") {
		Photosynthesis = PsModels[4];
	}
	else
		throw std::invalid_argument("Unknown photosynthesis model: '" + PsModelName + "'");
}

void Leaf::onInit1(protocol::Component*)
{
	scienceAPI.subscribe("canopy_water_balance", CanopyWaterBalanceFunction(&Leaf::onCanopyWaterBalance));
	scienceAPI.expose("TEFactor", "", "General Factor used to modify TE in plant model", TEFactor);
	scienceAPI.exposeWritable("TEFactor", "", "TEFactor", FloatSetter(&Leaf::onSetTEFactor));
}

void Leaf::onSetTEFactor(float Factor)
{
	TEFactor = Factor;
}

void Leaf::onEndCrop(vector<string>& dm_type,
	vector<float>& dlt_crop_dm,
	vector<float>& dlt_dm_n,
	vector<float>& dlt_dm_p,
	vector<float>& fraction_to_residue)
{
	SimplePart::onEndCrop(dm_type, dlt_crop_dm, dlt_dm_n, dlt_dm_p, fraction_to_residue);
	zeroAllGlobals();
	Photosynthesis->ZeroAll();

	// Ben Ababaei (2019.10.14): All PS models need to be set to zero.
	for (auto& Model : PsModels) Model->ZeroAll();

	// Ben Ababaei (2019.10.14): Get back to the initial Ps model
	ChoosePsModel(PsModelName1);

	TEFactor = 1.0;
	DCaPSTLowLAI = false;
}

void Leaf::onCanopyWaterBalance(protocol::CanopyWaterBalanceType& CWB)
//=======================================================================================
// Handler for CanopyWaterBalance event
{
	ExternalSWDemand = false;
	for (unsigned i = 0; i < CWB.Canopy.size(); i++)
	{
		if (CWB.Canopy[i].name == plant->Name())
		{
			sw_demand = CWB.Canopy[i].PotentialEp;
			ExternalSWDemand = true;
		}
	}
}

void Leaf::doNConccentrationLimits(float modifier)
{
	SimplePart::doNConccentrationLimits(modifier);
	g.n_conc_crit *= modifier;
	if (g.n_conc_crit <= g.n_conc_min)
		throw std::runtime_error("Aiieeee!!\nnconc_crit < nconc_min!\nWhat's happened to CO2??");
}

float Leaf::dmRetransSupply(void)
{
	float dm_part_avail = Green.NonStructuralDM();
	return (l_bound(dm_part_avail, 0.0));
}

void Leaf::doSWDemand(float SWDemandMaxFactor)         //(OUTPUT) crop water demand (mm)
{
	// Ben Ababaei (2019.10.14): If the Ps model calculates SW demand internally, the next section is not needed.
	if (ExternalSWDemand == true)
	{
		transpEff = dlt.dm_pot_rue / sw_demand;
		ExternalSWDemand = false;
	}
	else
	{
		// Return crop water demand from soil by the crop (mm) calculated by
		// dividing biomass production limited by radiation by transpiration efficiency.
		// get potential transpiration from potential
		// carbohydrate production and transpiration efficiency

		cproc_transp_eff_co2_1(plant->environment().vpdEstimate()
			, plant->phenology().doLookup(c.transpEffCf)
			, plant->getCo2Modifier()->te()
			, &transpEff);
		transpEff = transpEff * TEFactor;

		cproc_sw_demand1(dlt.dm_pot_rue - Respiration()
			, transpEff
			, &sw_demand_te);

		// Capping of sw demand will create an effective TE- recalculate it here
		// In an ideal world this should NOT be changed here - NIH

		float SWDemandMax = SWDemandMaxFactor * coverGreen();
		sw_demand = u_bound(sw_demand_te, SWDemandMax);
		transpEff = transpEff * (float)divide(sw_demand_te, sw_demand, 1.0);
	}
	Debug("Leaf.sw_demand=%f", sw_demand);
	Debug("Leaf.transpEff=%f", transpEff);
}

float Leaf::DMSupply(void)
//===========================================================================
{
	// Ben Ababaei (2019.10.14): If the Ps model calculates biomass production limited by radiation and water supply, the next section is not needed.
	if ((PsModelName == "dcapst" || PsModelName == "dcapstold")) {
		return dlt.dm_pot_rue;
	}
	else {
		// Takes biomass production limited by radiation and discounted by water supply.
		if (plant->Tops().SWDemand() > 0.0)
			return dlt.dm_pot_rue * plant->getSwdefPhoto();
		else
			return 0.0;
	}
}

float Leaf::coverTotal(void)
//=======================================================================================
{
	return 1.0f - (1.0f - coverLeaf.green) * (1.0f - coverLeaf.sen);
}

float Leaf::coverGreen(void)
//=======================================================================================
{
	return coverLeaf.green;
}

float Leaf::coverSen(void)
//=======================================================================================
{
	return coverLeaf.sen;
}

void Leaf::doCover(PlantSpatial& spatial)
//===========================================================================
{
	//+  Purpose
	//     Calculate leaf cover

	//+  Changes
	//     19 Jan 2006 JNGH - Programmed and Specified

	//- Implementation Section ----------------------------------

	legnew_cover(spatial.rowSpacing()
		, cXRowSpacing
		, cYExtinctCoef
		, cNumRowSpacing
		, spatial.canopyFac()
		, getLAI()
		, &coverLeaf.green);

	legnew_cover(spatial.rowSpacing()
		, cXRowSpacing
		, cYExtinctCoefDead
		, cNumRowSpacing
		, spatial.canopyFac()
		, getSLAI()
		, &coverLeaf.sen);
	Debug("leaf.cover.green=%f", coverLeaf.green);
	Debug("leaf.cover.sen=%f", coverLeaf.sen);
}

void Leaf::doDmPotRUE(void)		// (OUTPUT) potential dry matter (carbohydrate) production (g/m^2)
   //===========================================================================
{
	// Ben Ababaei (2019.10.14): If the Ps model calculates biomass production limited by radiation and water supply, 
	// the output is not 'potential' dry matter (carbohydrate) production.

	dlt.dm_pot_rue = Photosynthesis->PotentialDM(radiationInterceptedGreen); // this called the original "Potential()" function in previous versions (PFR)
	Debug("Leaf.dlt.dm_pot_rue=%f", dlt.dm_pot_rue);
}


