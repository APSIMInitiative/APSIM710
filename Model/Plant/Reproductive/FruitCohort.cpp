#include "StdPlant.h"

#include "FruitCohort.h"

#include "GrainPart.h"
#include "GrainPartGN.h"
#include "GrainPartHI.h"
#include "PodPart.h"

using namespace std;

// ##############################################################
// DPH: Most of this file can be removed by relying on
// composite part to do the looping over parts for dmGreenVeg,
// dmSenescedVeg etc. We would need to put in lines in the grain
// parts to return 0.0 for the dmGreenVeg, dmSenescedVeg etc.
// ##############################################################

//  initialise data members.
FruitCohort::FruitCohort(XMLNode params, ScienceAPI& scienceAPI, plantInterface *p, const string &name)
   : CompositePart(params, scienceAPI, p, name)
{
parameters = params;
}

// destructor
FruitCohort::~FruitCohort()
   // ====================================================================
   {
   }

void FruitCohort::onInit1(protocol::Component *system)
   //===========================================================================
   {
   zeroAllGlobals(); zeroDeltas();

   grainPart = dynamic_cast<fruitGrainPart*> (&find("grain"));
   podPart = dynamic_cast<fruitPodPart*> (&find("pod"));


   myVegParts.push_back(podPart);

   // call into base class.
   CompositePart::onInit1(system);

   // register some other things.
   //system->addGettableVar("dlt_dm_fruit", gDlt_dm, "g/m^2", "Change in dry matter");
   }

void FruitCohort::checkBounds (void)
{
   for (vector<plantPart *>::iterator part = myParts.begin();
        part != myParts.end();
        part++)
      (*part)->checkBounds();
}

// Assigment operator
//  assign data members of object
const FruitCohort &FruitCohort::operator=(const FruitCohort &/*other*/)
   //===========================================================================
{
   throw std::invalid_argument("Assignment operator NI for FruitCohort");
}

float FruitCohort::nMax(void)
   //===========================================================================
{
   float nMax = 0.0;
   vector<plantPart *>::const_iterator part;
   for (part = myVegParts.begin(); part != myVegParts.end(); part++)
      nMax += (*part)->nMax();
   return nMax;
}

float FruitCohort::pMaxPot(void)
   //===========================================================================
{
   float pMaxPot = 0.0;
   vector<plantPart *>::const_iterator part;
   for (part = myVegParts.begin(); part != myVegParts.end(); part++)
      pMaxPot += (*part)->pMaxPot();
   return pMaxPot;
}

float FruitCohort::pMinPot(void)
   //===========================================================================
{
   float pMinPot = 0.0;
   vector<plantPart *>::const_iterator part;
   for (part = myVegParts.begin(); part != myVegParts.end(); part++)
      pMinPot += (*part)->pMinPot();
   return pMinPot;
}

void FruitCohort::doDmDemand ( float dlt_dm_veg_supply)
   //===========================================================================
{
   doProcessBioDemand();
   //       (OUTPUT) assimilate demand for reproductive part (g/m^2)
   // calculate demands of reproductive parts
   podPart->doDmDemand(dlt_dm_veg_supply);
}

//=======================================================================================
// This object is treated as the plant Head for Stock. Combine the parts
// Pod, Meal and Oil into the Head. 
// Will generate these cohorts for the animal. Where head is either head_ripe/head_unripe
//	head, green, ddm
//	head, green, idm
//	head, senesced, ddm
//	head, senesced, idm
void FruitCohort::get_AvailableToAnimal(protocol::AvailableToAnimalType &avail)
{
	double seedPartTotal[4];			// weight of each cohort sent to animal
	double seedPoolWt[3][2][2];			// weight of each plant part, age, dig

	float sn = podPart->phenology();	// determine the stage for ripe/unripe
	
	protocol::AvailableToAnimalType dm;
	grainPart->get_AvailableToAnimal(dm);	// the grain part is meal and oil
	podPart->get_AvailableToAnimal(dm);

	// initialise the output array
	protocol::AvailableToAnimalelementType cohorts[4];
	for (int i = 0; i < 4; i++)
	{
		cohorts[i].Bottom = 0.0;
		cohorts[i].Weight = 0.0;
		cohorts[i].N = 0.0;
		cohorts[i].P = 0.0;
		cohorts[i].S = 0.0;
		cohorts[i].AshAlk = 0.0;
		seedPartTotal[i] = 0.0;		
	}

	// store the amounts for each cohort so fractions contributed to the 
	// cohorts sent to the animals can be calculated and stored
	int part, age, dig;
	for (size_t i = 0; i < dm.element.size(); i++)
	{
		protocol::AvailableToAnimalelementType item = dm.element[i];
		if (Str_i_Cmp(item.Organ, "meal") == 0)
			part = MEAL;
		else if (Str_i_Cmp(item.Organ, "oil") == 0)
			part = OIL;
		else if (Str_i_Cmp(item.Organ, "pod") == 0)
			part = POD;
		if (Str_i_Cmp(item.AgeID, "green") == 0)
			age = AGEGREEN;
		else if (Str_i_Cmp(item.AgeID, "senesced") == 0)
			age = AGESENESCED;
		if (Str_i_Cmp(item.Chem, "ddm") == 0)
			dig = DDM;
		else if (Str_i_Cmp(item.Chem, "idm") == 0)
			dig = IDM;
		seedPoolWt[part][age][dig] = item.Weight;
	}

	int index = -1;
	//for each head part of this plant build the cohorts for the animal
	for (size_t i = 0; i < dm.element.size(); i++)
	{
		protocol::AvailableToAnimalelementType item = dm.element[i];
		if (Str_i_Cmp(item.Chem, "ddm") == 0)
		{
			index = 0;
		}
		else
		{
			index = 1;
		}

		if (Str_i_Cmp(item.AgeID, "senesced") == 0)
		{
			index += 2;
		}
			   
		if (index >= 0)
		{
			cohorts[index].CohortID = item.CohortID;
			cohorts[index].AgeID = item.AgeID;
			if (sn >= 9.0)
				cohorts[index].Organ = "head_ripe";
			else
				cohorts[index].Organ = "head_unripe";
			cohorts[index].Bottom = min(cohorts[index].Bottom, item.Bottom);
			cohorts[index].Top = max(cohorts[index].Top, item.Top);
			cohorts[index].Chem = item.Chem;
			cohorts[index].Weight += item.Weight;
			cohorts[index].N += item.N;
			cohorts[index].AshAlk += item.AshAlk;
			seedPartTotal[index] += item.Weight;	// weight of each cohort sent to animal
			index = -1;
		}
	}

	// check that there is seed mass able to be used
	// if there isn't then don't send back any cohorts
	bool seedAvailable = false;
	int i = 0; 
	while ((i < 4) && !seedAvailable)
	{
		if (cohorts[i].Weight > 0)
			seedAvailable = true;
		i++;
	}

	if (seedAvailable)
	{
		// store the cohorts to return to the animal
		for (int i = 0; i < 4; i++)
		{
			avail.element.push_back(cohorts[i]);
		}

		// now store the mass proportions for each head part so the removal is done correctly
		int ages[4] = { AGEGREEN, AGEGREEN, AGESENESCED, AGESENESCED };
		int digests[4] = { DDM, IDM, DDM, IDM };
		for (int i = 0; i < 4; i++)
		{
			for (int part = POD; part <= OIL; part++)
			{
				if (seedPartTotal[i] > 0)
					seedPoolFraction[part][ages[i]][digests[i]] = seedPoolWt[part][ages[i]][digests[i]] / seedPartTotal[i];
				else
					seedPoolFraction[part][ages[i]][digests[i]] = 0.0;
			}
		}
	}
}

//=======================================================================================
// Remove head dry matter from the Pod, meal and oil parts on a proportional basis
// Expecting
//	head, green, ddm
//	head, green, idm
//	head, senesced, ddm
//	head, senesced, idm
void FruitCohort::set_RemovedByAnimal(const protocol::RemovedByAnimalType &dm)
{
	// cohorts for the pod and grain parts of this plant
	protocol::RemovedByAnimalType podpart;
	protocol::RemovedByAnimalType grainpart;

	int age, dig;
	std::string sdig;
	for (vector<protocol::RemovedByAnimalelementType>::const_iterator cohort = dm.element.begin(); cohort != dm.element.end(); cohort++)
	{
		if ((Str_i_Cmp(cohort->Organ, "head_ripe") == 0) || (Str_i_Cmp(cohort->Organ, "head_unripe") == 0))
		{
			protocol::RemovedByAnimalelementType pod;	//add a cohort pod
			protocol::RemovedByAnimalelementType oil;	//add meal and oil cohort
			protocol::RemovedByAnimalelementType meal;

			if (Str_i_Cmp(cohort->Chem, "ddm") == 0)
			{
				sdig = "ddm";
				dig = DDM;
			}
			else
			{
				sdig = "idm";
				dig = IDM;
			}

			if (Str_i_Cmp(cohort->AgeID, "green") == 0)
			{	
				sdig = "green";
				age = AGEGREEN;
			}
			else
			{
				sdig = "senesced";
				age = AGESENESCED;
			}

			pod.CohortID = cohort->CohortID;
			pod.Organ = "Pod";
			pod.Bottom = cohort->Bottom;
			pod.Top = cohort->Top;
			pod.Chem = sdig;

			oil.CohortID = cohort->CohortID;
			oil.Organ = "oil";
			oil.Bottom = cohort->Bottom;
			oil.Top = cohort->Top;
			oil.Chem = sdig;

			meal.CohortID = cohort->CohortID;
			meal.Organ = "meal";
			meal.Bottom = cohort->Bottom;
			meal.Top = cohort->Top;
			meal.Chem = sdig;

			pod.WeightRemoved = seedPoolFraction[POD][age][dig] * cohort->WeightRemoved;					//calc the green dm
			oil.WeightRemoved = seedPoolFraction[OIL][age][dig] * cohort->WeightRemoved;					//calc the green dm
			meal.WeightRemoved = seedPoolFraction[MEAL][age][dig] * cohort->WeightRemoved;					//calc the green dm

			podpart.element.push_back(pod);
			grainpart.element.push_back(oil);
			grainpart.element.push_back(meal);
		}
	}
	grainPart->set_RemovedByAnimal(grainpart);	// the grain part is meal and oil
	podPart->set_RemovedByAnimal(podpart);
}

