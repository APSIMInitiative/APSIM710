#include "StdPlant.h"

#include "PlantPart.h"

using namespace std;

plantPart::plantPart(ScienceAPI& api, plantInterface *p, const string &name)
//=======================================================================================
     : plantThing(api, name),
       // deltas
       GreenRemoved (api,"GreenRemoved",name),
       SenescedRemoved (api,"SenescedRemoved",name),

       // pools
       Green(*new Pool(*p, api, "Green", name)),
       Senesced(*new Pool(*p, api, "Senesced", name)),
       Growth(*new Delta(api, "Growth", name)),
       Senescing(*new Delta(api, "Senescing", name)),
       Detaching(*new Delta(api, "Detaching", name)),
       Retranslocation(*new Delta(api, "Retranslocation", name)),
                     
       // summary pools
       Total(*p, api, "Total", name),
       Grain(*p, api, "Grain", name),
       GrainTotal(*p, api, "GrainTotal", name),
       Vegetative(*p, api, "Vegetative", name),
       VegetativeTotal(*p, api, "VegetativeTotal", name)
     {
     plant = p;
     myName = name;

     Initialise(true);
     }

plantPart::plantPart(ScienceAPI& api, plantInterface *p, const string &name,
                     Pool& green, Pool& senesced, Delta& growth, Delta& senescing, Delta& detaching, Delta& retranslocation)
//=======================================================================================
     : plantThing(api, name),
       // deltas
       GreenRemoved (api, "GreenRemoved", name),
       SenescedRemoved (api, "SenescedRemoved", name),

       // pools
       Green(green),
       Senesced(senesced),
       Growth(growth), 
       Senescing(senescing),
       Detaching(detaching),
       Retranslocation(retranslocation),
                            
       // summary pools
       Total(*p, api, "Total", name),
       Grain(*p, api, "Grain", name),
       GrainTotal(*p, api, "GrainTotal", name),
       Vegetative(*p, api, "Vegetative", name),
       VegetativeTotal(*p, api, "VegetativeTotal", name)
   {
   plant = p;
   myName = name;
   Initialise(false);
   }

plantPart::~plantPart()
{
	delete &Green;
	delete &Senesced;
	delete &Growth;
	delete &Senescing;
	delete &Detaching;
	delete &Retranslocation;
}

void plantPart::Initialise(bool isVegetative)
   {
   // setup summary pools
   if (isVegetative)
      {
      Vegetative.AddPool(Green);
      VegetativeTotal.AddPool(Green);
      VegetativeTotal.AddPool(Senesced);
      }

   Total.AddPool(Green);
   Total.AddPool(Senesced);

   zeroAllGlobals();
   }


string plantPart::addPartToVar(const string& variableName)
   {
   // --------------------------------------------------------------------------
   // add the part name, if it isn't blank, to the specified variable name.
   // --------------------------------------------------------------------------
   string LcaseName = myName;
   To_lower(LcaseName);
   if (myName != "")
      return variableName + "_" + LcaseName;
   else
      return variableName;
   }

string plantPart::addPartToDesc(const string& description)
   {
   // --------------------------------------------------------------------------
   // add the part name, if it isn't blank, to the specified description
   // --------------------------------------------------------------------------
   if (myName != "")
      return description + myName;
   else
      return description + " plant";
   }

void plantPart::onInit1(protocol::Component*)
//=======================================================================================
   {

   scienceAPI.exposeFunction(addPartToVar("dlt_n_senesced_trans"), "g/m^2", addPartToDesc("N translocated to/from senesced "), FloatGetter(&plantPart::dltNSenescedTrans));
   scienceAPI.exposeFunction(addPartToVar("dlt_n_senesced_retrans"), "g/m^2", addPartToDesc("N retranslocated to/from senesced "), FloatGetter(&plantPart::dltNSenescedRetrans));
   scienceAPI.exposeFunction(addPartToVar("n_demand"), "g/m^2", addPartToDesc("N demand of "), FloatGetter(&plantPart::nDemand));

   if (tempFlagToShortCircuitInit1) return;


   scienceAPI.exposeFunction(addPartToVar("n_conc_crit"), "%", addPartToDesc("Critical N content in "), FloatGetter(&plantPart::nConcCrit));
   scienceAPI.exposeFunction(addPartToVar("n_conc_min"), "%", addPartToDesc("Minimum N content in "), FloatGetter(&plantPart::nConcMin));

   scienceAPI.exposeFunction(addPartToVar("dm_demand"), "g/m^2", addPartToDesc("DM demand of "), FloatGetter(&plantPart::dmGreenDemand));
   }

void plantPart::get_dm_green_demand(protocol::Component *system, protocol::QueryValueData &qd)
//=======================================================================================
   {
   system->sendVariable(qd, dmGreenDemand());
   }



void plantPart::zeroAllGlobals(void)
//=======================================================================================
   {
   Green.Clear();
   Senesced.Clear();
   g.n_conc_crit=0.0;
   g.n_conc_min=0.0;
   tempFlagToShortCircuitInit1 = false;

   zeroDeltas();
   }

void plantPart::zeroDeltas(void)
//=======================================================================================
   {
   Growth.Clear();
   Senescing.Clear();
   Detaching.Clear();
   Retranslocation.Clear();
   }

void plantPart::prepare(void)
//=======================================================================================
   {
   zeroDeltas();
   }

void plantPart::doNFixRetranslocate(float NFix, float NDemandDifferentialTotal)
//=======================================================================================
   {
   Growth.AddN(NFix * divide (nDemandDifferential(), NDemandDifferentialTotal, 0.0));
   }

void plantPart::doNPartition(float nSupply, float n_demand_sum, float n_capacity_sum)
   //============================================================================
{
   float n_excess = nSupply - n_demand_sum;
   n_excess = l_bound (n_excess, 0.0);

   if (n_excess>0.0)
      {
      float plant_part_fract = divide (nCapacity(), n_capacity_sum, 0.0);
      Growth.SetN(nDemand() + n_excess * plant_part_fract);
      }
   else
      {
      float plant_part_fract = divide (nDemand(), n_demand_sum, 0.0);
      Growth.SetN(nSupply * plant_part_fract);
      }
}


float plantPart::dlt_dm_green_retrans_hack(float delta)
   {
   Retranslocation.SetNonStructuralDM(delta);
   return delta;
   }

void plantPart::doInit1(protocol::Component *system){this->onInit1(system) ;}

float plantPart::nMin(void)  {return g.n_conc_min * Green.DM();}
float plantPart::nCrit(void)  {return g.n_conc_crit * Green.DM();}
void plantPart::checkBounds(void) {}
const string &plantPart::name(void) {return myName ;}

