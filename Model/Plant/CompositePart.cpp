#include "StdPlant.h"

#include "CompositePart.h"
#include "CompositePool.h"
#include "CompositeDelta.h"
#include "ThingFactory.h"
#include "Environment.h"
#include <General/stl_functions.h>
#include <General/string_functions.h>
#include "Parts.h"
using namespace std;

//  initialise data members.
CompositePart::CompositePart(XMLNode parameters, ScienceAPI& scienceAPI, plantInterface *p, const string &name, bool doCreateParts)
   : plantPart(scienceAPI, p, name,
               *new CompositePool(*p, scienceAPI, "Green", name),
               *new CompositePool(*p, scienceAPI, "Senesced", name),
               *new CompositeDelta (scienceAPI, "Growth", name),
               *new CompositeDelta (scienceAPI, "Senescing", name),
               *new CompositeDelta (scienceAPI, "Detaching", name),
               *new CompositeDelta (scienceAPI, "Retranslocation", name))
   {
   deleteChildren = true;
   if (doCreateParts)
      createParts(parameters);
   }

// destructor
CompositePart::~CompositePart()
   {
   if (deleteChildren)
      {
      for (vector<plantThing *>::iterator t = things.begin();
           t != things.end();
           t++)
         delete (*t);
      }
   }

ostream &operator<<(ostream &output, const CompositePart /*&pool*/)
{
   //   output << "CompositePart:" << endl;
   output << endl;
   return output;
}


// Assigment operator
//  assign data members of object
const CompositePart &CompositePart::operator=(const CompositePart &/*other*/)
   //===========================================================================
{
   throw std::invalid_argument("Assignment operator NI for CompositePart");
}

string CompositePart::addPartToVar(const string& variableName)
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

string CompositePart::addPartToDesc(const string& description)
   {
   // --------------------------------------------------------------------------
   // add the part name, if it isn't blank, to the specified description
   // --------------------------------------------------------------------------
   if (myName != "")
      return description + myName;
   else
      return description + " plant";
   }
void CompositePart::onInit1(protocol::Component *system)
   //===========================================================================
   {
   plantPart::onInit1(system);

   if (myName == "")  // If you don't have this then we get a TopsSWDemand - not needed.
      {
      scienceAPI.exposeFunction("sw_demand", "mm",  "Demand for soil water", FloatGetter(&CompositePart::SWDemand));
      scienceAPI.exposeFunction("sw_demand_te", "mm",  "TE Demand for soil water", FloatGetter(&CompositePart::SWDemandTE));
      scienceAPI.exposeFunction("dlt_dm_pot_rue", "g/m^2",  "Potential above_ground dry matter production via photosynthesis", FloatGetter(&CompositePart::dltDmPotRue));
      scienceAPI.exposeFunction("cover_green", "",  "Green cover", FloatGetter(&CompositePart::coverGreen));
      }

   for (unsigned i = 0; i != things.size(); i++)
      things[i]->onInit1(system);
   }


plantThing* CompositePart::get(const std::string& name)
   {
   vector<plantThing*>::iterator i = find_if(things.begin(), things.end(),
                                             PEqualToName<plantThing>(name));
   if (i == things.end())
      throw runtime_error("Cannot find a child called: " + name);
   return *i;
   }

plantThing* CompositePart::getOptional(const std::string& name)
   {
   vector<plantThing*>::iterator i = find_if(things.begin(), things.end(),
                                             PEqualToName<plantThing>(name));
   if (i == things.end())
      return NULL;
   return *i;
   }

plantPart* CompositePart::getPart(const std::string& name)
   {
   vector<plantPart*>::iterator i = find_if(myParts.begin(), myParts.end(),
                                             PEqualToName<plantPart>(name));
   if (i == myParts.end())
      throw runtime_error("Cannot find a part called: " + name);
   return *i;
   }

std::vector<plantPart*> CompositePart::getParts(const std::vector<std::string>& names)
   {
   vector<plantPart*> parts;
   for (unsigned i = 0; i != names.size(); i++)
      parts.push_back(getPart(names[i]));
   return parts;
   }

void CompositePart::createParts(XMLNode parameters)
   {
   //---------------------------------------------------------------------------
   // Create all child parts by looking at the parameters XML passed in
   //---------------------------------------------------------------------------
   for (XMLNode::iterator child = parameters.begin();
                          child != parameters.end();
                          child++)
      {
      string childType = child->getName();
      string childName = child->getAttribute("name");
      if (childName == "")
         childName = childType;

      plantThing* thing = createThing(*child, scienceAPI, *plant, childType, childName);
      plantPart* part = dynamic_cast<plantPart*> (thing);
      if (dynamic_cast<Parts*> (thing) == NULL && part != NULL)
         add(part);
      else
         things.push_back(thing);
      }
   }
void CompositePart::add(plantPart* part)
   //===========================================================================
   {
   myParts.push_back(part);
   things.push_back(part);

   // Add this part's pools to green and senesced composite pools
   CompositePool& GreenPool = (CompositePool&) Green;
   CompositePool& SenescedPool = (CompositePool&) Senesced;
   CompositeDelta& GrowthDelta = (CompositeDelta&) Growth;
   CompositeDelta& SenescingDelta = (CompositeDelta&) Senescing;
   CompositeDelta& DetachingDelta = (CompositeDelta&) Detaching;
   CompositeDelta& RetranslocationDelta = (CompositeDelta&) Retranslocation;
   
   CompositePool& GrainPool = (CompositePool&) Grain;
   CompositePool& GrainTotalPool = (CompositePool&) GrainTotal;
   GreenPool.AddPool(part->Green);
   SenescedPool.AddPool(part->Senesced);
   GrowthDelta.Add(part->Growth);
   SenescingDelta.Add(part->Senescing);
   DetachingDelta.Add(part->Detaching);
   RetranslocationDelta.Add(part->Retranslocation);

   Vegetative.AddPool(part->Vegetative);
   VegetativeTotal.AddPool(part->VegetativeTotal);

   GrainPool.AddPool(part->Grain);
   GrainTotalPool.AddPool(part->GrainTotal);
   }
void CompositePart::addThing(plantThing* thing)
   {
   things.push_back(thing);
   }

float CompositePart::nConcCrit()
//=======================================================================================
   {
   float sum = 0.0;
   float count = 0.0;
   for (vector <plantPart * >::const_iterator part = myParts.begin(); part != myParts.end(); part++)
      sum += (*part)->nConcCrit();
      count++;
   return sum/count;
   }

float CompositePart::nConcMin()
//=======================================================================================
   {
   float sum = 0.0;
   float count = 0.0;
   for (vector <plantPart * >::const_iterator part = myParts.begin(); part != myParts.end(); part++)
      sum += (*part)->nConcMin();
      count++;
   return sum/count;
   }



float CompositePart::n_conc_crit(void)
   //===========================================================================
{
   float sum = 0.0;
   for (vector <plantPart * >::const_iterator part = myParts.begin(); part != myParts.end(); part++)
      sum += (*part)->n_conc_crit();
   return divide (sum , myParts.size() , 0.0);           //unweighted mean
}


float CompositePart::n_conc_min(void)
   //===========================================================================
{
   float sum = 0.0;
   for (vector <plantPart * >::const_iterator part = myParts.begin(); part != myParts.end(); part++)
      sum += (*part)->n_conc_min();
   return divide (sum , myParts.size() , 0.0);           //unweighted mean
}



float CompositePart::dltNSenescedRetrans(void)
   //===========================================================================
{
   float sum = 0.0;
   for (vector <plantPart * >::const_iterator part = myParts.begin(); part != myParts.end(); part++)
      sum += (*part)->dltNSenescedRetrans();
   return sum;
}


float CompositePart::dltNSenescedTrans(void)
   //===========================================================================
{
   float sum = 0.0;
   for (vector <plantPart * >::const_iterator part = myParts.begin(); part != myParts.end(); part++)
      sum += (*part)->dltNSenescedTrans();
   return sum;
}


float CompositePart::dmGreenDemand(void)
   //===========================================================================
{
   float dmGreenDemand = 0.0;
   for (vector <plantPart * >::const_iterator part = myParts.begin(); part != myParts.end(); part++)
      dmGreenDemand += (*part)->dmGreenDemand();
   return dmGreenDemand;
}

float CompositePart::grainWaterContent(void)
   //===========================================================================
{
   float total = 0.0;
   float count = 0.0;
   vector<plantPart *>::const_iterator part;
   for (part = myParts.begin(); part != myParts.end(); part++)
   {
      total += (*part)->grainWaterContent();
      count +=1.0;
   }
//   return divide (total, count, 0.0);
   return total;                       //FIXME
}

float CompositePart::grainWt(void)
   //===========================================================================
{
   float grainWtTotal = 0.0;
   for (vector <plantPart * >::const_iterator part = myParts.begin(); part != myParts.end(); part++)
      grainWtTotal += (*part)->grainWt();
   return grainWtTotal;
}

float CompositePart::dltDmPotentialGrain()
   //===========================================================================
{
   float dlt = 0.0;
   for (vector <plantPart * >::const_iterator part = myParts.begin(); part != myParts.end(); part++)
      dlt += (*part)->dltDmPotentialGrain();
   return dlt;
}

float CompositePart::dmGrainWetTotal(void)
   //===========================================================================
{
   float dmTotal = 0.0;
   for (vector <plantPart * >::const_iterator part = myParts.begin(); part != myParts.end(); part++)
      dmTotal += (*part)->dmGrainWetTotal();
   return dmTotal;
}

float CompositePart::nMax(void)
   //===========================================================================
{
   float result = 0.0;
   for (vector <plantPart * >::const_iterator part = myParts.begin(); part != myParts.end(); part++)
      result += (*part)->nMax();                                      //FIXME Is this a conc?
   return result;
}

float CompositePart::nDemandGrain2(void)
   //===========================================================================
{
   float n_demand = 0.0;
   vector <plantPart *>::iterator part;
   for (part = myParts.begin(); part != myParts.end(); part++)
      n_demand += (*part)->nDemandGrain2();
   return n_demand;
}

float CompositePart::soilNDemand(void)
   //============================================================================
{
   float SoilNDemand = 0.0;
   for (vector <plantPart * >::iterator part = myParts.begin(); part != myParts.end(); part++)
      SoilNDemand += (*part)->soilNDemand();
   return SoilNDemand;
}

float CompositePart::nDemand(void)
   //============================================================================
{
   float n_demand = 0.0;
   for (vector <plantPart * >::const_iterator part = myParts.begin(); part != myParts.end(); part++)
      n_demand += (*part)->nDemand();
   return n_demand;
}

float CompositePart::nCapacity(void)
   //============================================================================
{
   float NCapacity = 0.0;
   vector <plantPart *>::iterator part;
   for (part = myParts.begin(); part != myParts.end(); part++)
      NCapacity += (*part)->nCapacity();
   return NCapacity;
}

void CompositePart::doNPartition(float nSupply, float n_demand_sum, float n_capacity_sum)
   //============================================================================
{
   //plantPart::doNPartition(nSupply, n_demand_sum, n_capacity_sum);   //FIXME need to remove this sometime
   float GrowthN;
   float n_excess = nSupply - n_demand_sum;
   n_excess = l_bound (n_excess, 0.0);

   if (n_excess>0.0)
      {
      float plant_part_fract = divide (nCapacity(), n_capacity_sum, 0.0);
      GrowthN = (nDemand() + n_excess * plant_part_fract);
      }
   else
      {
      float plant_part_fract = divide (nDemand(), n_demand_sum, 0.0);
      GrowthN=(nSupply * plant_part_fract);
      }

   n_demand_sum = nDemand();
   n_capacity_sum = nCapacity();

   vector <plantPart *>::iterator part;
   for (part = myParts.begin(); part != myParts.end(); part++)
      (*part)->doNPartition(GrowthN, n_demand_sum, n_capacity_sum);

   float dlt_n_green_sum = Growth.N();
   if (!reals_are_equal(dlt_n_green_sum - Growth.N(), 0.0))
      {
      string msg = myName + " dlt_n_green mass balance is off: dlt_n_green_sum ="
                  + ftoa(dlt_n_green_sum, ".6")
                  + " vs nSupply ="
                  + ftoa(Growth.N(), ".6");
      scienceAPI.warning(msg);
      }
}


float CompositePart::pMaxPot(void)
   //===========================================================================
{
   float pMaxPot = 0.0;
   for (vector <plantPart * >::const_iterator part = myParts.begin(); part != myParts.end(); part++)
      pMaxPot += (*part)->pMaxPot();                                      //FIXME Is this a conc?
   return pMaxPot;
}

float CompositePart::pMinPot(void)
   //===========================================================================
{
   float pMinPot = 0.0;
   for (vector <plantPart * >::const_iterator part = myParts.begin(); part != myParts.end(); part++)
      pMinPot += (*part)->pMinPot();
   return pMinPot;
}

void CompositePart::get_p_demand(vector<float> &p_demand)
   //===========================================================================
{
   vector <plantPart *>::iterator part;
   for (part = myParts.begin(); part != myParts.end(); part++)
      (*part)->get_p_demand(p_demand);
}

void CompositePart::get_name(vector<string> &name)
   //===========================================================================
{
   vector <plantPart *>::iterator part;
   for (part = myParts.begin(); part != myParts.end(); part++)
      (*part)->get_name(name);
}

void CompositePart::get_dlt_p_retrans(vector<float> &dlt_p_retrans)
   //===========================================================================
{
   vector <plantPart *>::iterator part;
   for (part = myParts.begin(); part != myParts.end(); part++)
      (*part)->get_dlt_p_retrans(dlt_p_retrans);
}

void CompositePart::get_dm_plant_min(vector<float> &dm_min)
   //===========================================================================
{
   vector <plantPart *>::iterator part;
   for (part = myParts.begin(); part != myParts.end(); part++)
      (*part)->get_dm_plant_min(dm_min);
}

void CompositePart::get_dm_green(vector<float> &dm_green)
   //===========================================================================
{
   vector <plantPart *>::iterator part;
   for (part = myParts.begin(); part != myParts.end(); part++)
      (*part)->get_dm_green(dm_green);
}

void CompositePart::get_dm_senesced(vector<float> &dm_senesced)
   //===========================================================================
{
   vector <plantPart *>::iterator part;
   for (part = myParts.begin(); part != myParts.end(); part++)
      (*part)->get_dm_senesced(dm_senesced);
}

void CompositePart::get_dlt_dm_green(vector<float> &dlt_dm_green)
   //===========================================================================
{
   vector <plantPart *>::iterator part;
   for (part = myParts.begin(); part != myParts.end(); part++)
      (*part)->get_dlt_dm_green(dlt_dm_green);
}

void CompositePart::get_dlt_dm_green_retrans(vector<float> &dlt_dm_green_retrans)
   //===========================================================================
{
   vector <plantPart *>::iterator part;
   for (part = myParts.begin(); part != myParts.end(); part++)
      (*part)->get_dlt_dm_green_retrans(dlt_dm_green_retrans);
}



void CompositePart::get_n_demanded(vector<float> &n_demand)
   //===========================================================================
{
   vector <plantPart *>::iterator part;
   for (part = myParts.begin(); part != myParts.end(); part++)
      (*part)->get_n_demanded(n_demand);
}

void CompositePart::doGrainNumber (void)
   //===========================================================================
   //       Calculate Grain Numer
{
   vector <plantPart *>::iterator part;
   for (part = myParts.begin(); part != myParts.end(); part++)
      (*part)->doGrainNumber();
}

void CompositePart::readCultivarParameters (protocol::Component *system, const string &cultivar)
   //===========================================================================
   {
   for (unsigned i = 0; i != things.size(); i++)
      things[i]->readCultivarParameters(system, cultivar);
   }

void CompositePart::writeCultivarInfo (protocol::Component *system)
   //===========================================================================
{
   // report
   vector <plantPart *>::iterator part;
   for (part = myParts.begin(); part != myParts.end(); part++)
      (*part)->writeCultivarInfo(system);
}

void CompositePart::morphology(void)
   //===========================================================================
{
   vector <plantPart *>::iterator part;
   for (part =  myParts.begin(); part != myParts.end(); part++)
      (*part)->morphology();
}

void CompositePart::zeroAllGlobals(void)
   //===========================================================================
   {
   for (unsigned i = 0; i != things.size(); i++)
      things[i]->zeroAllGlobals();
   }

void CompositePart::zeroDeltas(void)
   //===========================================================================
   {
   for (unsigned i = 0; i != things.size(); i++)
      things[i]->zeroDeltas();
   }

void CompositePart::zeroDltDmGreen(void)
   //===========================================================================
{
   vector <plantPart *>::iterator part;
   for (part =  myParts.begin(); part != myParts.end(); part++)
      (*part)->zeroDltDmGreen();
}

void CompositePart::zeroDltNSenescedTrans(void)
   //===========================================================================
{
   vector <plantPart *>::iterator part;
   for (part =  myParts.begin(); part != myParts.end(); part++)
      (*part)->zeroDltNSenescedTrans();
}


void CompositePart::doRemoveBiomass(protocol::RemoveCropDmType dmRemoved, bool c_remove_biomass_report)
//=======================================================================================
{
   vector <plantPart *>::iterator part;
   for (part =  myParts.begin(); part != myParts.end(); part++)
      (*part)->doRemoveBiomass(dmRemoved, c_remove_biomass_report);
}

void CompositePart::removeBiomass(void)
//=======================================================================================
{
   vector <plantPart *>::iterator part;
   for (part =  myParts.begin(); part != myParts.end(); part++)
      (*part)->removeBiomass();
}

void CompositePart::removeBiomass2(float chop_fr)
//=======================================================================================
{
   vector <plantPart *>::iterator part;
   for (part =  myParts.begin(); part != myParts.end(); part++)
      (*part)->removeBiomass2(chop_fr);
}

void CompositePart::onHarvest(float cutting_height, float remove_fr,
                           vector<string> &dm_type,
                           vector<float> &dlt_crop_dm,
                           vector<float> &dlt_dm_n,
                           vector<float> &dlt_dm_p,
                           vector<float> &fraction_to_residue)
   // ====================================================================
{
   vector <plantPart *>::iterator part;
   for (part =  myParts.begin(); part != myParts.end(); part++)
      (*part)->onHarvest(cutting_height, remove_fr,
                         dm_type,
                         dlt_crop_dm,
                         dlt_dm_n,
                         dlt_dm_p,
                         fraction_to_residue);
}

void CompositePart::onEmergence()
//=======================================================================================
   {
   vector <plantPart *>::iterator part;
   for (part =  myParts.begin(); part != myParts.end(); part++)
      (*part)->onEmergence();
   }

void CompositePart::onPlantEvent(const string &newPhaseName)
//=======================================================================================
   {
   for (unsigned i = 0; i!= things.size(); i++)
      things[i]->onPlantEvent(newPhaseName);
   }

void CompositePart::onKillStem(void)
   // ====================================================================
{
   vector <plantPart *>::iterator part;
   for (part =  myParts.begin(); part != myParts.end(); part++)
      (*part)->onKillStem();
}

void CompositePart::onEndCrop(vector<string> &dm_type,
                           vector<float> &dlt_crop_dm,
                           vector<float> &dlt_dm_n,
                           vector<float> &dlt_dm_p,
                           vector<float> &fraction_to_residue)
   // ====================================================================
{
   vector <plantPart *>::iterator part;
   for (part =  myParts.begin(); part != myParts.end(); part++)
      (*part)->onEndCrop(dm_type,
                         dlt_crop_dm,
                         dlt_dm_n,
                         dlt_dm_p,
                         fraction_to_residue);
}


void CompositePart::readConstants(protocol::Component *system, const string &section)
   //===========================================================================
   {
   for (unsigned i = 0; i != things.size(); i++)
      things[i]->readConstants(system, section);
   }

void CompositePart::readSpeciesParameters(protocol::Component *system, vector<string> &sections)
   //===========================================================================
   {
   for (unsigned i = 0; i != things.size(); i++)
      things[i]->readSpeciesParameters(system, sections);
   }


float CompositePart::dltDmRetranslocateSupply(float /* demand_differential*/)
   //===========================================================================
{
   float dlt_dm_green_retrans = 0.0;
   for (vector<plantPart *>::iterator part = myParts.begin(); part != myParts.end(); part++)
      {
      dlt_dm_green_retrans += (*part)->Retranslocation.DM();
      }
   return dlt_dm_green_retrans;
}

void CompositePart::doNSenescedRetrans(float navail, float n_demand_tot)
   //===========================================================================
{
   vector <plantPart *>::iterator part;
   for (part = myParts.begin(); part != myParts.end(); part++)
      (*part)->doNSenescedRetrans(navail, n_demand_tot);
}

void CompositePart::collectDetachedForResidue(vector<string> &part_name
                                           , vector<float> &dm_residue
                                           , vector<float> &dm_n
                                           , vector<float> &dm_p
                                           , vector<float> &fraction_to_residue)
   //===========================================================================
{
   vector <plantPart *>::iterator part;
   for (part = myParts.begin(); part != myParts.end(); part++)
      (*part)->collectDetachedForResidue(part_name
                                         , dm_residue
                                         , dm_n
                                         , dm_p
                                         , fraction_to_residue);
}


void CompositePart::update(void)
   //===========================================================================
{
   // Update
   vector <plantPart *>::iterator part;
   for (part = myParts.begin(); part != myParts.end(); part++)
      (*part)->update();
}

void CompositePart::doNConccentrationLimits(float modifier)
   //===========================================================================
{
   vector <plantPart *>::iterator part;
   for (part = myParts.begin(); part != myParts.end(); part++)
      (*part)->doNConccentrationLimits(modifier);
}

// Query
float CompositePart::coverTotal(void)
   //===========================================================================
{
   float cover = 0.0;
   vector <plantPart *>::iterator part;
   for (part = myParts.begin(); part != myParts.end(); part++)
      cover = add_covers (cover, (*part)->coverTotal());
   return cover;
}

float CompositePart::coverGreen(void)
   //===========================================================================
{
   float cover = 0.0;
   vector <plantPart *>::iterator part;
   for (part = myParts.begin(); part != myParts.end(); part++)
      cover = add_covers (cover, (*part)->coverGreen());
   return cover;
}

float CompositePart::coverSen(void)
   //===========================================================================
{
   float cover = 0.0;
   vector <plantPart *>::iterator part;
   for (part = myParts.begin(); part != myParts.end(); part++)
      cover = add_covers (cover, (*part)->coverSen());
   return cover;
}

//float CompositePart::total()
//{
//
//  return green.shell + green.meal + senesced.shell + senesced.meal + dead.shell + dead.meal;
//}

void CompositePart::display(ostream &os)
{
   //   os << "CompositePart:" << endl;
   //   os << "Green cover:    " << coverPod.green << endl;
   //   os << "Senesced cover: " << coverPod.sen << endl;
   //   os << "Dead cover:     " << coverPod.dead << endl;
   //   os << "Green shell: " << green.shell << endl;
   //   os << "Green meal: " << green.meal << endl;
   //   os << "Senesced shell: " << senesced.shell << endl;
   //   os << "Senesced meal: " << senesced.meal << endl;
   //   os << "Dead shell: " << dead.shell << endl;
   //   os << "Dead meal: " << dead.meal << endl << endl;
   os << endl;
}


void CompositePart::doCover(PlantSpatial &spatial)
   //===========================================================================
{
   vector <plantPart *>::iterator part;
   for (part = myParts.begin(); part != myParts.end(); part++)
      (*part)->doCover(spatial);
}
void CompositePart::prepare (void)
   //===========================================================================
   {
   vector <plantPart *>::iterator part;
   for (part =  myParts.begin(); part != myParts.end(); part++)
      (*part)->prepare ();
   }

void CompositePart::process(void)
   //===========================================================================
{
   vector <plantPart *>::iterator part;
   for (part =  myParts.begin(); part != myParts.end(); part++)
      (*part)->process();
}

void CompositePart::doProcessBioDemand(void)
   //===========================================================================
{
   vector <plantPart *>::iterator part;
   for (part =  myParts.begin(); part != myParts.end(); part++)
      (*part)->doProcessBioDemand();
}

float CompositePart::grainNo(void)
   //===========================================================================
{
   float grainNo = 0.0;
   for (vector <plantPart * >::const_iterator part = myParts.begin(); part != myParts.end(); part++)
      grainNo += (*part)->grainNo();
   return grainNo;
}

float CompositePart::nDemandGrain(void)
   //===========================================================================
{
   float nDemandGrain = 0.0;
   for (vector <plantPart * >::const_iterator part = myParts.begin(); part != myParts.end(); part++)
      nDemandGrain += (*part)->nDemandGrain();
   return nDemandGrain;
}

float CompositePart::transpirationEfficiency(void)
   //===========================================================================
{
   float transpEff = 0.0;
   vector <plantPart *>::const_iterator part;
   for (part =  myParts.begin(); part != myParts.end(); part++)
      transpEff += (*part)->transpirationEfficiency();   //FIXME - the sum is not the correct result
   return transpEff;
}

float CompositePart::dltDmPotRue(void)
   //===========================================================================
{
   float dltDmPotRue = 0.0;
   vector <plantPart *>::const_iterator part;
   for (part =  myParts.begin(); part != myParts.end(); part++)
      dltDmPotRue += (*part)->dltDmPotRue();
   return dltDmPotRue;
}
float CompositePart::Respiration(void)
   //===========================================================================
{
   float Respiration = 0.0;
   vector <plantPart *>::const_iterator part;
   for (part =  myParts.begin(); part != myParts.end(); part++)
      Respiration += (*part)->Respiration();
   return Respiration;
}

float CompositePart::DMSupply(void)
   //===========================================================================
{
   float supply = 0.0;
   vector <plantPart *>::const_iterator part;
   for (part =  myParts.begin(); part != myParts.end(); part++)
      supply += (*part)->DMSupply();
   return supply;
}

float CompositePart::dltDmGrainDemand(void)
   //===========================================================================
{
   float dltDmDemand = 0.0;
   vector <plantPart *>::const_iterator part;
   for (part =  myParts.begin(); part != myParts.end(); part++)
      dltDmDemand += (*part)->dltDmGrainDemand();
   return dltDmDemand;
}

void CompositePart::calcDlt_pod_area (void)
   //===========================================================================
{
   vector <plantPart *>::iterator part;
   for (part =  myParts.begin(); part != myParts.end(); part++)
      (*part)->calcDlt_pod_area();
}


float CompositePart::dltDmGreenRetransUptake(void)
   //===========================================================================
{
   float dltDmUptake = 0.0;
   vector <plantPart *>::const_iterator part;
   for (part =  myParts.begin(); part != myParts.end(); part++)
      dltDmUptake += (*part)->dltDmGreenRetransUptake();
   return dltDmUptake;
}

plantPart& CompositePart::find(const std::string& name)
   {
   //===========================================================================
   // Find a child part. Will throw if not found.
   //===========================================================================
   return dynamic_cast<plantPart&>(findThing(name));
   }
plantThing& CompositePart::findThing(const std::string& name)
   {
   //===========================================================================
   // Find a child thing. Will throw if not found.
   //===========================================================================
   string nameToFind = name;
   string remainder = "";
   unsigned PosDelimiter = name.find('.');
   if (PosDelimiter != string::npos)
      {
      nameToFind = name.substr(0, PosDelimiter);
      remainder = name.substr(PosDelimiter+1);
      }
   for (unsigned p = 0; p != things.size(); p++)
      {
      if (Str_i_Eq(things[p]->getName(), nameToFind))
         {
         if (remainder == "")
            return *things[p];
         else
            {
            CompositePart* child = dynamic_cast<CompositePart*>(things[p]);
            if (child != NULL)
               return child->findThing(remainder);
            }
         }
      }
   throw runtime_error("Cannot find thing: " + name);
   }
//===========================================================================
void CompositePart::doRadnPartition()
//===========================================================================
   {
   string canopyName = string("fr_intc_radn_") + string(plant->Name());
   float fractIncidentRadn = 0.0;
   scienceAPI.getOptional(canopyName, "", fractIncidentRadn, 0.0, 1.0);

   // back calculate transmitted solar radiation to canopy
   if (fractIncidentRadn <= 0.0)
      fractIncidentRadn = 1.0;
   else
      fractIncidentRadn = divide (fractIncidentRadn, coverGreen(), 0.0);

   float incomingSolarRadiation = plant->environment().radn() * fractIncidentRadn;

   string orderString;
   scienceAPI.read("RadiationPartitioningOrder", orderString);
   vector<string> partOrder;
   split(orderString, " ", partOrder);

   for (unsigned o = 0; o != partOrder.size(); o++)
      {
      plantPart& part = find(partOrder[o]);

      part.interceptRadiationGreen (incomingSolarRadiation);

      // calc the total interception from this part - what is left is transmitted
      // to the other parts.
      incomingSolarRadiation -= part.calcInterceptRadiationTotal(incomingSolarRadiation);
      }
   }

void CompositePart::interceptRadiationGreen (float radiation)
   //===========================================================================
{
   vector <plantPart *>::iterator part;
   for (part =  myParts.begin(); part != myParts.end(); part++)
      (*part)->interceptRadiationGreen (radiation);         //FIXME - divey up radiation
}

float CompositePart::calcInterceptRadiationTotal (float radiation)
   //===========================================================================
{
   float interceptRadiation = 0.0;
   vector <plantPart *>::iterator part;
   for (part =  myParts.begin(); part != myParts.end(); part++)
      interceptRadiation += (*part)->calcInterceptRadiationTotal (radiation);         //FIXME - divey up radiation
   return interceptRadiation;
}

void CompositePart::doDmPotRUE (void )
   //===========================================================================
{
   vector <plantPart *>::iterator part;
   for (part =  myParts.begin(); part != myParts.end(); part++)
      (*part)->doDmPotRUE ();
}

void CompositePart::doSWDemand(float SWDemandMaxFactor)
   //===========================================================================
{
   vector <plantPart *>::iterator part;
   for (part =  myParts.begin(); part != myParts.end(); part++)
      (*part)->doSWDemand(SWDemandMaxFactor);
}

float CompositePart::SWDemand(void)
   //===========================================================================
{
   float SWDemand = 0.0;
   vector <plantPart *>::iterator part;
   for (part =  myParts.begin(); part != myParts.end(); part++)
      SWDemand += (*part)->SWDemand();
   return SWDemand;
}

float CompositePart::SWDemandTE(void)
   //===========================================================================
{
   float SWDemandTE = 0.0;
   vector <plantPart *>::iterator part;
   for (part =  myParts.begin(); part != myParts.end(); part++)
      SWDemandTE += (*part)->SWDemandTE();
   return SWDemandTE;
}

void CompositePart::doNDemandGrain(float g_nfact_grain_conc      //   (INPUT)
                                 , float g_swdef_expansion)    //   grain N demand (g/m^2)
   //===========================================================================
{
   //    Calculate plant n demand

   vector <plantPart *>::iterator part;
   for (part =  myParts.begin(); part != myParts.end(); part++)
      (*part)->doNDemandGrain(g_nfact_grain_conc
                                        , g_swdef_expansion);
   }

void CompositePart::doDmDemand ( float dlt_dm_veg_supply)
   //===========================================================================
{
   //       (OUTPUT) assimilate demand for reproductive part (g/m^2)
   // calculate demands of reproductive parts

   vector <plantPart *>::iterator part;
   for (part =  myParts.begin(); part != myParts.end(); part++)
      (*part)->doDmDemand(dlt_dm_veg_supply);                                //FIXME - divey up dlt_dm_veg_supply? Only for HI approach
}

float CompositePart::dmDemandDifferential(void)
   //===========================================================================
{
   float dm_demand_differential = 0.0;
   vector <plantPart *>::const_iterator part;
   for (part =  myParts.begin(); part != myParts.end(); part++)
      dm_demand_differential += (*part)->dmDemandDifferential();
   return dm_demand_differential;
}

float CompositePart::giveDmGreen(float dmSupplied)
//=======================================================================================
// Arbritator has given us some DM to distribute amongst individual parts
   {
   float dmDemand = dmGreenDemand();
   float uptake = 0.0;
   for (vector<plantPart *>::iterator part = myParts.begin(); part != myParts.end(); part++)
      {
      float partFrac =  divide((*part)->dmGreenDemand(), dmDemand, 0.0);
      uptake += (*part)->giveDmGreen (dmSupplied * partFrac);
      }

   // do mass balance check
   if (!reals_are_equal(uptake, dmSupplied, 1.0E-4))
       {
       string msg = myName + " giveDmGreen mass balance is off:\n"
                   + "uptake = " + ftoa(uptake, ".6")
                   + " vs "
                   + "supplied = " + ftoa(dmSupplied, ".6") +"\n";
       for (vector<plantPart *>::iterator part = myParts.begin();
            part != myParts.end();
            part++)
         msg += (*part)->name() + "=" + ftoa((*part)->Growth.DM(), ".6") +"\n";

       scienceAPI.warning(msg);
       }

   return uptake;
   }


void CompositePart::doDmRetranslocate(float DMAvail, float DMDemandDifferentialTotal)
//=======================================================================================
   {
   float dm_demand_differential = dmDemandDifferential ();
   float dlt_dm_green_retrans = DMAvail * divide (dm_demand_differential, DMDemandDifferentialTotal, 0.0);

   // get available carbohydrate from local supply pools
   float demand_differential = dm_demand_differential - dlt_dm_green_retrans;

   for (vector<plantPart *>::iterator part = myParts.begin(); part != myParts.end(); part++)      //FIXME later
      {
      float dlt_dm_retrans_part = (*part)->dltDmRetranslocateSupply(demand_differential);
      demand_differential = demand_differential - dlt_dm_retrans_part;
      }

   float dlt_dm_green_retrans_tot = dlt_dm_green_retrans + (-Retranslocation.DM());

   // now distribute the assimilate to plant parts
   for (vector<plantPart *>::iterator part = myParts.begin(); part != myParts.end(); part++)      //FIXME later
       {
       (*part)->doDmRetranslocate (dlt_dm_green_retrans_tot, dm_demand_differential);
       }
   // do mass balance check

   if (!reals_are_equal(dltDmGreenRetransUptake (), dlt_dm_green_retrans, 1.0E-4))  // XX this is probably too much slop - try doubles XX
      {
      string msg = myName + " dlt_dm_green_retrans_tot mass balance is off: "
                   + ftoa(dltDmGreenRetransUptake (), ".6")
                   + " vs "
                   + ftoa(dlt_dm_green_retrans, ".6");
      scienceAPI.warning(msg);
      }
   }

void CompositePart::doSenescence (float sen_fr)       // (OUTPUT) actual biomass senesced from plant parts (g/m^2)
   //============================================================================
{
   //   Derives seneseced plant dry matter (g/m^2) for the day

   vector <plantPart *>::iterator part;
   for (part =  myParts.begin(); part != myParts.end(); part++)
      (*part)->doSenescence(sen_fr);
}


float CompositePart::availableRetranslocateN(void)
   //============================================================================
{
   float nAvail = 0.0;
   vector <plantPart *>::iterator part;
   for (part =  myParts.begin(); part != myParts.end(); part++)     //FIXME later - parts need to know if they hold a supply pool
      nAvail += (*part)->availableRetranslocateN();

   return nAvail;
}

float CompositePart::nDemandDifferential(void)
   //===========================================================================
{
   float n_demand_differential = 0.0;
   vector <plantPart *>::iterator part;
   for (part =  myParts.begin(); part != myParts.end(); part++)
      n_demand_differential += (*part)->nDemandDifferential();
   return n_demand_differential;
}

void CompositePart::doNFixRetranslocate(float NFix, float NDemandDifferentialTotal)
//=======================================================================================
{
    //plantPart::doNFixRetranslocate(NFix, NDemandDifferentialTotal);

   float n_demand_differential = nDemandDifferential();

        // now distribute the n fixed to plant parts

   NFix = NFix * divide (n_demand_differential, NDemandDifferentialTotal, 0.0);
   vector <plantPart *>::iterator part;
   for (part =  myParts.begin(); part != myParts.end(); part++)      //FIXME later
       (*part)->doNFixRetranslocate (NFix, n_demand_differential);
}

void CompositePart::doNRetranslocate( float N_supply, float g_grain_n_demand)
   //============================================================================
   //     Calculate the nitrogen retranslocation from the various plant parts to the grain.
   {

   // available N does not include grain
   // this should not presume grain is 0.

   // get actual grain N uptake by retransolcation
   // limit retranslocation to total available N

   const float  tolerence = 0.001 ;
   for (vector <plantPart *>::iterator part = myParts.begin();
        part != myParts.end();
        part++)
      {
      (*part)->doNRetranslocate(N_supply, g_grain_n_demand);           //FIXME - divy up?
      bound_check_real_var (scienceAPI,fabs((*part)->dltNRetransOut())
                          , 0.0, (*part)->availableRetranslocateN() + tolerence
                          , (string("dlt_N_retrans(") + (*part)->name() + string(")")).c_str() );
      }
   }

void CompositePart::doNDemand1(float dlt_dm             // (INPUT)  Whole plant the daily biomass production (g/m^2)
                            , float dlt_dm_pot_rue)  // (INPUT)  Whole plant potential dry matter production (g/m^2)
   //============================================================================
   //     Return plant nitrogen demand for each plant component
{
   vector <plantPart *>::iterator part;
   for (part = myParts.begin(); part != myParts.end(); part++)
      (*part)->doNDemand1(dlt_dm, dlt_dm_pot_rue);                     //FIXME - divy up?
}

void CompositePart::doNDemand1Pot(float dlt_dm             // (INPUT)  Whole plant the daily biomass production (g/m^2)
                               , float dlt_dm_pot_rue)  // (INPUT)  Whole plant potential dry matter production (g/m^2)
   //============================================================================
   //     Return plant nitrogen demand for each plant component
{
   vector <plantPart *>::iterator part;
   for (part = myParts.begin(); part != myParts.end(); part++)
      (*part)->doNDemand1Pot(dlt_dm, dlt_dm_pot_rue);                  //FIXME - divy up?
}

void CompositePart::doNDemand2(float dlt_dm             // (INPUT)  Whole plant the daily biomass production (g/m^2)
                            , float dlt_dm_pot_rue)  // (INPUT)  Whole plant potential dry matter production (g/m^2)
   //============================================================================
   //     Return plant nitrogen demand for each plant component
{
   vector <plantPart *>::iterator part;
   for (part = myParts.begin(); part != myParts.end(); part++)
      (*part)->doNDemand2(dlt_dm, dlt_dm_pot_rue);                     //FIXME - divy up?
}


void CompositePart::doSoilNDemand(void)
   //============================================================================
{
   vector <plantPart *>::iterator part;
   for (part = myParts.begin(); part != myParts.end(); part++)
      (*part)->doSoilNDemand();
}


void CompositePart::doNSenescence(void)
   //============================================================================
{
   vector <plantPart *>::iterator part;
   for (part = myParts.begin(); part != myParts.end(); part++)
      (*part)->doNSenescence();
}

void CompositePart::Detachment(void)
   //============================================================================
{
   vector <plantPart *>::iterator part;
   for (part = myParts.begin(); part != myParts.end(); part++)
      {
      (*part)->Detachment();

      }
}


void CompositePart::doPDemand(void)     // (INPUT)  Whole plant potential dry matter production (g/m^2)
   //============================================================================
{
   vector <plantPart *>::iterator part;
   for (part = myParts.begin(); part != myParts.end(); part++)
      {
      (*part)->doPDemand();
      }
}

void CompositePart::doPSenescence(void)
   //============================================================================
{
   vector <plantPart *>::iterator part;
   for (part = myParts.begin(); part != myParts.end(); part++)
      (*part)->doPSenescence();
}

float CompositePart::pDemand(void)
   //============================================================================
{
   float p_demand = 0.0;
   vector <plantPart *>::iterator part;
   for (part = myParts.begin(); part != myParts.end(); part++)
      p_demand += (*part)->pDemand();
   return p_demand;
}

float CompositePart::pRetransSupply(void)
   //============================================================================
{
   float p_retrans_supply = 0.0;
   vector <plantPart *>::iterator part;
   for (part = myParts.begin(); part != myParts.end(); part++)
      p_retrans_supply += (*part)->pRetransSupply();
   return p_retrans_supply;
}

float CompositePart::pRetransDemand(void)
   //============================================================================
{
   float p_retrans_demand = 0.0;
   vector <plantPart *>::iterator part;
   for (part = myParts.begin(); part != myParts.end(); part++)
      p_retrans_demand += (*part)->pRetransDemand();
   return p_retrans_demand;
}

float CompositePart::dmRetransSupply(void)
   //============================================================================
{
   float dm_retrans_supply = 0.0;
   vector <plantPart *>::const_iterator part;
   for (part = myParts.begin(); part != myParts.end(); part++)
      dm_retrans_supply += (*part)->dmRetransSupply();
   return dm_retrans_supply;
}

float CompositePart::dmRetransDemand(void)
//============================================================================
{
   float dm_retrans_demand = 0.0;
   vector <plantPart *>::iterator part;
   for (part = myParts.begin(); part != myParts.end(); part++)
      dm_retrans_demand += (*part)->dmRetransDemand();
   return dm_retrans_demand;
}

float CompositePart::nRetransSupply(void)
   //============================================================================
{
   float n_retrans_supply = 0.0;
   vector <plantPart *>::iterator part;
   for (part = myParts.begin(); part != myParts.end(); part++)
      n_retrans_supply += (*part)->nRetransSupply();
   return n_retrans_supply;
}

float CompositePart::dltNRetransOut(void)
   //============================================================================
{
   float dlt_n_retrans = 0.0;
   vector <plantPart *>::iterator part;
   for (part = myParts.begin(); part != myParts.end(); part++)
      dlt_n_retrans += (*part)->dltNRetransOut();

   return dlt_n_retrans;
}

float CompositePart::nRetransDemand(void)
   //============================================================================
{
   float n_retrans_demand = 0.0;
   vector <plantPart *>::iterator part;
   for (part = myParts.begin(); part != myParts.end(); part++)
      n_retrans_demand += (*part)->nRetransDemand();
   return n_retrans_demand;
}

void CompositePart::doPPartition(float p_uptake, float total_p_demand)
   //============================================================================
{
   float myP = p_uptake * divide(pDemand(), total_p_demand,  0.0); // Amount of P for this composite part
   for (vector <plantPart *>::iterator part = myParts.begin(); part != myParts.end(); part++)
      {
      (*part)->doPPartition(myP, pDemand());
      }
}

void CompositePart::doPRetranslocate(float total_p_supply, float total_p_demand)
   //============================================================================
{
   vector <plantPart *>::iterator part;
   for (part = myParts.begin(); part != myParts.end(); part++)
      (*part)->doPRetranslocate(total_p_supply, total_p_demand);    //FIXME - divy up?
}


float CompositePart::dmGreenStressDeterminant(void)
   //============================================================================
{
   float dm_green = 0.0;
   vector <plantPart *>::iterator part;
   for (part = myParts.begin(); part != myParts.end(); part++)
      dm_green +=  (*part)->dmGreenStressDeterminant();
   return dm_green;
}

float CompositePart::pGreenStressDeterminant(void)
   //============================================================================
{
   float p_green = 0.0;
   vector <plantPart *>::iterator part;
   for (part = myParts.begin(); part != myParts.end(); part++)
      p_green +=  (*part)->pGreenStressDeterminant();
   return p_green;
}

float CompositePart::pMaxPotStressDeterminant(void)
   //============================================================================
{
   float p_max_pot = 0.0;
   vector <plantPart *>::iterator part;
   for (part = myParts.begin(); part != myParts.end(); part++)
      p_max_pot +=  (*part)->pMaxPotStressDeterminant();
   return p_max_pot;
}

float CompositePart::pMinPotStressDeterminant(void)
//============================================================================
{
   float p_min_pot = 0.0;
   vector <plantPart *>::iterator part;
   for (part = myParts.begin(); part != myParts.end(); part++)
      p_min_pot +=  (*part)->pMinPotStressDeterminant();
   return p_min_pot;
}


bool CompositePart::isYieldPart(void)
//============================================================================
// True if at least one of our parts is a dm sink
   {
   for (vector <plantPart * >::const_iterator part = myParts.begin(); part != myParts.end(); part++)
      if ((*part)->isYieldPart()) return true;
   return false;
   }

bool CompositePart::isRetransPart(void)
// True if at least one of our parts supplies retranslocate
   {
   for (vector <plantPart * >::const_iterator part = myParts.begin(); part != myParts.end(); part++)
      if ((*part)->isRetransPart()) return true;
   return false;
   }
