#include "StdPlant.h"

#include "PlantFruitCohorting.h"
#include "Environment.h"
#include "Arbitrators/arbitrator.h"
#include "Leaf/Leaf.h"
#include "Population.h"
using namespace std;

void push_routine (const char *) {};
void pop_routine (const char *) {};

inline bool floatsAreEqual(float A, float B, float C) {return(fabs(A-B)<C);}
//  initialise data members.
PlantFruitCohorting::PlantFruitCohorting(XMLNode params, ScienceAPI& scienceAPI, plantInterface *p, const string &name)
   : CompositePart(params, scienceAPI, p, name)
{
   cohortNum = 0;
   parameters = params;
}

// destructor
PlantFruitCohorting::~PlantFruitCohorting()
{
}

ostream &operator<<(ostream &output, const PlantFruitCohorting /*&pool*/)
{
   //   output << "PlantFruitCohorting:" << endl;
   //   output << "   Green cover:    " << pool.coverPod.green << endl;
   //   output << "   Senesced cover: " << pool.coverPod.sen << endl;
   //   output << "   Dead cover:     " << pool.coverPod.dead << endl;
   //   output << endl;
   //   output << "   Green shell:    " << pool.green.shell << endl;
   //   output << "   Green meal:    " << pool.green.meal << endl;
   //   output << "   Senesced shell: " << pool.senesced.shell << endl;
   //   output << "   Senesced meal: " << pool.senesced.meal << endl;
   //   output << "   Dead shell:     " << pool.dead.shell << endl;
   //   output << "   Dead meal:     " << pool.dead.meal << endl << endl;
   output << endl;
   return output;
}

// Assigment operator
//  assign data members of object
const PlantFruitCohorting &PlantFruitCohorting::operator=(const PlantFruitCohorting &/*other*/)
   //===========================================================================
{
   throw std::invalid_argument("Assignment operator NI for PlantFruitCohorting");
}

void PlantFruitCohorting::prepare (void)
   {
   vector <plantPart *>::iterator part;
   for (part =  myParts.begin(); part != myParts.end(); part++)
      (*part)->prepare ();
   }

void PlantFruitCohorting::process (void)
   {
   vector <plantPart *>::iterator part;
   for (part =  myParts.begin(); part != myParts.end(); part++)
      (*part)->process ();

   dltDM.storeValue(plant->arbitrator().DMSupply());
   }

void PlantFruitCohorting::onInit1(protocol::Component *system)
   //===========================================================================
   {
   CompositePart::onInit1(system);
   addNewCohort(system);
   }

void PlantFruitCohorting::display(ostream &os)
{
   //   os << "PlantFruitCohorting:" << endl;
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

void PlantFruitCohorting::readCultivarParameters (protocol::Component *system, const string &cultivar)
   //===========================================================================
{
      scienceAPI.read("cutout_fract", p.cutout_fract, 0.0f, 1.0f);  // '(0-1)'

      fruit_sites_per_node.read(scienceAPI, "x_node_no_fruit_sites", "sites/node", 0.0, 100.0
                                          , "y_fruit_sites_per_node", "sites/node", 0.0, 100.0);

      scienceAPI.read("dm_fruit_set_min", p.dm_fruit_set_min, 0.0f, 10.0f);
      scienceAPI.read("dm_fruit_set_crit", p.dm_fruit_set_crit, 0.0f, 10.0f);

      scienceAPI.read("days_assimilate_ave", c.days_assimilate_ave, 0, 100);
      dltDM.setSize(c.days_assimilate_ave);
// Temporarily remove
//      scienceAPI.read("fruit_phen_end", c.fruit_phen_end, 0.0f, 1.0f);
      rel_fruit_site.read(scienceAPI, "x_temp_fruit_site", "oC", 0.0, 50.0
                                    , "y_rel_fruit_site", "-", 0.0, 1.0);

   CompositePart::readCultivarParameters(system, cultivar);
}

void PlantFruitCohorting::addNewCohort (protocol::Component *system)
   // ====================================================================
{
   cohortNum ++;
   ostringstream cohortName;
   cohortName << "cohort" << cohortNum;

   cohort = new FruitCohortFN(parameters, scienceAPI, plant, cohortName.str());
   cohort->onInit1(system);
   add(cohort);
   myCohorts.push_back(cohort);

   cohort->giveFlowerNo(dlt_flower_no);
}

void PlantFruitCohorting::doNewCohort (protocol::Component *system)
   // ====================================================================
{
   bool newCohort = false;
   // insert rules to make new cohort here

   if (newCohort)
      addNewCohort(system);
}


//+  Purpose
//       Get change in plant fruit number
void PlantFruitCohorting::doSiteNumber(void)
   //===========================================================================
{
    float dlt_node_no;

    float node_no_now =  plant->leaf().getNodeNo();

    if (plant->phenology().onDayOf("flowering"))       // initial fruit stage
        {
        node_no_first_flower = node_no_now;
        dlt_node_no = plant->leaf().getDltNodeNo();
        }
    else if (node_no_first_flower > 0.0)
        dlt_node_no = plant->leaf().getDltNodeNo();
    else
        dlt_node_no = 0.0;

    // C3
    float dlt_site_no_pot = dlt_node_no * plant->population().Density() * fruit_sites_per_node.value(node_no_now);

    float fruit_tt_target = plant->phenology().TTTargetInPhase("fruiting") * p.cutout_fract;
    float fruit_tt_cum = plant->phenology().TTInPhase("fruiting");

    float metabolic_fact = divide (fruit_tt_cum, fruit_tt_target, 0.0);
    metabolic_fact = bound (metabolic_fact, 0.0, 1.0);

    float temp_fac = rel_fruit_site.value(plant->environment().meant());

    dlt_site_no = dlt_site_no_pot
                * (1.0 - metabolic_fact)
                * temp_fac;
 }

// Purpose
//     Get change in plant flower number
void PlantFruitCohorting::doFlowerNumber (void)
   //===========================================================================
   {
    float flower_no_potential;
    if (setting_fruit)
       flower_no_potential = divide (dltDM.average(), p.dm_fruit_set_min, 0.0);
    else
       flower_no_potential = divide (dltDM.average(), p.dm_fruit_set_crit, 0.0);

    float dlt_flower_no_potential = flower_no_potential - flowerNo();
    dlt_flower_no_potential = l_bound(dlt_flower_no_potential, 0.0);

    dlt_flower_no = dlt_flower_no_potential;
    dlt_flower_no = u_bound (dlt_flower_no, dlt_site_no);
    }

float PlantFruitCohorting::flowerNo (void)
   //===========================================================================
{
   float total = 0.0;
   for (vector<FruitCohortFN *>::iterator cohortI = myCohorts.begin();
        cohortI != myCohorts.end();
        cohortI++)
      total += (*cohortI)->flowerNumber();
   return total;
}

