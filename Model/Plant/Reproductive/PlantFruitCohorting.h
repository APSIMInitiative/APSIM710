
// Modification log
// 2 Feb 05 J. Hargreaves  Implementation

#ifndef PLANTFRUITCOHORTING_H
#define PLANTFRUITCOHORTING_H

#include "../CompositePart.h"
#include "FruitCohortFN.h"


class PlantFruitCohorting : public CompositePart
{
   friend ostream &operator<<(ostream &, const PlantFruitCohorting &);
public:                                             // member functions
   PlantFruitCohorting(XMLNode parameters, ScienceAPI& scienceAPI, plantInterface *p, const string &name);
   ~PlantFruitCohorting();
   void display(ostream &os);

   const PlantFruitCohorting &operator=(const PlantFruitCohorting &other);        // Assigment operator

   void prepare(void);
   void process(void);
   void onInit1(protocol::Component *system);
   void readCultivarParameters (protocol::Component *, const string &);
   void doNewCohort(protocol::Component *system);
   void addNewCohort (protocol::Component *system);
   void doSiteNumber(void);
   void doFlowerNumber(void);
   float flowerNo (void);

private:
   XMLNode parameters;
   int cohortNum;

   // Fruit Cohort specific
   FruitCohortFN *cohort;
   vector<FruitCohortFN *> myCohorts;

    interpolationFunction fruit_sites_per_node;
    interpolationFunction rel_fruit_site;
    plantQueue dltDM;
    plantQueue vegRatio;

     bool  setting_fruit;
      float     node_no_first_flower;
      float     site_no;
      float     fruit_no_potential;
      float     dlt_site_no;
      float     dlt_flower_no;

   struct {
      float    dm_fruit_set_crit;
      float    dm_fruit_set_min;
      float     cutout_fract;
   } p; // Parameters

   struct {
      int         days_assimilate_ave;
   } c; // Constants

};

#endif
