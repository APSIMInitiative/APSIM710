#ifndef CompositePartH
#define CompositePartH

#include "PlantPart.h"

class CompositePart : public plantPart
{
   friend ostream &operator<<(ostream &, const CompositePart &);
public:                                             // member functions
   CompositePart(XMLNode parameters, ScienceAPI& scienceAPI, plantInterface *p, const string &name, bool createParts = true);

   const CompositePart &operator=(const CompositePart &other);      // Assigment operator

#if TEST_CompositePart
   virtual ~CompositePart();                            // destructor
#else
   ~CompositePart();
#endif
   bool deleteChildren;

   plantThing* get(const std::string& name);
   plantThing* getOptional(const std::string& name);
   plantPart* getPart(const std::string& name);
   std::vector<plantPart*> getParts(const std::vector<std::string>& names);

   std::vector<plantPart*> & parts() {return myParts;}
   void add(plantPart* part);
   void addThing(plantThing* thing);
   plantPart& find(const std::string& name);
   plantThing& findThing(const std::string& name);

   template <class T>
   T findByTypeOptional()
      {
      //===========================================================================
      // Find a child part. Returns NULL if not found.
      //===========================================================================
      for (unsigned p = 0; p != things.size(); p++)
         {
         T child = dynamic_cast<T> (things[p]);
         if (child != NULL)
            return child;
         }
      return NULL;
      }

   template <class T>
   T findByType(const std::string& type)
      {
      //===========================================================================
      // Find a child part. Will throw if not found.
      //===========================================================================
      T child = findByTypeOptional<T>();
      if (child == NULL)
         throw runtime_error("Cannot find part with type: " + type);
      return child;
      }

      // plant
   virtual void prepare(void);
   virtual void process(void);
   virtual float availableRetranslocateN(void);
   virtual void collectDetachedForResidue(vector<string> &part_name
                                          , vector<float> &dm_residue
                                          , vector<float> &dm_n
                                          , vector<float> &dm_p
                                          , vector<float> &fract);

   virtual float coverGreen(void) ;
   virtual float coverSen(void) ;
   virtual void  Detachment(void);
   virtual float DMSupply(void);
   virtual float dltDmPotRue(void);
   virtual float Respiration(void);
   virtual float dltNSenescedRetrans(void);
   virtual float dmGrainWetTotal(void);
   virtual void  doCover (PlantSpatial &spatial);
   virtual void  doDmDemand (float dlt_dm_supply_by_veg);
   virtual void  doDmPotRUE (void );                      // (OUTPUT) potential dry matter (carbohydrate) production (g/m^2)
   virtual void  doNConccentrationLimits(float);
   virtual void  doNDemand1Pot(float, float);
   virtual void  doNDemand1(float, float);
   virtual void  doNDemand2(float, float);
   virtual void  doNDemandGrain(float nfact_grain_conc, float swdef_expansion);
   virtual void  doNFixRetranslocate(float NFix, float NDemandDifferentialTotal);
   virtual void  doNPartition(float nSupply, float n_demand_sum, float n_capacity_sum);
   virtual void  doNRetranslocate( float N_avail_rep, float grain_n_demand);
   virtual void  doNSenescence(void);
   virtual void  doNSenescedRetrans(float navail, float n_demand_tot);
   virtual void  doPDemand(void);
   virtual void  doPPartition(float p_uptake, float total_p_demand);
   virtual void  doPRetranslocate(float total_p_supply, float total_p_demand);
   virtual void  doPSenescence(void);
   virtual void  doRemoveBiomass(protocol::RemoveCropBiomassType dmRemoved, bool c_remove_biomass_report);
   virtual void  doSenescence (float sen_fr);
   virtual void  doMaintenanceRespirationPFR(void);         // Plant and Food implementation
   virtual void  doSoilNDemand(void);
   virtual void  doSWDemand(float SWDemandMaxFactor);     //(OUTPUT) crop water demand (mm)               //FIXME
   virtual float grainNo(void);
   virtual float grainWt(void);
   virtual float dltDmPotentialGrain();
   virtual void  morphology(void);
   virtual float nCapacity(void);
   virtual float nDemand(void);
   virtual float nMax(void);
   virtual float pDemand(void);
   virtual float pRetransSupply(void);
   virtual float pRetransDemand(void);
   virtual void  removeBiomass(void);
   virtual float soilNDemand(void);
   virtual float SWDemand(void);                         //(OUTPUT) crop water demand (mm)               //FIXME
   virtual void  update(void);
   virtual float dmGreenDemand(void);
   virtual void  zeroDltNSenescedTrans(void);
   virtual void  get_AvailableToAnimal(protocol::AvailableToAnimalType &cohort);
   virtual void  set_RemovedByAnimal(const protocol::RemovedByAnimalType &removed);

   virtual void onEndCrop(vector<string> &dm_type,
                          vector<float> &dlt_crop_dm,
                          vector<float> &dlt_dm_n,
                          vector<float> &dlt_dm_p,
                          vector<float> &fraction_to_residue);

   virtual void onHarvest(float height, float remove_fr,
                          vector<string> &dm_type,
                          vector<float> &dlt_crop_dm,
                          vector<float> &dlt_dm_n,
                          vector<float> &dlt_dm_p,
                          vector<float> &fraction_to_residue);

   virtual void  onInit1(protocol::Component *);
   virtual void onKillStem(void);
   virtual void onPlantEvent(const string &);

   virtual void writeCultivarInfo (protocol::Component *);

   virtual void get_dlt_dm_green_retrans(vector<float> &);
   virtual void get_dlt_p_retrans(vector<float> &dlt_p_retrans);
   virtual void get_dm_plant_min(vector<float> &);
   virtual void get_dm_senesced(vector<float> &);
   virtual void get_name(vector<string> &name);
   virtual void get_n_demanded(vector<float> &);
   virtual void get_p_demand(vector<float> &p_demand);
   void doRadnPartition();
   virtual void zeroDltDmGreen(void);
   virtual void readConstants (protocol::Component *, const string &);
   virtual void readSpeciesParameters (protocol::Component *, vector<string> &);
   virtual void readCultivarParameters (protocol::Component *, const string &);
   virtual void zeroAllGlobals(void);
   virtual void zeroDeltas(void);
   virtual float dmGreenStressDeterminant(void);
   virtual float pGreenStressDeterminant(void);
   virtual float pMaxPotStressDeterminant(void);
   virtual float pMinPotStressDeterminant(void);

protected:
   virtual void createParts(XMLNode parameters);
   virtual void doProcessBioDemand(void);
   virtual void onEmergence();

   virtual void get_dlt_dm_green(vector<float> &);
   virtual void get_dm_green(vector<float> &);


   virtual void  removeBiomass2(float);

   virtual void doGrainNumber (void);




   virtual float coverTotal(void) ;
   virtual void interceptRadiationGreen(float radiation);
   virtual float calcInterceptRadiationTotal(float radiation);
   virtual float nDemandGrain(void);

   virtual float dltDmGrainDemand(void);
   virtual float dltDmRetranslocateSupply(float demand_differential) ;
   virtual float dltDmGreenRetransUptake(void);
   virtual float transpirationEfficiency(void);

   

   virtual float grainWaterContent(void);

   virtual float dmRetransSupply(void);
   virtual float dmRetransDemand(void) ;

   virtual float nDemandGrain2(void);
   virtual float nRetransSupply(void);
   virtual float nRetransDemand(void);


   virtual float dltNRetransOut(void);


   virtual float pMaxPot(void);
   virtual float pMinPot(void);


   virtual void display(ostream &os = cout) ;  // display function

   virtual void calcDlt_pod_area (void);   //FIXME

   virtual float SWDemandTE(void);                       //(OUTPUT) crop water demand (mm)               //FIXME

   virtual float giveDmGreen(float dmSupplied);
   virtual void doDmRetranslocate(float DMAvail, float DMDemandDifferentialTotal);
   virtual float dmDemandDifferential(void) ;
   virtual float nDemandDifferential(void);

   virtual float n_conc_crit(void);
   virtual float n_conc_min(void);

   virtual float dltNSenescedTrans(void);

   virtual bool isYieldPart(void);
   virtual bool isRetransPart(void);

   vector <plantPart *> myParts;


   private:
      std::vector<plantThing*> things;

      float nConcCrit();
      float nConcMin();
      std::string addPartToVar(const std::string& variableName);
      std::string addPartToDesc(const std::string& description);
      std::string sectionToReadFrom();

};

#endif
