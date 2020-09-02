#ifndef PlantStressH
#define PlantStressH

#include <ComponentInterface/DataTypes.h>
#include <ComponentInterface/Messages.h>
#include <ComponentInterface/ScienceAPI.h>

class RootBase;
class plantPart;

class SWStress {
public:
   SWStress(ScienceAPI& scienceAPI, protocol::Component *p);
   ~SWStress(void);
   void init(RootBase *root);
   void read_sw_constants (void);
   void get_swdef_pheno(protocol::Component *, protocol::QueryValueData &);
   void get_swdef_photo(protocol::Component *, protocol::QueryValueData &);
   void get_swdef_expan(protocol::Component *, protocol::QueryValueData &);
   void get_swdef_fixation(protocol::Component *, protocol::QueryValueData &);
   void get_swstress_pheno(protocol::Component *, protocol::QueryValueData &);
   void get_swstress_photo(protocol::Component *, protocol::QueryValueData &);
   void get_swstress_expan(protocol::Component *, protocol::QueryValueData &);
   void get_swstress_fixation(protocol::Component *, protocol::QueryValueData &);

      void doPlantWaterStress (float sw_demand);
      float SWDefExpansion(float sw_demand);
      float SWDefPhoto(float sw_demand);
      float SWDefPheno(interpolationFunction& cSwDefPheno);
      float SWDefFixation(void);
      float SWDefOxygen (float wet_root_fr);

   StressDeficit swDef;

private:
   ScienceAPI& scienceAPI;

   protocol::Component *parent;                 // The plant we are attached to
   RootBase *rootPart;

   interpolationFunction cSwDefPheno;
   interpolationFunction cSwwDefPhenoFlower;
   interpolationFunction cSwwDefPhenoGrainfill;
   interpolationFunction cSwwDefExpansion;
   interpolationFunction cSwwDefFix;
   interpolationFunction cOxDefPhoto;

}; //SWStress

class TempStress {
public:
   TempStress(ScienceAPI& scienceAPI, protocol::Component *p);
   ~TempStress(void);
   void init(void);
   void doPlantTempStress (Environment& Environment);
   void read_t_constants (void);
   void get_tstress_photo(protocol::Component *, protocol::QueryValueData &);

   StressDeficit tFact;

private:
   ScienceAPI& scienceAPI;

   protocol::Component *parent;                 // The plant we are attached to
   interpolationFunction cTStressPhoto;

}; //TempStress

class NStress {
public:
   NStress(ScienceAPI& scienceAPI, protocol::Component *p);
   ~NStress(void);
   void init(void);
   void doPlantNStress (string module, plantPart* leafPart, plantPart* stemPart);
   float critNFactor(vector< plantPart *> &, float );
   void read_n_constants (void);
   void get_nfact_pheno(protocol::Component *, protocol::QueryValueData &);
   void get_dlt_n_fixed_pot(protocol::Component *, protocol::QueryValueData &);
   void get_dlt_n_fixed(protocol::Component *, protocol::QueryValueData &);
   void get_nfact_photo(protocol::Component *, protocol::QueryValueData &);
   void get_nfact_expan(protocol::Component *, protocol::QueryValueData &);
   void get_nfact_grain(protocol::Component *, protocol::QueryValueData &);
   void get_nstress_photo(protocol::Component *, protocol::QueryValueData &);
   void get_nstress_pheno(protocol::Component *, protocol::QueryValueData &);
   void get_nstress_expan(protocol::Component *, protocol::QueryValueData &);
   void get_nstress_grain(protocol::Component *, protocol::QueryValueData &);

   StressDeficit nFact;

private:
   ScienceAPI& scienceAPI;

   protocol::Component *parent;                 // The plant we are attached to
   struct {
      StressDeficit nFact;
      int   n_stress_option;
   }  c;   // Constants

}; //NStress

class PStress {
public:
   PStress(ScienceAPI& scienceAPI, protocol::Component *p);
   ~PStress(void);
   void init(void);
   bool isPhosphorusAware(void);
   void zero_p_variables (void);
   void  doPlantPStress (vector<plantPart *>&);
   void read_p_constants (void);
   void get_pfact_photo(protocol::Component *, protocol::QueryValueData &qd);
   void get_pfact_pheno(protocol::Component *, protocol::QueryValueData &qd);
   void get_pfact_expansion(protocol::Component *, protocol::QueryValueData &qd);
   void get_pfact_expan(protocol::Component *, protocol::QueryValueData &qd);
   void get_pfact_grain(protocol::Component *, protocol::QueryValueData &qd);
   void get_pstress_photo(protocol::Component *, protocol::QueryValueData &qd);
   void get_pstress_pheno(protocol::Component *, protocol::QueryValueData &qd);
   void get_pstress_expansion(protocol::Component *, protocol::QueryValueData &qd);
   void get_pstress_grain(protocol::Component *, protocol::QueryValueData &qd);

   StressDeficit pFact;

private:
   ScienceAPI& scienceAPI;
   void PlantP_set_phosphorus_aware (void);
   float PlantP_Pfact (vector<plantPart *>&);

   protocol::Component *parent;                 // The plant we are attached to
   bool  phosphorus_aware;
   struct {
      StressDeficit pFactSlope;
   }  c;   // Constants

}; //PStress

#endif //PlantStress_H_

