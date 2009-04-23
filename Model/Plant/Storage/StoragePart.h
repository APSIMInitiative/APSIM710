#ifndef StoragePartH
#define StoragePartH
#include "SimplePart.h"

class StoragePart : public SimplePart
   {
   public:
      StoragePart(ScienceAPI& scienceAPI, plantInterface *p, const string &name)
         : SimplePart(scienceAPI, p, name) {};
      ~StoragePart() {};
      void onHarvest(float height, float remove_fr,
                     vector<string> &dm_type,
                     vector<float> &dlt_crop_dm,
                     vector<float> &dlt_dm_n,
                     vector<float> &dlt_dm_p,
                     vector<float> &fraction_to_residue);
      void  update(void);
      void  removeBiomass2(float);
   float dmGreenDemand(void)
     { return(1e6);};   // Maximum DM this part can take today - ask for all you can get - need to do this better
   };

#endif /* StoragePartH */
