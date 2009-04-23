#ifndef StemH
#define StemH


class Stem : public SimplePart
   {
   public:
      Stem(ScienceAPI& scienceAPI, plantInterface *p, const string &name)
         : SimplePart(scienceAPI, p, name) {};
      ~Stem() {};
      void onHarvest(float height, float remove_fr,
                     vector<string> &dm_type,
                     vector<float> &dlt_crop_dm,
                     vector<float> &dlt_dm_n,
                     vector<float> &dlt_dm_p,
                     vector<float> &fraction_to_residue);
      void  removeBiomass2(float);
   };

#endif /* StemPartH */
