#ifndef Co2ModifierH
#define Co2ModifierH

typedef enum {photosynthetic_pathway_UNDEF, photosynthetic_pathway_C3, photosynthetic_pathway_C4} photosynthetic_pathway_t;

class Co2Modifier {
public:
   Co2Modifier(ScienceAPI& scienceAPI, plantInterface& plant);
   ~Co2Modifier(void);
   void init(void);
   void zero_co2_variables (void);
   void doPlant_Co2Modifier();
   void read_co2_constants (void);
   float rue (void) const;
   float te (void) const;
   float n_conc (void);

   StressDeficit tFact;

private:
   ScienceAPI& scienceAPI;
   plantInterface& plant;

   interpolationFunction cTE;
   interpolationFunction cNConc;


      float     co2_modifier_rue;
      float     co2_modifier_te;
      float     co2_modifier_n_conc;
      float     co2_default;

   struct {
      photosynthetic_pathway_t photosynthetic_pathway;
   }  c;   // Constants

   float plant_rue_co2_modifier(float co2,                 //!CO2 level (ppm)
                               float meanT);          //!modifier (-)
}; //Co2Modifier


#endif //Co2Modifier_H_

