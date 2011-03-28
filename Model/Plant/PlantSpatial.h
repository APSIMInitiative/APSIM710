#ifndef PlantSpatialH
#define PlantSpatialH



class PlantSpatial {
  public:
  PlantSpatial(ScienceAPI& scienceAPI);
  ~PlantSpatial(void);

   void init(plantInterface *p);

   void zeroAllGlobals(void);
   void zeroDeltas(void);
   void read(ScienceAPI& scienceAPI);
   void startCrop(protocol::SowType& Sow);
   void setPlants(float plants);
   void setCanopyWidth(float canopy_width);
   float canopyFac(void);
   float rowSpacing(void);

//  private:
      float dlt_plants;
      float row_spacing_default;
      float skip_row_default;                           //Default skip row ()
      float skip_plant_default;                         //Default skip plant ()

      float sowing_depth;
      float row_spacing;                                // row spacing (m) [optional]
      float skip_row;                                   // skip row (0, 1, 2)
      float skip_plant;                                 // skip plant (0, 1, 2)
      float skip_row_fac;                               // skip row factor
      float skip_plant_fac;                             // skip plant factor

  private:
      ScienceAPI& scienceAPI;  
      float plants;
      float canopy_width;

      plantInterface *plant;                 // The plant we are attached to
};

#endif /* PlantSpatialH */

