#include "stdafx.h"
using namespace System;
using namespace System::Runtime::InteropServices;

//------ Wheat ------
public ref class WheatType
   {
   private:
      ComponentType^ Comp;
   public:
      WheatType(ComponentType^ c) : Comp(c) { }
   
      property Int32 active
         {
         Int32 get()
            {
            return Comp->Variable[ "active" ]->ToInt32();
            }       
         } 
      property String^ author
         {
         String^ get()
            {
            return Comp->Variable[ "author" ]->ToString();
            }       
         } 
      property Single biomass
         {
         Single get()
            {
            return Comp->Variable[ "biomass" ]->ToSingle();
            }       
         } 
      property Single biomass_n
         {
         Single get()
            {
            return Comp->Variable[ "biomass_n" ]->ToSingle();
            }       
         } 
      property Single biomass_p
         {
         Single get()
            {
            return Comp->Variable[ "biomass_p" ]->ToSingle();
            }       
         } 
      property Single biomass_wt
         {
         Single get()
            {
            return Comp->Variable[ "biomass_wt" ]->ToSingle();
            }       
         } 
      property Single cohort1detachingn
         {
         Single get()
            {
            return Comp->Variable[ "cohort1detachingn" ]->ToSingle();
            }       
         } 
      property Single cohort1detachingp
         {
         Single get()
            {
            return Comp->Variable[ "cohort1detachingp" ]->ToSingle();
            }       
         } 
      property Single cohort1detachingwt
         {
         Single get()
            {
            return Comp->Variable[ "cohort1detachingwt" ]->ToSingle();
            }       
         } 
      property Single cohort1grainn
         {
         Single get()
            {
            return Comp->Variable[ "cohort1grainn" ]->ToSingle();
            }       
         } 
      property Single cohort1grainnconc
         {
         Single get()
            {
            return Comp->Variable[ "cohort1grainnconc" ]->ToSingle();
            }       
         } 
      property Single cohort1grainnonstructuralwt
         {
         Single get()
            {
            return Comp->Variable[ "cohort1grainnonstructuralwt" ]->ToSingle();
            }       
         } 
      property Single cohort1grainp
         {
         Single get()
            {
            return Comp->Variable[ "cohort1grainp" ]->ToSingle();
            }       
         } 
      property Single cohort1grainpconc
         {
         Single get()
            {
            return Comp->Variable[ "cohort1grainpconc" ]->ToSingle();
            }       
         } 
      property Single cohort1grainstructuralwt
         {
         Single get()
            {
            return Comp->Variable[ "cohort1grainstructuralwt" ]->ToSingle();
            }       
         } 
      property Single cohort1graintotaln
         {
         Single get()
            {
            return Comp->Variable[ "cohort1graintotaln" ]->ToSingle();
            }       
         } 
      property Single cohort1graintotalnconc
         {
         Single get()
            {
            return Comp->Variable[ "cohort1graintotalnconc" ]->ToSingle();
            }       
         } 
      property Single cohort1graintotalnonstructuralwt
         {
         Single get()
            {
            return Comp->Variable[ "cohort1graintotalnonstructuralwt" ]->ToSingle();
            }       
         } 
      property Single cohort1graintotalp
         {
         Single get()
            {
            return Comp->Variable[ "cohort1graintotalp" ]->ToSingle();
            }       
         } 
      property Single cohort1graintotalpconc
         {
         Single get()
            {
            return Comp->Variable[ "cohort1graintotalpconc" ]->ToSingle();
            }       
         } 
      property Single cohort1graintotalstructuralwt
         {
         Single get()
            {
            return Comp->Variable[ "cohort1graintotalstructuralwt" ]->ToSingle();
            }       
         } 
      property Single cohort1graintotalwt
         {
         Single get()
            {
            return Comp->Variable[ "cohort1graintotalwt" ]->ToSingle();
            }       
         } 
      property Single cohort1grainwt
         {
         Single get()
            {
            return Comp->Variable[ "cohort1grainwt" ]->ToSingle();
            }       
         } 
      property Single cohort1greenn
         {
         Single get()
            {
            return Comp->Variable[ "cohort1greenn" ]->ToSingle();
            }       
         } 
      property Single cohort1greennconc
         {
         Single get()
            {
            return Comp->Variable[ "cohort1greennconc" ]->ToSingle();
            }       
         } 
      property Single cohort1greennonstructuralwt
         {
         Single get()
            {
            return Comp->Variable[ "cohort1greennonstructuralwt" ]->ToSingle();
            }       
         } 
      property Single cohort1greenp
         {
         Single get()
            {
            return Comp->Variable[ "cohort1greenp" ]->ToSingle();
            }       
         } 
      property Single cohort1greenpconc
         {
         Single get()
            {
            return Comp->Variable[ "cohort1greenpconc" ]->ToSingle();
            }       
         } 
      property Single cohort1greenremovedn
         {
         Single get()
            {
            return Comp->Variable[ "cohort1greenremovedn" ]->ToSingle();
            }       
         } 
      property Single cohort1greenremovedp
         {
         Single get()
            {
            return Comp->Variable[ "cohort1greenremovedp" ]->ToSingle();
            }       
         } 
      property Single cohort1greenremovedwt
         {
         Single get()
            {
            return Comp->Variable[ "cohort1greenremovedwt" ]->ToSingle();
            }       
         } 
      property Single cohort1greenstructuralwt
         {
         Single get()
            {
            return Comp->Variable[ "cohort1greenstructuralwt" ]->ToSingle();
            }       
         } 
      property Single cohort1greenwt
         {
         Single get()
            {
            return Comp->Variable[ "cohort1greenwt" ]->ToSingle();
            }       
         } 
      property Single cohort1growthn
         {
         Single get()
            {
            return Comp->Variable[ "cohort1growthn" ]->ToSingle();
            }       
         } 
      property Single cohort1growthp
         {
         Single get()
            {
            return Comp->Variable[ "cohort1growthp" ]->ToSingle();
            }       
         } 
      property Single cohort1growthwt
         {
         Single get()
            {
            return Comp->Variable[ "cohort1growthwt" ]->ToSingle();
            }       
         } 
      property Single cohort1retranslocationn
         {
         Single get()
            {
            return Comp->Variable[ "cohort1retranslocationn" ]->ToSingle();
            }       
         } 
      property Single cohort1retranslocationp
         {
         Single get()
            {
            return Comp->Variable[ "cohort1retranslocationp" ]->ToSingle();
            }       
         } 
      property Single cohort1retranslocationwt
         {
         Single get()
            {
            return Comp->Variable[ "cohort1retranslocationwt" ]->ToSingle();
            }       
         } 
      property Single cohort1senescedn
         {
         Single get()
            {
            return Comp->Variable[ "cohort1senescedn" ]->ToSingle();
            }       
         } 
      property Single cohort1senescednconc
         {
         Single get()
            {
            return Comp->Variable[ "cohort1senescednconc" ]->ToSingle();
            }       
         } 
      property Single cohort1senescednonstructuralwt
         {
         Single get()
            {
            return Comp->Variable[ "cohort1senescednonstructuralwt" ]->ToSingle();
            }       
         } 
      property Single cohort1senescedp
         {
         Single get()
            {
            return Comp->Variable[ "cohort1senescedp" ]->ToSingle();
            }       
         } 
      property Single cohort1senescedpconc
         {
         Single get()
            {
            return Comp->Variable[ "cohort1senescedpconc" ]->ToSingle();
            }       
         } 
      property Single cohort1senescedremovedn
         {
         Single get()
            {
            return Comp->Variable[ "cohort1senescedremovedn" ]->ToSingle();
            }       
         } 
      property Single cohort1senescedremovedp
         {
         Single get()
            {
            return Comp->Variable[ "cohort1senescedremovedp" ]->ToSingle();
            }       
         } 
      property Single cohort1senescedremovedwt
         {
         Single get()
            {
            return Comp->Variable[ "cohort1senescedremovedwt" ]->ToSingle();
            }       
         } 
      property Single cohort1senescedstructuralwt
         {
         Single get()
            {
            return Comp->Variable[ "cohort1senescedstructuralwt" ]->ToSingle();
            }       
         } 
      property Single cohort1senescedwt
         {
         Single get()
            {
            return Comp->Variable[ "cohort1senescedwt" ]->ToSingle();
            }       
         } 
      property Single cohort1senescingn
         {
         Single get()
            {
            return Comp->Variable[ "cohort1senescingn" ]->ToSingle();
            }       
         } 
      property Single cohort1senescingp
         {
         Single get()
            {
            return Comp->Variable[ "cohort1senescingp" ]->ToSingle();
            }       
         } 
      property Single cohort1senescingwt
         {
         Single get()
            {
            return Comp->Variable[ "cohort1senescingwt" ]->ToSingle();
            }       
         } 
      property Single cohort1totaln
         {
         Single get()
            {
            return Comp->Variable[ "cohort1totaln" ]->ToSingle();
            }       
         } 
      property Single cohort1totalnconc
         {
         Single get()
            {
            return Comp->Variable[ "cohort1totalnconc" ]->ToSingle();
            }       
         } 
      property Single cohort1totalnonstructuralwt
         {
         Single get()
            {
            return Comp->Variable[ "cohort1totalnonstructuralwt" ]->ToSingle();
            }       
         } 
      property Single cohort1totalp
         {
         Single get()
            {
            return Comp->Variable[ "cohort1totalp" ]->ToSingle();
            }       
         } 
      property Single cohort1totalpconc
         {
         Single get()
            {
            return Comp->Variable[ "cohort1totalpconc" ]->ToSingle();
            }       
         } 
      property Single cohort1totalstructuralwt
         {
         Single get()
            {
            return Comp->Variable[ "cohort1totalstructuralwt" ]->ToSingle();
            }       
         } 
      property Single cohort1totalwt
         {
         Single get()
            {
            return Comp->Variable[ "cohort1totalwt" ]->ToSingle();
            }       
         } 
      property Single cohort1vegetativen
         {
         Single get()
            {
            return Comp->Variable[ "cohort1vegetativen" ]->ToSingle();
            }       
         } 
      property Single cohort1vegetativenconc
         {
         Single get()
            {
            return Comp->Variable[ "cohort1vegetativenconc" ]->ToSingle();
            }       
         } 
      property Single cohort1vegetativenonstructuralwt
         {
         Single get()
            {
            return Comp->Variable[ "cohort1vegetativenonstructuralwt" ]->ToSingle();
            }       
         } 
      property Single cohort1vegetativep
         {
         Single get()
            {
            return Comp->Variable[ "cohort1vegetativep" ]->ToSingle();
            }       
         } 
      property Single cohort1vegetativepconc
         {
         Single get()
            {
            return Comp->Variable[ "cohort1vegetativepconc" ]->ToSingle();
            }       
         } 
      property Single cohort1vegetativestructuralwt
         {
         Single get()
            {
            return Comp->Variable[ "cohort1vegetativestructuralwt" ]->ToSingle();
            }       
         } 
      property Single cohort1vegetativetotaln
         {
         Single get()
            {
            return Comp->Variable[ "cohort1vegetativetotaln" ]->ToSingle();
            }       
         } 
      property Single cohort1vegetativetotalnconc
         {
         Single get()
            {
            return Comp->Variable[ "cohort1vegetativetotalnconc" ]->ToSingle();
            }       
         } 
      property Single cohort1vegetativetotalnonstructuralwt
         {
         Single get()
            {
            return Comp->Variable[ "cohort1vegetativetotalnonstructuralwt" ]->ToSingle();
            }       
         } 
      property Single cohort1vegetativetotalp
         {
         Single get()
            {
            return Comp->Variable[ "cohort1vegetativetotalp" ]->ToSingle();
            }       
         } 
      property Single cohort1vegetativetotalpconc
         {
         Single get()
            {
            return Comp->Variable[ "cohort1vegetativetotalpconc" ]->ToSingle();
            }       
         } 
      property Single cohort1vegetativetotalstructuralwt
         {
         Single get()
            {
            return Comp->Variable[ "cohort1vegetativetotalstructuralwt" ]->ToSingle();
            }       
         } 
      property Single cohort1vegetativetotalwt
         {
         Single get()
            {
            return Comp->Variable[ "cohort1vegetativetotalwt" ]->ToSingle();
            }       
         } 
      property Single cohort1vegetativewt
         {
         Single get()
            {
            return Comp->Variable[ "cohort1vegetativewt" ]->ToSingle();
            }       
         } 
      property Single cover_green
         {
         Single get()
            {
            return Comp->Variable[ "cover_green" ]->ToSingle();
            }       
         } 
      property Single cover_tot
         {
         Single get()
            {
            return Comp->Variable[ "cover_tot" ]->ToSingle();
            }       
         } 
      property String^ crop_class
         {
         String^ get()
            {
            return Comp->Variable[ "crop_class" ]->ToString();
            }       
         } 
      property String^ crop_type
         {
         String^ get()
            {
            return Comp->Variable[ "crop_type" ]->ToString();
            }       
         } 
      property Int32 daysafteremergence
         {
         Int32 get()
            {
            return Comp->Variable[ "daysafteremergence" ]->ToInt32();
            }       
         } 
      property Int32 daysafterend_crop
         {
         Int32 get()
            {
            return Comp->Variable[ "daysafterend_crop" ]->ToInt32();
            }       
         } 
      property Int32 daysafterend_grain_fill
         {
         Int32 get()
            {
            return Comp->Variable[ "daysafterend_grain_fill" ]->ToInt32();
            }       
         } 
      property Int32 daysafterend_of_juvenile
         {
         Int32 get()
            {
            return Comp->Variable[ "daysafterend_of_juvenile" ]->ToInt32();
            }       
         } 
      property Int32 daysafterfloral_initiation
         {
         Int32 get()
            {
            return Comp->Variable[ "daysafterfloral_initiation" ]->ToInt32();
            }       
         } 
      property Int32 daysafterflowering
         {
         Int32 get()
            {
            return Comp->Variable[ "daysafterflowering" ]->ToInt32();
            }       
         } 
      property Int32 daysaftergermination
         {
         Int32 get()
            {
            return Comp->Variable[ "daysaftergermination" ]->ToInt32();
            }       
         } 
      property Int32 daysafterharvest_ripe
         {
         Int32 get()
            {
            return Comp->Variable[ "daysafterharvest_ripe" ]->ToInt32();
            }       
         } 
      property Int32 daysaftermaturity
         {
         Int32 get()
            {
            return Comp->Variable[ "daysaftermaturity" ]->ToInt32();
            }       
         } 
      property Int32 daysafterout
         {
         Int32 get()
            {
            return Comp->Variable[ "daysafterout" ]->ToInt32();
            }       
         } 
      property Int32 daysaftersowing
         {
         Int32 get()
            {
            return Comp->Variable[ "daysaftersowing" ]->ToInt32();
            }       
         } 
      property Int32 daysafterstart_grain_fill
         {
         Int32 get()
            {
            return Comp->Variable[ "daysafterstart_grain_fill" ]->ToInt32();
            }       
         } 
      property Single deltastage
         {
         Single get()
            {
            return Comp->Variable[ "deltastage" ]->ToSingle();
            }       
         } 
      property Single detachingn
         {
         Single get()
            {
            return Comp->Variable[ "detachingn" ]->ToSingle();
            }       
         } 
      property Single detachingp
         {
         Single get()
            {
            return Comp->Variable[ "detachingp" ]->ToSingle();
            }       
         } 
      property Single detachingwt
         {
         Single get()
            {
            return Comp->Variable[ "detachingwt" ]->ToSingle();
            }       
         } 
      property Single dlt_dm
         {
         Single get()
            {
            return Comp->Variable[ "dlt_dm" ]->ToSingle();
            }       
         } 
      property Single dlt_dm_fruit
         {
         Single get()
            {
            return Comp->Variable[ "dlt_dm_fruit" ]->ToSingle();
            }       
         } 
      property Single dlt_dm_grain_demand
         {
         Single get()
            {
            return Comp->Variable[ "dlt_dm_grain_demand" ]->ToSingle();
            }       
         } 
      property array<Single>^ dlt_dm_green_retrans
         {
         array<Single>^ get()
            {
            return Comp->Variable[ "dlt_dm_green_retrans" ]->ToSingleArray();
            }       
         } 
      property Single dlt_dm_oil_conv_retrans
         {
         Single get()
            {
            return Comp->Variable[ "dlt_dm_oil_conv_retrans" ]->ToSingle();
            }       
         } 
      property Single dlt_dm_pot_rue
         {
         Single get()
            {
            return Comp->Variable[ "dlt_dm_pot_rue" ]->ToSingle();
            }       
         } 
      property Single dlt_dm_pot_rue_pod
         {
         Single get()
            {
            return Comp->Variable[ "dlt_dm_pot_rue_pod" ]->ToSingle();
            }       
         } 
      property Single dlt_lai_pot
         {
         Single get()
            {
            return Comp->Variable[ "dlt_lai_pot" ]->ToSingle();
            }       
         } 
      property Single dlt_lai_stressed
         {
         Single get()
            {
            return Comp->Variable[ "dlt_lai_stressed" ]->ToSingle();
            }       
         } 
      property Single dlt_leaf_no
         {
         Single get()
            {
            return Comp->Variable[ "dlt_leaf_no" ]->ToSingle();
            }       
         } 
      property Single dlt_leaf_no_pot
         {
         Single get()
            {
            return Comp->Variable[ "dlt_leaf_no_pot" ]->ToSingle();
            }       
         } 
      property Single dlt_n_fixed
         {
         Single get()
            {
            return Comp->Variable[ "dlt_n_fixed" ]->ToSingle();
            }       
         } 
      property Single dlt_n_fixed_pot
         {
         Single get()
            {
            return Comp->Variable[ "dlt_n_fixed_pot" ]->ToSingle();
            }       
         } 
      property Single dlt_n_senesced_retrans
         {
         Single get()
            {
            return Comp->Variable[ "dlt_n_senesced_retrans" ]->ToSingle();
            }       
         } 
      property Single dlt_n_senesced_retrans_cohort1
         {
         Single get()
            {
            return Comp->Variable[ "dlt_n_senesced_retrans_cohort1" ]->ToSingle();
            }       
         } 
      property Single dlt_n_senesced_retrans_grain
         {
         Single get()
            {
            return Comp->Variable[ "dlt_n_senesced_retrans_grain" ]->ToSingle();
            }       
         } 
      property Single dlt_n_senesced_retrans_head
         {
         Single get()
            {
            return Comp->Variable[ "dlt_n_senesced_retrans_head" ]->ToSingle();
            }       
         } 
      property Single dlt_n_senesced_retrans_leaf
         {
         Single get()
            {
            return Comp->Variable[ "dlt_n_senesced_retrans_leaf" ]->ToSingle();
            }       
         } 
      property Single dlt_n_senesced_retrans_meal
         {
         Single get()
            {
            return Comp->Variable[ "dlt_n_senesced_retrans_meal" ]->ToSingle();
            }       
         } 
      property Single dlt_n_senesced_retrans_oil
         {
         Single get()
            {
            return Comp->Variable[ "dlt_n_senesced_retrans_oil" ]->ToSingle();
            }       
         } 
      property Single dlt_n_senesced_retrans_pod
         {
         Single get()
            {
            return Comp->Variable[ "dlt_n_senesced_retrans_pod" ]->ToSingle();
            }       
         } 
      property Single dlt_n_senesced_retrans_root
         {
         Single get()
            {
            return Comp->Variable[ "dlt_n_senesced_retrans_root" ]->ToSingle();
            }       
         } 
      property Single dlt_n_senesced_retrans_stem
         {
         Single get()
            {
            return Comp->Variable[ "dlt_n_senesced_retrans_stem" ]->ToSingle();
            }       
         } 
      property Single dlt_n_senesced_trans
         {
         Single get()
            {
            return Comp->Variable[ "dlt_n_senesced_trans" ]->ToSingle();
            }       
         } 
      property Single dlt_n_senesced_trans_cohort1
         {
         Single get()
            {
            return Comp->Variable[ "dlt_n_senesced_trans_cohort1" ]->ToSingle();
            }       
         } 
      property Single dlt_n_senesced_trans_grain
         {
         Single get()
            {
            return Comp->Variable[ "dlt_n_senesced_trans_grain" ]->ToSingle();
            }       
         } 
      property Single dlt_n_senesced_trans_head
         {
         Single get()
            {
            return Comp->Variable[ "dlt_n_senesced_trans_head" ]->ToSingle();
            }       
         } 
      property Single dlt_n_senesced_trans_leaf
         {
         Single get()
            {
            return Comp->Variable[ "dlt_n_senesced_trans_leaf" ]->ToSingle();
            }       
         } 
      property Single dlt_n_senesced_trans_meal
         {
         Single get()
            {
            return Comp->Variable[ "dlt_n_senesced_trans_meal" ]->ToSingle();
            }       
         } 
      property Single dlt_n_senesced_trans_oil
         {
         Single get()
            {
            return Comp->Variable[ "dlt_n_senesced_trans_oil" ]->ToSingle();
            }       
         } 
      property Single dlt_n_senesced_trans_pod
         {
         Single get()
            {
            return Comp->Variable[ "dlt_n_senesced_trans_pod" ]->ToSingle();
            }       
         } 
      property Single dlt_n_senesced_trans_root
         {
         Single get()
            {
            return Comp->Variable[ "dlt_n_senesced_trans_root" ]->ToSingle();
            }       
         } 
      property Single dlt_n_senesced_trans_stem
         {
         Single get()
            {
            return Comp->Variable[ "dlt_n_senesced_trans_stem" ]->ToSingle();
            }       
         } 
      property Single dlt_node_no
         {
         Single get()
            {
            return Comp->Variable[ "dlt_node_no" ]->ToSingle();
            }       
         } 
      property array<Single>^ dlt_p_retrans
         {
         array<Single>^ get()
            {
            return Comp->Variable[ "dlt_p_retrans" ]->ToSingleArray();
            }       
         } 
      property Single dlt_pai
         {
         Single get()
            {
            return Comp->Variable[ "dlt_pai" ]->ToSingle();
            }       
         } 
      property Single dlt_slai_age
         {
         Single get()
            {
            return Comp->Variable[ "dlt_slai_age" ]->ToSingle();
            }       
         } 
      property Single dlt_slai_frost
         {
         Single get()
            {
            return Comp->Variable[ "dlt_slai_frost" ]->ToSingle();
            }       
         } 
      property Single dlt_slai_light
         {
         Single get()
            {
            return Comp->Variable[ "dlt_slai_light" ]->ToSingle();
            }       
         } 
      property Single dlt_slai_water
         {
         Single get()
            {
            return Comp->Variable[ "dlt_slai_water" ]->ToSingle();
            }       
         } 
      property Single dm_demand_cohort1
         {
         Single get()
            {
            return Comp->Variable[ "dm_demand_cohort1" ]->ToSingle();
            }       
         } 
      property Single dm_demand_grain
         {
         Single get()
            {
            return Comp->Variable[ "dm_demand_grain" ]->ToSingle();
            }       
         } 
      property Single dm_demand_head
         {
         Single get()
            {
            return Comp->Variable[ "dm_demand_head" ]->ToSingle();
            }       
         } 
      property Single dm_demand_leaf
         {
         Single get()
            {
            return Comp->Variable[ "dm_demand_leaf" ]->ToSingle();
            }       
         } 
      property Single dm_demand_meal
         {
         Single get()
            {
            return Comp->Variable[ "dm_demand_meal" ]->ToSingle();
            }       
         } 
      property Single dm_demand_oil
         {
         Single get()
            {
            return Comp->Variable[ "dm_demand_oil" ]->ToSingle();
            }       
         } 
      property Single dm_demand_pod
         {
         Single get()
            {
            return Comp->Variable[ "dm_demand_pod" ]->ToSingle();
            }       
         } 
      property Single dm_demand_root
         {
         Single get()
            {
            return Comp->Variable[ "dm_demand_root" ]->ToSingle();
            }       
         } 
      property Single dm_demand_stem
         {
         Single get()
            {
            return Comp->Variable[ "dm_demand_stem" ]->ToSingle();
            }       
         } 
      property array<Single>^ dm_plant_min
         {
         array<Single>^ get()
            {
            return Comp->Variable[ "dm_plant_min" ]->ToSingleArray();
            }       
         } 
      property Single effective_rue
         {
         Single get()
            {
            return Comp->Variable[ "effective_rue" ]->ToSingle();
            }       
         } 
      property Int32 emergence_das
         {
         Int32 get()
            {
            return Comp->Variable[ "emergence_das" ]->ToInt32();
            }       
         } 
      property Int32 emergence_date
         {
         Int32 get()
            {
            return Comp->Variable[ "emergence_date" ]->ToInt32();
            }       
         } 
      property Single emergencetttarget
         {
         Single get()
            {
            return Comp->Variable[ "emergencetttarget" ]->ToSingle();
            }       
         } 
      property Single end_croptttarget
         {
         Single get()
            {
            return Comp->Variable[ "end_croptttarget" ]->ToSingle();
            }       
         } 
      property Single end_grain_filltttarget
         {
         Single get()
            {
            return Comp->Variable[ "end_grain_filltttarget" ]->ToSingle();
            }       
         } 
      property Single end_of_juveniletttarget
         {
         Single get()
            {
            return Comp->Variable[ "end_of_juveniletttarget" ]->ToSingle();
            }       
         } 
      property Single ep
         {
         Single get()
            {
            return Comp->Variable[ "ep" ]->ToSingle();
            }       
         } 
      property array<Single>^ esw_layr
         {
         array<Single>^ get()
            {
            return Comp->Variable[ "esw_layr" ]->ToSingleArray();
            }       
         } 
      property Int32 floral_initiation_das
         {
         Int32 get()
            {
            return Comp->Variable[ "floral_initiation_das" ]->ToInt32();
            }       
         } 
      property Int32 floral_initiation_date
         {
         Int32 get()
            {
            return Comp->Variable[ "floral_initiation_date" ]->ToInt32();
            }       
         } 
      property Single floral_initiationtttarget
         {
         Single get()
            {
            return Comp->Variable[ "floral_initiationtttarget" ]->ToSingle();
            }       
         } 
      property Int32 flowering_das
         {
         Int32 get()
            {
            return Comp->Variable[ "flowering_das" ]->ToInt32();
            }       
         } 
      property Int32 flowering_date
         {
         Int32 get()
            {
            return Comp->Variable[ "flowering_date" ]->ToInt32();
            }       
         } 
      property Single floweringtttarget
         {
         Single get()
            {
            return Comp->Variable[ "floweringtttarget" ]->ToSingle();
            }       
         } 
      property Single germinationtttarget
         {
         Single get()
            {
            return Comp->Variable[ "germinationtttarget" ]->ToSingle();
            }       
         } 
      property Single grain1detachingn
         {
         Single get()
            {
            return Comp->Variable[ "grain1detachingn" ]->ToSingle();
            }       
         } 
      property Single grain1detachingp
         {
         Single get()
            {
            return Comp->Variable[ "grain1detachingp" ]->ToSingle();
            }       
         } 
      property Single grain1detachingwt
         {
         Single get()
            {
            return Comp->Variable[ "grain1detachingwt" ]->ToSingle();
            }       
         } 
      property Single grain1grainn
         {
         Single get()
            {
            return Comp->Variable[ "grain1grainn" ]->ToSingle();
            }       
         } 
      property Single grain1grainnconc
         {
         Single get()
            {
            return Comp->Variable[ "grain1grainnconc" ]->ToSingle();
            }       
         } 
      property Single grain1grainnonstructuralwt
         {
         Single get()
            {
            return Comp->Variable[ "grain1grainnonstructuralwt" ]->ToSingle();
            }       
         } 
      property Single grain1grainp
         {
         Single get()
            {
            return Comp->Variable[ "grain1grainp" ]->ToSingle();
            }       
         } 
      property Single grain1grainpconc
         {
         Single get()
            {
            return Comp->Variable[ "grain1grainpconc" ]->ToSingle();
            }       
         } 
      property Single grain1grainstructuralwt
         {
         Single get()
            {
            return Comp->Variable[ "grain1grainstructuralwt" ]->ToSingle();
            }       
         } 
      property Single grain1graintotaln
         {
         Single get()
            {
            return Comp->Variable[ "grain1graintotaln" ]->ToSingle();
            }       
         } 
      property Single grain1graintotalnconc
         {
         Single get()
            {
            return Comp->Variable[ "grain1graintotalnconc" ]->ToSingle();
            }       
         } 
      property Single grain1graintotalnonstructuralwt
         {
         Single get()
            {
            return Comp->Variable[ "grain1graintotalnonstructuralwt" ]->ToSingle();
            }       
         } 
      property Single grain1graintotalp
         {
         Single get()
            {
            return Comp->Variable[ "grain1graintotalp" ]->ToSingle();
            }       
         } 
      property Single grain1graintotalpconc
         {
         Single get()
            {
            return Comp->Variable[ "grain1graintotalpconc" ]->ToSingle();
            }       
         } 
      property Single grain1graintotalstructuralwt
         {
         Single get()
            {
            return Comp->Variable[ "grain1graintotalstructuralwt" ]->ToSingle();
            }       
         } 
      property Single grain1graintotalwt
         {
         Single get()
            {
            return Comp->Variable[ "grain1graintotalwt" ]->ToSingle();
            }       
         } 
      property Single grain1grainwt
         {
         Single get()
            {
            return Comp->Variable[ "grain1grainwt" ]->ToSingle();
            }       
         } 
      property Single grain1greenn
         {
         Single get()
            {
            return Comp->Variable[ "grain1greenn" ]->ToSingle();
            }       
         } 
      property Single grain1greennconc
         {
         Single get()
            {
            return Comp->Variable[ "grain1greennconc" ]->ToSingle();
            }       
         } 
      property Single grain1greennonstructuralwt
         {
         Single get()
            {
            return Comp->Variable[ "grain1greennonstructuralwt" ]->ToSingle();
            }       
         } 
      property Single grain1greenp
         {
         Single get()
            {
            return Comp->Variable[ "grain1greenp" ]->ToSingle();
            }       
         } 
      property Single grain1greenpconc
         {
         Single get()
            {
            return Comp->Variable[ "grain1greenpconc" ]->ToSingle();
            }       
         } 
      property Single grain1greenremovedn
         {
         Single get()
            {
            return Comp->Variable[ "grain1greenremovedn" ]->ToSingle();
            }       
         } 
      property Single grain1greenremovedp
         {
         Single get()
            {
            return Comp->Variable[ "grain1greenremovedp" ]->ToSingle();
            }       
         } 
      property Single grain1greenremovedwt
         {
         Single get()
            {
            return Comp->Variable[ "grain1greenremovedwt" ]->ToSingle();
            }       
         } 
      property Single grain1greenstructuralwt
         {
         Single get()
            {
            return Comp->Variable[ "grain1greenstructuralwt" ]->ToSingle();
            }       
         } 
      property Single grain1greenwt
         {
         Single get()
            {
            return Comp->Variable[ "grain1greenwt" ]->ToSingle();
            }       
         } 
      property Single grain1growthn
         {
         Single get()
            {
            return Comp->Variable[ "grain1growthn" ]->ToSingle();
            }       
         } 
      property Single grain1growthp
         {
         Single get()
            {
            return Comp->Variable[ "grain1growthp" ]->ToSingle();
            }       
         } 
      property Single grain1growthwt
         {
         Single get()
            {
            return Comp->Variable[ "grain1growthwt" ]->ToSingle();
            }       
         } 
      property Single grain1retranslocationn
         {
         Single get()
            {
            return Comp->Variable[ "grain1retranslocationn" ]->ToSingle();
            }       
         } 
      property Single grain1retranslocationp
         {
         Single get()
            {
            return Comp->Variable[ "grain1retranslocationp" ]->ToSingle();
            }       
         } 
      property Single grain1retranslocationwt
         {
         Single get()
            {
            return Comp->Variable[ "grain1retranslocationwt" ]->ToSingle();
            }       
         } 
      property Single grain1senescedn
         {
         Single get()
            {
            return Comp->Variable[ "grain1senescedn" ]->ToSingle();
            }       
         } 
      property Single grain1senescednconc
         {
         Single get()
            {
            return Comp->Variable[ "grain1senescednconc" ]->ToSingle();
            }       
         } 
      property Single grain1senescednonstructuralwt
         {
         Single get()
            {
            return Comp->Variable[ "grain1senescednonstructuralwt" ]->ToSingle();
            }       
         } 
      property Single grain1senescedp
         {
         Single get()
            {
            return Comp->Variable[ "grain1senescedp" ]->ToSingle();
            }       
         } 
      property Single grain1senescedpconc
         {
         Single get()
            {
            return Comp->Variable[ "grain1senescedpconc" ]->ToSingle();
            }       
         } 
      property Single grain1senescedremovedn
         {
         Single get()
            {
            return Comp->Variable[ "grain1senescedremovedn" ]->ToSingle();
            }       
         } 
      property Single grain1senescedremovedp
         {
         Single get()
            {
            return Comp->Variable[ "grain1senescedremovedp" ]->ToSingle();
            }       
         } 
      property Single grain1senescedremovedwt
         {
         Single get()
            {
            return Comp->Variable[ "grain1senescedremovedwt" ]->ToSingle();
            }       
         } 
      property Single grain1senescedstructuralwt
         {
         Single get()
            {
            return Comp->Variable[ "grain1senescedstructuralwt" ]->ToSingle();
            }       
         } 
      property Single grain1senescedwt
         {
         Single get()
            {
            return Comp->Variable[ "grain1senescedwt" ]->ToSingle();
            }       
         } 
      property Single grain1senescingn
         {
         Single get()
            {
            return Comp->Variable[ "grain1senescingn" ]->ToSingle();
            }       
         } 
      property Single grain1senescingp
         {
         Single get()
            {
            return Comp->Variable[ "grain1senescingp" ]->ToSingle();
            }       
         } 
      property Single grain1senescingwt
         {
         Single get()
            {
            return Comp->Variable[ "grain1senescingwt" ]->ToSingle();
            }       
         } 
      property Single grain1totaln
         {
         Single get()
            {
            return Comp->Variable[ "grain1totaln" ]->ToSingle();
            }       
         } 
      property Single grain1totalnconc
         {
         Single get()
            {
            return Comp->Variable[ "grain1totalnconc" ]->ToSingle();
            }       
         } 
      property Single grain1totalnonstructuralwt
         {
         Single get()
            {
            return Comp->Variable[ "grain1totalnonstructuralwt" ]->ToSingle();
            }       
         } 
      property Single grain1totalp
         {
         Single get()
            {
            return Comp->Variable[ "grain1totalp" ]->ToSingle();
            }       
         } 
      property Single grain1totalpconc
         {
         Single get()
            {
            return Comp->Variable[ "grain1totalpconc" ]->ToSingle();
            }       
         } 
      property Single grain1totalstructuralwt
         {
         Single get()
            {
            return Comp->Variable[ "grain1totalstructuralwt" ]->ToSingle();
            }       
         } 
      property Single grain1totalwt
         {
         Single get()
            {
            return Comp->Variable[ "grain1totalwt" ]->ToSingle();
            }       
         } 
      property Single grain1vegetativen
         {
         Single get()
            {
            return Comp->Variable[ "grain1vegetativen" ]->ToSingle();
            }       
         } 
      property Single grain1vegetativenconc
         {
         Single get()
            {
            return Comp->Variable[ "grain1vegetativenconc" ]->ToSingle();
            }       
         } 
      property Single grain1vegetativenonstructuralwt
         {
         Single get()
            {
            return Comp->Variable[ "grain1vegetativenonstructuralwt" ]->ToSingle();
            }       
         } 
      property Single grain1vegetativep
         {
         Single get()
            {
            return Comp->Variable[ "grain1vegetativep" ]->ToSingle();
            }       
         } 
      property Single grain1vegetativepconc
         {
         Single get()
            {
            return Comp->Variable[ "grain1vegetativepconc" ]->ToSingle();
            }       
         } 
      property Single grain1vegetativestructuralwt
         {
         Single get()
            {
            return Comp->Variable[ "grain1vegetativestructuralwt" ]->ToSingle();
            }       
         } 
      property Single grain1vegetativetotaln
         {
         Single get()
            {
            return Comp->Variable[ "grain1vegetativetotaln" ]->ToSingle();
            }       
         } 
      property Single grain1vegetativetotalnconc
         {
         Single get()
            {
            return Comp->Variable[ "grain1vegetativetotalnconc" ]->ToSingle();
            }       
         } 
      property Single grain1vegetativetotalnonstructuralwt
         {
         Single get()
            {
            return Comp->Variable[ "grain1vegetativetotalnonstructuralwt" ]->ToSingle();
            }       
         } 
      property Single grain1vegetativetotalp
         {
         Single get()
            {
            return Comp->Variable[ "grain1vegetativetotalp" ]->ToSingle();
            }       
         } 
      property Single grain1vegetativetotalpconc
         {
         Single get()
            {
            return Comp->Variable[ "grain1vegetativetotalpconc" ]->ToSingle();
            }       
         } 
      property Single grain1vegetativetotalstructuralwt
         {
         Single get()
            {
            return Comp->Variable[ "grain1vegetativetotalstructuralwt" ]->ToSingle();
            }       
         } 
      property Single grain1vegetativetotalwt
         {
         Single get()
            {
            return Comp->Variable[ "grain1vegetativetotalwt" ]->ToSingle();
            }       
         } 
      property Single grain1vegetativewt
         {
         Single get()
            {
            return Comp->Variable[ "grain1vegetativewt" ]->ToSingle();
            }       
         } 
      property Single grain_n
         {
         Single get()
            {
            return Comp->Variable[ "grain_n" ]->ToSingle();
            }       
         } 
      property Single grain_n_demand
         {
         Single get()
            {
            return Comp->Variable[ "grain_n_demand" ]->ToSingle();
            }       
         } 
      property Single grain_no
         {
         Single get()
            {
            return Comp->Variable[ "grain_no" ]->ToSingle();
            }       
         } 
      property Single grain_oil_conc
         {
         Single get()
            {
            return Comp->Variable[ "grain_oil_conc" ]->ToSingle();
            }       
         } 
      property Single grain_p
         {
         Single get()
            {
            return Comp->Variable[ "grain_p" ]->ToSingle();
            }       
         } 
      property Single grain_p_demand
         {
         Single get()
            {
            return Comp->Variable[ "grain_p_demand" ]->ToSingle();
            }       
         } 
      property Single grain_protein
         {
         Single get()
            {
            return Comp->Variable[ "grain_protein" ]->ToSingle();
            }       
         } 
      property Single grain_size
         {
         Single get()
            {
            return Comp->Variable[ "grain_size" ]->ToSingle();
            }       
         } 
      property Single grain_wt
         {
         Single get()
            {
            return Comp->Variable[ "grain_wt" ]->ToSingle();
            }       
         } 
      property Single graindetachingn
         {
         Single get()
            {
            return Comp->Variable[ "graindetachingn" ]->ToSingle();
            }       
         } 
      property Single graindetachingp
         {
         Single get()
            {
            return Comp->Variable[ "graindetachingp" ]->ToSingle();
            }       
         } 
      property Single graindetachingwt
         {
         Single get()
            {
            return Comp->Variable[ "graindetachingwt" ]->ToSingle();
            }       
         } 
      property Single graingrainn
         {
         Single get()
            {
            return Comp->Variable[ "graingrainn" ]->ToSingle();
            }       
         } 
      property Single graingrainnconc
         {
         Single get()
            {
            return Comp->Variable[ "graingrainnconc" ]->ToSingle();
            }       
         } 
      property Single graingrainnonstructuralwt
         {
         Single get()
            {
            return Comp->Variable[ "graingrainnonstructuralwt" ]->ToSingle();
            }       
         } 
      property Single graingrainp
         {
         Single get()
            {
            return Comp->Variable[ "graingrainp" ]->ToSingle();
            }       
         } 
      property Single graingrainpconc
         {
         Single get()
            {
            return Comp->Variable[ "graingrainpconc" ]->ToSingle();
            }       
         } 
      property Single graingrainstructuralwt
         {
         Single get()
            {
            return Comp->Variable[ "graingrainstructuralwt" ]->ToSingle();
            }       
         } 
      property Single graingraintotaln
         {
         Single get()
            {
            return Comp->Variable[ "graingraintotaln" ]->ToSingle();
            }       
         } 
      property Single graingraintotalnconc
         {
         Single get()
            {
            return Comp->Variable[ "graingraintotalnconc" ]->ToSingle();
            }       
         } 
      property Single graingraintotalnonstructuralwt
         {
         Single get()
            {
            return Comp->Variable[ "graingraintotalnonstructuralwt" ]->ToSingle();
            }       
         } 
      property Single graingraintotalp
         {
         Single get()
            {
            return Comp->Variable[ "graingraintotalp" ]->ToSingle();
            }       
         } 
      property Single graingraintotalpconc
         {
         Single get()
            {
            return Comp->Variable[ "graingraintotalpconc" ]->ToSingle();
            }       
         } 
      property Single graingraintotalstructuralwt
         {
         Single get()
            {
            return Comp->Variable[ "graingraintotalstructuralwt" ]->ToSingle();
            }       
         } 
      property Single graingraintotalwt
         {
         Single get()
            {
            return Comp->Variable[ "graingraintotalwt" ]->ToSingle();
            }       
         } 
      property Single graingrainwt
         {
         Single get()
            {
            return Comp->Variable[ "graingrainwt" ]->ToSingle();
            }       
         } 
      property Single graingreenn
         {
         Single get()
            {
            return Comp->Variable[ "graingreenn" ]->ToSingle();
            }       
         } 
      property Single graingreennconc
         {
         Single get()
            {
            return Comp->Variable[ "graingreennconc" ]->ToSingle();
            }       
         } 
      property Single graingreennonstructuralwt
         {
         Single get()
            {
            return Comp->Variable[ "graingreennonstructuralwt" ]->ToSingle();
            }       
         } 
      property Single graingreenp
         {
         Single get()
            {
            return Comp->Variable[ "graingreenp" ]->ToSingle();
            }       
         } 
      property Single graingreenpconc
         {
         Single get()
            {
            return Comp->Variable[ "graingreenpconc" ]->ToSingle();
            }       
         } 
      property Single graingreenremovedn
         {
         Single get()
            {
            return Comp->Variable[ "graingreenremovedn" ]->ToSingle();
            }       
         } 
      property Single graingreenremovedp
         {
         Single get()
            {
            return Comp->Variable[ "graingreenremovedp" ]->ToSingle();
            }       
         } 
      property Single graingreenremovedwt
         {
         Single get()
            {
            return Comp->Variable[ "graingreenremovedwt" ]->ToSingle();
            }       
         } 
      property Single graingreenstructuralwt
         {
         Single get()
            {
            return Comp->Variable[ "graingreenstructuralwt" ]->ToSingle();
            }       
         } 
      property Single graingreenwt
         {
         Single get()
            {
            return Comp->Variable[ "graingreenwt" ]->ToSingle();
            }       
         } 
      property Single graingrowthn
         {
         Single get()
            {
            return Comp->Variable[ "graingrowthn" ]->ToSingle();
            }       
         } 
      property Single graingrowthp
         {
         Single get()
            {
            return Comp->Variable[ "graingrowthp" ]->ToSingle();
            }       
         } 
      property Single graingrowthwt
         {
         Single get()
            {
            return Comp->Variable[ "graingrowthwt" ]->ToSingle();
            }       
         } 
      property Single grainn
         {
         Single get()
            {
            return Comp->Variable[ "grainn" ]->ToSingle();
            }       
         } 
      property Single grainnconc
         {
         Single get()
            {
            return Comp->Variable[ "grainnconc" ]->ToSingle();
            }       
         } 
      property Single grainnonstructuralwt
         {
         Single get()
            {
            return Comp->Variable[ "grainnonstructuralwt" ]->ToSingle();
            }       
         } 
      property Single grainp
         {
         Single get()
            {
            return Comp->Variable[ "grainp" ]->ToSingle();
            }       
         } 
      property Single grainpconc
         {
         Single get()
            {
            return Comp->Variable[ "grainpconc" ]->ToSingle();
            }       
         } 
      property Single grainretranslocationn
         {
         Single get()
            {
            return Comp->Variable[ "grainretranslocationn" ]->ToSingle();
            }       
         } 
      property Single grainretranslocationp
         {
         Single get()
            {
            return Comp->Variable[ "grainretranslocationp" ]->ToSingle();
            }       
         } 
      property Single grainretranslocationwt
         {
         Single get()
            {
            return Comp->Variable[ "grainretranslocationwt" ]->ToSingle();
            }       
         } 
      property Single grainsenescedn
         {
         Single get()
            {
            return Comp->Variable[ "grainsenescedn" ]->ToSingle();
            }       
         } 
      property Single grainsenescednconc
         {
         Single get()
            {
            return Comp->Variable[ "grainsenescednconc" ]->ToSingle();
            }       
         } 
      property Single grainsenescednonstructuralwt
         {
         Single get()
            {
            return Comp->Variable[ "grainsenescednonstructuralwt" ]->ToSingle();
            }       
         } 
      property Single grainsenescedp
         {
         Single get()
            {
            return Comp->Variable[ "grainsenescedp" ]->ToSingle();
            }       
         } 
      property Single grainsenescedpconc
         {
         Single get()
            {
            return Comp->Variable[ "grainsenescedpconc" ]->ToSingle();
            }       
         } 
      property Single grainsenescedremovedn
         {
         Single get()
            {
            return Comp->Variable[ "grainsenescedremovedn" ]->ToSingle();
            }       
         } 
      property Single grainsenescedremovedp
         {
         Single get()
            {
            return Comp->Variable[ "grainsenescedremovedp" ]->ToSingle();
            }       
         } 
      property Single grainsenescedremovedwt
         {
         Single get()
            {
            return Comp->Variable[ "grainsenescedremovedwt" ]->ToSingle();
            }       
         } 
      property Single grainsenescedstructuralwt
         {
         Single get()
            {
            return Comp->Variable[ "grainsenescedstructuralwt" ]->ToSingle();
            }       
         } 
      property Single grainsenescedwt
         {
         Single get()
            {
            return Comp->Variable[ "grainsenescedwt" ]->ToSingle();
            }       
         } 
      property Single grainsenescingn
         {
         Single get()
            {
            return Comp->Variable[ "grainsenescingn" ]->ToSingle();
            }       
         } 
      property Single grainsenescingp
         {
         Single get()
            {
            return Comp->Variable[ "grainsenescingp" ]->ToSingle();
            }       
         } 
      property Single grainsenescingwt
         {
         Single get()
            {
            return Comp->Variable[ "grainsenescingwt" ]->ToSingle();
            }       
         } 
      property Single grainstructuralwt
         {
         Single get()
            {
            return Comp->Variable[ "grainstructuralwt" ]->ToSingle();
            }       
         } 
      property Single graintotaln
         {
         Single get()
            {
            return Comp->Variable[ "graintotaln" ]->ToSingle();
            }       
         } 
      property Single graintotalnconc
         {
         Single get()
            {
            return Comp->Variable[ "graintotalnconc" ]->ToSingle();
            }       
         } 
      property Single graintotalnonstructuralwt
         {
         Single get()
            {
            return Comp->Variable[ "graintotalnonstructuralwt" ]->ToSingle();
            }       
         } 
      property Single graintotalp
         {
         Single get()
            {
            return Comp->Variable[ "graintotalp" ]->ToSingle();
            }       
         } 
      property Single graintotalpconc
         {
         Single get()
            {
            return Comp->Variable[ "graintotalpconc" ]->ToSingle();
            }       
         } 
      property Single graintotalstructuralwt
         {
         Single get()
            {
            return Comp->Variable[ "graintotalstructuralwt" ]->ToSingle();
            }       
         } 
      property Single graintotalwt
         {
         Single get()
            {
            return Comp->Variable[ "graintotalwt" ]->ToSingle();
            }       
         } 
      property Single grainvegetativen
         {
         Single get()
            {
            return Comp->Variable[ "grainvegetativen" ]->ToSingle();
            }       
         } 
      property Single grainvegetativenconc
         {
         Single get()
            {
            return Comp->Variable[ "grainvegetativenconc" ]->ToSingle();
            }       
         } 
      property Single grainvegetativenonstructuralwt
         {
         Single get()
            {
            return Comp->Variable[ "grainvegetativenonstructuralwt" ]->ToSingle();
            }       
         } 
      property Single grainvegetativep
         {
         Single get()
            {
            return Comp->Variable[ "grainvegetativep" ]->ToSingle();
            }       
         } 
      property Single grainvegetativepconc
         {
         Single get()
            {
            return Comp->Variable[ "grainvegetativepconc" ]->ToSingle();
            }       
         } 
      property Single grainvegetativestructuralwt
         {
         Single get()
            {
            return Comp->Variable[ "grainvegetativestructuralwt" ]->ToSingle();
            }       
         } 
      property Single grainvegetativetotaln
         {
         Single get()
            {
            return Comp->Variable[ "grainvegetativetotaln" ]->ToSingle();
            }       
         } 
      property Single grainvegetativetotalnconc
         {
         Single get()
            {
            return Comp->Variable[ "grainvegetativetotalnconc" ]->ToSingle();
            }       
         } 
      property Single grainvegetativetotalnonstructuralwt
         {
         Single get()
            {
            return Comp->Variable[ "grainvegetativetotalnonstructuralwt" ]->ToSingle();
            }       
         } 
      property Single grainvegetativetotalp
         {
         Single get()
            {
            return Comp->Variable[ "grainvegetativetotalp" ]->ToSingle();
            }       
         } 
      property Single grainvegetativetotalpconc
         {
         Single get()
            {
            return Comp->Variable[ "grainvegetativetotalpconc" ]->ToSingle();
            }       
         } 
      property Single grainvegetativetotalstructuralwt
         {
         Single get()
            {
            return Comp->Variable[ "grainvegetativetotalstructuralwt" ]->ToSingle();
            }       
         } 
      property Single grainvegetativetotalwt
         {
         Single get()
            {
            return Comp->Variable[ "grainvegetativetotalwt" ]->ToSingle();
            }       
         } 
      property Single grainvegetativewt
         {
         Single get()
            {
            return Comp->Variable[ "grainvegetativewt" ]->ToSingle();
            }       
         } 
      property Single grainwt
         {
         Single get()
            {
            return Comp->Variable[ "grainwt" ]->ToSingle();
            }       
         } 
      property Single green_biomass
         {
         Single get()
            {
            return Comp->Variable[ "green_biomass" ]->ToSingle();
            }       
         } 
      property Single green_biomass_n
         {
         Single get()
            {
            return Comp->Variable[ "green_biomass_n" ]->ToSingle();
            }       
         } 
      property Single green_biomass_p
         {
         Single get()
            {
            return Comp->Variable[ "green_biomass_p" ]->ToSingle();
            }       
         } 
      property Single green_biomass_wt
         {
         Single get()
            {
            return Comp->Variable[ "green_biomass_wt" ]->ToSingle();
            }       
         } 
      property Single greenn
         {
         Single get()
            {
            return Comp->Variable[ "greenn" ]->ToSingle();
            }       
         } 
      property Single greennconc
         {
         Single get()
            {
            return Comp->Variable[ "greennconc" ]->ToSingle();
            }       
         } 
      property Single greennonstructuralwt
         {
         Single get()
            {
            return Comp->Variable[ "greennonstructuralwt" ]->ToSingle();
            }       
         } 
      property Single greenp
         {
         Single get()
            {
            return Comp->Variable[ "greenp" ]->ToSingle();
            }       
         } 
      property Single greenpconc
         {
         Single get()
            {
            return Comp->Variable[ "greenpconc" ]->ToSingle();
            }       
         } 
      property Single greenremovedn
         {
         Single get()
            {
            return Comp->Variable[ "greenremovedn" ]->ToSingle();
            }       
         } 
      property Single greenremovedp
         {
         Single get()
            {
            return Comp->Variable[ "greenremovedp" ]->ToSingle();
            }       
         } 
      property Single greenremovedwt
         {
         Single get()
            {
            return Comp->Variable[ "greenremovedwt" ]->ToSingle();
            }       
         } 
      property Single greenstructuralwt
         {
         Single get()
            {
            return Comp->Variable[ "greenstructuralwt" ]->ToSingle();
            }       
         } 
      property Single greenwt
         {
         Single get()
            {
            return Comp->Variable[ "greenwt" ]->ToSingle();
            }       
         } 
      property Single growthn
         {
         Single get()
            {
            return Comp->Variable[ "growthn" ]->ToSingle();
            }       
         } 
      property Single growthp
         {
         Single get()
            {
            return Comp->Variable[ "growthp" ]->ToSingle();
            }       
         } 
      property Single growthwt
         {
         Single get()
            {
            return Comp->Variable[ "growthwt" ]->ToSingle();
            }       
         } 
      property Single harvest_ripetttarget
         {
         Single get()
            {
            return Comp->Variable[ "harvest_ripetttarget" ]->ToSingle();
            }       
         } 
      property Single headdetachingn
         {
         Single get()
            {
            return Comp->Variable[ "headdetachingn" ]->ToSingle();
            }       
         } 
      property Single headdetachingp
         {
         Single get()
            {
            return Comp->Variable[ "headdetachingp" ]->ToSingle();
            }       
         } 
      property Single headdetachingwt
         {
         Single get()
            {
            return Comp->Variable[ "headdetachingwt" ]->ToSingle();
            }       
         } 
      property Single headgrainn
         {
         Single get()
            {
            return Comp->Variable[ "headgrainn" ]->ToSingle();
            }       
         } 
      property Single headgrainnconc
         {
         Single get()
            {
            return Comp->Variable[ "headgrainnconc" ]->ToSingle();
            }       
         } 
      property Single headgrainnonstructuralwt
         {
         Single get()
            {
            return Comp->Variable[ "headgrainnonstructuralwt" ]->ToSingle();
            }       
         } 
      property Single headgrainp
         {
         Single get()
            {
            return Comp->Variable[ "headgrainp" ]->ToSingle();
            }       
         } 
      property Single headgrainpconc
         {
         Single get()
            {
            return Comp->Variable[ "headgrainpconc" ]->ToSingle();
            }       
         } 
      property Single headgrainstructuralwt
         {
         Single get()
            {
            return Comp->Variable[ "headgrainstructuralwt" ]->ToSingle();
            }       
         } 
      property Single headgraintotaln
         {
         Single get()
            {
            return Comp->Variable[ "headgraintotaln" ]->ToSingle();
            }       
         } 
      property Single headgraintotalnconc
         {
         Single get()
            {
            return Comp->Variable[ "headgraintotalnconc" ]->ToSingle();
            }       
         } 
      property Single headgraintotalnonstructuralwt
         {
         Single get()
            {
            return Comp->Variable[ "headgraintotalnonstructuralwt" ]->ToSingle();
            }       
         } 
      property Single headgraintotalp
         {
         Single get()
            {
            return Comp->Variable[ "headgraintotalp" ]->ToSingle();
            }       
         } 
      property Single headgraintotalpconc
         {
         Single get()
            {
            return Comp->Variable[ "headgraintotalpconc" ]->ToSingle();
            }       
         } 
      property Single headgraintotalstructuralwt
         {
         Single get()
            {
            return Comp->Variable[ "headgraintotalstructuralwt" ]->ToSingle();
            }       
         } 
      property Single headgraintotalwt
         {
         Single get()
            {
            return Comp->Variable[ "headgraintotalwt" ]->ToSingle();
            }       
         } 
      property Single headgrainwt
         {
         Single get()
            {
            return Comp->Variable[ "headgrainwt" ]->ToSingle();
            }       
         } 
      property Single headgreenn
         {
         Single get()
            {
            return Comp->Variable[ "headgreenn" ]->ToSingle();
            }       
         } 
      property Single headgreennconc
         {
         Single get()
            {
            return Comp->Variable[ "headgreennconc" ]->ToSingle();
            }       
         } 
      property Single headgreennonstructuralwt
         {
         Single get()
            {
            return Comp->Variable[ "headgreennonstructuralwt" ]->ToSingle();
            }       
         } 
      property Single headgreenp
         {
         Single get()
            {
            return Comp->Variable[ "headgreenp" ]->ToSingle();
            }       
         } 
      property Single headgreenpconc
         {
         Single get()
            {
            return Comp->Variable[ "headgreenpconc" ]->ToSingle();
            }       
         } 
      property Single headgreenremovedn
         {
         Single get()
            {
            return Comp->Variable[ "headgreenremovedn" ]->ToSingle();
            }       
         } 
      property Single headgreenremovedp
         {
         Single get()
            {
            return Comp->Variable[ "headgreenremovedp" ]->ToSingle();
            }       
         } 
      property Single headgreenremovedwt
         {
         Single get()
            {
            return Comp->Variable[ "headgreenremovedwt" ]->ToSingle();
            }       
         } 
      property Single headgreenstructuralwt
         {
         Single get()
            {
            return Comp->Variable[ "headgreenstructuralwt" ]->ToSingle();
            }       
         } 
      property Single headgreenwt
         {
         Single get()
            {
            return Comp->Variable[ "headgreenwt" ]->ToSingle();
            }       
         } 
      property Single headgrowthn
         {
         Single get()
            {
            return Comp->Variable[ "headgrowthn" ]->ToSingle();
            }       
         } 
      property Single headgrowthp
         {
         Single get()
            {
            return Comp->Variable[ "headgrowthp" ]->ToSingle();
            }       
         } 
      property Single headgrowthwt
         {
         Single get()
            {
            return Comp->Variable[ "headgrowthwt" ]->ToSingle();
            }       
         } 
      property Single headretranslocationn
         {
         Single get()
            {
            return Comp->Variable[ "headretranslocationn" ]->ToSingle();
            }       
         } 
      property Single headretranslocationp
         {
         Single get()
            {
            return Comp->Variable[ "headretranslocationp" ]->ToSingle();
            }       
         } 
      property Single headretranslocationwt
         {
         Single get()
            {
            return Comp->Variable[ "headretranslocationwt" ]->ToSingle();
            }       
         } 
      property Single headsenescedn
         {
         Single get()
            {
            return Comp->Variable[ "headsenescedn" ]->ToSingle();
            }       
         } 
      property Single headsenescednconc
         {
         Single get()
            {
            return Comp->Variable[ "headsenescednconc" ]->ToSingle();
            }       
         } 
      property Single headsenescednonstructuralwt
         {
         Single get()
            {
            return Comp->Variable[ "headsenescednonstructuralwt" ]->ToSingle();
            }       
         } 
      property Single headsenescedp
         {
         Single get()
            {
            return Comp->Variable[ "headsenescedp" ]->ToSingle();
            }       
         } 
      property Single headsenescedpconc
         {
         Single get()
            {
            return Comp->Variable[ "headsenescedpconc" ]->ToSingle();
            }       
         } 
      property Single headsenescedremovedn
         {
         Single get()
            {
            return Comp->Variable[ "headsenescedremovedn" ]->ToSingle();
            }       
         } 
      property Single headsenescedremovedp
         {
         Single get()
            {
            return Comp->Variable[ "headsenescedremovedp" ]->ToSingle();
            }       
         } 
      property Single headsenescedremovedwt
         {
         Single get()
            {
            return Comp->Variable[ "headsenescedremovedwt" ]->ToSingle();
            }       
         } 
      property Single headsenescedstructuralwt
         {
         Single get()
            {
            return Comp->Variable[ "headsenescedstructuralwt" ]->ToSingle();
            }       
         } 
      property Single headsenescedwt
         {
         Single get()
            {
            return Comp->Variable[ "headsenescedwt" ]->ToSingle();
            }       
         } 
      property Single headsenescingn
         {
         Single get()
            {
            return Comp->Variable[ "headsenescingn" ]->ToSingle();
            }       
         } 
      property Single headsenescingp
         {
         Single get()
            {
            return Comp->Variable[ "headsenescingp" ]->ToSingle();
            }       
         } 
      property Single headsenescingwt
         {
         Single get()
            {
            return Comp->Variable[ "headsenescingwt" ]->ToSingle();
            }       
         } 
      property Single headtotaln
         {
         Single get()
            {
            return Comp->Variable[ "headtotaln" ]->ToSingle();
            }       
         } 
      property Single headtotalnconc
         {
         Single get()
            {
            return Comp->Variable[ "headtotalnconc" ]->ToSingle();
            }       
         } 
      property Single headtotalnonstructuralwt
         {
         Single get()
            {
            return Comp->Variable[ "headtotalnonstructuralwt" ]->ToSingle();
            }       
         } 
      property Single headtotalp
         {
         Single get()
            {
            return Comp->Variable[ "headtotalp" ]->ToSingle();
            }       
         } 
      property Single headtotalpconc
         {
         Single get()
            {
            return Comp->Variable[ "headtotalpconc" ]->ToSingle();
            }       
         } 
      property Single headtotalstructuralwt
         {
         Single get()
            {
            return Comp->Variable[ "headtotalstructuralwt" ]->ToSingle();
            }       
         } 
      property Single headtotalwt
         {
         Single get()
            {
            return Comp->Variable[ "headtotalwt" ]->ToSingle();
            }       
         } 
      property Single headvegetativen
         {
         Single get()
            {
            return Comp->Variable[ "headvegetativen" ]->ToSingle();
            }       
         } 
      property Single headvegetativenconc
         {
         Single get()
            {
            return Comp->Variable[ "headvegetativenconc" ]->ToSingle();
            }       
         } 
      property Single headvegetativenonstructuralwt
         {
         Single get()
            {
            return Comp->Variable[ "headvegetativenonstructuralwt" ]->ToSingle();
            }       
         } 
      property Single headvegetativep
         {
         Single get()
            {
            return Comp->Variable[ "headvegetativep" ]->ToSingle();
            }       
         } 
      property Single headvegetativepconc
         {
         Single get()
            {
            return Comp->Variable[ "headvegetativepconc" ]->ToSingle();
            }       
         } 
      property Single headvegetativestructuralwt
         {
         Single get()
            {
            return Comp->Variable[ "headvegetativestructuralwt" ]->ToSingle();
            }       
         } 
      property Single headvegetativetotaln
         {
         Single get()
            {
            return Comp->Variable[ "headvegetativetotaln" ]->ToSingle();
            }       
         } 
      property Single headvegetativetotalnconc
         {
         Single get()
            {
            return Comp->Variable[ "headvegetativetotalnconc" ]->ToSingle();
            }       
         } 
      property Single headvegetativetotalnonstructuralwt
         {
         Single get()
            {
            return Comp->Variable[ "headvegetativetotalnonstructuralwt" ]->ToSingle();
            }       
         } 
      property Single headvegetativetotalp
         {
         Single get()
            {
            return Comp->Variable[ "headvegetativetotalp" ]->ToSingle();
            }       
         } 
      property Single headvegetativetotalpconc
         {
         Single get()
            {
            return Comp->Variable[ "headvegetativetotalpconc" ]->ToSingle();
            }       
         } 
      property Single headvegetativetotalstructuralwt
         {
         Single get()
            {
            return Comp->Variable[ "headvegetativetotalstructuralwt" ]->ToSingle();
            }       
         } 
      property Single headvegetativetotalwt
         {
         Single get()
            {
            return Comp->Variable[ "headvegetativetotalwt" ]->ToSingle();
            }       
         } 
      property Single headvegetativewt
         {
         Single get()
            {
            return Comp->Variable[ "headvegetativewt" ]->ToSingle();
            }       
         } 
      property Single height
         {
         Single get()
            {
            return Comp->Variable[ "height" ]->ToSingle();
            }       
         } 
      property array<Single>^ kl
         {
         array<Single>^ get()
            {
            return Comp->Variable[ "kl" ]->ToSingleArray();
            }       
         } 
      property Single lai
         {
         Single get()
            {
            return Comp->Variable[ "lai" ]->ToSingle();
            }       
         } 
      property Single lai_sum
         {
         Single get()
            {
            return Comp->Variable[ "lai_sum" ]->ToSingle();
            }       
         } 
      property array<Single>^ leaf_area
         {
         array<Single>^ get()
            {
            return Comp->Variable[ "leaf_area" ]->ToSingleArray();
            }       
         } 
      property Single leaf_area_tot
         {
         Single get()
            {
            return Comp->Variable[ "leaf_area_tot" ]->ToSingle();
            }       
         } 
      property Single leaf_no
         {
         Single get()
            {
            return Comp->Variable[ "leaf_no" ]->ToSingle();
            }       
         } 
      property Single leaf_no_sen
         {
         Single get()
            {
            return Comp->Variable[ "leaf_no_sen" ]->ToSingle();
            }       
         } 
      property Single leafdetachingn
         {
         Single get()
            {
            return Comp->Variable[ "leafdetachingn" ]->ToSingle();
            }       
         } 
      property Single leafdetachingp
         {
         Single get()
            {
            return Comp->Variable[ "leafdetachingp" ]->ToSingle();
            }       
         } 
      property Single leafdetachingwt
         {
         Single get()
            {
            return Comp->Variable[ "leafdetachingwt" ]->ToSingle();
            }       
         } 
      property Single leafgreendigestibilityavg
         {
         Single get()
            {
            return Comp->Variable[ "leafgreendigestibilityavg" ]->ToSingle();
            }       
         } 
      property Single leafgreendigestibilitymax
         {
         Single get()
            {
            return Comp->Variable[ "leafgreendigestibilitymax" ]->ToSingle();
            }       
         } 
      property Single leafgreendigestibilitymin
         {
         Single get()
            {
            return Comp->Variable[ "leafgreendigestibilitymin" ]->ToSingle();
            }       
         } 
      property Single leafgreenn
         {
         Single get()
            {
            return Comp->Variable[ "leafgreenn" ]->ToSingle();
            }       
         } 
      property Single leafgreennconc
         {
         Single get()
            {
            return Comp->Variable[ "leafgreennconc" ]->ToSingle();
            }       
         } 
      property Single leafgreennonstructuralwt
         {
         Single get()
            {
            return Comp->Variable[ "leafgreennonstructuralwt" ]->ToSingle();
            }       
         } 
      property Single leafgreenp
         {
         Single get()
            {
            return Comp->Variable[ "leafgreenp" ]->ToSingle();
            }       
         } 
      property Single leafgreenpconc
         {
         Single get()
            {
            return Comp->Variable[ "leafgreenpconc" ]->ToSingle();
            }       
         } 
      property Single leafgreenremovedn
         {
         Single get()
            {
            return Comp->Variable[ "leafgreenremovedn" ]->ToSingle();
            }       
         } 
      property Single leafgreenremovedp
         {
         Single get()
            {
            return Comp->Variable[ "leafgreenremovedp" ]->ToSingle();
            }       
         } 
      property Single leafgreenremovedwt
         {
         Single get()
            {
            return Comp->Variable[ "leafgreenremovedwt" ]->ToSingle();
            }       
         } 
      property Single leafgreenstructuralwt
         {
         Single get()
            {
            return Comp->Variable[ "leafgreenstructuralwt" ]->ToSingle();
            }       
         } 
      property Single leafgreenwt
         {
         Single get()
            {
            return Comp->Variable[ "leafgreenwt" ]->ToSingle();
            }       
         } 
      property Single leafgrowthn
         {
         Single get()
            {
            return Comp->Variable[ "leafgrowthn" ]->ToSingle();
            }       
         } 
      property Single leafgrowthp
         {
         Single get()
            {
            return Comp->Variable[ "leafgrowthp" ]->ToSingle();
            }       
         } 
      property Single leafgrowthwt
         {
         Single get()
            {
            return Comp->Variable[ "leafgrowthwt" ]->ToSingle();
            }       
         } 
      property Single leafretranslocationn
         {
         Single get()
            {
            return Comp->Variable[ "leafretranslocationn" ]->ToSingle();
            }       
         } 
      property Single leafretranslocationp
         {
         Single get()
            {
            return Comp->Variable[ "leafretranslocationp" ]->ToSingle();
            }       
         } 
      property Single leafretranslocationwt
         {
         Single get()
            {
            return Comp->Variable[ "leafretranslocationwt" ]->ToSingle();
            }       
         } 
      property Single leafsenesceddigestibilityavg
         {
         Single get()
            {
            return Comp->Variable[ "leafsenesceddigestibilityavg" ]->ToSingle();
            }       
         } 
      property Single leafsenesceddigestibilitymax
         {
         Single get()
            {
            return Comp->Variable[ "leafsenesceddigestibilitymax" ]->ToSingle();
            }       
         } 
      property Single leafsenesceddigestibilitymin
         {
         Single get()
            {
            return Comp->Variable[ "leafsenesceddigestibilitymin" ]->ToSingle();
            }       
         } 
      property Single leafsenescedn
         {
         Single get()
            {
            return Comp->Variable[ "leafsenescedn" ]->ToSingle();
            }       
         } 
      property Single leafsenescednconc
         {
         Single get()
            {
            return Comp->Variable[ "leafsenescednconc" ]->ToSingle();
            }       
         } 
      property Single leafsenescednonstructuralwt
         {
         Single get()
            {
            return Comp->Variable[ "leafsenescednonstructuralwt" ]->ToSingle();
            }       
         } 
      property Single leafsenescedp
         {
         Single get()
            {
            return Comp->Variable[ "leafsenescedp" ]->ToSingle();
            }       
         } 
      property Single leafsenescedpconc
         {
         Single get()
            {
            return Comp->Variable[ "leafsenescedpconc" ]->ToSingle();
            }       
         } 
      property Single leafsenescedremovedn
         {
         Single get()
            {
            return Comp->Variable[ "leafsenescedremovedn" ]->ToSingle();
            }       
         } 
      property Single leafsenescedremovedp
         {
         Single get()
            {
            return Comp->Variable[ "leafsenescedremovedp" ]->ToSingle();
            }       
         } 
      property Single leafsenescedremovedwt
         {
         Single get()
            {
            return Comp->Variable[ "leafsenescedremovedwt" ]->ToSingle();
            }       
         } 
      property Single leafsenescedstructuralwt
         {
         Single get()
            {
            return Comp->Variable[ "leafsenescedstructuralwt" ]->ToSingle();
            }       
         } 
      property Single leafsenescedwt
         {
         Single get()
            {
            return Comp->Variable[ "leafsenescedwt" ]->ToSingle();
            }       
         } 
      property Single leafsenescingn
         {
         Single get()
            {
            return Comp->Variable[ "leafsenescingn" ]->ToSingle();
            }       
         } 
      property Single leafsenescingp
         {
         Single get()
            {
            return Comp->Variable[ "leafsenescingp" ]->ToSingle();
            }       
         } 
      property Single leafsenescingwt
         {
         Single get()
            {
            return Comp->Variable[ "leafsenescingwt" ]->ToSingle();
            }       
         } 
      property Single leaftotaln
         {
         Single get()
            {
            return Comp->Variable[ "leaftotaln" ]->ToSingle();
            }       
         } 
      property Single leaftotalnconc
         {
         Single get()
            {
            return Comp->Variable[ "leaftotalnconc" ]->ToSingle();
            }       
         } 
      property Single leaftotalnonstructuralwt
         {
         Single get()
            {
            return Comp->Variable[ "leaftotalnonstructuralwt" ]->ToSingle();
            }       
         } 
      property Single leaftotalp
         {
         Single get()
            {
            return Comp->Variable[ "leaftotalp" ]->ToSingle();
            }       
         } 
      property Single leaftotalpconc
         {
         Single get()
            {
            return Comp->Variable[ "leaftotalpconc" ]->ToSingle();
            }       
         } 
      property Single leaftotalstructuralwt
         {
         Single get()
            {
            return Comp->Variable[ "leaftotalstructuralwt" ]->ToSingle();
            }       
         } 
      property Single leaftotalwt
         {
         Single get()
            {
            return Comp->Variable[ "leaftotalwt" ]->ToSingle();
            }       
         } 
      property Single leafvegetativen
         {
         Single get()
            {
            return Comp->Variable[ "leafvegetativen" ]->ToSingle();
            }       
         } 
      property Single leafvegetativenconc
         {
         Single get()
            {
            return Comp->Variable[ "leafvegetativenconc" ]->ToSingle();
            }       
         } 
      property Single leafvegetativenonstructuralwt
         {
         Single get()
            {
            return Comp->Variable[ "leafvegetativenonstructuralwt" ]->ToSingle();
            }       
         } 
      property Single leafvegetativep
         {
         Single get()
            {
            return Comp->Variable[ "leafvegetativep" ]->ToSingle();
            }       
         } 
      property Single leafvegetativepconc
         {
         Single get()
            {
            return Comp->Variable[ "leafvegetativepconc" ]->ToSingle();
            }       
         } 
      property Single leafvegetativestructuralwt
         {
         Single get()
            {
            return Comp->Variable[ "leafvegetativestructuralwt" ]->ToSingle();
            }       
         } 
      property Single leafvegetativetotaln
         {
         Single get()
            {
            return Comp->Variable[ "leafvegetativetotaln" ]->ToSingle();
            }       
         } 
      property Single leafvegetativetotalnconc
         {
         Single get()
            {
            return Comp->Variable[ "leafvegetativetotalnconc" ]->ToSingle();
            }       
         } 
      property Single leafvegetativetotalnonstructuralwt
         {
         Single get()
            {
            return Comp->Variable[ "leafvegetativetotalnonstructuralwt" ]->ToSingle();
            }       
         } 
      property Single leafvegetativetotalp
         {
         Single get()
            {
            return Comp->Variable[ "leafvegetativetotalp" ]->ToSingle();
            }       
         } 
      property Single leafvegetativetotalpconc
         {
         Single get()
            {
            return Comp->Variable[ "leafvegetativetotalpconc" ]->ToSingle();
            }       
         } 
      property Single leafvegetativetotalstructuralwt
         {
         Single get()
            {
            return Comp->Variable[ "leafvegetativetotalstructuralwt" ]->ToSingle();
            }       
         } 
      property Single leafvegetativetotalwt
         {
         Single get()
            {
            return Comp->Variable[ "leafvegetativetotalwt" ]->ToSingle();
            }       
         } 
      property Single leafvegetativewt
         {
         Single get()
            {
            return Comp->Variable[ "leafvegetativewt" ]->ToSingle();
            }       
         } 
      property Single leaves_per_node
         {
         Single get()
            {
            return Comp->Variable[ "leaves_per_node" ]->ToSingle();
            }       
         } 
      property array<Single>^ ll
         {
         array<Single>^ get()
            {
            return Comp->Variable[ "ll" ]->ToSingleArray();
            }       
         } 
      property array<Single>^ ll_dep
         {
         array<Single>^ get()
            {
            return Comp->Variable[ "ll_dep" ]->ToSingleArray();
            }       
         } 
      property Int32 maturity_das
         {
         Int32 get()
            {
            return Comp->Variable[ "maturity_das" ]->ToInt32();
            }       
         } 
      property Int32 maturity_date
         {
         Int32 get()
            {
            return Comp->Variable[ "maturity_date" ]->ToInt32();
            }       
         } 
      property Single maturitytttarget
         {
         Single get()
            {
            return Comp->Variable[ "maturitytttarget" ]->ToSingle();
            }       
         } 
      property Single mealdetachingn
         {
         Single get()
            {
            return Comp->Variable[ "mealdetachingn" ]->ToSingle();
            }       
         } 
      property Single mealdetachingp
         {
         Single get()
            {
            return Comp->Variable[ "mealdetachingp" ]->ToSingle();
            }       
         } 
      property Single mealdetachingwt
         {
         Single get()
            {
            return Comp->Variable[ "mealdetachingwt" ]->ToSingle();
            }       
         } 
      property Single mealgrainn
         {
         Single get()
            {
            return Comp->Variable[ "mealgrainn" ]->ToSingle();
            }       
         } 
      property Single mealgrainnconc
         {
         Single get()
            {
            return Comp->Variable[ "mealgrainnconc" ]->ToSingle();
            }       
         } 
      property Single mealgrainnonstructuralwt
         {
         Single get()
            {
            return Comp->Variable[ "mealgrainnonstructuralwt" ]->ToSingle();
            }       
         } 
      property Single mealgrainp
         {
         Single get()
            {
            return Comp->Variable[ "mealgrainp" ]->ToSingle();
            }       
         } 
      property Single mealgrainpconc
         {
         Single get()
            {
            return Comp->Variable[ "mealgrainpconc" ]->ToSingle();
            }       
         } 
      property Single mealgrainstructuralwt
         {
         Single get()
            {
            return Comp->Variable[ "mealgrainstructuralwt" ]->ToSingle();
            }       
         } 
      property Single mealgraintotaln
         {
         Single get()
            {
            return Comp->Variable[ "mealgraintotaln" ]->ToSingle();
            }       
         } 
      property Single mealgraintotalnconc
         {
         Single get()
            {
            return Comp->Variable[ "mealgraintotalnconc" ]->ToSingle();
            }       
         } 
      property Single mealgraintotalnonstructuralwt
         {
         Single get()
            {
            return Comp->Variable[ "mealgraintotalnonstructuralwt" ]->ToSingle();
            }       
         } 
      property Single mealgraintotalp
         {
         Single get()
            {
            return Comp->Variable[ "mealgraintotalp" ]->ToSingle();
            }       
         } 
      property Single mealgraintotalpconc
         {
         Single get()
            {
            return Comp->Variable[ "mealgraintotalpconc" ]->ToSingle();
            }       
         } 
      property Single mealgraintotalstructuralwt
         {
         Single get()
            {
            return Comp->Variable[ "mealgraintotalstructuralwt" ]->ToSingle();
            }       
         } 
      property Single mealgraintotalwt
         {
         Single get()
            {
            return Comp->Variable[ "mealgraintotalwt" ]->ToSingle();
            }       
         } 
      property Single mealgrainwt
         {
         Single get()
            {
            return Comp->Variable[ "mealgrainwt" ]->ToSingle();
            }       
         } 
      property Single mealgreendigestibilityavg
         {
         Single get()
            {
            return Comp->Variable[ "mealgreendigestibilityavg" ]->ToSingle();
            }       
         } 
      property Single mealgreendigestibilitymax
         {
         Single get()
            {
            return Comp->Variable[ "mealgreendigestibilitymax" ]->ToSingle();
            }       
         } 
      property Single mealgreendigestibilitymin
         {
         Single get()
            {
            return Comp->Variable[ "mealgreendigestibilitymin" ]->ToSingle();
            }       
         } 
      property Single mealgreenn
         {
         Single get()
            {
            return Comp->Variable[ "mealgreenn" ]->ToSingle();
            }       
         } 
      property Single mealgreennconc
         {
         Single get()
            {
            return Comp->Variable[ "mealgreennconc" ]->ToSingle();
            }       
         } 
      property Single mealgreennonstructuralwt
         {
         Single get()
            {
            return Comp->Variable[ "mealgreennonstructuralwt" ]->ToSingle();
            }       
         } 
      property Single mealgreenp
         {
         Single get()
            {
            return Comp->Variable[ "mealgreenp" ]->ToSingle();
            }       
         } 
      property Single mealgreenpconc
         {
         Single get()
            {
            return Comp->Variable[ "mealgreenpconc" ]->ToSingle();
            }       
         } 
      property Single mealgreenremovedn
         {
         Single get()
            {
            return Comp->Variable[ "mealgreenremovedn" ]->ToSingle();
            }       
         } 
      property Single mealgreenremovedp
         {
         Single get()
            {
            return Comp->Variable[ "mealgreenremovedp" ]->ToSingle();
            }       
         } 
      property Single mealgreenremovedwt
         {
         Single get()
            {
            return Comp->Variable[ "mealgreenremovedwt" ]->ToSingle();
            }       
         } 
      property Single mealgreenstructuralwt
         {
         Single get()
            {
            return Comp->Variable[ "mealgreenstructuralwt" ]->ToSingle();
            }       
         } 
      property Single mealgreenwt
         {
         Single get()
            {
            return Comp->Variable[ "mealgreenwt" ]->ToSingle();
            }       
         } 
      property Single mealgrowthn
         {
         Single get()
            {
            return Comp->Variable[ "mealgrowthn" ]->ToSingle();
            }       
         } 
      property Single mealgrowthp
         {
         Single get()
            {
            return Comp->Variable[ "mealgrowthp" ]->ToSingle();
            }       
         } 
      property Single mealgrowthwt
         {
         Single get()
            {
            return Comp->Variable[ "mealgrowthwt" ]->ToSingle();
            }       
         } 
      property Single mealretranslocationn
         {
         Single get()
            {
            return Comp->Variable[ "mealretranslocationn" ]->ToSingle();
            }       
         } 
      property Single mealretranslocationp
         {
         Single get()
            {
            return Comp->Variable[ "mealretranslocationp" ]->ToSingle();
            }       
         } 
      property Single mealretranslocationwt
         {
         Single get()
            {
            return Comp->Variable[ "mealretranslocationwt" ]->ToSingle();
            }       
         } 
      property Single mealsenesceddigestibilityavg
         {
         Single get()
            {
            return Comp->Variable[ "mealsenesceddigestibilityavg" ]->ToSingle();
            }       
         } 
      property Single mealsenesceddigestibilitymax
         {
         Single get()
            {
            return Comp->Variable[ "mealsenesceddigestibilitymax" ]->ToSingle();
            }       
         } 
      property Single mealsenesceddigestibilitymin
         {
         Single get()
            {
            return Comp->Variable[ "mealsenesceddigestibilitymin" ]->ToSingle();
            }       
         } 
      property Single mealsenescedn
         {
         Single get()
            {
            return Comp->Variable[ "mealsenescedn" ]->ToSingle();
            }       
         } 
      property Single mealsenescednconc
         {
         Single get()
            {
            return Comp->Variable[ "mealsenescednconc" ]->ToSingle();
            }       
         } 
      property Single mealsenescednonstructuralwt
         {
         Single get()
            {
            return Comp->Variable[ "mealsenescednonstructuralwt" ]->ToSingle();
            }       
         } 
      property Single mealsenescedp
         {
         Single get()
            {
            return Comp->Variable[ "mealsenescedp" ]->ToSingle();
            }       
         } 
      property Single mealsenescedpconc
         {
         Single get()
            {
            return Comp->Variable[ "mealsenescedpconc" ]->ToSingle();
            }       
         } 
      property Single mealsenescedremovedn
         {
         Single get()
            {
            return Comp->Variable[ "mealsenescedremovedn" ]->ToSingle();
            }       
         } 
      property Single mealsenescedremovedp
         {
         Single get()
            {
            return Comp->Variable[ "mealsenescedremovedp" ]->ToSingle();
            }       
         } 
      property Single mealsenescedremovedwt
         {
         Single get()
            {
            return Comp->Variable[ "mealsenescedremovedwt" ]->ToSingle();
            }       
         } 
      property Single mealsenescedstructuralwt
         {
         Single get()
            {
            return Comp->Variable[ "mealsenescedstructuralwt" ]->ToSingle();
            }       
         } 
      property Single mealsenescedwt
         {
         Single get()
            {
            return Comp->Variable[ "mealsenescedwt" ]->ToSingle();
            }       
         } 
      property Single mealsenescingn
         {
         Single get()
            {
            return Comp->Variable[ "mealsenescingn" ]->ToSingle();
            }       
         } 
      property Single mealsenescingp
         {
         Single get()
            {
            return Comp->Variable[ "mealsenescingp" ]->ToSingle();
            }       
         } 
      property Single mealsenescingwt
         {
         Single get()
            {
            return Comp->Variable[ "mealsenescingwt" ]->ToSingle();
            }       
         } 
      property Single mealtotaln
         {
         Single get()
            {
            return Comp->Variable[ "mealtotaln" ]->ToSingle();
            }       
         } 
      property Single mealtotalnconc
         {
         Single get()
            {
            return Comp->Variable[ "mealtotalnconc" ]->ToSingle();
            }       
         } 
      property Single mealtotalnonstructuralwt
         {
         Single get()
            {
            return Comp->Variable[ "mealtotalnonstructuralwt" ]->ToSingle();
            }       
         } 
      property Single mealtotalp
         {
         Single get()
            {
            return Comp->Variable[ "mealtotalp" ]->ToSingle();
            }       
         } 
      property Single mealtotalpconc
         {
         Single get()
            {
            return Comp->Variable[ "mealtotalpconc" ]->ToSingle();
            }       
         } 
      property Single mealtotalstructuralwt
         {
         Single get()
            {
            return Comp->Variable[ "mealtotalstructuralwt" ]->ToSingle();
            }       
         } 
      property Single mealtotalwt
         {
         Single get()
            {
            return Comp->Variable[ "mealtotalwt" ]->ToSingle();
            }       
         } 
      property Single mealvegetativen
         {
         Single get()
            {
            return Comp->Variable[ "mealvegetativen" ]->ToSingle();
            }       
         } 
      property Single mealvegetativenconc
         {
         Single get()
            {
            return Comp->Variable[ "mealvegetativenconc" ]->ToSingle();
            }       
         } 
      property Single mealvegetativenonstructuralwt
         {
         Single get()
            {
            return Comp->Variable[ "mealvegetativenonstructuralwt" ]->ToSingle();
            }       
         } 
      property Single mealvegetativep
         {
         Single get()
            {
            return Comp->Variable[ "mealvegetativep" ]->ToSingle();
            }       
         } 
      property Single mealvegetativepconc
         {
         Single get()
            {
            return Comp->Variable[ "mealvegetativepconc" ]->ToSingle();
            }       
         } 
      property Single mealvegetativestructuralwt
         {
         Single get()
            {
            return Comp->Variable[ "mealvegetativestructuralwt" ]->ToSingle();
            }       
         } 
      property Single mealvegetativetotaln
         {
         Single get()
            {
            return Comp->Variable[ "mealvegetativetotaln" ]->ToSingle();
            }       
         } 
      property Single mealvegetativetotalnconc
         {
         Single get()
            {
            return Comp->Variable[ "mealvegetativetotalnconc" ]->ToSingle();
            }       
         } 
      property Single mealvegetativetotalnonstructuralwt
         {
         Single get()
            {
            return Comp->Variable[ "mealvegetativetotalnonstructuralwt" ]->ToSingle();
            }       
         } 
      property Single mealvegetativetotalp
         {
         Single get()
            {
            return Comp->Variable[ "mealvegetativetotalp" ]->ToSingle();
            }       
         } 
      property Single mealvegetativetotalpconc
         {
         Single get()
            {
            return Comp->Variable[ "mealvegetativetotalpconc" ]->ToSingle();
            }       
         } 
      property Single mealvegetativetotalstructuralwt
         {
         Single get()
            {
            return Comp->Variable[ "mealvegetativetotalstructuralwt" ]->ToSingle();
            }       
         } 
      property Single mealvegetativetotalwt
         {
         Single get()
            {
            return Comp->Variable[ "mealvegetativetotalwt" ]->ToSingle();
            }       
         } 
      property Single mealvegetativewt
         {
         Single get()
            {
            return Comp->Variable[ "mealvegetativewt" ]->ToSingle();
            }       
         } 
      property Single n_conc_crit
         {
         Single get()
            {
            return Comp->Variable[ "n_conc_crit" ]->ToSingle();
            }       
         } 
      property Single n_conc_crit_cohort1
         {
         Single get()
            {
            return Comp->Variable[ "n_conc_crit_cohort1" ]->ToSingle();
            }       
         } 
      property Single n_conc_crit_grain
         {
         Single get()
            {
            return Comp->Variable[ "n_conc_crit_grain" ]->ToSingle();
            }       
         } 
      property Single n_conc_crit_head
         {
         Single get()
            {
            return Comp->Variable[ "n_conc_crit_head" ]->ToSingle();
            }       
         } 
      property Single n_conc_crit_leaf
         {
         Single get()
            {
            return Comp->Variable[ "n_conc_crit_leaf" ]->ToSingle();
            }       
         } 
      property Single n_conc_crit_meal
         {
         Single get()
            {
            return Comp->Variable[ "n_conc_crit_meal" ]->ToSingle();
            }       
         } 
      property Single n_conc_crit_oil
         {
         Single get()
            {
            return Comp->Variable[ "n_conc_crit_oil" ]->ToSingle();
            }       
         } 
      property Single n_conc_crit_pod
         {
         Single get()
            {
            return Comp->Variable[ "n_conc_crit_pod" ]->ToSingle();
            }       
         } 
      property Single n_conc_crit_root
         {
         Single get()
            {
            return Comp->Variable[ "n_conc_crit_root" ]->ToSingle();
            }       
         } 
      property Single n_conc_crit_stem
         {
         Single get()
            {
            return Comp->Variable[ "n_conc_crit_stem" ]->ToSingle();
            }       
         } 
      property Single n_conc_grain
         {
         Single get()
            {
            return Comp->Variable[ "n_conc_grain" ]->ToSingle();
            }       
         } 
      property Single n_conc_meal
         {
         Single get()
            {
            return Comp->Variable[ "n_conc_meal" ]->ToSingle();
            }       
         } 
      property Single n_conc_min
         {
         Single get()
            {
            return Comp->Variable[ "n_conc_min" ]->ToSingle();
            }       
         } 
      property Single n_conc_min_cohort1
         {
         Single get()
            {
            return Comp->Variable[ "n_conc_min_cohort1" ]->ToSingle();
            }       
         } 
      property Single n_conc_min_grain
         {
         Single get()
            {
            return Comp->Variable[ "n_conc_min_grain" ]->ToSingle();
            }       
         } 
      property Single n_conc_min_head
         {
         Single get()
            {
            return Comp->Variable[ "n_conc_min_head" ]->ToSingle();
            }       
         } 
      property Single n_conc_min_leaf
         {
         Single get()
            {
            return Comp->Variable[ "n_conc_min_leaf" ]->ToSingle();
            }       
         } 
      property Single n_conc_min_meal
         {
         Single get()
            {
            return Comp->Variable[ "n_conc_min_meal" ]->ToSingle();
            }       
         } 
      property Single n_conc_min_oil
         {
         Single get()
            {
            return Comp->Variable[ "n_conc_min_oil" ]->ToSingle();
            }       
         } 
      property Single n_conc_min_pod
         {
         Single get()
            {
            return Comp->Variable[ "n_conc_min_pod" ]->ToSingle();
            }       
         } 
      property Single n_conc_min_root
         {
         Single get()
            {
            return Comp->Variable[ "n_conc_min_root" ]->ToSingle();
            }       
         } 
      property Single n_conc_min_stem
         {
         Single get()
            {
            return Comp->Variable[ "n_conc_min_stem" ]->ToSingle();
            }       
         } 
      property Single n_conc_stover
         {
         Single get()
            {
            return Comp->Variable[ "n_conc_stover" ]->ToSingle();
            }       
         } 
      property Single n_demand
         {
         Single get()
            {
            return Comp->Variable[ "n_demand" ]->ToSingle();
            }       
         } 
      property Single n_demand_cohort1
         {
         Single get()
            {
            return Comp->Variable[ "n_demand_cohort1" ]->ToSingle();
            }       
         } 
      property Single n_demand_grain
         {
         Single get()
            {
            return Comp->Variable[ "n_demand_grain" ]->ToSingle();
            }       
         } 
      property Single n_demand_head
         {
         Single get()
            {
            return Comp->Variable[ "n_demand_head" ]->ToSingle();
            }       
         } 
      property Single n_demand_leaf
         {
         Single get()
            {
            return Comp->Variable[ "n_demand_leaf" ]->ToSingle();
            }       
         } 
      property Single n_demand_meal
         {
         Single get()
            {
            return Comp->Variable[ "n_demand_meal" ]->ToSingle();
            }       
         } 
      property Single n_demand_oil
         {
         Single get()
            {
            return Comp->Variable[ "n_demand_oil" ]->ToSingle();
            }       
         } 
      property Single n_demand_pod
         {
         Single get()
            {
            return Comp->Variable[ "n_demand_pod" ]->ToSingle();
            }       
         } 
      property Single n_demand_root
         {
         Single get()
            {
            return Comp->Variable[ "n_demand_root" ]->ToSingle();
            }       
         } 
      property Single n_demand_stem
         {
         Single get()
            {
            return Comp->Variable[ "n_demand_stem" ]->ToSingle();
            }       
         } 
      property array<Single>^ n_demanded
         {
         array<Single>^ get()
            {
            return Comp->Variable[ "n_demanded" ]->ToSingleArray();
            }       
         } 
      property Single n_fixed_tops
         {
         Single get()
            {
            return Comp->Variable[ "n_fixed_tops" ]->ToSingle();
            }       
         } 
      property Single n_grain_pcnt
         {
         Single get()
            {
            return Comp->Variable[ "n_grain_pcnt" ]->ToSingle();
            }       
         } 
      property Single n_stress_expan
         {
         Single get()
            {
            return Comp->Variable[ "n_stress_expan" ]->ToSingle();
            }       
         } 
      property Single n_stress_grain
         {
         Single get()
            {
            return Comp->Variable[ "n_stress_grain" ]->ToSingle();
            }       
         } 
      property Single n_stress_pheno
         {
         Single get()
            {
            return Comp->Variable[ "n_stress_pheno" ]->ToSingle();
            }       
         } 
      property Single n_stress_photo
         {
         Single get()
            {
            return Comp->Variable[ "n_stress_photo" ]->ToSingle();
            }       
         } 
      property Single n_supply_soil
         {
         Single get()
            {
            return Comp->Variable[ "n_supply_soil" ]->ToSingle();
            }       
         } 
      property Single n_uptake
         {
         Single get()
            {
            return Comp->Variable[ "n_uptake" ]->ToSingle();
            }       
         } 
      property Single n_uptake_stover
         {
         Single get()
            {
            return Comp->Variable[ "n_uptake_stover" ]->ToSingle();
            }       
         } 
      property String^ name
         {
         String^ get()
            {
            return Comp->Variable[ "name" ]->ToString();
            }       
         } 
      property Single nfact_expan
         {
         Single get()
            {
            return Comp->Variable[ "nfact_expan" ]->ToSingle();
            }       
         } 
      property Single nfact_grain
         {
         Single get()
            {
            return Comp->Variable[ "nfact_grain" ]->ToSingle();
            }       
         } 
      property Single nfact_grain_tot
         {
         Single get()
            {
            return Comp->Variable[ "nfact_grain_tot" ]->ToSingle();
            }       
         } 
      property Single nfact_pheno
         {
         Single get()
            {
            return Comp->Variable[ "nfact_pheno" ]->ToSingle();
            }       
         } 
      property Single nfact_photo
         {
         Single get()
            {
            return Comp->Variable[ "nfact_photo" ]->ToSingle();
            }       
         } 
      property array<Single>^ nh4_uptake
         {
         array<Single>^ get()
            {
            return Comp->Variable[ "nh4_uptake" ]->ToSingleArray();
            }       
         } 
      property array<Single>^ nh4gsm_uptake_pot
         {
         array<Single>^ get()
            {
            return Comp->Variable[ "nh4gsm_uptake_pot" ]->ToSingleArray();
            }       
         } 
      property Single no3_demand
         {
         Single get()
            {
            return Comp->Variable[ "no3_demand" ]->ToSingle();
            }       
         } 
      property Single no3_tot
         {
         Single get()
            {
            return Comp->Variable[ "no3_tot" ]->ToSingle();
            }       
         } 
      property array<Single>^ no3_uptake
         {
         array<Single>^ get()
            {
            return Comp->Variable[ "no3_uptake" ]->ToSingleArray();
            }       
         } 
      property array<Single>^ no3gsm_uptake_pot
         {
         array<Single>^ get()
            {
            return Comp->Variable[ "no3gsm_uptake_pot" ]->ToSingleArray();
            }       
         } 
      property Single node_no
         {
         Single get()
            {
            return Comp->Variable[ "node_no" ]->ToSingle();
            }       
         } 
      property Single oildetachingn
         {
         Single get()
            {
            return Comp->Variable[ "oildetachingn" ]->ToSingle();
            }       
         } 
      property Single oildetachingp
         {
         Single get()
            {
            return Comp->Variable[ "oildetachingp" ]->ToSingle();
            }       
         } 
      property Single oildetachingwt
         {
         Single get()
            {
            return Comp->Variable[ "oildetachingwt" ]->ToSingle();
            }       
         } 
      property Single oilgrainn
         {
         Single get()
            {
            return Comp->Variable[ "oilgrainn" ]->ToSingle();
            }       
         } 
      property Single oilgrainnconc
         {
         Single get()
            {
            return Comp->Variable[ "oilgrainnconc" ]->ToSingle();
            }       
         } 
      property Single oilgrainnonstructuralwt
         {
         Single get()
            {
            return Comp->Variable[ "oilgrainnonstructuralwt" ]->ToSingle();
            }       
         } 
      property Single oilgrainp
         {
         Single get()
            {
            return Comp->Variable[ "oilgrainp" ]->ToSingle();
            }       
         } 
      property Single oilgrainpconc
         {
         Single get()
            {
            return Comp->Variable[ "oilgrainpconc" ]->ToSingle();
            }       
         } 
      property Single oilgrainstructuralwt
         {
         Single get()
            {
            return Comp->Variable[ "oilgrainstructuralwt" ]->ToSingle();
            }       
         } 
      property Single oilgraintotaln
         {
         Single get()
            {
            return Comp->Variable[ "oilgraintotaln" ]->ToSingle();
            }       
         } 
      property Single oilgraintotalnconc
         {
         Single get()
            {
            return Comp->Variable[ "oilgraintotalnconc" ]->ToSingle();
            }       
         } 
      property Single oilgraintotalnonstructuralwt
         {
         Single get()
            {
            return Comp->Variable[ "oilgraintotalnonstructuralwt" ]->ToSingle();
            }       
         } 
      property Single oilgraintotalp
         {
         Single get()
            {
            return Comp->Variable[ "oilgraintotalp" ]->ToSingle();
            }       
         } 
      property Single oilgraintotalpconc
         {
         Single get()
            {
            return Comp->Variable[ "oilgraintotalpconc" ]->ToSingle();
            }       
         } 
      property Single oilgraintotalstructuralwt
         {
         Single get()
            {
            return Comp->Variable[ "oilgraintotalstructuralwt" ]->ToSingle();
            }       
         } 
      property Single oilgraintotalwt
         {
         Single get()
            {
            return Comp->Variable[ "oilgraintotalwt" ]->ToSingle();
            }       
         } 
      property Single oilgrainwt
         {
         Single get()
            {
            return Comp->Variable[ "oilgrainwt" ]->ToSingle();
            }       
         } 
      property Single oilgreendigestibilityavg
         {
         Single get()
            {
            return Comp->Variable[ "oilgreendigestibilityavg" ]->ToSingle();
            }       
         } 
      property Single oilgreendigestibilitymax
         {
         Single get()
            {
            return Comp->Variable[ "oilgreendigestibilitymax" ]->ToSingle();
            }       
         } 
      property Single oilgreendigestibilitymin
         {
         Single get()
            {
            return Comp->Variable[ "oilgreendigestibilitymin" ]->ToSingle();
            }       
         } 
      property Single oilgreenn
         {
         Single get()
            {
            return Comp->Variable[ "oilgreenn" ]->ToSingle();
            }       
         } 
      property Single oilgreennconc
         {
         Single get()
            {
            return Comp->Variable[ "oilgreennconc" ]->ToSingle();
            }       
         } 
      property Single oilgreennonstructuralwt
         {
         Single get()
            {
            return Comp->Variable[ "oilgreennonstructuralwt" ]->ToSingle();
            }       
         } 
      property Single oilgreenp
         {
         Single get()
            {
            return Comp->Variable[ "oilgreenp" ]->ToSingle();
            }       
         } 
      property Single oilgreenpconc
         {
         Single get()
            {
            return Comp->Variable[ "oilgreenpconc" ]->ToSingle();
            }       
         } 
      property Single oilgreenremovedn
         {
         Single get()
            {
            return Comp->Variable[ "oilgreenremovedn" ]->ToSingle();
            }       
         } 
      property Single oilgreenremovedp
         {
         Single get()
            {
            return Comp->Variable[ "oilgreenremovedp" ]->ToSingle();
            }       
         } 
      property Single oilgreenremovedwt
         {
         Single get()
            {
            return Comp->Variable[ "oilgreenremovedwt" ]->ToSingle();
            }       
         } 
      property Single oilgreenstructuralwt
         {
         Single get()
            {
            return Comp->Variable[ "oilgreenstructuralwt" ]->ToSingle();
            }       
         } 
      property Single oilgreenwt
         {
         Single get()
            {
            return Comp->Variable[ "oilgreenwt" ]->ToSingle();
            }       
         } 
      property Single oilgrowthn
         {
         Single get()
            {
            return Comp->Variable[ "oilgrowthn" ]->ToSingle();
            }       
         } 
      property Single oilgrowthp
         {
         Single get()
            {
            return Comp->Variable[ "oilgrowthp" ]->ToSingle();
            }       
         } 
      property Single oilgrowthwt
         {
         Single get()
            {
            return Comp->Variable[ "oilgrowthwt" ]->ToSingle();
            }       
         } 
      property Single oilretranslocationn
         {
         Single get()
            {
            return Comp->Variable[ "oilretranslocationn" ]->ToSingle();
            }       
         } 
      property Single oilretranslocationp
         {
         Single get()
            {
            return Comp->Variable[ "oilretranslocationp" ]->ToSingle();
            }       
         } 
      property Single oilretranslocationwt
         {
         Single get()
            {
            return Comp->Variable[ "oilretranslocationwt" ]->ToSingle();
            }       
         } 
      property Single oilsenesceddigestibilityavg
         {
         Single get()
            {
            return Comp->Variable[ "oilsenesceddigestibilityavg" ]->ToSingle();
            }       
         } 
      property Single oilsenesceddigestibilitymax
         {
         Single get()
            {
            return Comp->Variable[ "oilsenesceddigestibilitymax" ]->ToSingle();
            }       
         } 
      property Single oilsenesceddigestibilitymin
         {
         Single get()
            {
            return Comp->Variable[ "oilsenesceddigestibilitymin" ]->ToSingle();
            }       
         } 
      property Single oilsenescedn
         {
         Single get()
            {
            return Comp->Variable[ "oilsenescedn" ]->ToSingle();
            }       
         } 
      property Single oilsenescednconc
         {
         Single get()
            {
            return Comp->Variable[ "oilsenescednconc" ]->ToSingle();
            }       
         } 
      property Single oilsenescednonstructuralwt
         {
         Single get()
            {
            return Comp->Variable[ "oilsenescednonstructuralwt" ]->ToSingle();
            }       
         } 
      property Single oilsenescedp
         {
         Single get()
            {
            return Comp->Variable[ "oilsenescedp" ]->ToSingle();
            }       
         } 
      property Single oilsenescedpconc
         {
         Single get()
            {
            return Comp->Variable[ "oilsenescedpconc" ]->ToSingle();
            }       
         } 
      property Single oilsenescedremovedn
         {
         Single get()
            {
            return Comp->Variable[ "oilsenescedremovedn" ]->ToSingle();
            }       
         } 
      property Single oilsenescedremovedp
         {
         Single get()
            {
            return Comp->Variable[ "oilsenescedremovedp" ]->ToSingle();
            }       
         } 
      property Single oilsenescedremovedwt
         {
         Single get()
            {
            return Comp->Variable[ "oilsenescedremovedwt" ]->ToSingle();
            }       
         } 
      property Single oilsenescedstructuralwt
         {
         Single get()
            {
            return Comp->Variable[ "oilsenescedstructuralwt" ]->ToSingle();
            }       
         } 
      property Single oilsenescedwt
         {
         Single get()
            {
            return Comp->Variable[ "oilsenescedwt" ]->ToSingle();
            }       
         } 
      property Single oilsenescingn
         {
         Single get()
            {
            return Comp->Variable[ "oilsenescingn" ]->ToSingle();
            }       
         } 
      property Single oilsenescingp
         {
         Single get()
            {
            return Comp->Variable[ "oilsenescingp" ]->ToSingle();
            }       
         } 
      property Single oilsenescingwt
         {
         Single get()
            {
            return Comp->Variable[ "oilsenescingwt" ]->ToSingle();
            }       
         } 
      property Single oiltotaln
         {
         Single get()
            {
            return Comp->Variable[ "oiltotaln" ]->ToSingle();
            }       
         } 
      property Single oiltotalnconc
         {
         Single get()
            {
            return Comp->Variable[ "oiltotalnconc" ]->ToSingle();
            }       
         } 
      property Single oiltotalnonstructuralwt
         {
         Single get()
            {
            return Comp->Variable[ "oiltotalnonstructuralwt" ]->ToSingle();
            }       
         } 
      property Single oiltotalp
         {
         Single get()
            {
            return Comp->Variable[ "oiltotalp" ]->ToSingle();
            }       
         } 
      property Single oiltotalpconc
         {
         Single get()
            {
            return Comp->Variable[ "oiltotalpconc" ]->ToSingle();
            }       
         } 
      property Single oiltotalstructuralwt
         {
         Single get()
            {
            return Comp->Variable[ "oiltotalstructuralwt" ]->ToSingle();
            }       
         } 
      property Single oiltotalwt
         {
         Single get()
            {
            return Comp->Variable[ "oiltotalwt" ]->ToSingle();
            }       
         } 
      property Single oilvegetativen
         {
         Single get()
            {
            return Comp->Variable[ "oilvegetativen" ]->ToSingle();
            }       
         } 
      property Single oilvegetativenconc
         {
         Single get()
            {
            return Comp->Variable[ "oilvegetativenconc" ]->ToSingle();
            }       
         } 
      property Single oilvegetativenonstructuralwt
         {
         Single get()
            {
            return Comp->Variable[ "oilvegetativenonstructuralwt" ]->ToSingle();
            }       
         } 
      property Single oilvegetativep
         {
         Single get()
            {
            return Comp->Variable[ "oilvegetativep" ]->ToSingle();
            }       
         } 
      property Single oilvegetativepconc
         {
         Single get()
            {
            return Comp->Variable[ "oilvegetativepconc" ]->ToSingle();
            }       
         } 
      property Single oilvegetativestructuralwt
         {
         Single get()
            {
            return Comp->Variable[ "oilvegetativestructuralwt" ]->ToSingle();
            }       
         } 
      property Single oilvegetativetotaln
         {
         Single get()
            {
            return Comp->Variable[ "oilvegetativetotaln" ]->ToSingle();
            }       
         } 
      property Single oilvegetativetotalnconc
         {
         Single get()
            {
            return Comp->Variable[ "oilvegetativetotalnconc" ]->ToSingle();
            }       
         } 
      property Single oilvegetativetotalnonstructuralwt
         {
         Single get()
            {
            return Comp->Variable[ "oilvegetativetotalnonstructuralwt" ]->ToSingle();
            }       
         } 
      property Single oilvegetativetotalp
         {
         Single get()
            {
            return Comp->Variable[ "oilvegetativetotalp" ]->ToSingle();
            }       
         } 
      property Single oilvegetativetotalpconc
         {
         Single get()
            {
            return Comp->Variable[ "oilvegetativetotalpconc" ]->ToSingle();
            }       
         } 
      property Single oilvegetativetotalstructuralwt
         {
         Single get()
            {
            return Comp->Variable[ "oilvegetativetotalstructuralwt" ]->ToSingle();
            }       
         } 
      property Single oilvegetativetotalwt
         {
         Single get()
            {
            return Comp->Variable[ "oilvegetativetotalwt" ]->ToSingle();
            }       
         } 
      property Single oilvegetativewt
         {
         Single get()
            {
            return Comp->Variable[ "oilvegetativewt" ]->ToSingle();
            }       
         } 
      property Single outtttarget
         {
         Single get()
            {
            return Comp->Variable[ "outtttarget" ]->ToSingle();
            }       
         } 
      property Single oxdef_photo
         {
         Single get()
            {
            return Comp->Variable[ "oxdef_photo" ]->ToSingle();
            }       
         } 
      property Single p_conc_stover
         {
         Single get()
            {
            return Comp->Variable[ "p_conc_stover" ]->ToSingle();
            }       
         } 
      property Single p_demand
         {
         Single get()
            {
            return Comp->Variable[ "p_demand" ]->ToSingle();
            }       
         } 
      property array<Single>^ p_demand_parts
         {
         array<Single>^ get()
            {
            return Comp->Variable[ "p_demand_parts" ]->ToSingleArray();
            }       
         } 
      property Single p_uptake
         {
         Single get()
            {
            return Comp->Variable[ "p_uptake" ]->ToSingle();
            }       
         } 
      property Single p_uptake_stover
         {
         Single get()
            {
            return Comp->Variable[ "p_uptake_stover" ]->ToSingle();
            }       
         } 
      property Single pai
         {
         Single get()
            {
            return Comp->Variable[ "pai" ]->ToSingle();
            }       
         } 
      property Single parasite_dm_supply
         {
         Single get()
            {
            return Comp->Variable[ "parasite_dm_supply" ]->ToSingle();
            }       
         } 
      property Single phase
         {
         Single get()
            {
            return Comp->Variable[ "phase" ]->ToSingle();
            }       
         void set(Single value)
            {
            Comp->Variable[ "phase" ]->Set(value);
            }       
         }
      property String^ plant_status
         {
         String^ get()
            {
            return Comp->Variable[ "plant_status" ]->ToString();
            }       
         } 
      property Single plants
         {
         Single get()
            {
            return Comp->Variable[ "plants" ]->ToSingle();
            }       
         } 
      property Single poddetachingn
         {
         Single get()
            {
            return Comp->Variable[ "poddetachingn" ]->ToSingle();
            }       
         } 
      property Single poddetachingp
         {
         Single get()
            {
            return Comp->Variable[ "poddetachingp" ]->ToSingle();
            }       
         } 
      property Single poddetachingwt
         {
         Single get()
            {
            return Comp->Variable[ "poddetachingwt" ]->ToSingle();
            }       
         } 
      property Single podgreendigestibilityavg
         {
         Single get()
            {
            return Comp->Variable[ "podgreendigestibilityavg" ]->ToSingle();
            }       
         } 
      property Single podgreendigestibilitymax
         {
         Single get()
            {
            return Comp->Variable[ "podgreendigestibilitymax" ]->ToSingle();
            }       
         } 
      property Single podgreendigestibilitymin
         {
         Single get()
            {
            return Comp->Variable[ "podgreendigestibilitymin" ]->ToSingle();
            }       
         } 
      property Single podgreenn
         {
         Single get()
            {
            return Comp->Variable[ "podgreenn" ]->ToSingle();
            }       
         } 
      property Single podgreennconc
         {
         Single get()
            {
            return Comp->Variable[ "podgreennconc" ]->ToSingle();
            }       
         } 
      property Single podgreennonstructuralwt
         {
         Single get()
            {
            return Comp->Variable[ "podgreennonstructuralwt" ]->ToSingle();
            }       
         } 
      property Single podgreenp
         {
         Single get()
            {
            return Comp->Variable[ "podgreenp" ]->ToSingle();
            }       
         } 
      property Single podgreenpconc
         {
         Single get()
            {
            return Comp->Variable[ "podgreenpconc" ]->ToSingle();
            }       
         } 
      property Single podgreenremovedn
         {
         Single get()
            {
            return Comp->Variable[ "podgreenremovedn" ]->ToSingle();
            }       
         } 
      property Single podgreenremovedp
         {
         Single get()
            {
            return Comp->Variable[ "podgreenremovedp" ]->ToSingle();
            }       
         } 
      property Single podgreenremovedwt
         {
         Single get()
            {
            return Comp->Variable[ "podgreenremovedwt" ]->ToSingle();
            }       
         } 
      property Single podgreenstructuralwt
         {
         Single get()
            {
            return Comp->Variable[ "podgreenstructuralwt" ]->ToSingle();
            }       
         } 
      property Single podgreenwt
         {
         Single get()
            {
            return Comp->Variable[ "podgreenwt" ]->ToSingle();
            }       
         } 
      property Single podgrowthn
         {
         Single get()
            {
            return Comp->Variable[ "podgrowthn" ]->ToSingle();
            }       
         } 
      property Single podgrowthp
         {
         Single get()
            {
            return Comp->Variable[ "podgrowthp" ]->ToSingle();
            }       
         } 
      property Single podgrowthwt
         {
         Single get()
            {
            return Comp->Variable[ "podgrowthwt" ]->ToSingle();
            }       
         } 
      property Single podretranslocationn
         {
         Single get()
            {
            return Comp->Variable[ "podretranslocationn" ]->ToSingle();
            }       
         } 
      property Single podretranslocationp
         {
         Single get()
            {
            return Comp->Variable[ "podretranslocationp" ]->ToSingle();
            }       
         } 
      property Single podretranslocationwt
         {
         Single get()
            {
            return Comp->Variable[ "podretranslocationwt" ]->ToSingle();
            }       
         } 
      property Single podsenesceddigestibilityavg
         {
         Single get()
            {
            return Comp->Variable[ "podsenesceddigestibilityavg" ]->ToSingle();
            }       
         } 
      property Single podsenesceddigestibilitymax
         {
         Single get()
            {
            return Comp->Variable[ "podsenesceddigestibilitymax" ]->ToSingle();
            }       
         } 
      property Single podsenesceddigestibilitymin
         {
         Single get()
            {
            return Comp->Variable[ "podsenesceddigestibilitymin" ]->ToSingle();
            }       
         } 
      property Single podsenescedn
         {
         Single get()
            {
            return Comp->Variable[ "podsenescedn" ]->ToSingle();
            }       
         } 
      property Single podsenescednconc
         {
         Single get()
            {
            return Comp->Variable[ "podsenescednconc" ]->ToSingle();
            }       
         } 
      property Single podsenescednonstructuralwt
         {
         Single get()
            {
            return Comp->Variable[ "podsenescednonstructuralwt" ]->ToSingle();
            }       
         } 
      property Single podsenescedp
         {
         Single get()
            {
            return Comp->Variable[ "podsenescedp" ]->ToSingle();
            }       
         } 
      property Single podsenescedpconc
         {
         Single get()
            {
            return Comp->Variable[ "podsenescedpconc" ]->ToSingle();
            }       
         } 
      property Single podsenescedremovedn
         {
         Single get()
            {
            return Comp->Variable[ "podsenescedremovedn" ]->ToSingle();
            }       
         } 
      property Single podsenescedremovedp
         {
         Single get()
            {
            return Comp->Variable[ "podsenescedremovedp" ]->ToSingle();
            }       
         } 
      property Single podsenescedremovedwt
         {
         Single get()
            {
            return Comp->Variable[ "podsenescedremovedwt" ]->ToSingle();
            }       
         } 
      property Single podsenescedstructuralwt
         {
         Single get()
            {
            return Comp->Variable[ "podsenescedstructuralwt" ]->ToSingle();
            }       
         } 
      property Single podsenescedwt
         {
         Single get()
            {
            return Comp->Variable[ "podsenescedwt" ]->ToSingle();
            }       
         } 
      property Single podsenescingn
         {
         Single get()
            {
            return Comp->Variable[ "podsenescingn" ]->ToSingle();
            }       
         } 
      property Single podsenescingp
         {
         Single get()
            {
            return Comp->Variable[ "podsenescingp" ]->ToSingle();
            }       
         } 
      property Single podsenescingwt
         {
         Single get()
            {
            return Comp->Variable[ "podsenescingwt" ]->ToSingle();
            }       
         } 
      property Single podtotaln
         {
         Single get()
            {
            return Comp->Variable[ "podtotaln" ]->ToSingle();
            }       
         } 
      property Single podtotalnconc
         {
         Single get()
            {
            return Comp->Variable[ "podtotalnconc" ]->ToSingle();
            }       
         } 
      property Single podtotalnonstructuralwt
         {
         Single get()
            {
            return Comp->Variable[ "podtotalnonstructuralwt" ]->ToSingle();
            }       
         } 
      property Single podtotalp
         {
         Single get()
            {
            return Comp->Variable[ "podtotalp" ]->ToSingle();
            }       
         } 
      property Single podtotalpconc
         {
         Single get()
            {
            return Comp->Variable[ "podtotalpconc" ]->ToSingle();
            }       
         } 
      property Single podtotalstructuralwt
         {
         Single get()
            {
            return Comp->Variable[ "podtotalstructuralwt" ]->ToSingle();
            }       
         } 
      property Single podtotalwt
         {
         Single get()
            {
            return Comp->Variable[ "podtotalwt" ]->ToSingle();
            }       
         } 
      property Single podvegetativen
         {
         Single get()
            {
            return Comp->Variable[ "podvegetativen" ]->ToSingle();
            }       
         } 
      property Single podvegetativenconc
         {
         Single get()
            {
            return Comp->Variable[ "podvegetativenconc" ]->ToSingle();
            }       
         } 
      property Single podvegetativenonstructuralwt
         {
         Single get()
            {
            return Comp->Variable[ "podvegetativenonstructuralwt" ]->ToSingle();
            }       
         } 
      property Single podvegetativep
         {
         Single get()
            {
            return Comp->Variable[ "podvegetativep" ]->ToSingle();
            }       
         } 
      property Single podvegetativepconc
         {
         Single get()
            {
            return Comp->Variable[ "podvegetativepconc" ]->ToSingle();
            }       
         } 
      property Single podvegetativestructuralwt
         {
         Single get()
            {
            return Comp->Variable[ "podvegetativestructuralwt" ]->ToSingle();
            }       
         } 
      property Single podvegetativetotaln
         {
         Single get()
            {
            return Comp->Variable[ "podvegetativetotaln" ]->ToSingle();
            }       
         } 
      property Single podvegetativetotalnconc
         {
         Single get()
            {
            return Comp->Variable[ "podvegetativetotalnconc" ]->ToSingle();
            }       
         } 
      property Single podvegetativetotalnonstructuralwt
         {
         Single get()
            {
            return Comp->Variable[ "podvegetativetotalnonstructuralwt" ]->ToSingle();
            }       
         } 
      property Single podvegetativetotalp
         {
         Single get()
            {
            return Comp->Variable[ "podvegetativetotalp" ]->ToSingle();
            }       
         } 
      property Single podvegetativetotalpconc
         {
         Single get()
            {
            return Comp->Variable[ "podvegetativetotalpconc" ]->ToSingle();
            }       
         } 
      property Single podvegetativetotalstructuralwt
         {
         Single get()
            {
            return Comp->Variable[ "podvegetativetotalstructuralwt" ]->ToSingle();
            }       
         } 
      property Single podvegetativetotalwt
         {
         Single get()
            {
            return Comp->Variable[ "podvegetativetotalwt" ]->ToSingle();
            }       
         } 
      property Single podvegetativewt
         {
         Single get()
            {
            return Comp->Variable[ "podvegetativewt" ]->ToSingle();
            }       
         } 
      property Single remove_biom_pheno
         {
         Single get()
            {
            return Comp->Variable[ "remove_biom_pheno" ]->ToSingle();
            }       
         } 
      property Single respiration
         {
         Single get()
            {
            return Comp->Variable[ "respiration" ]->ToSingle();
            }       
         } 
      property Single retranslocationn
         {
         Single get()
            {
            return Comp->Variable[ "retranslocationn" ]->ToSingle();
            }       
         } 
      property Single retranslocationp
         {
         Single get()
            {
            return Comp->Variable[ "retranslocationp" ]->ToSingle();
            }       
         } 
      property Single retranslocationwt
         {
         Single get()
            {
            return Comp->Variable[ "retranslocationwt" ]->ToSingle();
            }       
         } 
      property array<Single>^ rld
         {
         array<Single>^ get()
            {
            return Comp->Variable[ "rld" ]->ToSingleArray();
            }       
         } 
      property array<Single>^ rlv
         {
         array<Single>^ get()
            {
            return Comp->Variable[ "rlv" ]->ToSingleArray();
            }       
         } 
      property Single root_depth
         {
         Single get()
            {
            return Comp->Variable[ "root_depth" ]->ToSingle();
            }       
         } 
      property array<Single>^ root_length
         {
         array<Single>^ get()
            {
            return Comp->Variable[ "root_length" ]->ToSingleArray();
            }       
         } 
      property array<Single>^ root_length_senesced
         {
         array<Single>^ get()
            {
            return Comp->Variable[ "root_length_senesced" ]->ToSingleArray();
            }       
         } 
      property Single rootdetachingn
         {
         Single get()
            {
            return Comp->Variable[ "rootdetachingn" ]->ToSingle();
            }       
         } 
      property Single rootdetachingp
         {
         Single get()
            {
            return Comp->Variable[ "rootdetachingp" ]->ToSingle();
            }       
         } 
      property Single rootdetachingwt
         {
         Single get()
            {
            return Comp->Variable[ "rootdetachingwt" ]->ToSingle();
            }       
         } 
      property Single rootgreenn
         {
         Single get()
            {
            return Comp->Variable[ "rootgreenn" ]->ToSingle();
            }       
         } 
      property Single rootgreennconc
         {
         Single get()
            {
            return Comp->Variable[ "rootgreennconc" ]->ToSingle();
            }       
         } 
      property Single rootgreennonstructuralwt
         {
         Single get()
            {
            return Comp->Variable[ "rootgreennonstructuralwt" ]->ToSingle();
            }       
         } 
      property Single rootgreenp
         {
         Single get()
            {
            return Comp->Variable[ "rootgreenp" ]->ToSingle();
            }       
         } 
      property Single rootgreenpconc
         {
         Single get()
            {
            return Comp->Variable[ "rootgreenpconc" ]->ToSingle();
            }       
         } 
      property Single rootgreenremovedn
         {
         Single get()
            {
            return Comp->Variable[ "rootgreenremovedn" ]->ToSingle();
            }       
         } 
      property Single rootgreenremovedp
         {
         Single get()
            {
            return Comp->Variable[ "rootgreenremovedp" ]->ToSingle();
            }       
         } 
      property Single rootgreenremovedwt
         {
         Single get()
            {
            return Comp->Variable[ "rootgreenremovedwt" ]->ToSingle();
            }       
         } 
      property Single rootgreenstructuralwt
         {
         Single get()
            {
            return Comp->Variable[ "rootgreenstructuralwt" ]->ToSingle();
            }       
         } 
      property Single rootgreenwt
         {
         Single get()
            {
            return Comp->Variable[ "rootgreenwt" ]->ToSingle();
            }       
         } 
      property Single rootgrowthn
         {
         Single get()
            {
            return Comp->Variable[ "rootgrowthn" ]->ToSingle();
            }       
         } 
      property Single rootgrowthp
         {
         Single get()
            {
            return Comp->Variable[ "rootgrowthp" ]->ToSingle();
            }       
         } 
      property Single rootgrowthwt
         {
         Single get()
            {
            return Comp->Variable[ "rootgrowthwt" ]->ToSingle();
            }       
         } 
      property Single rootretranslocationn
         {
         Single get()
            {
            return Comp->Variable[ "rootretranslocationn" ]->ToSingle();
            }       
         } 
      property Single rootretranslocationp
         {
         Single get()
            {
            return Comp->Variable[ "rootretranslocationp" ]->ToSingle();
            }       
         } 
      property Single rootretranslocationwt
         {
         Single get()
            {
            return Comp->Variable[ "rootretranslocationwt" ]->ToSingle();
            }       
         } 
      property Single rootsenescedn
         {
         Single get()
            {
            return Comp->Variable[ "rootsenescedn" ]->ToSingle();
            }       
         } 
      property Single rootsenescednconc
         {
         Single get()
            {
            return Comp->Variable[ "rootsenescednconc" ]->ToSingle();
            }       
         } 
      property Single rootsenescednonstructuralwt
         {
         Single get()
            {
            return Comp->Variable[ "rootsenescednonstructuralwt" ]->ToSingle();
            }       
         } 
      property Single rootsenescedp
         {
         Single get()
            {
            return Comp->Variable[ "rootsenescedp" ]->ToSingle();
            }       
         } 
      property Single rootsenescedpconc
         {
         Single get()
            {
            return Comp->Variable[ "rootsenescedpconc" ]->ToSingle();
            }       
         } 
      property Single rootsenescedremovedn
         {
         Single get()
            {
            return Comp->Variable[ "rootsenescedremovedn" ]->ToSingle();
            }       
         } 
      property Single rootsenescedremovedp
         {
         Single get()
            {
            return Comp->Variable[ "rootsenescedremovedp" ]->ToSingle();
            }       
         } 
      property Single rootsenescedremovedwt
         {
         Single get()
            {
            return Comp->Variable[ "rootsenescedremovedwt" ]->ToSingle();
            }       
         } 
      property Single rootsenescedstructuralwt
         {
         Single get()
            {
            return Comp->Variable[ "rootsenescedstructuralwt" ]->ToSingle();
            }       
         } 
      property Single rootsenescedwt
         {
         Single get()
            {
            return Comp->Variable[ "rootsenescedwt" ]->ToSingle();
            }       
         } 
      property Single rootsenescingn
         {
         Single get()
            {
            return Comp->Variable[ "rootsenescingn" ]->ToSingle();
            }       
         } 
      property Single rootsenescingp
         {
         Single get()
            {
            return Comp->Variable[ "rootsenescingp" ]->ToSingle();
            }       
         } 
      property Single rootsenescingwt
         {
         Single get()
            {
            return Comp->Variable[ "rootsenescingwt" ]->ToSingle();
            }       
         } 
      property Single roottotaln
         {
         Single get()
            {
            return Comp->Variable[ "roottotaln" ]->ToSingle();
            }       
         } 
      property Single roottotalnconc
         {
         Single get()
            {
            return Comp->Variable[ "roottotalnconc" ]->ToSingle();
            }       
         } 
      property Single roottotalnonstructuralwt
         {
         Single get()
            {
            return Comp->Variable[ "roottotalnonstructuralwt" ]->ToSingle();
            }       
         } 
      property Single roottotalp
         {
         Single get()
            {
            return Comp->Variable[ "roottotalp" ]->ToSingle();
            }       
         } 
      property Single roottotalpconc
         {
         Single get()
            {
            return Comp->Variable[ "roottotalpconc" ]->ToSingle();
            }       
         } 
      property Single roottotalstructuralwt
         {
         Single get()
            {
            return Comp->Variable[ "roottotalstructuralwt" ]->ToSingle();
            }       
         } 
      property Single roottotalwt
         {
         Single get()
            {
            return Comp->Variable[ "roottotalwt" ]->ToSingle();
            }       
         } 
      property Single rootvegetativen
         {
         Single get()
            {
            return Comp->Variable[ "rootvegetativen" ]->ToSingle();
            }       
         } 
      property Single rootvegetativenconc
         {
         Single get()
            {
            return Comp->Variable[ "rootvegetativenconc" ]->ToSingle();
            }       
         } 
      property Single rootvegetativenonstructuralwt
         {
         Single get()
            {
            return Comp->Variable[ "rootvegetativenonstructuralwt" ]->ToSingle();
            }       
         } 
      property Single rootvegetativep
         {
         Single get()
            {
            return Comp->Variable[ "rootvegetativep" ]->ToSingle();
            }       
         } 
      property Single rootvegetativepconc
         {
         Single get()
            {
            return Comp->Variable[ "rootvegetativepconc" ]->ToSingle();
            }       
         } 
      property Single rootvegetativestructuralwt
         {
         Single get()
            {
            return Comp->Variable[ "rootvegetativestructuralwt" ]->ToSingle();
            }       
         } 
      property Single rootvegetativetotaln
         {
         Single get()
            {
            return Comp->Variable[ "rootvegetativetotaln" ]->ToSingle();
            }       
         } 
      property Single rootvegetativetotalnconc
         {
         Single get()
            {
            return Comp->Variable[ "rootvegetativetotalnconc" ]->ToSingle();
            }       
         } 
      property Single rootvegetativetotalnonstructuralwt
         {
         Single get()
            {
            return Comp->Variable[ "rootvegetativetotalnonstructuralwt" ]->ToSingle();
            }       
         } 
      property Single rootvegetativetotalp
         {
         Single get()
            {
            return Comp->Variable[ "rootvegetativetotalp" ]->ToSingle();
            }       
         } 
      property Single rootvegetativetotalpconc
         {
         Single get()
            {
            return Comp->Variable[ "rootvegetativetotalpconc" ]->ToSingle();
            }       
         } 
      property Single rootvegetativetotalstructuralwt
         {
         Single get()
            {
            return Comp->Variable[ "rootvegetativetotalstructuralwt" ]->ToSingle();
            }       
         } 
      property Single rootvegetativetotalwt
         {
         Single get()
            {
            return Comp->Variable[ "rootvegetativetotalwt" ]->ToSingle();
            }       
         } 
      property Single rootvegetativewt
         {
         Single get()
            {
            return Comp->Variable[ "rootvegetativewt" ]->ToSingle();
            }       
         } 
      property Single senescedn
         {
         Single get()
            {
            return Comp->Variable[ "senescedn" ]->ToSingle();
            }       
         } 
      property Single senescednconc
         {
         Single get()
            {
            return Comp->Variable[ "senescednconc" ]->ToSingle();
            }       
         } 
      property Single senescednonstructuralwt
         {
         Single get()
            {
            return Comp->Variable[ "senescednonstructuralwt" ]->ToSingle();
            }       
         } 
      property Single senescedp
         {
         Single get()
            {
            return Comp->Variable[ "senescedp" ]->ToSingle();
            }       
         } 
      property Single senescedpconc
         {
         Single get()
            {
            return Comp->Variable[ "senescedpconc" ]->ToSingle();
            }       
         } 
      property Single senescedremovedn
         {
         Single get()
            {
            return Comp->Variable[ "senescedremovedn" ]->ToSingle();
            }       
         } 
      property Single senescedremovedp
         {
         Single get()
            {
            return Comp->Variable[ "senescedremovedp" ]->ToSingle();
            }       
         } 
      property Single senescedremovedwt
         {
         Single get()
            {
            return Comp->Variable[ "senescedremovedwt" ]->ToSingle();
            }       
         } 
      property Single senescedstructuralwt
         {
         Single get()
            {
            return Comp->Variable[ "senescedstructuralwt" ]->ToSingle();
            }       
         } 
      property Single senescedwt
         {
         Single get()
            {
            return Comp->Variable[ "senescedwt" ]->ToSingle();
            }       
         } 
      property Single senescingn
         {
         Single get()
            {
            return Comp->Variable[ "senescingn" ]->ToSingle();
            }       
         } 
      property Single senescingp
         {
         Single get()
            {
            return Comp->Variable[ "senescingp" ]->ToSingle();
            }       
         } 
      property Single senescingwt
         {
         Single get()
            {
            return Comp->Variable[ "senescingwt" ]->ToSingle();
            }       
         } 
      property Single slai
         {
         Single get()
            {
            return Comp->Variable[ "slai" ]->ToSingle();
            }       
         } 
      property Int32 sowing_das
         {
         Int32 get()
            {
            return Comp->Variable[ "sowing_das" ]->ToInt32();
            }       
         } 
      property Int32 sowing_date
         {
         Int32 get()
            {
            return Comp->Variable[ "sowing_date" ]->ToInt32();
            }       
         } 
      property Single sowingtttarget
         {
         Single get()
            {
            return Comp->Variable[ "sowingtttarget" ]->ToSingle();
            }       
         } 
      property Single stage
         {
         Single get()
            {
            return Comp->Variable[ "stage" ]->ToSingle();
            }       
         } 
      property String^ stagename
         {
         String^ get()
            {
            return Comp->Variable[ "stagename" ]->ToString();
            }       
         } 
      property Single start_grain_filltttarget
         {
         Single get()
            {
            return Comp->Variable[ "start_grain_filltttarget" ]->ToSingle();
            }       
         } 
      property String^ state
         {
         String^ get()
            {
            return Comp->Variable[ "state" ]->ToString();
            }       
         } 
      property Single stemdetachingn
         {
         Single get()
            {
            return Comp->Variable[ "stemdetachingn" ]->ToSingle();
            }       
         } 
      property Single stemdetachingp
         {
         Single get()
            {
            return Comp->Variable[ "stemdetachingp" ]->ToSingle();
            }       
         } 
      property Single stemdetachingwt
         {
         Single get()
            {
            return Comp->Variable[ "stemdetachingwt" ]->ToSingle();
            }       
         } 
      property Single stemgreendigestibilityavg
         {
         Single get()
            {
            return Comp->Variable[ "stemgreendigestibilityavg" ]->ToSingle();
            }       
         } 
      property Single stemgreendigestibilitymax
         {
         Single get()
            {
            return Comp->Variable[ "stemgreendigestibilitymax" ]->ToSingle();
            }       
         } 
      property Single stemgreendigestibilitymin
         {
         Single get()
            {
            return Comp->Variable[ "stemgreendigestibilitymin" ]->ToSingle();
            }       
         } 
      property Single stemgreenn
         {
         Single get()
            {
            return Comp->Variable[ "stemgreenn" ]->ToSingle();
            }       
         } 
      property Single stemgreennconc
         {
         Single get()
            {
            return Comp->Variable[ "stemgreennconc" ]->ToSingle();
            }       
         } 
      property Single stemgreennonstructuralwt
         {
         Single get()
            {
            return Comp->Variable[ "stemgreennonstructuralwt" ]->ToSingle();
            }       
         } 
      property Single stemgreenp
         {
         Single get()
            {
            return Comp->Variable[ "stemgreenp" ]->ToSingle();
            }       
         } 
      property Single stemgreenpconc
         {
         Single get()
            {
            return Comp->Variable[ "stemgreenpconc" ]->ToSingle();
            }       
         } 
      property Single stemgreenremovedn
         {
         Single get()
            {
            return Comp->Variable[ "stemgreenremovedn" ]->ToSingle();
            }       
         } 
      property Single stemgreenremovedp
         {
         Single get()
            {
            return Comp->Variable[ "stemgreenremovedp" ]->ToSingle();
            }       
         } 
      property Single stemgreenremovedwt
         {
         Single get()
            {
            return Comp->Variable[ "stemgreenremovedwt" ]->ToSingle();
            }       
         } 
      property Single stemgreenstructuralwt
         {
         Single get()
            {
            return Comp->Variable[ "stemgreenstructuralwt" ]->ToSingle();
            }       
         } 
      property Single stemgreenwt
         {
         Single get()
            {
            return Comp->Variable[ "stemgreenwt" ]->ToSingle();
            }       
         } 
      property Single stemgrowthn
         {
         Single get()
            {
            return Comp->Variable[ "stemgrowthn" ]->ToSingle();
            }       
         } 
      property Single stemgrowthp
         {
         Single get()
            {
            return Comp->Variable[ "stemgrowthp" ]->ToSingle();
            }       
         } 
      property Single stemgrowthwt
         {
         Single get()
            {
            return Comp->Variable[ "stemgrowthwt" ]->ToSingle();
            }       
         } 
      property Single stemretranslocationn
         {
         Single get()
            {
            return Comp->Variable[ "stemretranslocationn" ]->ToSingle();
            }       
         } 
      property Single stemretranslocationp
         {
         Single get()
            {
            return Comp->Variable[ "stemretranslocationp" ]->ToSingle();
            }       
         } 
      property Single stemretranslocationwt
         {
         Single get()
            {
            return Comp->Variable[ "stemretranslocationwt" ]->ToSingle();
            }       
         } 
      property Single stemsenesceddigestibilityavg
         {
         Single get()
            {
            return Comp->Variable[ "stemsenesceddigestibilityavg" ]->ToSingle();
            }       
         } 
      property Single stemsenesceddigestibilitymax
         {
         Single get()
            {
            return Comp->Variable[ "stemsenesceddigestibilitymax" ]->ToSingle();
            }       
         } 
      property Single stemsenesceddigestibilitymin
         {
         Single get()
            {
            return Comp->Variable[ "stemsenesceddigestibilitymin" ]->ToSingle();
            }       
         } 
      property Single stemsenescedn
         {
         Single get()
            {
            return Comp->Variable[ "stemsenescedn" ]->ToSingle();
            }       
         } 
      property Single stemsenescednconc
         {
         Single get()
            {
            return Comp->Variable[ "stemsenescednconc" ]->ToSingle();
            }       
         } 
      property Single stemsenescednonstructuralwt
         {
         Single get()
            {
            return Comp->Variable[ "stemsenescednonstructuralwt" ]->ToSingle();
            }       
         } 
      property Single stemsenescedp
         {
         Single get()
            {
            return Comp->Variable[ "stemsenescedp" ]->ToSingle();
            }       
         } 
      property Single stemsenescedpconc
         {
         Single get()
            {
            return Comp->Variable[ "stemsenescedpconc" ]->ToSingle();
            }       
         } 
      property Single stemsenescedremovedn
         {
         Single get()
            {
            return Comp->Variable[ "stemsenescedremovedn" ]->ToSingle();
            }       
         } 
      property Single stemsenescedremovedp
         {
         Single get()
            {
            return Comp->Variable[ "stemsenescedremovedp" ]->ToSingle();
            }       
         } 
      property Single stemsenescedremovedwt
         {
         Single get()
            {
            return Comp->Variable[ "stemsenescedremovedwt" ]->ToSingle();
            }       
         } 
      property Single stemsenescedstructuralwt
         {
         Single get()
            {
            return Comp->Variable[ "stemsenescedstructuralwt" ]->ToSingle();
            }       
         } 
      property Single stemsenescedwt
         {
         Single get()
            {
            return Comp->Variable[ "stemsenescedwt" ]->ToSingle();
            }       
         } 
      property Single stemsenescingn
         {
         Single get()
            {
            return Comp->Variable[ "stemsenescingn" ]->ToSingle();
            }       
         } 
      property Single stemsenescingp
         {
         Single get()
            {
            return Comp->Variable[ "stemsenescingp" ]->ToSingle();
            }       
         } 
      property Single stemsenescingwt
         {
         Single get()
            {
            return Comp->Variable[ "stemsenescingwt" ]->ToSingle();
            }       
         } 
      property Single stemtotaln
         {
         Single get()
            {
            return Comp->Variable[ "stemtotaln" ]->ToSingle();
            }       
         } 
      property Single stemtotalnconc
         {
         Single get()
            {
            return Comp->Variable[ "stemtotalnconc" ]->ToSingle();
            }       
         } 
      property Single stemtotalnonstructuralwt
         {
         Single get()
            {
            return Comp->Variable[ "stemtotalnonstructuralwt" ]->ToSingle();
            }       
         } 
      property Single stemtotalp
         {
         Single get()
            {
            return Comp->Variable[ "stemtotalp" ]->ToSingle();
            }       
         } 
      property Single stemtotalpconc
         {
         Single get()
            {
            return Comp->Variable[ "stemtotalpconc" ]->ToSingle();
            }       
         } 
      property Single stemtotalstructuralwt
         {
         Single get()
            {
            return Comp->Variable[ "stemtotalstructuralwt" ]->ToSingle();
            }       
         } 
      property Single stemtotalwt
         {
         Single get()
            {
            return Comp->Variable[ "stemtotalwt" ]->ToSingle();
            }       
         } 
      property Single stemvegetativen
         {
         Single get()
            {
            return Comp->Variable[ "stemvegetativen" ]->ToSingle();
            }       
         } 
      property Single stemvegetativenconc
         {
         Single get()
            {
            return Comp->Variable[ "stemvegetativenconc" ]->ToSingle();
            }       
         } 
      property Single stemvegetativenonstructuralwt
         {
         Single get()
            {
            return Comp->Variable[ "stemvegetativenonstructuralwt" ]->ToSingle();
            }       
         } 
      property Single stemvegetativep
         {
         Single get()
            {
            return Comp->Variable[ "stemvegetativep" ]->ToSingle();
            }       
         } 
      property Single stemvegetativepconc
         {
         Single get()
            {
            return Comp->Variable[ "stemvegetativepconc" ]->ToSingle();
            }       
         } 
      property Single stemvegetativestructuralwt
         {
         Single get()
            {
            return Comp->Variable[ "stemvegetativestructuralwt" ]->ToSingle();
            }       
         } 
      property Single stemvegetativetotaln
         {
         Single get()
            {
            return Comp->Variable[ "stemvegetativetotaln" ]->ToSingle();
            }       
         } 
      property Single stemvegetativetotalnconc
         {
         Single get()
            {
            return Comp->Variable[ "stemvegetativetotalnconc" ]->ToSingle();
            }       
         } 
      property Single stemvegetativetotalnonstructuralwt
         {
         Single get()
            {
            return Comp->Variable[ "stemvegetativetotalnonstructuralwt" ]->ToSingle();
            }       
         } 
      property Single stemvegetativetotalp
         {
         Single get()
            {
            return Comp->Variable[ "stemvegetativetotalp" ]->ToSingle();
            }       
         } 
      property Single stemvegetativetotalpconc
         {
         Single get()
            {
            return Comp->Variable[ "stemvegetativetotalpconc" ]->ToSingle();
            }       
         } 
      property Single stemvegetativetotalstructuralwt
         {
         Single get()
            {
            return Comp->Variable[ "stemvegetativetotalstructuralwt" ]->ToSingle();
            }       
         } 
      property Single stemvegetativetotalwt
         {
         Single get()
            {
            return Comp->Variable[ "stemvegetativetotalwt" ]->ToSingle();
            }       
         } 
      property Single stemvegetativewt
         {
         Single get()
            {
            return Comp->Variable[ "stemvegetativewt" ]->ToSingle();
            }       
         } 
      property Single stover_wt
         {
         Single get()
            {
            return Comp->Variable[ "stover_wt" ]->ToSingle();
            }       
         } 
      property Single sw_demand
         {
         Single get()
            {
            return Comp->Variable[ "sw_demand" ]->ToSingle();
            }       
         } 
      property Single sw_demand_te
         {
         Single get()
            {
            return Comp->Variable[ "sw_demand_te" ]->ToSingle();
            }       
         } 
      property Single sw_stress_expan
         {
         Single get()
            {
            return Comp->Variable[ "sw_stress_expan" ]->ToSingle();
            }       
         } 
      property Single sw_stress_fixation
         {
         Single get()
            {
            return Comp->Variable[ "sw_stress_fixation" ]->ToSingle();
            }       
         } 
      property Single sw_stress_pheno
         {
         Single get()
            {
            return Comp->Variable[ "sw_stress_pheno" ]->ToSingle();
            }       
         } 
      property Single sw_stress_photo
         {
         Single get()
            {
            return Comp->Variable[ "sw_stress_photo" ]->ToSingle();
            }       
         } 
      property Single sw_supply
         {
         Single get()
            {
            return Comp->Variable[ "sw_supply" ]->ToSingle();
            }       
         } 
      property array<Single>^ sw_supply_layr
         {
         array<Single>^ get()
            {
            return Comp->Variable[ "sw_supply_layr" ]->ToSingleArray();
            }       
         } 
      property array<Single>^ sw_uptake
         {
         array<Single>^ get()
            {
            return Comp->Variable[ "sw_uptake" ]->ToSingleArray();
            }       
         } 
      property Single swdef_expan
         {
         Single get()
            {
            return Comp->Variable[ "swdef_expan" ]->ToSingle();
            }       
         } 
      property Single swdef_fixation
         {
         Single get()
            {
            return Comp->Variable[ "swdef_fixation" ]->ToSingle();
            }       
         } 
      property Single swdef_pheno
         {
         Single get()
            {
            return Comp->Variable[ "swdef_pheno" ]->ToSingle();
            }       
         } 
      property Single swdef_photo
         {
         Single get()
            {
            return Comp->Variable[ "swdef_photo" ]->ToSingle();
            }       
         } 
      property Single temp_stress_photo
         {
         Single get()
            {
            return Comp->Variable[ "temp_stress_photo" ]->ToSingle();
            }       
         } 
      property Single temp_stress_photo_co2
         {
         Single get()
            {
            return Comp->Variable[ "temp_stress_photo_co2" ]->ToSingle();
            }       
         } 
      property Single tlai
         {
         Single get()
            {
            return Comp->Variable[ "tlai" ]->ToSingle();
            }       
         } 
      property Single topsdetachingn
         {
         Single get()
            {
            return Comp->Variable[ "topsdetachingn" ]->ToSingle();
            }       
         } 
      property Single topsdetachingp
         {
         Single get()
            {
            return Comp->Variable[ "topsdetachingp" ]->ToSingle();
            }       
         } 
      property Single topsdetachingwt
         {
         Single get()
            {
            return Comp->Variable[ "topsdetachingwt" ]->ToSingle();
            }       
         } 
      property Single topsgrainn
         {
         Single get()
            {
            return Comp->Variable[ "topsgrainn" ]->ToSingle();
            }       
         } 
      property Single topsgrainnconc
         {
         Single get()
            {
            return Comp->Variable[ "topsgrainnconc" ]->ToSingle();
            }       
         } 
      property Single topsgrainnonstructuralwt
         {
         Single get()
            {
            return Comp->Variable[ "topsgrainnonstructuralwt" ]->ToSingle();
            }       
         } 
      property Single topsgrainp
         {
         Single get()
            {
            return Comp->Variable[ "topsgrainp" ]->ToSingle();
            }       
         } 
      property Single topsgrainpconc
         {
         Single get()
            {
            return Comp->Variable[ "topsgrainpconc" ]->ToSingle();
            }       
         } 
      property Single topsgrainstructuralwt
         {
         Single get()
            {
            return Comp->Variable[ "topsgrainstructuralwt" ]->ToSingle();
            }       
         } 
      property Single topsgraintotaln
         {
         Single get()
            {
            return Comp->Variable[ "topsgraintotaln" ]->ToSingle();
            }       
         } 
      property Single topsgraintotalnconc
         {
         Single get()
            {
            return Comp->Variable[ "topsgraintotalnconc" ]->ToSingle();
            }       
         } 
      property Single topsgraintotalnonstructuralwt
         {
         Single get()
            {
            return Comp->Variable[ "topsgraintotalnonstructuralwt" ]->ToSingle();
            }       
         } 
      property Single topsgraintotalp
         {
         Single get()
            {
            return Comp->Variable[ "topsgraintotalp" ]->ToSingle();
            }       
         } 
      property Single topsgraintotalpconc
         {
         Single get()
            {
            return Comp->Variable[ "topsgraintotalpconc" ]->ToSingle();
            }       
         } 
      property Single topsgraintotalstructuralwt
         {
         Single get()
            {
            return Comp->Variable[ "topsgraintotalstructuralwt" ]->ToSingle();
            }       
         } 
      property Single topsgraintotalwt
         {
         Single get()
            {
            return Comp->Variable[ "topsgraintotalwt" ]->ToSingle();
            }       
         } 
      property Single topsgrainwt
         {
         Single get()
            {
            return Comp->Variable[ "topsgrainwt" ]->ToSingle();
            }       
         } 
      property Single topsgreenn
         {
         Single get()
            {
            return Comp->Variable[ "topsgreenn" ]->ToSingle();
            }       
         } 
      property Single topsgreennconc
         {
         Single get()
            {
            return Comp->Variable[ "topsgreennconc" ]->ToSingle();
            }       
         } 
      property Single topsgreennonstructuralwt
         {
         Single get()
            {
            return Comp->Variable[ "topsgreennonstructuralwt" ]->ToSingle();
            }       
         } 
      property Single topsgreenp
         {
         Single get()
            {
            return Comp->Variable[ "topsgreenp" ]->ToSingle();
            }       
         } 
      property Single topsgreenpconc
         {
         Single get()
            {
            return Comp->Variable[ "topsgreenpconc" ]->ToSingle();
            }       
         } 
      property Single topsgreenremovedn
         {
         Single get()
            {
            return Comp->Variable[ "topsgreenremovedn" ]->ToSingle();
            }       
         } 
      property Single topsgreenremovedp
         {
         Single get()
            {
            return Comp->Variable[ "topsgreenremovedp" ]->ToSingle();
            }       
         } 
      property Single topsgreenremovedwt
         {
         Single get()
            {
            return Comp->Variable[ "topsgreenremovedwt" ]->ToSingle();
            }       
         } 
      property Single topsgreenstructuralwt
         {
         Single get()
            {
            return Comp->Variable[ "topsgreenstructuralwt" ]->ToSingle();
            }       
         } 
      property Single topsgreenwt
         {
         Single get()
            {
            return Comp->Variable[ "topsgreenwt" ]->ToSingle();
            }       
         } 
      property Single topsgrowthn
         {
         Single get()
            {
            return Comp->Variable[ "topsgrowthn" ]->ToSingle();
            }       
         } 
      property Single topsgrowthp
         {
         Single get()
            {
            return Comp->Variable[ "topsgrowthp" ]->ToSingle();
            }       
         } 
      property Single topsgrowthwt
         {
         Single get()
            {
            return Comp->Variable[ "topsgrowthwt" ]->ToSingle();
            }       
         } 
      property Single topsretranslocationn
         {
         Single get()
            {
            return Comp->Variable[ "topsretranslocationn" ]->ToSingle();
            }       
         } 
      property Single topsretranslocationp
         {
         Single get()
            {
            return Comp->Variable[ "topsretranslocationp" ]->ToSingle();
            }       
         } 
      property Single topsretranslocationwt
         {
         Single get()
            {
            return Comp->Variable[ "topsretranslocationwt" ]->ToSingle();
            }       
         } 
      property Single topssenescedn
         {
         Single get()
            {
            return Comp->Variable[ "topssenescedn" ]->ToSingle();
            }       
         } 
      property Single topssenescednconc
         {
         Single get()
            {
            return Comp->Variable[ "topssenescednconc" ]->ToSingle();
            }       
         } 
      property Single topssenescednonstructuralwt
         {
         Single get()
            {
            return Comp->Variable[ "topssenescednonstructuralwt" ]->ToSingle();
            }       
         } 
      property Single topssenescedp
         {
         Single get()
            {
            return Comp->Variable[ "topssenescedp" ]->ToSingle();
            }       
         } 
      property Single topssenescedpconc
         {
         Single get()
            {
            return Comp->Variable[ "topssenescedpconc" ]->ToSingle();
            }       
         } 
      property Single topssenescedremovedn
         {
         Single get()
            {
            return Comp->Variable[ "topssenescedremovedn" ]->ToSingle();
            }       
         } 
      property Single topssenescedremovedp
         {
         Single get()
            {
            return Comp->Variable[ "topssenescedremovedp" ]->ToSingle();
            }       
         } 
      property Single topssenescedremovedwt
         {
         Single get()
            {
            return Comp->Variable[ "topssenescedremovedwt" ]->ToSingle();
            }       
         } 
      property Single topssenescedstructuralwt
         {
         Single get()
            {
            return Comp->Variable[ "topssenescedstructuralwt" ]->ToSingle();
            }       
         } 
      property Single topssenescedwt
         {
         Single get()
            {
            return Comp->Variable[ "topssenescedwt" ]->ToSingle();
            }       
         } 
      property Single topssenescingn
         {
         Single get()
            {
            return Comp->Variable[ "topssenescingn" ]->ToSingle();
            }       
         } 
      property Single topssenescingp
         {
         Single get()
            {
            return Comp->Variable[ "topssenescingp" ]->ToSingle();
            }       
         } 
      property Single topssenescingwt
         {
         Single get()
            {
            return Comp->Variable[ "topssenescingwt" ]->ToSingle();
            }       
         } 
      property Single topstotaln
         {
         Single get()
            {
            return Comp->Variable[ "topstotaln" ]->ToSingle();
            }       
         } 
      property Single topstotalnconc
         {
         Single get()
            {
            return Comp->Variable[ "topstotalnconc" ]->ToSingle();
            }       
         } 
      property Single topstotalnonstructuralwt
         {
         Single get()
            {
            return Comp->Variable[ "topstotalnonstructuralwt" ]->ToSingle();
            }       
         } 
      property Single topstotalp
         {
         Single get()
            {
            return Comp->Variable[ "topstotalp" ]->ToSingle();
            }       
         } 
      property Single topstotalpconc
         {
         Single get()
            {
            return Comp->Variable[ "topstotalpconc" ]->ToSingle();
            }       
         } 
      property Single topstotalstructuralwt
         {
         Single get()
            {
            return Comp->Variable[ "topstotalstructuralwt" ]->ToSingle();
            }       
         } 
      property Single topstotalwt
         {
         Single get()
            {
            return Comp->Variable[ "topstotalwt" ]->ToSingle();
            }       
         } 
      property Single topsvegetativen
         {
         Single get()
            {
            return Comp->Variable[ "topsvegetativen" ]->ToSingle();
            }       
         } 
      property Single topsvegetativenconc
         {
         Single get()
            {
            return Comp->Variable[ "topsvegetativenconc" ]->ToSingle();
            }       
         } 
      property Single topsvegetativenonstructuralwt
         {
         Single get()
            {
            return Comp->Variable[ "topsvegetativenonstructuralwt" ]->ToSingle();
            }       
         } 
      property Single topsvegetativep
         {
         Single get()
            {
            return Comp->Variable[ "topsvegetativep" ]->ToSingle();
            }       
         } 
      property Single topsvegetativepconc
         {
         Single get()
            {
            return Comp->Variable[ "topsvegetativepconc" ]->ToSingle();
            }       
         } 
      property Single topsvegetativestructuralwt
         {
         Single get()
            {
            return Comp->Variable[ "topsvegetativestructuralwt" ]->ToSingle();
            }       
         } 
      property Single topsvegetativetotaln
         {
         Single get()
            {
            return Comp->Variable[ "topsvegetativetotaln" ]->ToSingle();
            }       
         } 
      property Single topsvegetativetotalnconc
         {
         Single get()
            {
            return Comp->Variable[ "topsvegetativetotalnconc" ]->ToSingle();
            }       
         } 
      property Single topsvegetativetotalnonstructuralwt
         {
         Single get()
            {
            return Comp->Variable[ "topsvegetativetotalnonstructuralwt" ]->ToSingle();
            }       
         } 
      property Single topsvegetativetotalp
         {
         Single get()
            {
            return Comp->Variable[ "topsvegetativetotalp" ]->ToSingle();
            }       
         } 
      property Single topsvegetativetotalpconc
         {
         Single get()
            {
            return Comp->Variable[ "topsvegetativetotalpconc" ]->ToSingle();
            }       
         } 
      property Single topsvegetativetotalstructuralwt
         {
         Single get()
            {
            return Comp->Variable[ "topsvegetativetotalstructuralwt" ]->ToSingle();
            }       
         } 
      property Single topsvegetativetotalwt
         {
         Single get()
            {
            return Comp->Variable[ "topsvegetativetotalwt" ]->ToSingle();
            }       
         } 
      property Single topsvegetativewt
         {
         Single get()
            {
            return Comp->Variable[ "topsvegetativewt" ]->ToSingle();
            }       
         } 
      property Single totaln
         {
         Single get()
            {
            return Comp->Variable[ "totaln" ]->ToSingle();
            }       
         } 
      property Single totalnconc
         {
         Single get()
            {
            return Comp->Variable[ "totalnconc" ]->ToSingle();
            }       
         } 
      property Single totalnonstructuralwt
         {
         Single get()
            {
            return Comp->Variable[ "totalnonstructuralwt" ]->ToSingle();
            }       
         } 
      property Single totalp
         {
         Single get()
            {
            return Comp->Variable[ "totalp" ]->ToSingle();
            }       
         } 
      property Single totalpconc
         {
         Single get()
            {
            return Comp->Variable[ "totalpconc" ]->ToSingle();
            }       
         } 
      property Single totalstructuralwt
         {
         Single get()
            {
            return Comp->Variable[ "totalstructuralwt" ]->ToSingle();
            }       
         } 
      property Single totalwt
         {
         Single get()
            {
            return Comp->Variable[ "totalwt" ]->ToSingle();
            }       
         } 
      property Single transp_eff
         {
         Single get()
            {
            return Comp->Variable[ "transp_eff" ]->ToSingle();
            }       
         } 
      property Single tt
         {
         Single get()
            {
            return Comp->Variable[ "tt" ]->ToSingle();
            }       
         } 
      property Single ttafteremergence
         {
         Single get()
            {
            return Comp->Variable[ "ttafteremergence" ]->ToSingle();
            }       
         } 
      property Single ttafterend_crop
         {
         Single get()
            {
            return Comp->Variable[ "ttafterend_crop" ]->ToSingle();
            }       
         } 
      property Single ttafterend_grain_fill
         {
         Single get()
            {
            return Comp->Variable[ "ttafterend_grain_fill" ]->ToSingle();
            }       
         } 
      property Single ttafterend_of_juvenile
         {
         Single get()
            {
            return Comp->Variable[ "ttafterend_of_juvenile" ]->ToSingle();
            }       
         } 
      property Single ttafterfloral_initiation
         {
         Single get()
            {
            return Comp->Variable[ "ttafterfloral_initiation" ]->ToSingle();
            }       
         } 
      property Single ttafterflowering
         {
         Single get()
            {
            return Comp->Variable[ "ttafterflowering" ]->ToSingle();
            }       
         } 
      property Single ttaftergermination
         {
         Single get()
            {
            return Comp->Variable[ "ttaftergermination" ]->ToSingle();
            }       
         } 
      property Single ttafterharvest_ripe
         {
         Single get()
            {
            return Comp->Variable[ "ttafterharvest_ripe" ]->ToSingle();
            }       
         } 
      property Single ttaftermaturity
         {
         Single get()
            {
            return Comp->Variable[ "ttaftermaturity" ]->ToSingle();
            }       
         } 
      property Single ttafterout
         {
         Single get()
            {
            return Comp->Variable[ "ttafterout" ]->ToSingle();
            }       
         } 
      property Single ttaftersowing
         {
         Single get()
            {
            return Comp->Variable[ "ttaftersowing" ]->ToSingle();
            }       
         } 
      property Single ttafterstart_grain_fill
         {
         Single get()
            {
            return Comp->Variable[ "ttafterstart_grain_fill" ]->ToSingle();
            }       
         } 
      property String^ type
         {
         String^ get()
            {
            return Comp->Variable[ "type" ]->ToString();
            }       
         } 
      property Single vegetativen
         {
         Single get()
            {
            return Comp->Variable[ "vegetativen" ]->ToSingle();
            }       
         } 
      property Single vegetativenconc
         {
         Single get()
            {
            return Comp->Variable[ "vegetativenconc" ]->ToSingle();
            }       
         } 
      property Single vegetativenonstructuralwt
         {
         Single get()
            {
            return Comp->Variable[ "vegetativenonstructuralwt" ]->ToSingle();
            }       
         } 
      property Single vegetativep
         {
         Single get()
            {
            return Comp->Variable[ "vegetativep" ]->ToSingle();
            }       
         } 
      property Single vegetativepconc
         {
         Single get()
            {
            return Comp->Variable[ "vegetativepconc" ]->ToSingle();
            }       
         } 
      property Single vegetativestructuralwt
         {
         Single get()
            {
            return Comp->Variable[ "vegetativestructuralwt" ]->ToSingle();
            }       
         } 
      property Single vegetativetotaln
         {
         Single get()
            {
            return Comp->Variable[ "vegetativetotaln" ]->ToSingle();
            }       
         } 
      property Single vegetativetotalnconc
         {
         Single get()
            {
            return Comp->Variable[ "vegetativetotalnconc" ]->ToSingle();
            }       
         } 
      property Single vegetativetotalnonstructuralwt
         {
         Single get()
            {
            return Comp->Variable[ "vegetativetotalnonstructuralwt" ]->ToSingle();
            }       
         } 
      property Single vegetativetotalp
         {
         Single get()
            {
            return Comp->Variable[ "vegetativetotalp" ]->ToSingle();
            }       
         } 
      property Single vegetativetotalpconc
         {
         Single get()
            {
            return Comp->Variable[ "vegetativetotalpconc" ]->ToSingle();
            }       
         } 
      property Single vegetativetotalstructuralwt
         {
         Single get()
            {
            return Comp->Variable[ "vegetativetotalstructuralwt" ]->ToSingle();
            }       
         } 
      property Single vegetativetotalwt
         {
         Single get()
            {
            return Comp->Variable[ "vegetativetotalwt" ]->ToSingle();
            }       
         } 
      property Single vegetativewt
         {
         Single get()
            {
            return Comp->Variable[ "vegetativewt" ]->ToSingle();
            }       
         } 
      property String^ version
         {
         String^ get()
            {
            return Comp->Variable[ "version" ]->ToString();
            }       
         } 
      property Single width
         {
         Single get()
            {
            return Comp->Variable[ "width" ]->ToSingle();
            }       
         } 
      property array<Single>^ xf
         {
         array<Single>^ get()
            {
            return Comp->Variable[ "xf" ]->ToSingleArray();
            }       
         } 
      property Single yield
         {
         Single get()
            {
            return Comp->Variable[ "yield" ]->ToSingle();
            }       
         } 
      property Single zadok_stage
         {
         Single get()
            {
            return Comp->Variable[ "zadok_stage" ]->ToSingle();
            }       
         } 
   };
