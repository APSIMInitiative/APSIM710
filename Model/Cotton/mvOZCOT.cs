
using System;
using System.Collections.Generic;
using System.Linq;
using System.Xml;
using CSGeneral;
using CMPServices;       //xml and TTypedValues (NOTE: TTypedValues are 1 based arrays!)

namespace ManagedComponent.MvOZCOT
{
    //==========================================================================
    /// <summary>
    /// This is a user object for containing the OZCOT CMP model
    /// </summary>
    //==========================================================================
    public class mvOZCOTClass : TBaseComp
    {
        //set some instance specific information
        private static String _STYPE = "Cotton model";  
        private static String _SVERSION = "2.00";
        private static String _SAUTHOR = "CSIRO Agriculture - Cotton Research";


        //subscribed events
        private const int EVENT_SOW = 1;           //sow using 5 input parameters  (APSIM oriented)
        private const int EVENT_PROCESS = 2;       //APSIM oriented  - pre, process, post  events
        private const int EVENT_HARVEST = 3;       //APSIM oriented
        private const int EVENT_RESET = 4;         //APSIM oriented
        private const int EVENT_TICK = 5;          //standard CMP compliant event for time step
        private const int EVENT_DO_GROWTH = 6;     //CPI (AusFarm) oriented event for daily plant growth
        private const int EVENT_DO_SOW = 7;        //sow with no parameters (CPI oriented). All values initialised.
        private const int EVENT_DO_HARVEST = 8;    //CPI (AusFarm) oriented
        private const int EVENT_DEFOLIATE = 9;     
        private const int EVENT_END_CROP = 10;
        private const int EVENT_KILL_CROP = 11;   

 

        //published events
        private const int evtSowing = 20;
        private const int evtHarvesting = 21;
        private const int evtHarvested = 22;
        private const int evtBiomassRemoved = 23;
        private const int evtIncorpFOM = 24;  
        private const int evtNewCrop = 25;       

        //TODO: DBJ  should these events be published? What conditions act as triggers?  
        //private const int evtExternalMassFlow = ?;



        //event states
        private const int SOW_STATE_ACQUIRE = 1;
        private const int SOW_STATE_EXECUTE = 2;
        private const int GROWTH_STATE_ACQUIRE = 1;
        private const int GROWTH_STATE_EXECUTE = 2;
        private const int HARVEST_STATE_ACQUIRE = 1;
        private const int HARVEST_STATE_EXECUTE = 2;
        private const int DEFOLIATE_STATE_ACQUIRE = 1;
        private const int DEFOLIATE_STATE_EXECUTE = 2;
        private const int KILL_CROP_STATE_ACQUIRE = 1;
        private const int KILL_CROP_STATE_EXECUTE = 2;
        private const int END_CROP_STATE_ACQUIRE = 1;
        private const int END_CROP_STATE_EXECUTE = 2;
        private const int RESET_STATE_DO = 1;
        private const int TICK_STATE_DO = 1;

        //driver properties   text string defined in addDriver()  
        //                    value assigned to local var in assignDriver()
        private const int DRV_MAXT = 1;
        private const int DRV_MINT = 2;
        private const int DRV_RADN = 3;
        private const int DRV_RAIN = 4;
        //
        //soilwater driver properties
        private const int DRV_DLAYR = 5;
        private const int DRV_BULKD = 6;
        private const int DRV_LL15 = 7;
        private const int DRV_DUL = 8;
        private const int DRV_SAT = 9;
        private const int DRV_SW = 10;
        private const int DRV_EO = 11;
        private const int DRV_ES = 12;
        private const int DRV_RUNOFF = 13;

        //soilN driver properties (for APSIM SOILN)
        private const int DRV_NO3_MIN = 14;
        private const int DRV_NO3 = 15;
        private const int DRV_NH4_MIN = 16;
        private const int DRV_NH4 = 17;
        private const int DRV_UREA = 18;

        //CO2 driver properties (for Climate Change)
        private const int DRV_CO2 = 19;




        //owned properties        init properties defined with text value by addProperty() in constructor
        //                        assigned to local var in initProperty()
        //                        also  assigned to local var in writeProperty()
        //                        also  gets value from local var value in readProperty()
        private const int PROP_crop_type = PROP_START_INDEX + 0;
        //private const int PROP_row_spacing_default = PROP_START_INDEX + 1;  // and so forth for owned properties that will be accessed by other components
        //private const int PROP_skiprow_default = PROP_START_INDEX + 2;
        private const int PROP_leaf_res_n_conc = PROP_START_INDEX + 3;
        private const int PROP_hucut = PROP_START_INDEX + 4;
        private const int PROP_baset = PROP_START_INDEX + 5;
        // private const int PROP_ul1 = PROP_START_INDEX + 6;  soil property
        // private const int PROP_cona = PROP_START_INDEX + 7; soil property
        private const int PROP_open_def = PROP_START_INDEX + 8;
        private const int PROP_a_root_leaf = PROP_START_INDEX + 9;
        private const int PROP_a_stem_leaf = PROP_START_INDEX + 10;
        private const int PROP_e_par = PROP_START_INDEX + 11;
        private const int PROP_specific_lw = PROP_START_INDEX + 12;
        private const int PROP_t_opt = PROP_START_INDEX + 13;
        private const int PROP_t_base = PROP_START_INDEX + 14;
        private const int PROP_wt_area_max = PROP_START_INDEX + 15;
        private const int PROP_embryo = PROP_START_INDEX + 16;
        private const int PROP_f_leaf = PROP_START_INDEX + 17;
        private const int PROP_f_stem = PROP_START_INDEX + 18;
        private const int PROP_f_root = PROP_START_INDEX + 19;
        private const int PROP_elevation_default = PROP_START_INDEX + 20;
        private const int PROP_wlog_assimilate_red = PROP_START_INDEX + 21;
        private const int PROP_wlog_carcap_red = PROP_START_INDEX + 22;
        private const int PROP_watlog_c = PROP_START_INDEX + 23;
        private const int PROP_watlog_n = PROP_START_INDEX + 24;
        private const int PROP_wlog_carcap_red_stress = PROP_START_INDEX + 25;
        private const int PROP_smi_affect_wlog = PROP_START_INDEX + 26;
        private const int PROP_days_relief_wlog = PROP_START_INDEX + 27;
        private const int PROP_frost_kill_immediate = PROP_START_INDEX + 28;
        private const int PROP_rtdep_max = PROP_START_INDEX + 29;
        private const int PROP_harvest_n_frac = PROP_START_INDEX + 30;
        private const int PROP_cutout_smi_crit = PROP_START_INDEX + 31;
        private const int PROP_cutout_smi_days = PROP_START_INDEX + 32;
        private const int PROP_cutout_smi_site_red = PROP_START_INDEX + 33;
        private const int PROP_epcoef1 = PROP_START_INDEX + 34;
        private const int PROP_epcoef2 = PROP_START_INDEX + 35;
        private const int PROP_epcoef_smi_crit = PROP_START_INDEX + 36;
        private const int PROP_fbwstr_low = PROP_START_INDEX + 37;
        private const int PROP_fbwstr_high = PROP_START_INDEX + 38;
        private const int PROP_fbwstr_a = PROP_START_INDEX + 39;
        private const int PROP_fbnstr_low = PROP_START_INDEX + 40;
        private const int PROP_fbnstr_high = PROP_START_INDEX + 41;
        private const int PROP_fbnstr_a = PROP_START_INDEX + 42;
        private const int PROP_relp_smi_crit = PROP_START_INDEX + 43;
        private const int PROP_relp_intercept = PROP_START_INDEX + 44;
        private const int PROP_relp_slope = PROP_START_INDEX + 45;
        private const int PROP_relp_low = PROP_START_INDEX + 46;
        private const int PROP_relp_high = PROP_START_INDEX + 47;
        private const int PROP_relp_a = PROP_START_INDEX + 48;
        private const int PROP_vsnstr_low = PROP_START_INDEX + 49;
        private const int PROP_vsnstr_high = PROP_START_INDEX + 50;
        private const int PROP_vsnstr_a = PROP_START_INDEX + 51;
        private const int PROP_flfsmi_low = PROP_START_INDEX + 52;
        private const int PROP_flfsmi_high = PROP_START_INDEX + 53;
        private const int PROP_flfsmi_a = PROP_START_INDEX + 54;
        private const int PROP_vlnstr_low = PROP_START_INDEX + 55;
        private const int PROP_vlnstr_high = PROP_START_INDEX + 56;
        private const int PROP_vlnstr_a = PROP_START_INDEX + 57;
        private const int PROP_fw_low = PROP_START_INDEX + 58;
        private const int PROP_fw_high = PROP_START_INDEX + 59;
        private const int PROP_fw_a = PROP_START_INDEX + 60;
        private const int PROP_adjust_low = PROP_START_INDEX + 61;
        private const int PROP_adjust_high = PROP_START_INDEX + 62;
        private const int PROP_adjust_a = PROP_START_INDEX + 63;
        private const int PROP_fwstrs_low = PROP_START_INDEX + 64;
        private const int PROP_fwstrs_high = PROP_START_INDEX + 65;
        private const int PROP_fwstrs_a = PROP_START_INDEX + 66;
        private const int PROP_smi_delay_crit = PROP_START_INDEX + 67;
        private const int PROP_cold_shock_delay_crit = PROP_START_INDEX + 68;
        private const int PROP_cold_shock_delay = PROP_START_INDEX + 69;
        private const int PROP_fert_crit = PROP_START_INDEX + 70;
        private const int PROP_fert_detect = PROP_START_INDEX + 71;
        private const int PROP_days_since_fert_max = PROP_START_INDEX + 72;
        private const int PROP_ll = PROP_START_INDEX + 73;
        private const int PROP_cultivar = PROP_START_INDEX + 74;
        private const int PROP_sow_depth = PROP_START_INDEX + 75;
        private const int PROP_row_spacing = PROP_START_INDEX + 76;
        private const int PROP_plants_per_m_row = PROP_START_INDEX + 77;
        private const int PROP_skiprow = PROP_START_INDEX + 78;
        private const int PROP_percent_l = PROP_START_INDEX + 79;
        private const int PROP_scboll = PROP_START_INDEX + 80;
        private const int PROP_respcon = PROP_START_INDEX + 81;
        private const int PROP_sqcon = PROP_START_INDEX + 82;
        private const int PROP_fcutout = PROP_START_INDEX + 83;
        private const int PROP_flai = PROP_START_INDEX + 84;
        private const int PROP_ddisq = PROP_START_INDEX + 85;
        private const int PROP_popcon = PROP_START_INDEX + 86;
        private const int PROP_acotyl = PROP_START_INDEX + 87;
        private const int PROP_rlai = PROP_START_INDEX + 88;
        private const int PROP_fburr = PROP_START_INDEX + 89;
        private const int PROP_dlds_max = PROP_START_INDEX + 90;
        private const int PROP_rate_emergence = PROP_START_INDEX + 91;
        private const int PROP_frudd = PROP_START_INDEX + 92;
        private const int PROP_bltme = PROP_START_INDEX + 93;
        private const int PROP_wt = PROP_START_INDEX + 94;
        private const int PROP_ozcot_status = PROP_START_INDEX + 95;
        private const int PROP_status = PROP_START_INDEX + 96;
        private const int PROP_ozcot_sumDD = PROP_START_INDEX + 97;
        private const int PROP_lai = PROP_START_INDEX + 98;
        private const int PROP_dw_total = PROP_START_INDEX + 99;
        private const int PROP_dw_boll = PROP_START_INDEX + 100;
        private const int PROP_dn_plant = PROP_START_INDEX + 101;
        private const int PROP_assimilate = PROP_START_INDEX + 102;
        private const int PROP_growthWt = PROP_START_INDEX + 103;
        private const int PROP_ep = PROP_START_INDEX + 104;
        private const int PROP_crop_in = PROP_START_INDEX + 105;
        // PROP_??  = PROP_START_INDEX + 106   vacant

        private const int PROP_das = PROP_START_INDEX + 107;
        private const int PROP_sites = PROP_START_INDEX + 108;
        private const int PROP_squarz = PROP_START_INDEX + 109;
        private const int PROP_fru_no_cat = PROP_START_INDEX + 110;
        private const int PROP_bollz = PROP_START_INDEX + 111;
        private const int PROP_openz = PROP_START_INDEX + 112;
        private const int PROP_lint = PROP_START_INDEX + 113;
        private const int PROP_openwt_kg = PROP_START_INDEX + 114;
        private const int PROP_frudw = PROP_START_INDEX + 115;
        private const int PROP_frudw_tot = PROP_START_INDEX + 116;
        private const int PROP_frudw_shed = PROP_START_INDEX + 117;
        private const int PROP_frun = PROP_START_INDEX + 118;
        private const int PROP_bload = PROP_START_INDEX + 119;
        private const int PROP_carcap_c = PROP_START_INDEX + 120;
        private const int PROP_vnstrs = PROP_START_INDEX + 121;
        private const int PROP_fnstrs = PROP_START_INDEX + 122;
        private const int PROP_dw_root = PROP_START_INDEX + 123;
        private const int PROP_dw_leaf = PROP_START_INDEX + 124;
        private const int PROP_dw_stem = PROP_START_INDEX + 125;
        private const int PROP_totnup = PROP_START_INDEX + 126;
        private const int PROP_yield = PROP_START_INDEX + 127;
        private const int PROP_lint_yield = PROP_START_INDEX + 128;
        private const int PROP_cover_green = PROP_START_INDEX + 129;
        private const int PROP_cover_tot = PROP_START_INDEX + 130;
        private const int PROP_availn = PROP_START_INDEX + 131;
        private const int PROP_uptakn = PROP_START_INDEX + 132;
        private const int PROP_tsno3 = PROP_START_INDEX + 133;
        private const int PROP_ysno3 = PROP_START_INDEX + 134;
        private const int PROP_tsnh4 = PROP_START_INDEX + 135;
        private const int PROP_ysnh4 = PROP_START_INDEX + 136;
        private const int PROP_d_nup = PROP_START_INDEX + 137;
        private const int PROP_rtdep = PROP_START_INDEX + 138;
        private const int PROP_s_bed_mi = PROP_START_INDEX + 139;
        private const int PROP_smi = PROP_START_INDEX + 140;
        private const int PROP_wli = PROP_START_INDEX + 141;
        private const int PROP_evap_plant = PROP_START_INDEX + 142;
        private const int PROP_evap_soil = PROP_START_INDEX + 143;
        private const int PROP_evap_pot = PROP_START_INDEX + 144;
        private const int PROP_evap_tot = PROP_START_INDEX + 145;
        private const int PROP_bolls_sc = PROP_START_INDEX + 146;
        private const int PROP_nuptake = PROP_START_INDEX + 147;
        private const int PROP_squarz_max = PROP_START_INDEX + 148;
        private const int PROP_lai_max = PROP_START_INDEX + 149;
        private const int PROP_defol_das = PROP_START_INDEX + 150;
        private const int PROP_defol2_das = PROP_START_INDEX + 151;
        private const int PROP_height = PROP_START_INDEX + 152;
        private const int PROP_carcap_n = PROP_START_INDEX + 153;
        private const int PROP_lint_bales = PROP_START_INDEX + 154;
        private const int PROP_lint_kg = PROP_START_INDEX + 155;
        private const int PROP_background_retention = PROP_START_INDEX + 156;

        private const int PROP_cultivar_default = PROP_START_INDEX + 157;
        private const int PROP_cultivars_list = PROP_START_INDEX + 162;
        private const int PROP_n_uptake = PROP_START_INDEX + 163;
        private const int PROP_ep_cm = PROP_START_INDEX + 164;
        private const int PROP_plant_status = PROP_START_INDEX + 165;

        private const int PROP_cottonCO2 = PROP_START_INDEX + 166;

        // private const int PROP_x_co2_fert = PROP_START_INDEX + 167;
        // private const int PROP_y_co2_fert = PROP_START_INDEX + 168;
        private const int PROP_x_stem_wt = PROP_START_INDEX + 169;
        private const int PROP_y_height = PROP_START_INDEX + 170;

        private const int PROP_dm_green = PROP_START_INDEX + 171;
        private const int PROP_dm_senesced = PROP_START_INDEX + 172;
        private const int PROP_dm_shed = PROP_START_INDEX + 173;
        private const int PROP_dm_harvested = PROP_START_INDEX + 174;
        private const int PROP_dlt_dm_live = PROP_START_INDEX + 175;
        private const int PROP_n_green = PROP_START_INDEX + 176;
        private const int PROP_n_senesced = PROP_START_INDEX + 177;

        private const int PROP_dm = PROP_START_INDEX + 178;
        private const int PROP_daysAfterSowing = PROP_START_INDEX + 179;

        private const int PROP_PlantStatus = PROP_START_INDEX + 180;          //dulicate of PROP_plant_status +165
        private const int PROP_CoverLive = PROP_START_INDEX + 181;            //dulicate of PROP_cover_green  +129
        private const int PROP_CoverTotal = PROP_START_INDEX + 182;           //dulicate of PROP_cover_tot +130

        private const int PROP_rootExpansionDelay = PROP_START_INDEX + 183;        //delay days to full root expansion (DAS)

        private const int PROP_useDDsumTo1stSq = PROP_START_INDEX + 184;           //flag to use sumDDto1stSq = true, or RateOfDevel = false (default)

        private const int PROP_ozcot_rwu = PROP_START_INDEX + 185;  //sv- 2015_04_01 needed because ep does not equal root water uptake summed over profile. THIS NEEDS TO BE FIXED

       // private const int PROP_openBollsArray = PROP_START_INDEX + 186;     //dbj- 2015_Nov openBolls details by cohort: cnt, dryWt, accumRain
       //                                                                       currently cannot pass 2D arrays to Manager2 
        private const int PROP_openBollCnts = PROP_START_INDEX + 186;         //dbj- 2015_Nov openBollCnts details openBollCnts by cohort
        private const int PROP_openBollWts = PROP_START_INDEX + 187;          //dbj- 2015_Nov openBollWts details openBollWts by cohort
        private const int PROP_openBollAccumRain = PROP_START_INDEX + 188;    //dbj- 2015_Nov openBollAccumRain details accumRain by cohort

        private const int PROP_FirstSquareDAS = PROP_START_INDEX + 189;     //FirstSquare as DaysAfterSowing
        private const int PROP_FirstFlowerDAS = PROP_START_INDEX + 190;     //FirstFlower as DaysAfterSowing
        private const int PROP_FirstOpenBollDAS = PROP_START_INDEX + 191;   //FirstOpenBoll as DaysAfterSowing

        private const int PROP_stage = PROP_START_INDEX + 192;        //crop development stage (numeric value: 1 - 7)
        private const int PROP_stageName = PROP_START_INDEX + 193;    //crop development stage name (string)


        // end of owned properties list


        //Setter properties - properties owned by other components
        //                    that are updated and 'set requested' by this component
        private int setterID;

        // flag to allow initialisation drivers to be read on first call only
        private bool firstCall = true;

        // DateTime value extracted from Tick Event
        private DateTime Today;

        //----------------------------------------------
        // component predefined constant values
        //----------------------------------------------
        // name of this module size (7) 
        private const string module_name = "Cotton";
        private const int max_layers = 100;

        private const double gm2kg = 0.001;             //converting grams to kilograms
        private const double sm2ha = 0.0001;            //converting square metres to hectares
        private const double kg2gm = 1000.0;            //converting kilograms to grams
        private const double ha2sm = 10000.0;           //converting hectares to square metres




        //Readable Properties DDML 
        public const string typeDouble2D = @"<type   array=""T"" >
                                                    <element  array=""T"" kind=""double"" >
                                                    </element>
                                             </type>";


        //Event DDML 
        public const string typeSow = @"<type name = ""Sow"" >
                                          <field name = ""Cultivar""            kind=""string"" />
                                          <field name = ""plants""              kind=""double"" />
                                          <field name = ""sowing_depth""        kind=""double"" />
                                          <field name = ""row_spacing""         kind=""double"" />
                                          <field name = ""SkipRow""             kind=""double"" />
                                          <field name = ""SkipPlant""           kind=""double"" />
                                          <field name = ""Establishment""       kind=""string"" />
                                          <field name = ""crop_class""          kind=""string"" />
                                          <field name = ""tiller_no_fertile""   kind=""string"" />
                                          <field name = ""Skip""                kind=""string"" />
                                          <field name = ""plants_pm""           kind=""double"" />
                                          <field name = ""Ratoon""              kind=""integer4"" />
                                          <field name = ""sbdur""               kind=""integer4"" />
                                          <field name = ""nplh""                kind=""double"" />
                                          <field name = ""nh""                  kind=""double"" />
                                          <field name = ""nplsb""               kind=""double"" />
                                          <field name = ""nplds""               kind=""double"" />
                                       </type>";



        public const string typeHarvest = @"<type name = ""Harvest"" >
                                           <field name = ""Plants""            kind=""double"" />
                                           <field name = ""Remove""            kind=""double"" />
                                           <field name = ""Height""            kind=""double"" />
                                           <field name = ""Report""            kind=""string"" />
                                       </type>";


        public const string typeKillCrop = @"<type name = ""KillCrop"" >
                                            <field name = ""KillFraction""     kind=""single"" />
                                       </type>";




        public const string typeCultivars = @"<type name=""cultivars""  array=""T"" >
                                                <element>
                                                  <field name=""cultivar""        kind=""string""  />
                                                  <field name=""percent_l""       kind=""double""  />
                                                  <field name=""scboll""          kind=""double""  />
                                                  <field name=""respcon""         kind=""double""  />
                                                  <field name=""sqcon""           kind=""double""  />
                                                  <field name=""fcutout""         kind=""double""  />
                                                  <field name=""flai""            kind=""double""  />
                                                  <field name=""DDISQ""           kind=""double""  />
                                                  <field name=""TIPOUT""          kind=""double""  />
                                                  <field name=""FRUDD""           kind=""double""    array=""T""  />
                                                  <field name=""BLTME""           kind=""double""    array=""T""  />
                                                  <field name=""WT""              kind=""double""    array=""T""  />
                                                  <field name=""dlds_max""        kind=""double""  />
                                                  <field name=""rate_emergence""  kind=""double""  />
                                                  <field name=""popcon""          kind=""double""  />
                                                  <field name=""fburr""           kind=""double""  />
                                                  <field name=""ACOTYL""          kind=""double""  />
                                                  <field name=""RLAI""            kind=""double""  />
                                                  <field name=""BckGndRetn""      kind=""double""  />
                                                </element>
                                              </type> ";


        // Published Event DDML
        public const string typeSowing = @"<type name=""Sowing"">
                                           </type>";

        public const string typeHarvesting = @"<type name=""Harvesting"">
                                               </type>";

        public const string typeNewCrop = @"<type name=""NewCrop"">
                                                   <field name=""crop_type""    kind=""string""   />
                                                   <field name=""sender""       kind=""string""   />
                                            </type>";


        public const string typeBiomassRemoved = @"<type name=""BiomassRemoved""> 
                                                    <field name=""crop_type""           descr="""" kind=""string""/> 
                                                    <field name=""dm_type""             descr="""" kind=""string""   array=""T""/> 
                                                    <field name=""dlt_crop_dm""         descr="""" kind=""single""   array=""T""   unit=""kg/ha""/>
                                                    <field name=""dlt_dm_n""            descr="""" kind=""single""   array=""T""   unit=""kg/ha""/>
                                                    <field name=""dlt_dm_p""            descr="""" kind=""single""   array=""T""   unit=""kg/ha""/>
                                                    <field name=""fraction_to_residue"" descr="""" kind=""single""   array=""T""/>
                                                </type>   ";


        
        // This is a full listing of this event type, not all available fields will be populated.
        public const string typeIncorpFOM = @"<type name=""FOMLayer"">
                                                   <field name=""Type""   kind=""string"" /> 
                                                   <field name=""Layer""  array=""T"">
                                                       <element>
                                                          <field name=""FOM"">
                                                              <field name=""amount""  kind=""single"" unit=""kg/ha"" /> 
                                                              <field name=""C""       kind=""single"" unit=""kg/ha"" /> 
                                                              <field name=""N""       kind=""single"" unit=""kg/ha"" /> 
                                                              <field name=""P""       kind=""single"" unit=""kg/ha"" /> 
                                                              <field name=""AshAlk""  kind=""single"" unit=""kg/ha"" /> 
                                                          </field>
                                                          <field name=""CNR""     kind=""single"" /> 
                                                          <field name=""LabileP"" kind=""single"" /> 
                                                       </element>
                                                   </field>
                                              </type>";



        
        //<!--!   !!!!! Sysbal types  !!!!!!!!!!!!!!!!!!!!!!-->
        //<type name = "ExternalMassFlow" >
        //   < field name="PoolClass" kind="string" unit="-"/>
        //   <field name = "FlowType" kind="string" unit="-"/>
        //   <field name = "C" kind="single" unit="kg/ha"/>
        //   <field name = "N" kind="single" unit="kg/ha"/>
        //   <field name = "P" kind="single" unit="kg/ha"/>
        //   <field name = "DM" kind="single" unit="kg/ha"/>
        //   <field name = "SW" kind="single" unit="mm"/>
        //</type>






        //     ! fruit age classes
        private const int small_sqz = 1;
        private const int medium_sqz = 2;
        private const int large_sqz = 3;
        private const int flowers = 4;
        private const int small_bolls = 5;
        private const int medium_bolls = 6;
        private const int large_bolls = 7;
        private const int inedible_bolls = 8;
        private const int open_bolls = 9;

        private const int max_categories = 9;

        private const int max_cohort = 300;

        private const double ozcot_kvalue = 1.0;

        private const int sw_sowing = 2;
        private const int rain_preboll = 3;
        private const int rain_postboll = 4;
        private const int rain_fallow = 5;
        private const int rain_pre_post_boll = 7;
        private const int et_accum = 8;

        private const int age1 = 1;
        private const int age3 = 3;
        private const int age6 = 6;
        private const int age7 = 7;

        private const int max_age = 7;

        //        ! crop status
        private const string status_alive = "alive";
        private const string status_dead = "dead";
        private const string status_out = "out";
        private const string crop_inactive = "inactive";



        //!  crop stage constant values
        private const int germination = 1;    //period from sowing to emergence
        private const int preSquaring = 2;    //period from emergence to First Square
        private const int firstSquare = 3;    //period from FS to FF
        private const int firstFlower = 4;    //period from FF to FOB
        private const int firstOpenBoll = 5;  //period from FOB to %OpenBolls marking maturity (default= 60% open)
        private const int mature = 6;         //period from defined %OpenBolls to end of leaf drop  (defoliation period)
        private const int harvest_ripe = 7;    //period from crop ready for harvest to being harvested
        // number of crop stages
        private const int max_stage = 7;

        private readonly string[] stageName = new string[] { "","germination", "presquaring", "squaring", "flowering", "openbolls", "mature", "harvest_ripe" };



        //!  crop parts constant values
        private const int root = 1;
        private const int leaf = 2;
        private const int stem = 3;
        private const int pod  = 4;  // pod or green bolls
        private const int meal = 5;  // 'meal' or seed in open bolls 
        private const int lint = 6;  // lint in open bolls
        private const int oil  = 7;  // seed oil
        // number of plant parts
        private const int max_part = 7;

        private readonly string[] part_name = new string[] { "", "root", "leaf", "stem", "pod", "meal", "lint", "oil" };  //zero based array

        // default values for planting
        private const double ppm_row_default = 10.0;            // plants per metre row
        private const double sowing_depth_default = 50.0;       // sowing depth mm
        private const double row_spacing_default = 1000.0;      // row spacing mm
        private const int skiprow_default = 0;                  // 0 = solid, 1 = single skip,  2 = double skip

        // arrays for dry matter accounting
        private double[] dm_green = new double[max_part + 1];       // dry matter of green (growing) crop (g/m2))
        private double[] dm_senesced = new double[max_part + 1];    // dry matter of senesced (dead) crop (g/m2))
        private double[] dm_shed = new double[max_part + 1];        // dry matter of shed (removed/lost) crop (g/m2))
        private double[] dm_harvested = new double[max_part + 1];   // dry matter of harvested crop (g/m2))

        // N concentration of crop components for dry matter and N accounting purposes (shedding and harvesting)
        private const double root_res_n_conc = 0.02;  // N concentration of root residue
        //private const double leaf_res_n_conc = 0.025;  // N concentration of leaf residue
        private double leaf_res_n_conc;  // N concentration of leaf residue  (init variable - user definable)
        private const double stem_res_n_conc = 0.015;  // N concentration of stem residue 
        private const double pod_res_n_conc = 0.02;   // avg N concentration of squares and green bolls (small)
        private const double meal_res_n_conc = 0.04;  // N concentration of cotton seed 
        private const double lint_res_n_conc = 0.02;  // N concentration of lint 

        // working arrays for reporting  (1 based arrays)
        private double[] dm_crop = new double[max_part + 1];    // dry matter of crop (g/m2) converted and reported as kg/ha
        private double[] dm_N = new double[max_part + 1];       // N content of dry matter (g/m2)



        // default co2 concentration ppm  
        // (OZCOT developed on photosynthetic equations based on 330ppm;  as of 2015 co2 is estimated to be close to 400ppm)
        private const double co2_default = 400.0;


        private const double c_uptakn_max = 240.0;  // kg/ha


        //----------------------------------------------
        // component variables (local names)
        //----------------------------------------------

        private int num_ll_vals;
        private double[] unul = new double[max_layers + 1];  //array of soil water values if LL values are unavailable

        // varietal parameters
        private double percent_l;   // as %
        private double scboll;
        private double respcon;
        private double sqcon;
        private double fcutout;
        private double flai;
        private double ddisq;
        private double dlds_max;
        private double popcon;
        private double acotyl;    // mm2
        private double rlai;
        private double[] frudd = new double[max_categories];  // 9 categories but 8 transition values. 8 values in '1' based array ==> array of 9
        private double[] bltme = new double[max_categories];  // 9 categories but 8 transition values. 8 values in '1' based array ==> array of 9
        private double[] wt = new double[max_categories];     // 9 categories but 8 transition values. 8 values in '1' based array ==> array of 9
        private double fburr;
        private double rate_emergence;   //  mm/dd
        private double background_retention;

        //adjusted values as used in the OZCOT functions
        private double acotyl_m2;  //acotyl is read in in mm2 but used in setting leaf area as m2
        private double pcLint_frac;  //percent_L is read in as percent value, but used as a proportion
        


        private string crop_type;
        private string cultivar_default;

        //jh      real dlds
        private double hucut;
        private double baset;
        private double open_def;



        // dry matter production calculation factors
        private double a_root_leaf;
        private double a_stem_leaf;
        private double specific_lw;
        private double t_opt;
        private double t_base;
        private double embryo;
        private double f_leaf;
        private double f_stem;
        private double f_root;
        private double wt_area_max;
        //jh         real    wt_area_min
        private double e_par;
        private double elevation_default;

        private double watlog_c;
        private double watlog_n;
        private double wlog_assimilate_red;	    // assimilate reduction with waterlogging
        private double wlog_carcap_red;	        // carrying capacity reduction with waterlogging
        private double wlog_carcap_red_stress;	// carrying capacity reduction with waterlogging after stress
        private double smi_affect_wlog;	        // level of water stress below which could affect waterlogging
        private double days_relief_wlog;	    // number of days of stress relief after which has no effect on waterlogging

        private int deadFlagCnt;                // count of days that the crop is effectively dead. After 5 days set cropStatus to 'status_dead'.

        private double frost_kill_immediate;	// min temperature below which immediate kill
        private double rtdep_max;
        private double harvest_n_frac;			// fraction of uptake n for potential n harvested

        private int rootExpansionDelay;         // delay in days from sowing until roots begin rapid expansion (default for Narrabri = 33)

        private bool useDDsumTo1stSq;           // flag to use RateOfDevelopmentToFirstSq  or  the old accumulation of DayDegrees to 1st Square

        private int cropStage;                  // stage of crop developement




        //       !c-----------------------------------------------------------------------------
        //       !c     if coutout due to water stress or full boll load ie smi < 0.75
        //       !c     10% of fruiting sites become inactive for frugen every day after 5 days
        //       !c-----------------------------------------------------------------------------
        private double cutout_smi_crit;
        private int cutout_smi_days;
        private double cutout_smi_site_red;

        private double smi_delay_crit;
        private double cold_shock_delay_crit;
        private double cold_shock_delay;


        private double epcoef1;
        private double epcoef2;
        private double epcoef_smi_crit;

        //       ! water stress on bolls
        private double fbwstr_low;	// lower limit
        private double fbwstr_high;	// upper limit
        private double fbwstr_a;	// power

        //       ! n stress
        private double fbnstr_low;
        private double fbnstr_high;
        private double fbnstr_a;

        //       ! water stress on photosynthesis
        private double relp_smi_crit;   // critical level of smi below which it affects pp
        private double relp_intercept;	// intercept  of f(smi) 
        private double relp_slope;	    // slope  of f(smi) 

        //       ! severity of effect of water stress on photosynthesis
        private double relp_low;
        private double relp_high;
        private double relp_a;

        //       ! n stress on fruiting site number
        private double vsnstr_low;
        private double vsnstr_high;
        private double vsnstr_a;

        //       ! water stress on pre-squaring lai
        private double flfsmi_low;
        private double flfsmi_high;
        private double flfsmi_a;

        //       ! n stress on lai
        private double vlnstr_low;
        private double vlnstr_high;
        private double vlnstr_a;

        //       ! effect of water stress on leaf senescence
        private double fw_low;
        private double fw_high;
        private double fw_a;

        //       ! plant n adjustment for supply/demand ratio
        private double adjust_low;
        private double adjust_high;
        private double adjust_a;

        //       ! water stress for boll growth
        private double fwstrs_low;
        private double fwstrs_high;
        private double fwstrs_a;

        private double fert_crit;
        private double fert_detect;
        private int days_since_fert_max;


        //------------------------

        private double applied_n;
        private double total_applied;

        //private string title;	        //   size * 15
        private string plant_status;	//   size * (10)
        private double wli;
        private double f_limiting;
        private double smi_row;
        private double smi_pre;
        private bool useskip;

        // met details
        private double tempmx;
        private double tempmn;
        private double solrad;
        private double rain_cm;
        private double rain_mm;
        //jh         real    epan
        private double tempdy;
        private double tempwt;
        private double wind;
        private double tempav;
        private double hunits;
        //jh v2001         real    eos
        // private double qa;  // upper boundary for solrad
        private double solrto;
        private double q;

        // evapotranspiration calculation values (cm not mm values)
        private double eo;
        private double es;
        private double ep;
        private double et;
        private double ho;

        private double tr;     //transmission of radiation on ground area basis
        private double f_intz;
        private double rtsw;
        private double vpd;
        private double bper;
        private double[] dlayr = new double[max_layers + 1];
        private double[] dlayr_cm = new double[max_layers + 1];
        private double[] ullayr = new double[max_layers + 1];
        private double[] dullayr = new double[max_layers + 1];
        private double[] stlayr = new double[max_layers + 1];
        private double[] swlayr = new double[max_layers + 1];
        private double sw;
        private double ul;
        private double[] bulkd = new double[max_layers + 1];

        private double wpwc;
        private double laiField;
        private double laiRow;
        private double rtdep;
        private string cultivar;   // name of cultivar (char * 20)
        private double row_space_mm;
        private double row_spacing;
        private double ppm2;
        private double sdepth;
        private double sow_depth_mm;
        private int skiprow;
        private double profileDepth;
        private double shedlf;
        private double smi;
        private double s;
        private double effectiveRowSpacing;
        private double pp;
        private double ps;
        //                 real    snaplc(2)
        private double snaplc;
        private double availn;
        //private double initialn;
        //                 real    soilnt
        private double uptakn;
        private double frun;
        private double plantn;
        private double seed_nc;
        private double[] frucat = new double[max_categories + 1];
        private double[,] fmkcat = new double[max_categories + 1, max_age + 1];
        private double dd;
        private double ddmerg;
        private double sumdd;
        private double bgrvar;
        private double frudw;
        private double squarz;
        private double bollz;
        private double openz;
        private double harvested;
        private double sites;
        private double sites1;
        private double size;
        private double bload;
        private double cutout;
        private double vnstrs;
        private double fnstrs;
        private double rad;
        private double carcap_c;
        private double carcap_n;

        private double[] fruno = new double[max_cohort + 1];
        private double[] fruwt = new double[max_cohort + 1];
        private double[,] frmark = new double[max_cohort + 1, max_age + 1];
        private double[] fyzage = new double[max_cohort + 1];
        private double[,] openBollsArray = new double[max_cohort + 1, 4];   // array details of open bolls [DAS, [0=unused,1=fruitCnt,2=fruitWt,3=cummRain]]
        private double[] dlai = new double[max_cohort + 1];
        private double alaiz;
        private double plntnz;

        private double alint;
        private double sqzx;

        private double s_bed_mi;
        private double delayDD;
        private double accumRateTo1stSqr;   // rate of development to 1st square (used in ozcot_istsq() )
        private double[] bpsum = new double[max_cohort + 1];
        private double bollgr;
        private double dlai_pot;
        private double assimilate;    // (g/m2)
        private double total_assimilate;
        private double height;

        private double reserve;
        private double res_cap;
        private double root_feedback;

        //Dry Matter and Dry Weights
        //  internal storage 'dm' as g/m2 ,  reported 'wt' as kg/ha
        private double dm_fruShedTot;     //dry weight of shed fruit whole of crop  (g/m2)
        private double dm_fruShedToday;   //dry weight of shed fruit today  (g/m2)
        private double dm_fruWtToday;     //dry weight of fruit grown today  (g/m2)
        private double dm_allBolls;       //dry weight of (green + open) bolls  (g/m2)
        private double dm_openBolls;      //dry weight of open bolls  (g/m2)
        private double dm_leaf;
        private double dm_stem;
        private double dm_meal;
        private double dm_lint;
        private double dm_total;         //above ground dry matter
        private double dm_root;
        private double[] ddm_l = new double[max_cohort + 1];   //change in leaf dry matter
        private double ddm_boll;
        private double ddm_leaf;
        private double ddm_root;
        private double ddm_root_max;
        private double ddm_stem;
        private double ddm_meal;
        private double ddm_lint;
        private double leaf_res;
        private double stem_res;
        private double root_res;
        private double leaf_res_n;
        private double stem_res_n;
       // private double pod_res_n;
        private double root_res_n;
        private double boll_n;
        private double leaf_n;
        private double stem_n;
        private double root_n;
        private double total_n;
        private double dn_plant;

        private double tsno3;
        private double tsnh4;
        private double[] no3mn = new double[max_layers + 1];
        private double[] nh4mn = new double[max_layers + 1];
        private double yest_tsn;
        private double yest_tsno3;
        private double yest_tsnh4;
        private double[] ano3 = new double[max_layers + 1];
        private double[] anh4 = new double[max_layers + 1];
        private double ppm2_target;
        private double ppm_row;  //property initialised
        private int nday_co;  //day counter to emergence
        private int nwet_co;
        private double sum_tmx;
        private double ave_tx;
        private double delay_emerg;
        private double dd_emerg;
        private double ppm_sown;
        private double ppm_emerge;
        private double ppm_establish;
        private double fail_emrg;
        private double f_die;

        private int last_iday;
        private int jdate;
        private int DOY;
        private int nlayr;
        private int nrtlayr;
        private int isw;
        private int iemrg;
        private int isow;
        private int isq;
        private bool firstCallIstSq;
        private int iday;

        private int[] lfru = new int[max_categories + 1];
        private int idayco;
        private int ilaiz;
        private int iplntn;
        private int isqzx;
        private int maturityDAS;
        private int defoliationCnt;
        private int defol1DAS;
        private int defol2DAS;
        private int DAS;
        private int iend;
        private int idayx;
        private int lastlf;
        private int n_cutout;
        private int istress;
        private int ireliefco;
        private bool initial_call;
        private int days_since_fert;
        private bool crop_in;	// is a crop in ? 
        private double[] sfmcat = new double[max_categories + 1];
        private double[] sw_start = new double[max_layers + 1];
        private int nsince;
        private double co2;
        private int firstFlowerDOY;
        private int firstFlowerDAS;
        private int firstOpenBollDOY;
        private int firstOpenBollDAS;

        //Greenhouse gas studies
        private double[] x_co2_fert = new double[20];
        private double[] y_co2_fert = new double[20];
        private int num_co2_fert = 0;

        //Plant height verses wt dry matter 
        private double[] x_stem_wt = new double[20];  //stem wt per plant
        private double[] y_height = new double[20];
        private int num_height = 0;

        //APSIM SoilWat2 specific arrays
        private double[] sat_layers = new double[max_layers + 1];
        private double[] dul_layers = new double[max_layers + 1];
        private double[] ll_layers = new double[max_layers + 1];
        private double[] sat_adj = new double[max_layers + 1];
        private double[] ll_adj = new double[max_layers + 1];

        //APSIM SoilN specific arrays
        private double[] no3 = new double[max_layers + 1];
        private double[] nh4 = new double[max_layers + 1];
        private double[] urea = new double[max_layers + 1];

        //Defoliation parameters
        private double defoliationDDSum = 0.0;
        private int defoliationDayCnt = 0;





        //private double test_tr;
        //private double test_f_int;
        //private double test_f_intz;
        //private double test_alai_row;
        //private double test_ep;
        //private double test_alight;
        //private double test_ep_int;



        //==========================================================================
        // References to supporting classes
        //==========================================================================
        // mvDataFunctions mvUtils = new mvDataFunctions() ;

        Cultivars ozcotCultivars = new Cultivars();


        //==========================================================================
        //  
        //
        //==========================================================================
        public mvOZCOTClass(uint compID, uint parentCompID, MessageFromLogic messageCallback)
            : base(compID, parentCompID, messageCallback, _STYPE, _SVERSION, _SAUTHOR)
        {

            //add subscribed events
            addEvent("sow", EVENT_SOW, TypeSpec.KIND_SUBSCRIBEDEVENT, typeSow, "Sow Event (APSIM)", "Sow Event with parameters", 0);
            defineEventState(EVENT_SOW, SOW_STATE_ACQUIRE, TStateMachine.NONLOGIC);
            defineEventState(EVENT_SOW, SOW_STATE_EXECUTE, TStateMachine.LOGIC);
            defineEventTransition(EVENT_SOW, TStateMachine.IDLE, 0, SOW_STATE_ACQUIRE, true);   //idle -> acquire
            defineEventTransition(EVENT_SOW, SOW_STATE_ACQUIRE, 0, SOW_STATE_EXECUTE, false);
            defineEventTransition(EVENT_SOW, SOW_STATE_EXECUTE, 0, TStateMachine.DONE, false); //done
            
            addEvent("process", EVENT_PROCESS, TypeSpec.KIND_SUBSCRIBEDEVENT, TypeSpec.TYPEEMPTY,"Process Daily Growth", "APSIM Daily Growth",  0);
            defineEventState(EVENT_PROCESS, GROWTH_STATE_ACQUIRE, TStateMachine.NONLOGIC);
            defineEventState(EVENT_PROCESS, GROWTH_STATE_EXECUTE, TStateMachine.LOGIC);
            defineEventTransition(EVENT_PROCESS, TStateMachine.IDLE, 0, GROWTH_STATE_ACQUIRE, true);   //idle -> acquire
            defineEventTransition(EVENT_PROCESS, GROWTH_STATE_ACQUIRE, 0, GROWTH_STATE_EXECUTE, false);
            defineEventTransition(EVENT_PROCESS, GROWTH_STATE_EXECUTE, 0, TStateMachine.DONE, false); //done

            addEvent("do_cotton_growth", EVENT_DO_GROWTH, TypeSpec.KIND_SUBSCRIBEDEVENT, TypeSpec.TYPEEMPTY, "Do Daily Growth", "Default Daily Growth", 0);
            defineEventState(EVENT_DO_GROWTH, GROWTH_STATE_ACQUIRE, TStateMachine.NONLOGIC);
            defineEventState(EVENT_DO_GROWTH, GROWTH_STATE_EXECUTE, TStateMachine.LOGIC);
            defineEventTransition(EVENT_DO_GROWTH, TStateMachine.IDLE, 0, GROWTH_STATE_ACQUIRE, true);   //idle -> acquire
            defineEventTransition(EVENT_DO_GROWTH, GROWTH_STATE_ACQUIRE, 0, GROWTH_STATE_EXECUTE, false);
            defineEventTransition(EVENT_DO_GROWTH, GROWTH_STATE_EXECUTE, 0, TStateMachine.DONE, false); //done

            addEvent("harvest", EVENT_HARVEST, TypeSpec.KIND_SUBSCRIBEDEVENT, typeHarvest, "Harvest Cotton", "APSIM Harvest event", 0);
            defineEventState(EVENT_HARVEST, HARVEST_STATE_ACQUIRE, TStateMachine.NONLOGIC);
            defineEventState(EVENT_HARVEST, HARVEST_STATE_EXECUTE, TStateMachine.LOGIC);
            defineEventTransition(EVENT_HARVEST, TStateMachine.IDLE, 0, HARVEST_STATE_ACQUIRE, true);   //idle -> acquire
            defineEventTransition(EVENT_HARVEST, HARVEST_STATE_ACQUIRE, 0, HARVEST_STATE_EXECUTE, false);
            defineEventTransition(EVENT_HARVEST, HARVEST_STATE_EXECUTE, 0, TStateMachine.DONE, false); //done

            addEvent("defoliate", EVENT_DEFOLIATE, TypeSpec.KIND_SUBSCRIBEDEVENT, TypeSpec.TYPEEMPTY, "Defoliate Cotton", "Cotton Defoliation event", 0);
            defineEventState(EVENT_DEFOLIATE, DEFOLIATE_STATE_ACQUIRE, TStateMachine.NONLOGIC);
            defineEventState(EVENT_DEFOLIATE, DEFOLIATE_STATE_EXECUTE, TStateMachine.LOGIC);
            defineEventTransition(EVENT_DEFOLIATE, TStateMachine.IDLE, 0, DEFOLIATE_STATE_ACQUIRE, true);   //idle -> acquire
            defineEventTransition(EVENT_DEFOLIATE, DEFOLIATE_STATE_ACQUIRE, 0, DEFOLIATE_STATE_EXECUTE, false);
            defineEventTransition(EVENT_DEFOLIATE, DEFOLIATE_STATE_EXECUTE, 0, TStateMachine.DONE, false); //done

            addEvent("end_crop", EVENT_END_CROP, TypeSpec.KIND_SUBSCRIBEDEVENT, TypeSpec.TYPEEMPTY, "End cotton crop", "End Crop event", 0);
            defineEventState(EVENT_END_CROP, END_CROP_STATE_ACQUIRE, TStateMachine.NONLOGIC);
            defineEventState(EVENT_END_CROP, END_CROP_STATE_EXECUTE, TStateMachine.LOGIC);
            defineEventTransition(EVENT_END_CROP, TStateMachine.IDLE, 0, END_CROP_STATE_ACQUIRE, true);   //idle -> acquire
            defineEventTransition(EVENT_END_CROP, END_CROP_STATE_ACQUIRE, 0, END_CROP_STATE_EXECUTE, false);
            defineEventTransition(EVENT_END_CROP, END_CROP_STATE_EXECUTE, 0, TStateMachine.DONE, false); //done

            addEvent("kill_crop", EVENT_KILL_CROP, TypeSpec.KIND_SUBSCRIBEDEVENT, typeKillCrop, "Kill Crop", "Kill crop event", 0);
            defineEventState(EVENT_KILL_CROP, KILL_CROP_STATE_ACQUIRE, TStateMachine.NONLOGIC);
            defineEventState(EVENT_KILL_CROP, KILL_CROP_STATE_EXECUTE, TStateMachine.LOGIC);
            defineEventTransition(EVENT_KILL_CROP, TStateMachine.IDLE, 0, KILL_CROP_STATE_ACQUIRE, true);   //idle -> acquire
            defineEventTransition(EVENT_KILL_CROP, KILL_CROP_STATE_ACQUIRE, 0, KILL_CROP_STATE_EXECUTE, false);
            defineEventTransition(EVENT_KILL_CROP, KILL_CROP_STATE_EXECUTE, 0, TStateMachine.DONE, false); //done

            addEvent("do_cotton_sow", EVENT_DO_SOW, TypeSpec.KIND_SUBSCRIBEDEVENT, TypeSpec.TYPEEMPTY, "Sow event (default)", "Sow event using preset values", 0);
            defineEventState(EVENT_DO_SOW, SOW_STATE_ACQUIRE, TStateMachine.NONLOGIC);
            defineEventState(EVENT_DO_SOW, SOW_STATE_EXECUTE, TStateMachine.LOGIC);
            defineEventTransition(EVENT_DO_SOW, TStateMachine.IDLE, 0, SOW_STATE_ACQUIRE, true);   //idle -> acquire
            defineEventTransition(EVENT_DO_SOW, SOW_STATE_ACQUIRE, 0, SOW_STATE_EXECUTE, false);
            defineEventTransition(EVENT_DO_SOW, SOW_STATE_EXECUTE, 0, TStateMachine.DONE, false); //done

            addEvent("do_cotton_harvest", EVENT_DO_HARVEST, TypeSpec.KIND_SUBSCRIBEDEVENT, TypeSpec.TYPEEMPTY, "Harvest event (default)", "CPI Harvest event call", 0);
            defineEventState(EVENT_DO_HARVEST, HARVEST_STATE_ACQUIRE, TStateMachine.NONLOGIC);
            defineEventState(EVENT_DO_HARVEST, HARVEST_STATE_EXECUTE, TStateMachine.LOGIC);
            defineEventTransition(EVENT_DO_HARVEST, TStateMachine.IDLE, 0, HARVEST_STATE_ACQUIRE, true);   //idle -> acquire
            defineEventTransition(EVENT_DO_HARVEST, HARVEST_STATE_ACQUIRE, 0, HARVEST_STATE_EXECUTE, false);
            defineEventTransition(EVENT_DO_HARVEST, HARVEST_STATE_EXECUTE, 0, TStateMachine.DONE, false); //done


            addEvent("reset_cotton", EVENT_RESET, TypeSpec.KIND_SUBSCRIBEDEVENT, TypeSpec.TYPEEMPTY, "Reset cotton", "Reset variables ready for starting a new crop", 0);
            defineEventState(EVENT_RESET, RESET_STATE_DO, TStateMachine.LOGIC);
            defineEventTransition(EVENT_RESET, TStateMachine.IDLE, 0, RESET_STATE_DO, true);   //idle -> reset
            defineEventTransition(EVENT_RESET, RESET_STATE_DO, 0, TStateMachine.DONE, false);

            addEvent("tick", EVENT_TICK, TypeSpec.KIND_SUBSCRIBEDEVENT, TTimeStep.typeTIMESTEP,  "Time step", "Time step (daily)", 0);
            defineEventState(EVENT_TICK, TICK_STATE_DO, TStateMachine.LOGIC);
            defineEventTransition(EVENT_TICK, TStateMachine.IDLE, 0, TICK_STATE_DO, true);   //idle -> tick
            defineEventTransition(EVENT_TICK, TICK_STATE_DO, 0, TStateMachine.DONE, false);


            //add published events
            addEvent("newCrop", evtNewCrop, TypeSpec.KIND_PUBLISHEDEVENT, typeNewCrop, "New Crop", "New Crop initiated", 0);
            addEvent("sowing", evtSowing, TypeSpec.KIND_PUBLISHEDEVENT, TypeSpec.TYPEEMPTY, "Cotton is sowing", "Cotton is sowing", 0);
            addEvent("harvesting", evtHarvesting, TypeSpec.KIND_PUBLISHEDEVENT, TypeSpec.TYPEEMPTY, "Cotton harvesting", "Cotton is harvesting", 0);
            addEvent("harvested", evtHarvested, TypeSpec.KIND_PUBLISHEDEVENT, TypeSpec.TYPEEMPTY, "Cotton harvested", "Cotton has been harvested", 0);
            addEvent("BiomassRemoved", evtBiomassRemoved, TypeSpec.KIND_PUBLISHEDEVENT, typeBiomassRemoved, "Cotton stubble mulched", "Cotton stubble mulched", 0);
            addEvent("incorpFOM", evtIncorpFOM, TypeSpec.KIND_PUBLISHEDEVENT, typeIncorpFOM, "Cotton roots incorporated", "Cotton roots incorporated", 0);



            //add properties and drivers

            //addProperty (sName, ID,   bRead, bWrite, bInit, sUnit, bIsArray, sType, sShortDescr, sFullDescr)
            //setPropertyRange(propID,dDefault,dMinVal,dMaxVal)

            // Example **********
            // addProperty("day_degree", PROP_x, true, true, true, "-", false, "double"); //==FComputedValue
            // setPropertyRange(PROP_x, 0, 0, 10);
            // ******************

            TInitValue newProperty;

            newProperty = addProperty("crop_type", PROP_crop_type, true, false, true, "-", false, "string", "Crop Type", "Crop Type (default: cotton)");
            newProperty.setDefault("cotton");

            addProperty("leaf_res_n_conc", PROP_leaf_res_n_conc, true, false, true, "-", false, "double", "N conc leaf residue", "N concentration of leaf residue");
            setPropertyRange(PROP_leaf_res_n_conc, 0.02, 0.0, 1.0);

            addProperty("hucut", PROP_hucut, true, false, true, "oC", false, "double", "Heat Unit calc max temp ", "MaxT limit for Heat Unit caluculation");
            setPropertyRange(PROP_hucut, 40.0, 0.0, 100.0);

            addProperty("baset", PROP_baset, true, false, true, "oC", false, "double", "Heat Unit calc base temp", "Base temperature (min) for Heat Unit calculation");
            setPropertyRange(PROP_baset, 12.0, 0.0, 30.0);

            addProperty("open_def", PROP_open_def, true, false, true, "%", false, "double", "defoliation open percentage", "defoliation open percentage");  //defoliation open percentage
            setPropertyRange(PROP_open_def, 60.0, 0.0, 100.0);

            addProperty("a_root_leaf", PROP_a_root_leaf, true, false, true, "-", false, "double", "allometric constant root:leaf", "allometric constant root:leaf");
            setPropertyRange(PROP_a_root_leaf, 1.01, 0.0, 10.0);

            addProperty("a_stem_leaf", PROP_a_stem_leaf, true, false, true, "-", false, "double", "allometric constant stem:leaf", "allometric constant stem:leaf");
            setPropertyRange(PROP_a_stem_leaf, 1.25, 0.0, 10.0);

            addProperty("e_par", PROP_e_par, true, false, true, "g/MJ", false, "double", "rate of assimilate production g/MJ", "rate of assimilate production g/MJ");
            setPropertyRange(PROP_e_par, 2.5, 0.0, 10.0);

            addProperty("specific_lw", PROP_specific_lw, true, false, true, "g/m^2", false, "double", "dry wt production leaf demand", "dry wt production leaf demand");
            setPropertyRange(PROP_specific_lw, 58.0, 0.0, 100.0);

            addProperty("t_opt", PROP_t_opt, true, false, true, "oC", false, "double", "optimum temperature for assimilate production", "optimum temperature for assimilate production");
            setPropertyRange(PROP_t_opt, 25.0, 0.0, 50.0);

            addProperty("t_base", PROP_t_base, true, false, true, "oC", false, "double", "base temperature for assimilate production", "base temperature for assimilate production");
            setPropertyRange(PROP_t_base, 8.0, 0.0, 20.0);

            addProperty("wt_area_max", PROP_wt_area_max, true, false, true, "-", false, "double", "feed back for leaf dry wt/area", "feed back for leaf dry wt/area");
            setPropertyRange(PROP_wt_area_max, 150.0, 0.0, 400.0);

            addProperty("embryo", PROP_embryo, true, false, true, "g", false, "double", "dry weight of seedling at emergence", "dry weight of seedling at emergence");
            setPropertyRange(PROP_embryo, 0.75, 0.0, 2.0);

            addProperty("f_leaf", PROP_f_leaf, true, false, true, "-", false, "double", "leaf portion of dry weight at emergence", "leaf portion of dry weight at emergence");
            setPropertyRange(PROP_f_leaf, 0.6, 0.0, 1.0);

            addProperty("f_stem", PROP_f_stem, true, false, true, "-", false, "double", "stem portion of dry weight at emergence", "stem portion of dry weight at emergence");
            setPropertyRange(PROP_f_stem, 0.15, 0.0, 1.0);

            addProperty("f_root", PROP_f_root, true, false, true, "-", false, "double", "root portion of dry weight at emergence", "root portion of dry weight at emergence");
            setPropertyRange(PROP_f_root, 0.25, 0.0, 1.0);

            addProperty("elevation_default", PROP_elevation_default, true, false, true, "m", false, "double", "default elevation of crop field", "default elevation of crop field");
            setPropertyRange(PROP_elevation_default, 200.0, -100.0, 1000.0);

            addProperty("wlog_assimilate_red", PROP_wlog_assimilate_red, true, false, true, "-", false, "double", "reduction in assimilate", "reduction in assimilate production due to waterlogging");
            setPropertyRange(PROP_wlog_assimilate_red, 0.2, 0.0, 1.0);

            addProperty("wlog_carcap_red", PROP_wlog_carcap_red, true, false, true, "-", false, "double", "reduction in carrying capacity", "reduction in carrying capacity due to waterlogging");
            setPropertyRange(PROP_wlog_carcap_red, 0.2, 0.0, 1.0);

            addProperty("watlog_c", PROP_watlog_c, true, false, true, "-", false, "double", "sw/ul ratio impacting carrying capacity", "sw/ul ratio above which carrying capacity(carbon) is compromised");
            setPropertyRange(PROP_watlog_c, 0.87, 0.0, 1.0);

            addProperty("watlog_n", PROP_watlog_n, true, false, true, "-", false, "double", "sw/ul ratio impacting nitrogen uptake", "sw/ul ratio above which nitrogen uptake is compromised");
            setPropertyRange(PROP_watlog_n, 0.87, 0.0, 1.0);

            addProperty("wlog_carcap_red_stress", PROP_wlog_carcap_red_stress, true, false, true, "-", false, "double", "reduction in carrying capacity with water logging and stress", "reduction in carrying capacity with water logging and stress");
            setPropertyRange(PROP_wlog_carcap_red_stress, 0.01, 0.0, 1.0);

            addProperty("smi_affect_wlog", PROP_smi_affect_wlog, true, false, true, "-", false, "double", "smi stress point for amplifying water logging ", "low smi stress point that amplifies water logging in following few days");
            setPropertyRange(PROP_smi_affect_wlog, 0.75, 0.0, 1.0);

            addProperty("days_relief_wlog", PROP_days_relief_wlog, true, false, true, "days", false, "integer4", "number of days to relieve low smi stress", "number of days to relieve low smi stress which amplifies water logging effect");
            setPropertyRange(PROP_days_relief_wlog, 7, 0, 28);

            addProperty("frost_kill_immediate", PROP_frost_kill_immediate, true, false, true, "oC", false, "double", "min temperature below which immediate kill", "min temperature below which immediate kill");
            setPropertyRange(PROP_frost_kill_immediate, 2.0, -5.0, 5.0);

            addProperty("rtdep_max", PROP_rtdep_max, true, false, true, "cm", false, "double", "maximum depth of rooting", "maximum depth of rooting - unused");
            setPropertyRange(PROP_rtdep_max, 0.0, 0.0, 1000.0);  //if not set it defaults to profile depth

            addProperty("harvest_n_frac", PROP_harvest_n_frac, true, false, true, "-", false, "double", "fraction of uptake N for potential N harvested", "fraction of uptake N for potential N harvested");
            setPropertyRange(PROP_harvest_n_frac, 0.85, 0.0, 1.0);

            addProperty("cutout_smi_crit", PROP_cutout_smi_crit, true, false, true, "-", false, "double", "smi critical level for cutout", "smi critical level for cutout");
            setPropertyRange(PROP_cutout_smi_crit, 0.75, 0.0, 1.0);  //currently not used in ozcot

            addProperty("cutout_smi_days", PROP_cutout_smi_days, true, false, true, "days", false, "integer4", "delay before smi induces cutout", "delay before smi induces cutout");
            setPropertyRange(PROP_cutout_smi_days, 5, 0, 10);

            addProperty("cutout_smi_site_red", PROP_cutout_smi_site_red, true, false, true, "-", false, "double", "rate of site reduction due to smi cutout", "rate of site reduction if cutout induced by low smi");
            setPropertyRange(PROP_cutout_smi_site_red, 0.1, 0.0, 1.0);

            addProperty("epcoef1", PROP_epcoef1, true, false, true, "-", false, "double", "ep coefficient for high smi values", "ep coefficient for smi values exceeding epcoef_smi_crit value");
            setPropertyRange(PROP_epcoef1, 3.051, 0.0, 10.0);

            addProperty("epcoef2", PROP_epcoef2, true, false, true, "-", false, "double", "ep coefficient for low smi values", "ep coefficient for smi values below epcoef_smi_crit value");
            setPropertyRange(PROP_epcoef2, 2.436, 0.0, 10.0);

            addProperty("epcoef_smi_crit", PROP_epcoef_smi_crit, true, false, true, "-", false, "double", "critical smi value affecting ep", "critical smi value affecting ep");
            setPropertyRange(PROP_epcoef_smi_crit, 0.5, 0.0, 1.0);

            addProperty("fbwstr_low", PROP_fbwstr_low, true, false, true, "-", false, "double", "water stress on bolls - lower limit", "water stress on bolls - lower limit");
            setPropertyRange(PROP_fbwstr_low, 0.0, 0.0, 10.0);

            addProperty("fbwstr_high", PROP_fbwstr_high, true, false, true, "-", false, "double", "water stress on bolls - upper limit", "water stress on bolls - upper limit");
            setPropertyRange(PROP_fbwstr_high, 0.5, 0.0, 10.0);

            addProperty("fbwstr_a", PROP_fbwstr_a, true, false, true, "-", false, "double", "water stress on bolls - amplifier", "water stress on bolls - amplifier");
            setPropertyRange(PROP_fbwstr_a, 1.0, 0.0, 10.0);

            addProperty("fbnstr_low", PROP_fbnstr_low, true, false, true, "-", false, "double", "N stress on bolls - lower limit", "N stress on bolls - lower limit");
            setPropertyRange(PROP_fbnstr_low, 0.0, 0.0, 10.0);

            addProperty("fbnstr_high", PROP_fbnstr_high, true, false, true, "-", false, "double", "N stress on bolls - upper limit", "N stress on bolls - upper limit");
            setPropertyRange(PROP_fbnstr_high, 0.5, 0.0, 10.0);

            addProperty("fbnstr_a", PROP_fbnstr_a, true, false, true, "-", false, "double", "N stress on bolls - amplifier", "N stress on bolls - amplifier");
            setPropertyRange(PROP_fbnstr_a, 1.0, 0.0, 10.0);

            addProperty("relp_smi_crit", PROP_relp_smi_crit, true, false, true, "-", false, "double", "critical level of smi below which it affects photosynthesis", "critical level of smi below which it affects photosynthesis");
            setPropertyRange(PROP_relp_smi_crit, 0.5, 0.0, 1.0);

            addProperty("relp_intercept", PROP_relp_intercept, true, false, true, "-", false, "double", "intercept  of f(smi)", "intercept  of function smi vs photosynthesis");
            setPropertyRange(PROP_relp_intercept, 0.25, -1.0, 10.0);

            addProperty("relp_slope", PROP_relp_slope, true, false, true, "-", false, "double", "slope of f(smi)", "slope of the function smi vs photosynthesis");
            setPropertyRange(PROP_relp_slope, 0.864, -1.0, 10.0);

            addProperty("relp_low", PROP_relp_low, true, false, true, "-", false, "double", "water stress effect on photosynthesis - lower limit", "severity of effect of water stress on photosynthesis - lower limit");
            setPropertyRange(PROP_relp_low, 0.0, 0.0, 10.0);

            addProperty("relp_high", PROP_relp_high, true, false, true, "-", false, "double", "water stress effect on photosynthesis - upper limit", "severity of effect of water stress on photosynthesis - upper limit");
            setPropertyRange(PROP_relp_high, 1.0, 0.0, 10.0);

            addProperty("relp_a", PROP_relp_a, true, false, true, "-", false, "double", "water stress effect on photosynthesis - amplitude", "severity of effect of water stress on photosynthesis - amplitude");
            setPropertyRange(PROP_relp_a, 0.5, 0.0, 10.0);

            addProperty("vsnstr_low", PROP_vsnstr_low, true, false, true, "-", false, "double", "N stress on fruiting site number - lower limit", "N stress on fruiting site number - lower limit");
            setPropertyRange(PROP_vsnstr_low, 0.0, 0.0, 10.0);

            addProperty("vsnstr_high", PROP_vsnstr_high, true, false, true, "-", false, "double", "N stress on fruiting site number - upper limit", "N stress on fruiting site number - upper limit");
            setPropertyRange(PROP_vsnstr_high, 0.9, 0.0, 10.0);

            addProperty("vsnstr_a", PROP_vsnstr_a, true, false, true, "-", false, "double", "N stress on fruiting site number - amplitude", "N stress on fruiting site number - amplitude");
            setPropertyRange(PROP_vsnstr_a, 1.0, 0.0, 10.0);

            addProperty("flfsmi_low", PROP_flfsmi_low, true, false, true, "-", false, "double", "water stress on pre-squaring LAI - lower limit", "water stress on pre-squaring LAI - lower limit");
            setPropertyRange(PROP_flfsmi_low, 0.0, 0.0, 10.0);   //currently not used

            addProperty("flfsmi_high", PROP_flfsmi_high, true, false, true, "-", false, "double", "water stress on pre-squaring LAI - upper limit", "water stress on pre-squaring LAI - upper limit");
            setPropertyRange(PROP_flfsmi_high, 0.5, 0.0, 10.0);  //currently not used

            addProperty("flfsmi_a", PROP_flfsmi_a, true, false, true, "-", false, "double", "water stress on pre-squaring LAI - amplitude", "water stress on pre-squaring LAI - amplitude");
            setPropertyRange(PROP_flfsmi_a, 1.0, 0.0, 10.0);     //currently not used

            addProperty("vlnstr_low", PROP_vlnstr_low, true, false, true, "-", false, "double", "N stress on LAI - lower limit", "N stress on LAI - lower limit");
            setPropertyRange(PROP_vlnstr_low, 0.0, 0.0, 10.0);

            addProperty("vlnstr_high", PROP_vlnstr_high, true, false, true, "-", false, "double", "N stress on LAI - upper limit", "N stress on LAI - upper limit");
            setPropertyRange(PROP_vlnstr_high, 0.9, 0.0, 10.0);

            addProperty("vlnstr_a", PROP_vlnstr_a, true, false, true, "-", false, "double", "N stress on LAI - amplitude", "N stress on LAI - amplitude");
            setPropertyRange(PROP_vlnstr_a, 1.0, 0.0, 10.0);

            addProperty("fw_low", PROP_fw_low, true, false, true, "-", false, "double", "effect of water stress on leaf senescence - lower limit", "effect of water stress on leaf senescence - lower limit");
            setPropertyRange(PROP_fw_low, 0.0, 0.0, 10.0);

            addProperty("fw_high", PROP_fw_high, true, false, true, "-", false, "double", "effect of water stress on leaf senescence - upper limit", "effect of water stress on leaf senescence - upper limit");
            setPropertyRange(PROP_fw_high, 0.25, 0.0, 10.0);

            addProperty("fw_a", PROP_fw_a, true, false, true, "-", false, "double", "effect of water stress on leaf senescence - amplitude", "effect of water stress on leaf senescence - amplitude");
            setPropertyRange(PROP_fw_a, 1.0, 0.0, 10.0);

            addProperty("adjust_low", PROP_adjust_low, true, false, true, "-", false, "double", "plant N adjustment for supply/demand ratio - lower limit", "plant N adjustment for supply/demand ratio - lower limit");
            setPropertyRange(PROP_adjust_low, 0.0, 0.0, 10.0);

            addProperty("adjust_high", PROP_adjust_high, true, false, true, "-", false, "double", "plant N adjustment for supply/demand ratio - upper limit", "plant N adjustment for supply/demand ratio - upper limit");
            setPropertyRange(PROP_adjust_high, 5.0, 0.0, 10.0);

            addProperty("adjust_a", PROP_adjust_a, true, false, true, "-", false, "double", "plant N adjustment for supply/demand ratio - amplitude", "plant N adjustment for supply/demand ratio - amplitude");
            setPropertyRange(PROP_adjust_a, 1.0, 0.0, 10.0);

            addProperty("fwstrs_low", PROP_fwstrs_low, true, false, true, "-", false, "double", "effect of water stress on boll growth - lower limit", "effect of water stress on boll growth - lower limit");
            setPropertyRange(PROP_fwstrs_low, 0.0, 0.0, 10.0);

            addProperty("fwstrs_high", PROP_fwstrs_high, true, false, true, "-", false, "double", "effect of water stress on boll growth - upper limit", "effect of water stress on boll growth - upper limit");
            setPropertyRange(PROP_fwstrs_high, 0.5, 0.0, 10.0);

            addProperty("fwstrs_a", PROP_fwstrs_a, true, false, true, "-", false, "double", "effect of water stress on boll growth - amplitude", "effect of water stress on boll growth - amplitude");
            setPropertyRange(PROP_fwstrs_a, 4.0, 0.0, 10.0);

            addProperty("smi_delay_crit", PROP_smi_delay_crit, true, false, true, "-", false, "double", "smi trigger value for delayed development","smi value that delays development to 1st square");
            setPropertyRange(PROP_smi_delay_crit, 0.25, 0.0, 1.0);

            addProperty("cold_shock_delay_crit", PROP_cold_shock_delay_crit, true, false, true, "oC", false, "double", "temperature that triggers cold shock delay for development", "min temperature value below which development before 1st square is delayed");
            setPropertyRange(PROP_cold_shock_delay_crit, 11.0, 0.0, 20.0);

            addProperty("cold_shock_delay", PROP_cold_shock_delay, true, false, true, "dd", false, "double", "DayDegree delay imposed by cold shock", "DayDegree delay imposed by cold shock");
            setPropertyRange(PROP_cold_shock_delay, 5.2, 0.0, 20.0);

            addProperty("fert_crit", PROP_fert_crit, true, false, true, "kg/ha", false, "double", "amount of change in soil N", "amount of change in soil N that indicates that a fertiliser event has occurred");
            setPropertyRange(PROP_fert_crit, 1.0, 0.0, 100.0);

            addProperty("fert_detect", PROP_fert_detect, true, false, true, "kg/ha", false, "double", "amount of soil N required to detect a fertiliser event", "amount of soil N required to detect a fertiliser event");
            setPropertyRange(PROP_fert_detect, 10.0, 0.0, 100.0);

            addProperty("days_since_fert_max", PROP_days_since_fert_max, true, false, true, "days", false, "integer4", "max days prior to crop that fertiliser application is effective", "max days prior to crop that fertiliser application is considered effective");
            setPropertyRange(PROP_days_since_fert_max, 100, 0, 100);

            // read the sowing details
            newProperty = addProperty("cultivar_default", PROP_cultivar_default, true, false, true, "-", false, "string", "default cultivar", "default cultivar if none specified or details not found");
            newProperty.setDefault("S71BR");
            //"S189" is a conventional variety alternative

            //read the cultivars list (if present)
            addProperty("cultivars", PROP_cultivars_list, true, false, true, typeCultivars, "cultivars list", "List of additional or modified cultivars");


            //Greenhouse gas studies

            // CO2 Effect on Cotton Photosynthesis - Field crop Canopy response
            //  Based on work by K. Reddy, V. Kakani and H. Hodges   (2008)
            //   CO2 Effect = 0.004050*X - 0.000004006*X^2 + 0.000000001303*X^3
            //   this is resolved and calibrated for Effect = 1.0 @ 330 ppm (OZCOT original calibration) and subsequent values calculated
            //   [Response reviewed and mods made to OZCOT code Nov 2013 by Michael Bange and David Johnston]
            //   [Further review is called for as more data is published - particularly from Australian field studies]

            //2013 value reported to be approx 394ppm with about 2ppm gain per year
            addProperty("cotton_co2", PROP_cottonCO2, true, false, false, "ppm", false, "double", "co2 concentration", "co2 concentration");
            setPropertyRange(PROP_cottonCO2, 400.0, 0.0, 1000.0);


           
            //eg <x_co2_fert>?.</x_co2_fert>
            //addProperty("x_co2_fert", PROP_x_co2_fert, true, false, false, "ppm", true, "double", "co2 concentration", "co2 concentration");

            //eg <y_co2_fert>?.</y_co2_fert>
            //addProperty("y_co2_fert", PROP_y_co2_fert, true, false, false, "-", true, "double", "co2 fert effect", "co2 fert effect");


            //Plant height
            //eg <x_stem_wt>?.</x_stem_wt>
            addProperty("x_stem_wt", PROP_x_stem_wt, true, false, true, "g/plant", true, "double", "Stem DryWt per Plant", "Stem DryWt per Plant");
            setPropertyRange(PROP_x_stem_wt, 0.0, 0.0, 500.0);
            propertyList[PROP_x_stem_wt].setElementCount(5);
            propertyList[PROP_x_stem_wt].item(1).setValue(5.0);
            propertyList[PROP_x_stem_wt].item(2).setValue(10.0);
            propertyList[PROP_x_stem_wt].item(3).setValue(20.0);
            propertyList[PROP_x_stem_wt].item(4).setValue(40.0);
            propertyList[PROP_x_stem_wt].item(5).setValue(60.0);

            //eg <y_height>?.</y_height>
            addProperty("y_height", PROP_y_height, true, false, true, "mm", true, "double", "Plant Ht for Stem drywt", "Plant Ht for Stem drywt");
            setPropertyRange(PROP_y_height, 900.0, 0.0, 1500.0);
            propertyList[PROP_y_height].setElementCount(5);
            propertyList[PROP_y_height].item(1).setValue(50.0);
            propertyList[PROP_y_height].item(2).setValue(450.0);
            propertyList[PROP_y_height].item(3).setValue(800.0);
            propertyList[PROP_y_height].item(4).setValue(1050.0);
            propertyList[PROP_y_height].item(5).setValue(1100.0);


            // root expansion delay
            addProperty("rootExpansionDelay", PROP_rootExpansionDelay, true, false, true, "-", false, "integer4", "delay days for root expansion", "delay days after sowing until roots rapidly expand - default = 33 ");
            setPropertyRange(PROP_rootExpansionDelay, 33, 0, 100);

            // use sum of DayDegrees to 1st Square flag
             addProperty("useDDsumTo1stSq", PROP_useDDsumTo1stSq, true, false, true, "-", false, "boolean", "use DayDegree sum for 1st Square", "use DayDegree sum to predict 1st Square. default = false ie use RateOfDevelopment");
             setPropertyRange(PROP_useDDsumTo1stSq, 0,0,1);


            //====================================================================
            //Output 'readable only' properties
            addProperty("plant_status", PROP_plant_status, true, false, false, "-", false, "string", "Plant status", "Plant status");
            addProperty("PlantStatus", PROP_PlantStatus, true, false, false, "-", false, "string", "Plant status", "Plant status");
            addProperty("ozcot_status", PROP_ozcot_status, true, false, false, "-", false, "integer4", "model status", "model status - termination reason");
            addProperty("status", PROP_status, true, false, false, "-", false, "integer4", "model status", "model status - apsim terminology");
            addProperty("crop_in", PROP_crop_in, true, false, false, "-", false, "integer4", "status flag - crop planted", "status flag - crop is in the ground");
            addProperty("stage", PROP_stage, true, false, false, "-", false, "integer4", "crop stage number", "crop stage number 1-germination...7-harvest_ripe");
            addProperty("stageName", PROP_stageName, true, false, false, "-", false, "string", "crop stage name", "crop stage name");
            addProperty("das", PROP_das, true, false, false, "days", false, "integer4", "Days After Sowing", "Days After Sowing");
            addProperty("DaysAfterSowing", PROP_daysAfterSowing, true, false, false, "days", false, "integer4", "Days After Sowing", "Days After Sowing");
            addProperty("ozcot_sumDD", PROP_ozcot_sumDD, true, false, false, "-", false, "double", "accumulated DayDegrees", "accumulated DayDegrees");

            addProperty("ep", PROP_ep, true, false, false, "mm", false, "double", "plant evapotranspiration", "plant evapotranspiration");
            addProperty("LAI", PROP_lai, true, false, false, "m2/m2", false, "double", "crop LAI", "crop LAI");

            addProperty("sites", PROP_sites, true, false, false, "1/m2", false, "double", "sites", "number of sites");
            addProperty("squarz", PROP_squarz, true, false, false, "1/m2", false, "double", "squares", "number of squares");
            addProperty("fru_no_cat", PROP_fru_no_cat, true, false, false, "-", true, "double", "fruit numbers by category", "fruit numbers by fruit category");
            addProperty("bollz", PROP_bollz, true, false, false, "1/m2", false, "double", "green bolls", "number of green bolls");
            addProperty("openz", PROP_openz, true, false, false, "1/m2", false, "double", "open bolls", "number of open bolls");
            addProperty("lint", PROP_lint, true, false, false, "kg/ha", false, "double", "lint (kg/ha)", "lint yield in kg/ha");
            addProperty("openwt", PROP_openwt_kg, true, false, false, "kg/ha", false, "double", "dry wt of open bolls", "dry wt of open bolls");
            addProperty("frudw", PROP_frudw, true, false, false, "kg/ha", false, "double", "dry wt of growing fruit", "dry wt of all squares and green bolls");
            addProperty("frudw_tot", PROP_frudw_tot, true, false, false, "kg/ha", false, "double", "dry wt of all fruit", "dry wt of squares + green bolls + open bolls");
            addProperty("frudw_shed", PROP_frudw_shed, true, false, false, "kg/ha", false, "double", "dry wt of shed fruit", "dry wt of shed fruit");
            addProperty("frun", PROP_frun, true, false, false, "-", false, "double", "N in fruit kg/ha", "N in fruit kg/ha");
            addProperty("bload", PROP_bload, true, false, false, "-", false, "double", "boll load", "boll load");
            addProperty("carcap_c", PROP_carcap_c, true, false, false, "-", false, "double", "carrying capacity - assimilate", "carrying capacity as limited by assimilate");
            addProperty("carcap_n", PROP_carcap_n, true, false, false, "-", false, "double", "carrying capacity - N", "carrying capacity as limited by Nitrogen");
            addProperty("vnstrs", PROP_vnstrs, true, false, false, "-", false, "double", "vegetative nitrogen stress", "stress indicator based on vegetative demand for Nitrogen");
            addProperty("fnstrs", PROP_fnstrs, true, false, false, "-", false, "double", "fruit nitrogen stress", "stress indicator based on fruit demand for Nitrogen");
            
            addProperty("dw_total", PROP_dw_total, true, false, false, "kg/ha", false, "double", "total crop dry wt", "dry wt of the complete crop");  //TODO: check units???
            addProperty("dw_boll", PROP_dw_boll, true, false, false, "kg/ha", false, "double", "dry wt bolls", "dry wt bolls");
            addProperty("dn_plant", PROP_dn_plant, true, false, false, "kg/ha", false, "double", "daily increment of plant n to system", "daily increment of plant n to system");
            addProperty("assimilate", PROP_assimilate, true, false, false, "g/m2", false, "double", "new dry matter passed daily from s/r assimilation", "new dry matter passed daily from s/r assimilation");
            addProperty("growthWt", PROP_growthWt, true, false, false, "g/m2", false, "double", "same as assimilate", "same as assimilate");
            addProperty("dm", PROP_dm, true, false, false, "g/m2", false, "double", "total dry wt of crop", "Total dry wt of growing crop");
            addProperty("dm_green", PROP_dm_green, true, false, false, "g/m2", true, "double", "dry wt of growing crop (including reserve pool)", "dry wt of growing crop (including reserve pool)");
            addProperty("dm_senesced", PROP_dm_senesced, true, false, false, "g/m2", true, "double", "dry wt of senesced crop", "dry wt of senesced crop");
            addProperty("dm_shed", PROP_dm_shed, true, false, false, "g/m2", true, "double", "dry wt of shed crop components", "dry wt of shed crop components");
            addProperty("dm_harvested", PROP_dm_harvested, true, false, false, "g/m2", true, "double", "dry wt of harvested crop", "dry wt of harvested crop");
            addProperty("dlt_dm_live", PROP_dlt_dm_live, true, false, false, "g/m2", true, "double", "daily change in crop dry matter live (green) wt", "daily change in crop dry matter live (green) wt");
            addProperty("n_green", PROP_n_green, true, false, false, "g/m2", true, "double", "N content of green crop", "N content of green crop");
            addProperty("n_senesced", PROP_n_senesced, true, false, false, "g/m2", true, "double", "N content of senesced crop", "N content of senesced crop");
            addProperty("dw_root", PROP_dw_root, true, false, false, "kg/ha", false, "double", "dry wt roots", "dry wt roots");
            addProperty("dw_leaf", PROP_dw_leaf, true, false, false, "kg/ha", false, "double", "dry wt leaf", "dry wt leaf");
            addProperty("dw_stem", PROP_dw_stem, true, false, false, "kg/ha", false, "double", "dry wt stem", "dry wt stem");
            //addProperty("totnup", PROP_totnup, true, false, false, "kg/ha", false, "double", "total Nitrogen uptake", "total Nitrogen uptake");
            addProperty("yield", PROP_yield, true, false, false, "kg/ha", false, "double", "lint yield kg/ha", "lint yield kg/ha");
            addProperty("lint_yield", PROP_lint_yield, true, false, false, "kg/ha", false, "double", "lint yield kg/ha", "lint yield kg/ha");
            addProperty("cover_green", PROP_cover_green, true, false, false, "-", false, "double", "crop coverage", "crop coverage of the ground on an area basis");
            addProperty("CoverLive", PROP_CoverLive, true, false, false, "-", false, "double", "crop coverage", "crop coverage of the ground on an area basis");
            addProperty("cover_tot", PROP_cover_tot, true, false, false, "-", false, "double", "crop coverage of ground", "");
            addProperty("CoverTotal", PROP_CoverTotal, true, false, false, "-", false, "double", "crop coverage of ground", "");
            addProperty("availn", PROP_availn, true, false, false, "kg/ha", false, "double", "N available in soil", "N available in soil for uptake");
            addProperty("nuptake_potential", PROP_uptakn, true, false, false, "kg/ha", false, "double", "potential N uptake", "potential N uptake");
            addProperty("tsno3", PROP_tsno3, true, false, false, "kg/ha", false, "double", "soil nitrate", "total soil nitrate");
            addProperty("ysno3", PROP_ysno3, true, false, false, "kg/ha", false, "double", "prior day's soil nitrate", "total soil nitrate yesterday");
            addProperty("tsnh4", PROP_tsnh4, true, false, false, "kg/ha", false, "double", "soil ammonium", "total soil ammonium");
            addProperty("ysnh4", PROP_ysnh4, true, false, false, "kg/ha", false, "double", "prior day's soil ammonium", "total soil ammonium yesterday");
           // addProperty("d_nup", PROP_d_nup, true, false, false, "kg/ha", false, "double", "daily N uptake", "daily N uptake kg/ha");
            addProperty("nuptake_daily", PROP_n_uptake, true, false, false, "kg/ha", false, "double", "daily N uptake", "daily N uptake kg/ha");
            addProperty("rtdep", PROP_rtdep, true, false, false, "cm", false, "double", "rooting depth", "rooting depth");
            addProperty("s_bed_mi", PROP_s_bed_mi, true, false, false, "-", false, "double", "seed bed moisture index", "seed bed moisture index");
            addProperty("smi", PROP_smi, true, false, false, "-", false, "double", "SMI", "soil moisture index");
            addProperty("wli", PROP_wli, true, false, false, "-", false, "double", "WLI", "waterlogging index");
            addProperty("ep_cm", PROP_ep_cm, true, false, false, "cm", false, "double", "evap in cm", "Plant evap in cm");
            addProperty("evap_plant", PROP_evap_plant, true, false, false, "mm", false, "double", "Plant evap mm", "Plant evap in mm");
            addProperty("evap_soil", PROP_evap_soil, true, false, false, "mm", false, "double", "soil evaporation", "evaporation from soil surface");
            addProperty("evap_pot", PROP_evap_pot, true, false, false, "mm", false, "double", "eo potential evaporation", "eo potential evaporation from soil surface");
            addProperty("evap_tot", PROP_evap_tot, true, false, false, "mm", false, "double", "total evaporation et", "total evapotranspiration et");
            addProperty("bolls_sc", PROP_bolls_sc, true, false, false, "g/boll", false, "double", "seed cotton g/boll ", "weight seed cotton g/boll");
            addProperty("nuptake_total", PROP_nuptake, true, false, false, "kg/ha", false, "double", "total N uptake", "total N uptake for crop");
            addProperty("squarz_max", PROP_squarz_max, true, false, false, "1/m2", false, "double", "max squares", "peak number of squares");
            addProperty("lai_max", PROP_lai_max, true, false, false, "m2/m2", false, "double", "LAI max", "peak LAI value");
            addProperty("defol_das", PROP_defol_das, true, false, false, "das", false, "integer4", "1st defoliation DAS", "1st defoliation DAS");
            addProperty("defol2_das", PROP_defol2_das, true, false, false, "das", false, "integer4", "2nd defoliation DAS", "2nd defoliation DAS");
            addProperty("height", PROP_height, true, false, false, "mm", false, "double", "height of crop mm", "height of crop mm");
            addProperty("yield_bales", PROP_lint_bales, true, false, false, "bales/ha", false, "double", "lint yield bales/ha", "lint yield bales/ha");
            addProperty("yield_kg", PROP_lint_kg, true, false, false, "kg/ha", false, "double", "lint yield kg/ha", "lint yield kg/ha");
            
            addProperty("ozcot_rwu", PROP_ozcot_rwu, true, false, false, "mm", true, "double", "root water uptake", "root water uptake");  //sv- 2015_04_01 needed because ep does not equal root water uptake summed over profile. THIS NEEDS TO BE FIXED

           // addProperty("openBollsArray", PROP_openBollsArray, true, false, false, "-", false, typeDouble2D, "array of open boll details ", "array of open boll details (cnt,wt,accumRain) ");
            addProperty("openBollCnts", PROP_openBollCnts, true, false, false, "-", true, "double", "array of open boll numbers ", "array of open boll numbers ");
            addProperty("openBollWts", PROP_openBollWts, true, false, false, "-", true, "double", "array of open boll wts ", "array of open boll weights ");
            addProperty("openBollAccumRain", PROP_openBollAccumRain, true, false, false, "-", true, "double", "array of accumulated rain for open boll cohorts ", "array of accumulated rain for open boll cohorts ");

            addProperty("firstSquareDAS", PROP_FirstSquareDAS, true, false, false, "das", false, "integer4", "first square DAS ", "first square DAS ");
            addProperty("firstFlowerDAS", PROP_FirstFlowerDAS, true, false, false, "das", false, "integer4", "first flower DAS ", "first flower DAS ");
            addProperty("firstOpenBollDAS", PROP_FirstOpenBollDAS, true, false, false, "das", false, "integer4", "first open boll DAS ", "first open bpll DAS ");

            // soil properties
            //addProperty("ul1", PROP_ul1, true, false, false, "-", false, "double", "limit of es stage1", "limit of es stage1");   //limit of es stage1
            //addProperty("cona", PROP_cona, true, false, false, "-", false, "double", "rate in stage2 es", "rate in stage2 es");  //rate in stage2 es
            
            // report the soil water lower limits "ll" for the cotton root extraction  being used (might be CLL or LL15 if cll not available)
            //    eg  <ll>0.250   0.289   0.276   0.296   0.324   0.353   0.428 ()</ll>
            addProperty("ll", PROP_ll, true, false, false, "mm/mm", true, "double", "crop lower limit", "crop lower limit for this soil by layer");

            // cultivar details
            addProperty("percent_l", PROP_percent_l, true, false, false, "%", false, "double", "percent lint", "percent lint in boll by weight");
            addProperty("scboll", PROP_scboll, true, false, false, "g", false, "double", "seed cotton per boll", "boll size grams of seed cotton");
            addProperty("respcon", PROP_respcon, true, false, false, "-", false, "double", "respiration constant", "respiration constant, demand on assimilates");
            addProperty("sqcon", PROP_sqcon, true, false, false, "-", false, "double", "squaring constant", "squaring constant, rate of sites per DD");
            addProperty("fcutout", PROP_fcutout, true, false, false, "-", false, "double", "fruiting cutout", "boll load cutout per fruit category");
            addProperty("flai", PROP_flai, true, false, false, "-", false, "double", "cultivar adjustment for leaf area per site", "cultivar adjustment for leaf area per site");
            addProperty("ddisq", PROP_ddisq, true, false, false, "-", false, "double", "DD to 1st square", "DD to 1st square");
            addProperty("popcon", PROP_popcon, true, false, false, "-", false, "double", "population constant", "population constant - adjusts site production based on plant density");
            addProperty("acotyl", PROP_acotyl, true, false, false, "mm^2", false, "double", "cotyledon leaf area", "initial leaf area of cotyledons at emergence");
            addProperty("rlai", PROP_rlai, true, false, false, "-", false, "double", "LAI increase rate pre squaring", "unstressed LAI increase rate pre squaring");
            addProperty("fburr", PROP_fburr, true, false, false, "-", false, "double", "burr fraction", "proportional value of whole boll weight to seed cotton weight ~1.23 ");
            addProperty("dlds_max", PROP_dlds_max, true, false, false, "-", false, "double", "max LAI increase per site", "max LAI increase per site");
            addProperty("rate_emergence", PROP_rate_emergence, true, false, false, "mm/dd", false, "double", "rate of emergence", "growth rate mm/DD for sowing to emergence");
            addProperty("Background_retention", PROP_background_retention, true, false, false, "-", false, "double", "background retention", "rate of underlying retention of fruit (1-shedding rate)");

            //eg <FRUDD>50.   161.   307.   338.   484.   630.   848.  1071.</FRUDD>
            addProperty("frudd", PROP_frudd, true, false, false, "dd", true, "double", "fruit development in DD", "fruit category progression in day degrees");

            //eg <BLTME>0.00   0.00   0.00   0.07   0.21   0.33   0.55   1.00</BLTME>
            addProperty("bltme", PROP_bltme, true, false, false, "-", true, "double", "boll time", "boll progression as proportion of open boll");

            //eg <WT>0.0104 0.0272 0.1441 0.0988 0.5042 0.9617 1.0000 0.5785</WT>
            addProperty("wt", PROP_wt, true, false, false, "-", true, "double", "fruit assimilate demand as proportion of large green boll", "fruit assimilate demand as proportion of large green boll - boll load caluculations ");








            //addDriver (sName, driverIDList, MinConn, MaxConn, sUnit, bIsArray, sType, sourceID)
            //
            // Example **********
            // addDriver("maxt", DRV_MAXT, 0, 1, "C", false, "double", 0);
            // ******************

            // add climate drivers
            addDriver("maxt", DRV_MAXT, 0, 1, "oC", false, "double", "Max temp", "Maximum temperature", 0);
            addDriver("mint", DRV_MINT, 0, 1, "oC", false, "double", "Min temp", "Minimum temperature", 0);
            addDriver("radn", DRV_RADN, 0, 1, "MJ/m^2", false, "double", "Radiation", "Radiation MJ/m2", 0);
            addDriver("rain", DRV_RAIN, 0, 1, "mm", false, "double", "Rain", "Rainfall mm", 0);

            // add soilwater drivers
            addDriver("dlayer", DRV_DLAYR, 0, 1, "mm", true, "double", "Depth of layers", "Depth of each soil layer", 0);
            addDriver("bd", DRV_BULKD, 0, 1, "g/cm^3", true, "double", "Bulk Density", "Bulk Density by layer", 0);
            addDriver("ll15", DRV_LL15, 0, 1, "mm/mm", true, "double", "LL15", "LL15 by layer", 0);
            addDriver("dul", DRV_DUL, 0, 1, "mm/mm", true, "double", "DUL", "DUL by layer", 0);
            addDriver("sat", DRV_SAT, 0, 1, "mm/mm", true, "double", "Sat", "Saturation by layer", 0);
            addDriver("sw", DRV_SW, 0, 1, "mm/mm", true, "double", "SW", "Soil Water content by layer", 0);
            addDriver("eo", DRV_EO, 0, 1, "mm", false, "double", "Potential evaporation", "Potential evaporation from soil surface", 0);
            addDriver("es", DRV_ES, 0, 1, "mm", false, "double", "Emissivity of evap surface", "Emissivity of evap surface", 0);
            addDriver("runoff", DRV_RUNOFF, 0, 1, "mm", false, "double", "Runoff", "Runoff from rain event", 0);

            // add soil nitrogen drivers
            addDriver("no3_min", DRV_NO3_MIN, 0, 1, "kg/ha", true, "double", "", "", 0);
            addDriver("no3", DRV_NO3, 0, 1, "kg/ha", true, "double", "", "", 0);
            addDriver("nh4_min", DRV_NH4_MIN, 0, 1, "kg/ha", true, "double", "", "", 0);
            addDriver("nh4", DRV_NH4, 0, 1, "kg/ha", true, "double", "", "", 0);
            addDriver("urea", DRV_UREA, 0, 1, "kg/ha", true, "double", "", "", 0);

            // add climate change drivers
            addDriver("co2", DRV_CO2, 0, 1, "ppm", false, "double", "CO2", "Atmospheric CO2 ppm", 0);




        }
        //==========================================================================
        /// <summary>
        /// Execute one of the init stages
        /// </summary>
        //==========================================================================
        public override void initialise(int iStage)
        {
            if (iStage == 1)
            {
                ozcot_InitVarsFlags();   //Note: this is done AFTER the initProperty is executed
            }
            if (iStage == 2)
            {

                // Report progress 
                Console.WriteLine("\n\n------- OZCOT Initialisation ----------------------------------\n"); 

                // add properties to be set in other components (eg SoilWater updated values returned)
                //
                // Example **********
                // addSetterProperty("dlt_sw_dep", 0, "dlt_sw_dep", 0, "mm", true, buf);
                // ******************

                // add property for SoilWat2.dlt_sw_dep   (change in soil water by layer)
                setterID = setPropertyList.Count ;
                string buf = "";
                MakeDDML("dlt_sw_dep", "double", true, "mm", ref buf);
                addSetterProperty("dlt_sw_dep", setterID, "dlt_sw_dep", 0, "mm", true, buf);

                // add property for SoilN2.dlt_no3   (change in soil nitrate by layer)
                setterID = setPropertyList.Count ;
                buf = "";
                MakeDDML("dlt_no3", "double", true, "kg/ha", ref buf);
                addSetterProperty("dlt_no3", setterID, "dlt_no3", 0, "kg/ha", true, buf);
            }
        }

        //==========================================================================
        /// <summary>
        /// Set the value of one of the custom init variables
        /// </summary>
        //==========================================================================
        public override void initProperty(int iPropertyID, TTypedValue aValue)
        {
            uint i = 0;  //loop index
            uint j = 0;  // secondary loop index


            switch (iPropertyID)
            {
                case PROP_crop_type: crop_type = aValue.asStr();
                    break;

                case PROP_leaf_res_n_conc: leaf_res_n_conc = aValue.asDouble();
                    break;

                case PROP_hucut: hucut = aValue.asDouble();
                    break;

                case PROP_baset: baset = aValue.asDouble();
                    break;

                case PROP_open_def: open_def = aValue.asDouble();
                    break;

                case PROP_a_root_leaf: a_root_leaf = aValue.asDouble();
                    break;

                case PROP_a_stem_leaf: a_stem_leaf = aValue.asDouble();
                    break;

                case PROP_e_par: e_par = aValue.asDouble();
                    break;

                case PROP_specific_lw: specific_lw = aValue.asDouble();
                    break;

                case PROP_t_opt: t_opt = aValue.asDouble();
                    break;

                case PROP_t_base: t_base = aValue.asDouble();
                    break;

                case PROP_wt_area_max: wt_area_max = aValue.asDouble();
                    break;

                case PROP_embryo: embryo = aValue.asDouble();
                    break;

                case PROP_f_leaf: f_leaf = aValue.asDouble();
                    break;

                case PROP_f_stem: f_stem = aValue.asDouble();
                    break;

                case PROP_f_root: f_root = aValue.asDouble();
                    break;

                case PROP_elevation_default: elevation_default = aValue.asDouble();
                    break;

                case PROP_wlog_assimilate_red: wlog_assimilate_red = aValue.asDouble();
                    break;

                case PROP_wlog_carcap_red: wlog_carcap_red = aValue.asDouble();
                    break;

                case PROP_watlog_c: watlog_c = aValue.asDouble();
                    break;

                case PROP_watlog_n: watlog_n = aValue.asDouble();
                    break;

                case PROP_wlog_carcap_red_stress: wlog_carcap_red_stress = aValue.asDouble();
                    break;

                case PROP_smi_affect_wlog: smi_affect_wlog = aValue.asDouble();
                    break;

                case PROP_days_relief_wlog: days_relief_wlog = aValue.asInt();
                    break;

                case PROP_frost_kill_immediate: frost_kill_immediate = aValue.asDouble();
                    break;

                case PROP_rtdep_max: rtdep_max = aValue.asDouble();
                    break;

                case PROP_harvest_n_frac: harvest_n_frac = aValue.asDouble();
                    break;

                case PROP_cutout_smi_crit: cutout_smi_crit = aValue.asDouble();
                    break;

                case PROP_cutout_smi_days: cutout_smi_days = aValue.asInt();
                    break;

                case PROP_cutout_smi_site_red: cutout_smi_site_red = aValue.asDouble();
                    break;

                case PROP_epcoef1: epcoef1 = aValue.asDouble();
                    break;

                case PROP_epcoef2: epcoef2 = aValue.asDouble();
                    break;

                case PROP_epcoef_smi_crit: epcoef_smi_crit = aValue.asDouble();
                    break;

                case PROP_fbwstr_low: fbwstr_low = aValue.asDouble();
                    break;

                case PROP_fbwstr_high: fbwstr_high = aValue.asDouble();
                    break;

                case PROP_fbwstr_a: fbwstr_a = aValue.asDouble();
                    break;

                case PROP_fbnstr_low: fbnstr_low = aValue.asDouble();
                    break;

                case PROP_fbnstr_high: fbnstr_high = aValue.asDouble();
                    break;

                case PROP_fbnstr_a: fbnstr_a = aValue.asDouble();
                    break;

                case PROP_relp_smi_crit: relp_smi_crit = aValue.asDouble();
                    break;

                case PROP_relp_intercept: relp_intercept = aValue.asDouble();
                    break;

                case PROP_relp_slope: relp_slope = aValue.asDouble();
                    break;

                case PROP_relp_low: relp_low = aValue.asDouble();
                    break;

                case PROP_relp_high: relp_high = aValue.asDouble();
                    break;

                case PROP_relp_a: relp_a = aValue.asDouble();
                    break;

                case PROP_vsnstr_low: vsnstr_low = aValue.asDouble();
                    break;

                case PROP_vsnstr_high: vsnstr_high = aValue.asDouble();
                    break;

                case PROP_vsnstr_a: vsnstr_a = aValue.asDouble();
                    break;

                case PROP_flfsmi_low: flfsmi_low = aValue.asDouble();
                    break;

                case PROP_flfsmi_high: flfsmi_high = aValue.asDouble();
                    break;

                case PROP_flfsmi_a: flfsmi_a = aValue.asDouble();
                    break;

                case PROP_vlnstr_low: vlnstr_low = aValue.asDouble();
                    break;

                case PROP_vlnstr_high: vlnstr_high = aValue.asDouble();
                    break;

                case PROP_vlnstr_a: vlnstr_a = aValue.asDouble();
                    break;

                case PROP_fw_low: fw_low = aValue.asDouble();
                    break;

                case PROP_fw_high: fw_high = aValue.asDouble();
                    break;

                case PROP_fw_a: fw_a = aValue.asDouble();
                    break;

                case PROP_adjust_low: adjust_low = aValue.asDouble();
                    break;

                case PROP_adjust_high: adjust_high = aValue.asDouble();
                    break;

                case PROP_adjust_a: adjust_a = aValue.asDouble();
                    break;

                case PROP_fwstrs_low: fwstrs_low = aValue.asDouble();
                    break;

                case PROP_fwstrs_high: fwstrs_high = aValue.asDouble();
                    break;

                case PROP_fwstrs_a: fwstrs_a = aValue.asDouble();
                    break;

                case PROP_smi_delay_crit: smi_delay_crit = aValue.asDouble();
                    break;

                case PROP_cold_shock_delay_crit: cold_shock_delay_crit = aValue.asDouble();
                    break;

                case PROP_cold_shock_delay: cold_shock_delay = aValue.asDouble();
                    break;

                case PROP_fert_crit: fert_crit = aValue.asDouble();
                    break;

                case PROP_fert_detect: fert_detect = aValue.asDouble();
                    break;

                case PROP_days_since_fert_max: days_since_fert_max = aValue.asInt();
                    break;

                case PROP_ll: 
                    unul = new double[aValue.count() + 1];
                    unul[0] = 0;
                    for (i = 1; i <= aValue.count(); i++)
                    {
                        unul[i] = aValue.item(i).asDouble();
                    }
                    num_ll_vals = (int)aValue.count();
                    break;

                case PROP_cultivar_default: cultivar_default = aValue.asStr();
                    break;


                case PROP_cultivars_list :
                    CultivarParams[] initCultivars = new CultivarParams[aValue.count()];
                    for (i = 1; i <= aValue.count(); i++)
                    {
                        TTypedValue currentCultivar = aValue.item(i);
                        CultivarParams newCultivar = new CultivarParams();

                        //C# is 0 based, TTypedValues are 1 based arrays
                        newCultivar.name = currentCultivar.member("cultivar").asString();
                        newCultivar.pclint = currentCultivar.member("percent_l").asDouble();
                        newCultivar.scboll = currentCultivar.member("scboll").asDouble();
                        newCultivar.respcon = currentCultivar.member("respcon").asDouble();
                        newCultivar.sqcon = currentCultivar.member("sqcon").asDouble();
                        newCultivar.fcutout = currentCultivar.member("fcutout").asDouble();
                        newCultivar.flai = currentCultivar.member("flai").asDouble();
                        newCultivar.ddisq = currentCultivar.member("DDISQ").asDouble();
                        newCultivar.tipout = currentCultivar.member("TIPOUT").asDouble();

                        newCultivar.FRUDD[0] = 0; // initialise position 0 to 0
                        for (j=1; j<=currentCultivar.member("FRUDD").count(); j++)
                        {
                            newCultivar.FRUDD[j] = currentCultivar.member("FRUDD").item(j).asDouble();
                        }

                        newCultivar.BLTME[0] = 0; // initialise position 0 to 0
                        for (j=1; j<=currentCultivar.member("BLTME").count(); j++)
                        {
                            newCultivar.BLTME[j] = currentCultivar.member("BLTME").item(j).asDouble();
                        }

                        newCultivar.WT[0] = 0; // initialise position 0 to 0
                        for (j=1; j<=currentCultivar.member("WT").count(); j++)
                        {
                            newCultivar.WT[j] = currentCultivar.member("WT").item(j).asDouble();
                        }

                        newCultivar.dlds_max = currentCultivar.member("dlds_max").asDouble();
                        newCultivar.rate_emergence = currentCultivar.member("rate_emergence").asDouble();
                        newCultivar.popcon = currentCultivar.member("popcon").asDouble();
                        newCultivar.fburr = currentCultivar.member("fburr").asDouble();
                        newCultivar.acotyl = currentCultivar.member("ACOTYL").asDouble();
                        newCultivar.rlai = currentCultivar.member("RLAI").asDouble();
                        newCultivar.bckGndRetn = currentCultivar.member("BckGndRetn").asDouble();

                        initCultivars[i - 1] = newCultivar;
                    }
                    // Update the default Cultivar list with these values from the init script
                    ozcotCultivars.updateCultivarList(initCultivars);

                    break;


                case PROP_cottonCO2: 
                    co2 = aValue.asDouble();  //may be overwritten by reading of driver variable from system component
                    if (co2 < 300) co2 = co2_default;
                    break;



                case PROP_x_stem_wt:
                    if (aValue.count() > 0)
                    {
                        Array.Clear(x_stem_wt, 0, x_stem_wt.Length);
                        for (i = 1; i <= aValue.count(); i++)
                        {
                            x_stem_wt[i-1] = aValue.item(i).asDouble();  //create a zero based array
                        }
                    }
                    break;


                case PROP_y_height:
                    if (aValue.count() > 0)
                    {
                        Array.Clear(y_height, 0, y_height.Length);
                        for (i = 1; i <= aValue.count(); i++)
                        {
                            y_height[i-1] = aValue.item(i).asDouble();   //create a zero based array
                        }

                        num_height = (int)aValue.count();
                    }
                    break;


                case PROP_rootExpansionDelay: rootExpansionDelay = aValue.asInt();
                    break;


                case PROP_useDDsumTo1stSq: useDDsumTo1stSq = aValue.asBoolean();
                    break;




                default:
                    // ??
                    break;

            }
        }
        //==========================================================================
        /// <summary>
        /// Set the value of one of the driving variables
        /// </summary>
        //==========================================================================
        public override void assignDriver(int iDriverID, uint iProvider, TTypedValue aValue)
        {
            uint i;  //loop index

            switch (iDriverID)
            {
                // Climate drivers
                case DRV_MAXT:
                    tempmx = aValue.asDouble();
                    break;

                case DRV_MINT:
                    tempmn = aValue.asDouble();
                    break;

                case DRV_RADN:
                    solrad = aValue.asDouble();
                    break;

                case DRV_RAIN:
                    rain_mm = aValue.asDouble();
                    break;

                
                // Soil Water Drivers
                case DRV_DLAYR:
                    dlayr = new double[aValue.count() + 1];
                    dlayr[0] = 0;
                    for (i = 1; i <= aValue.count(); i++)
                    {
                        dlayr[i] = aValue.item(i).asDouble();
                    }
                    break;

                case DRV_BULKD:
                    bulkd = new double[aValue.count() + 1];
                    bulkd[0] = 0;
                    for (i = 1; i <= aValue.count(); i++)
                    {
                        bulkd[i] = aValue.item(i).asDouble();
                    }
                    break;

                case DRV_LL15:
                    if (num_ll_vals == 0)                      //get unavailable sw - use LL15 as crop ll has not been read
                    {
                        unul = new double[aValue.count() + 1];
                        unul[0] = 0;
                        for (i = 1; i <= aValue.count(); i++)
                        {
                            unul[i] = aValue.item(i).asDouble();
                        }
                        num_ll_vals = (int)aValue.count();
                    }
                   break;

               case DRV_DUL:
                   dul_layers = new double[aValue.count() + 1];
                   dul_layers[0] = 0;
                   for (i = 1; i <= aValue.count(); i++)
                   {
                       dul_layers[i] = aValue.item(i).asDouble();
                   }
                   break;

               case DRV_SAT:
                   sat_layers = new double[aValue.count() + 1];
                   sat_layers[0] = 0;
                   for (i = 1; i <= aValue.count(); i++)
                   {
                       sat_layers[i] = aValue.item(i).asDouble();
                   }
                   break;

               case DRV_SW:
                   swlayr = new double[aValue.count() + 1];
                   swlayr[0] = 0;
                   for (i = 1; i <= aValue.count(); i++)
                   {
                       swlayr[i] = aValue.item(i).asDouble();
                   }
                   break;

               case DRV_EO:
                   eo = aValue.asDouble();
                   break;

               case DRV_ES:
                   es = aValue.asDouble();
                   break;


               case DRV_RUNOFF:
                   q = aValue.asDouble();
                   break;


                // Soil Nitrogen Drivers
               case DRV_NO3_MIN:
                   no3mn = new double[aValue.count() + 1];
                   no3mn[0] = 0;
                   for (i = 1; i <= aValue.count(); i++)
                   {
                       no3mn[i] = aValue.item(i).asDouble();
                   }
                   break;

               case DRV_NO3:
                   no3 = new double[aValue.count() + 1];
                   no3[0] = 0;
                   for (i = 1; i <= aValue.count(); i++)
                   {
                       no3[i] = aValue.item(i).asDouble();
                   }
                   break;

               case DRV_NH4_MIN:
                   nh4mn = new double[aValue.count() + 1];
                   nh4mn[0] = 0;
                   for (i = 1; i <= aValue.count(); i++)
                   {
                       nh4mn[i] = aValue.item(i).asDouble();
                   }
                   break;

               case DRV_NH4:
                   nh4 = new double[aValue.count() + 1];
                   nh4[0] = 0;
                   for (i = 1; i <= aValue.count(); i++)
                   {
                       nh4[i] = aValue.item(i).asDouble();
                   }
                   break;

               case DRV_UREA:
                   urea = new double[aValue.count() + 1];
                   urea[0] = 0;
                   for (i = 1; i <= aValue.count(); i++)
                   {
                       urea[i] = aValue.item(i).asDouble();
                   }
                   break;



               // Climate Change Drivers
               case DRV_CO2:
                   double system_co2 = aValue.asDouble();
                    if (system_co2 > 300.0)       // if system value is above 300 (reasonable value)  then use it
                    {
                        co2 = system_co2;
                    }
                    else
                    {
                        if (co2 < 300.0)     //  if no reasonable value currently set
                        {
                            co2 = co2_default;   // use the cotton default value 
                        }
                    }
                   break;



                //case DRV_TIME:
                //    TTimeStep step = new TTimeStep();
                //    step.Set(aValue);
                //    Today = step.getStart().asDateTime();
                //    break;


                default:
                    base.assignDriver(iDriverID, iProvider, aValue);
                    break;
            }
        }
        //==========================================================================
        /// <summary>
        /// Allows the simulation system to set the value of a local property. This must
        /// be implemented here if you want to set a writeable property.
        /// </summary>
        /// <param name="iPropertyID">Local ID of the property</param>
        /// <param name="aValue">New value as a TTypedValue</param>
        /// <returns>Returns true if the value could be set</returns>
        //==========================================================================
        public override bool writeProperty(int iPropertyID, TTypedValue aValue)
        {
            bool result = false;
            uint i;

            switch (iPropertyID)
            {
               case PROP_crop_type: crop_type = aValue.asStr();
                    break;

                case PROP_cultivar_default: cultivar_default = aValue.asStr();
                    break;


                case PROP_leaf_res_n_conc: leaf_res_n_conc = aValue.asDouble();
                    break;

                case PROP_hucut: hucut = aValue.asDouble();
                    break;

                case PROP_baset: baset = aValue.asDouble();
                    break;

                case PROP_open_def: open_def = aValue.asDouble();
                    break;

                case PROP_a_root_leaf: a_root_leaf = aValue.asDouble();
                    break;

                case PROP_a_stem_leaf: a_stem_leaf = aValue.asDouble();
                    break;

                case PROP_e_par: e_par = aValue.asDouble();
                    break;

                case PROP_specific_lw: specific_lw = aValue.asDouble();
                    break;

                case PROP_t_opt: t_opt = aValue.asDouble();
                    break;

                case PROP_t_base: t_base = aValue.asDouble();
                    break;

                case PROP_wt_area_max: wt_area_max = aValue.asDouble();
                    break;

                case PROP_embryo: embryo = aValue.asDouble();
                    break;

                case PROP_f_leaf: f_leaf = aValue.asDouble();
                    break;

                case PROP_f_stem: f_stem = aValue.asDouble();
                    break;

                case PROP_f_root: f_root = aValue.asDouble();
                    break;

                case PROP_elevation_default: elevation_default = aValue.asDouble();
                    break;

                case PROP_wlog_assimilate_red: wlog_assimilate_red = aValue.asDouble();
                    break;

                case PROP_wlog_carcap_red: wlog_carcap_red = aValue.asDouble();
                    break;

                case PROP_watlog_c: watlog_c = aValue.asDouble();
                    break;

                case PROP_watlog_n: watlog_n = aValue.asDouble();
                    break;

                case PROP_wlog_carcap_red_stress: wlog_carcap_red_stress = aValue.asDouble();
                    break;

                case PROP_smi_affect_wlog: smi_affect_wlog = aValue.asDouble();
                    break;

                case PROP_days_relief_wlog: days_relief_wlog = aValue.asInt();
                    break;

                case PROP_frost_kill_immediate: frost_kill_immediate = aValue.asDouble();
                    break;

                case PROP_rtdep_max: rtdep_max = aValue.asDouble();
                    break;

                case PROP_harvest_n_frac: harvest_n_frac = aValue.asDouble();
                    break;

                case PROP_cutout_smi_crit: cutout_smi_crit = aValue.asDouble();
                    break;

                case PROP_cutout_smi_days: cutout_smi_days = aValue.asInt();
                    break;

                case PROP_cutout_smi_site_red: cutout_smi_site_red = aValue.asDouble();
                    break;

                case PROP_epcoef1: epcoef1 = aValue.asDouble();
                    break;

                case PROP_epcoef2: epcoef2 = aValue.asDouble();
                    break;

                case PROP_epcoef_smi_crit: epcoef_smi_crit = aValue.asDouble();
                    break;

                case PROP_fbwstr_low: fbwstr_low = aValue.asDouble();
                    break;

                case PROP_fbwstr_high: fbwstr_high = aValue.asDouble();
                    break;

                case PROP_fbwstr_a: fbwstr_a = aValue.asDouble();
                    break;

                case PROP_fbnstr_low: fbnstr_low = aValue.asDouble();
                    break;

                case PROP_fbnstr_high: fbnstr_high = aValue.asDouble();
                    break;

                case PROP_fbnstr_a: fbnstr_a = aValue.asDouble();
                    break;

                case PROP_relp_smi_crit: relp_smi_crit = aValue.asDouble();
                    break;

                case PROP_relp_intercept: relp_intercept = aValue.asDouble();
                    break;

                case PROP_relp_slope: relp_slope = aValue.asDouble();
                    break;

                case PROP_relp_low: relp_low = aValue.asDouble();
                    break;

                case PROP_relp_high: relp_high = aValue.asDouble();
                    break;

                case PROP_relp_a: relp_a = aValue.asDouble();
                    break;

                case PROP_vsnstr_low: vsnstr_low = aValue.asDouble();
                    break;

                case PROP_vsnstr_high: vsnstr_high = aValue.asDouble();
                    break;

                case PROP_vsnstr_a: vsnstr_a = aValue.asDouble();
                    break;

                case PROP_flfsmi_low: flfsmi_low = aValue.asDouble();
                    break;

                case PROP_flfsmi_high: flfsmi_high = aValue.asDouble();
                    break;

                case PROP_flfsmi_a: flfsmi_a = aValue.asDouble();
                    break;

                case PROP_vlnstr_low: vlnstr_low = aValue.asDouble();
                    break;

                case PROP_vlnstr_high: vlnstr_high = aValue.asDouble();
                    break;

                case PROP_vlnstr_a: vlnstr_a = aValue.asDouble();
                    break;

                case PROP_fw_low: fw_low = aValue.asDouble();
                    break;

                case PROP_fw_high: fw_high = aValue.asDouble();
                    break;

                case PROP_fw_a: fw_a = aValue.asDouble();
                    break;

                case PROP_adjust_low: adjust_low = aValue.asDouble();
                    break;

                case PROP_adjust_high: adjust_high = aValue.asDouble();
                    break;

                case PROP_adjust_a: adjust_a = aValue.asDouble();
                    break;

                case PROP_fwstrs_low: fwstrs_low = aValue.asDouble();
                    break;

                case PROP_fwstrs_high: fwstrs_high = aValue.asDouble();
                    break;

                case PROP_fwstrs_a: fwstrs_a = aValue.asDouble();
                    break;

                case PROP_smi_delay_crit: smi_delay_crit = aValue.asDouble();
                    break;

                case PROP_cold_shock_delay_crit: cold_shock_delay_crit = aValue.asDouble();
                    break;

                case PROP_cold_shock_delay: cold_shock_delay = aValue.asDouble();
                    break;

                case PROP_fert_crit: fert_crit = aValue.asDouble();
                    break;

                case PROP_fert_detect: fert_detect = aValue.asDouble();
                    break;

                case PROP_days_since_fert_max: days_since_fert_max = aValue.asInt();
                    break;

                case PROP_ll: //unul = aValue.asDouble();
                    //TODO:  dbj should this be checked against count of soil layers??
                    unul = new double[aValue.count() + 1];
                    unul[0] = 0;
                    for (i = 1; i <= aValue.count(); i++)
                    {
                        unul[i] = aValue.item(i).asDouble();
                    }
                    num_ll_vals = (int)aValue.count();
                    break;


                case PROP_percent_l: percent_l = aValue.asDouble();
                  //  percent_l = percent_l / 100.0;	        // convert to fraction  //DBJ use pcLint_frac near point of use
                    break;

                case PROP_scboll: scboll = aValue.asDouble();
                    break;

                case PROP_respcon: respcon = aValue.asDouble();
                    break;

                case PROP_sqcon: sqcon = aValue.asDouble();
                    break;

                case PROP_fcutout: fcutout = aValue.asDouble();
                    break;

                case PROP_flai: flai = aValue.asDouble();
                    break;

                case PROP_ddisq: ddisq = aValue.asDouble();
                    break;

                case PROP_popcon: popcon = aValue.asDouble();
                    break;

                case PROP_acotyl: acotyl = aValue.asDouble();
                //    acotyl = acotyl / 1000000.0;		// convert from mm2 to m2   //DBJ use acotyl_m2 at point of use
                    break;

                case PROP_rlai: rlai = aValue.asDouble();
                    break;

                case PROP_fburr: fburr = aValue.asDouble();
                    break;

                case PROP_dlds_max: dlds_max = aValue.asDouble();
                    break;

                case PROP_rate_emergence: rate_emergence = aValue.asDouble();
                   // rate_emergence = rate_emergence / 10.0;	         // convert from mm to cm   //DBJ to do conversion near point of use in emerg
                    break;

                case PROP_frudd:
                    Array.Clear(frudd, 0, frudd.Length);
                    frudd[0] = 0;
                    for (i = 1; i <= aValue.count(); i++)
                    {
                        frudd[i] = aValue.item(i).asDouble();
                    }
                    break;

                case PROP_bltme:
                    Array.Clear(bltme, 0, bltme.Length);
                    bltme[0] = 0;
                    for (i = 1; i <= aValue.count(); i++)
                    {
                        bltme[i] = aValue.item(i).asDouble();
                    }
                    break;

                case PROP_wt:
                    Array.Clear(wt, 0, wt.Length);
                    wt[0] = 0;
                    for (i = 1; i <= aValue.count(); i++)
                    {
                        wt[i] = aValue.item(i).asDouble();
                    }
                    break;
                
                default:
                    result = false;
                    break;
            }

            if (!result)
            {
                return base.writeProperty(iPropertyID, aValue);
            }
            return result;
        }
        //==========================================================================
        /// <summary>
        /// Returns (sends) the current value of owned properties
        /// </summary>
        //==========================================================================
        public override void readProperty(int iPropertyID, uint iRequestor, ref TPropertyInfo aValue)
        {
            uint i, cohorts;

            switch (iPropertyID)
            {
                case PROP_crop_type: aValue.setValue(crop_type);
                   break;

                case PROP_plant_status: 
                case PROP_PlantStatus: 
                    aValue.setValue(plant_status);
                    break;

                case PROP_leaf_res_n_conc: aValue.setValue(leaf_res_n_conc);
                    break;

                case PROP_hucut: aValue.setValue(hucut);
                    break;

                case PROP_baset: aValue.setValue(baset);
                    break;

                case PROP_open_def: aValue.setValue(open_def);
                    break;

                case PROP_a_root_leaf: aValue.setValue(a_root_leaf);
                    break;

                case PROP_a_stem_leaf: aValue.setValue(a_stem_leaf);
                    break;

                case PROP_e_par: aValue.setValue(e_par);
                    break;

                case PROP_specific_lw: aValue.setValue(specific_lw);
                    break;

                case PROP_t_opt: aValue.setValue(t_opt);
                    break;

                case PROP_t_base: aValue.setValue(t_base);
                    break;

                case PROP_wt_area_max: aValue.setValue(wt_area_max);
                    break;

                case PROP_embryo: aValue.setValue(embryo);
                    break;

                case PROP_f_leaf: aValue.setValue(f_leaf);
                    break;

                case PROP_f_stem: aValue.setValue(f_stem);
                    break;

                case PROP_f_root: aValue.setValue(f_root);
                    break;

                case PROP_elevation_default: aValue.setValue(elevation_default);
                    break;

                case PROP_wlog_assimilate_red: aValue.setValue(wlog_assimilate_red);
                    break;

                case PROP_wlog_carcap_red: aValue.setValue(wlog_carcap_red);
                    break;

                case PROP_watlog_c: aValue.setValue(watlog_c);
                    break;

                case PROP_watlog_n: aValue.setValue(watlog_n);
                    break;

                case PROP_wlog_carcap_red_stress: aValue.setValue(wlog_carcap_red_stress);
                    break;

                case PROP_smi_affect_wlog: aValue.setValue(smi_affect_wlog);
                    break;

                case PROP_days_relief_wlog: aValue.setValue(days_relief_wlog);
                    break;

                case PROP_frost_kill_immediate: aValue.setValue(frost_kill_immediate);
                    break;

                case PROP_rtdep_max: aValue.setValue(rtdep_max);
                    break;

                case PROP_harvest_n_frac: aValue.setValue(harvest_n_frac);
                    break;

                case PROP_cutout_smi_crit: aValue.setValue(cutout_smi_crit);
                    break;

                case PROP_cutout_smi_days: aValue.setValue(cutout_smi_days);
                    break;

                case PROP_cutout_smi_site_red: aValue.setValue(cutout_smi_site_red);
                    break;

                case PROP_epcoef1: aValue.setValue(epcoef1);
                    break;

                case PROP_epcoef2: aValue.setValue(epcoef2);
                    break;

                case PROP_epcoef_smi_crit: aValue.setValue(epcoef_smi_crit);
                    break;

                case PROP_fbwstr_low: aValue.setValue(fbwstr_low);
                    break;

                case PROP_fbwstr_high: aValue.setValue(fbwstr_high);
                    break;

                case PROP_fbwstr_a: aValue.setValue(fbwstr_a);
                    break;

                case PROP_fbnstr_low: aValue.setValue(fbnstr_low);
                    break;

                case PROP_fbnstr_high: aValue.setValue(fbnstr_high);
                    break;

                case PROP_fbnstr_a: aValue.setValue(fbnstr_a);
                    break;

                case PROP_relp_smi_crit: aValue.setValue(relp_smi_crit);
                    break;

                case PROP_relp_intercept: aValue.setValue(relp_intercept);
                    break;

                case PROP_relp_slope: aValue.setValue(relp_slope);
                    break;

                case PROP_relp_low: aValue.setValue(relp_low);
                    break;

                case PROP_relp_high: aValue.setValue(relp_high);
                    break;

                case PROP_relp_a: aValue.setValue(relp_a);
                    break;

                case PROP_vsnstr_low: aValue.setValue(vsnstr_low);
                    break;

                case PROP_vsnstr_high: aValue.setValue(vsnstr_high);
                    break;

                case PROP_vsnstr_a: aValue.setValue(vsnstr_a);
                    break;

                case PROP_flfsmi_low: aValue.setValue(flfsmi_low);
                    break;

                case PROP_flfsmi_high: aValue.setValue(flfsmi_high);
                    break;

                case PROP_flfsmi_a: aValue.setValue(flfsmi_a);
                    break;

                case PROP_vlnstr_low: aValue.setValue(vlnstr_low);
                    break;

                case PROP_vlnstr_high: aValue.setValue(vlnstr_high);
                    break;

                case PROP_vlnstr_a: aValue.setValue(vlnstr_a);
                    break;

                case PROP_fw_low: aValue.setValue(fw_low);
                    break;

                case PROP_fw_high: aValue.setValue(fw_high);
                    break;

                case PROP_fw_a: aValue.setValue(fw_a);
                    break;

                case PROP_adjust_low: aValue.setValue(adjust_low);
                    break;

                case PROP_adjust_high: aValue.setValue(adjust_high);
                    break;

                case PROP_adjust_a: aValue.setValue(adjust_a);
                    break;

                case PROP_fwstrs_low: aValue.setValue(fwstrs_low);
                    break;

                case PROP_fwstrs_high: aValue.setValue(fwstrs_high);
                    break;

                case PROP_fwstrs_a: aValue.setValue(fwstrs_a);
                    break;

                case PROP_smi_delay_crit: aValue.setValue(smi_delay_crit);
                    break;

                case PROP_cold_shock_delay_crit: aValue.setValue(cold_shock_delay_crit);
                    break;

                case PROP_cold_shock_delay: aValue.setValue(cold_shock_delay);
                    break;

                case PROP_fert_crit: aValue.setValue(fert_crit);
                    break;

                case PROP_fert_detect: aValue.setValue(fert_detect);
                    break;

                case PROP_days_since_fert_max: aValue.setValue(days_since_fert_max);
                    break;

                case PROP_ll: //unul
                    aValue.setElementCount((uint)unul.Length - 1);
                    for (i = 1; i < unul.Length; i++)
                    {
                        aValue.item(i).setValue(unul[i]);
                    }
                    break;

                case PROP_cultivar: aValue.setValue(cultivar);
                    break;

                case PROP_sow_depth: aValue.setValue(sow_depth_mm);
                    break;

                case PROP_row_spacing: aValue.setValue(row_spacing);
                    break;

                case PROP_plants_per_m_row: aValue.setValue(ppm_row);
                    break;

                case PROP_skiprow: aValue.setValue(skiprow);
                    break;

                case PROP_percent_l: aValue.setValue(percent_l);
                    break;

                case PROP_scboll: aValue.setValue(scboll);
                    break;

                case PROP_respcon: aValue.setValue(respcon);
                    break;

                case PROP_sqcon: aValue.setValue(sqcon);
                    break;

                case PROP_fcutout: aValue.setValue(fcutout);
                    break;

                case PROP_flai: aValue.setValue(flai);
                    break;

                case PROP_ddisq: aValue.setValue(ddisq);
                    break;

                case PROP_popcon: aValue.setValue(popcon);
                    break;

                case PROP_acotyl: aValue.setValue(acotyl);
                    break;

                case PROP_rlai: aValue.setValue(rlai);
                    break;

                case PROP_fburr: aValue.setValue(fburr);
                    break;

                case PROP_dlds_max: aValue.setValue(dlds_max);
                    break;

                case PROP_rate_emergence: aValue.setValue(rate_emergence);
                    break;

                case PROP_background_retention: aValue.setValue(background_retention);
                    break;

                case PROP_frudd: //frudd
                    aValue.setElementCount((uint)frudd.Length-1);
                    for (i = 1; i < frudd.Length ; i++)
                    {
                        aValue.item(i).setValue(frudd[i]);
                    }
                    break;

                case PROP_bltme: //bltme
                    aValue.setElementCount((uint)bltme.Length - 1);
                    for (i = 1; i < bltme.Length ; i++)
                    {
                        aValue.item(i).setValue(bltme[i]);
                    }
                    break;

                case PROP_wt: //wt
                    aValue.setElementCount((uint)wt.Length - 1);
                    for (i = 1; i < wt.Length ; i++)
                    {
                        aValue.item(i).setValue(wt[i]);
                    }
                    break;
               
                
                case PROP_ozcot_status: aValue.setValue(iend);
                    break;

                case PROP_status: aValue.setValue(iend);
                    break;

                case PROP_ozcot_sumDD: aValue.setValue(sumdd);
                    break;

                case PROP_lai: aValue.setValue(laiField);
                    break;

                case PROP_dw_total: aValue.setValue(dm_total * 10.0);  // g/m2  --> kg/ha
                    break;

                case PROP_dw_boll: aValue.setValue(dm_allBolls * 10.0);   // g/m2  --> kg/ha
                    break;

                case PROP_dn_plant: aValue.setValue(dn_plant * 10.0);  // g/m2  --> kg/ha
                    break;

                case PROP_assimilate: aValue.setValue(assimilate);  // g/m2  
                    break;

                // case PROP_growthWt: aValue.setValue(assimilate);  // g/m2    // assimilate production 
                case PROP_growthWt: aValue.setValue(ddm_leaf + ddm_stem + ddm_boll);  // g/m2  // growth rate of organs (assimilate +- reserve adjustment)
                                                                                               // excludes roots as this is not part of assimilate partioning
                    break;

                case PROP_n_green:  // (g/m2)
                    Array.Clear(dm_N, 0, dm_N.Length);   // 1 based array  
                    dm_N[root] = 0.0;       // roots not included in 'dry matter' (above ground parts only)
                    dm_N[stem] = stem_n;
                    dm_N[leaf] = leaf_n;
                    dm_N[pod] = boll_n;
                    dm_N[meal] = 0.0;      // seed content of seed cotton (open bolls)
                    dm_N[lint] = 0.0;      // lint content of seed cotton (open bolls)

                    aValue.setElementCount((uint)dm_N.Length - 1);
                    for (i = 1; i < dm_N.Length; i++)
                    {
                        aValue.item(i).setValue(dm_N[i]);
                    }
                    break;

                case PROP_n_senesced:  // (g/m2)
                    Array.Clear(dm_N, 0, dm_N.Length);   // 1 based array  
                    dm_N[root] = 0.0;
                    dm_N[stem] = 0.0;
                    dm_N[leaf] = leaf_res_n;   // g/m2
                    dm_N[pod] = 0.0;       // green bolls don't senesce
                    dm_N[meal] = 0.0;      // seed content of seed cotton (open bolls)
                    dm_N[lint] = 0.0;      // lint content of seed cotton (open bolls)

                    aValue.setElementCount((uint)dm_N.Length-1);
                    for (i = 1; i < dm_N.Length; i++)
                    {
                        aValue.item(i).setValue(dm_N[i]);
                    }
                    break;

                case PROP_dlt_dm_live:  // (g/m2)
                    Array.Clear(dm_crop, 0, dm_crop.Length);   // 1 based array  
                    dm_crop[root] = 0.0;
                    dm_crop[stem] = ddm_stem;
                    dm_crop[leaf] = ddm_leaf;
                    dm_crop[pod] = ddm_boll;
                    dm_crop[meal] = 0.0;      // seed content of seed cotton (open bolls)
                    dm_crop[lint] = 0.0;      // lint content of seed cotton (open bolls)

                    aValue.setElementCount((uint)dm_crop.Length - 1);
                    for (i = 1; i < dm_crop.Length; i++)
                    {
                        aValue.item(i).setValue(dm_crop[i]);
                    }
                    break;


                case PROP_dm: aValue.setValue(dm_total);  // g/m2  
                    break;

                case PROP_dm_green:  // (g/m2)
                    Array.Clear(dm_crop, 0, dm_crop.Length);   // 1 based array  
                    dm_crop[root] = 0.0;  // g/m2
                    dm_crop[stem] = dm_green[stem];   // + reserve  ??;          
                    dm_crop[leaf] = dm_green[leaf];
                    dm_crop[pod] = dm_green[pod];
                    dm_crop[meal] = 0.0;       // open bolls no longer growing are senesced
                    dm_crop[lint] = 0.0;       // open bolls no longer growing are senesced

                    aValue.setElementCount((uint)dm_crop.Length-1);
                    for (i = 1; i < dm_crop.Length; i++)
                    {
                        aValue.item(i).setValue(dm_crop[i]);
                    }
                    break;

                case PROP_dm_senesced:  // (g/m2)
                    Array.Clear(dm_crop, 0, dm_crop.Length);   // 1 based array  
                    dm_crop[root] = 0.0;
                    dm_crop[stem] = 0.0;
                    dm_crop[leaf] = dm_senesced[leaf];  // g/m2
                    dm_crop[pod] = 0.0;           // green bolls are growing or shed, but not senesced
                    dm_crop[meal] = dm_senesced[meal];  // seed content of seed cotton (open bolls)
                    dm_crop[lint] = dm_senesced[lint];  // lint content of seed cotton (open bolls)

                    aValue.setElementCount((uint)dm_crop.Length-1);
                    for (i = 1; i < dm_crop.Length; i++)
                    {
                        aValue.item(i).setValue(dm_crop[i]);
                    }
                    break;

                case PROP_dm_shed:  // (g/m2)
                    Array.Clear(dm_crop, 0, dm_crop.Length);   // 1 based array  
                    dm_crop[root] = 0.0;
                    dm_crop[stem] = 0.0;
                    dm_crop[leaf] = dm_shed[leaf];  // g/m2
                    dm_crop[pod] = dm_shed[pod];   // squares and green bolls shed
                    dm_crop[meal] = 0.0;
                    dm_crop[lint] = 0.0;

                    aValue.setElementCount((uint)dm_crop.Length - 1);
                    for (i = 1; i < dm_crop.Length; i++)
                    {
                        aValue.item(i).setValue(dm_crop[i]);
                    }
                    break;


                case PROP_dm_harvested:  // (g/m2)
                    Array.Clear(dm_crop, 0, dm_crop.Length);   // 1 based array  
                    dm_crop[root] = 0.0;
                    dm_crop[stem] = 0.0;
                    dm_crop[leaf] = 0.0;  
                    dm_crop[pod] = 0.0;   
                    dm_crop[meal] = dm_harvested[meal];  // seed in bolls
                    dm_crop[lint] = dm_harvested[lint];  // lint in bolls

                    aValue.setElementCount((uint)dm_crop.Length - 1);
                    for (i = 1; i < dm_crop.Length; i++)
                    {
                        aValue.item(i).setValue(dm_crop[i]);
                    }
                    break;


                case PROP_ep: aValue.setValue(ep * 10.0);   //cm to mm
                    break;

                case PROP_crop_in: 
                    if (crop_in) 
                    { 
                        aValue.setValue(1);
                    }
                    else
                    {   
                        aValue.setValue(0);
                    }
                    break;


                case PROP_stage:
                    aValue.setValue(cropStage);   
                    break;

                case PROP_stageName:
                    aValue.setValue(stageName[cropStage]);
                    break;



                case PROP_das: aValue.setValue(DAS);
                    break;

                case PROP_daysAfterSowing: aValue.setValue(DAS);
                    break;

                case PROP_sites: aValue.setValue(sites);
                    break;

                case PROP_squarz: aValue.setValue(squarz);
                    break;

                case PROP_fru_no_cat:                                     //output fruit nums in each category
                    aValue.setElementCount((uint)max_categories);
                    for (i = 1; i <= max_categories; i++)
                    {
                        aValue.item(i).setValue(frucat[i]);
                    }
                    break;

                case PROP_bollz: aValue.setValue(bollz);
                    break;

                case PROP_openz: aValue.setValue(openz);
                    break;

                case PROP_lint: aValue.setValue(alint);
                    break;

                case PROP_openwt_kg: aValue.setValue(dm_openBolls * 10.0);
                    break;

                case PROP_frudw: aValue.setValue(frudw * 10.0);
                    break;

                case PROP_frudw_tot: aValue.setValue((frudw + dm_openBolls) * 10.0);
                    break;

                case PROP_frudw_shed: aValue.setValue(dm_fruShedTot * 10.0);
                    break;

                case PROP_frun: aValue.setValue(frun * 10.0);
                    break;

                case PROP_bload: aValue.setValue(bload);
                    break;

                case PROP_carcap_c: aValue.setValue(carcap_c);
                    break;

                case PROP_carcap_n: aValue.setValue(carcap_n);
                    break;

                case PROP_vnstrs: aValue.setValue(vnstrs);
                    break;

                case PROP_fnstrs: aValue.setValue(fnstrs);
                    break;

                case PROP_dw_root: aValue.setValue(dm_root * 10.0);    //internal storage as g/m2 ,  reported as kg/ha
                    break;

                case PROP_dw_leaf: aValue.setValue(dm_leaf * 10.0);    //internal storage as g/m2 ,  reported as kg/ha
                    break;

                case PROP_dw_stem: aValue.setValue(dm_stem * 10.0);    //internal storage as g/m2 ,  reported as kg/ha
                    break;

                
                //case PROP_totnup: aValue.setValue(total_n * 10.0);    //internal storage as g/m2 ,  reported as kg/ha
                //  duplicate of PROP_nuptake "NUptake_total"
                //    break;

                case PROP_yield: aValue.setValue(alint);
                    break;

                case PROP_lint_yield: aValue.setValue(alint);
                    break;

                case PROP_cover_green:
                case PROP_CoverLive:
                    double cover = 0.0;
                    if (plant_status == status_alive)
                    {
                        cover = Math.Max((1.0 - tr), 0.0);
                    }

                    aValue.setValue(cover);
                    break;

                case PROP_cover_tot:
                case PROP_CoverTotal:
                    double cover_tot = 0.0;
                    if (plant_status == status_alive)
                    {
                        cover_tot = f_intz;
                    }

                    aValue.setValue(cover_tot);
                    break;
                    

                case PROP_availn: aValue.setValue(availn);
                    break;

                case PROP_uptakn: aValue.setValue(uptakn);
                    break;

                case PROP_tsno3: aValue.setValue(tsno3);
                    break;

                case PROP_ysno3: aValue.setValue(yest_tsno3);
                    break;

                case PROP_tsnh4: aValue.setValue(tsnh4);
                    break;

                case PROP_ysnh4: aValue.setValue(yest_tsnh4);
                    break;

                //case PROP_d_nup: aValue.setValue(dn_plant * 10.0);
                //   duplicate of PROP_n_uptake
                //    break;

                case PROP_n_uptake: aValue.setValue(dn_plant * 10.0);
                    break;

                case PROP_rtdep: aValue.setValue(rtdep);
                    break;

                case PROP_s_bed_mi: aValue.setValue(s_bed_mi);
                    break;

                case PROP_smi: aValue.setValue(smi);
                    break;

                case PROP_wli: aValue.setValue(wli);
                    break;

                case PROP_evap_plant: aValue.setValue(ep*10.0);  //cm to mm
                    break;

                case PROP_evap_soil: aValue.setValue(es * 10.0);  //cm to mm
                    break;

                case PROP_evap_pot: aValue.setValue(eo * 10.0);  //cm to mm
                    break;

                case PROP_evap_tot: aValue.setValue(et * 10.0);  //cm to mm
                    break;

                case PROP_ep_cm: aValue.setValue(ep);  //in cm 
                    break;


                case PROP_bolls_sc:
                    if (openz > 0.0)
                    {
                        aValue.setValue((dm_openBolls / openz));
                    }
                    else
                    {
                        aValue.setValue(0.0);
                    }
                    break;

                case PROP_nuptake: aValue.setValue(total_n * 10.0);
                    break;

                case PROP_squarz_max: aValue.setValue(sqzx);
                    break;

                case PROP_lai_max: aValue.setValue(alaiz);
                    break;

                case PROP_defol_das: aValue.setValue(defol1DAS);
                    break;

                case PROP_defol2_das: aValue.setValue(defol2DAS);
                    break;

                case PROP_lint_bales: aValue.setValue(alint/227);
                    break;

                case PROP_lint_kg: aValue.setValue(alint);
                    break;

                case PROP_cottonCO2: aValue.setValue(co2);  //Reports what value of CO2 cotton is using
                    break;

                //case PROP_x_co2_fert: //greenhouse gas
                //    aValue.setElementCount((uint)x_co2_fert.Length);
                //    for (i = 1; i <= x_co2_fert.Length; i++)
                //    {
                //        aValue.item(i).setValue(x_co2_fert[i-1]);
                //    }
                //    break;

                //case PROP_y_co2_fert: //greenhouse gas
                //    aValue.setElementCount((uint)y_co2_fert.Length);
                //    for (i = 1; i <= y_co2_fert.Length; i++)
                //    {
                //        aValue.item(i).setValue(y_co2_fert[i-1]);
                //    }
                //    break;

                //case PROP_x_stem_wt: //stem weight per plant  --> plant height
                //    aValue.setElementCount((uint)x_stem_wt.Length);
                //    for (i = 1; i <= x_stem_wt.Length; i++)
                //    {
                //        aValue.item(i).setValue(x_stem_wt[i-1]);
                //    }
                //    break;

                //case PROP_y_height: //plant height
                //    aValue.setElementCount((uint)y_height.Length);
                //    for (i = 1; i <= y_height.Length; i++)
                //    {
                //        aValue.item(i).setValue(y_height[i-1]);
                //    }
                //    break;

                case PROP_height: aValue.setValue(height);  
                    break;

                case PROP_ozcot_rwu: 
                     //sv- 2015_04_01 needed because ep does not equal root water uptake summed over profile. THIS NEEDS TO BE FIXED

                   //!sv- ozcot root water uptake. I added this to test if ep actually is the same as
                   //!    what the ozcot delta's the soilwater module by using dlt_sw_dep each day.
                    //!    Code below was copied from ozcot_set_other_module_vars()

                    double[] dlt_sw_dep = new double[nlayr + 1];  // soil water uptake in layer mm
                    // 
                    // SOIL WATER
                    // ==========
                    //  convert water from plant available  (cm/cm)

                    for (int layer = 1; layer <= nlayr; layer++)
                        {
                        dlt_sw_dep[layer] = (swlayr[layer] - sw_start[layer]) * dlayr_cm[layer] * 10.0;
                        dlt_sw_dep[layer] = Math.Min(0.0, dlt_sw_dep[layer]);
                        // bound_check_real_var(dlt_sw_dep[layer], -sw_start[layer] * dlayr_cm[layer] * 10.0, 0.0, "dlt_sw_dep(layer)");
                        // TODO: dbj check if bound check is really needed and code if it is
                        }

                    aValue.setElementCount((uint)dlt_sw_dep.Length - 1);
                    for (i = 1; i < dlt_sw_dep.Length; i++)
                        {
                        aValue.item(i).setValue(dlt_sw_dep[i]);
                        }

                    break;


                    // currently cannot pass 2D arrays to Manager2 scripts. Disable for now and pass individual arrays below.
                ////case PROP_openBollsArray:
                ////    //dbj - Nov_2015 details of open boll cohorts  (2D array)  [cohort,[count , dryWt , accumRainfall , empty]]

                ////    uint cohorts = (uint)openBollsArray.GetLength(0) - 1;
                ////    uint values = (uint)openBollsArray.GetLength(1);  // 300 cohorts * 4 values
                ////    aValue.setElementCount(cohorts);
                ////    for ( i = 1; i <= cohorts; i++)
                ////    {
                ////        aValue.item((uint)i).setElementCount(values);
                ////        for ( j = 1; j <= 4; j++)
                ////        {
                ////            double d = openBollsArray[i, j - 1];   //debug line
                ////            aValue.item((uint)i).item((uint)j).setValue(d);
                ////        }
                ////    }

                ////    break;




                case PROP_openBollCnts:
                    //dbj - Nov_2015 details of open boll cohorts fruit counts taken from  (2D array)  [cohort,[count , dryWt , accumRainfall , empty]]

                    cohorts = (uint)openBollsArray.GetLength(0) - 1;  // 300 cohorts
                    aValue.setElementCount(cohorts);
                    for (i = 1; i <= cohorts; i++)
                    {
                        double d = openBollsArray[i, 0];   //get the value of the fruit count
                        aValue.item(i).setValue(d);
                    }

                    break;




                case PROP_openBollWts:
                    //dbj - Nov_2015 details of open boll cohorts fruit weights taken from  (2D array)  [cohort,[count , dryWt , accumRainfall , empty]]

                    cohorts = (uint)openBollsArray.GetLength(0) - 1;  // 300 cohorts
                    aValue.setElementCount(cohorts);
                    for (i = 1; i <= cohorts; i++)
                    {
                        double d = openBollsArray[i, 1];   //get the value of the fruit weights
                        aValue.item(i).setValue(d);
                    }

                    break;




                case PROP_openBollAccumRain:
                    //dbj - Nov_2015 details of open boll cohorts accumulated rain taken from  (2D array)  [cohort,[count , dryWt , accumRainfall , empty]]

                    cohorts = (uint)openBollsArray.GetLength(0) - 1;  // 300 cohorts
                    aValue.setElementCount(cohorts);
                    for (i = 1; i <= cohorts; i++)
                    {
                        double d = openBollsArray[i, 2];   //get the value of the accumulated rain
                        aValue.item(i).setValue(d);
                    }

                    break;



                case PROP_FirstSquareDAS:
                    aValue.setValue(isq);
                    break;


                case PROP_FirstFlowerDAS:
                    aValue.setValue(firstFlowerDAS);
                    break;


                case PROP_FirstOpenBollDAS:
                    aValue.setValue(firstOpenBollDAS);
                    break;

                case PROP_useDDsumTo1stSq:
                    aValue.setValue(useDDsumTo1stSq);
                    break;


                case PROP_rootExpansionDelay:
                    aValue.setValue(rootExpansionDelay);
                    break;






                default:
                    //    result = false;
                    break;


            }
        }
        //==========================================================================
        /// <summary>
        /// Handles a returnInfo
        /// </summary>
        //==========================================================================
        public override void processEntityInfo(string sReqName, string sReturnName, uint iOwnerID,
                                               uint iEntityID, int iKind, string sDDML)
        {

        }
        //==========================================================================
        /// <summary>
        /// Processes one state of an event
        /// Returns: The guard condition/result of the state's processing
        /// </summary>
        //==========================================================================
        public override int processEventState(int iEventID, int iState, uint iPublisher, TTypedValue Params)
        {
            int gdCondition = 0;

            switch (iEventID)
            {
                case EVENT_PROCESS:   //if crop is sown then grow it 
                case EVENT_DO_GROWTH:  
                    if (iState == GROWTH_STATE_ACQUIRE) 
                    {
                        ozcot_get_drivers_daily(iEventID);    
                        // sendDriverRequest(DRV_MAXT, iEventID);	//firstly get the maxt from weather
                    }
                    else if (iState == GROWTH_STATE_EXECUTE) 
                    {
                        if ((crop_in) & (plant_status == status_alive))
                        {
                            if (jdate != isow)
                            {
                                DAS = DAS + 1;
                            }
                        }
                        else
                        {
                        }

                        ozcot2();                        // CALL OZCOT GROWTH MODEL

                        if ( DAS == max_cohort)   //
                        {
                            terminateCropError(" ***  Crop remains unterminated at 300 DAS. Check that manager harvest criteria contains an 'end_crop' event.");
                            // force a call of end_crop
                            ozcot_end_crop();
                        }
                        else
                        {
                            ozcot_set_other_module_vars(); //update other modules with changes from this component
                        }
                    }

                    gdCondition = 0;
                    break;


                case EVENT_DO_SOW:  //sow with defaults.  If crop is not sown then sow it
                    if (iState == SOW_STATE_ACQUIRE)
                    {
                        // sendDriverRequest(DRV_MAXT, iEventID);	//firstly get the maxt from weather
                        //break;
                    }
                    else if (iState == SOW_STATE_EXECUTE)
                    {
                        ozcot_use_sow_defaults();
                        ozcot_sow();
                    }

                    gdCondition = 0;
                    break;

                case EVENT_SOW:  //sow with passed parameters if crop is not sown then sow it
                    if(iState == SOW_STATE_ACQUIRE)
                    {
                            // sendDriverRequest(DRV_MAXT, iEventID);	//firstly get the maxt from weather
                            //break;
                    }
                    else if(iState == SOW_STATE_EXECUTE)
                    {
                        ozcot_use_sow_params(Params);    
                        ozcot_sow();
                    }

                    gdCondition = 0;
                    break;

                case EVENT_HARVEST: //harvest the crop
                case EVENT_DO_HARVEST: //harvest the crop on do_cotton_harvest
                    if (iState == HARVEST_STATE_ACQUIRE)
                    {
                    }
                    else if(iState == HARVEST_STATE_EXECUTE)
                    {
                            ozcot_harvest(Params);
                    }
                    gdCondition = 0;
                    break;

                case EVENT_DEFOLIATE: //defoliate the crop
                    if (iState == DEFOLIATE_STATE_ACQUIRE)
                    {
                    }
                    else if (iState == DEFOLIATE_STATE_EXECUTE)
                    {
                        ozcot_defoliate_crop();
                    }
                    gdCondition = 0;
                    break;


                case EVENT_END_CROP: //end the crop
                    if (iState == END_CROP_STATE_ACQUIRE)
                    {
                    }
                    else if (iState == END_CROP_STATE_EXECUTE)
                    {
                        ozcot_end_crop();
                    }
                    gdCondition = 0;
                    break;


                case EVENT_KILL_CROP: //kill the crop
                    if (iState == KILL_CROP_STATE_ACQUIRE)
                    {
                    }
                    else if (iState == KILL_CROP_STATE_EXECUTE)
                    {
                        ozcot_kill_crop(Params);
                    }
                    gdCondition = 0;
                    break;



                case EVENT_RESET:  // reset the component's data storage
                    	
                    //TODO: dbj call initValues ??
                    gdCondition = 0;
                    break;

                case EVENT_TICK:  //daily clock tick 
                    if (iState == TICK_STATE_DO)
                    {
                        // extract date values from tick time structure
                        // Today is a DateTime variable , could also use a TTimeStep variable
                        TTimeStep step = new TTimeStep();
                        step.Set(Params);
                        Today = step.getStart().asDateTime();
                        DOY = Today.DayOfYear;
                        jdate = DOY;
                    }
                    gdCondition = 0;
                    break;

                default:
                    gdCondition = base.processEventState(iEventID, iState, iPublisher, Params);
                    break;
            }

            return gdCondition;
        }
        //==========================================================================
        /// <summary>
        /// Handles a terminateCrop error
        /// </summary>
        //==========================================================================
        public void terminateCropError(string errText)
        {
            System.Console.WriteLine("Terminal Cotton Crop Error: " + errText);
            // end_crop called
        }

        //***************************** OZCOT CODE **********************************

        // ====================================================================
        //      subroutine init
        public void ozcot_InitVarsFlags()
        {
            // ====================================================================
            //ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
            //      initializes variables in all common blocks                  c
            //                                                                  c
            //      NOTE: Property variables read in from the SDML script       c
            //            will be overwritten here if included.                 c
            //            Initialise(Stage_1) (this routine) is called AFTER    c
            //            InitProperties (reads in SDML script)                 c
            //ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc


            crop_in = false;

            cropStage = 0;

            iend = 0;
            iday = 0;
            idayx = 0;
            last_iday = 0;
 
            jdate = 0;

            DAS = 0;
            nsince = 0;


            tempmx = 0.0;
            tempmn = 0.0;
            solrad = 0.0;
            rain_mm = 0.0;
            rain_cm = 0.0;
            tempdy = 0.0;
            tempwt = 0.0;
            wind = 0.0;
            tempav = 0.0;
            alint = 0.0;
            hunits = 0.0;
            //jh      hucut=40.0 ! abh changed from 30 to 40 - 11/11/83  //SDML
            //jh      baset=12.0                                         //SDML
            vpd = 0.0;
            //qa = 0.0;  not used
            solrto = 0.0;
            q = 0.0;
            eo = 0.0;
            es = 0.0;
            ep = 0.0;
            et = 0.0;
            ho = 0.0;
            tr = 0.0;

            isw = 0;
            rtsw = 0.0;
            // flag to initialise water balance in solwat
            //jh      ambda=1.44 ! priestly-taylor  darosa 1983
            //jh      ul1=1.4  ! limit for stage 1 es  darosa 1983     //SDML
            //jh      cona=0.35 ! es rate in stage 2  darosa 1983      //SDML

            nlayr = 0;
            nrtlayr = 0;
            ep = 0.0;
            rtsw = 1.0;

            Array.Clear(dlayr, 0, dlayr.Length);
            Array.Clear(dlayr_cm, 0, dlayr_cm.Length);
            Array.Clear(ullayr, 0, ullayr.Length);
            Array.Clear(dullayr, 0, dullayr.Length);
            Array.Clear(stlayr, 0, stlayr.Length);
            Array.Clear(swlayr, 0, swlayr.Length);
            Array.Clear(bulkd, 0, bulkd.Length);
            Array.Clear(sw_start, 0, sw_start.Length);

            sw = 0.0;
            ul = 0.0;
            wpwc = 0.0;
            
            wli = 0.0;
            f_limiting = 0.0;
            smi_row = 0.0;
            smi_pre = 0.0;
            s_bed_mi = 0.0;
            useskip = false;
            f_intz = 0.0;    //total soil cover
           // test_f_intz = 0.0;    //total soil cover

            tsno3 = 0.0;
            tsnh4 = 0.0;
            Array.Clear(no3mn, 0, no3mn.Length);
            Array.Clear(nh4mn, 0, nh4mn.Length);
            yest_tsno3 = 0.0;
            yest_tsn = 0.0;
            yest_tsnh4 = 0.0;
            Array.Clear(ano3, 0, ano3.Length);
            Array.Clear(anh4, 0, anh4.Length);

            days_since_fert = 0;

            //------------------------------------------------------------------------------
            //      'bplnt' plant parameters
            //------------------------------------------------------------------------------
            plant_status = status_out;
            istress = 0;
            ireliefco = 0;
            iemrg = 0;
            isow = 0;
            isq = 0;
            firstCallIstSq = true;
            lastlf = 0;
            ppm2 = 0.0;
            ppm2_target = 0.0;
            //ppm_row = 0.0;  //in SDML script
            ppm_sown = 0.0;
            ppm_emerge = 0.0;
            ppm_establish = 0.0;

            laiField = 0.0;
            laiRow = 0.0;
            shedlf = 0.0;
            frudw = 0.0;
            smi = 0.0;
            sdepth = 0.0;
            rtdep = 0.0;
            profileDepth = 0.0;  //rootDepthMax
            shedlf = 0.0;
            s = 0.0;
            effectiveRowSpacing = 0.0;
            pp = 0.0;
            ps = 0.0;
            // skiprow = 0.0; //in SDML

            n_cutout = 0;  //count days of cutout
            delayDD = 0.0;
            accumRateTo1stSqr = 0.0;
            delay_emerg = 0.0;
            dd_emerg = 0.0;
            nday_co = 0;
            nwet_co = 0;
            sum_tmx = 0.0;
            ave_tx = 0.0;
            fail_emrg = 0.0;
            f_die = 0.0;

            ddm_boll = 0.0;
            ddm_leaf = 0.0;
            ddm_root = 0.0;
            ddm_root_max = 0.0;
            ddm_stem = 0.0;
            leaf_res = 0.0;
            stem_res = 0.0;
            root_res = 0.0;
            leaf_res_n = 0.0;
            stem_res_n = 0.0;
            root_res_n = 0.0;
            total_n = 0.0;
            dn_plant = 0.0;


            //------------------------------------------------------------------------------
            //      'bnitr' soil nitrogen transformation parameters
            //------------------------------------------------------------------------------
            uptakn = 0.0;
            availn = 0.0;

            snaplc = 0.0;
            applied_n = 0.0;
            total_applied = 0.0;

            //------------------------------------------------------------------------------
            //      'fruits' parameters are numbers,weights,abscission counts
            //      for bolls and squares.
            //------------------------------------------------------------------------------
            bgrvar = 0.0;
            dd = 0.0;
            ddmerg = 0.0;
            sumdd = 0.0;

            Array.Clear(frucat, 0, frucat.Length);
            Array.Clear(lfru, 0, lfru.Length);
            Array.Clear(dlai, 0, dlai.Length);
            Array.Clear(ddm_l, 0, ddm_l.Length);
            Array.Clear(bpsum, 0, bpsum.Length);
            Array.Clear(fyzage, 0, fyzage.Length);
            Array.Clear(fruno, 0, fruno.Length);
            Array.Clear(fruwt, 0, fruwt.Length);
            Array.Clear(frmark, 0, frmark.Length);
            Array.Clear(fmkcat, 0, fmkcat.Length);
            Array.Clear(sfmcat, 0, sfmcat.Length);
            Array.Clear(openBollsArray, 0, openBollsArray.Length);

            //daysqz = 0.0;
            //daysfl = 0.0;
            //daysop = 0.0;


            //------------------------------------------------------------------------------
            //      'totals' counts of squares, bolls, and sites at any given time
            //------------------------------------------------------------------------------

            size = 0.0;
            bload = 0.0;
            bollz = 0.0;
            openz = 0.0;
            harvested = 0.0;
            dm_openBolls = 0.0;
            sites = 0.0;
            dm_fruShedTot = 0.0;
            dm_fruShedToday = 0.0;
            dm_fruWtToday = 0.0;

            sites1 = 0.0;
            squarz = 0.0;
            firstFlowerDOY = 0;
            firstFlowerDAS = 0;
            firstOpenBollDOY = 0;
            firstOpenBollDAS = 0;

            //------------------------------------------------------------------------------
            //      'index' stress and survival indices.
            //------------------------------------------------------------------------------
            carcap_c = 0.0;
            carcap_n = 0.0;
            cutout = 0.0;
            vnstrs = 1.0;
            fnstrs = 1.0;
            rad = 0.0;
            pcLint_frac = 0.0;
            carcap_c = 0.0;
            carcap_n = 0.0;

            idayco = 0;

            //------------------------------------------------------------------------------
            //      'bpnitr' are for plant nitrogen.
            //------------------------------------------------------------------------------
            uptakn = 0.0;
            plantn = 0.0;
            frun = 0.0;
            seed_nc = 0.0;

            //------------------------------------------------------------------------------
            //     /yield/ for output with yield
            //------------------------------------------------------------------------------
            alaiz = 0.0;
            ilaiz = 0;
            plntnz = 0.0;
            iplntn = 0;
            sqzx = 0.0;
            isqzx = 0;

            //------------------------------------------------------------------------------
            //     /pick/ variables to simulated defoliation and picking
            //------------------------------------------------------------------------------
            defoliationCnt = 0;
            defol1DAS = 0;
            defol2DAS = 0;
            maturityDAS = 0;
            defoliationDDSum = 0.0;
            defoliationDayCnt = 0;


        //------------------------------------------------------------------------------
        //     /sow/
        //------------------------------------------------------------------------------
        //jh      iwindow = 60     ! width of sowing window
        //jh      sow_sw = 0.0     ! for subsoil water rule
        //------------------------------------------------------------------------------
        //     /drywt/ variables for simulation of dry weight increase
        //------------------------------------------------------------------------------
        //jh      a_root_leaf = 1.01 ! allometric constant root:leaf. huxley's data 1964   //SDML
        //jh      a_stem_leaf = 1.25 ! allometric constant stem:leaf. huxley's data 1964   //SDML
        //jh      e_par = 2.5        ! g/mj charles-edwards et al 1986                     //SDML
        //jh      fburr = 1.23       ! abh                                                 //SDML
        //jh      specific_lw = 58.0 ! g/m2  gac 71/72, hoffman & rawlins 1971, ben-porath //SDML
        //jh      t_opt = 25.        ! constable 1981                                      //SDML
        //jh      t_base = 8.        ! constable 1981                                      //SDML
        //jh      wt_area_max = 150. ! hesketh and low 1968, hoffman & rawlins = 80.       //SDML
        //jh      wt_area_min = 30.0 ! huxley 1964
        //jh      embryo = 0.75      ! dry weight of seedling at emergence                 //SDML
        //jh      f_leaf = 0.6       ! proportion of leaf dry weight at emergence          //SDML
        //jh      f_stem = 0.15      ! ditto for stem                                      //SDML
        //jh      f_root = 0.25      ! ditto for root  -  data by extrapolation from huxlry //SDML

            bollgr = 0.0;
            bper = 0.0;
            dlai_pot = 0.0;
            dm_allBolls = 0.0;
           // dm_harvested = 0.0;
            dm_openBolls = 0.0;
            dm_leaf = 0.0;
            dm_root = 0.0;
            dm_stem = 0.0;
            dm_meal = 0.0;
            dm_lint = 0.0;
            dm_total = 0.0;
            boll_n = 0.0;
            leaf_n = 0.0;
            stem_n = 0.0;
            root_n = 0.0;
            total_n = 0.0;
            reserve = 0.0;
            res_cap = 0.0;
            root_feedback = 0.0;
            total_assimilate = 0.0;

            Array.Clear(dm_green, 0, dm_green.Length);
            Array.Clear(dm_senesced, 0, dm_senesced.Length);
            Array.Clear(dm_shed, 0, dm_shed.Length);
            Array.Clear(dm_harvested, 0, dm_harvested.Length);

            height = 0.0;

            deadFlagCnt = 0;

            return;  //ozcot_InitVarsFlags()
        }



        //         ===========================================================
        public void ozcotGetInitDrivers(int iEventID)
        {
            //     ===========================================================
            //+  purpose
            //       request the values of driver variables of this component 
            //       from other modules owning those properties.
            //       Specifically, APSIM SoilWat2 and SoilN.
            //       This is done after INIT1 and INIT2 to ensure that all components
            //       are fully registered, but needs to be done before the regular
            //       daily reading of Driver values to setup the number of values to be read.
            //       (i.e. Establish the number of Soil Layers, etc).

            //+  changes
            //     
            //      

            //+  calls
            //       sends requests to SoilWat2 and SoilN

            //+  constant values

            //+  local variables
            int layer;
            // 

            //- implementation section ----------------------------------

            // CO2 from Met file if specified
            sendDriverRequest(DRV_CO2, iEventID);

            // SOIL WATER
            // get depth of each soil water layer
            Array.Clear(dlayr, 0, dlayr.Length);
            sendDriverRequest(DRV_DLAYR, iEventID);

            // get moist bulk density
            Array.Clear(bulkd, 0, bulkd.Length);
            sendDriverRequest(DRV_BULKD, iEventID);




            // ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
            // Code to read init values from 'other' components - hence the outerNode.
            // (code by Eric Zurcher Mar 2014)

            // Crop Lower Limit values for the soil
            XmlNode initDataNode = FSDMLComp.initDataOuterNode();
            if (initDataNode != null)
            {
                XmlNode ll = FSDMLComp.firstElementChild(initDataNode, "ll");
                if (ll != null)
                {
                    string vals = ll.InnerText;
                    string[] Values = vals.Split(null as char[], StringSplitOptions.RemoveEmptyEntries);
                    double[] DoubleValues = MathUtility.StringsToDoubles(Values);
                    unul = new double[DoubleValues.Length + 1];
                    unul[0] = 0;
                    for (int i = 0; i < DoubleValues.Length; i++)
                    {
                        unul[i + 1] = DoubleValues[i];
                    }
                    num_ll_vals = DoubleValues.Length;
                }
            }
            // ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



            // get unavailable sw - use ll15 if crop_ll is unavailable
            if (num_ll_vals == 0)
            {
                Array.Clear(unul, 0, unul.Length);
                sendDriverRequest(DRV_LL15, iEventID);
            }

            // get upper limit of available sw
            Array.Clear(dul_layers, 0, dul_layers.Length);
            sendDriverRequest(DRV_DUL, iEventID);

            // get saturated value of sw
            Array.Clear(sat_layers, 0, sat_layers.Length);
            sendDriverRequest(DRV_SAT, iEventID);

            // get the current amount of soil water in each layer
            Array.Clear(swlayr, 0, swlayr.Length);
            sendDriverRequest(DRV_SW, iEventID);

 

            // Now convert values from APSIM values to values that suit OZCOT
            //===============================================================

			// convert field capacity relative to wilting point.
			// convert units to cm and cm/cm

			profileDepth = 0.0;
			ul = 0.0;
			wpwc = 0.0;
            nlayr = dlayr.Length-1;

            for (layer = 1; layer <= nlayr; layer++)
            {
                dlayr_cm[layer] = dlayr[layer] / 10.0;
                sat_adj[layer] = (dul_layers[layer] - unul[layer]) / 0.87 + unul[layer];   // adjust to match ozcot soil characterisation
                sat_adj[layer] = Math.Min(sat_adj[layer], sat_layers[layer]);
                ullayr[layer] = sat_adj[layer] - unul[layer];   // upper limit of profile
                profileDepth = profileDepth + dlayr_cm[layer];				// depth of profile
                ul = ul + ullayr[layer] * dlayr_cm[layer];		// upper limit for profile
                wpwc = wpwc + unul[layer] * dlayr_cm[layer];
            }

            if (rtdep_max == 0.0) rtdep_max = profileDepth;  // set max rooting depth to profile depth if rtdep_max not set


            // SOIL NITROGEN
            //--------------
            // get the initial estimate of soil NO3
            Array.Clear(no3mn, 0, no3mn.Length);
            sendDriverRequest(DRV_NO3_MIN, iEventID);

            // get the current soil NO3 - OPTIONAL
            Array.Clear(no3, 0, no3.Length);
            sendDriverRequest(DRV_NO3, iEventID);

            // get the initial estimate of soil NH4
            Array.Clear(nh4mn, 0, nh4mn.Length);
            sendDriverRequest(DRV_NH4_MIN, iEventID);

            // get the current soil NH4 - OPTIONAL
            Array.Clear(nh4, 0, nh4.Length);
            sendDriverRequest(DRV_NH4, iEventID);

            // get the estimate of soil UREA - OPTIONAL
            Array.Clear(urea, 0, urea.Length);
            sendDriverRequest(DRV_UREA, iEventID);


            // Sum soil nitrate over all layers  (kg/ha)
            tsno3 = 0.0;
            for (layer = 1; layer <= nlayr; layer++)
            {
                ano3[layer] = no3[layer] - no3mn[layer];
                tsno3 = tsno3 + ano3[layer];
            }

            //  Sum soil ammonimum over all layers  (kg/ha)
            tsnh4 = 0.0;
            for (layer = 1; layer <= nlayr; layer++)
            {
                anh4[layer] = nh4[layer] - nh4mn[layer];
                tsnh4 = tsnh4 + anh4[layer];
            }
  

            //// Sum soil nitrate over all layers  (kg/ha)
            //tsno3 = 0.0;
            //for (layer = 1; layer <= nlayr; layer++)
            //{
            //    ano3[layer] = no3[layer] - no3mn[layer];
            //    tsno3 = tsno3 + ano3[layer];
            //}

          
            
            // For Apsim - Report 
            // Console.WriteLine(" OZCOT GetInitDrivers() ");  //This reports AFTER the start of the simulation and before the Manager creates it varialbes


            return; // ozcotGetInitDrivers
        }

        //         ===========================================================
        public void ozcot_get_drivers_daily(int iEventID)
        {
            //     ===========================================================
            //+  purpose
            //       request the values of driver variables of this component 
            //       from other modules owning those properties.
            //       Specifically, APSIM SoilWat2 and SoilN  and a Met component.

            //+  changes
            //     
            //      

            //+  calls
            //       sends requests to SoilWat2 and SoilN

            //+  constant values

            //+  local variables
            bool N_in_system;
            int layer;
            // 

            //- implementation section ----------------------------------

            //
            // if this is the first call of this simulation run then
            // call the initialisation routine to get driver variables
            // from other components before calling the daily get driver vars.
            //
            if (firstCall)
            {
                ozcotGetInitDrivers(iEventID);
                firstCall = false;
            }

            // get climate values
            tempmx = 0.0;
            tempmn = 0.0;
            solrad = 0.0;
            rain_mm = 0.0;
            sendDriverRequest(DRV_MAXT, iEventID);
            sendDriverRequest(DRV_MINT, iEventID);
            sendDriverRequest(DRV_RADN, iEventID);
            sendDriverRequest(DRV_RAIN, iEventID);	

            // do conversions necessary
            tempav = (tempmx + tempmn) / 2.0;   // calc daily avg temp
            solrad = solrad / 0.04184;          // convert to langleys  (revised from 0.04186 Jan 2012)
            rain_cm = rain_mm / 10.0;                 // convert to cm



            // SOIL WATER
            // get the current amount of soil water in each layer
            Array.Clear(swlayr, 0, swlayr.Length);
            sendDriverRequest(DRV_SW, iEventID);

            // Now convert values from APSIM values to values that suit OZCOT
            //===============================================================

			// convert field capacity relative to wilting point.
			// convert units to cm and cm/cm

			profileDepth = 0.0;
			ul = 0.0;
			wpwc = 0.0;
            nlayr = dlayr.Length-1;

            for (layer = 1; layer <= nlayr; layer++)
            {
                dlayr_cm[layer] = dlayr[layer] / 10.0;
                sat_adj[layer] = (dul_layers[layer] - unul[layer]) / 0.87 + unul[layer];   // adjust to match ozcot soil characterisation
                sat_adj[layer] = Math.Min(sat_adj[layer], sat_layers[layer]);
                ullayr[layer] = sat_adj[layer] - unul[layer];   // upper limit of profile
                profileDepth = profileDepth + dlayr_cm[layer];				// depth of profile
                ul = ul + ullayr[layer] * dlayr_cm[layer];		// upper limit for profile
                wpwc = wpwc + unul[layer] * dlayr_cm[layer];
            }

            if (rtdep_max == 0.0) rtdep_max = profileDepth;  // set max rooting depth to profile depth if rtdep_max not set

            //  read in swlayr is in mm/mm
            //  convert water to plant available  (cm/cm)
            for (layer = 1; layer <= nlayr; layer++)
            {
                //jh        g.swlayr(layer) = g.swlayr(layer) - ll_adj(layer)
                swlayr[layer] = swlayr[layer] - unul[layer];
                swlayr[layer] = Math.Max(0.0, swlayr[layer]);
                sw_start[layer] = swlayr[layer];                  
            }

            s_bed_mi = swlayr[1] / ullayr[1];     // seed bed moisture index


            // read other values
            sendDriverRequest(DRV_ES, iEventID);
            es = es / 10.0;  // convert es from mm to cm for OZCOT

            //sv- added to get eo from soilwat instead of calculating in here -> ozcot_evap()
            sendDriverRequest(DRV_EO, iEventID);
            eo = eo / 10.0;  // convert eo from mm to cm for OZCOT


            sendDriverRequest(DRV_RUNOFF, iEventID);  // leave runoff in mm


            // SOIL NITROGEN
            //--------------
            // get the current soil NO3 - OPTIONAL
            Array.Clear(no3, 0, no3.Length);
            sendDriverRequest(DRV_NO3, iEventID);

            // get the current soil NH4 - OPTIONAL
            Array.Clear(nh4, 0, nh4.Length);
            sendDriverRequest(DRV_NH4, iEventID);

            // get the estimate of soil UREA - OPTIONAL
            Array.Clear(urea, 0, urea.Length);
            sendDriverRequest(DRV_UREA, iEventID);


            // Sum soil nitrate over all layers  (kg/ha)
            tsno3 = 0.0;
            for (layer = 1; layer <= nlayr; layer++)
            {
                ano3[layer] = no3[layer] - no3mn[layer];
                tsno3 = tsno3 + ano3[layer];
            }

            //  Sum soil ammonimum over all layers  (kg/ha)
            tsnh4 = 0.0;
            for (layer = 1; layer <= nlayr; layer++)
            {
                anh4[layer] = nh4[layer] - nh4mn[layer];
                tsnh4 = tsnh4 + anh4[layer];
            }
           
            // Need to check for situation of no SoilN module in system.
            // N_in_system = (numvals == nlayr);                    //TODO: DBJ  numvals is count of values read in. This does not exist!!  Change test!!
            
            N_in_system = ((tsno3 + tsnh4) > 0.0);         //if there is nitrate and/or ammonimum present then there is N in the system
            if ((N_in_system))      //   do nothing
            {
            }
            else
            {
                //   there is no n model in system.  feed ozcot 150 units of n to ensure that it is not limiting!!
                //   distributed throughout the profile
                for (layer = 1; layer <= nlayr; layer++)
                {
                    no3[layer] = (150.0 / nlayr * 100.0) / dlayr_cm[layer];
                }
            }
            //pc
            //jh         g.availn = g.tsno3 + g.total_n*10.0
            //jh         g.availn = g.tsno3 + g.tsnh4

            //jh      if (.not. n_in_system) then
            //jh         g.availn = g.tsno3
            //jh
            //jh      else if(g.yest_tsno3.ne.0.) then
            //jh         g.availn = g.availn + g.tsno3 - g.yest_tsno3
            //jh
            //jh      else
            //jh         g.availn = g.availn
            //jh      endif

            // get the total nitrogen in the soil layers
            double sum_no3 = 0.0;
            double sum_nh4 = 0.0;
            double sum_urea = 0.0;
            for (int i = 1; i <= no3.Length-1; i++)
            {
                sum_no3 += no3[i];
            }
            for (int i = 1; i <= nh4.Length-1; i++)
            {
                sum_nh4 += nh4[i];
            }
            for (int i = 1; i <= urea.Length-1; i++)
            {
                sum_urea += urea[i];
            }

            if (((sum_no3 + sum_nh4 + sum_urea) >= yest_tsn + fert_detect) & (yest_tsn > 0.0))
            {
                // is there a fertiliser application?
                if ((!crop_in))
                {
                    availn = yest_tsno3;
                    snaplc = 0.0;
                    total_applied = 0.0;
                    applied_n = 0.0;
                    days_since_fert = 0;
                }
            }

            if ((!crop_in & days_since_fert > days_since_fert_max))
            {
                availn = 0.0;
                snaplc = 0.0;
                total_applied = 0.0;
                applied_n = 0.0;
                days_since_fert = 0;
            }
            else
            {
                days_since_fert = days_since_fert + 1;
            }

            if ((availn <= 0.0001 & crop_in))
            {
                availn = yest_tsno3;  //  there has been no fertiliser event before sowing
            }

            //        !jhnote may be better to use sums from above.
            //        !jhnote need to put a limit of 40 days on this before sowing.

            if ((tsno3 > yest_tsno3 & yest_tsno3 > 0.0))
            {
                snaplc = tsno3 - yest_tsno3;    // a possible fertiliser event
                if ((snaplc >= fert_crit & availn > 0.0))
                {
                                               // it is a fertiliser event or result of a fertiliser event
                }
                else
                {
                    snaplc = 0.0;            // not a fertiliser event
                }
            }
            else
            {
                snaplc = 0.0;              // not a fertiliser event
            }


            yest_tsn = sum_no3 + sum_nh4 + sum_urea;


            return; // ozcot_get_drivers_daily
        }

        // ====================================================================
        public void ozcot_ResetVarsFlags()
        {
            // ====================================================================
            //ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
            //      resets variables and flags for a new crop                   c
            //ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc


            crop_in = false;
            plant_status = status_out;
            cropStage = 0;

            iend = 0;
            iday = 0;
            idayx = 0;
            last_iday = 0;
 
            jdate = 0;

            DAS = 0;
            nsince = 0;

            tsno3 = 0.0;
            tsnh4 = 0.0;
            yest_tsno3 = 0.0;
            yest_tsn = 0.0;
            yest_tsnh4 = 0.0;

            days_since_fert = 0;

            //------------------------------------------------------------------------------
            //      'bplnt' plant parameters
            //------------------------------------------------------------------------------
            istress = 0;
            ireliefco = 0;
            iemrg = 0;
            isow = 0;
            isq = 0;
            firstCallIstSq = true;
            lastlf = 0;
            ppm2 = 0.0;
            ppm2_target = 0.0;
            ppm_sown = 0.0;
            ppm_emerge = 0.0;
            ppm_establish = 0.0;

            alint = 0.0;

            laiField = 0.0;
            laiRow = 0.0;
            shedlf = 0.0;
            frudw = 0.0;
            rtdep = 0.0;
            profileDepth = 0.0;
            shedlf = 0.0;

            
            n_cutout = 0;  //count days of cutout
            delayDD = 0.0;
            accumRateTo1stSqr = 0.0;
            delay_emerg = 0.0;
            dd_emerg = 0.0;
            nday_co = 0;
            nwet_co = 0;
            sum_tmx = 0.0;
            ave_tx = 0.0;
            fail_emrg = 0.0;
            f_die = 0.0;

            f_intz = 0.0;    //total soil cover
            //test_f_intz = 0.0;    //total soil cover
            ep = 0.0;



            //------------------------------------------------------------------------------
            //      'bnitr' soil nitrogen transformation parameters
            //------------------------------------------------------------------------------
            uptakn = 0.0;
           // availn = 0.0;

            snaplc = 0.0;
            applied_n = 0.0;
            total_applied = 0.0;

            //------------------------------------------------------------------------------
            //      'fruits' parameters are numbers,weights,abscission counts
            //      for bolls and squares.
            //------------------------------------------------------------------------------
            bgrvar = 0.0;
            dd = 0.0;
            ddmerg = 0.0;
            sumdd = 0.0;

            Array.Clear(frucat, 0, frucat.Length);
            Array.Clear(lfru, 0, lfru.Length);
            Array.Clear(dlai, 0, dlai.Length);
            Array.Clear(ddm_l, 0, ddm_l.Length);
            Array.Clear(bpsum, 0, bpsum.Length);
            Array.Clear(fyzage, 0, fyzage.Length);
            Array.Clear(fruno, 0, fruno.Length);
            Array.Clear(fruwt, 0, fruwt.Length);
            Array.Clear(frmark, 0, frmark.Length);
            Array.Clear(fmkcat, 0, fmkcat.Length);
            Array.Clear(sfmcat, 0, sfmcat.Length);
            Array.Clear(openBollsArray, 0, openBollsArray.Length);

            //daysqz = 0.0;
            //daysfl = 0.0;
            //daysop = 0.0;


            //------------------------------------------------------------------------------
            //      'totals' counts of squares, bolls, and sites at any given time
            //------------------------------------------------------------------------------

            size = 0.0;
            bload = 0.0;
            bollz = 0.0;
            openz = 0.0;
            harvested = 0.0;
            sites = 0.0;

            sites1 = 0.0;
            squarz = 0.0;
            firstFlowerDOY = 0;
            firstFlowerDAS = 0;
            firstOpenBollDOY = 0;
            firstOpenBollDAS = 0;

            //------------------------------------------------------------------------------
            //      'index' stress and survival indices.
            //------------------------------------------------------------------------------
            //carcap_c = 0.0;
            //carcap_n = 0.0;
            cutout = 0.0;
            vnstrs = 1.0;
            fnstrs = 1.0;
            idayco = 0;

            //------------------------------------------------------------------------------
            //      'bpnitr' are for plant nitrogen.
            //------------------------------------------------------------------------------
            uptakn = 0.0;
            plantn = 0.0;
            frun = 0.0;
            seed_nc = 0.0;

            //------------------------------------------------------------------------------
            //     /yield/ for output with yield
            //------------------------------------------------------------------------------
            alaiz = 0.0;
            ilaiz = 0;
            plntnz = 0.0;
            iplntn = 0;
            sqzx = 0.0;
            isqzx = 0;

            //------------------------------------------------------------------------------
            //     /pick/ variables to simulated defoliation and picking
            //------------------------------------------------------------------------------
            defoliationCnt = 0;
            defol1DAS = 0;
            defol2DAS = 0;
            maturityDAS = 0;
            defoliationDDSum = 0.0;
            defoliationDayCnt = 0;

            dm_allBolls = 0.0;
            dm_fruShedTot = 0.0;
            dm_fruShedToday = 0.0;
            dm_fruWtToday = 0.0;
            dm_openBolls = 0.0;
            dm_leaf = 0.0;
            dm_root = 0.0;
            dm_stem = 0.0;
            dm_meal = 0.0;
            dm_lint = 0.0;
            dm_total = 0.0;
            boll_n = 0.0;
            leaf_n = 0.0;
            stem_n = 0.0;
            root_n = 0.0;
            total_n = 0.0;
            reserve = 0.0;
            res_cap = 0.0;
            root_feedback = 0.0;

            ddm_boll = 0.0;
            ddm_leaf = 0.0;
            ddm_root = 0.0;
            ddm_root_max = 0.0;
            ddm_stem = 0.0;
            leaf_res = 0.0;
            stem_res = 0.0;
            root_res = 0.0;
            leaf_res_n = 0.0;
            stem_res_n = 0.0;
            root_res_n = 0.0;
            total_n = 0.0;
            dn_plant = 0.0;
            total_assimilate = 0.0;  // (g/m2)

            Array.Clear(dm_green, 0, dm_green.Length);
            Array.Clear(dm_senesced, 0, dm_senesced.Length);
            Array.Clear(dm_shed, 0, dm_shed.Length);
            Array.Clear(dm_harvested, 0, dm_harvested.Length);


            height = 0.0;

            deadFlagCnt = 0;

            // Write Console line to report
            Console.WriteLine(" ");   
            Console.WriteLine(" ** Cotton Component Reset ");   
            Console.WriteLine(" ");   

            return;  //ozcot_ResetVarsFlags()
        }

        
        // ====================================================================
        public void ozcot_set_other_module_vars()
        {
			// ====================================================================
			//+  purpose
			//     update variables owned by other modules.

			//+  changes
            //

			//+  constant values

			//+  local variables
			int layer;   // layer number as integer
            double[] sno3 = new double[nlayr + 1];        // available soil nitrate in layer kg/ha
            double[] snh4 = new double[nlayr + 1];        // available soil ammonium in layer kg/ha
            double[] dlt_no3 = new double[nlayr + 1];     // soil no3 uptake in kg/ha
            double[] dlt_nh4 = new double[nlayr + 1];     // soil nh4 uptake in kg/ha
            double[] dlt_sw_dep = new double[nlayr + 1];  // soil water uptake in layer mm
            double trtsno3;    // total no3 in root zone.
            double trtsnh4;    // total nh4 in root zone.
			//jh      real    sw_dep(max_layers) ! soil water uptake in layer mm

            TSetterProperty setter;
        //    IntPtr msg;

			//- implementation section ----------------------------------

            Array.Clear(dlt_sw_dep, 0, dlt_sw_dep.Length);
            Array.Clear(dlt_no3, 0, dlt_no3.Length);
            Array.Clear(dlt_nh4, 0, dlt_nh4.Length);
			//jh      sw_dep(:) = 0.0

            // 
            // SOIL WATER
            // ==========
            //  convert water from plant available  (cm/cm)
            for (layer = 1; layer <= nlayr; layer++)
            {
				dlt_sw_dep[layer] = (swlayr[layer] - sw_start[layer]) * dlayr_cm[layer] * 10.0;
				dlt_sw_dep[layer] = Math.Min(0.0, dlt_sw_dep[layer]);
				// bound_check_real_var(dlt_sw_dep[layer], -sw_start[layer] * dlayr_cm[layer] * 10.0, 0.0, "dlt_sw_dep(layer)");
                // TODO: dbj check if bound check is really needed and code if it is
			}

			//  send updated soil water
            if (getSetterByName("dlt_sw_dep", ref setterID))
            {
                setter = (TSetterProperty)setPropertyList[setterID];
                //setter.setValue(dlt_sw_dep);
                setter.setElementCount((uint)dlt_sw_dep.Length - 1);
                for (uint i = 1; i < dlt_sw_dep.Length; i++)
                {
                    setter.item(i).setValue(dlt_sw_dep[i]);
                }

                sendReqSetValue(setterID, setter);
                //setPropertyList[setterID] = setter;
                //// msg = buildRequestSetMsg(  (setterID, (TTypedValue)setPropertyList[setterID]);
                //msg = (IntPtr)null;
                //buildRequestSetMsg(setterID, (TTypedValue)setPropertyList[setterID], ref msg);
                // SendMsg(pWrapper, msg);
            }
            else
            {
                // setter property not found
                sendError("Setter Property '" + dlt_sw_dep + "' has not been found", false);
            }

       
            // 
            // SOIL NITROGEN
            // =============
            trtsno3 = 0.0;
            for (layer = 1; layer <= nrtlayr; layer++)
            {
                trtsno3 += ano3[layer];
            }

			trtsnh4 = 0.0;
            if(dn_plant > ((trtsno3 + trtsnh4) / 10.0))   dn_plant = ((trtsno3 + trtsnh4) / 10.0);

			for (layer = 1; layer <= nlayr; layer++) {
				if ((trtsno3 + trtsnh4 > 0.0 & layer <= nrtlayr))
				{
					// dbj  dlt_no3[layer] = dn_plant * 10.0 * ano3[layer] / (trtsno3 + trtsnh4);
                    dlt_no3[layer] = -dn_plant * 10.0 * ano3[layer] / (trtsno3 + trtsnh4);
                    dlt_no3[layer] = Math.Min(0.0, dlt_no3[layer]);
					//jh    dlt_nh4(layer) = -g.dn_plant*10. * g.anh4(layer)/ (trtsno3 + trtsnh4)
				}
				else
				{
					dlt_no3[layer] = 0.0;
					dlt_nh4[layer] = 0.0;
				}
				ano3[layer] = ano3[layer] + dlt_no3[layer];
				ano3[layer] = Math.Max(0.0, ano3[layer]);
				sno3[layer] = ano3[layer] + no3mn[layer];
			}

           // Console.WriteLine("\n dlt_no3 = {0} {1} {2} {3} {4}  ", dlt_no3[1], dlt_no3[2],dlt_no3[3],dlt_no3[4],dlt_no3[5]) ;
           // Console.WriteLine("  \n   ");
            
            
            //jh      if (trtsno3+ trtsnh4 .gt. 0.0) then
			//jh         g.yest_tsno3 = g.tsno3 - (g.dn_plant*10.)
			//jh     :                       * trtsno3 /(trtsno3+trtsnh4)
			//jh         g.yest_tsnh4 = g.tsnh4 - (g.dn_plant*10.)
			//jh     :                       * trtsnh4 /(trtsno3+trtsnh4)
			//jh      else
			//jh         g.yest_tsno3 = 0.0
			//jh         g.yest_tsnh4 = 0.0
			//jh       endif
			yest_tsno3 = tsno3;

            // send updated soil n
            if (getSetterByName("dlt_no3", ref setterID))
            {
                setter = (TSetterProperty)setPropertyList[setterID];
                //setter.setValue(dlt_no3);
                setter.setElementCount((uint)dlt_no3.Length - 1);
                for (uint i = 1; i < dlt_no3.Length; i++)
                {
                    setter.item(i).setValue(dlt_no3[i]);
                }

                sendReqSetValue(setterID, setter);
                //setPropertyList[setterID] = setter;
                //// msg = buildRequestSetMsg(setterID, (TTypedValue)setPropertyList[setterID]);
                //msg = (IntPtr)null;
                //buildRequestSetMsg(setterID, (TTypedValue)setPropertyList[setterID], ref msg);
                //SendMsg(pWrapper, msg);
            }
            else
            {
                // error setter property not found
                sendError("Setter Property '" + dlt_no3 + "' has not been found.", false);

            }

			//jh      call set_real_array (unknown_module, 'dlt_nh4', '(kg/ha)'
			//jh     :                    , dlt_nh4, g.nlayr)

	
            return; // ozcot_set_other_module_vars
        }
        // ====================================================================
        public void ozcot_use_sow_defaults()
        {
            // ====================================================================
            //ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
            //      intiialise with sowing defaults                             c
            //ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

            Console.WriteLine("\n{0} {1} {2} (Day of year={3}), cotton:  ", Today.Day, Today.ToString("MMMM"), Today.Year, DOY);
            Console.WriteLine("      Sowing");

            cultivar = cultivar_default;
            sow_depth_mm = sowing_depth_default;
            row_space_mm = row_spacing_default;
            ppm_row = ppm_row_default;
            skiprow = skiprow_default;

            CultivarParams sowCultivar = ozcotCultivars.getCultivar(cultivar_default);
            cultivar = sowCultivar.name;

            // varietal parameters
            percent_l = sowCultivar.pclint;
            scboll = sowCultivar.scboll;
            respcon = sowCultivar.respcon;
            sqcon = sowCultivar.sqcon;
            fcutout = sowCultivar.fcutout;
            flai = sowCultivar.flai;
            ddisq = sowCultivar.ddisq;
            dlds_max = sowCultivar.dlds_max;
            popcon = sowCultivar.popcon;
            acotyl = sowCultivar.acotyl;
            rlai = sowCultivar.rlai;
            frudd = sowCultivar.FRUDD;
            bltme = sowCultivar.BLTME;
            wt = sowCultivar.WT;
            fburr = sowCultivar.fburr;
            rate_emergence = sowCultivar.rate_emergence;
            background_retention = sowCultivar.bckGndRetn;
            //tipout = sowCultivar.tipout;  //not assigned / not used

            //report the cultivar parameters being used
            ozcot_report_cultivar_params();
            
            
            return;
        }
        // ====================================================================
        public void ozcot_use_sow_params(TTypedValue Parameters)
        {
            // ====================================================================
            //ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
            //      intiialise with sowing parameters                           c
            //ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

            Console.WriteLine("\n{0} {1} {2} (Day of year={3}), cotton:  ", Today.Day, Today.ToString("MMMM"), Today.Year, DOY);
            Console.WriteLine("      Sowing");

            cultivar = Parameters.member("Cultivar").asString();
            sow_depth_mm = Parameters.member("sowing_depth").asDouble();
            row_space_mm = Parameters.member("row_spacing").asDouble();
            ppm_row = Parameters.member("plants_pm").asDouble();
            double plants = Parameters.member("plants").asDouble();
            skiprow = (int)(Parameters.member("SkipRow").asDouble()) ;

            // allow sowing density in plant/m2  instead of plants per metre row.
            if (plants > 0.0 && ppm_row == 0.0)
            {
                ppm_row = plants / (row_space_mm * 1000);
                // skiprow adjustment needed?  depends on what value is entered as plants.
            }
           

            // Retrieve the cultivar specific parameters 
            //  and assign them to the working values
            
            CultivarParams sowCultivar = new CultivarParams();

            if (ozcotCultivars.cultivarExists(cultivar))
            {
                sowCultivar = ozcotCultivars.getCultivar(cultivar);
            }
            else   // Cultivar not found  -  Report error and use Defaults
            {
                Console.WriteLine("Sowing Cultivar '{0}' not found. Using default cultivar '{1}' parameters.", cultivar, cultivar_default);

                sowCultivar = ozcotCultivars.getCultivar(cultivar_default);
            }

            cultivar = sowCultivar.name;

            // varietal parameters
            percent_l = sowCultivar.pclint;
            scboll = sowCultivar.scboll;
            respcon = sowCultivar.respcon;
            sqcon = sowCultivar.sqcon;
            fcutout = sowCultivar.fcutout;
            flai = sowCultivar.flai;
            ddisq = sowCultivar.ddisq;
            dlds_max = sowCultivar.dlds_max;
            popcon = sowCultivar.popcon;
            acotyl = sowCultivar.acotyl;
            rlai = sowCultivar.rlai;
            frudd = sowCultivar.FRUDD;
            bltme = sowCultivar.BLTME;
            wt = sowCultivar.WT;
            fburr = sowCultivar.fburr;
            rate_emergence = sowCultivar.rate_emergence;
            background_retention = sowCultivar.bckGndRetn;
            //tipout = sowCultivar.tipout;  //not assigned / not used
            

            //report the cultivar parameters being used
            ozcot_report_cultivar_params();

            return;
        }

        // ====================================================================
        public void ozcot_report_cultivar_params()
        {
            // ====================================================================
            //ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
            //      output to console the cultivar parameters                   c
            //ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

            Console.WriteLine("\n\n        - Reading Cultivar Parameters");
            Console.WriteLine("         ---------------------------------------");
            Console.WriteLine("         Cultivar   =  {0}", cultivar);
            Console.WriteLine("         percent_L  =  {0}", percent_l.ToString("#00.00"));
            Console.WriteLine("         scboll     =  {0}", scboll.ToString("##0.0"));
            Console.WriteLine("         respcon    =  {0}", respcon.ToString("##0.000"));
            Console.WriteLine("         sqcon      =  {0}", sqcon.ToString("##0.000"));
            Console.WriteLine("         fcutout    =  {0}", fcutout.ToString("##0.0000"));
            Console.WriteLine("         flai       =  {0}", flai.ToString("##0.0"));
            Console.WriteLine("         ddisq      =  {0}", ddisq.ToString("##0.0"));
            Console.WriteLine("         dlds_max   =  {0}", dlds_max.ToString("##0.00"));
            Console.WriteLine("         popcon     =  {0}", popcon.ToString("##0.00000"));
            Console.WriteLine("         acotyl     =  {0}", acotyl.ToString("##0.0"));
            Console.WriteLine("         rlai       =  {0}", rlai.ToString("##0.000"));
            Console.Write    ("         frudd      =  ") ; PrintArray(frudd,"frudd");
            Console.Write    ("         bltme      =  ") ; PrintArray(bltme,"bltme");
            Console.Write    ("         wt         =  ") ; PrintArray(wt,"wt");
            Console.WriteLine("         fburr      =  {0}", fburr.ToString("##0.00"));
            Console.WriteLine("         rate_emergence       =  {0}", rate_emergence.ToString("##0.0"));
            Console.WriteLine("         background_retention =  {0}", background_retention.ToString("##0.00"));
            Console.WriteLine("\n");

            Console.WriteLine("         co2 at  {0} ppm", co2.ToString("##0"));
            Console.WriteLine("");


            return;
        }

        // ====================================================================
        // output array values
        static void PrintArray< E >( E[] inputArray, string arrayName )
        {
            string str;
            switch (arrayName)
            {
               case "frudd":
                   {
                     for(uint i = 1; i <= 8; i++)
                     {
                         str = String.Format("{0:##0.}",inputArray[i]);
                          Console.Write("{0}" + "  ",str) ; 
                     }

                     Console.WriteLine("");
                     break;
                   }

               case "bltme":
                   {
                       for (uint i = 1; i <= 8; i++)
                       {
                           str = String.Format("{0:0.00}", inputArray[i]);
                           Console.Write("{0}" + "  ", str);
                       }

                       Console.WriteLine("");
                       break;
                   }
               case "wt":
                   {
                       for (uint i = 1; i <= 8; i++)
                       {
                           str = String.Format("{0:0.0000}", inputArray[i]);
                           Console.Write("{0}" + "  ", str);
                       }

                       Console.WriteLine( "" );
                       break;
                   }

               default: 
                   break;

           // Console.WriteLine( "\n" );
           }  // end switch

        } // end method PrintArray 
        // ====================================================================

        // ====================================================================
        //      subroutine sow
        public void ozcot_sow()
        {
            // ====================================================================
            //ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
            //      initializes variables at sowing                             c
            //                                                                  c
            //      key variables:                                              c
            //ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc



            // Publish New Crop event
            // set crop_type and sender
            TTypedValue thisEvent = eventList[evtNewCrop];
            thisEvent.member("crop_type").setValue("cotton");
            thisEvent.member("sender").setValue("Cotton");
            eventList[evtNewCrop] = thisEvent;
            sendPublishEvent(evtNewCrop, false);

            // Publish a 'Sowing' event  (no parameters)
            sendPublishEvent(evtSowing, false);



            crop_in = true;
            DAS = 0;
            plant_status = status_alive;
            cropStage = germination;
            sdepth = sow_depth_mm / 10.0;
            row_spacing = row_space_mm / 1000.0;       // row spacing in metres
            isow = jdate;
            rtdep = sdepth;
            effectiveRowSpacing = row_spacing * (2.0 + skiprow) / 2.0;    // effective row spacing with skip
            ppm2 = ppm_row / effectiveRowSpacing;                        // effective population density (plants per square metre)
            ppm2_target = ppm2;                                           // adjust target population for non standard rows incl skip
            pp = ppm2_target * effectiveRowSpacing;                      //TODO: dbj to check calculation and application
            ps = (1.0 / effectiveRowSpacing) / pp;
            s = ps / effectiveRowSpacing;
            //jh v2001       g.rrig(sw_sowing) = g.sw               ! soil water at sowing
            iend = 0;
            initial_call = true;


            //         ! report

            Console.WriteLine("\n\n                      Crop Sowing Data");
            Console.WriteLine("         ------------------------------------------------");
            Console.WriteLine("         Sowing  Depth  Plants   Spacing    Cultivar   Skip "  );
            Console.WriteLine("         Day no    mm     /m        mm        Name     Row  ");
            Console.WriteLine("         ---------------------------------------------------");
            Console.WriteLine("           {0}    {1}    {2}    {3}      {4}      {5}", isow, (sdepth*10).ToString("##.0"), pp.ToString("##.0"), row_space_mm.ToString("####.0"), cultivar, skiprow);
            Console.WriteLine("         ---------------------------------------------------\n\n");



            // Report soil here as component has no point at which to break when Driver Variables are being read

            Console.WriteLine("\n               Root Profile");
            Console.WriteLine("        -----------------------------");
            Console.WriteLine("            Layer          Lower ");
            Console.WriteLine("            Depth          Limit ");
            Console.WriteLine("             (cm)         (mm/mm) ");
            Console.WriteLine("        -----------------------------");

            for (uint i = 1; i <= num_ll_vals; i++)  // array entries [0] are blank. Values stored in layer numbers.
            {
                Console.WriteLine("             {0}          {1} ", dlayr_cm[i].ToString("#0.0"), unul[i].ToString("0.000"));
            }

            Console.WriteLine("        -----------------------------\n\n");


            return;  //ozcot_sow()
            
        }


        //     ===========================================================
        public void ozcot_kill_crop(TTypedValue Parameters)
        {
            //  ===========================================================

            //+  purpose
            //       stop the crop growing
            //       



            if (crop_in)
            {

                //place holders - kill_crop parameters not implemented in Cotton - kill_crop kills entire crop
                double killCropFraction = Parameters.member("KillFraction").asDouble();  //fraction of crop to be removed

                // needs coding enhancements
                plant_status = status_dead;

                // transfer drymatter pools into 'dead' pools?

            }
            else
            {
            }
            return; // ozcot_kill_crop
        }



        //     ===========================================================
        public void ozcot_end_crop()
        {
            //     ===========================================================

            //+  purpose
            //       terminate the crop and return organic matter to SurfaceOM
            //       
            //+  changes
            //     080316 dbj coded

            //+  constant values

            //+  local variables



            double[] fraction_to_residue = new double[max_part + 1];    // fraction sent to residue (0-1) as double
            double[] dlt_dm_crop = new double[max_part + 1];            // change in dry matter of crop (kg/ha) as double
            double[] dlt_dm_n = new double[max_part + 1];               // change in n content of dry matter (kg/ha) as double
            double[] root_length_array = new double[max_layers];        // lenght of roots in given layer
            double[] dlt_dm_incorp = new double[max_layers];            // change in n content of dry matter (kg/ha) to be incorporated
            double[] dlt_N_incorp = new double[max_layers];             // change in n content of N (kg/ha) to be incorporated

            int deepest_layer;


            //- implementation section ----------------------------------


            if (crop_in)
            {

                //      res_dm = (dm_total - openwt / effectiveRowSpacing ) * 10.
                //      res_dm = (dm_total - openwt) * 10.
                //      if (res_dm.le.0.) res_dm = 0.
                //      res_n = res_dm * 0.4 / 100.0

                Array.Clear(fraction_to_residue, 0, fraction_to_residue.Length);
                Array.Clear(dlt_dm_crop, 0, dlt_dm_crop.Length);
                Array.Clear(dlt_dm_n, 0, dlt_dm_n.Length);

                //     ! update biomass and n pools.  different types of plant pools are affected differently.
                //     ! =====================================================================================
                //     
                //     NOTE:  Values are passed in kg/ha units
                //            ('dm' is traditionally g/m2  and  'wt' is kg/ha)
                //

                // Roots used in IncorpFOM  not  for BiomassRemoved
                dlt_dm_crop[root] = Math.Max((dm_root * gm2kg / sm2ha), 0.0); //converting g/m2 to kg/ha  ( * 10 )
                dlt_dm_n[root] = Math.Max((root_n * gm2kg / sm2ha), 0.0);
                fraction_to_residue[root] = 0.0;

                //stem
                dlt_dm_crop[stem] = dm_green[stem] + dm_senesced[stem];  // include green and senesced (still on plant)
                dlt_dm_crop[stem] = Math.Max((dm_stem * gm2kg / sm2ha), 0.0);
                dlt_dm_n[stem] = Math.Max((dlt_dm_crop[stem] * stem_res_n_conc ), 0.0);
                fraction_to_residue[stem] = 1.0;

                //leaf
                dlt_dm_crop[leaf] = dm_green[leaf] + dm_senesced[leaf];  // include green and senesced (still on plant)
                dlt_dm_crop[leaf] = Math.Max((dm_leaf * gm2kg / sm2ha), 0.0);
                dlt_dm_n[leaf] = Math.Max((dlt_dm_crop[leaf] * leaf_res_n_conc ), 0.0);
                fraction_to_residue[leaf] = 1.0;

                // Green bolls not yet opened
                dlt_dm_crop[pod] = dm_green[pod] + dm_senesced[pod];  // include green and senesced (still on plant)
                dlt_dm_crop[pod] = Math.Max(dlt_dm_crop[pod] * gm2kg / sm2ha, 0.0);
                dlt_dm_n[pod] = Math.Max((dlt_dm_crop[pod] * pod_res_n_conc ), 0.0);
                fraction_to_residue[pod] = 1.0;

                // Open bolls should have been removed in harvest_update()
                // include here in case no harvest performed, or regrowth since harvest
                // seed portion of seed cotton in open bolls
                dlt_dm_crop[meal] = dm_senesced[meal];  // include senesced (still on plant)
                dlt_dm_crop[meal] = Math.Max((dlt_dm_crop[meal] * gm2kg / sm2ha), 0.0);
                dlt_dm_n[meal] = Math.Max((dlt_dm_crop[meal] * meal_res_n_conc), 0.0);                  //? N content of seed?
                fraction_to_residue[meal] = 1.0;

                // lint portion of seed cotton in open bolls
                dlt_dm_crop[lint] = dm_senesced[lint];  // include senesced (still on plant)
                dlt_dm_crop[lint] = Math.Max((dlt_dm_crop[lint] * gm2kg / sm2ha), 0.0);
                dlt_dm_n[lint] = Math.Max((dlt_dm_crop[lint] * meal_res_n_conc), 0.0);                  //? N content of lint?
                fraction_to_residue[meal] = 1.0;



                // Publish a 'BiomassRemoved' event  
                double dm_sum = 0.0;
                for (int i = 2; i < max_part; i++)  // exclude 'roots' (index = 1) from dm total
                {
                    dm_sum += dlt_dm_crop[i];
                }

                //if (dlt_dm_crop.Sum() > 0.0)
                if (dm_sum > 0.0)
                {
                    // need to set up the arrays of values for this event
                    TTypedValue thisEvent = eventList[evtBiomassRemoved];
                    thisEvent.member("crop_type").setValue(crop_type);           //single string value
                    thisEvent.member("dm_type").setValue(part_name);             //array of strings
                    thisEvent.member("dlt_crop_dm").setValue(dlt_dm_crop);       //array of double
                    thisEvent.member("dlt_dm_n").setValue(dlt_dm_n);             //array of double
                                                                                 // thisEvent.member("dlt_dm_p").setValue(part_name);    //phospherous not modelled in OZCOT
                    thisEvent.member("fraction_to_residue").setValue(fraction_to_residue);     //array of single
                    eventList[evtBiomassRemoved] = thisEvent;

                    sendPublishEvent(evtBiomassRemoved, false);

                }
                else
                {
                    // no surface residue
                }


                //    
                // Publish a 'IncorpFom' event  
                //============================

                ozcot_crop_root_length_distrib(ref root_length_array, dlt_dm_crop[root]);   // Note:  passed as kg/ha

                if (dlt_dm_crop[root] > 0.0)   //only do this if there are roots to incorporate
                {
                    ozcot_crop_root_incorp(dlt_dm_crop[root],    //kg/ha
                                            dlt_dm_n[root],      //kg/ha
                                            dlayr,
                                            root_length_array,
                                            rtdep,
                                            ref dlt_dm_incorp,    //kg/ha
                                            ref dlt_N_incorp);    //kg/ha


                    //DDML for new IncorpFOM event
                    //    public const string typeIncorpFOM = @"<type name=""FOMLayer"">
                    //                                <field name=""Type""   kind=""string"" /> 
                    //                                <field name=""Layer""  array=""T"">
                    //                                    <element>
                    //                                        <field name=""FOM"" kind=""defined"">
                    //                                            <field name=""amount""  kind=""single"" unit=""kg/ha"" /> 
                    //                                            <field name=""C""       kind=""single"" unit=""kg/ha"" /> 
                    //                                            <field name=""N""       kind=""single"" unit=""kg/ha"" /> 
                    //                                            <field name=""P""       kind=""single"" unit=""kg/ha"" /> 
                    //                                            <field name=""AshAlk""  kind=""single"" unit=""kg/ha"" /> 
                    //                                        </field>
                    //                                        <field name=""CNR""     kind=""single"" /> 
                    //                                        <field name=""LabileP"" kind=""single"" /> 
                    //                                    </element>
                    //                                </field>
                    //                            </type>";



                    // need to set up the arrays of values for this event
                    TTypedValue IncorpFOMEvent = eventList[evtIncorpFOM];
                    IncorpFOMEvent.member("Type").setValue(crop_type);     //cotton  - APSIM Soil probably treats this as 'Default'
                    IncorpFOMEvent.member("Layer").setElementCount((uint)nlayr);

                    deepest_layer = mvDataFunctions.find_layer_no(rtdep, dlayr, max_layers);
                    for (uint i = 1; i <= deepest_layer; i++)
                    {
                        IncorpFOMEvent.member("Layer").item(i).member("FOM").member("amount").setValue(dlt_dm_incorp[i]);
                        IncorpFOMEvent.member("Layer").item(i).member("FOM").member("N").setValue(dlt_N_incorp[i]);
                        //Zero the other values
                        IncorpFOMEvent.member("Layer").item(i).member("FOM").member("C").setValue(0.0);
                        IncorpFOMEvent.member("Layer").item(i).member("FOM").member("P").setValue(0.0);
                        IncorpFOMEvent.member("Layer").item(i).member("FOM").member("AshAlk").setValue(0.0);

                        IncorpFOMEvent.member("Layer").item(i).member("CNR").setValue(0.0);
                        IncorpFOMEvent.member("Layer").item(i).member("LabileP").setValue(0.0);
                    }

                    eventList[evtIncorpFOM] = IncorpFOMEvent;

                    sendPublishEvent(evtIncorpFOM, false);
                }   //end if  roots to incorporate


                //============

                // Zero variables on an END_CROP event 

                crop_in = false;
                plant_status = status_out;
                cropStage = 0;
                iend = 0;      //ozcot_status reset to zero

                ozcot_ResetVarsFlags();

            }
            else
            {
                ozcot_ResetVarsFlags();
            }

            return; // ozcot_end_crop
        }



        //     ===========================================================
        public void ozcot_harvest(TTypedValue Parameters)
        {
            //+  purpose
            //       report occurence of harvest and the current crop values
            //       remove harvestable crop components


            if (crop_in)
            {

                // Publish Event 'Harvesting'  Cotton is about to be harvested
                sendPublishEvent(evtHarvesting, false);

                //place holders - harvest parameters not implemented in Cotton - all open bolls are harvested
                double harvestPlants = Parameters.member("Plants").asDouble();  //fraction of plants to remove from population
                double harvestRemove = Parameters.member("Remove").asDouble();  //fraction of stover to move to remove from dry matter pool
                double harvestHeight = Parameters.member("Height").asDouble();  //height at which to cut crop
                string harvestReport = Parameters.member("Report").asString();  //report harvest parameters

                ozcot_harvest_report();
                ozcot_harvest_update();

            }
            else
            {
            }
            return; // ozcot_harvest
        }


        //     ===========================================================
        public void ozcot_harvest_report()
        {
            //     ===========================================================

            //+  purpose
            //     report the current status of specific variables.

            //+  changes
            //     051101 jngh specified and programmed

            //+  constant values

            //+  local variables

            // double yield;              // lint yield dry wt (kg/ha) 
            double wt;                 // dryMatter kg/ha
            double totnup;             // n uptake kg/ha
            double bollsc;             // seed cotton per boll (g/boll)

            //- implementation section ----------------------------------


            //  Crop harvested. Report status to summary file.

            wt = (dm_green.Sum() + dm_senesced.Sum()) * gm2kg / sm2ha;   //converting g/m2 to kg/ha (*10)  : above ground dry matter is in g/m2  while wt is converted to kg/ha
            totnup = total_n * gm2kg / sm2ha;
            if (openz > 0.0)
            {
                bollsc = dm_openBolls / openz;         // g sc/boll
            }
            else
            {
                bollsc = 0.0;
            }


            Console.WriteLine("\n      Harvest");

            Console.WriteLine("\n\n      Days after sowing      =   {0}", DAS.ToString("##0"));
            Console.WriteLine("      bolls/m2               =   {0}        Lint (kg/ha)            =   {1}", openz.ToString("##0.0"),alint.ToString("#####0.0"));
            Console.WriteLine("      N uptake (kg/ha)       =   {0}        bolls sc (g/boll)       =      {1}", totnup.ToString("##0.0"), bollsc.ToString("####0.0"));
            Console.WriteLine("      max squares das (days) =   {0}          max lai das (days)      =    {1}", isqzx.ToString("##0"), ilaiz.ToString("####0"));
            Console.WriteLine("      maximum squares/m2     =   {0}        maximum lai (m2/m2)     =      {1}", sqzx.ToString("##0.0"), alaiz.ToString("#0.00"));
            Console.WriteLine("      Total above ground biomass (kg/ha) =  {0}", wt.ToString("#####0.0"));
            Console.WriteLine("\n");

            return; // ozcot_harvest_report
        }


        //     ===========================================================
        public void ozcot_harvest_update()
		{
            //     ===========================================================

            //+  purpose
            //       remove open bolls from the system
            //       dry matter of open bolls (seed cotton including lint) removed.

            //+  changes
            //     

            //+  constant values

            //+  local variables
            int newestOpenBoll;  // marker for youngest (uppermost) openBoll cohort
            int cohort;



            //*- Implementation Section ----------------------------------

            //   Zero variables 
            //   (only zero variables that are removed in harvest)
            //   Need to go through fruit cohort arrays and zero all open boll counts and weights
            //   and zero dry matter arrays for open bolls

            newestOpenBoll = lfru[open_bolls];		        // 1st active element in fruit array, oldest immature fruit
            if (newestOpenBoll > 0)                         // there are cohorts of open bolls
            {
                for (cohort = 1; cohort <= newestOpenBoll; cohort++)
                {
                    fruno[cohort] = 0.0;
                    fruwt[cohort] = 0.0;
                    harvested = openz;
                    openz = 0.0;
                }
            }

            //   Dry matter of open bolls is 'lost' to the system : Do not pass to BiomassRemoved event
            //   Move senesced meal and lint (open boll components) to harvested array
            dm_allBolls = dm_allBolls - dm_openBolls;
            dm_openBolls = 0.0;
            dm_harvested[meal] = dm_senesced[meal];
            dm_harvested[lint] = dm_senesced[lint];
            dm_senesced[meal] = 0.0;
            dm_senesced[lint] = 0.0;
           

            // Publish Event 'Harvested'  Cotton has been harvested
            sendPublishEvent(evtHarvested, false);


           // Console.WriteLine("      Total assimilate (g/m2) =  {0}", total_assimilate.ToString("#####0.0"));
            Console.WriteLine("      Total green biomass (g/m2) =  {0}", dm_green.Sum().ToString("#####0.0"));
            Console.WriteLine("      Total sensced biomass (g/m2) =  {0}", dm_senesced.Sum().ToString("#####0.0"));
            Console.WriteLine("      Total shed biomass (g/m2) =  {0}", dm_shed.Sum().ToString("#####0.0"));
            Console.WriteLine("      (Total fruit shed biomass (g/m2) =  {0})", dm_fruShedTot.ToString("#####0.0"));
            Console.WriteLine("      Total harvested biomass (g/m2) =  {0}", dm_harvested.Sum().ToString("#####0.0"));
            Console.WriteLine("\n");



            return; // ozcot_harvest_update
		}


        //     ===============================================================================
        public void ozcot_crop_root_length_distrib(ref double[] root_array, double root_sum)
        {
            //     ===========================================================================

            //*+  Sub-Program Arguments
            //      real       root_array(*)         ! (OUTPUT) root length array to contain distributed material
            //      real       root_sum              ! (INPUT) Material to be distributed

            //*+  Purpose
            //*       Distribute root material over profile

            //*+  Changes
            //*       290994 jngh specified and programmed (APSRU)

            //*+  Constant Values
            //        c_root_extinction = 3.0
            //      

            //*+  Local Variables

            double cum_depth;                               // cumulative depth (mm)
            int layer;                                      // layer number ()
            int deepest_layer;                              // deepest layer in which the roots are growing
            double rootsInLayer;                            // temporary value to save to array entry

            double[] root_distrb = new double[max_layers];  // root distribution ()
            double root_distrb_sum;                         // sum of root distribution array

            const double c_root_extinction = 3.0;


            //*- Implementation Section ----------------------------------

            Array.Clear(root_array, 0, max_layers);
            Array.Clear(root_distrb, 0, max_layers);

            deepest_layer = mvDataFunctions.find_layer_no(rtdep, dlayr, max_layers);
            cum_depth = 0.0;
            for (layer = 1; layer <= deepest_layer; layer++)
            {
                cum_depth = cum_depth + dlayr[layer];
                cum_depth = Math.Min(cum_depth, rtdep);  
                root_distrb[layer] = Math.Exp(-c_root_extinction * mvDataFunctions.divide(cum_depth, rtdep, 0.0));
            }

            root_distrb_sum = mvDataFunctions.SumArray(root_distrb, deepest_layer);

            for (layer = 1; layer <= deepest_layer; layer++)
            {
                rootsInLayer = 0.0;
                rootsInLayer = root_sum * mvDataFunctions.divide(root_distrb[layer], root_distrb_sum, 0.0);
                root_array[layer] = Math.Max(rootsInLayer, 0.0);
            }


            return; // ozcot_crop_root_length_distrib
        }



        //     ===========================================================
        public void ozcot_crop_root_incorp(double dlt_dm_root,
                                           double dlt_N_root,
                                           double[] dlayer, 
                                           double[] root_length, 
                                           double root_depth,
                                           ref double[] dlt_dm_incorp,
                                           ref double[] dlt_N_incorp)
		{
		//     ===========================================================
        //!+  Sub-Program Arguments
        //      real       dlt_dm_root           ! (INPUT) new root residue dm (kg/ha)
        //      real       dlt_N_root            ! (INPUT) new root residue N (kg/ha)
        //      real       dlayer(*)             ! (INPUT) layer thicknesses (mm)
        //      real       root_length(*)        ! (INPUT) layered root length (mm)
        //      real       root_depth            ! (INPUT) root depth (mm)
        //      real       dlt_dm_incorp         ! (OUT) new root residue dm to be incorporated (kg/ha)
        //      real       dlt_N_incorp          ! (OUT) new root residue N to be incorporated (kg/ha)
        //
        //!+  Purpose
        //!     Pass root material to the soil modules (based on root length distribution)

        //!+  Mission Statement

        //!+  Changes
        //!     <insert here>
        //!      280800 jngh changed literal incorp_fom to ACTION_incorp_fom

        //!+  Constant Values
        //

        //!+  Local Variables


        //!- Implementation Section ----------------------------------

                if (dlt_dm_root > 0.0) 
                {   // send out root residue
                    ozcot_crop_root_content_dist(root_length, root_depth, dlt_dm_root, ref dlt_dm_incorp);

                        //call bound_check_real_array          &      //check(value, lowerlimit, upperlimit, 'name of var', array_size)
                        //    (          &
                        //        dlt_dm_incorp          &
                        //    ,0.0          &
                        //    ,dlt_dm_root * gm2kg/sm2ha          &
                        //    ,'dlt_dm_incorp'          &
                        //    ,max_layer          &
                        //    )
                    

                    ozcot_crop_root_content_dist(root_length, root_depth, dlt_N_root, ref dlt_N_incorp);

                        //call bound_check_real_array          &     //check(value, lowerlimit, upperlimit, 'name of var', array_size)
                        //    (          &
                        //    dlt_n_incorp          &
                        //    ,0.0          &
                        //    ,dlt_n_root * gm2kg/sm2ha          &
                        //    ,'dlt_n_incorp'          &
                        //    ,max_layer          &
                        //    )

                }
                else
                {
                    // no roots to incorporate
                }

			   
            return; // ozcot_crop_root_incorp
		}


        //     ===========================================================
        public void ozcot_crop_root_content_dist (double [] root_length, double root_depth, double root_sum, ref double [] root_array)
		{
		//     ===========================================================

        //  Sub-Program Arguments
        //      REAL       root_length(*)        ! (INPUT)
        //      REAL       root_depth            ! (INPUT)  depth of roots (mm)
        //      real       root_sum              ! (INPUT) Material to be distributed
        //      real       root_array(*)         ! (OUTPUT) array to contain distributed material

        //  Purpose
        //       Distribute root material over profile based upon root
        //       length distribution.

        //  Mission Statement
        //   Distribute %5 over the profile according to root distribution

        //  Changes
        //*    apsim specified and programmed
        //     <insert here>

        //  Constant Values
        //      

        //  Local Variables
            int     layer;             // layer number
            int     deepest_layer;     // deepest layer in which the roots are growing
            double  root_length_sum;   // sum of root distribution array
            double rootsInLayer;       // temporary value to save to array entry

        //- Implementation Section ----------------------------------

        // Distribute roots over profile to root_depth

            Array.Clear(root_array, 0, max_layers);

            deepest_layer = mvDataFunctions.find_layer_no(rtdep, dlayr, max_layers);

            root_length_sum = mvDataFunctions.SumArray(root_length, deepest_layer);

            for (layer = 1 ; layer <= deepest_layer; layer++)
            {
                rootsInLayer = 0.0;
                rootsInLayer = root_sum * mvDataFunctions.divide(root_length[layer], root_length_sum, 0.0);
                //ensure no negative values
                root_array[layer] = Math.Max(rootsInLayer, 0.0);
            }


            return; // ozcot_crop_root_content_dist
		}


        //     ===========================================================
        public void ozcot_update()
        {
            //     ===========================================================
            //
            //  Purpose
            //       Report the current status of specific
            //       variables.
            //
            //  Changes
            //     051101 jngh specified and programmed
            //
            //     ===========================================================
            //
            //  Constant Values
            //
            //  Local Variables
            //      real    res_dm                  ! Residue dry weight (kg/ha)
            //      real    res_N                   ! Amount of N in residue (kg/ha)
            //      real    fraction_to_Residue(max_part)   ! fraction sent to residue (0-1)
            //      real    dlt_dm_crop(max_part)           ! change in dry matter of crop (kg/ha)
            //      real    dlt_dm_N(max_part)              ! change in N content of dry matter (kg/ha)
            //
            //     ===========================================================


            // Implementation Section ----------------------------------


            double[] fraction_to_residue = new double[max_part + 1];    // fraction sent to residue (0-1) as double
            double[] dlt_dm_crop = new double[max_part + 1];            // change in dry matter of crop (kg/ha) as double
            double[] dlt_dm_n = new double[max_part + 1];               // change in n content of dry matter (kg/ha) as double

            // Update biomass and N pools.  Different types of plant pools are affected differently.
            // =====================================================================================

            //   Roots not removed in BiomassRemoved event
            //      dlt_dm_crop(root) = g%dm_root * gm2kg/sm2ha                
            //      dlt_dm_N(root) =  dlt_dm_crop(root) * 0.4 / 100.0
            //      fraction_to_Residue(root) = 1.0
            //
            //  Seed cotton only removed on Harvest event
            //      dlt_dm_crop(meal) = g%openwt * gm2kg/sm2ha
            //      dlt_dm_N(meal) =  dlt_dm_crop(meal) * 0.4 / 100.0
            //      fraction_to_Residue(meal) = 0.0
            //
            //  Stem not removed during season, only at end_crop
            //      dlt_dm_crop(stem) = g%dm_stem * gm2kg/sm2ha
            //      dlt_dm_N(stem) = dlt_dm_crop(stem) * 0.4 / 100.0
            //      fraction_to_Residue(stem) = 1.0

            // currently leaf detachment is not simulated, so senesced leaf is detached immediately (see ozcot_laigen() )
            dm_senesced[leaf] -= leaf_res;             // (g/m2)
            dm_shed[leaf] += leaf_res;
            dlt_dm_crop[leaf] = Math.Max((leaf_res * gm2kg / sm2ha), 0.0);  //BiomassRemoved event uses (kg/ha)
            dlt_dm_n[leaf] = Math.Max((leaf_res_n_conc * gm2kg / sm2ha), 0.0);
            fraction_to_residue[leaf] = 1.0;

            //     Shed fruit/squares need to be added here 
            dm_green[pod] -= dm_fruShedToday;                       // remove fruit shed from green dm pool  (g/m2)
            dm_shed[pod] += dm_fruShedToday;                        // add fruit shed to shed dm pool
            dlt_dm_crop[pod] = Math.Max((dm_fruShedToday * gm2kg / sm2ha),0.0);
            dlt_dm_n[pod] = Math.Max((dm_fruShedToday * pod_res_n_conc * gm2kg / sm2ha),0.0);    // N content of green bolls being shed
            fraction_to_residue[pod] = 1.0;

            // Publish a 'BiomassRemoved' event  
            if (dlt_dm_crop.Sum() > 0.0)
            {
                // need to set up the arrays of values for this event
                TTypedValue thisEvent = eventList[evtBiomassRemoved];
                thisEvent.member("crop_type").setValue(crop_type);
                thisEvent.member("dm_type").setValue(part_name);
                thisEvent.member("dlt_crop_dm").setValue(dlt_dm_crop);
                thisEvent.member("dlt_dm_n").setValue(dlt_dm_n);
                // thisEvent.member("dlt_dm_p").setValue(part_name);    //phospherous not modelled in OZCOT
                thisEvent.member("fraction_to_residue").setValue(fraction_to_residue);
                eventList[evtBiomassRemoved] = thisEvent;

                sendPublishEvent(evtBiomassRemoved, false);

            }
            else
            {
                // no surface residue
            }



            ddm_boll = 0.0;
            ddm_leaf = 0.0;
            ddm_root = 0.0;
            ddm_stem = 0.0;


            return; // ozcot_update
        }



        // ====================================================================


        //cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
        //                                                                cc
        //                                                                cc
        //                       program ozcot                            cc
        //                                                                cc
        //                          27/5/83                               cc
        //                                                                cc
        //                                                                cc
        //cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
        //cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
        //                                                                  c
        //     begun during a visit to  t.a.e.s. blackland research center  c
        //     temple, texas 1981 by a.b.hearn.                             c
        //                                                                  c
        //     developed at nars for namoi valley cotton  1982 to 1988      c
        //     by hearn and da roza.                                        c
        //         components:                                              c
        //           water balance   - ritchie model taken largely from     c
        //                             cornf(stapper & arkin)               c
        //           nitrogen model -  developed at nars by hearn & da roza c
        //           fruit submodel  - taken from siratac by hearn          c
        //                                                                  c
        //     ozcot1 - version for retrospective simulation of specific    c
        //              crops; runs for one season only, irrigation dates   c
        //              given.                                              c
        //     ozcot2 - version for predictive simulation  through many     c
        //              seasons; irrigation dates predicted.                c
        //     ozcot3 - version for optimising irrigation                   c
        //     ozcot4 - version for calibration with minos5                 c
        //     ozcot5 - version for physiological analyis                   c
        //                                                                  c
        //                                                                  c
        //cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
        //
        //     structure
        //       ozcot2  init
        //               cinput2
        //               newdate
        //               metdat2 hhunc
        //                       evap    fn satvp
        //               decide_irg
        //               solwat  sevap   fn watco
        //                       swbal
        //               sowday
        //               emerg
        //               snbal   n_fertilise
        //               pltgrw  actlai
        //                       laigen  fn senlf, fn stress
        //                       cropn
        //                       istsq
        //                       fruit   bollwt  fn stress
        //                               carrying_capacity fn stress
        //                               overload
        //                               actfru
        //                               update
        //                               fn frugen       fn stress
        //                               fn survive
        //               harvest
        //               dayout2
        //               (day_dudley    option for norm dudley)
        //               yield
        //               reset
        //
        //       note modules ozcot2, cinput2, metdat2, dayout2, decide_irg
        //                    newdate, reset are specific to ozcot2.
        //
        //       ozcot.inc common blocks
        //
        //       input files: met.inp, soilw.inp, agron.inp - 1
        //       output files: fruit.out, yield.out, calib.out - units = 2,3
        //
        // link/exe=ozcot2 ozcot2,init,cinput2,metdat2,hfunc,evap,satvp, -
        //                 decide_irg,solwat,sevap,watco,swbal, -
        //                 sowday,emerg,snbal,n_fertilise, -
        //                 pltgrw,actlai,laigen,senlf,stress,cropn,istsq, -
        //                 fruit,bollwt,carrying_capacity,overload,actfru, -
        //                 update,frugen,survive, -
        //                 harvest,dayout2,yield,reset
        //
        //
        //                 ozcot2 - calling program
        //                 ---------------------------
        //
        //
        //      program ozcot2
        // ====================================================================
        public void ozcot2()
        {
            // ====================================================================

            //- implementation section ----------------------------------

            
            if ((crop_in))
            {
                ozcot_metdat2();
                ozcot_solwat();
            }
            //             if(defirr(2).ne.0.) call decide_irg       ! irrigated crop?
            //             if(isow.le.0)then           ! crop sown yet?
            //                  call sowday (i,iend)    ! sow tomorrow?
            //                  call sowday             ! sow tomorrow?
            //                  if(iend.ge.3) go to 32  ! passed sowing window or fallow
            //              elseif(i.gt.isow .and. iemerle.0)then     ! crop emerged yet?
            //jh v2001              if(iemrle.0) then
            //                  call emerg (i)          ! emerge today?
            //jh v2001                  call ozcot_emerg              ! emerge today?
            //jh v2001              endif
            else
            {
            }
            //              call snbal(i)               ! soil n balance
            ozcot_snbal();
            // soil n balance
            //              if(isow.gt.0 .and. i.gt.isow) call pltgrw (i,iend,nszn)
            //              if(openz.gt.0.0) call harvest(iend)
            //      print*, crop_in, DAS, isow, openz, iend

            if (crop_in & (plant_status == status_alive))
            {
                if ((isow > 0) & (DAS > 0))
                {
                    ozcot_pltgrw();   // grow the crop UNLESS crop is havest mature; then no further development should happen, only accumulation of rain
 
                    ozcot_yield();                   // calculate yield

                }

                else
                {
                }
            }
            return; 
        }




        // ====================================================================
        //      subroutine pltgrw (i,iend,nszn)
        public void ozcot_pltgrw()
		{
			// ====================================================================

			//-------------------------------------------------------------------
			//      calls the various plant growing routines.  at this point    !
			//      there are also some variable conversions to allow merging   !
			//      of the independently derived soil and plant portions of the !
			//      model.                                                      !
			//-------------------------------------------------------------------

			int j;
			double rtdep1, rtdep2;
			double lbollgr;


			//- implementation section ----------------------------------

            ozcot_update();

			iday = DAS;

			if ((iday == (max_cohort - 1)) & (cropStage < mature))
			{
                // error trap: Maturity trigger not met. Force maturity stage
                iend = 10;
                cropStage = mature;
                maturityDAS = DAS;
                Console.WriteLine("{0} {1} {2} (Day of year={3}), cotton: ", Today.Day, Today.ToString("MMMM"), Today.Year, DOY);
                Console.WriteLine("   *** season at 300 days:  crop stage forced to 'maturity'.");
                return; 
			}

			//---- crop development complete? ---------------------------------------------
			//   error trap: Open bolls with nothing more growing ==> force stage to maturity
			if ((openz > 0.0)  &  (bollz * effectiveRowSpacing < 1.0) )
			{
                if ((iend == 0) & (cropStage < mature))
                {
                    iend = 6;
                    cropStage = mature;
                    maturityDAS = DAS;
                    Console.WriteLine("{0} {1} {2} (Day of year={3}), cotton: ", Today.Day, Today.ToString("MMMM"), Today.Year, DOY);
                    Console.WriteLine("   *** crop has open bolls but no more green bolls:  crop stage forced to 'maturity'.");
                }
            }

            dd = hunits;
			rad = solrad;
			//pclint = percent_l;	// set lint percent for variety
            pcLint_frac = percent_l / 100.0;  // set lint fraction for variety

			if (iday > 1)
			{
				for (j = 1; j <= iday; j++) {
					fyzage[j] = fyzage[j] + dd;		// physiological aging
					continue;
				}
			}
			else
			{
			}
			sumdd = sumdd + dd;


            // if crop has been defoliated then accumulate the DD's and count the days
            //     ready to trigger harvest_ripe
            if (defoliationCnt > 0)
            {
                defoliationDayCnt += 1;
                defoliationDDSum += dd;
            }


            // if crop is harvest ripe then allow leaf scenescence, and weathering of open bolls
            //     but skip all other logic   
            if (cropStage == harvest_ripe)
            {
                ozcot_laigen();
                ozcot_weather_open_bolls();
            }
            else
            {
                //----- increase root depth ----------------------------------------------------

                rtdep1 = sdepth + ((20.0 - sdepth) / 36.0) * (double)iday;          // g da r from 82/83 expt

                // original code 
                // rtdep2 = profileDepth * (1.0 - 2.65 * Math.Exp(-0.03 * (double)iday));	// profileDepth replaced 122.38 12/5/95

                //DBJ modified 30/5/2013 for Steve Yeates
                //rtdep2 = profileDepth * (1.0 - 2.65 * Math.Exp(-0.03 * (double)(iday + 35)  ));	

                //DBJ modified 21/3/2014 as enhancement
                // original code tuned for ACRI.  
                // Phase I from rtdep was slow expansion and lasted for about 37 days, then Phase II expansion from rtdep2
                // Externalising 'rootExpansionDelay' allows flexibility in delay period. Yeates found short delays before rapid root expansion in Burdekin soils.
                // This was noted as water stressed seedlings in simulation, but no stress in reality. Root development was too slow in model.
                // This may be due to different climate, soil type or an interaction of both. 
                // Adjustment means that default delay period is actually 33 days for ACRI - translates to model delay of about 37 days as per original response.
                // A delay of 0 days causes rtdep values to be skipped and rtdep2 values to be used from day 1 - rapid root expansion.
                // Rooting depth limited to rtdep_max as a max (profile depth).  (rtdep_max is init variable)
                //
                //rtdep2 = profileDepth * (1.0 - 2.65 * Math.Exp(-0.03 * (double)(iday + 33 - 33)));
                //rtdep2 = profileDepth * (1.0 - 2.65 * Math.Exp(-0.03 * (double)(iday + 33 - rootExpansionDelay)));
                rtdep2 = profileDepth * (1.0 - 2.65 * Math.Exp(-0.03 * (double)(DAS + rootExpansionDelay)));

                rtdep = Math.Max(rtdep1, rtdep2);

                if ((rtdep > rtdep_max)) rtdep = rtdep_max;


                //---- check if frost terminates crop -----------------------------------------
                //
                // TODO: DBJ Review frost rules
                //       Frost is killing and terminating a mature crop --> NO YIELD, which is in error ??
                //       (early frost might kill a crop if severe enough, otherwise it should mature bolls and 
                //        reduce functioning leaves  ???  but leave the crop to be harvested)
                //        iend is used in ozcot_harvest() and also reported as ozcot-status and status
                //        thus terminating the crop when manager script is looking for status > 0.
                //
                if (tempmn <= frost_kill_immediate & (iemrg > 0.0))   // frost after emergence?
                {
                    if ((iend == 0) & (bollz == 0) & (cropStage < firstFlower))           // pre-fruiting?   green bolls yet?
                    {
                        iend = 3;               // flag for frost killing young crop 
                                                //write#1,//format(' *** crop killed by frost before fruiting')
                        Console.WriteLine("{0} {1} {2} (Day of year={3}), cotton: ", Today.Day, Today.ToString("MMMM"), Today.Year, DOY);
                        Console.WriteLine("   *** crop killed by frost before fruiting.");
                    }
                    else if ((iend == 0) & (openz == 0.0) & (cropStage < firstOpenBoll))    // open bolls yet?   None - immature crop
                    {
                        iend = 4;               // flag for frost on immature crop - try to force open bolls > 80. mature
                                                //write#1,//format(' *** crop killed by frost during fruiting, before open bolls')
                                                //                                    ??  should this kill the crop?  or some other stress effect?
                        Console.WriteLine("{0} {1} {2} (Day of year={3}), cotton: ", Today.Day, Today.ToString("MMMM"), Today.Year, DOY);
                        Console.WriteLine("   *** crop encounter frost during fruiting, before open bolls.");
                    }
                    else
                    {
                        if ((iend == 0) & (cropStage < mature))
                        {
                            iend = 5;              // flag for frost on older crop - force open bolls > 80. mature
                            cropStage = mature;
                            maturityDAS = DAS;
                            Console.WriteLine("{0} {1} {2} (Day of year={3}), cotton: ", Today.Day, Today.ToString("MMMM"), Today.Year, DOY);
                            Console.WriteLine("   *** crop with open bolls frosted:  crop stage forced to 'maturity'.");
                        }
                    }
                }






                //---- if hail damage do here   ----------------------------------------------


                //----- emergence ------------------------------------

                if (iemrg <= 0) ozcot_emerg();          // crop emerged yet? emerge today?
                if (DAS == iemrg) ddmerg = sumdd - dd;

                //----- increase leaf area -----------------------------------------------------
                ozcot_laigen();

                //----- crop nitrogen ---------------------------------------------------------
                ozcot_cropn();

                //----- test for a 'dead' crop -----------------------------------
                if (iemrg > 0)
                {
                    if (laiField < 0.1  &  smi < 0.2  &  defol1DAS == 0)
                    {
                        deadFlagCnt ++ ;
                        if (deadFlagCnt > 5) plant_status = status_dead;
                        Console.WriteLine("{0} {1} {2} (Day of year={3}), cotton: ", Today.Day, Today.ToString("MMMM"), Today.Year, DOY);
                        Console.WriteLine("         Crop failed - status 'dead'.");
                    }
                    else
                    {
                        deadFlagCnt = 0;
                    }
                }

                //---- grow plant -------------------------------------------------------------

                if (isq == 0)
                {
                    ozcot_istsq();          // call istsq (i,nszn)
                }
                else
                {
                    if ((DAS > isq)) ozcot_fruit();   // fruiting has started
                }

                lbollgr = bollgr;
                ozcot_dryxmatter();
                ozcot_plant_n();
                bollgr = lbollgr;
                ozcot_plant_height();

                //------ following are for use in s/r yield -----------------------------------

                if (laiField > alaiz)
                {
                    alaiz = laiField;               // max lai
                    ilaiz = iday;               // day of max lai
                    plntnz = plantn;            // plantn on day of max lai
                    iplntn = ilaiz;
                }

                if (squarz > sqzx)
                {
                    sqzx = squarz;              // peak square numbers
                    isqzx = iday;               // day of peak squares
                }




                //---- has the crop reached 'maturity' 
                //       
                //     set day of maturity if it has not been set and we have reached the % of open bolls
                //    ----------------------------------------------------------------------------------
                ozcot_check_maturity();



                //----------- final test to see if the crop has been defoliated and is ready for harvest
                if ((defoliationCnt > 0) & (laiField < 0.1) & (cropStage < harvest_ripe))
                {
                    iend = 6;               // crop finished - from this point on NO further fruit development should happen.
                    cropStage = harvest_ripe;

                    Console.WriteLine("{0} {1} {2} (Day of year={3}), cotton: ", Today.Day, Today.ToString("MMMM"), Today.Year, DOY);
                    Console.WriteLine("         Crop ready for harvest.");
                }

            }  // end test on harvest maturity

            return; 
		}


        // ====================================================================
        //      subroutine bollwt(idayx,l)
        public void ozcot_bollwt()
        {
            // ====================================================================

            //     calculates nominal (actual)(bgrvar) and potential (bollgr) boll growth rates.
            //     bollgrowth rate is driven by dd,limited by water,
            //     n and c(incl water effects on photosynthesis) stress



            //------stuff done on 1st call of the day - stresses & growth rate -------------
            //     !  functions


            //     ! local variables
            double fbcstr;
            double fbwstr;
            double fbnstr;
            double strsbl;
            double strsbl_1;
            double f_temp;

            //- implementation section ----------------------------------

            if (idayx == 0 | iday == idayx + 1) // 1st call for this day?
            {
                idayx = iday;                // if so, set flag to exit sub more calls
                fbcstr = 1.0;                // cc stress factor to reduce bollgr
                if ((bload > 0.0)) fbcstr = carcap_c / bload;     // supply/demand ratio for when
                if ((fbcstr > 1.0)) fbcstr = 1.0;                 // boll growth limited by cc supply
                fbwstr = ozcot_stress(fbwstr_low, fbwstr_high, fbwstr_a, smi);
                // water stress on bolls         'const  sw_stress_boll_min, sw_stress_boll_max, sw_stress_boll_pwr
                //jh v2001 deleted        fbwstr = 1.                          ! try no direct stress - 24/4/92
                if ((bollz + openz < carcap_n))    // final boll req < uptake
                {
                    fbnstr = 1.0;                  // do not apply n stress
                }
                else                               // final boll req < uptake
                {
                    fbnstr = ozcot_stress(fbnstr_low, fbnstr_high, fbnstr_a, fnstrs);   // apply n stress       'const  n_stress_boll_min, n_stress_boll_max, n_stress_boll_pwr
                }
                strsbl_1 = Math.Min(fbcstr, fbwstr);      // minimum of cc & water stress
                strsbl = Math.Min(strsbl_1, fbnstr);      // minimum of min(cc, water) & n stress
                //        strsbl = 1.0  ! debug

                //------- efect of temperature on final boll weight ----------------------------

                if ((tempav < 20.0))                  // const   x_temp_boll   15 20 30 35
                {
                    f_temp = -3.0 + 0.2 * tempav;     // temperature scaling factor         'const   y_stress_boll  0  1  1  0
                }
                else if ((tempav > 30.0))             // for boll weight                    'const
                {
                    f_temp = 7.0 - 0.2 * tempav;      // derived from hesketh and low 1968   'const
                }
                else
                {
                    f_temp = 1.0;        //  equiv to x temp 15 20 30 35                                    'const
                }                        //           y fac   0  1  1  0
                
                if ((f_temp < 0.0)) f_temp = 0.0;
                if ((f_temp > 1.0)) f_temp = 1.0;

                bgrvar = scboll * f_temp * bper;       // reference (nominal) boll growth rate this day
                bollgr = bgrvar * 1.3;                 // potential unstressed rate - ex constable            'const     bollgr_pot_fac
                bollgr = bollgr * strsbl;              // todays actual rate per boll
                if (bollgr < 0.0) bollgr = 0.0;

            }

            return; 
        }


        // ====================================================================
        //      subroutine carrying_capacity(i)
        public void ozcot_carrying_capacity()
        {
            // ====================================================================

            //     estimates carrying capacity of crop on basis of photosynthesis.
            //     selects parameter for variety. adjusted for water stress.
            //     carcap is carrying capacity, maximum number of bolls the crop
            //     can carry, therefore the boll load that causes 100. sheddin


            double alight;
            double radn_watts;
            double p_gossym;
            double pot_pn;
            double pn;
            double rel_p;
            double templf;
            double rfac;
            double rm;


            //      data from "siratac" - 1987-88  hearn (pers. comm.)


            //- implementation section ----------------------------------

            //psc      if(i.eq. isq+1) then
            if (DAS == isq + 1)
            {
                istress = 0;                // reset stress flag
                ireliefco = 0;              // reset stress relief counter
            }

            //psc      if(bload.gt.cutout .and. smi.lt.0.75) istress = 1 ! set stress cutout flag

            //psc      if(smi.gt.0.75 .and. istress.gt.0) then

            if (bload > cutout & smi < smi_affect_wlog) istress = 1;    // 0.25 replaced 0.75 - abh 5/11/96    'const  wlog_relief_smi
            if (smi > smi_affect_wlog & istress > 0)                    // 0.25 replaced 0.75 - abh 5/11/96
            {
                ireliefco = ireliefco + 1;                // count days since relief of stress
                if (ireliefco == days_relief_wlog)        // const        wlog_relief_days
                {
                    istress = 0;                    // end stress effect on waterlogging
                    ireliefco = 0;                  // reset counter
                }
            }
            //      istress = 0  !debug
            //----photosynthetic capacity --------------------------------------------------
            //-----light interception modified to give hedgerow effect with skip row - abh 5/11/96 ------

            //sv- 2015_04_24  We are leaving this different caluculation for light interception as is.
            //    strictly speaking the future we should replace it with  ozcot_fract_interception()

            laiRow = laiField;
            if (skiprow > 0) laiRow = laiField * effectiveRowSpacing;                     // lai in hedgerow
            alight = (1.0 - Math.Exp(-ozcot_kvalue * laiRow));     // original code  - now gives interception in hedgerow

            //**********************************  dbj test 22/5/15
           // test_alight = alight;
            //**********************************  dbj test 22/5/15
                                                                       
            if (skiprow > 0)
            {
                alight = alight / effectiveRowSpacing;                // interception on ground area basis
                //jh         laiField = laiField/effectiveRowSpacing         ! restore lai to ground area basis
            }

            radn_watts = solrad * 0.8942;            // convert radn from ly to watts m^2                'const
            p_gossym = 2.391 + radn_watts * (1.374 - radn_watts * 0.0005414);    // gossym line1275      'const
            pot_pn = p_gossym * 0.068;                    // potential photosynthesis g/m2 ch2o          'const
            // pn = pot_pn * alight;                      // net photosynthesis term
            pn = pot_pn * alight * ozcot_co2FertFX();     // net photosynthesis term with co2 term


            //----- effect of water stress on photosysthesis -------------------------------
            //jh v2001
            rel_p = 1.0;
            //jh v2001
            if (smi < relp_smi_crit)           //const    sw_stress_pn_crit
            {
                rel_p = relp_intercept + relp_slope * smi;   // effect of water stress on Pp              'const    sw_stress_pn_smi_intc, sw_stress_smi_pn_slope
                //   rel_p =relp_intercept+relp_slope*0.86  !debug            ! effect of water stress on Pp              !const    sw_stress_pn_smi_intc, sw_stress_smi_pn_slope
                if (rel_p > 1.0) rel_p = 1.0;                // (turner et al 1986).                    'const
                rel_p = rel_p - relp_intercept;              //const
                rel_p = ozcot_stress(relp_low, relp_high, relp_a, rel_p);      // increase severity of stress         'const   sw_stress_relp_min, sw_stress_relp_max, sw_stress_relp_pwr,
                rel_p = rel_p + relp_intercept;              //const
                pn = pn * rel_p;                             // photosynthesis adjusted for water stress

            }

            //----- waterlogging effect additional to n uptake - hearn & constable 1984 eqn 4

            //jh v2001 deleted !c      if(istress.gt.0) then
            //jh v2001 deleted !c        if(sw/ul.gt.0.87) pn = pn * 0.1 ! carrying capacity reduced
            //jh v2001 deleted !c      else
            //jh v2001 deleted !c        if(sw/ul.gt.0.87) pn = pn * 0.1 ! carrying capacity reduced
            //jh v2001 deleted !c      endif

            //---- maintenance respiration -------------------------------------------------

            templf = tempav + 5.0 - 10.0 * smi;               // leaf temperature                             'const
            if (templf < tempav) templf = tempav;
            rfac = Math.Pow(2.2, (templf - 35.0) / 10.0);     // resp temp factor, horie(constable)           'const
            rm = sites * respcon * rfac;                      // maintenance respiration term

            //----- carrying capacity carbon - photosynthesis divided by boll growth rate

            if (bgrvar > 0.0) carcap_c = (pn - rm) / (bgrvar * fburr);  // carrying capacity, carbon, no stress
            if (carcap_c < 0.0) carcap_c = 0.0;                         // trap  TODO:  review notes on possible error

            //jh v2001
            //jh need effectiveRowSpacing correction
            if (carcap_c > 500.0) carcap_c = 500.0;           // limit carrying capacity when low temp gives low bgr (boll growth rate)
            //jh v2001

            //----- waterlogging effect additional to n uptake - hearn & constable 1984 eqn 4
            //jh      print*, 'DAS, caracp_c, wli, rm, istress, bload, cutout,smi'
            //jh      print*,DAS,carcap_c,wli,rm,istress,bload,cutout,smi
            if (istress > 0)
            {
                //jh v2001 deleted         if(sw/ul.gt.watlog_c)
                if (wli > watlog_c) carcap_c = carcap_c * wlog_carcap_red_stress;  // carrying capacity reduced 
            }
            else
            {
                //jh v2001 deleted         if(sw/ul.gt.watlog_c)
                if (wli > watlog_c) carcap_c = carcap_c * wlog_carcap_red;        // carrying capacity reduced 
            }
            cutout = carcap_c * fcutout;              // boll load for cutout, sq prodn exit subs
            

            return; 
        }



        // ====================================================================
        //      subroutine cropn(i)
        public void ozcot_cropn()
        {
            // ====================================================================

            double harvest_n;
            double bgr;
            double up_kg_ha;
            double veg_n;

            //     assumes all n available for season (availn) from snbal
            //     is taken up (uptakn).
            //     computes: n harvested on basis of constable and rochester 1988
            //               n in fruit frun
            //               carrying capacity (carcap_n), number of bolls for which
            //                       harvest_n is adequate
            //                       used in frugen and survive for squares
            //               stresses:
            //                       vegetative (vnstrs for laigen) = f(1-harvest_n/uptakn)
            //                               ie function of proportion of n remaining in veg
            //                         fruit (fnstrs for bollwt) 1- frun/harvest_n
            //                               ie function of n to be harvested not in fruit


            //- implementation section ----------------------------------

            //----- potential uptake for season --------------------------------------------
            //jh v2001 changed      uptakn = availn   ! potential uptake for season based on available n
            uptakn = availn / 10.0;            // potential uptake for season based on available n

            //-----  compute potential n harvested ----------------------------------------

            harvest_n = uptakn * harvest_n_frac;            // harvestable n; 0.85 from constable & rochester       'const
            //jh v2001 deleted      harvest_n = harvest_n/10.      !  kg/ha to g/m**2, n limiting fruit.               !const

            //----- compute n already in fruit -------------------------------------------
            //pdev this is generating an underflow warning:
            //pdev ---------------------------------vvvvvvvvvv
            //jh v2001
            up_kg_ha = uptakn * 10.0;            // need kg/ha for next calculation
            seed_nc = 0.02407 + 0.000147 * up_kg_ha - 0.00000034 * Math.Pow(up_kg_ha, 2.0);   // gac dat 3/6/88         'const
            //jh v2001
            frun = (frudw + dm_openBolls) * ((1.0 - pcLint_frac) * seed_nc + (fburr - 1.0) * 0.005);    // n in fruit           'const

            //----- compute n carrying capacity --------------------------------------------

            bgr = scboll;            // seed coton per boll
            carcap_n = harvest_n / (bgr * 0.6 * 0.03);         // bolls per m     'const

            //----- compute n stresses -----------------------------------------------------

            //jh v2001 deleted      vnstrs = (uptakn-10.)/uptakn ! vegetative stress 10=uptake @ 1st boll          !const
            //jh v2001

            if ((bollz == 0.0))        // during veg growth, before 1st boll
            {
                veg_n = uptakn;        // n in veg tissues before 1st boll
            }
            else                       // during reproductive growth
            {
                veg_n = uptakn - frun;  // n in veg tissues after 1st boll
            }

            if ((veg_n > 0.0))
            {
                vnstrs = (veg_n - 1.0) / veg_n;  // vegetative stress, 10=uptake @ 1st boll
            }
            else
            {
                vnstrs = 0.0;
            }
            //jh v2001
            if ((bollz == 0.0))           // during veg growth, before 1st boll
            {
                fnstrs = 1.0;
            }
            else
            {
                //jh v2001
                if ((harvest_n >= frun))
                {
                    fnstrs = 1.0 - frun / harvest_n;  // fraction of harvestable not in fruit
                }
                else
                {
                    fnstrs = 0.0;
                }
                //jh v2001
            }

            if ((vnstrs > 1.0)) vnstrs = 1.0;
            if ((vnstrs < 0.0)) vnstrs = 0.0;
            if ((fnstrs > 1.0)) fnstrs = 1.0;
            if ((fnstrs < 0.0)) fnstrs = 0.0;
            //----------------------------------------------------------------------------

            uptakn = uptakn * 10.0;      // g/m2 to kg/ha

            return; 
        }


        // ====================================================================
        //      subroutine emerg (i)
        public void ozcot_emerg()
		{
			// ====================================================================
			//------------------------------------------------------------------------------------------------
			//
			//     simulates  emergence
			//
			//     originally wanjura's complex function was used.
			//     replaced by a simple heat sum (60 dd) as an alternative, probably in 1990.
			//     revised in nov 1996 by abh at apsru in order to deal with adverse conditions.
			//     data from constable 1976, anderson 1971 and wanjura et al 1969 and 1970 were used.
			//     wanjura's 1970 complex function was linearised to give 0.0041cm/hr/deg which gives
			//     0.1 cm per dd12, which when divided into seed depth (sdepth) gives dd requirement = 50
			//     for sowing at 5 cm.  5dd in soil is equivalent to  6dd air in sept/oct,
			//     to give 60 dd for emergence, as previously used.
			//     anderson 's data gave values of dd requirement from 50 to 75,
			//     the variation being associated with tmax.
			//     consequently the dd requirement increases by 2.5 each deg c of mean tmax over 20 deg 
			//     all three papers, and others, show a relationship between days to emerge and . emergence.
			//     constable's data allowed a quantitative relationship to be deleveloped.
			//     constable's data also showed an effect of soil water; when tensiommeter readings were zero,
			//     (ie soil saturated smi>0.87) emergence was reduced by 20., consequently emergence
			//     was reduced by 0.03 per daywhen smi>0.87.
			//     wanjura 1969 contained data relating subsequent survival of seedlings to elapsed
			//     days to emergence.
			//
			//     constable's original data will be reworked to derive relationships directly,
			//     instead of indirectly.
			//
			//------------------------------------------------------------------------------------------------

            double establish;


			//- implementation section ----------------------------------

			if ((iemrg > 0))
			{
				return; 
			}

			//--------------------------------------- ------------------------------------------------------


			if (initial_call == true)      // first call of season
			{
				initial_call = false;      // reset flag for next season
				nday_co = 0;               // counter for days to emergence
				nwet_co = 0;               // counter for wet days to mergence
				sum_tmx = 0.0;
				ave_tx = 0.0;
				delay_emerg = 0.0;
				dd_emerg = (sdepth / (rate_emergence/10.0)) * (6.0 / 5.0);   //sdepth in cm,  rate_emergence converted mm to cm
				                                                             // 0.1 (default rate_emerg) is from linear increase phase of wanjura 1970
				ppm_sown = 0.0;
				ppm_emerge = 0.0;
				ppm_establish = 0.0;
				fail_emrg = 0.0;
				f_die = 0.0;
			}

			//--------------------------------------- ------------------------------------------------------
			nday_co = nday_co + 1;			            // count days to emergence
			if ((smi > 0.87)) nwet_co = nwet_co + 1; 	// count wet days to emergence
			sum_tmx = sum_tmx + tempmx;                 // sum tempmx
			ave_tx = sum_tmx / nday_co;                 // mean tmax during emergence
			if ((ave_tx > 20.0)) delay_emerg = (ave_tx - 20.0) * 2.5; // increase dd requirement - anderson"s data

			if ((sumdd < dd_emerg + delay_emerg)) return;  // does not emerge today

            //------ crop emerges ---------------------------------------------------------------------------

			iemrg = DAS;
            cropStage = preSquaring;  //emerged heading towards first square


            //------ estimate emergence ---------------------------------------------------------------------

            fail_emrg = 0.015 * (double)(nday_co - 7);			// slow emergence; constable 1976
			fail_emrg = fail_emrg + 0.03 * (double)(nwet_co);	// wet conditions; constable 1976
			if ((fail_emrg > 1.0)) fail_emrg = 1.0; 
			if ((fail_emrg < 0.0)) fail_emrg = 0.0; 
			ppm_sown = ppm2_target / 0.8;			       // seeds sown - assume 80. emerge
			ppm_emerge = ppm_sown * (1.0 - fail_emrg);	   // adjust ppm2 for failure to emerge

			//------ estimate establishment   -----------------------------------------------------------

			if ((nday_co > 8))
			{
 				f_die = 0.0567 * 3.0 + 0.1 * (nday_co - 8); // estimate proportion of seedlings
				                                            // that fail to survive as a function
			}                                               // of days to emerge
            else if ((nday_co > 5)) {
				f_die = 0.0567 * (nday_co - 5);
			}
			if ((f_die > 1.0)) f_die = 1.0; 

			ppm_establish = ppm_emerge * (1.0 - f_die);			// adjust ppm2 for seedling death
			ppm2 = Math.Min(ppm2_target, ppm_establish);			// to avoid confusion, ppm2 cannot exceed target

			//jhtemp disable establishment
			ppm2 = ppm2_target;

			establish = ppm2 * 100.0 / ppm2_target;			// percentage established

            // report emergence
            Console.WriteLine("{0} {1} {2} (Day of year={3}), cotton:", Today.Day, Today.ToString("MMMM"), Today.Year, DOY);
            Console.WriteLine("    *** Crop emerged with {0} plants per m sq, {1}% of target population of {2}", ppm2.ToString("#0.0"), establish, ppm2_target.ToString("#0.0"));
            Console.WriteLine("");

			if ((establish < 25.0))
			{
				iend = 2;				// flag to terminate season

                // report failed establishment
                Console.WriteLine("{0} {1} {2} (Day of year={3}), cotton:", Today.Day, Today.ToString("MMMM"), Today.Year, DOY);
                Console.WriteLine("    *** crop failed to establish;  stand was less than 25% of target population of {0} plants per m sq", ppm2_target);
            }
			return; 
		}

        //-------------------------------------------------------------------

        //      previous version jackson/arkin

        //ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
        //      calculates hypocotle elongation leading up to emergence     c
        //      based on the growing-degree-day concept.                    c
        //      the elongation rate of 0.12 cm/gdd is use to a maximum rate c
        //      of 2.04 cm/gdd at about 32                                c
        //      the gdd base temperature is set in the "init" subroutine.   c
        //      elongation rate data are from wanjura et.al. 1970.          c
        //                                                                  c
        //      key variables:                                              c
        //          sdepth = seed sowing depth                              c
        //          stemp = soil temperature (cal in subroutine "metdat") c
        //          iemrg = emergence date                                  c
        //ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

        //       t = stemp                             ! soil temp
        //       if(stemlt.14.44)t = 14.44+(14.44-t) ! to comput lag whenstemp < 14.44
        //       hypoel = 0.0853-0.0057/(41.9-t)*(t-34.44)**2 ! wanjura's function
        //       hypoel = hypoel*24.                   ! convert to daily rate
        //       hypoel = hypoel*0.6                   ! tune to namoi valley
        //       if(stemlt.14.44)hypoel = hypoel*(-0.5) ! delay below 14.44
        //       esum = esum+hypoel

        //      if(esum .lt. sdepth) return
        //      iemrg = i

        //      return
        //      end subroutine



        private double ozcot_fract_interception( int skiprow, double alai, double rs )
        {
            // ====================================================================
            // Calculates the fraction of incident radiation being intercepted
            //
            // Code by A. Moore April 2015
            // ====================================================================
            //sv- 2015_04_24 Instead of calculating Light Interception differently in different parts of this code.
            //      ozcot_evap()
            //      ozcot_carrying_capacity()
            //      ozcot_evapotransp()


            const double  intercept_k = 0.52;
            const double breakpoint_lai = 1.6;
            const double closure_lai = 3.0;

            double alai_row;
            double f_int;
            double breakpoint_f_int;

            //correct alai if we are using skip rows instead of solid.
            if (skiprow > 0)
            { alai_row =  alai * rs;  }
            else
            { alai_row =  alai; }

            //calculation f_int from alai
            if (alai_row <= 0.0)               // error trap for lai < 0
            { f_int = 0.0; }
            else if (alai_row < breakpoint_lai)  // between 0 and 1.6
            { f_int = 1.0 - Math.Exp(-intercept_k * alai_row); }
           else if (alai_row < closure_lai)      // between 1.6 and 3.0
            {
                breakpoint_f_int = 1.0 - Math.Exp(-intercept_k * breakpoint_lai);
                f_int = breakpoint_f_int + (1.0 - breakpoint_f_int) * (alai_row - breakpoint_lai) / (closure_lai - breakpoint_lai);
            }
            else                                  // lai > 3.0
            { f_int = 1.0; }

            return f_int;
        }


        // ====================================================================
        //      subroutine evap (i)
        public void ozcot_evap()
		{
			// ====================================================================
			// retained to calculate eo for ep calculation.
			//ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
			//      calculates potential evapotransperation from modified pen-  c
			//      man equation as reported by ritchie.                        c
			//                                                                  c
			//      key variables:                                              c
			//          delta = slope of the water vapor/ air tem curve       c
			//          albedo = combined plant-soil albedo                     c
			//          h = net radiation balance                               c
			//          eo = potential et over bare soil                        c
			//          eos = potential et below plant canopy                   c
			//ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc


			double elev;
			double pp;
			double gamma;
			double tk;
			double delta;
			double d;
			double svpmax;
			double svpdry;
			double svpwet;
			double vpdry;
			double salpha;
			double albedo;
			double r4;
			double r6;
			double h;
			double at;
			double f_int;
            double interceptGndAreaBasis;


			//- implementation section ----------------------------------

			//---------- calculate bowen ratio term -----------------------------------------

			elev = elevation_default;			             // elevation (m)                  'const
			//jh        elev=200.0                                    ! elevation (m)                  !const
			pp = 1013.0 - 0.105 * elev;		                 // mean pressure(mb)              'const
			gamma = 0.00066 * pp * (1.0 + 0.00115 * tempwt); // psychrometer constant             'const
			tk = tempav + 273.0;			                 // mean temp(deg kelvin)        'const
			delta = (Math.Exp(21.255 - 5304.0 / tk)) * (5304.0 / (Math.Pow(tk, 2.0)));	//slope sat vap pres curve      'const
			d = delta / (delta + gamma);

			//------ calculate fraction radiation reaching surface(tr) ----------------------
			//       culls functions(unpublished)



            ////=============================================================  DBJ 19/5/15  for debug
            //xn1 = 0.404 * Math.Log(s) + 1.49;
            //test_alai_row = laiField;
            //if ((skiprow > 0)) test_alai_row = laiField * effectiveRowSpacing; 		  // lai in hedgerow
            //if ((test_alai_row < xn1))                         // when lai below xn1 threshold
            //{
            //    test_tr = Math.Exp(-0.6 * test_alai_row);
            //}
            //else                                          // when lai above xn1 threshold
            //{
            //    test_tr = Math.Exp(-0.398 * test_alai_row);
            //}

            //if ((skiprow > 0))
            //{
            //    test_f_int = 1.0 - test_tr;				// interception in hedgerow
            //    test_f_int = test_f_int / effectiveRowSpacing;				// interception on ground area basis
            //    test_tr = 1.0 - test_f_int;				// transmission on ground area basis
            //}
            ////jh         laiField = laiField/effectiveRowSpacing        ! restore lai to ground area basis
            //else
            //{
            //   // test_f_int = 0.0;  Bug needed correcting 28/04/2015
            //    test_f_int = 1.0 - test_tr;
            //}

            //test_f_intz = Math.Max(test_f_intz, test_f_int);   // f_intz is cover_tot
            ////=============================================================

            //=============================================================

            //=============================================================

            // dbj Apr2015  added call to common routine ozcot_fract_intercerption()
            // set the amount of radiation being intercepted and the crop cover values
            f_int = ozcot_fract_interception(skiprow, laiField, effectiveRowSpacing);
            tr = 1.0 - f_int;                   // tr = transmission only needed for cover_green
            // need to add effectiveRowSpacing adjustment
            if (skiprow > 0)
            {
                interceptGndAreaBasis = f_int / effectiveRowSpacing;				// interception on ground area basis, skiprow influence
            }
            else
            {
                interceptGndAreaBasis = f_int;		           // interception on ground area basis
            }

            f_intz = Math.Max(f_intz, interceptGndAreaBasis);

            //****************************************************************************************************************************************
            //dbj debug 19/05/2015
            //Console.WriteLine("    test_f_int: {0:N6}      f_int: {1:N6}   test_ep_int: {2:N4}   test_f_intz: {3:N6}    f_intz: {4:N6}     test_tr: {5:N4}    tr: {6:N4}   test_ep: {7:N4}   ep: {8:N4}   test_alight: {9:N3}   test_alai_row: {10:N2} ", test_f_int, f_int, test_ep_int, test_f_intz, f_intz, test_tr, tr, test_ep, ep, test_alight, test_alai_row);
            //****************************************************************************************************************************************


            //=============================================================
			//      if(effectiveRowSpacing.gt.1.) tr = (tr+effectiveRowSpacing-1.)/effectiveRowSpacing ! adjust for rows wider than im

			//-----vapor pressure deficit: mean of vpd at 9am and 3pm ,assuming atmos.
			//     v(vpdry) remains constant throughout day, and tmax=t3pm-----------------

			svpmax = ozcot_satvp(tempmx);			// sat vp at tmax=sat vp at t3pm
			svpdry = ozcot_satvp(tempdy);			// sat vp at 9am tdry
			svpwet = ozcot_satvp(tempwt);			// sat vp at 9am twet

			vpdry = svpwet - gamma * (tempdy - tempwt);	 // atmospheric vp
			if ((vpdry <= 0.0)) vpdry = 0.1; 			 // cannot be -ve.

			vpd = ((svpdry - vpdry) + (svpmax - vpdry)) / 2.0;    // used in laigen routine

			//------ calculate potential evaporation (eo) -----------------------------------

			salpha = 0.09;
			albedo = 0.3067 + 0.3333 * salpha - ((0.23 - salpha) / 0.75) * tr;			// ??   'const

			//------net longwave radiation(r6,cal/cm2)--------------------------------------

			//      ritchie(1975)unpub.;idso & jackson(1969)j geophys res,74:5397-5403;
			//      jensen et al(1969)proc am soc civil engin,96:61-79                             ! temple,texas
			//        r4=1.-.261*exp(-7.77e-04*tempav**2.)        ! sky emissivity,ea
			//        r6=(r4-.96)*1.17e-07*tk**4.*(.2+.8*solrto)  ! ((ea-es)*@*tk**4)*(-)

			r4 = 1.08 * (1.0 - Math.Exp(-Math.Pow(vpdry, (tk / 2016.0))));
			r6 = (r4 - 1.0) * 1.0 * 1.17E-07 * Math.Pow(tk, 4.0) * (0.2 + 0.8 * solrto);
			// ((ea-es)*@*tk^4)*(-)       'const
			//   es=emissivity of evap surface  !const

			//-------net radiation(h,cal/cm2)----------------------------------------------

			h = (1.0 - albedo) * solrad + r6;
			if ((h < 0.0)) h = 0.0;
            //Console.WriteLine("     h {0}      albedo {1}      solrad {2}    r6 {3}   tempmn {4}\n\n", h, albedo, solrad, r6, tempmn);

			ho = h / 583.0;			// net radiation(cm)             'const
			//       go=g/583.                                   ! soil heat flux(cm)

			//-------advection or aerodynamic term ------------------------------------------

			if ((wind != 0.0 & tempwt != 0.0))
			{
				at = (1.0 - d) * 0.01 * (1.0 + 0.028 * wind) * vpd;	// advection, wind & vpd       'const
			}
            else if ((wind == 0.0 & tempwt != 0.0)) {
				at = -11.9813 + 0.012 * vpd + 0.0404 * tk;			// advection, vpd but no wind   'const
			}
			else
			{
				at = -20.4355 + 0.0697 * tk; 				       // advection,  no vpd or wind   'const
			}
			if ((at < 0.0)) at = 0.0; 

			//dbj Apr2015 'eo' read in daily from SOILWAT component to standardise values used and overcome
            //  perculiar calculation issues in OZCOT
			
            //eo = ho * d + at;
            
            
            //Console.WriteLine("     eo{0}      ho{1}      d{2}    at{3}\n\n", eo, ho, d, at);

			// gdar mar"85, update abh mar88.

			//------ calculate potential below canopy evaporation (eos) ---------------------

			//jh v2001        rns=ho*tr ! gdar mar'85
			//jh v2001        eos=d*rns ! gdar mar'85
			//jh v2001        if(eos.lt.0.0)eos=0.0
			//jh v2001        if(eos.gt.eo)eos=eo

			return; 
		}



        // ====================================================================
        //      function frugen(i)
        public double ozcot_frugen(double ndas)
        {
            double fruitGenerated = 0.0;
            // ====================================================================
            //
            //     estimates generation of new fruit as function of existing
            //     fruiting sites and bolload adjusted for nitrogen stress

            //     "cutout" occurs in frugen when squaring (fruiting site) production stops.
            //     which is caused by:
            //           boll n demand > supply  --  accounted for by fnstrs=0, terminal
            //           boll c demand > supply  --  which occurs when
            //               (i) assimilation not reduced by water stress or waterlogging
            //                   and boll load exceeds capacity; terminal until 2nd cycle
            //                   cycle after enough bolls open to reduce load, but capacity
            //                   may be reduced by this time by reduced lai
            //               (ii) assimilation reduced by water stress or waterloggin;
            //                   this need not be terminal if the stress ends, but the rate
            //                   of squaring will be rediced if the stress is prolonged,

            //      data from "siratac" - 1987-88 hearn (pers. comm.).



            //pc   real sites1
            double blr;
            double dfru;
            double vsnstr;
            double ppm_row;
            double popfac;

            //- implementation section ----------------------------------

            //-----------------------------------------------------------------------------
            //     initialise on first call for new crop
            //-----------------------------------------------------------------------------

            //      if(i.eq.isq+1)then           ! 1st call,ie day after 1st square
            if ((ndas == isq + 1))          // 1st call,ie day after 1st square
            {
                n_cutout = 0;               // flag to show cutout, >= 1 = not squaring
                sites1 = 0.0;               // to save current value of sites
            }
            fruitGenerated = 0.0;
            // delete y"day"s value and reset to zero
            //-----------------------------------------------------------------------------
            //     is this 1st cycle, cutout or 2nd cycle (regrowth)?
            //     crop cuts out, either temporarily or terminaly, when boll load exceeds
            //     the value cutout (from s/r carrying). cutout is zero the day after 1st
            //     square, which is therefore treated as a special case below.
            //-----------------------------------------------------------------------------

            if ((cutout > 0.0 & bollz > 0.0))      // post 1st flower
            {
                blr = bload / cutout;              // boll load ratio
                if ((blr > 1.0)) blr = 1.0;        // full boll load
            }
            else if ((cutout > 0.0 & bollz == 0.0))  // pre-1st fl / all boll shed
            {
                //          if(ndas.eq.isq+1) then      ! day after 1st square ?
                blr = 0.0;                 // if so, no boll load
            }
            else if ((cutout == 0.0 & ndas == isq + 1))
            {
                blr = 0.0;                // if so, no boll load
            }
            else                          // cutout=0 and not day after 1st square
            {
                blr = 1.0;                // full boll load because cutout=0
            }

            //     if cutout is caused by water stress (ie smi < 0.25) then
            //     10% of fruiting sites become inactive for frugen every day after 5 days
            //     which simulates lag in recovery after relief of stress
            //-----------------------------------------------------------------------------
            if ((blr == 1.0 | fnstrs == 0.0))
            {
                n_cutout = n_cutout + 1;                // count days cutout
                if ((n_cutout > cutout_smi_days))
                {
                    sites1 = sites1 + cutout_smi_site_red * size * ppm2;     // inactive sites  
                }
                return fruitGenerated;
            }

            else if ((n_cutout >= 1))       // stress finished, squaring resumes
            {
                
                if ((sites1 > sites))
                {
                    sites1 = sites;
                }
                n_cutout = 0;              // recovery complete, squaring resumes
            }

            //-----------------------------------------------------------------------------
            //     square production for this day
            //-----------------------------------------------------------------------------

            size = (sites - sites1) / ppm2;            // active sites per plant for frugen & surviv
            if ((size < 1.0)) size = 1.0;             // average  plants has 1 site
            if ((size < 0.5 / ppm2)) size = 0.5 / ppm2; // average  plants has 1 site   

            dfru = sqcon * Math.Sqrt(size) * ppm2 * (1.0 - blr);     // sites per dd
            vsnstr = ozcot_stress(vsnstr_low, vsnstr_high, vsnstr_a, vnstrs); // vegetative n stress             dfru = dfru * vsnstr;

            ppm_row = ppm2 * effectiveRowSpacing;                      // plants per m row for popfac
            popfac = 1.0 / (1.0 + popcon * ppm_row); // plant population factor within row
            dfru = dfru * popfac;                    // adjust for plant population
            
            fruitGenerated = dfru * dd;         // today"s squares
            if ((fruitGenerated < 0.0)) fruitGenerated = 0.0;
            return fruitGenerated;

        }


        // ====================================================================
        //     subroutine fruit (i,iend)
        public void ozcot_fruit()
		{
			// ====================================================================

			//      ages fruit, sums fruit in categories, estimates physiological
			//      shedding and survival of fruit.  calls s/r actfru to estimate
			//      new squares flowers and open bolls from counts. alternatively
			//      calls s/r ozcot_frugen to estimate new squares when counts not
			//      available.


			int cat;
			int age;
			int cohort;
			int lf;
			int nfl;
            int inFlower;
            int cohortsInFlower;  // was ndayfl
            int markage;
            int idxFruitCat;
			double surv;
			double fruit_shed_dm;
            List<int> listCohortsInFlower = new List<int>();


            //- implementation section ----------------------------------


            // Each day initialise arrays and local variables.
            // These are re-populated in the loops that follow.
            squarz = 0.0;
            cohortsInFlower = 0;
            frudw = 0.0;
            Array.Clear(frucat, 0, frucat.Length);
            Array.Clear(sfmcat, 0, sfmcat.Length);
            Array.Clear(fmkcat, 0, fmkcat.Length);
            
            bollz = 0.0;            // Clear the green boll count (used in bollwt() to calc boll stress)
            dm_fruShedToday = 0.0;  //clear accumultion of shed fruit weights for today
            dm_fruWtToday = 0.0;    // clear accumulation of dry weight added by growing bolls


            //---- compute boll period ------------------------------------------------------

            // use Hearn's standard OZCOT equation for boll period

            bper = Math.Exp(5.385 - 0.0512 * tempav);           // boll period constable
            bper = bper * (frudd[inedible_bolls] - frudd[large_sqz]) / 788.0;   // adjust for variety
            bper = 1.0 / bper;                              // fraction for day


            // Calculate the nominal(reference)(bgrvar) and actual (bollgr) boll growth rates for today 
            // Note: this is based on the existing fruit load and carrying capacities (from yesterday) 
            ozcot_bollwt();

            //-----------------------------------------------------------------------------
            //     The next loop ( Main Cohort Loop ) goes through the fruit arrays
            //     starting with the element with oldest fruit and therefore the smallest
            //     index, in order to develop or abscise fruit, and put cohorts into categories.
            //     Before entering the loop, set marker for the oldest fruit.
            //-----------------------------------------------------------------------------

            lf = lfru[open_bolls];		        // 1st active element in fruit array, oldest immature fruit

			if ((lf == 0)) lf = isq - 10; 		// start in fruno 10 days before 1st square             'const


            // MAIN COHORT LOOP START
            for (cohort = lf; cohort <= DAS - 1; cohort++)           //loop from oldest growing boll to previous day
            {		
                if (cohort > max_cohort) break;                   // exceeds 300 days, exit loop

                //---- age & abscise marked fruit----------------------------------------

                //  Each daily cohort has an array of 7 days marked fruit associated with it.
                //  Fruit Removal happens daily by removing 'Day 7' fruit.  Stress causes 
                //   fruit to be added to FruitMarked at Day 1 for removal in 7 days time.
                //
                //  First, shuffle the marked fruit along one for today (discarding the old 'day 7')

                for (age = age7; age >= 2; age += -1)
                {
                    frmark[cohort, age] = frmark[cohort, age - 1];
                }

                frmark[cohort, age1] = 0.0;		// clear first day

                //
                // Now shed the fruit at day 7 of the marked array (adjust fruit numbers and fruit weight)
                // (first report a warning to the error log if there are insufficient fruit to shed) 
                //
                if ((fruno[cohort] > 0.0 & frmark[cohort, age7] > 0.0))  // fruit shed today?
                {
                    if ((frmark[cohort, age7] > fruno[cohort]))
                    {
                        //write#1, cohort,fruno(cohort) ,frmark(cohort,age7)
                        //format (' too many marked fruit:',i4,2f5.1)
                        frmark[cohort, age7] = fruno[cohort];   		// Limit marked fruit to what is available
                    }
                    fruit_shed_dm = 0.0;
                    fruit_shed_dm = (fruwt[cohort] / fruno[cohort]) * frmark[cohort, age7] ;    // calc wt shed this cohort  g/m2
                    dm_fruShedToday = dm_fruShedToday + fruit_shed_dm;              //record shed_dm all cohorts for today to pass to ozcot_update to remove biomass
                    dm_fruShedTot = dm_fruShedTot + fruit_shed_dm;                  // accumulate total dm fruit shed for whole crop   g/m2
                    fruwt[cohort] = fruwt[cohort] - fruit_shed_dm;					// adjust wt in cohort   g/m2
                    fruno[cohort] = fruno[cohort] - frmark[cohort, age7];			// remove marked fruit   number
                }
                

                //
                //------  increase in weight of bolls (seed cotton) ---------------------------
                if (cohort <= lfru[flowers])   // for cohorts older (smaller cohort#) than flowers: ie. bolls
                {
                    fruwt[cohort] = fruwt[cohort] + fruno[cohort] * bollgr;     // increase today for cohort day bolls
                    dm_fruWtToday += (fruno[cohort] * bollgr);                  // record increase in 'pod' dm weight , added to dm_green[pod] in ozcot_dryxmatter()
                }
                else
                {
                    // should dry matter accumulation of squares (fruit categories 1 - 3) be added to keep dm_green[pod] values 'more' accurate?  dbj march 2016
                }

                //
                //------ fruit not growing - mark fruit for shedding  ------------------------------------------------
                //
                //  If Category 5 and 6 bolls (small and medium bolls) are not effectively growing (very small size), they should be shed tomorrow
                //
                if ((cohort > lfru[small_bolls] & cohort <= lfru[large_bolls] & fruno[cohort] > 0.0))
                {
                    if ((fruwt[cohort] / fruno[cohort] < 0.1)) frmark[cohort, age6] = fruno[cohort];
                }

                //
                // Increment Boll Period for Fruit at or past flowering (green bolls)
                // ------------------------------------------------------------------
                // (Note fyzage[] in DDs has been previously incremented in routine ozcot_pltgrw)

                if ((fyzage[cohort] > frudd[large_sqz])) bpsum[cohort] = bpsum[cohort] + bper; 	// develop this cohort's bolls


                //
                // Now accelerate this development for any number of reasons (eg stress, defoliation, etc...)
                //

                // defoliation
                if ((defoliationCnt > 0) | (iend == 5))  	// develop faster after defoliation or frost stress
                {
                    bpsum[cohort] = bpsum[cohort] / 0.99;
                }

                // frost or last few late opening bolls
                if ( iend == 4 | iend == 5 | iend == 6)   // frost or green bolls < 1/mtr with some open bolls (crop complete)
                                              // ignore iend = 3  as this is frost on young crops with no bolls
                {
                    if ((bpsum[cohort] >= 0.9)) bpsum[cohort] = 1.0; 	// crop finishing; opens phys mat bolls
                }

                // carbon supply stress or water stress  (code added by MPBange & SMilroy in 2002 'fix' code)
                if ((bload > carcap_c) | (f_limiting < 1.0))         // carbon or water stress
                {
                    bpsum[cohort] = bpsum[cohort] / 0.98;           // accelerate bolls in this cohort
                    if (bpsum[cohort] >= 0.9) bpsum[cohort] = 1.0;  // force open near physiologically mature bolls
                }



                //  
                //  now assign this cohort to a fruit maturity category
                //  ---------------------------------------------------
                cat = 1;	//fruit start in category 1 - small squares

                //   use age in dds for fruit categories 1 (small_sqz) - 3 (large_sqz)
                if ((fyzage[cohort] >= frudd[small_sqz])) cat = medium_sqz; 	 // medium squares
                if ((fyzage[cohort] >= frudd[medium_sqz])) cat = large_sqz; 	 // large squares
                if ((fyzage[cohort] >= frudd[large_sqz])) cat = flowers; 	     // flowers

                //   use bollperiod for fruit categories 4 (flowers) - 9 (open_bolls)
                if ((bpsum[cohort] >= bltme[flowers])) cat = small_bolls;        // small green bolls
                if ((bpsum[cohort] >= bltme[small_bolls])) cat = medium_bolls;   // medium green bolls
                if ((bpsum[cohort] >= bltme[medium_bolls])) cat = large_bolls; 	 // large green bolls
                if ((bpsum[cohort] >= bltme[large_bolls])) cat = inedible_bolls; // inedible green bolls
                if ((bpsum[cohort] >= bltme[inedible_bolls])) cat = open_bolls;  // open bolls

                //   
                //  now adjust the fruit category pointers which are stored in lfru array
                //  ------------------------------------------------------------------------
                //  
                if (cohort > lfru[cat])
                {
                    // cohort has changed categories and sets a new top marker for the category
                    lfru[cat] = cohort;

                    //  count the number of cohorts that become flowers today
                    //     count needed for use with fruit counts - yet to be re-implemented (dbj)
                    if (cat == flowers) cohortsInFlower = cohortsInFlower + 1;

                    //  if fruit just opened as a boll, then add this to the 'opened' bucket which is not reset each day and so needs to be added at point of change
                    if (cat == open_bolls)
                    {
                        openz = openz + fruno[cohort]; 	// total open bolls
                        dm_openBolls = dm_openBolls + fruwt[cohort];

                    }
                }

                // 
                //  We are now in a position to add this cohort's fruit to the relevant category's fruit count, fruit wt, etc
                //  ---------------------------------------------------------------------------------------------------------
                // 
                //  Categories 1 - 8.  
                //  Note: Category 9 fruit (open bolls) has been done already in the cohort loop 
                //        and added to a separate total (openz and openwt)
                if ((cat <= inedible_bolls)) frucat[cat] = frucat[cat] + fruno[cohort]; 	// sum fruit nos in categories

                //  count the number of squares today
                if ((cat <= large_sqz)) squarz = squarz + fruno[cohort]; 		// total squares

                //  add the fruit number and dry wt to green bolls if in category 4 - 8
                if ((cat >= flowers & cat <= inedible_bolls))
                {
                    bollz = bollz + fruno[cohort];			// total green bolls
                    frudw = frudw + fruwt[cohort];
                }

                // add this day's array of fruit marked for shedding to the 'fruit category by day' total and by 'fruit category total'
                //  only for fruit categories 1 - 7.  category 8 fruit (inedible green bolls do not shed easily)
                if ((cat <= large_bolls))
                {
                    for (markage = 2; markage <= 6; markage++)
                    {
                        // loop through marked fruit for this cohort (day 1 has not been calculated yet step  day 7 has already been shed)
                        fmkcat[cat, markage] = fmkcat[cat, markage] + frmark[cohort, markage];	// sum marked fruit
                        sfmcat[cat] = sfmcat[cat] + frmark[cohort, markage];					// sum fruit marked for bload
                    }
                }

            }  // END OF MAIN COHORT LOOP
            //=========================================================================

            // TODO:  dbj insert new code to allow use of fruit counts (as per stand-alone version)

            // 
            //  Set first square/fruit/boll dates (doy & DAS)
            //  ---------------------------------------------
            //  
            
            // Squaring
            //  (note: first square is set in ozcot_istsq())

            // Flowering                 ? dbj bollz at this point is exclusive of bolls shed today. 
            //                            (mpb thinks cgifirstflowerdoy test should be inc!, but how?)
            if ((firstFlowerDOY == 0) & (bollz > (ppm2 / 2.0)))
            {
                // mark first flower as date DAS (50% of plants)
                firstFlowerDOY = jdate;
                firstFlowerDAS = DAS;
                cropStage = firstFlower;
            }

            // Open Bolls
            if ((firstOpenBollDOY == 0 & openz > (ppm2 / 2.0)))
            {
                // mark first open boll as date DAS (50% of plants)
                firstOpenBollDOY = jdate;
                firstOpenBollDAS = DAS;
                cropStage = firstOpenBoll;
            }

            // move dry matter of bolls that are open into senesced pool
            dm_meal = dm_senesced[meal];
            dm_lint = dm_senesced[lint];
            dm_senesced[meal] = dm_openBolls * (1.0 - pcLint_frac);  // approx 57% seed by weight
            dm_senesced[lint] = dm_openBolls * pcLint_frac;  // approx 43% lint by weight
            ddm_meal = dm_senesced[meal] - dm_meal;   // today's increase in seed weight
            ddm_lint = dm_senesced[lint] - dm_lint;   // today's increase in lint weight

            // 
            // The plants fruit distribution is now at a 'known' point for today.
            //  we are now in a position to calculate the boll load and calculate the number of new sites 
            //  that the plant will be able to generate today, plus estimate the survival rate of the squares
            //  at these new site.

            // 
            // Calculate Boll Load
            // -------------------
            bload = 0.0;
            double fruitCount = 0.0;
            for (idxFruitCat = 1; idxFruitCat <= max_categories - 1; idxFruitCat++)
            {
                if (((frucat[idxFruitCat] - sfmcat[idxFruitCat]) >= 0.0))
                {
                    fruitCount = (frucat[idxFruitCat] - sfmcat[idxFruitCat]);
                }
                else
                {
                    fruitCount = 0.0;
                }

                bload = bload + fruitCount * wt[idxFruitCat];   //  boll load = fruit num * wt (rel_C_Demand of category, varietal parameter) (cats 1 thru 8)
            }


 			//---- this day's production of fruit -----------------------------------------

			// Call carrying_capacity(i)
			ozcot_carrying_capacity();

            // Abort excess fruit  //TODO:  dbj add code for handling fruit counts
			if ((bload > carcap_c) | (bollz + openz > carcap_n)) ozcot_overload(); 

			//      if (i.le.idate)then                            ! use counted fruit?
			//      if (DAS.le.idate)then                            ! use counted fruit?
			//          call actfru (i)
			//          call actfru
			//          call update(1,1,daysqz,squarz,frudd)
			//          if(lfru(4).ne.0..or.daysfl.ne.0.)
			//     *        call update(4,ndayfl,daysfl,bollz,frudd)
			//          if(lfru(9).ne.0..or.daysone.0.)
			//     *        call update(9,1,daysop,openz,frudd)
			//      else
			//psc           icount = i               ! dummy parameter to use frugen
			//          fruno(i-isow)=frugen(i)  ! frugen is function generating squares
			
            // Calculate this day's production of fruit (sites/squares)
            fruno[DAS] = ozcot_frugen(DAS);

			//---- mark this day's fruit for physiological shedding      -----------------

            // surv = ozcot_survive(carcap_c, bload);  // calculate background shedding (shedding = 1 - survival rate)
            surv = ozcot_survive();  // calculate background shedding (shedding = 1 - survival rate)
                                     // square survival rate




            frmark[DAS, age1] = fruno[DAS] * (1.0 - surv);      //background shedding for today's cohort
			if ((frmark[DAS, age1] < 0.0)) frmark[DAS, age1] = 0.0; 
			fmkcat[small_sqz, age1] = fmkcat[small_sqz, age1] + frmark[DAS, age1];  //accumulate marked fruit for small_squares

            if ((cohortsInFlower > 0))  
            {                              // there are flowers today
                for (nfl = 1; nfl <= cohortsInFlower; nfl++)
                {
                    inFlower = lfru[flowers] - nfl + 1;
                    frmark[inFlower, age1] = fruno[inFlower] * (1.0 - surv);
                    if ((frmark[inFlower, age1] < 0.0)) frmark[inFlower, age1] = 0.0;
                    fmkcat[flowers, age1] = fmkcat[flowers, age1] + frmark[inFlower, age1];
                }
            }

            //---- add new fruit to totals ------------------------------------------------

            //      sites = sites+fruno(i-isow)
            //      if(i.le.idate)return ! squarz & frucat updated in update
            //      squarz = squarz+fruno(i-isow)
            //      frucat(1) = frucat(1)+fruno(i-isow)

            //TODO: dbj  revise values if fruit counts are being used/set
            sites = sites + fruno[DAS];
			squarz = squarz + fruno[DAS];
			frucat[small_sqz] = frucat[small_sqz] + fruno[DAS];




            return; 
		}



        // =======================================================================
        public void ozcot_check_maturity()
        {
            // ====================================================================

            //     this subroutine test the maturity of the crop
            //     logic removed from old ozcot_harvest() that tested for defoliation conditions
            //

            // setting of iend = 6 "Crop ready to Harvest"  moved to end of ozcot_pltgrw()  dbj Nov 2015
            // and is dependant upon defoliationCnt and fieldlai < 0.1

            if (cropStage >= mature)
            {
                return;
            }

            if ((openz / (bollz + openz) > open_def / 100.0) )
            {
                cropStage = mature;
                maturityDAS = DAS;
               // iend = 6;  don't set this here as APSIM will see it as 'harvest'
                Console.WriteLine("{0} {1} {2} (Day of year={3}), cotton: ", Today.Day, Today.ToString("MMMM"), Today.Year, DOY);
                Console.WriteLine("   Crop has reached the percent open bolls as defined for maturity. Crop mature.");
            }


            return;
        }




        // =======================================================================
        public void ozcot_weather_open_bolls()
        {
            //-----------------------------------------------------------------------------
            // DBJ added Nov 2015 for Steve Yeates N. Australian studies
            //-----------------------------------------------------------------------------
            //     the next loop goes through the fruit array for the open bolls ONLY
            //     starting with the element with oldest fruit and therefore the smallest
            //     index, and ending with the marker for the youngest open bolls cohort.
            //     Values are copied to the openBollsArray[] (new entries) for fruitNum, fruitWt 
            //     and today's rainfall added to the accumulated rain for each cohort.
            //     (this can be used for colour deterioration calculations).
            //-----------------------------------------------------------------------------
            int newestOpenBoll;  // marker for youngest (uppermost) openBoll cohort
            int cohort;

            newestOpenBoll = lfru[open_bolls];		        // 1st active element in fruit array, oldest immature fruit
            if (newestOpenBoll > 0)                         // there are cohorts of open bolls
            {
                for (cohort = 1; cohort <= newestOpenBoll ; cohort++)
                {
                    if (fruno[cohort] > 0.0)
                    {
                        openBollsArray[cohort, 0] = fruno[cohort];  //set|reset the number of fruit
                        openBollsArray[cohort, 1] = fruwt[cohort];  //set|reset the dry wt of fruit
                        openBollsArray[cohort, 2] += rain_mm;       //add todays rain to the accumulated rainfall for this cohort of open bolls
                    }
                }
                if (cropStage == harvest_ripe)
                {
                  //  Console.WriteLine("{0} {1} {2} (Day of year={3}), cotton: Rain added today = {4}", Today.Day, Today.ToString("MMMM"), Today.Year, DOY, rain_mm);
                }

            }


            return;
        }



        // =======================================================================
        public void ozcot_defoliate_crop()
        {
            // ====================================================================
            //     this subroutine simulates defoliation of the crop
            //     logic removed from old ozcot_harvest() that tested for defoliation conditions
            //     this has now been moved out of mvOZCOT and relies on Manager scripts to monitor 
            //     conditions and call Defoliation and Harvest as required.
            //
            //     Nov 2015. Revised logic:  Only call Defoliate once on a crop.  
            //               Force effective defoliation in 200 DD  or  max 23 days (whichever is shortest)


            laiRow = laiField;
            if (skiprow > 0) laiRow = laiField * effectiveRowSpacing;       // lai in hedgerow

            // if (laiRow < 0.5) {//do nothing }   Hearn code to not defoliate low LAI crop
            defoliationCnt += 1;                                // increment defoliant spray count
            if (defoliationCnt == 1) defol1DAS = DAS;				// day of 1st defol.
            if (defoliationCnt == 2) defol2DAS = DAS;				// day of 2nd defol.

            Console.WriteLine("{0} {1} {2} (Day of year={3}), cotton: ", Today.Day, Today.ToString("MMMM"), Today.Year, DOY);
            Console.WriteLine("         Defoliant spray   {0}", defoliationCnt);

            return;
        }




  ////      // ====================================================================
  ////      //      subroutine harvest(iend)
  ////      public void ozcot_harvest()
		////{
		////	// ====================================================================

		////	//     this subroutine simulates defoliation and picking
		////	//     use n_def, n_pick and j_pick for cost of defoliation:       nb action
  ////          //
  ////          //     subjects for future development in a more comprehensive whole farm
		////	//     management model.



		////	//- implementation section ----------------------------------

		////	//jh skip row correction
		////	laiRow = laiField;
		////	if (skiprow > 0) laiRow = laiField * effectiveRowSpacing; 		// lai in hedgerow

		////	if ((openz / (bollz + openz) > open_def / 100.0) & (defoliationCnt == 0))
		////	{
		////		j_def = iday;				     // day open_def. open
		////		//jh skip row correction
		////		if ((laiRow < 0.5))
		////		{
		////		}
  ////              else if ((defoliationCnt == 0)) {
		////			defoliationCnt = 1;					// 1st defoliant spray     'const
		////			defol1DAS = iday;				// day of 1st defol.
  ////                  Console.WriteLine("{0} {1} {2} (Day of year={3}), cotton: ", Today.Day, Today.ToString("MMMM"), Today.Year, DOY);
  ////                  Console.WriteLine("         Defoliant spray   {0}", defoliationCnt);
		////		}
                  
		////		else
		////		{
		////		}
		////	}
  ////          else if ((defoliationCnt == 1 & iday - defol1DAS == 10)) {	// 10 days since 1st defoliation?
		////		//jh skip row correction
		////		if ((laiRow > 0.2))
		////		{
		////			defoliationCnt = 2;					// 2nd defoliant spray      
		////			defol2DAS = iday;				// day of 2nd defol
  ////                  Console.WriteLine("{0} {1} {2} (Day of year={3}), cotton: ", Today.Day, Today.ToString("MMMM"), Today.Year, DOY);
  ////                  Console.WriteLine("         Defoliant spray   {0}", defoliationCnt);
  ////              }
		////		else
		////		{
		////			j_pick = jdate;				// date of picking
		////			//jh need effectiveRowSpacing correction   ??? TODO:  check for correction
		////			if ((bollz < 10))          // 10 bolls worth picking
		////			{
		////				n_pick = 1;			   // count picks 
  ////                  }
		////			//write #1,")"first pick ", iday," days from sowin "                      +  "there are not enough bolls for a 2nd pick."
		////			else                      // 10 bolls worth picking
		////			{
		////				n_pick = 2;			  // count picks 
		////				//write #1,")"first pick ", iday," days from sowin "                      +  "there are enough bolls for a 2nd pick."
		////			}
		////		}
		////	}
  ////          else if ((defoliationCnt == 2 & iday - defol2DAS == 10)) {
		////		j_pick = jdate;				// date of picking
		////		//jh need effectiveRowSpacing correction  ???  TODO: check for correction
		////		if ((bollz < 10))           // 10 bolls worth picking
		////		{
		////			n_pick = 1;				// count picks
		////		}
		////		//write #1,")"first pick ", iday," days from sowin "                   +  "there are not enough bolls for a 2nd pick."
		////		else
		////		{
		////			n_pick = 2;				// count picks
		////			//write #1,")"first pick ", iday," days from sowin "                   +  "there are enough bolls for a 2nd pick."
		////		}
		////	}

		////	//jh      if(j_pick.ne.0 .and. bollz.lt.1.0) then
		////	if ((j_pick != 0))
		////	{
		////		iend = 6; 				// terminate crop.  BYPASS 2nd Pick logic as obsolete!   
  ////              Console.WriteLine("{0} {1} {2} (Day of year={3}), cotton: ", Today.Day, Today.ToString("MMMM"), Today.Year, DOY);
  ////              Console.WriteLine("         Crop to be picked and terminated.");
  ////          }

		////	return; 
		////}


        // ====================================================================
        //      subroutine hfunc (i)
        public void ozcot_hfunc()
        {
            // ====================================================================
            //ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
            //      calculates daily heat units based on a 12.0 deg c base      c
            //      temperature.  the heat unit sum is based an integrated      c
            //      diurnal sin-wave with the maximum and minimum daily temp-   c
            //      eratures as the peak and trough, respectively.  heat units  c
            //      are not allowed to accumulate above a cutoff of 30.0        c
            //      (changed to 40 by ABH in 1983/4) the base (baset) and       c
            //      cutoff (hucut) temperatures are set in                      c
            //      the 'init' subroutine.                                      c
            //ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

            double amp;
            double tmax;
            double zeta;

            //- implementation section ----------------------------------


            // pi = 3.14159;               // const  replaced with Math.PI (DBJ 2012)
            // tempav=(tempmn+tempmx)/2.   // set immediately after daily weather is read (driver variables)

            amp = tempmx - tempav;
            tmax = tempmx;
            if (tmax >= hucut) tmax = hucut;   // limit max temperature effect

            if (tempmn >= baset) 
            {
                hunits = (tmax + tempmn) / 2.0 - baset;
            }
            else if (tmax <= baset)  
            {
                hunits = 0.0;
            }
            else
            {
                zeta = Math.Asin((baset - tempav) / amp);
                hunits = 1.0 / Math.PI * (amp * Math.Cos(zeta) + (tempav - baset) * (Math.PI / 2.0 - zeta));
            }
            
            return;
        }


        // ====================================================================
        //      subroutine istsq (i,nszn)
        public void ozcot_istsq()
		{
			// ====================================================================

			//     identifies first square event and gives initial value for
			//     fruno(1),sites & squarz.
			//     delayed by water stress (smi < 0.25) and cold shock (tempmn < 11)
			//     delayed
			//     currently sets isq=i, should be isq=iday for consistency between seasons
			//     when convenient, change and check all refs to isq

            //******************************************************************************
            //_____________________________________________________________________________
            //
            //        Modification LOG
            //_____________________________________________________________________________
            //
            //Who     Date      Description
            //------------------------------------------------------------------------------
            //DavidJ  14/12/12  Recoded to use RateOfDevelopmentToFirstSquare (Bange & Milroy 2003)
            //                   as  accumRateTo1stSqr
            //DavidJ  21/10/14  Add switch to allow Accumulation of DD to 1st Square OR  RateOfDevelopment
            //                  Switch is init property useDDsumTo1stSq
            //******************************************************************************

			//- implementation section ----------------------------------
            double rateToday;
            double adjRate;


            // reinstate code as optional 
            if (useDDsumTo1stSq == true)
            {
                if (firstCallIstSq)
                {
                    firstCallIstSq = false;
                    Console.WriteLine("Cotton development to First Square using DDSum ");
                }
                if (smi < smi_delay_crit)           // water stress delays squaring
                {
                    delayDD = delayDD + (1.0 - (smi / smi_delay_crit)) * hunits;
                }

                // cold shock constable (pers. comm)
                if (tempmn < cold_shock_delay_crit)
                {
                    delayDD = delayDD + cold_shock_delay;
                }

                //      if(hail) delay = delay + tipout + hail_lag    ! tipping out delay
                if (sumdd < ddisq + delayDD)
                {
                    return;
                }
                else
                {
                }
            }
            else  //useDDsumTo1stSq is either blank or "false"
            //-- new code ------------------------------------------------------------
            {
                if (firstCallIstSq)
                {
                    firstCallIstSq = false;
                    Console.WriteLine("Cotton development to First Square using RateOfDevelopment ");
                }

                rateToday = 0.0;
                adjRate = 0.0;

                rateToday = 0.03 * (1 - Math.Exp(-0.20 * (tempav - 15.03)));   // Rate of Development to First Square : Bange & Milroy 2003  (sowing to 1stSq)
                //lrRateToday = 0.0368*(1-EXP(-0.2305*(clrTempAv - 15.4906)))  !Bange revised Feb 2014 for Emergence to 1stSq

                if (smi < smi_delay_crit)           // water stress delays squaring
                {
                    adjRate = (1.0 - (smi / smi_delay_crit)) * rateToday;
                    rateToday = rateToday - adjRate;
                }

                if (rateToday > 0.0)              // some development today
                {
                    accumRateTo1stSqr = accumRateTo1stSqr + rateToday;
                }

                if (accumRateTo1stSqr < 1.0)     // still not reached 1st square
                {
                    return;
                }
            }

            //-- end new code ------------------------------------------------------------

			//      fruno(i-isow)=1.0*ppm2  ! average plant has 1 square
			//      fruno(i-isow)=0.5      ! as in 1983/84 version
			//  fruno[DAS] = 1.0 * ppm2;			// average plant has 1 square
			//  fruno[DAS] = 0.5 * ppm2;			// 50% plants have 1 square

            // TODO:  check for correction for row spacing
			//jh need effectiveRowSpacing correction ??? this has 0.5 fruit/m2 regardless of rowspacing
			fruno[DAS] = 0.5;			    // as in 1983/84 version


			//     square & site production on first day

			//      squarz=squarz+fruno(i-isow) ! sum squares
			//      sites=sites+fruno(i-isow) ! sum sites
			//      isq=i                     ! should be isq=iday see above
			squarz = squarz + fruno[DAS];		// sum squares
			sites = sites + fruno[DAS];			// sum sites
			isq = DAS;                          // should be isq=iday see above

            cropStage = firstSquare;

            return; 
		}


        // ====================================================================
        //      subroutine laigen (i)
        public void ozcot_laigen()
        {
            // ====================================================================

            //     estimates current lai. generates new leaf area daily as a
            //     function of dd and fruiting site production. initial area
            //     is a function of dd to first square then a function of new
            //     sites. relative rate of area expansion a function of dd
            //     to 1st square, then a function of dsites .
            //     areas are square m leaf per square m land (laiField & dlai)
            //     or square m per site (dlds & dlds_max)

            //     revised by abh april 1995

            //     Note: leaves are senesced but there is no routine to shed (detach) senesced leaves (DBJ Aug 2015)
            //           This has implications for calculation of cover_tot and surfaceOM in the APSIM system

            double dlds;
            double dlds_x;
            double vlnstr;
            double flfsmi;
            double flfsmi_x;
            double actrgr;
            double alaix;
            double ddleaf;
            double range;
            double hi;
            double a;
            int index;
            int lin;
            int l;

            //jh      data acotyl /.00035/
            //jh      data rlai /.00835/
            //jh      data dlds /0.0362/
            //jh      data leaf_res_n_conc /0.02/

            //- implementation section ----------------------------------


            //------- initialise leaf area on day of emergence -----------------------------

            if ((DAS == iemrg))
            {
                // already emerged?
                //        dlai(iemrg-isow)=acotyl*ppm2            ! initial area
                //        laiField=dlai(iemrg-isow)
                //        lastlf=iemrg-isow                      ! set index for oldest leaf
                acotyl_m2 = acotyl / 1000000.0;  // convert mm2 to m2
                dlai[iemrg] = acotyl_m2 * ppm2;    // initial area
                laiField = dlai[iemrg];
                lastlf = iemrg;                // set index for oldest leaf

                return; 
            }

            //------- calculate water stress from smi and vpd -------------------------------

            range = vpd - 15.0;
            if ((range > 15.0)) range = 15.0;

            hi = 0.5 + 0.4 / 15.0 * range;         // smi below which stress occurs
            if ((hi > 1.0)) hi = 1.0;
            if ((hi < 0.01)) hi = 0.01;

            a = 1.0 - 0.7 / 15.0 * range;          // shapes of response to smi
            if ((a > 1.0)) a = 1.0;
            if ((a < 0.01)) a = 0.01;

            flfsmi = ozcot_stress(0.0, hi, a, smi);         // stress factor
            flfsmi_x = ozcot_stress(0.0, hi, a, 1.0);       // stress factor

            //------- calculate rate of leaf area expansion --------------------------------

            //jh      a  =   0.1847  !
            //jh      b1 =  -0.1165  ! constants for lai calibration eqn
            //jh      b2 =  -0.01514 ! 29 april 1988
            //jh      b3 =   0.01984 ! expts wn8283 & 8384

            //      dlds_x = (a+b1*0.75+b2*vpd+b3*0.75*vpd) ! sqrt area/site, no water stress
            //      if(dlds_x.lt.0.) dlds_x = 0.
            //      dlds_x = dlds_x**2                      ! area per site, no water stress

            //      dlds = (a+b1*smi+b2*vpd+b3*smi*vpd) ! sqrt area per site
            //      dlds = (a+b1*smi*1.2+b2*vpd+b3*smi*1.2*vpd) !debug  ! sqrt area per site
            //      dlds = (a+b1*0.75+b2*vpd+b3*0.75*vpd) !debug  ! sqrt area per site
            //      if(dlds.lt.0.) dlds = 0.
            //      dlds = dlds**2                          ! area per site
            //      flfsmi = ozcot_stress(flfsmi_low
            //     :                      ,flfsmi_high
            //     :                      ,flfsmi_a
            //     :                      ,smi) ! pre-squaring

            //-------------------------------------------------------------------------------

            if ((isq == 0))    // crop not yet squaring
            {
                actrgr = rlai;             // actual rgr
                  //jh           actrgr=actrgr*flfsmi          ! water stress
                alaix = laiField;              // save previous lai
                //index = ifix(dd);          // index for do loop below to grow leaf
                index = (int)Math.Truncate(dd);          // index for do loop below to grow leaf

                for (lin = 1; lin <= index; lin++)
                {
                    alaix = alaix * (1.0 + actrgr);   // grow leaf without water stress
                }
                dlai_pot = alaix - laiField;              // days increase without water stress

                actrgr = actrgr * flfsmi;             // water stress
                alaix = laiField;                         // save previous lai again

                for (lin = 1; lin <= index; lin++)
                {
                    alaix = alaix * (1.0 + actrgr);   // grow leaf with water stress
                }
                dlai[iday] = alaix - laiField;            // days increase with water
            }
            else             // crop now squaring
            {
                dlds = dlds_max * flfsmi;             // adjust for water stress
                if (dlds < 0.0) dlds = 0.0;
                dlds = Math.Pow(dlds, 2.0);             // area per site
                dlds = dlds * flai;                   // adjust for variety, 87 mki calib"n
                dlai[iday] = fruno[iday - 1] * dlds;  // days incr in lai
                                                      //  without water stress
                
                //    dlds_x = dlds_max*flfsmi        ! adjust for water stress
                dlds_x = dlds_max * flfsmi_x;
                if (dlds_x < 0.0) dlds_x = 0.0;
                dlds_x = Math.Pow(dlds_x, 2.0);            // area per site
                dlds_x = dlds_x * flai;                  // adjust for variety, 87 mki calibration
                dlai_pot = fruno[iday - 1] * dlds_x;     // days incr in lai
                                                         //  with water stress
            }

            vlnstr = ozcot_stress(vlnstr_low, vlnstr_high, vlnstr_a, vnstrs);
            dlai[iday] = dlai[iday] * vlnstr;            // adjust for n stress
            dlai_pot = dlai_pot * vlnstr;                // adjust for n stress
            laiField = laiField + dlai[iday];

            //*******senescence ***************************************************

            // TODO: dbj  defoliation delay effect needs to be based on thermal time or some other criteria to handle different conditions/regions
            // Hearn's original value was > 7 (days)
            ddleaf = ozcot_senlf();

            //if (defoliationCnt == 1 & (iday - defol1DAS) > 7) ddleaf = ddleaf * 0.33;      // 1st defol"n
            //if (defoliationCnt == 2 & (iday - defol2DAS) > 7) ddleaf = ddleaf * 0.0;       // 2nd defol"n , kill all leaf

            // hard coded test for Steve Yeates Nov 2015 
            //if ((defoliationDDSum > 100.0) | (defoliationDayCnt > 12)) ddleaf = ddleaf * 0.33;  // kill 2/3 of leaf
            //if ((defoliationDDSum > 200.0) | (defoliationDayCnt > 22)) ddleaf = ddleaf * 0.0;   // kill all leaf

            if ( (defoliationDayCnt >  7) | (defoliationDDSum > 100.0)) ddleaf = ddleaf * 0.33;   // kill 2/3 of leaf
            if ( (defoliationDayCnt > 14) | (defoliationDDSum > 200.0)) ddleaf = ddleaf * 0.0;    // kill all leaf

            shedlf = 0.0;       // initialise for this day
            leaf_res = 0.0;

            if (lastlf > 0)           // called after measured lai finished
            {
                for (l = lastlf; l <= DAS; l++)        // loop thro unshed leaves : oldest to youngest
                {
                    if ((fyzage[l] < ddleaf)) break;   // are this days leaves shed today?
                    laiField = laiField - dlai[l];                // reduce lai
                    shedlf = shedlf + dlai[l];            // sum area of shed leaves
                    dlai[l] = 0.0;                        // day's area now zero
                    dm_leaf = dm_leaf - ddm_l[l];         // reduce leaf dry matter
                    leaf_res = leaf_res + ddm_l[l];       // sum this day's residues
                    ddm_l[l] = 0.0;                       // this day"s leaf wt now zero
                    lastlf = l + 1;                       // set index for oldest remaining leaves.
                }
            }

            leaf_res_n = leaf_res * leaf_res_n_conc;   // n content of leaf residues
            leaf_n = leaf_n - leaf_res_n;              // estimate of N content of leaf

            // move sensced leaf from green pool to sensced pool
            dm_green[leaf] -= leaf_res;
            dm_senesced[leaf]  += leaf_res;


            //  TODO: dbj  need leaf detachment logic to return lead to SurfaceOM
            //  BiomassRemoved (returned to SurfaceOM) called in ozcot_update().
            
            //    for interim, assume senesced leaf is detached immediately
            //    so in ozcot_update 'leaf_res' is moved to the 'dm_shed' pool and passed to SurfaceOM

            


            return; 
        }


        // ====================================================================
        //      subroutine metdat2 (i,iend)
        public void ozcot_metdat2()
        {
            // ====================================================================
            //ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
            //      subroutine to check input climate data for missing or er-   c
            //      roneous values.  input variable units are assumed input     c
            //      and converted as follows:                                   c
            //          temperature -      deg c*10 to deg c                    c
            //          rainfall    -      mm/day*10 to cm/day                  c
            //          rainfall    -      inches/day to cm/day                 c
            //          solrad      -      ly/day (not converted)               c
            //                                                                  c
            //      unique variables:                                           c
            //          qa = an upper boundary for solrad                       c
            //          solrto = needed constant for net radiation in "evap"    c
            //                                                                  c
            //       when temperature (including wet and dry bulb) and          c
            //       radiation are missing or not avaialble, ezstimates are     c
            //       made as function of days since last rain.                  c
            //       consequently a full suite of met data elements can be      c
            //       generated from rainfall alone.                             c
            //                                                                  c
            //ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

            double root;
            double rclear;
            double srad;
            double wet_depress;

            //double f_int;    //fraction of intercepted radiation within row basis
            //double interceptGndAreaBasis;  // intercepted radiation on Ground Area basis

            //- implementation section ----------------------------------

            //         **** read met data  ***************************************

            //       read(1,151,end=153) jdate,imyr,
            //     *  rain,epan,tempmx,tempmn,tempdy,tempwt,wind,solrad
            //151    format(4x,i3,5x,i4,f5.0,f4.1,2f4.0,2f4.1,f4.0,f4.0) ! new


            //psc        solrad = solrad / 0.04186
            //psc        rain_cm = rain_mm / 10.0
            wind = 0.0;
            tempdy = 0.0;
            tempwt = 0.0;
            //psc        epan = 0.


            //jh
            //jh v2001        if(jdate.eq.1) then               ! new year?
            //jh v2001          mdpy = 365                     ! reset days per year
            //jh v2001          if((imyr/4*4).eq.imyr) mdpy=366 ! leap year
            //jh v2001        endif


            //       if(epan.eq.0. .or. epan.eq.99.9)      epan=epanx
            //       epanx=epan

            //       if(wind.eq.0. .or. wind.eq.999)  then
            //           if(windx.ne.999) then
            //               wind=windx
            //           else
            //               wind=0
            //           endif
            //       endif
            //       windx=wind

            //      go to 155
            //153   iend=1 ! flag end of data
            //      return

            //155   continue
            //psc      epan=epan/10.

            //         **** check and convert rainfall (cm) ****

            //        if(rain.lt.0.0) rain=0.0
            //       rain=rain/100.0

            //  local rain data

            //       if(newran.eq.0) go to 100
            //       if(jdate.ge.nrnsdy(1).or.jdate.le.nrnsdy(newran)) rain=0.0
            //        do 10 j=1,newran
            //         if(jdate.eq.nrnsdy(j)) rain=ransub(j)/10.
            //10      continue
            //100    continue

            //  days since rain

            //jh v2001       if(isow.gt.0) then  ! from sowing to first boll
            //jh v2001         if(bollz.eq.0.0) then
            //jh v2001           rrig(rain_preboll) = rrig(rain_preboll)+rain     ! accumulate rainfall before bolling
            //jh v2001         else if(bollz.gt.0.0 .and. openz/(openz+bollz).lt.0.2)
            //jh v2001      :  then ! bolling
            //jh v2001           rrig(rain_postboll) = rrig(rain_postboll)+rain     ! accumulate rainfall in bolling
            //jh v2001         endif
            //jh v2001       else
            //jh v2001           rrig(rain_fallow) = rrig(rain_fallow)+rain     ! accumulate rainfall in fallow
            //jh v2001       endif

            if ((rain_cm <= 0.1))
            {
                if ((nsince < 1)) nsince = 1;
                nsince = nsince + 1;
            }
            else if ((nsince > 1))
            {
                nsince = 1;                       // 1st day of rain period, light rain
                if ((rain_cm > 10.0)) nsince = 0;    // heavy rain
            }
            else
            {
                nsince = 0;                       // rain period
            }
            root = nsince;
            if ((root > 5.0)) root = 5.0;
            if ((root > 0.0)) root = Math.Sqrt(root);
            if ((nsince > 9)) nsince = 9;


            //      **** check solar radiation and fill in for missing data. ****
            //      **** please notice that in the following lines location  ****
            //      **** specific equations are used.    (cal/cm2)           ****

            //  calculate extraterrestrial radiation at nars

            //       xrad=(jdate+9)*(360./365.)*.0174533 ! day of year as angle in radians
            //        qa=749.6+(302.4*sin(1.562+xrad)) ! extra terrestrial radiation(nars)

            //       if(solrad.lt.0.0)solrad=0.0
            //        if(solrad.gt.0.0 .and. solrad.ne.999.) go to 30

            // estimate missing ground measured solar radiation

            //       if(jdate.gt.135)go to 152
            //       qqa=0.66-0.4708*exp(-0.75*nsince)        ! q/qa for days 1-135
            //        if(nsince.le.1)qqa=0.4658-0.003485*rain
            //        go to 160
            //152    continue
            //       if(jdate.ge.225)go to 154
            //       qqa=0.5892-0.7986*exp(-1.219*nsince)     ! q/qa for days 136-225
            //        if(nsince.le.1)qqa=0.1382+0.2777*exp(-0.04375*rain)
            //        go to 160
            //154    continue
            //       qqa=0.63324-0.7693*exp(-1.0*nsince)
            //        if(nsince.le.1)qqa=0.2148+0.2087*exp(-0.01875*rain)
            //160    continue
            //       solrad=qa*qqa           ! est of ground rad =f(days since rain)
            //        solrad_min = qa*0.18      ! minimum - 0.18 from brutsart(1982)
            //       if(solrad.lt.solrad_min)solrad=solrad_min
            //30     continue

            //   actual solar/clear day solar radiation ratio for longwave estimate

            rclear = 551.52 + 246.4 * Math.Sin(0.0172 * (jdate + 99.96));  //clear day sol rad(nars)
            srad = solrad;
            if ((srad > rclear)) srad = rclear;
            solrto = srad / rclear;
            //        solrad=srad

            //       **** check and convert air temperatures (deg c) ****

            //       tempmx=tempmx/10.0
            //       tempmn=tempmn/10.0

            //      if(tempmx.eq.0. .or. tempmx.eq.99.9) then

            //  estimate missing tempmx

            //       tmaxxx=26.24+8.325*sin(1.172+xrad)   ! tmax 8 or more days after rain
            //       ftmax=1.03-0.1812*exp(-0.1953*nsince)! tmax/tmaxxx
            //       tempmx=tmaxxx*ftmax               ! tmax=f(days since rain)
            //       if(rain.gt.4.0)tempmx=tmaxxx*.83
            //       if(rain.gt.5.0)tempmx=tmaxxx*.8

            //      endif

            //      if(tempmn.eq.99.9) then

            //  estimate missing tempmn

            //       tminxx=11.45+8.144*sin(1.078+xrad)             ! tmin on dry days
            //       if(nsince.le.1)tminxx=13.47+5.949*sin(1.+xrad) ! tmin on wetdays
            //       if(nsince.eq.2)ftmin=.993                      ! first day after rain
            //       if(nsince.gt.2)ftmin=.925+.01321*nsince
            //       if(nsince.le.1)ftmin=1.003+.005169*rain-.0001039*rain**2
            //       tempmn=tminxx*ftmin                         ! estimate of tmin

            //      end if

            //       estimate wet and dry bulb when odd day missing

            if ((mvDataFunctions.decimals_are_equal(tempdy, 0.0)) || (mvDataFunctions.decimals_are_equal(tempdy, 99.9)))
            {
                tempdy = -0.54 + 0.57 * tempmx + 0.4 * tempmn;
            }
            if (((mvDataFunctions.decimals_are_equal(tempwt, 0.0)) || (mvDataFunctions.decimals_are_equal(tempwt, 99.9))))
            {
                wet_depress = -3.103 + 0.28 * tempmx - 0.07 * tempmn + 0.62 * root;
                if ((wet_depress < 0.0)) wet_depress = 0.0;
                tempwt = tempdy - wet_depress;
            }

            //          **** calculate soil heat flux (cal/cm2) ****

            //jh v2001         ndate=jdate+183               ! convert to northern hemisphere
            //jh v2001         if(ndate.gt.mdpy) ndate=ndate-mdpy
            //jh v2001         date=real(ndate)
            //jh v2001         g=1.7+14.6*sin(0.0172*(date-51.0))           ! temple,texas
            //jh v2001         if(lt.0.0) g=0.0

            //      call hfunc (i) ! calculate heat units or daydegrees
            ozcot_hfunc();
            //jh v2001       stemp=(tempmx+tempmn)/2.0*asoil

            //      call evap (i) ! calculate potential evaporation
            // ozcot_evap() does not calc eo now as potential evaporation (eo) is now read in daily from SOILWAT
            ozcot_evap();

            return;
        }


        // ====================================================================
       // public void ozcot_n_fertilise(double applied, double availn, double applied_avail)
        public void ozcot_n_fertilise(ref double applied_avail)
        {
            // ====================================================================
            //c
            //c      simulates uptake of fertiliser nitrogen. assumes that there is an upper
            //c      limit to the amount of nitrogen a crop can take up and the rate of uptake
            //c      or recovery of fertiliser n decreases linearly from a maximum initial
            //c      value to zero when uptake limit is reached (basinski et al 1975, cot
            //c      gr rev). assume intial rate is 1.0 (100. recovery) and maximum uptake
            //c      is 240 kg/ha.
            //c

            //data uptakn_max /240.0/
            double uptakn_max = 240.0;
            double rate_reducer;
            double availnx;
            double lfraction;
            int n;
            int nkg;


            //- implementation section ----------------------------------

            nkg = (int)Math.Truncate(snaplc + 0.5);   //  integer of kgs, index for do loop
            rate_reducer = 1.0 / uptakn_max;          //  recovery decreases with uptake
            availnx = availn;                         // available n before application
            for (n = 1; n <= nkg; n++)
            {
                lfraction = Math.Max(0.0, (1.0 - rate_reducer * availn));   //fraction of next kg available
                availn = availn + lfraction;
            }
            applied_avail = availn - availnx;        // n applied now that will be available

            return; 
        }


        // ====================================================================
        public void ozcot_overload()
        {
            // ====================================================================

            //-------------------------------------------------------------------------------
            //     simulates abscission or abortion of older fruit under extreme stress
            //     ie when boll load exceeds carrying capacity.
            //-------------------------------------------------------------------------------

            //use infrastructure
            //implicit none

            double over_c;
            double over_n;
            double fload;
            double capacity;
            double excess;
            double available;
            double abort;
            int cohort;
            int icat;
            int age;


            //----- determine if overload called for c or n ---------------------------------

            over_c = 999.0;            // very large when carcap_c=0.
            if (carcap_c > 0.0) over_c = bload / carcap_c;

            over_n = 999.0;            // very large when carcap_n=0.
            if (carcap_n > 0.0) over_n = bollz / carcap_n;

            if (over_c > over_n)       // cc is limiting
            {
                fload = bload;                 // fruit load
                capacity = carcap_c;           // cpacity
            }
            else                      // n is limiting
            {
                fload = bollz;                // fruit load
                capacity = carcap_n;          // cpacity
            }

            //----- count days fruit load exceeds capacity ---------------------------------

            if (iday - last_iday > 1) idayco = 0;            // reset for break
            if (fload > capacity) idayco = idayco + 1;       // count days fload>carcap
            last_iday = iday;                                // last day counted
            if (idayco < 3) return;                          // buffered by reserves


            //----- compute excess fruit and buffer effect ---------------------------------

            excess = fload - capacity;            // bolls in excess of cc supply
            excess = excess * 0.1;                // damp effect for carbon stress

            //----- loop through arrays to find available fruit ----------------------------

            for (cohort = lfru[small_bolls]; cohort >= (lfru[inedible_bolls] - 1); cohort += -1)   // loop through categories 5, 6, 7
            {
                if (cohort < 1) break;                // no fruit to abort
                icat = flowers;
                if (cohort <= lfru[small_bolls])  icat = small_bolls;
                if (cohort <= lfru[medium_bolls]) icat = medium_bolls;
                if (cohort <= lfru[large_bolls])  icat = large_bolls;
                if (fruno[cohort] == 0.0) continue;   // no fruit, skip to next loop iteration

                available = fruno[cohort];            // fruit available to abort
                for (age = age1; age <= 6; age++)
                {
                    available = available - frmark[cohort, age];   // less fruit marked
                }

                if (icat == large_bolls & fruwt[cohort] / fruno[cohort] < 0.1)   // fruit not grown yet ?
                {
                    frmark[cohort, age6] = fruno[cohort];                        // abort such fruit
                    available = available - frmark[cohort, age6];                // adjust fruit available
                }

                if (available > 0.0)
                {
                    available = available * 0.1;                            // damp effect
                    abort = available;                                      // abort available fruit
                    if (abort > excess) abort = excess;                     // limit to requirement
                    frmark[cohort, age6] = frmark[cohort, age6] + abort;
                    fmkcat[icat, age6] = fmkcat[icat, age6] + abort;
                    excess = excess - abort;                                // reduce excess no. bolls
                    if (excess <= 0.0) break;                               // excess depleted
                }
            }

            return; 
        }


        // ====================================================================
        public double ozcot_satvp(double tdeg)
        {
            double satVP = 0.0;
            // ====================================================================

            double tabs;
            double tr;
            double trlog;
            double ts;
            double tt;
            double ewslog;
            double ew;

            //- implementation section ----------------------------------

            tabs = tdeg + 273.16;
            tr = 373.16 / tabs;
            trlog = Math.Log10(tr);
            tr = tr - 1.0;
            ts = (Math.Pow(10.0, (11.344 * (1.0 - tabs / 373.16))) - 1.0) / Math.Pow(10.0, 7.0);
            tt = (Math.Pow(10.0, (-3.49149 * tr)) - 1.0) / Math.Pow(10.0, 3.0);
            ewslog = Math.Log10(1013.246);
            ew = -7.90298 * tr + 5.02808 * trlog - 1.3816 * ts + 8.1328 * tt + ewslog;
            satVP = Math.Pow(10.0, ew);

            return satVP;
        }




        // ====================================================================
        public double ozcot_senlf()
        {
            double leafLifeDD = 0.0;
            // ====================================================================

            //     estimates leaf longevity. ranges between 833 dd & 1110 dd
            //     reduced by water stress, nitrogen stress, boll load and self shading of
            //     canopy when lai gt 3.

            double fb;
            double fw;
            double fn;
            double fl;
            double f1, f2;

            //- implementation section ----------------------------------

            fb = 1.0;            // local boll load
            fw = 1.0;            // water stress factor
            fn = 1.0;            // nitrogen stress factor
            fl = 1.0;            // lai  factor
            if ((carcap_c > 0.0)) fb = 1.0 - bload / carcap_c;      // reduce by boll load
            if ((fb > 1.0)) fb = 1.0;
            if ((fb < 0.0)) fb = 0.0;
            fw = ozcot_stress(fw_low, fw_high, fw_a, smi);          // effect of water stress

            leafLifeDD = 833.0 + 277.0 * fb * fw * fn;

            //     proposal 3/1/97 try when convenient abh

            fn = ozcot_stress(0.75, 1.0, 1.0, fnstrs);            // nitrogen stress 30 dec 1992
            
            //jh skip row correction ??  TODO:  check for skip row correction
            laiRow = laiField;
            //      if(nskigt.0)laiRow = laiField*effectiveRowSpacing        ! lai in hedgerow

            if ((laiRow > 3.0)) fl = 1.0 - (laiRow - 3.0) / 3.0;    // effect of lai > 3
            if ((fl > 1.0)) fl = 1.0;
            if ((fl < 0.0)) fl = 0.0;

            f1 = Math.Min((fb * fw), fn);
            f2 = Math.Min(f1, fl);
            //      senlf=833.+277.*f2

            // TODO:  calcs on fl are not used.  Check validity

            return leafLifeDD;

        }


        // ====================================================================
        //jh      subroutine snbal(i)
        public void ozcot_snbal()
		{
			// ====================================================================

			//!      this subroutine estimates potential total n uptake of the crop (availn).
			//!      it does not maintain a daily available n balance; this will be done in
			//!      a later version.
			//!      fertiliser and non-fertiliser n is entered as input. non-fertiliser n
			//!      is immediately added to availn. a fraction of fertiliser n is added on
			//!      day of application in s/r n_fertilise, the fraction being a function of
			//!      n already available. all available n is reduced by waterloggin
			//!      fertiliser n is reduced by low available soil water content is.
			//!      day of application of n fertiliser, nday(j) from cinput2 & agron.inp,
			//!      can be day of year (+ve) or days after sowing (-ve), assigned to local
			//!      variable jnaplc(j) on first day of season or day of sowin
			double applied_avail;
			double reduction;


			//- implementation section ----------------------------------

			//!---- reset variables for new season ------------------------------------------
			if ((DAS == 1))              // start of new season?
			{
				applied_n = 0.0;	    // reset for new season
				total_applied = 0.0;	//       ditto

				//jh          do j=1,nfert
				//jh             jnaplc(j) = 0                      ! reset
				//jh             if(nday(j).gt.0) jnaplc(j)=nday(j) ! day of applcn as of year
				//jh          enddo
			}

			//jhc---- find day of n application if tied to sowing date ------------------------

			//jh      if(i.eq.isow) then                        ! sown this day?
			//jh         jsow = isow+imet-1                     ! sowing as day of year
			//jh         if(jsow.gt.mdpy) jsow=jsow-mdpy        ! jsow next year? if so adjust
			//jh         do j=1,nfert
			//jh             if(nday(j).lt.0 .and. snaplc(j).gt.0.) then
			//jh                 nafter = -nday(j)              ! make +ve
			//jh                 jnaplc(j) = jsow+nafter        ! day of applcn as of year
			//jh                 if(jnaplc(j).gt.mdpy) jnaplc(j)=jnaplc(j)-mdpy ! next year?
			//jh             endif
			//jh         enddo
			//jh      endif

			//!--- update available n if fertiliser applied ---------------------------------

			//jh      if(jdate.eq.jnaplc(j)) then               ! apply n this day?
			if ((snaplc > 0.0))
			{
                applied_avail = 0.0;
                          // ozcot_n_fertilise(snaplc, availn, applied_avail);	// if so, do it
                ozcot_n_fertilise(ref applied_avail);	                // if so, do it
                total_applied = total_applied + snaplc;				// total n applied
				applied_n = applied_n + applied_avail;				// total applied n avail
			}

            //!--- adjust available n during active fruiting after hearn & constable 1984-----
			if ((squarz > 0.0 & openz == 0.0))        // active fruiting?

			{
				//!        if(def.lt.2.5) then                          ! waterlogging
				//jh        if(sw/ul.gt.watlog_n) then                    ! waterlogging 28/5/96

				if ((Math.Round(wli,6) > watlog_n))               // waterlogging 28/5/96
				{
					if ((availn > 30.0))
					{
						availn = availn - 0.983;   // hearn & constable
					}
					else
					{
						availn = availn * 0.99;    // hearn constable
					}
				}

				if ((smi < 0.3 & applied_n > 0.0 & bollz > 0.0))   // dry?
				{
					reduction = 0.0316 * total_applied;			// reduces avail n
					applied_n = applied_n - reduction;			// reduce applied n avail
					if ((applied_n < 0.0)) applied_n = 0.0;
					availn = availn - reduction;				// reduce available n
					if ((availn < 0.0)) availn = 0.0; 
				}
			}

			return; 
		}



        // ====================================================================
        //jh      subroutine solwat (i)
        public void ozcot_solwat()
        {
            // ====================================================================

            //ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
            //      calculates the soil water balance as a function of soil     c
            //      layer depth.  modified from the model of ritchie (1972).    c
            //                                                                  c
            //     order of execution - numbers are original order:             c
            //          initialise in 1st season                                c
            //          reset intial sw if specified                            c
            //        1  call cultivate                                         c
            //        4  call sevap    gives es                                 c
            //        5  call evapotrans uses smi, gives ep & et                c
            //        7  call swbal_et   removes ep & es from layers            c
            //        8  call sum_sw - sum layers as sw gives def               c
            //        2  call irrigate uses def adds irrig water to rainfall    c
            //        3  call runoff   uses sw, gives q                         c
            //        6  call swbal_add  adds rain & irrigation to layers       c
            //        8  call sum_sw - sum layers as sw gives def               c
            //        9  call water_info information for output                 c
            //       10   call indices  smi & wli                               c
            //       11   call drainage                                         c
            //       12   call skipwater                                        c
            //                                                                  c
            //ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

            //-------initialising -----------------------------------------------------------
            if ((isw != 1))      // replaces initial > 1 for ozcot2&4 may89
            {
                isw = 1;
                smi = swlayr[1] / ullayr[1];
            }


            ozcot_evapotransp();                   // uses smi, gives ep & et
            ozcot_swbal_et();                      // removes ep & es from layers
            ozcot_sum_sw();                        // sums sw and gives def
            ozcot_indices();                       // gives smi & wli
            if (skiprow > 0) ozcot_skipwater();    // for skip row
            
            return; 
        }


        // ====================================================================
        public void ozcot_skipwater()
        {
            // ====================================================================

            //     this subroutine deals with use of soil water in the skip row.
            //     at the start of a drying cycle water is drawn only from the plant row,
            //     and smi calculated for the plant row, until the supply is limiting ep
            //     (shown by f_limiting < 1, from watco).
            //     with supply limiting water is drawn from skip until it is depleted
            //     to same level as plant row (shown when smi for plant+skip =<
            //     smi for plant row) where upon water is drawn from plant and skip rows.
            //     until smi for plant+skip < smi for plant row, smi for plant row is
            //     effective smi. when profile rewetted and drying cycle ends, procedure
            //     starts again with water drawn from plant row until supply limited.
            //     wli (waterlogging index) adjusted for plant row,
            //     assuming water logging is limited to plant row.


            //jh      logical useskip/.false./          ! water used from skip when true

            //jh      smi_in = smi                      ! to write o/p in development

            if ((!useskip))
            {
                smi_row = 1 + (smi - 1) * effectiveRowSpacing;      // smi in plant row
                if ((f_limiting < 1.0))            // supply limiting ep?
                {
                    useskip = true;                // yes, water from skip
                    smi_pre = smi;                 // save value of smi
                    //    f_smi = smi_row/smi      ! to adjust smi when water in skip used
                }
                smi = smi_row;                     // effective smi
            }
            else
            {
                if ((smi > smi_pre))              // profile rewetted?
                {
                    useskip = false;                // yes, rewetting occurs
                    smi_row = 1 + (smi - 1) * effectiveRowSpacing;   // smi in plant row
                    smi = smi_row;                  // effective smi
                }
                else
                {
                    smi = Math.Min(smi, smi_row);   // no, drying continues
                    //    smi = smi*f_smi            ! adjust smi when using water in skip
                }
            }

            wli = 1 - effectiveRowSpacing * (1 - wli);              // water logging index in row

            //      write (4,10) smi_in,smi,f_limiting,smi_pre,f_smi,useskip
            //format(5f5.2,l5)

            return; 
        }


        // ====================================================================
        public void ozcot_evapotransp()
		{
			// ====================================================================

			//     calculates ep, adds to es, to get et, limits to eo    (all in cm!!)

            // ====================================================================

            // original code replaced by call to new routine ozcot_fract_interception(skiprow, alia, effectiveRowSpacing)
            //  which is used as a common routine in both ozcot_evap() and ozcot_evapotransp() so as to 
            //  calculate ep and es on the same basis.
            //  (dbj April 2015)

            //================================================================
            ////sv- this is not what is written in the Hearn paper (operator is around the wrong way in: if (alai_row > 1.6))
            ////Hearn, a. B. 1994. OZCOT: A simulation model for cotton crop management. Agric. Syst. 44(3): 257–299 Available at http://linkinghub.elsevier.com/retrieve/pii/0308521X94902233.
            ////but it has been determined that what is written in the paper is wrong and the code below is correct.
            ////The two reasons for greater realism of the code below is that it satisfies the following two conditions and the paper does not,
            //// ep=0 at LAI=0, (no leaves, no plant evaporation) and, by LAI>3, es should be approaching zero (full canopy means no soil evaporation).

            //****************************************************************  DBJ debug 21/5/15
                    //test_alai_row = laiField;
                    //if (skiprow > 0) test_alai_row = laiField * effectiveRowSpacing; 	// lai in hedgerow - abh 5/11/96

                    //if (test_alai_row > 3.0)
                    //{
                    //    test_ep = eo - es;
                    //}
                    //else
                    //    if (test_alai_row > 1.6)
                    //    {
                    //        test_ep = (0.08 + 0.3066 * test_alai_row) * eo;	// l.mateos 1987, abh 1988
                    //    }
                    //    else
                    //    {
                    //        test_ep = (1.0 - Math.Exp(-0.5186 * test_alai_row)) * eo;	// l.mateos 1987, abh 1988
                    //    }

                    //if (test_alai_row == 0.0) test_ep = 0.0;
                    //if (test_ep < 0.0) test_ep = 0.0;

                    //test_ep_int = test_ep / eo;
            //================================================================
            //****************************************************************  DBJ debug 21/5/15
            //================================================================

            ep = ozcot_fract_interception(skiprow, laiField, effectiveRowSpacing) * eo;
			
            if (skiprow > 0)
			{
				ep = ep / effectiveRowSpacing;				       // ep on ground area basis
			}

			//------ limit ep using watco(smi) stress factor --------------------------------

			et = es + ep;
			if (eo < et)
			{
				et = eo;
				ep = et - es;
				if (ep < 0.0) ep = 0.0; 
			}
			f_limiting = ozcot_watco(smi, eo, 0.4, 0.0);  // factor when supply limiting ep
			ep = ep * f_limiting;

            // what if ep exceeds available water from rooting layers?



			et = es + ep;


			return; 
		}


        // ====================================================================
        public void ozcot_indices()
        {
            // ====================================================================

            //     calculates smi and wli

            // ====================================================================

            double depth;
            double rtul;
            int idxLayer;
            double smi_rt;
            double smi_30;

            depth = 0.0;
            rtsw = 0.0;
            rtul = 0.0;
            nrtlayr = 0;

            for (idxLayer = 1; idxLayer <= nlayr; idxLayer++)
            {
                depth = depth + dlayr_cm[idxLayer];
                nrtlayr = idxLayer;
                if (rtdep <= depth)
                {
                    rtsw = rtsw + swlayr[idxLayer] * (rtdep + dlayr_cm[idxLayer] - depth);
                    rtul = rtul + ullayr[idxLayer] * (rtdep + dlayr_cm[idxLayer] - depth);
                    break;
                }

                rtul = rtul + ullayr[idxLayer] * dlayr_cm[idxLayer];
                rtsw = rtsw + swlayr[idxLayer] * dlayr_cm[idxLayer];
            }


            smi_rt = rtsw / rtul;                 // smi in root zone
            smi_30 = swlayr[1] / ullayr[1];       // smi in top 30cm
            if (dlayr_cm[1] < 30.0)
            {
                smi_30 = (smi_30 * dlayr_cm[1] + (swlayr[2] / ullayr[2]) * (30.0 - dlayr_cm[1])) / 30.0;
            }

            
            smi = Math.Max(smi_rt, smi_30);

            smi = Math.Min(1.0, smi);
            //jh      print*, smi_rt, rtdep
            //if (DAS < 20)  Console.WriteLine("\n smi = {0}, smi_rt = {1}, smi_30 = {2}, rtdep = {3} ", smi, smi_rt, smi_30, rtdep) ;

            wli = smi;

            // correction of indices for soilwat2 water movements
            //jhtemp        smi = -0.107 + 1.187*smi
            //jhtemp        smi = max(0.0,min(1.0,smi))                        ! waterlogging index
            //jhtemp        wli = -0.107 + 1.187*wli                         ! waterlogging index
            //jhtemp        wli = max(0.0,min(1.0,wli))                        ! waterlogging index

            return; 
        }

        // ====================================================================
        public void ozcot_sum_sw()
        {
            // ====================================================================

            //     calculates sw in each layer & sums down profile

            // ====================================================================

            int l;

            sw = 0.0;

            for (l = 1; l <= nlayr; l++)
            {
                sw = sw + swlayr[l] * dlayr_cm[l];
            }

            //jh v2001        def = ul-sw

            return; 
        }




        // ====================================================================
        public double ozcot_stress(double low, double high, double a, double strs)
        {
            double stressAdjustor;
            double stressFactor = 0.0;
            // ====================================================================

            //       computes or adjusts a factor.
            //       input is state variable strs with upper and lower limits, high,low.
            //       output is between 0 and 1.
            //       a =  1 gives factor a linear fn of ratio of strs to high - low
            //       a gt 1 accentuates effect of strs on value
            //       a lt 1 damps effect.


            //- implementation section ----------------------------------

            stressAdjustor = (strs - low) / (high - low);
            if ((stressAdjustor > 1.0))
            {
                stressFactor = 1.0;
            }
            else if ((stressAdjustor <= 0.0))
            {
                stressFactor = 0.0;
            }
            else
            {
                stressFactor = (1.0 - Math.Pow((1.0 - stressAdjustor), a));
            }
            return stressFactor;
        }


        // ====================================================================
        public double ozcot_survive()
        {
            double FruitSurviving = 0.0;
            // ====================================================================
            double a;
            double b;

            //     estimates survival of fruit as function of boll load.

            //- implementation section ----------------------------------

            FruitSurviving = 0.0;
            if (carcap_c == 0.0)
            {
                return FruitSurviving;
            }

            a = 1.0;                   // intercept of survival function 
            b = a / carcap_c;          // slope of survival function

            FruitSurviving = a - b * bload;        // proportion surviving

            if ((FruitSurviving < 0.0)) FruitSurviving = 0.0;
            if ((FruitSurviving > 1.0)) FruitSurviving = 1.0;

            if (background_retention > 0.0)   // background, sub-threshold shedding,  externalised value
            {
                FruitSurviving = FruitSurviving * background_retention;
            }
            else
            {
                FruitSurviving = FruitSurviving * 0.8;   // default value
            }

            return FruitSurviving;
        }


        // ====================================================================
        //       subroutine swbal(i,rainef)
        public void ozcot_swbal_et()
        {
            // ====================================================================


            //   **** soil & plant water balance including rain and soil evaporation, ****
            //   **** beginning with the top soil layer.                      ****


            double epcoef;        // exponential coefficient to apply to ep
            double uob;           // adjusted value of ep based on exponential eqn
            double depth;         // total depth extraction calculated to
            double tdepth;        // depth to top of current extraction layer
            double swlr;          // soil water in layer
            double ux;            // total sw extracted so far 
            double sum;           // total demand calculated to depth (loop increments by layer)
            double dswmx;         // demand for layer
            double epd;           // error value if demand is not extracted (neg value)
            double swRoot;        // sw in rooting layers (after initial extraction)
            double fracMissing;   // fraction of sw still to be extracted at end of main extraction loop
            double part_dlayer;   // partial layer depth
            double swlRoot;       // sw of partially rooted layer
            int l;
            int nRootLayers;

            //- implementation section ----------------------------------


            //       percol=rainef
            //       exes=es

              //    IF(swrSoilMoistIdx.GE.0.5)lrEPCoeff=3.051       !  lrW*N
              //    IF(swrSoilMoistIdx.LT.0.5)lrEPCoeff=2.436       !  82/83


            ux = 0.0;
            if (smi >= epcoef_smi_crit)
            {
                epcoef = epcoef1;      //  w*n   (water by nitrogen trials)
            }
            else
            {
                epcoef = epcoef2;       //(smi < epcoef_smi_crit))  82/83   
            }
            uob = ep / (1.0 - Math.Exp(-epcoef));
            sum = 0.0;
            depth = 0.0;
            tdepth = 0.0;
            epd = 0.0;

            //***********************************************************************

            for (l = 1; l <= nlayr; l++)
            {
                swlr = swlayr[l] * dlayr_cm[l];

                //**** extract ep from this layer
                depth = depth + dlayr_cm[l];              // moved here 29/6/88

                if ((DAS < iemrg)) depth = 0.0;           // crop emerged?
                if (rtdep > tdepth)                      
                {
                    if ((rtdep < depth)) depth = rtdep;   
                    sum = uob * (1.0 - Math.Exp(-epcoef * depth / rtdep));
                    dswmx = sum - ux;
                    swlr = swlr - dswmx;                     // extract ep from this layer
                    if ((swlr < 0.0))
                    {
                        epd = swlr;
                        swlr = 0.0;
                    }

                    tdepth = depth;
                    ux = sum + epd;     // epd corrects ux if layer is dry
                    epd = 0.0;
                }
           
                swlayr[l] = swlr / dlayr_cm[l];
            }

            // test if all of ep has been extracted  30/7/15  
            dswmx = sum - ux;
            if (dswmx > 0.0)
            {   // unextracted demand remains
                // extract it proportionally from rooting layers that have water
                swRoot = 0.0;
                nRootLayers = 0;

                for (l = 1; l <= nlayr; l++)
                {
                    if (rtdep > depth)
                    {
                        nRootLayers = l;

                        if (rtdep > (depth + dlayr_cm[l]))   // roots through full layer
                        {
                            swlr = swlayr[l] * dlayr_cm[l];
                            swRoot = swRoot + swlr;
                            depth = depth + dlayr_cm[l];
                        }
                        else                                 // roots partially through layer
                        {
                            part_dlayer = (rtdep - depth);
                            swlr = swlayr[l] * part_dlayer;
                            swRoot = swRoot + swlr;
                            depth = depth + part_dlayer;
                        }
                    }
                }

                fracMissing = dswmx / swRoot;               // deficit as fraction of soil water (paw) in root zone

                for (l = 1; l <= nRootLayers; l++)
                {
                    if (rtdep > (depth + dlayr_cm[l]))      // roots through full layer
                    {
                        swlayr[l] = swlayr[l] * (1 - fracMissing);
                        depth = depth + dlayr_cm[l];
                    }
                    else                                    // only part way through layer
                    {
                        part_dlayer = (rtdep - depth);              // root depth in layer
                        swlr = swlayr[l] * dlayr_cm[l];             // soil water of full layer
                        swlRoot = swlayr[l] * part_dlayer;          // soil water of rooted depth of layer
                        swlr = swlr - (swlRoot * fracMissing);      // adjusted soil water of full layer 
                        swlayr[l] = swlr * dlayr_cm[l];             // updated swlayr[] array
                        depth = depth + dlayr_cm[l];
                    }
                }  //end of removal of extra sw
            } //end of block for ep deficit detected

            return; 
        }


        //ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
        //      water stress function for root growth.                      c
        //ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

        // ====================================================================
        public double ozcot_watco(double smi, double eo, double x3, double x1)
        {
            double functionReturnValue = 0.0;
            // ====================================================================
            //ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
            //      water stress function for root growth.                      c
            //ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

            double x2;
            double y0;
            double slope;

            //- implementation section ----------------------------------

            x2 = x3 * eo / 0.8;
            if (x2 > 1.0) x2 = 1.0;
            if (smi < x2)
            {
                slope = 1.0 / (x2 - x1);
                y0 = 1.0 - slope * x2;
                functionReturnValue = slope * smi + y0;
                if (functionReturnValue < 0.0) functionReturnValue = 0.0;
            }
            else
            {
                functionReturnValue = 1.0;
            }
            return functionReturnValue;
        }


        // ====================================================================
        public void ozcot_yield()
        {
            // ====================================================================

            //     estimates yield at end of season

            //     calculate yield **********************************************************

            alint = dm_openBolls * 10.0 * pcLint_frac;              // g sc/m to kg lint/ha
            //psc      alint = alint/effectiveRowSpacing                   ! adjust for row spacing 2/5/90

            //      plntz = uptakn                      // nitrogen uptake

            ozcot_residues();                           // to calculate stem and root residues

            return; 


        }



        // ====================================================================
        public void ozcot_plant_n()
        {
            // ====================================================================
            //     call from pltgrw before ozcot_cropn
            //     calculates nitrogen content of dry matter increments and sums them
            //     adjusts n increments as soil n supply diminishes
            //     variable ddm_leaf etc from s/r dry_matter
            //              supply_n from system
            //               dn_plant = daily increment of plant n to system


            double supply_n = 1.0;
            double conc_l = 0.04;
            double conc_s = 0.02;
            double conc_r = 0.02;
            double conc_b = 0.015;
            double dn_leaf;
            double dn_stem;
            double dn_root;
            double dn_boll;
            double sup_dem;
            double adjust;
            double total_n_pot;
            double ldn_plant;
            double uptakn_max;



            //- implementation section ----------------------------------

            //jhadded
            supply_n = Math.Min(2.0, Math.Min(uptakn / 10.0 - total_n, tsno3 / 10.0) / 5.0);
            // max uptake of 2 g/m2. 5 days to takeup avail n.
            //      supply_n = min(1.0, uptakn/240.0)   ! max uptake of 240 kg/ha specified in n_fertilise

            //      calculate daily increment for components of dry matter

            dn_leaf = ddm_leaf * conc_l;            // leaf nitrogen
            dn_stem = ddm_stem * conc_s;            // stem nitrogen
            dn_root = ddm_root * conc_r;            // root nitrogen
            dn_boll = ddm_boll * conc_b;            // boll nitrogen

            dn_plant = dn_leaf + dn_stem + dn_root + dn_boll;      // plant n increment

            //      adjust uptake when soil supply limiting

            if ((dn_plant > 0.0))
            {
                sup_dem = supply_n / dn_plant;      // supply/demand ratio
            }
            else
            {
                sup_dem = 0.0;
            }
            adjust = ozcot_stress(adjust_low, adjust_high, adjust_a, sup_dem);    // factor to adjust
            if ((adjust < 1.0))
            {
                dn_leaf = dn_leaf * adjust;         // leaf nitrogen adjusted
                dn_stem = dn_stem * adjust;         // stem nitrogen adjusted
                dn_root = dn_root * adjust;         // root nitrogen adjusted
                dn_boll = dn_boll * adjust;         // boll nitrogen adjusted
                dn_plant = dn_leaf + dn_stem + dn_root + dn_boll;      // plant n increment
            }

            total_n_pot = total_n + dn_plant;            // accumulate uptake for season
            uptakn_max = Math.Min(c_uptakn_max, uptakn);
            //     compare accumulated uptake with projected uptake for season
            //     if accumulated exceeds projected, assume requirements met by remobilisation
            //     and set this day's increments to zero

            //jh      if(total_n.ge.uptakn/10.) then
            //jh          total_n =total_n - dn_plant  ! adjust uptake for season
            //jh          dn_leaf = 0.0                ! leaf nitrogen adjusted
            //jh          dn_stem = 0.0                ! stem nitrogen adjusted
            //jh          dn_root = 0.0                ! root nitrogen adjusted
            //jh          dn_boll = 0.0                ! boll nitrogen adjusted
            //jh          dn_plant = 0.0               ! plant n increment
            if ((total_n_pot > uptakn_max / 10.0))
            {
                ldn_plant = Math.Max(0.0, uptakn_max / 10.0 - total_n);    // plant n increment
                adjust = mvDataFunctions.divide_check(ldn_plant, dn_plant, 0.0);
                dn_leaf = dn_leaf * adjust;                // leaf nitrogen adjusted
                dn_stem = dn_stem * adjust;                // stem nitrogen adjusted
                dn_root = dn_root * adjust;                // root nitrogen adjusted
                dn_boll = dn_boll * adjust;                // boll nitrogen adjusted
                dn_plant = ldn_plant;                       // plant n increment
            }

            //      if (DAS .gt. 100
            //     :    .and. dn_plant.lt. 0.0001
            //     :    .and. total_n .lt. uptakn/10.0)then
            //         dn_plant = uptakn/10.0 - total_n
            //      if ((iend.eq.6 .or. iend.eq.10)
            //     :    .and. total_n .lt. uptakn_max/10.0)then
            //         dn_plant = uptakn_max/10.0 - total_n
            //      else
            //      endif
            //      print*,  iend, total_n, uptakn_max/10.0

            total_n = total_n + dn_plant;            // adjust uptake for season

            boll_n = boll_n + dn_boll;
            leaf_n = leaf_n + dn_leaf;
            stem_n = stem_n + dn_stem;
            root_n = root_n + dn_root;


            //      write(4,222) iday,supply_n,ldn_plant,total_n,uptakn,dm_total
            //222   format(i5,5f8.3)


            return; 
        }

        // ====================================================================
        public void ozcot_residues()
        {
            // ====================================================================

            //      called from s/r yield to calculate stem and root residues

            double conc_res = 0.005;

            //data conc_res /0.005/           ' n concentration of residues

            //- implementation section ----------------------------------

            stem_res = dm_stem;                // stem residues dry matter
            stem_res_n = dm_stem * conc_res;   // n content of stem residues
            root_res = dm_root;                // root residues dry matter
            root_res_n = dm_root * conc_res;   // n content of root residues

            return; 
        }



        // ====================================================================
        //     subroutine dry_matter (i)
        public void ozcot_dryxmatter()
        {
            // ====================================================================

            //     first demand for assimilate is estimated for leaf, stem, root and boll.
            //     leaf demand is determined from potential increase in area. water stress
            //     may reduce actual area increase, thus giving thicker leaves. inadequate
            //     assimilate supply may reduce actal weight increase giving thinner leaves.
            //     upper and lower limits are set by leaf weight:area ratio. if the upper
            //     limit is exceeded, weight increase is reduced. if lower limit is not
            //     reached, area increase is reduced. stem and root demand are determined
            //     from potential leaf demand using allometric relationships.

            //     calls s/r assimilation to compute assimilate production for the day.
            //     the supply available to meet demand is obtained from sum of the days
            //     assimilate production and any reserves.

            //     supply and demand are then compared in order to partition dry matter
            //     increase between leaf, stem, boll and root.
            //     if supply exceeds demand, up to two days supply can be stored as reserves.
            //     if demand exceeds supply, distribution is simulated on the basis
            //     of proximity to the source of supply. leaf, stem and fruit are assumed to
            //     be equidistant from the source. if supply exceeds the sum of leaf, stem
            //     and fruit demand, their needs are met in full and the balance goes to
            //     root. if the sum of leaf, stem and fruit demand exceeds supply,
            //     their needs are met in proportion to the supply/demand ratio and the
            //     root receives none. the root supply:demand ratio or a decline in root
            //     growth provide feedback to reduce increase in root depth in s/r pltgrw.

            //     local variables:
            //       assimilate new dry matter passed daily from s/r assimilation  (g/m2)
            //       ddm_boll   day's increase in boll dry weight
            //       ddm_leaf   day's increase in leaf dry weight
            //       ddm_stem   day's increase in stem dry weight
            //       ddm_root   day's increase in root dry weight
            //       ddm_root_max   maximum value of increase in root dry weight
            //       demand     demand for assimilate for potential day's growth
            //       fnstrs2    n stress for boll growth - on/off
            //       fwstrs     water stress for boll growth
            //       sd_ratio   supply:demand ratio for leaf, stem and boll growth
            //       sd_root    supply:demand ratio for root growth
            //       strsbl     stress factor for boll growth, minimum of n and water
            //       supply     day's supply of assimilate available for potential growth
            //       wt_area    leaf weight:area ratio



            double wt_area;
            double fwstrs;
            double fnstrs2;
            double strsbl;
           // double assimilate;
            double supply;
            double demand;
            double sd_ratio;
            double sd_root;
            //      real ddm_root_max


            //- implementation section ----------------------------------

            //------------------------------------------------------------------------------
            //     initialise leaf, stem and root dry matter at start of new crop
            //------------------------------------------------------------------------------

            if ((dm_leaf == 0.0))      // leaf weight initialise to zero?
            {
                dm_leaf = embryo * f_leaf * ppm2;           // initial leaf dry weight per m
                dm_stem = embryo * f_stem * ppm2;           // initial stem dry weight per m
                dm_root = embryo * f_root * ppm2;           // initial root dry weight per m
            }

            //------------------------------------------------------------------------------
            //     calculate demand (potential growth) for leaf, stem and root
            //------------------------------------------------------------------------------

            ddm_leaf = dlai_pot * specific_lw;                                    // leaf demand
            ddm_stem = mvDataFunctions.divide_check(a_stem_leaf * ddm_leaf * dm_stem, dm_leaf, 0.0);    // ditto for stem
            ddm_root = mvDataFunctions.divide_check(a_root_leaf * ddm_leaf * dm_root, dm_leaf, 0.0);    // ditto for root

            //------------------------------------------------------------------------------
            //     feed back of leaf weight/area ratio
            //------------------------------------------------------------------------------

            if (dlai[iday] > 0.0)    // leaf growth today?
            {
                wt_area = mvDataFunctions.divide_check(ddm_leaf, dlai[iday], 0.0);   // leaf weight/area ratio
                if ((wt_area > wt_area_max))                   // too thick
                {
                    ddm_leaf = dlai[iday] * wt_area_max;      // reduce weight
                    //          else if(wt_area.lt.wt_area_min) then            ! too thin
                    //              dlai(iday) = ddm_leaf/wt_area_min           ! reduce area
                }
            }

            //------------------------------------------------------------------------------
            //     calculate demand for bolls
            //------------------------------------------------------------------------------

            //      if(isq.gt.0 .and. i.ge.isq+2) then        ! fruit called yet?
            if ((isq > 0 & DAS >= isq + 2))             // fruit called yet? 
            {
                fwstrs = ozcot_stress(fwstrs_low, fwstrs_high, fwstrs_a, smi);  // water stress on bolls 
                fnstrs2 = 1.0;                                                  // n stress for bolls off
                if ((fnstrs == 0.0)) fnstrs2 = 0.0;                             // n stress for bolls on
                strsbl = Math.Min(fwstrs, fnstrs2);                                // minimum of water or n stress
                bollgr = scboll * bper * fburr;                                 // boll gr rate this day
                bollgr = bollgr * strsbl;                                       // adjust for stress
                if ((bollgr < 0.0)) bollgr = 0.0;
                ddm_boll = bollz * bollgr;                                      // boll demand - potential growth

            }

            //------------------------------------------------------------------------------
            //   determine supply of assimilate
            //------------------------------------------------------------------------------
            assimilate = 0.0;
            ozcot_assimilation();            // day's assimilate
            total_assimilate += assimilate;  // accumulate total assimilate for the crop (g/m2)
            supply = assimilate + reserve;             // compute supply
            reserve = 0.0;                             // reserve used

            //------------------------------------------------------------------------------
            //   compare total demand with supply to partition assimilate
            //------------------------------------------------------------------------------

            demand = ddm_leaf + ddm_boll + ddm_stem + ddm_root;   // compute total demand

            if (supply >= demand)         // demand met, potential growth achieved
            {
                reserve = reserve + supply - demand;              // excess becomes reserve
                if (reserve > res_cap) reserve = res_cap;       // limit to reserve
                sd_ratio = 1.0;                                   // supply,demand ratio for leaf,stem,boll
                sd_root = 1.0;                                    // supply,demand for root
            }
            else                           // demand not met, reduce potential growth
            {
                demand = demand - ddm_root;                       // demand for leaf, stem and fruit
                if (supply >= demand)        // their potential growth achieved
                {
                    sd_ratio = 1.0;                                      // supply,demand ratio for leaf,stem,boll          'const
                    sd_root = mvDataFunctions.divide_check((supply - demand), ddm_root, 0.0);  // supply,demand for root
                    ddm_root = supply - demand;                          // rest to root
                }
                else                          // leaf, stem and fruit demand not met
                {
                    sd_ratio = mvDataFunctions.divide_check(supply, demand, 0.0);            // supply,demand ratio
                    ddm_leaf = ddm_leaf * sd_ratio;                    // actual leaf growth
                    ddm_boll = ddm_boll * sd_ratio;                    // actual fruit growth
                    ddm_stem = ddm_stem * sd_ratio;                    // actual stem growth
                    ddm_root = 0.0;                                    // no root growth
                    sd_root = 0.0;                                     // supply,demand for root
                    bollgr = bollgr * sd_ratio;                        // adjust boll growth rate
                }
            }

            //------------------------------------------------------------------------------
            //     grow crop by updating dry weights for leaf, stem, bolls and roots
            //------------------------------------------------------------------------------

            dm_leaf = dm_leaf + ddm_leaf;                        // total leaf dry weight
            dm_stem = dm_stem + ddm_stem;                        // total stem dry weight
            //dm_allBolls = dm_allBolls + ddm_boll;              // total boll dry weight (based on dryxmatter() accumulation)
            dm_allBolls = dm_allBolls + dm_fruWtToday;           // total boll dry weight (based on fruit() wt accumulation)
            dm_total = dm_leaf + dm_stem + dm_allBolls ;         // total dry weight above ground (roots excluded)

            dm_root = dm_root + ddm_root;                        // total root dry weight

            ddm_l[iday] = ddm_leaf;                              // this day"s leaf dry wt
            //      laiField = laiField+dlai(iday)                           ! update lai with increase

            // update 'accounting' arrays as well
            dm_green[leaf] += ddm_leaf;
            dm_green[stem] += ddm_stem;
            //dm_green[pod] += ddm_boll;                       // uses different assimilate function to fruit(). Two 'models' do not match.
            dm_green[pod] += dm_fruWtToday;                    // use dm from ozcot_fruit(), should be more accurate and match weights when fruit are shed.

            //------------------------------------------------------------------------------
            //     feed back from root grow to root depth
            //------------------------------------------------------------------------------
            if ((iday == 1)) ddm_root_max = ddm_root;            // initialise max root rate

            if ((ddm_root > ddm_root_max))
            {
                ddm_root_max = ddm_root;                         // save maximum root rate
                root_feedback = 1.0;                             // feedback of dw on depth
            }
            else
            {
                if ((ddm_root_max == 0.0))
                {
                    root_feedback = 1.0;
                }
                else
                {
                    root_feedback = mvDataFunctions.divide_check(ddm_root, ddm_root_max, 0.0);  // feedback of dw on depth
                }
            }

            root_feedback = Math.Min(root_feedback, sd_root);            // feedback of dw on depth
            if ((root_feedback > 0.0))
            {
                root_feedback = Math.Pow(root_feedback, 0.333);       // cubic to linear
            }


            return;   // ozcot_dryxmatter()
        }


        // ====================================================================
        public void ozcot_plant_height()
        {
            // ====================================================================
            //
            //     Estimate the height of the crop (mm)
            //
            // ====================================================================

            double heightToday;


            // Calculation to update estimated plant height  (mm)

            //Based on equation developed by Bange, Milroy and Richards
            // "Development of simple techniques for rapid leaf area measurement in cotton"
            //  published in Proceedings of the 2002 Australian Cotton Conference

            heightToday = 0.0;

            // need lai on a 1m row solid plant basis to estimate height from this equation
            if (laiField > 0.0)
            {
                laiRow = laiField * effectiveRowSpacing;
                heightToday = (laiRow + 0.0352) / 0.00347;                  // estimated height (mm)

                // If there is a list of heights and stem wts 
                //  then estimate a specialised relationship
                if (num_height > 0)    
                {   // height is estimated by dividing dry_wt_stem (g/m^2) by plant population --> wt/plant, 
                    // then interpolating from data points supplied
                    heightToday = mvDataFunctions.linear_interp_real(mvDataFunctions.divide(dm_stem, pp, 0.0),
                                                                  x_stem_wt,
                                                                  y_height,
                                                                  num_height
                                                                );
                }
            }

            height = Math.Max(height, Math.Round(heightToday, 0));   // round height to mm integer

            return ;  // plant_height

        }


        // ====================================================================
        //      subroutine assimilation (assimilate,i)
        public void ozcot_assimilation()
        {
            // ====================================================================

            //     assimilate production for the day is estimated from intercepted
            //     photosynthetically active radiation (montieth 1977).
            //     adjusted for effects of water stress using data of turner et al 1986
            //     collected at narrabri ars.
            //     effect of water logging based on observation of hearn and constable 1984
            //     on yield and hodgson on photosynthesis.
            //     effect of temperature, see constables thesis 1981, direct effect on
            //     photosynthesis and respiration, using angus and wilson's 1976 expression
            //     for a scalar and constables value for base and optimum.
            //     carrying capacity, carcap,estimated. it is maximum number of bolls the
            //     crop can carry, and is therefore the boll load that causes 100. sheddin
            //     local variables:
            //       assim_mean      3 day mean of assimilate supply
            //       assim_1         previous day's assimilate supply
            //       assim_2         day before previous day's supply
            //       rad_mj          radiation in mega joules
            //       rel_p           relative photosynthesis, scalar for water stress
            //       par_int         intercepted photosynthetically active radiation
            //       tf              temperature scalar for dry matter production

            double rad_mj;
            double alight;
            double par_int;
            double rel_p;
            double tf;

            //- implementation section ----------------------------------

            //------------------------------------------------------------------------------
            //     initialise for new season
            //------------------------------------------------------------------------------

            //pc   if(iday.eq.1) then
            //pc       assim_1 = 0.0                    ! previous day's assimilation
            //pc       assim_2 = 0.0                    ! day before that
            //psc  endif

            //------------------------------------------------------------------------------
            //     photosynthesis
            //------------------------------------------------------------------------------

            rad_mj = rad / 23.87;                    // langleys to mjoules
            //  par_int = rad_mj*(1.-tr)*0.5         ! intercepted par

            //jh      alight = 1.-exp(-1.*laiField)      ! light interception, beer's law.
            //jh         changed to the following to accommodate skip row
            laiRow = laiField;
            if (skiprow > 0)
            {
                laiRow = laiField * effectiveRowSpacing;                  // lai in hedgerow
            }
            alight = (1.0 - Math.Exp(-ozcot_kvalue * laiRow));    // original code  - now gives interception in hedgerow
            if ((skiprow > 0))
            {
                alight = alight / effectiveRowSpacing;                // interception on ground area basis
                //jh         laiField = laiField/effectiveRowSpacing         ! restore lai to ground area basis
            }

            par_int = rad_mj * (alight) * 0.5;       // intercepted par, ex old ozcot 

            assimilate = par_int * e_par;            // assimilation
            if ((assimilate * 2.0) > res_cap)
            {
                res_cap = assimilate * 2.0;          // capacity to store reserves 
            }

            //------------------------------------------------------------------------------
            //     effect of water stress on assimilation
            //------------------------------------------------------------------------------

            rel_p = 0.25 + 0.864 * smi;            // effect of water stress on pp 
            if ((rel_p > 1.0)) rel_p = 1.0;        // (turner et al 1986). 
            assimilate = assimilate * rel_p;       // adjust for water stress

            //------------------------------------------------------------------------------
            //     effect of temperature on dry matter production
            //------------------------------------------------------------------------------

            tf = (tempav - t_base) / (t_opt - t_base);     // temperature scalar after
            tf = 2 * tf - Math.Pow(tf, 2);                 // angus & wilson 1976, constable 1981 
            assimilate = assimilate * tf;                  // adjust assimilate for temp

            //------------------------------------------------------------------------------
            //     effect of waterlogging on photosynthesis - hearn & constable 1984 eqn 4
            //------------------------------------------------------------------------------

            //      if(def.lt.2.5) then                    ! waterlogged?
            //jh      if(sw/ul.gt.watlog_c) then           ! waterlogged? 
            if ((wli > watlog_c))                         // waterlogged? 
            {
                assimilate = assimilate * wlog_assimilate_red;    // adjust for water logging - old ozcot 
            }

            //jhadded      effect of n stress
            assimilate = assimilate * vnstrs;

            //      if(isq.eq.0 .or. i.lt.isq+2) return  ! do not proceed to carrying capacity
            if ((isq == 0 | DAS < isq + 2))
            {
                return; 
                // do not proceed to carrying capacity
            }
            else
            {
            }

            //------------------------------------------------------------------------------
            //     carrying capacity - photosynthetic capacity divided by boll growth rate
            //------------------------------------------------------------------------------

            //      disable rest of subroutine for use with ozcot2 in apsru system

            //      if(assim_1.gt.0.0 .and.assim_1.gt.0.0) then      ! not 1st or 2nd crop day
            //          assim_mean = (assimilate+assim_1+assim_2)/3. ! 3 day running mean
            //      else                                             ! is 1st or 2nd crop day
            //          assim_mean = assimilate
            //      endif
            //                                                  ! use mean to buffer carcap
            //      assim_2 = assim_1                            ! 3rd day's for tomorrow
            //      assim_1 = assimilate                         ! 2nd day's for tomorrow

            //      if(bollgr.gt.0.0) then
            //          carcap_c = assim_mean/bollgr             ! carrying capacity
            //      else
            //          carcap_c = 0.0                           ! zero when bolls not growing
            //      endif

            //      if(carcap_lt.0.) carcap_c = 0.             ! trap
            //      carcap  = carcap_c
            //      cutout = carcap*fcutout(ivar)                ! boll load for cutout

            //      cutout = carcap*1.00                         ! sensitivity to fcutout

            //------------------------------------------------------------------------------

            return; 
        }



        // ====================================================================
        //      real function co2FertFX ()
        public double ozcot_co2FertFX()
        {
            // ====================================================================
            //
            //     RUE modification via CO2
            //
            // ====================================================================

            double co2FertValue;

            // error trap
            if (co2 < 300.0) co2 = co2_default;

            if (num_co2_fert > 0)   // are there values provided that define a response curve?
            {
                co2FertValue = mvDataFunctions.linear_interp_real(co2, x_co2_fert, y_co2_fert, num_co2_fert);
            }
            else  // use standard response curve 
            {
                co2FertValue = (0.004050 * co2) - (0.000004006 * Math.Pow(co2, 2.0)) + (0.000000001303 * Math.Pow(co2, 3.0));

            }

            return co2FertValue;

        }

    }
}
