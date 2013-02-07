
using System;
using System.Collections.Generic;
using System.Linq;
using System.Windows.Forms;
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
        private static String _STYPE = "mvOZCOT model";  //CompClass
        private static String _SVERSION = "2.00";
        private static String _SAUTHOR = "CSIRO Plant Industry - Cotton Research";
      //  protected static String _PROXY_COMP = "mvCotton.dll";                    //obsolete 

        //subscribed events
        private const int EVENT_SOW = 1;           //sow using 5 input parameters
        private const int EVENT_PROCESS = 2;
        private const int EVENT_HARVEST = 3;
        private const int EVENT_RESET = 4;
        private const int EVENT_TICK = 5;
        private const int EVENT_DO_GROWTH = 6;
        private const int EVENT_DO_SOW = 7;        //sow with no parameters
        private const int EVENT_DO_HARVEST = 8;

        //TODO: DBJ should these events be re-instated?  
        //private const int EVENT_End_Crop = ?;
        //private const int EVENT_Kill_Crop = ?;   //no code executed in original APSIM-OZCOT
        //private const int EVENT_End_Run = ?;
        //private const int EVENT_NewMet = ?;


        //published events
        private const int evtSowing = 20;
        private const int evtHarvesting = 21;
        private const int evtBiomassRemoved = 22;
        private const int evtIncorpFOM = 23;  
        private const int evtNewCrop = 24;       

        //TODO: DBJ  when should these be published?  Not in original APSIM-OZCOT
        //private const int evtNewPotentialGrowth = ?;
        //private const int evtPhaseChanged = ?;
        //private const int evtNewCanopy = ?;
        //private const int evtBiomassRemoved = ?;
        //private const int evtExternalMassFlow = ?;



        //event states
        private const int SOW_STATE_ACQUIRE = 1;
        private const int SOW_STATE_EXECUTE = 2;
        private const int GROWTH_STATE_ACQUIRE = 1;
        private const int GROWTH_STATE_EXECUTE = 2;
        private const int HARVEST_STATE_ACQUIRE = 1;
        private const int HARVEST_STATE_EXECUTE = 2;
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
        private const int DRV_ES = 11;
        private const int DRV_RUNOFF = 12;

        //soilN driver properties (for APSIM SOILN)
        private const int DRV_NO3_MIN = 13;
        private const int DRV_NO3 = 14;
        private const int DRV_NH4_MIN = 15;
        private const int DRV_NH4 = 16;
        private const int DRV_UREA = 17;

       //  private const int DRV_TIME = 18;



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
        private const int PROP_ul1 = PROP_START_INDEX + 6;
        private const int PROP_cona = PROP_START_INDEX + 7;
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
        private const int PROP_ozcot_sumDD = PROP_START_INDEX + 96;
        private const int PROP_lai = PROP_START_INDEX + 97;
        private const int PROP_dw_total = PROP_START_INDEX + 98;
        private const int PROP_dw_boll = PROP_START_INDEX + 99;
        private const int PROP_dn_plant = PROP_START_INDEX + 100;
        private const int PROP_assimilate = PROP_START_INDEX + 101;
        private const int PROP_GrowthWt = PROP_START_INDEX + 102;
        private const int PROP_ep = PROP_START_INDEX + 103;
       // private const int PROP_dm_green = PROP_START_INDEX + 104;  //moved to end
       // private const int PROP_n_green = PROP_START_INDEX + 105;   //moved to end
        private const int PROP_crop_in = PROP_START_INDEX + 106;

        private const int PROP_das = PROP_START_INDEX + 107;
        private const int PROP_sites = PROP_START_INDEX + 108;
        private const int PROP_squarz = PROP_START_INDEX + 109;
        private const int PROP_fru_no_cat = PROP_START_INDEX + 110;
        private const int PROP_bollz = PROP_START_INDEX + 111;
        private const int PROP_openz = PROP_START_INDEX + 112;
        private const int PROP_lint = PROP_START_INDEX + 113;
        private const int PROP_openwt = PROP_START_INDEX + 114;
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
        private const int PROP_plants_pm_default = PROP_START_INDEX + 158;
        private const int PROP_sowing_depth_default = PROP_START_INDEX + 159;
        private const int PROP_row_spacing_default = PROP_START_INDEX + 160;  
        private const int PROP_skiprow_default = PROP_START_INDEX + 161;
        private const int PROP_cultivars_list = PROP_START_INDEX + 162;
        private const int PROP_n_uptake = PROP_START_INDEX + 163;
        private const int PROP_ep_cm = PROP_START_INDEX + 164;
        private const int PROP_plant_status = PROP_START_INDEX + 165;

        private const int PROP_co2 = PROP_START_INDEX + 166;

        private const int PROP_x_co2_fert = PROP_START_INDEX + 167;
        private const int PROP_y_co2_fert = PROP_START_INDEX + 168;
        private const int PROP_x_stem_wt = PROP_START_INDEX + 169;
        private const int PROP_y_height = PROP_START_INDEX + 170;

        private const int PROP_dm_green = PROP_START_INDEX + 171;
        private const int PROP_dm_senesced = PROP_START_INDEX + 172;
        private const int PROP_dlt_dm_green = PROP_START_INDEX + 173;
        private const int PROP_n_green = PROP_START_INDEX + 174;
        private const int PROP_n_senesced = PROP_START_INDEX + 175;

        private const int PROP_dm = PROP_START_INDEX + 176;
        private const int PROP_daysAfterSowing = PROP_START_INDEX + 177;  


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
        private const string module_name = "OZCOT";
        private const int max_layers = 100;

        private const double gm2kg = 0.001;             //converting grams to kilograms
        private const double sm2ha = 0.0001;            //converting square metres to hectares
        private const double kg2gm = 1000.0;            //converting kilograms to grams
        private const double ha2sm = 10000.0;           //converting hectares to square metres


        //Event DDML 
        public const string typeSow = @"<type><field name=""cultivar""     kind=""string""/>
                                              <field name=""sowing_depth"" kind=""double""/>
                                              <field name=""plants_pm""    kind=""double""/>
                                              <field name=""row_spacing""  kind=""double""/>
                                              <field name=""skiprow""      kind=""double""/> 
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
        private const int age6 = 6;
        private const int age7 = 7;

        private const int max_age = 7;

        //        ! crop status
        private const string status_alive = "alive";
        private const string status_dead = "dead";
        private const string status_out = "out";
        private const string crop_inactive = "inactive";


        private const double c_uptakn_max = 240.0;  // kg/ha


        //!  crop parts constant values
        private const int root = 1;
        private const int leaf = 2;
        private const int stem = 3;
        private const int pod  = 4;  // pod or boll
        private const int meal = 5;  // meal - excludes oil component
        private const int oil  = 6;  // seed oil
        // number of plant parts
        private const int max_part = 6;

        private readonly string[] part_name = new string[] { "root", "leaf", "stem", "pod", "meal", "oil" };

        // working arrays for reporting  (1 based arrays)
        private double[] dm_crop = new double[max_part + 1];    // dry matter of crop (kg/ha)
        private double[] dm_N = new double[max_part + 1];       // N content of dry matter (kg/ha)


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
        private double ppm_row_default;
        private double sowing_depth_default;
        private double row_spacing_default;
        private double skiprow_default;

        private double leaf_res_n_conc;
        //jh      real dlds
        private double hucut;
        private double baset;
        //jh         real    ambda
        private double cona;
        private double ul1;
        private double open_def;
        //jh         integer iwindow
        //jh         real    sow_sw



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


        private double frost_kill_immediate;	// min temperature below which immediate kill
        private double rtdep_max;
        private double harvest_n_frac;			// fraction of uptake n for potential n harvested

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

        private double frudw_shed;
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
        private double rain;
        //jh         real    epan
        private double tempdy;
        private double tempwt;
        private double wind;
        private double tempav;
        private double hunits;
       // private double asoil;
        //jh v2001         real    eos
        private double qa;  // upper boundary for solrad
        private double solrto;
        private double q;

        // evapotranspiration calculation values
        private double eo;
        private double es;
        private double ep;
        private double et;
        private double ho;

        private double tr;
        private double f_intz;
        //cjh v2001          real    rrig(max_rrig)
        private double rtsw;
        //jh v2001         real    defirg
        //                 real    ransub(20)
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
        private double dul;
        private double sat;
        private double[] bulkd = new double[max_layers + 1];

        private double wpwc;
        //                 real    sweplr(10)
        //                 real    swwep
        //jh v2001         real    tswl(max_layers)
        //jh v2001         real    setlyr(max_layers)
        private double alai;
        private double alai_row;
        private double rtdep;
        private string cultivar;   // name of cultivar (char * 20)
        private double row_space_mm;
        private double row_spacing;
        private double ppm;
        private double sdepth;
        private double sow_depth_mm;
        private double skiprow;
        private double rtdepm;
        private double shedlf;
        private double smi;
        private double s;
        private double rs;
        private double pp;
        private double ps;
        //private double fll;
        //                 real    snaplc(2)
        private double snaplc;
        private double availn;
        //private double initialn;
        //                 real    soilnt
        private double uptakn;
        //private double vegn;
        private double frun;
        private double plantn;
        private double seed_nc;
        //private double strucn;
        private double[] frucat = new double[max_categories + 1];
        //private double daysqz;
        //private double daysfl;
        //private double daysop;
        private double[,] fmkcat = new double[max_categories + 1, max_age + 1];
        private double dd;
        private double ddmerg;
        private double sumdd;
        private double bgrvar;
        private double frudw;
        private double squarz;
        private double bollz;
        private double openz;
        private double sites;
        private double sites1;
        private double size;
        private double bload;
        private double openwt;
        //jh         real    sqcon(10)
        //jh         real    respcon(10)
        //jh         real    flai(10)
        //jh         real    fcutout(10)
        //private double carcap;
        private double cutout;
        private double vnstrs;
        private double fnstrs;
        private double rad;
        private double pclint;
        private double carcap_c;
        private double carcap_n;
        //jh         real    scboll(10)
        private double[] fruno = new double[max_cohort + 1];
        private double[] fruwt = new double[max_cohort + 1];
        private double[,] frmark = new double[max_cohort + 1, max_age + 1];
        private double[] fyzage = new double[max_cohort + 1];
        private double[] dlai = new double[max_cohort + 1];
        private double alaiz;
        private double plntnz;
        //         real    defirr(20)
        //private double twater;
        private double alint;
        //private double gross_marg;
        //jh v2001         real    def_last
        private double sqzx;

        private double s_bed_mi;
        private double delay;
        private double[] bpsum = new double[max_cohort + 1];
        private double bollgr;
        private double dlai_pot;
        private double dw_boll;
        private double dw_leaf;
        private double dw_root;
        private double dw_stem;
        private double dw_total;
        private double assimilate;
        private double height;

        private double reserve;
        private double res_cap;
        private double root_feedback;
        private double[] ddw_l = new double[max_cohort + 1];
        private double ddw_boll;
        private double ddw_leaf;
        private double ddw_root;
        private double ddw_root_max;
        private double ddw_stem;
        private double leaf_res;
        private double stem_res;
        private double root_res;
        private double leaf_res_n;
        private double stem_res_n;
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
        private double ppm_target;
        private double ppm_row;  //property initialised
        private double nskip;
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
        //         integer imet
        //private int immo;
        //private int imdy;
        //private int imyr;
        private int jdate;
        private int DOY;
        //cjh v2001          integer mdpy
        //         integer igday(20)
        //         integer newran
        //         integer nrnsdy(20)
        private int nlayr;
        private int nrtlayr;
        private int isw;
        private int iemrg;
        private int isow;
        //         integer ismo
        //         integer isdy
        //         integer isyr
        private int isq;
        //private int ivar;
        //jh         integer idate
        //private int ilai;
        private int iday;
        //         integer iermo
        //         integer ierdy
        //         integer ieryr
        //         integer nfert
        //         integer nday(2)
        private int[] lfru = new int[max_categories + 1];
        //         integer jco(25)
        //         integer ico(25)
        private int idayco;
        private int last_day;
        private int ilaiz;
        private int iplntn;
        private int nirr;
        private int isqzx;
        private int j_pick;
        private int n_pick;
        private int n_def;
        private int i_def;
        private int i_def2;
        private int j_def;
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
        private bool zero_variables;
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
        private double[] x_stem_wt = new double[20];
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

            addEvent("harvest", EVENT_HARVEST, TypeSpec.KIND_SUBSCRIBEDEVENT, TypeSpec.TYPEEMPTY, "Harvest Cotton", "APSIM Harvest event", 0);
            defineEventState(EVENT_HARVEST, HARVEST_STATE_ACQUIRE, TStateMachine.NONLOGIC);
            defineEventState(EVENT_HARVEST, HARVEST_STATE_EXECUTE, TStateMachine.LOGIC);
            defineEventTransition(EVENT_HARVEST, TStateMachine.IDLE, 0, HARVEST_STATE_ACQUIRE, true);   //idle -> acquire
            defineEventTransition(EVENT_HARVEST, HARVEST_STATE_ACQUIRE, 0, HARVEST_STATE_EXECUTE, false);
            defineEventTransition(EVENT_HARVEST, HARVEST_STATE_EXECUTE, 0, TStateMachine.DONE, false); //done

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

            newProperty = addProperty("crop_type", PROP_crop_type, true, false, true, "-", false, "string", "Crop Type", "Crop Type (default: Cotton)");
            newProperty.setDefault("Cotton");

            addProperty("leaf_res_n_conc", PROP_leaf_res_n_conc, true, false, true, "-", false, "double", "N conc leaf residue", "N concentration of leaf residue");
            setPropertyRange(PROP_leaf_res_n_conc, 0.02, 0.0, 1.0);

            addProperty("hucut", PROP_hucut, true, false, true, "oC", false, "double", "Heat Unit calc max temp ", "MaxT limit for Heat Unit caluculation");
            setPropertyRange(PROP_hucut, 40.0, 0.0, 100.0);

            addProperty("baset", PROP_baset, true, false, true, "oC", false, "double", "Heat Unit calc base temp", "Base temperature (min) for Heat Unit calculation");
            setPropertyRange(PROP_baset, 12.0, 0.0, 30.0);

            addProperty("ul1", PROP_ul1, true, false, true, "-", false, "double", "limit of es stage1", "limit of es stage1");   //limit of es stage1
            setPropertyRange(PROP_ul1, 1.4, 0.0, 10.0);

            addProperty("cona", PROP_cona, true, false, true, "-", false, "double", "rate in stage2 es", "rate in stage2 es");  //rate in stage2 es
            setPropertyRange(PROP_cona, 0.35, 0.0, 1.0);

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
            setPropertyRange(PROP_rtdep_max, 130.0, 0.0, 1000.0);

            addProperty("harvest_n_frac", PROP_harvest_n_frac, true, false, true, "-", false, "double", "fraction of uptake N for potential N harvested", "fraction of uptake N for potential N harvested");
            setPropertyRange(PROP_harvest_n_frac, 0.85, 0.0, 1.0);

            addProperty("cutout_smi_crit", PROP_cutout_smi_crit, true, false, true, "-", false, "double", "smi critical level for cutout", "smi critical level for cutout");
            setPropertyRange(PROP_cutout_smi_crit, 0.75, 0.0, 1.0);

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
            setPropertyRange(PROP_flfsmi_low, 0.0, 0.0, 10.0);

            addProperty("flfsmi_high", PROP_flfsmi_high, true, false, true, "-", false, "double", "water stress on pre-squaring LAI - upper limit", "water stress on pre-squaring LAI - upper limit");
            setPropertyRange(PROP_flfsmi_high, 0.5, 0.0, 10.0);

            addProperty("flfsmi_a", PROP_flfsmi_a, true, false, true, "-", false, "double", "water stress on pre-squaring LAI - amplitude", "water stress on pre-squaring LAI - amplitude");
            setPropertyRange(PROP_flfsmi_a, 1.0, 0.0, 10.0);

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

            // read the soil water lower limits "ll" for the cotton root extraction
            //    eg  <ll>0.250   0.289   0.276   0.296   0.324   0.353   0.428 ()</ll>
            addProperty("ll", PROP_ll, true, false, true, "mm/mm", true, "double", "crop lower limit", "crop lower limit for this soil by layer");
            setPropertyRange(PROP_ll, 0.30, 0.0, 1.0);

            // read the sowing details
            newProperty = addProperty("cultivar_default", PROP_cultivar_default, true, false, true, "-", false, "string", "default cultivar", "default cultivar if none specified or details not found");
            newProperty.setDefault("S189");
            //"S189"

            addProperty("sowing_depth_default", PROP_sowing_depth_default, true, false, true, "mm", false, "double", "default sowing depth", "default sowing depth mm");
            setPropertyRange(PROP_sowing_depth_default, 50.0, 0.0, 100.0);

            addProperty("row_spacing_default", PROP_row_spacing_default, true, false, true, "mm", false, "double", "default row spacing", "default row spacing mm");
            setPropertyRange(PROP_row_spacing_default, 1000, 100, 5000);

            addProperty("plants_pm_default", PROP_plants_pm_default, true, false, true, "-", false, "double", "default plants per metre row", "default plants per metre row");
            setPropertyRange(PROP_plants_pm_default, 10.0, 1.0, 100.0);

            addProperty("skiprow_default", PROP_skiprow_default, true, false, true, "-", false, "integer4", "skip row default setting", "skip row default setting"); //nskip
            setPropertyRange(PROP_skiprow_default, 0, -1, 10);

            //read the cultivars list (if present)
            addProperty("cultivars", PROP_cultivars_list, true, false, true, typeCultivars, "cultivars list", "List of additional or modified cultivars");
            
            // read the cultivar details
            addProperty("percent_l", PROP_percent_l, true, false, true, "%", false, "double", "percent lint", "percent lint in boll by weight");
            setPropertyRange(PROP_percent_l, 42.0, 30.0, 55.0);

            addProperty("scboll", PROP_scboll, true, false, true, "g", false, "double", "seed cotton per boll", "boll size grams of seed cotton");
            setPropertyRange(PROP_scboll, 4.7, 1.0, 8.0);

            addProperty("respcon", PROP_respcon, true, false, true, "-", false, "double", "respiration constant", "respiration constant, demand on assimilates");
            setPropertyRange(PROP_respcon, 0.01593, 0.001, 0.03);

            addProperty("sqcon", PROP_sqcon, true, false, true, "-", false, "double", "squaring constant", "squaring constant, rate of sites per DD");
            setPropertyRange(PROP_sqcon, 0.0217, 0.001, 0.03);

            addProperty("fcutout", PROP_fcutout, true, false, true, "-", false, "double", "fruiting cutout", "boll load cutout per fruit category");
            setPropertyRange(PROP_fcutout, 0.5411, 0.0, 1.0);

            addProperty("flai", PROP_flai, true, false, true, "-", false, "double", "cultivar adjustment for leaf area per site", "cultivar adjustment for leaf area per site");
            setPropertyRange(PROP_flai, 0.52, 0.0, 1.0);

            addProperty("ddisq", PROP_ddisq, true, false, true, "-", false, "double", "DD to 1st square", "DD to 1st square");
            setPropertyRange(PROP_ddisq, 402.0, 0.0, 1000.0);

            addProperty("popcon", PROP_popcon, true, false, true, "-", false, "double", "population constant", "population constant - adjusts site production based on plant density");
            setPropertyRange(PROP_popcon, 0.03633, 0.0, 1.0);

            addProperty("acotyl", PROP_acotyl, true, false, true, "mm^2", false, "double", "cotyledon leaf area", "initial leaf area of cotyledons at emergence");
            setPropertyRange(PROP_acotyl, 525.0, 0.0, 1000.0);

            addProperty("rlai", PROP_rlai, true, false, true, "-", false, "double", "LAI increase rate pre squaring", "unstressed LAI increase rate pre squaring");
            setPropertyRange(PROP_rlai, 0.010, 0.0, 1.0);

            addProperty("fburr", PROP_fburr, true, false, true, "-", false, "double", "burr fraction", "proportional value of whole boll weight to seed cotton weight ~1.23 ");
            setPropertyRange(PROP_fburr, 1.23, 0.0, 5.0);

            addProperty("dlds_max", PROP_dlds_max, true, false, true, "-", false, "double", "max LAI increase per site", "max LAI increase per site");
            setPropertyRange(PROP_dlds_max, 0.12, 0.0, 5.0);

            addProperty("rate_emergence", PROP_rate_emergence, true, false, true, "mm/dd", false, "double", "rate of emergence", "growth rate mm/DD for sowing to emergence");
            setPropertyRange(PROP_rate_emergence, 1.0, 0.0, 10.0);

            addProperty("Background_retention", PROP_background_retention, true, false, true, "-", false, "double", "background retention", "rate of underlying retention of fruit (1-shedding rate)");
            setPropertyRange(PROP_background_retention, 0.8, 0.2, 1.0);

            //eg <FRUDD>50.   161.   307.   338.   484.   630.   848.  1071.</FRUDD>
            addProperty("frudd", PROP_frudd, true, false, true, "dd", true, "double", "fruit development in DD", "fruit category progression in day degrees");
            setPropertyRange(PROP_frudd, 0.0, 0.0, 1500.0);
            propertyList[PROP_frudd].setElementCount(8);        //1 based array (defined in Delphi)
            propertyList[PROP_frudd].item(1).setValue(50.0);
            propertyList[PROP_frudd].item(2).setValue(161.0);
            propertyList[PROP_frudd].item(3).setValue(307.0);
            propertyList[PROP_frudd].item(4).setValue(338.0);
            propertyList[PROP_frudd].item(5).setValue(484.0);
            propertyList[PROP_frudd].item(6).setValue(630.0);
            propertyList[PROP_frudd].item(7).setValue(848.0);
            propertyList[PROP_frudd].item(8).setValue(1071.0);
            
            

            //eg <BLTME>0.00   0.00   0.00   0.07   0.21   0.33   0.55   1.00</BLTME>
            addProperty("bltme", PROP_bltme, true, false, true, "-", true, "double", "boll time", "boll progression as proportion of open boll");
            setPropertyRange(PROP_bltme, 0.0, 0.0, 1.0);
            propertyList[PROP_bltme].setElementCount(8);        //1 based array (defined in Delphi)
            propertyList[PROP_bltme].item(1).setValue(0.0);
            propertyList[PROP_bltme].item(2).setValue(0.0);
            propertyList[PROP_bltme].item(3).setValue(0.0);
            propertyList[PROP_bltme].item(4).setValue(0.07);
            propertyList[PROP_bltme].item(5).setValue(0.21);
            propertyList[PROP_bltme].item(6).setValue(0.33);
            propertyList[PROP_bltme].item(7).setValue(0.55);
            propertyList[PROP_bltme].item(8).setValue(1.0);


            //eg <WT>0.0104 0.0272 0.1441 0.0988 0.5042 0.9617 1.0000 0.5785</WT>
            addProperty("wt", PROP_wt, true, false, true, "-", true, "double", "fruit assimilate demand as proportion of large green boll", "fruit assimilate demand as proportion of large green boll - boll load caluculations ");
            setPropertyRange(PROP_wt, 0.0, 0.0, 1.0);
            propertyList[PROP_wt].setElementCount(8);          //1 based array (defined in Delphi)
            propertyList[PROP_wt].item(1).setValue(0.0104);
            propertyList[PROP_wt].item(2).setValue(0.0272);
            propertyList[PROP_wt].item(3).setValue(0.1441);
            propertyList[PROP_wt].item(4).setValue(0.0988);
            propertyList[PROP_wt].item(5).setValue(0.5042);
            propertyList[PROP_wt].item(6).setValue(0.9617);
            propertyList[PROP_wt].item(7).setValue(1.0);
            propertyList[PROP_wt].item(8).setValue(0.5785);



            //Greenhouse gas studies
            addProperty("co2", PROP_co2, true, false, true, "mm", false, "double", "co2 concentration", "co2 concentration");
            setPropertyRange(PROP_co2, 350.0, 0.0, 1000.0);

            //eg <x_co2_fert>?.</x_co2_fert>
            addProperty("x_co2_fert", PROP_x_co2_fert, true, false, true, "-", true, "double", "", "");
            setPropertyRange(PROP_x_co2_fert, 500.0, 300.0, 1000.0);
            propertyList[PROP_x_co2_fert].setElementCount(1);
            propertyList[PROP_x_co2_fert].item(1).setValue(500.0);

            //eg <y_co2_fert>?.</y_co2_fert>
            addProperty("y_co2_fert", PROP_y_co2_fert, true, false, true, "-", true, "double", "", "");
            setPropertyRange(PROP_y_co2_fert, 2.0, 0.0, 10.0);
            propertyList[PROP_y_co2_fert].setElementCount(1);
            propertyList[PROP_y_co2_fert].item(1).setValue(2.0);

            //Plant height
            //eg <x_stem_wt>?.</x_stem_wt>
            addProperty("x_stem_wt", PROP_x_stem_wt, true, false, true, "g/plant", true, "double", "", "");
            setPropertyRange(PROP_x_stem_wt, 0.0, 0.0, 1000.0);
            propertyList[PROP_x_stem_wt].setElementCount(1);
            propertyList[PROP_x_stem_wt].item(1).setValue(0.0);

            //eg <y_height>?.</y_height>
            addProperty("y_height", PROP_y_height, true, false, true, "mm", true, "double", "", "");
            setPropertyRange(PROP_y_height, 900.0, 0.0, 1500.0);
            propertyList[PROP_y_height].setElementCount(1);
            propertyList[PROP_y_height].item(1).setValue(900.0);


            //====================================================================
            //Output 'readable only' properties
            addProperty("PlantStatus", PROP_plant_status, true, false, false, "-", false, "string", "Plant status", "Plant status");
            addProperty("ozcot_status", PROP_ozcot_status, true, false, false, "-", false, "integer4", "model status", "model status - termination reason");
            addProperty("ozcot_sumDD", PROP_ozcot_sumDD, true, false, false, "-", false, "double", "accumulated DayDegrees", "accumulated DayDegrees");
            addProperty("lai", PROP_lai, true, false, false, "m2/m2", false, "double", "crop LAI", "crop LAI");
            addProperty("dw_total", PROP_dw_total, true, false, false, "kg/ha", false, "double", "total crop dry wt", "dry wt of the complete crop");  //TODO: check units???
            addProperty("dw_boll", PROP_dw_boll, true, false, false, "kg/ha", false, "double", "dry wt bolls", "dry wt bolls");
            addProperty("dn_plant", PROP_dn_plant, true, false, false, "kg/ha", false, "double", "daily increment of plant n to system", "daily increment of plant n to system");
            addProperty("assimilate", PROP_assimilate, true, false, false, "kg/ha", false, "double", "new dry matter passed daily from s/r assimilation", "new dry matter passed daily from s/r assimilation");
            addProperty("GrowthWt", PROP_GrowthWt, true, false, false, "kg/ha", false, "double", "same as assimilate", "same as assimilate");
            addProperty("dm", PROP_dm, true, false, false, "kg/ha", false, "double", "total dry wt of crop", "Total dry wt of growing crop");
            addProperty("dm_green", PROP_dm_green, true, false, false, "kg/ha", true, "double", "dry wt of growing crop", "dry wt of growing crop");
            addProperty("dm_senesced", PROP_dm_senesced, true, false, false, "kg/ha", true, "double", "dry wt of senesced crop", "dry wt of senesced crop");
            addProperty("dlt_dm_green", PROP_dlt_dm_green, true, false, false, "kg/ha", true, "double", "daily change in crop dry matter wt", "daily change in crop dry matter wt");
            addProperty("n_green", PROP_n_green, true, false, false, "kg/ha", true, "double", "N content of green crop", "N content of green crop");
            addProperty("n_senesced", PROP_n_senesced, true, false, false, "kg/ha", true, "double", "N content of senesced crop", "N content of senesced crop");
            addProperty("ep", PROP_ep, true, false, false, "-", false, "double", "plant evapotranspiration", "plant evapotranspiration");
            addProperty("crop_in", PROP_crop_in, true, false, false, "-", false, "integer4", "status flag - crop planted", "status flag - crop is in the ground");

            addProperty("das", PROP_das, true, false, false, "days", false, "integer4", "Days After Sowing", "Days After Sowing");
            addProperty("DaysAfterSowing", PROP_daysAfterSowing, true, false, false, "days", false, "integer4", "Days After Sowing", "Days After Sowing");
            addProperty("sites", PROP_sites, true, false, false, "1/m2", false, "double", "sites", "number of sites");
            addProperty("squarz", PROP_squarz, true, false, false, "1/m2", false, "double", "squares", "number of squares");
            addProperty("fru_no_cat", PROP_fru_no_cat, true, false, false, "-", true, "double", "fruit numbers by category", "fruit numbers by fruit category");
            addProperty("bollz", PROP_bollz, true, false, false, "1/m2", false, "double", "green bolls", "number of green bolls");
            addProperty("openz", PROP_openz, true, false, false, "1/m2", false, "double", "open bolls", "number of open bolls");
            addProperty("lint", PROP_lint, true, false, false, "kg/ha", false, "double", "lint (kg/ha)", "lint yield in kg/ha");
            addProperty("openwt", PROP_openwt, true, false, false, "kg/ha", false, "double", "dry wt of open bolls", "dry wt of open bolls");
            addProperty("frudw", PROP_frudw, true, false, false, "kg/ha", false, "double", "dry wt of growing fruit", "dry wt of all squares and green bolls");
            addProperty("frudw_tot", PROP_frudw_tot, true, false, false, "kg/ha", false, "double", "dry wt of all fruit", "dry wt of squares + green bolls + open bolls");
            addProperty("frudw_shed", PROP_frudw_shed, true, false, false, "kg/ha", false, "double", "dry wt of shed fruit", "dry wt of shed fruit");
            addProperty("frun", PROP_frun, true, false, false, "-", false, "double", "N in fruit kg/ha", "N in fruit kg/ha");
            addProperty("bload", PROP_bload, true, false, false, "", false, "double", "boll load", "boll load");
            addProperty("carcap_c", PROP_carcap_c, true, false, false, "-", false, "double", "carrying capacity - assimilate", "carrying capacity as limited by assimilate");
            addProperty("carcap_n", PROP_carcap_n, true, false, false, "-", false, "double", "carrying capacity - N", "carrying capacity as limited by Nitrogen");
            addProperty("vnstrs", PROP_vnstrs, true, false, false, "-", false, "double", "vegetative nitrogen stress", "stress indicator based on vegetative demand for Nitrogen");
            addProperty("fnstrs", PROP_fnstrs, true, false, false, "-", false, "double", "fruit nitrogen stress", "stress indicator based on fruit demand for Nitrogen");
            addProperty("dw_root", PROP_dw_root, true, false, false, "kg/ha", false, "double", "dry wt roots", "dry wt roots");
            addProperty("dw_leaf", PROP_dw_leaf, true, false, false, "kg/ha", false, "double", "dry wt leaf", "dry wt leaf");
            addProperty("dw_stem", PROP_dw_stem, true, false, false, "kg/ha", false, "double", "dry wt stem", "dry wt stem");
            addProperty("totnup", PROP_totnup, true, false, false, "kg/ha", false, "double", "total Nitrogen uptake", "total Nitrogen uptake");
            addProperty("yield", PROP_yield, true, false, false, "kg/ha", false, "double", "lint yield kg/ha", "lint yield kg/ha");
            addProperty("lint_yield", PROP_lint_yield, true, false, false, "kg/ha", false, "double", "lint yield kg/ha", "lint yield kg/ha");
            addProperty("cover_green", PROP_cover_green, true, false, false, "-", false, "double", "crop coverage", "crop coverage of the ground on an area basis");
            addProperty("covertotal", PROP_cover_tot, true, false, false, "-", false, "double", "crop coverage of ground", "");
            addProperty("availn", PROP_availn, true, false, false, "kg/ha", false, "double", "N available in soil", "N available in soil for uptake");
            addProperty("uptakn", PROP_uptakn, true, false, false, "kg/ha", false, "double", "N taken up by crop", "N taken up by crop");
            addProperty("tsno3", PROP_tsno3, true, false, false, "kg/ha", false, "double", "soil nitrate", "total soil nitrate");
            addProperty("ysno3", PROP_ysno3, true, false, false, "kg/ha", false, "double", "prior day's soil nitrate", "total soil nitrate yesterday");
            addProperty("tsnh4", PROP_tsnh4, true, false, false, "kg/ha", false, "double", "soil ammonium", "total soil ammonium");
            addProperty("ysnh4", PROP_ysnh4, true, false, false, "kg/ha", false, "double", "prior day's soil ammonium", "total soil ammonium yesterday");
            addProperty("d_nup", PROP_d_nup, true, false, false, "kg/ha", false, "double", "daily N uptake", "daily N uptake kg/ha");
            addProperty("n_uptake", PROP_n_uptake, true, false, false, "kg/ha", false, "double", "N uptake", "N uptake kg/ha");
            addProperty("rtdep", PROP_rtdep, true, false, false, "cm", false, "double", "rooting depth", "rooting depth");
            addProperty("s_bed_mi", PROP_s_bed_mi, true, false, false, "-", false, "double", "seed bed moisture index", "seed bed moisture index");
            addProperty("smi", PROP_smi, true, false, false, "", false, "double", "SMI", "soil moisture index");
            addProperty("wli", PROP_wli, true, false, false, "-", false, "double", "WLI", "waterlogging index");
            addProperty("ep_cm", PROP_ep_cm, true, false, false, "cm", false, "double", "evap in cm", "Plant evap in cm");
            addProperty("evap_plant", PROP_evap_plant, true, false, false, "mm", false, "double", "Plant evap mm", "Plant evap in mm");
            addProperty("evap_soil", PROP_evap_soil, true, false, false, "mm", false, "double", "soil evaporation", "evaporation from soil surface");
            addProperty("evap_pot", PROP_evap_pot, true, false, false, "mm", false, "double", "eo potential evaporation", "eo potential evaporation from soil surface");
            addProperty("evap_tot", PROP_evap_tot, true, false, false, "mm", false, "double", "total evaporation et", "total evapotranspiration et");
            addProperty("bolls_sc", PROP_bolls_sc, true, false, false, "g/boll", false, "double", "seed cotton g/boll ", "weight seed cotton g/boll");
            addProperty("nuptake", PROP_nuptake, true, false, false, "kg/ha", false, "double", "N uptake", "N uptake");
            addProperty("squarz_max", PROP_squarz_max, true, false, false, "1/m2", false, "double", "max squares", "peak number of squares");
            addProperty("lai_max", PROP_lai_max, true, false, false, "m2/m2", false, "double", "LAI max", "peak LAI value");
            addProperty("defol_das", PROP_defol_das, true, false, false, "das", false, "double", "1st defoliation DAS", "1st defoliation DAS");
            addProperty("defol2_das", PROP_defol2_das, true, false, false, "das", false, "double", "2nd defoliation DAS", "2nd defoliation DAS");
            addProperty("height", PROP_height, true, false, false, "mm", false, "double", "height of crop mm", "height of crop mm");
            addProperty("yield_bales", PROP_lint_bales, true, false, false, "bales/ha", false, "double", "lint yield bales/ha", "lint yield bales/ha");
            addProperty("yield_kg", PROP_lint_kg, true, false, false, "kg/ha", false, "double", "lint yield kg/ha", "lint yield kg/ha");




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
            addDriver("es", DRV_ES, 0, 1, "mm", false, "double", "Emissivity of evap surface", "Emissivity of evap surface", 0);
            addDriver("runoff", DRV_RUNOFF, 0, 1, "mm", false, "double", "Runoff", "Runoff from rain event", 0);

            // add soil nitrogen drivers
            addDriver("no3_min", DRV_NO3_MIN, 0, 1, "kg/ha", true, "double", "", "", 0);
            addDriver("no3", DRV_NO3, 0, 1, "kg/ha", true, "double", "", "", 0);
            addDriver("nh4_min", DRV_NH4_MIN, 0, 1, "kg/ha", true, "double", "", "", 0);
            addDriver("nh4", DRV_NH4, 0, 1, "kg/ha", true, "double", "", "", 0);
            addDriver("urea", DRV_UREA, 0, 1, "kg/ha", true, "double", "", "", 0);

            // addDriver("time", DRV_TIME, 0, 1, "", false, TTimeStep.typeTIMESTEP, 0);




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

                case PROP_ul1: ul1 = aValue.asDouble();
                    break;

                case PROP_cona: cona = aValue.asDouble();
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

                case PROP_sowing_depth_default: sowing_depth_default = aValue.asDouble();
                    break;

                case PROP_row_spacing_default: row_spacing_default = aValue.asDouble();
                    break;

                case PROP_plants_pm_default: ppm_row_default = aValue.asDouble();
                    break;

                case PROP_skiprow_default: skiprow_default = aValue.asInt();
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

                
                case PROP_percent_l: percent_l = aValue.asDouble();
                   // percent_l = percent_l / 100.0;	        // convert to fraction  //DBJ to use pcLint_frac  at point of use
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
               //     acotyl = acotyl / 1000000.0;		// convert from mm2 to m2  //DBJ to use acotyl_m2 near point of use
                    break;

                case PROP_rlai: rlai = aValue.asDouble();
                    break;

                case PROP_fburr: fburr = aValue.asDouble();
                    break;

                case PROP_dlds_max: dlds_max = aValue.asDouble();
                    break;

                case PROP_rate_emergence: rate_emergence = aValue.asDouble();
              //      rate_emergence = rate_emergence / 10.0;	         // convert from mm to cm  //DBJ to do at point of use in emerg
                    break;

                case PROP_background_retention: background_retention = aValue.asDouble();
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

                case PROP_co2: co2 = aValue.asDouble();
                    break;

                case PROP_x_co2_fert:
                    Array.Clear(x_co2_fert, 0, x_co2_fert.Length);
                    for (i = 1; i < aValue.count(); i++)
                    {
                        x_co2_fert[i] = aValue.item(i).asDouble();
                    }
                    break;


                case PROP_y_co2_fert:
                    Array.Clear(y_co2_fert, 0, y_co2_fert.Length);
                    for (i = 1; i < aValue.count(); i++)
                    {
                        y_co2_fert[i] = aValue.item(i).asDouble();
                    }

                    num_co2_fert = (int) aValue.count();

                    break;


                case PROP_x_stem_wt:
                    Array.Clear(x_stem_wt, 0, x_stem_wt.Length);
                    for (i = 1; i < aValue.count(); i++)
                    {
                        x_stem_wt[i] = aValue.item(i).asDouble();
                    }
                    break;


                case PROP_y_height:
                    Array.Clear(y_height, 0, y_height.Length);
                    for (i = 1; i < aValue.count(); i++)
                    {
                        y_height[i] = aValue.item(i).asDouble();
                    }

                    num_height = (int)aValue.count();

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
                    rain = aValue.asDouble();
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

                case PROP_sowing_depth_default: sowing_depth_default = aValue.asDouble();
                    break;

                case PROP_row_spacing_default: row_spacing_default = aValue.asDouble();
                    break;

                case PROP_plants_pm_default: ppm_row_default = aValue.asDouble();
                    break;

                case PROP_skiprow_default: skiprow_default = aValue.asInt();
                    break;

                case PROP_leaf_res_n_conc: leaf_res_n_conc = aValue.asDouble();
                    break;

                case PROP_hucut: hucut = aValue.asDouble();
                    break;

                case PROP_baset: baset = aValue.asDouble();
                    break;

                case PROP_ul1: ul1 = aValue.asDouble();
                    break;

                case PROP_cona: cona = aValue.asDouble();
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
            uint i;

            switch (iPropertyID)
            {
               case PROP_crop_type: aValue.setValue(crop_type);
                   break;

                case PROP_leaf_res_n_conc: aValue.setValue(leaf_res_n_conc);
                    break;

                case PROP_hucut: aValue.setValue(hucut);
                    break;

                case PROP_baset: aValue.setValue(baset);
                    break;

                case PROP_ul1: aValue.setValue(ul1);
                    break;

                case PROP_cona: aValue.setValue(cona);
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

                case PROP_skiprow: aValue.setValue(nskip);
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

                case PROP_ozcot_sumDD: aValue.setValue(sumdd);
                    break;

                case PROP_lai: aValue.setValue(alai);
                    break;

                case PROP_dw_total: aValue.setValue(dw_total * 10.0);  // g/m2  --> kg/ha
                    break;

                case PROP_dw_boll: aValue.setValue(dw_boll * 10.0);   // g/m2  --> kg/ha
                    break;

                case PROP_dn_plant: aValue.setValue(dn_plant * 10.0);  // g/m2  --> kg/ha
                    break;

                case PROP_assimilate: aValue.setValue(assimilate * 10.0);  // g/m2  --> kg/ha
                    break;

                case PROP_GrowthWt: aValue.setValue(assimilate * 10.0);  // g/m2  --> kg/ha
                    break;

                case PROP_n_green:  // (kg/ha)
                    Array.Clear(dm_N, 0, dm_N.Length);   // 1 based array  
                    dm_N[root] = root_n * 10.0;  // g/m2  --> kg/ha
                    dm_N[meal] = 0.0;      // meal is included in pod
                    dm_N[stem] = stem_n * 10.0;
                    dm_N[leaf] = leaf_n * 10.0;
                    dm_N[pod] = boll_n * 10.0;

                    aValue.setElementCount((uint)dm_N.Length - 1);
                    for (i = 1; i < dm_N.Length; i++)
                    {
                        aValue.item(i).setValue(dm_N[i]);
                    }
                    break;

                case PROP_n_senesced:  // (kg/ha)
                    Array.Clear(dm_N, 0, dm_N.Length);   // 1 based array  
                    dm_N[root] = 0.0;
                    dm_N[meal] = 0.0;      // meal is included in pod
                    dm_N[stem] = 0.0;
                    dm_N[leaf] = leaf_res_n * 10.0;   // g/m2  --> kg/ha
                    dm_N[pod] = 0.0;      // frudw_shed

                    aValue.setElementCount((uint)dm_N.Length-1);
                    for (i = 1; i < dm_N.Length; i++)
                    {
                        aValue.item(i).setValue(dm_N[i]);
                    }
                    break;

                case PROP_dlt_dm_green:  // (kg/ha)
                    Array.Clear(dm_crop, 0, dm_crop.Length);   // 1 based array  
                    dm_crop[root] = ddw_root * 10.0;  // g/m2  --> kg/ha
                    dm_crop[meal] = 0.0;      // meal is included in pod
                    dm_crop[stem] = ddw_stem * 10.0;
                    dm_crop[leaf] = ddw_leaf * 10.0;
                    dm_crop[pod] = ddw_boll * 10.0;

                    aValue.setElementCount((uint)dm_crop.Length-1);
                    for (i = 1; i < dm_crop.Length; i++)
                    {
                        aValue.item(i).setValue(dm_crop[i]);
                    }
                    break;

                case PROP_dm: aValue.setValue(dw_total * 10.0);  // g/m2  --> kg/ha
                    break;

                case PROP_dm_green:  // (kg/ha)
                    Array.Clear(dm_crop, 0, dm_crop.Length);   // 1 based array  
                    dm_crop[root] = dw_root * 10.0;  // g/m2  --> kg/ha
                    dm_crop[meal] = openwt * 10.0;
                    dm_crop[stem] = dw_stem * 10.0;
                    dm_crop[leaf] = dw_leaf * 10.0;
                    dm_crop[pod] = (Math.Max( (dw_boll - openwt), 0.0)) * 10.0;

                    aValue.setElementCount((uint)dm_crop.Length-1);
                    for (i = 1; i < dm_crop.Length; i++)
                    {
                        aValue.item(i).setValue(dm_crop[i]);
                    }
                    break;

                case PROP_dm_senesced:  // (kg/ha)
                    Array.Clear(dm_crop, 0, dm_crop.Length);   // 1 based array  
                    dm_crop[root] = 0.0;
                    dm_crop[meal] = 0.0;      
                    dm_crop[stem] = 0.0;
                    dm_crop[leaf] = leaf_res * 10.0;  // g/m2  --> kg/ha
                    dm_crop[pod] = 0.0;   // frudw_shed

                    aValue.setElementCount((uint)dm_crop.Length-1);
                    for (i = 1; i < dm_crop.Length; i++)
                    {
                        aValue.item(i).setValue(dm_crop[i]);
                    }
                    break;

                case PROP_ep: aValue.setValue(ep);
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

                case PROP_openwt: aValue.setValue(openwt * 10.0);
                    break;

                case PROP_frudw: aValue.setValue(frudw * 10.0);
                    break;

                case PROP_frudw_tot: aValue.setValue((frudw + openwt) * 10.0);
                    break;

                case PROP_frudw_shed: aValue.setValue(frudw_shed * 10.0);
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

                case PROP_dw_root: aValue.setValue(dw_root * 10.0);
                    break;

                case PROP_dw_leaf: aValue.setValue(dw_leaf * 10.0);
                    break;

                case PROP_dw_stem: aValue.setValue(dw_stem * 10.0);
                    break;

                case PROP_totnup: aValue.setValue(total_n * 10.0);
                    break;

                case PROP_yield: aValue.setValue(alint);
                    break;

                case PROP_lint_yield: aValue.setValue(alint);
                    break;

                case PROP_cover_green:
                    double cover = 0.0;
                    if (plant_status == status_alive)
                    {
                        cover = Math.Max((1.0 - tr), 0.0);
                    }

                    aValue.setValue(cover);
                    break;

                case PROP_cover_tot:
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

                case PROP_d_nup: aValue.setValue(dn_plant * 10.0);
                    break;

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
                        aValue.setValue((openwt / openz));
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

                case PROP_defol_das: aValue.setValue(i_def);
                    break;

                case PROP_defol2_das: aValue.setValue(i_def2);
                    break;

                case PROP_lint_bales: aValue.setValue(alint/227);
                    break;

                case PROP_lint_kg: aValue.setValue(alint);
                    break;

                case PROP_co2: aValue.setValue(co2);
                    break;

                case PROP_x_co2_fert: //greenhouse gas
                    aValue.setElementCount((uint)x_co2_fert.Length);
                    for (i = 1; i <= x_co2_fert.Length; i++)
                    {
                        aValue.item(i).setValue(x_co2_fert[i-1]);
                    }
                    break;

                case PROP_y_co2_fert: //greenhouse gas
                    aValue.setElementCount((uint)y_co2_fert.Length);
                    for (i = 1; i <= y_co2_fert.Length; i++)
                    {
                        aValue.item(i).setValue(y_co2_fert[i-1]);
                    }
                    break;

                case PROP_x_stem_wt: //plant height
                    aValue.setElementCount((uint)x_stem_wt.Length);
                    for (i = 1; i <= x_stem_wt.Length; i++)
                    {
                        aValue.item(i).setValue(x_stem_wt[i-1]);
                    }
                    break;

                case PROP_y_height: //plant height
                    aValue.setElementCount((uint)y_height.Length);
                    for (i = 1; i <= y_height.Length; i++)
                    {
                        aValue.item(i).setValue(y_height[i-1]);
                    }
                    break;

                case PROP_height: aValue.setValue(height);  
                    break;

               
               


                
                //default:
                //    result = false;
                //    break;


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
                        if (crop_in)
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

                        if ((iend != 0 & DAS > 400))
                        {
                            fatal_error("Crop remains unharvested at 400 das. Check that manager harvest criteria contains a test for ozcot_status > 0");
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
                            // sendDriverRequest(DRV_MAXT, iEventID);	//firstly get the maxt from weather
                    }
                    else if(iState == HARVEST_STATE_EXECUTE)
                    {
                            ozcot_end_crop();
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
        /// Handles a fatal error
        /// </summary>
        //==========================================================================
        public void fatal_error(string errText)
        {
            System.Console.WriteLine("Fatal Error: " + errText);
            // TODO dbj  call terminate crop? or simulation?
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
            zero_variables = false;

            iend = 0;
            iday = 0;
            idayx = 0;
            last_iday = 0;
            //jh      g.idate = 0

            jdate = 0;

            i_def2 = 0;
            j_def = 0;
            DAS = 0;
            nsince = 0;


            tempmx = 0.0;
            tempmn = 0.0;
            solrad = 0.0;
            rain = 0.0;
            tempdy = 0.0;
            tempwt = 0.0;
            wind = 0.0;
            tempav = 0.0;
            alint = 0.0;
            hunits = 0.0;
            //jh      hucut=40.0 ! abh changed from 30 to 40 - 11/11/83  //SDML
            //jh      baset=12.0                                         //SDML
            vpd = 0.0;
            qa = 0.0;
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
            nirr = 0;
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
            dul = 0.0;
            sat = 0.0;
            wpwc = 0.0;
            
            wli = 0.0;
            f_limiting = 0.0;
            smi_row = 0.0;
            smi_pre = 0.0;
            s_bed_mi = 0.0;
            useskip = false;
            f_intz = 0.0;    //total soil cover

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
            lastlf = 0;
            ppm = 0.0;
            ppm_target = 0.0;
            //ppm_row = 0.0;  //in SDML script
            ppm_sown = 0.0;
            ppm_emerge = 0.0;
            ppm_establish = 0.0;

            alai = 0.0;
            alai_row = 0.0;
            shedlf = 0.0;
            frudw = 0.0;
            smi = 0.0;
            sdepth = 0.0;
            rtdep = 0.0;
            rtdepm = 0.0;
            shedlf = 0.0;
            s = 0.0;
            rs = 0.0;
            pp = 0.0;
            ps = 0.0;
            // nskip = 0.0; //in SDML

            n_cutout = 0;  //count days of cutout
            delay = 0.0;
            delay_emerg = 0.0;
            dd_emerg = 0.0;
            nday_co = 0;
            nwet_co = 0;
            sum_tmx = 0.0;
            ave_tx = 0.0;
            fail_emrg = 0.0;
            f_die = 0.0;

            ddw_boll = 0.0;
            ddw_leaf = 0.0;
            ddw_root = 0.0;
            ddw_root_max = 0.0;
            ddw_stem = 0.0;
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
            Array.Clear(ddw_l, 0, ddw_l.Length);
            Array.Clear(bpsum, 0, bpsum.Length);
            Array.Clear(fyzage, 0, fyzage.Length);
            Array.Clear(fruno, 0, fruno.Length);
            Array.Clear(fruwt, 0, fruwt.Length);
            Array.Clear(frmark, 0, frmark.Length);
            Array.Clear(fmkcat, 0, fmkcat.Length);
            Array.Clear(sfmcat, 0, sfmcat.Length);

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
            openwt = 0.0;
            sites = 0.0;
            frudw_shed = 0.0;
 
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
            pclint = 0.0;
            pcLint_frac = 0.0;
            carcap_c = 0.0;
            carcap_n = 0.0;

            idayco = 0;
            last_day = 0;

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
            j_pick = 0;
            n_pick = 0;
            n_def = 0;
            i_def = 0;
            
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
            dw_boll = 0.0;
            dw_leaf = 0.0;
            dw_root = 0.0;
            dw_stem = 0.0;
            dw_total = 0.0;
            boll_n = 0.0;
            leaf_n = 0.0;
            stem_n = 0.0;
            root_n = 0.0;
            total_n = 0.0;
            reserve = 0.0;
            res_cap = 0.0;
            root_feedback = 0.0;

            height = 0.0;
            //num_height = 0;

            co2 = 0.0;
            //num_co2_fert = 0;


            //TODO: DBJ  Raise event to report component initialised?
            //           and write Console line to report
            //Console.WriteLine("OZCOT InitVarsFlags() ");   // This is too early! Just after component is loaded, before any others are initialised.

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

            // SOIL WATER
            // get depth of each soil water layer
            Array.Clear(dlayr, 0, dlayr.Length);
            sendDriverRequest(DRV_DLAYR, iEventID);

            // get moist bulk density
            Array.Clear(bulkd, 0, bulkd.Length);
            sendDriverRequest(DRV_BULKD, iEventID);

            // get unavailable g.sw - use ll15 because (if) crop ll is unavailable
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

			rtdepm = 0.0;
			ul = 0.0;
			sat = 0.0;
			wpwc = 0.0;
            nlayr = dlayr.Length-1;

            for (layer = 1; layer <= nlayr; layer++)
            {
                dlayr_cm[layer] = dlayr[layer] / 10.0;
                sat_adj[layer] = (dul_layers[layer] - unul[layer]) / 0.87 + unul[layer];   // adjust to match ozcot soil characterisation
                sat_adj[layer] = Math.Min(sat_adj[layer], sat_layers[layer]);
                ullayr[layer] = sat_adj[layer] - unul[layer];   // upper limit of profile
                rtdepm = rtdepm + dlayr_cm[layer];				// depth of profile
                ul = ul + ullayr[layer] * dlayr_cm[layer];		// upper limit for profile
                wpwc = wpwc + unul[layer] * dlayr_cm[layer];
            }


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
            rain = 0.0;
            sendDriverRequest(DRV_MAXT, iEventID);
            sendDriverRequest(DRV_MINT, iEventID);
            sendDriverRequest(DRV_RADN, iEventID);
            sendDriverRequest(DRV_RAIN, iEventID);	

            // do conversions necessary
            tempav = (tempmx + tempmn) / 2.0;   // calc daily avg temp
            solrad = solrad / 0.04184;          // convert to langleys  (revised from 0.04186 Jan 2012)
            rain = rain / 10.0;                 // convert to cm


            // SOIL WATER
            // get the current amount of soil water in each layer
            Array.Clear(swlayr, 0, swlayr.Length);
            sendDriverRequest(DRV_SW, iEventID);

            // Now convert values from APSIM values to values that suit OZCOT
            //===============================================================

			// convert field capacity relative to wilting point.
			// convert units to cm and cm/cm

			rtdepm = 0.0;
			ul = 0.0;
			sat = 0.0;
			wpwc = 0.0;
            nlayr = dlayr.Length-1;

            for (layer = 1; layer <= nlayr; layer++)
            {
                dlayr_cm[layer] = dlayr[layer] / 10.0;
                sat_adj[layer] = (dul_layers[layer] - unul[layer]) / 0.87 + unul[layer];   // adjust to match ozcot soil characterisation
                sat_adj[layer] = Math.Min(sat_adj[layer], sat_layers[layer]);
                ullayr[layer] = sat_adj[layer] - unul[layer];   // upper limit of profile
                rtdepm = rtdepm + dlayr_cm[layer];				// depth of profile
                ul = ul + ullayr[layer] * dlayr_cm[layer];		// upper limit for profile
                wpwc = wpwc + unul[layer] * dlayr_cm[layer];
            }

            //  read in swlayr is in mm/mm
            //  convert water to plant available  (cm/cm)
            for (layer = 1; layer <= nlayr; layer++)
            {
                //jh        g.swlayr(layer) = g.swlayr(layer) - ll_adj(layer)
                swlayr[layer] = swlayr[layer] - unul[layer];
                swlayr[layer] = Math.Max(0.0, swlayr[layer]);
                sw_start[layer] = swlayr[layer];                  //TODO: dbj - what is this???
            }

            s_bed_mi = swlayr[1] / ullayr[1];     // seed bed moisture index


            // read other values
            sendDriverRequest(DRV_ES, iEventID);
            es = es / 10.0;  // convert es from mm to cm for OZCOT

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
            IntPtr msg;

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
			// dn_plant = u_bound(dn_plant, (trtsno3 + trtsnh4) / 10.0);
            // TODO:DBJ  set to min of (arg1, agr2)
            if(dn_plant > ((trtsno3 + trtsnh4) / 10.0))   dn_plant = ((trtsno3 + trtsnh4) / 10.0);

			for (layer = 1; layer <= nlayr; layer++) {
				if ((trtsno3 + trtsnh4 > 0.0 & layer <= nrtlayr))
				{
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

            cultivar = Parameters.member("cultivar").asString();
            sow_depth_mm = Parameters.member("sowing_depth").asDouble();
            row_space_mm = Parameters.member("row_spacing").asDouble();
            ppm_row = Parameters.member("plants_pm").asDouble();
            skiprow = Parameters.member("skiprow").asDouble();

            // Retrieve the cultivar specific parameters 
            //  and assign them to the working values
            CultivarParams sowCultivar = ozcotCultivars.getCultivar(cultivar);

            if (cultivar.ToUpper() == sowCultivar.name.ToUpper())   // Cultivar found
            {
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
            }
            else   // Cultivar not found
            {
                //   Report error and use Defaults
                Console.WriteLine("Sowing Cultivar '{0}' not found. Using default cultivar '{1}' parameters.", cultivar, cultivar_default);

                sowCultivar = ozcotCultivars.getCultivar(cultivar_default);
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
            }

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
            thisEvent.member("crop_type").setValue("Cotton");
            thisEvent.member("sender").setValue("OZCOT");
            eventList[evtNewCrop] = thisEvent;
            sendPublishEvent(evtNewCrop, false);

            // Publish a 'Sowing' event  (no parameters)
              // thisEvent = eventList[evtSowing];
              // eventList[evtSowing] = thisEvent;
            sendPublishEvent(evtSowing, false);



            crop_in = true;
            DAS = 0;
            plant_status = status_alive;
            sdepth = sow_depth_mm / 10.0;
            row_spacing = row_space_mm / 1000.0;       // row spacing in metres
            isow = jdate;
            rtdep = sdepth;
            rs = row_spacing * (2.0 + nskip) / 2.0;    // effective row spacing with skip
            ppm = ppm_row / rs;                        // effective population density
            nskip = skiprow;
            ppm_target = ppm_row / rs;                 // adjust target population for non standard rows incl skip
            pp = ppm_target * rs;                      //TODO: dbj to check calculation and application
            ps = (1.0 / rs) / pp;
            s = ps / rs;
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
        public void ozcot_end_crop()
        {
            //     ===========================================================

            //+  purpose
            //       report occurence of harvest and the current status of specific
            //       variables.

            //+  changes
            //     010994 jngh specified and programmed
            //      191200 dph  changed from unknown_module to all_active_modules
            //                  unknown_module not supported in apsim2.


            //+  calls
            //                                      ! lu_scr_sum

            //+  constant values

            //+  local variables
            // double res_dm;            // residue dry weight (kg/ha) as double
            // double res_n;             // amount of n in residue (kg/ha) as double

            //- implementation section ----------------------------------


            if (crop_in)
            {
                //         ! crop harvested. report status

                // publish event "harvesting"
                sendPublishEvent(evtHarvesting, false);

                ozcot_harvest_report();
                ozcot_harvest_update();
            }
            else
            {
            }
            return; // ozcot_end_crop
        }

        //     ===========================================================
        public void ozcot_harvest_report()
        {
            //     ===========================================================

            //+  purpose
            //       report the current status of specific
            //       variables.

            //+  changes
            //     051101 jngh specified and programmed

            //+  constant values

            //+  local variables
            // string lstring;            // local message
            // double yield;              // grain yield dry wt (kg/ha) as double
            double dm;
            double totnup;             // n uptake kg/ha as double
            double bollsc;

            //- implementation section ----------------------------------

            //call push_routine (my_name)

            //         ! crop harvested. report status
            //DBJ to raise event for harvest

            dm = dw_total * 10.0;
            totnup = total_n * 10.0;
            if ((openz > 0.0))
            {
                bollsc = openwt / openz;         // g sc/boll
            }
            else
            {
                bollsc = 0.0;
            }

            Console.WriteLine("\n      Harvest");

            Console.WriteLine("\n\n      Days after sowing      =   {0}", DAS.ToString("##0"));
            //write #1,")                  " days after sowing      = ", DAS

            Console.WriteLine("      bolls/m2               =   {0}        Lint (kg/ha)            =   {1}", openz.ToString("##0.0"),alint.ToString("#####0.0"));
            //write #1,")" bolls/m2               = ",openz                , " lint (kg/ha)           = ",alint

            Console.WriteLine("      N uptake (kg/ha)       =    {0}        bolls sc (g/boll)       =      {1}", totnup.ToString("##0.0"), bollsc.ToString("####0.0"));
            //write #1,")" n uptake (kg/ha)       = ", totnup                , " bolls sc (g/boll)      = ", bollsc

            Console.WriteLine("      max squares das (days) =   {0}          max lai das (days)      =    {1}", isqzx.ToString("##0"), ilaiz.ToString("####0"));
            //write #1,")" max squares DAS (days) = ", isqzx                , " max lai DAS (days)     = ", ilaiz

            Console.WriteLine("      maximum squares/m2     =   {0}        maximum lai (m2/m2)     =      {1}", sqzx.ToString("##0.0"), alaiz.ToString("#0.00"));
            //write #1,")" maximum squares/m2     = ", sqzx                , " maximum lai (m2/m2)    = ", alaiz

            Console.WriteLine("      Total above ground biomass (kg/ha) =  {0}", dm.ToString("#####0.0"));
            //'write #1,")                  " total above ground biomass (kg/ha) = ", dm

            Console.WriteLine("\n");


            return; // ozcot_harvest_report
        }


        //     ===========================================================
        public void ozcot_harvest_update()
		{
			//     ===========================================================

			//+  purpose
			//       report the current status of specific
			//       variables.

			//+  changes
			//     051101 jngh specified and programmed

			//+  constant values

			//+  local variables
			//       real    res_dm                  ! residue dry weight (kg/ha)
			//       real    res_n                   ! amount of n in residue (kg/ha)

			double[] fraction_to_residue = new double[max_part + 1];    // fraction sent to residue (0-1) as double
			double[] dlt_dm_crop = new double[max_part + 1];            // change in dry matter of crop (kg/ha) as double
			double[] dlt_dm_n = new double[max_part + 1];               // change in n content of dry matter (kg/ha) as double
            double[] root_length = new double[max_layers + 1];          // lenght of roots in given layer


			//- implementation section ----------------------------------

            // Publish Event 'Harvesting'
            sendPublishEvent(evtHarvesting, false);


			//      res_dm = (dw_total - openwt / rs ) * 10.
			//      res_dm = (dw_total - openwt) * 10.
			//      if (res_dm.le.0.) res_dm = 0.
			//      res_n = res_dm * 0.4 / 100.0

			Array.Clear(fraction_to_residue,0,fraction_to_residue.Length) ;
            Array.Clear(dlt_dm_crop, 0, dlt_dm_crop.Length);
            Array.Clear(dlt_dm_n, 0, dlt_dm_n.Length);

			//     ! update biomass and n pools.  different types of plant pools are
			//     ! ===============================================================
			//     ! affected differently.
			//     ! =====================

			dlt_dm_crop[root] = dw_root * gm2kg / sm2ha;
            dlt_dm_n[root] = root_n * gm2kg / sm2ha;
			fraction_to_residue[root] = 0.0;

			dlt_dm_crop[meal] = openwt * gm2kg / sm2ha;
			dlt_dm_n[meal] = dlt_dm_crop[meal] * 0.4 / 100.0;
			fraction_to_residue[meal] = 0.0;

			dlt_dm_crop[stem] = dw_stem * gm2kg / sm2ha;
            dlt_dm_n[stem] = stem_n * gm2kg / sm2ha;
			fraction_to_residue[stem] = 1.0;

			dlt_dm_crop[leaf] = dw_leaf * gm2kg / sm2ha;
            dlt_dm_n[leaf] = leaf_n * gm2kg / sm2ha;
			fraction_to_residue[leaf] = 1.0;

			dlt_dm_crop[pod] = (dw_boll - ddw_boll- openwt)*gm2kg/sm2ha;
            dlt_dm_crop[pod] = Math.Max(dlt_dm_crop[pod], 0.0); 
			dlt_dm_n[pod] = boll_n * gm2kg/sm2ha - dlt_dm_n[meal];
            dlt_dm_n[pod] = Math.Max(dlt_dm_n[pod], 0.0); 
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


            ozcot_root_distrib (ref root_length , (dlt_dm_crop[root] * kg2gm/ha2sm) );

            //
            //TODO: DBJ find what this is and implement it!!
            // APSIM-OZCOT  original code
                     //       crop_root_incorp (
                     //.          dlt_dm_crop(root) * kg2gm/ha2sm
                     //:         ,dlt_dm_N(root) * kg2gm/ha2sm
                     //:         ,g%dlayr
                     //:         ,root_length
                     //:         ,g%rtdep
                     //:         ,c%crop_type
                     //:         ,max_layers
                     //:         ,id%IncorpFOM)

            //DDML for new IncorpFOM event
//                public const string typeIncorpFOM = @"<type name=""IncorpFom"">
//                                                <field name=""Type""   kind=""string"" /> 
//                                                <field name=""Layer""  array=""T"">
//                                                    <element>
//                                                        <field name=""FOM"" kind=""defined"">
//                                                            <field name=""amount""  kind=""single"" unit=""kg/ha"" /> 
//                                                            <field name=""C""       kind=""single"" unit=""kg/ha"" /> 
//                                                            <field name=""N""       kind=""single"" unit=""kg/ha"" /> 
//                                                            <field name=""P""       kind=""single"" unit=""kg/ha"" /> 
//                                                            <field name=""AshAlk""  kind=""single"" unit=""kg/ha"" /> 
//                                                        </field>
//                                                        <field name=""CNR""     kind=""single"" /> 
//                                                        <field name=""LabileP"" kind=""single"" /> 
//                                                    </element>
//                                                </field>
//                                            </type>";
            


            // Publish a 'IncorpFom' event  
            // need to set up the arrays of values for this event
            TTypedValue IncorpFOMEvent = eventList[evtIncorpFOM];
            IncorpFOMEvent.member("Type").setValue(crop_type);     //Cotton  - APSIM Soil probably treats this as 'Default'
            IncorpFOMEvent.member("Layer").setElementCount((uint) nlayr);

            //TODO: DBJ  need the APSIM logic for 'crop_root_incorp (...)' to calculate layer values for "amount" and "N"
            //                                     from dlt_dm_crop[root] and dlt_dm_n[root] OR  from total values?
            //for (uint i = 1; i <= nlayr; i++)
            //{
            //    IncorpFOMEvent.member("Layer").item(i).member("FOM").member("amount").setValue(dlt_dm_crop[root]);  //this is a total value reported for EACH layer
            //    IncorpFOMEvent.member("Layer").item(i).member("FOM").member("N").setValue(dlt_dm_n[root]);          //this is a total value reported for EACH layer
                
            //    //IncorpFOMEvent.member("Layer").item(i).member("CNR").setValue(??);
            //}
            
            eventList[evtIncorpFOM] = IncorpFOMEvent;

            sendPublishEvent(evtIncorpFOM, false);
	  

            
            // Set these only on an END_CROP event handler
            //   (in harvest only zero variables that are removed in harvest)
            //   Note: Harvest_Update ONLY called from End_Crop currently
			crop_in = false; 
			plant_status = status_out;
			zero_variables = true;
			iend = 0;      //ozcot_status reset to zero

            openwt = 0.0;
            total_n = 0.0;
            boll_n = 0.0;
            leaf_n = 0.0;
            root_n = 0.0;
            stem_n = 0.0;
            dw_boll = 0.0;
            dw_leaf = 0.0;
            dw_root = 0.0;
            dw_stem = 0.0;
            ddw_boll = 0.0;
            ddw_leaf = 0.0;
            ddw_root = 0.0;
            ddw_stem = 0.0;
            dw_total = 0.0;
            leaf_res = 0.0;
            leaf_res_n = 0.0;
            Array.Clear(dlai, 0, dlai.Length);



			return; // ozcot_harvest_update
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

            //      dlt_dm_crop(root) = g%dw_root * gm2kg/sm2ha
            //      dlt_dm_N(root) =  dlt_dm_crop(root) * 0.4 / 100.0
            //      fraction_to_Residue(root) = 1.0
            //
            //      dlt_dm_crop(meal) = g%openwt * gm2kg/sm2ha
            //      dlt_dm_N(meal) =  dlt_dm_crop(meal) * 0.4 / 100.0
            //      fraction_to_Residue(meal) = 0.0
            //
            //      dlt_dm_crop(stem) = g%dw_stem * gm2kg/sm2ha
            //      dlt_dm_N(stem) = dlt_dm_crop(stem) * 0.4 / 100.0
            //      fraction_to_Residue(stem) = 1.0

            dlt_dm_crop[leaf] = leaf_res * gm2kg/sm2ha;
            dlt_dm_n[leaf] = leaf_res_n * gm2kg/sm2ha;
            fraction_to_residue[leaf] = 1.0;

            //      dlt_dm_crop(pod) = (g%dw_boll - g%openwt) * gm2kg/sm2ha
            //      dlt_dm_crop(pod) = l_bound(dlt_dm_crop(pod), 0.0)
            //      dlt_dm_N(pod) = dlt_dm_crop(pod) * 0.4 / 100.0
            //      fraction_to_Residue(pod) = 1.0
            //     call crop_top_residue (c%crop_type, dm_residue, N_residue)


            //TODO: DBJ  implement Crop_Chopped event   

            if (dlt_dm_crop.Sum() > 0.0)
            {
                //    call Send_Crop_Chopped_Event
                //:             (c%crop_type
                //:            , part_name
                //:            , dlt_dm_crop
                //:            , dlt_dm_N
                //:            , fraction_to_Residue
                //:            , max_part)
            }
          else
            {
               // no surface residue
            }

         

            ddw_boll = 0.0;
            ddw_leaf = 0.0;
            ddw_root = 0.0;
            ddw_stem = 0.0;


			return; // ozcot_update
		}



        //     ===========================================================
        public void ozcot_root_distrib(ref double [] root_array, double root_sum)
		{
		//     ===========================================================

        //*+  Sub-Program Arguments
        //      real       root_array(*)         ! (OUTPUT) array to contain distributed material
        //      real       root_sum              ! (INPUT) Material to be distributed

        //*+  Purpose
        //*       Distribute root material over profile

        //*+  Mission statement
        //*       Distribute root material over profile

        //*+  Changes
        //*       290994 jngh specified and programmed

        //*+  Constant Values
        //      
        //      

        //*+  Local Variables
        //      real       cum_depth             ! cumulative depth (mm)
        //      integer    layer                 ! layer number ()
        //      integer    deepest_layer         ! deepest layer in which the roots are
        //                                       ! growing
        //      real       root_distrb(max_layers) ! root distribution ()
        //      real       root_distrb_sum       ! sum of root distribution array

        //      real       c_root_extinction
        //      parameter (c_root_extinction = 3.0)

            double  cum_depth;
            int     layer;
            int     deepest_layer;

            double [] root_distrb = new double[max_layers + 1];
            double root_distrb_sum;

            const double c_root_extinction = 3.0;


            //*- Implementation Section ----------------------------------

            Array.Clear(root_array, 0, max_layers);
            Array.Clear(root_distrb, 0, max_layers);

            deepest_layer = mvDataFunctions.find_layer_no(rtdep, dlayr, max_layers);
            cum_depth = 0.0;
            for (layer = 1 ; layer <= deepest_layer; layer++)
            {
                 cum_depth = cum_depth + dlayr[layer];
                 cum_depth = Math.Min(cum_depth, rtdep);  //u_bound?
                 root_distrb[layer] = Math.Exp(-c_root_extinction *  mvDataFunctions.divide (cum_depth, rtdep, 0.0));
            }


            root_distrb_sum = mvDataFunctions.SumArray(root_distrb, deepest_layer);
            for (layer = 1 ; layer <= deepest_layer; layer++)
            {
                root_array[layer] = root_sum * mvDataFunctions.divide(root_distrb[layer], root_distrb_sum, 0.0);
            }


			return; // ozcot_root_distrib
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

                //      do 10 nszn = 1,100                     ! seasonal loop
                //          do 20 i = 1,1000                   ! daily loop through whole year
                //             call metdat2 (i,iend)       ! get met data
                //              if(iend.eq.1)go to 31       ! end of met data
                ozcot_metdat2();
                //             if(defirr(2).ne.0.) call decide_irg (i)  ! irrigated crop?
                //              call solwat (i,dayir,npre)  ! soil water
                ozcot_solwat();
                // soil water
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
            if (crop_in)
            {
                if ((isow > 0) & (DAS > 0)) ozcot_pltgrw();
                //              if(iend.eq.2)  go to 32     ! end of season
                //              if(iend.ne.2)  then
                //                call dayout2(i)             ! daily output
                //psc                call dayout2             ! daily output
                //               call day_dudley             ! output for norm dudley
                //20             continue                           ! end of daily loop
                //                return
                //              endif
                //32           continue
                //              call yield(nszn,iend)           ! calculate yield
                //              call reset(iend)                ! reset variables for new season
                ozcot_yield();
                // calculate yield
            }
            //psc              call reset                      ! reset variables for new season
            //10       continue                               ! end of seasonal loop
            //31        continue
            //          call exit
            //      stop
            else
            {
            }

            return; // TODO: might not be correct. Was : Exit Sub
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
			double rtdep2;
			double lbollgr;


			//- implementation section ----------------------------------

            ozcot_update();

			//psc      iday=i-isow ! replaced ncrpdy throughout, 15 nov 1983
			iday = DAS;

			if (iday == 300)
			{
				iend = 10;
				// terminate crop growth
				//    'open report.txt for append as #1
				//    write#1,            ' mature bolls will be forced open.
				//format(' *** season > 300 days; terminate cro')
				//    call write_string #1,
				//   call pop_routine(myname)
				//    close #1
				return; 
			}

			//---- crop development complete? ---------------------------------------------
			//      if(openz.gt.0.0 .and. bollz.lt.1.0 .and. iend.eq.0) then
			//jh skip row correction
			if (openz > 0.0 & bollz * rs < 1.0 & iend == 0)
			{
				iend = 6;
				// bolls/m < 1; end of season
				//write#1,              ' mature bolls will be forced open.
				//format(' *** all bolls open; crop finished.')
				//call write_string #1,
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

			//----- increase root depth ----------------------------------------------------

			rtdep = sdepth + ((20.0 - sdepth) / 36.0) * (double)iday;
			// g da r from 82/83 expt
			rtdep2 = rtdepm * (1.0 - 2.65 * Math.Exp(-0.03 * (double)iday ));
			// rtdepm replaced 122.38 12/5/95
			if (rtdep2 > rtdep) rtdep = rtdep2; 
			// added 12/5/95

			//jh v2001      if(iday.le.36)rtdep=sdepth+((20.-sdepth)/36.)*real(iday) ! w*n          !const  rtdep_das_crit, rtdep_sdepth_crit
			//jh v2001!cpsc        changed maximum rooting depth
			//jh v2001!cpsc  if(iday.ge.37.0)rtdep=122.38*(1.-2.65*exp(-.03*iday)) ! 82/8
			//jh v2001      if(iday.ge.37)rtdep=rtdep_max
			//jh v2001     :                       *(1.-2.65*exp(-.03*real(iday))) ! 82/8                !const
			if ((rtdep > rtdepm)) rtdep = rtdepm; 


			//---- check if frost terminates crop -----------------------------------------

			if (tempmn <= frost_kill_immediate & iemrg > 0.0)
			{
				// frost after emergence?
				if ((bollz == 0))           // pre-fruiting?
				{
					iend = 2;				// flag for frost -
				}
				//write#1,//format(' *** crop killed by frost before fruitin')
                else if ((openz == 0.0)) {	// green bolls yet?
					iend = 2;				// flag for frost - force open bolls > 80. mature
				}
				//write#1,//format(' *** crop killed by frost during fruitin')
				else                       // open bolls yet?
                {
					iend = 2;              // flag for frost - force open bolls > 80. mature
					//write#1,//format(' *** crop terminated by frost.')
				}
			}

			
			
			


			//---- if hail damage do here   ----------------------------------------------

			//jh      if(hail) call hail                ! hail in karyl's expts

			//----- emergence ( why not call ozcot_emerg here ------------------------------------

			if (iemrg <= 0) ozcot_emerg(); 			// crop emerged yet? emerge today?
			if (DAS == iemrg) ddmerg = sumdd - dd; 

			//----- increase leaf area -----------------------------------------------------
			//      if(i.le.ilai)then
			//       call actlai(i)
			//       call actlai
			//      else
			//       call laigen(i)
			//      end if

            //----- increase leaf area -----------------------------------------------------
            ozcot_laigen();

            //----- crop nitrogen ---------------------------------------------------------
            ozcot_cropn();

            //---- grow plant -------------------------------------------------------------

            if ((isq == 0))
            {
                ozcot_istsq();          // call istsq (i,nszn)
            }
            else
            {
                if ((DAS > isq)) ozcot_fruit();   // fruiting has started
            }

            if ((openz > 0.0)) ozcot_harvest();   // test for harvest condition

            lbollgr = bollgr;
            ozcot_dryxmatter();
            ozcot_plant_n();
            bollgr = lbollgr;

            //      if(isyr.eq.82 .and. jdate.eq.354) call hail(i) ! hail in 1982-83 experiment
            //      if(isyr.eq.82 .and. jdate.eq.354) call hail    ! hail in 1982-83 experiment

            //------ following are for use in s/r yield -----------------------------------

			if (alai > alaiz)
			{
				alaiz = alai;				// max lai
				ilaiz = iday;				// day of max lai
				plntnz = plantn;			// plantn on day of max lai
				iplntn = ilaiz;
			}

			if (squarz > sqzx)
			{
				sqzx = squarz;				// peak square numbers
				isqzx = iday;				// day of peak squares
			}

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


            //     ! locals (i hope!)
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

            alai_row = alai;
            if (nskip > 0) alai_row = alai * rs;                     // lai in hedgerow
            alight = (1.0 - Math.Exp(-ozcot_kvalue * alai_row));     // original code  - now gives interception in hedgerow
                                                                       
            if (nskip > 0)
            {
                alight = alight / rs;                // interception on ground area basis
                //jh         alai = alai/rs         ! restore lai to ground area basis
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
            //jh need rs correction
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
            
            //-------------------------------------------------------------------------------
            //jh v2001 not used      if(carcap_c .gt. 0.)then
            //jh v2001 not used        fruindex1 = 1. - bload / cutout
            //jh v2001 not used        fruindex2 = 1. - bload / carcap_c
            //jh v2001 not used      else
            //jh v2001 not used        fruindex1  = -1.0
            //jh v2001 not used        fruindex2  = -1.0
            //jh v2001 not used      end if

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
            frun = (frudw + openwt) * ((1.0 - pcLint_frac) * seed_nc + (fburr - 1.0) * 0.005);    // n in fruit           'const

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
				//jh v2001 deleted      else
				//jh v2001 deleted !c----- simple heat sum as an alternative to wanjura's function -----

				//jh v2001 deleted         if(sumdd .lt. 60.) then                                            !const
				//jh v2001 deleted            !return
				//jh v2001 deleted         else
				//jh v2001 deleted            iemrg = DAS
				//jh v2001 deleted         endif
			}

			//--------------------------------------- ------------------------------------------------------


			if ((initial_call == true))     // first call of season
			{
				initial_call = false;      // reset flag for next season
				nday_co = 0;               // counter for days to emergence
				nwet_co = 0;               // counter for wet days to mergence
				sum_tmx = 0.0;
				ave_tx = 0.0;
				delay_emerg = 0.0;
				dd_emerg = (sdepth / (rate_emergence/10.0)) * (6.0 / 5.0);   //sdepth in cm,  rate_emergence converted mm to cm
				// 0.1 is from linear increase phase of wanjura 1970
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
			ave_tx = sum_tmx / nday_co;                 // mean tmax during mergence
			if ((ave_tx > 20.0)) delay_emerg = (ave_tx - 20.0) * 2.5; // increase dd requirement - anderson"s data

			if ((sumdd < dd_emerg + delay_emerg)) return;  // does not emerge today

            //------ crop emerges ---------------------------------------------------------------------------

			iemrg = DAS;

			//      if(mode.le.1) then
			//         ppm = ppm_target                       ! input gives established population
			//          write(2,146)  jdate,iday,ppm
			//  146     format(i4,i6,' *** crop emerged with',f5.1,
			//     *                 ' plants per m sq.')
			//jh      if(mode.le.2) then                         ! mpb setting population in management mode
			//jh          ppm = ppm_target                       ! input gives established population
			//jh          write(2,146)  jdate,iday,ppm
			//jh 146     format(i4,i6,' *** crop emerged with',f5.1,
			//jh     *                 ' plants per m sq.')
			//jh      else                                       ! mode=2, simulate population

			//------ estimate emergence ---------------------------------------------------------------------

			fail_emrg = 0.015 * (double)(nday_co - 7);			// slow emergence; constable 1976
			fail_emrg = fail_emrg + 0.03 * (double)(nwet_co);	// wet conditions; constable 1976
			if ((fail_emrg > 1.0)) fail_emrg = 1.0; 
			//jh v2001
			if ((fail_emrg < 0.0)) fail_emrg = 0.0; 
			ppm_sown = ppm_target / 0.8;			       // seeds sown - assume 80. emerge
			ppm_emerge = ppm_sown * (1.0 - fail_emrg);	   // adjust ppm for failure to emerge

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

			ppm_establish = ppm_emerge * (1.0 - f_die);			// adjust ppm for seedling death
			ppm = Math.Min(ppm_target, ppm_establish);			// to avoid confusion, ppm cannot exceed target

			//jhtemp disable establishment
			ppm = ppm_target;

			establish = ppm * 100.0 / ppm_target;			// percentage established

            // report emergence
            Console.WriteLine("{0}/{1}/{2} (Day of year={3}), cotton:", Today.Day, Today.Month, Today.Year, DOY);
            Console.WriteLine("    *** Crop emerged with {0} plants per m sq, {1}% of target population of {2}", ppm.ToString("#0.0"), establish, ppm_target.ToString("#0.0"));
            Console.WriteLine("");

			if ((establish < 25.0))
			{
				iend = 5;				// flag to terminate season

                // report failed establishment
                Console.WriteLine("{0}/{1}/{2} (Day of year={3}), cotton:", Today.Day, Today.Month, Today.Year, DOY);
                Console.WriteLine("    *** crop failed to establish;  stand was less than 25% of target population of {0} plants per m sq", ppm_target);
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
			double xn1;
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


			xn1 = 0.404 * Math.Log(s) + 1.49;
			alai_row = alai;
			if ((nskip > 0)) alai_row = alai * rs; 		  // lai in hedgerow
			if ((alai_row < xn1))                         // when lai below xn1 threshold
			{
				tr = Math.Exp(-0.6 * alai_row);
			}
			else                                          // when lai above xn1 threshold
			{
				tr = Math.Exp(-0.398 * alai_row);
			}

			if ((nskip > 0))
			{
				f_int = 1.0 - tr;				// intercetion in hedgerow
				f_int = f_int / rs;				// interception on ground area basis
				tr = 1.0 - f_int;				// transmission on ground area basis
			}
			//jh         alai = alai/rs        ! restore lai to ground area basis
			else
			{
				f_int = 0.0;
			}

			f_intz = Math.Max(f_intz, f_int);

			//      if(rs.gt.1.) tr = (tr+rs-1.)/rs ! adjust for rows wider than im

			//-----vapor pressure deficit: mean of vpd at 9am and 3pm ,assuming atmos.
			//     v(vpdry) remains constant throughout day, and tmax=t3pm-----------------

			svpmax = ozcot_satvp(tempmx);			// sat vp at tmax=sat vp at t3pm
			svpdry = ozcot_satvp(tempdy);			// sat vp at 9am tdry
			svpwet = ozcot_satvp(tempwt);			// sat vp at 9am twet

			vpdry = svpwet - gamma * (tempdy - tempwt);	 // atmospheric vp
			if ((vpdry <= 0.0)) vpdry = 0.1; 			 // cannot be -ve.
			vpd = ((svpdry - vpdry) + (svpmax - vpdry)) / 2.0;

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

			//jhnote test removing this
			eo = ho * d + at;
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
            double functionReturnValue = 0.0;
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
            functionReturnValue = 0.0;
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
                    sites1 = sites1 + cutout_smi_site_red * size * ppm;     // inactive sites  
                }
                return functionReturnValue;
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

            size = (sites - sites1) / ppm;            // active sites per plant for frugen & surviv
            if ((size < 1.0)) size = 1.0;             // average  plants has 1 site
            if ((size < 0.5 / ppm)) size = 0.5 / ppm; // average  plants has 1 site   

            dfru = sqcon * Math.Sqrt(size) * ppm * (1.0 - blr);     // sites per dd
            vsnstr = ozcot_stress(vsnstr_low, vsnstr_high, vsnstr_a, vnstrs); // vegetative n stress             dfru = dfru * vsnstr;

            ppm_row = ppm * rs;                      // plants per m row for popfac
            popfac = 1.0 / (1.0 + popcon * ppm_row); // plant population factor within row
            dfru = dfru * popfac;                    // adjust for plant population
            
            //TODO: DBJ rename

            functionReturnValue = dfru * dd;         // today"s squares
            if ((functionReturnValue < 0.0)) functionReturnValue = 0.0;
            return functionReturnValue;

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
			double fruit_wt_shed;


			//- implementation section ----------------------------------

			//----  re-intialise arrays and variables to zero where appropriate -------------

			//      if(i.eq.isq+1)then        ! 1st call of new season on day after 1st square
			//          do 10 n=1,300
			//              bpsum(n)=0.0
			//10        continue
			//          idayx = 0             ! flag to bollwt, called for 1st time this day
			//      endif


            // Each day initialise arrays and local variables.
            // These are re-populated in the loops that follow.
            squarz = 0.0;
            cohortsInFlower = 0;
            frudw = 0.0;
            Array.Clear(frucat,0,frucat.Length);
            Array.Clear(sfmcat, 0, sfmcat.Length);
            Array.Clear(fmkcat, 0, fmkcat.Length);

			//---- compute boll period ------------------------------------------------------

			bper = Math.Exp(5.385 - 0.0512 * tempav);			// boll period constable
			bper = bper * (frudd[inedible_bolls] - frudd[large_sqz]) / 788.0;   // adjust for variety
			bper = 1.0 / bper;                  			// fraction for day

            // Calculate the nominal(reference)(bgrvar) and actual (bollgr) boll growth rates for today 
            // Note: this is based on the existing fruit load and carrying capacities (from yesterday) 
            ozcot_bollwt();

            // Clear the green boll count (used in bollwt() to calc boll stress)
            bollz = 0.0;


			//-----------------------------------------------------------------------------
			//     the next loop ( do 200 l=....) goes through the fruit arrays
			//     starting with the element with oldest fruit and therefore the smallest
			//     index, in order to develop or abscise fruit, and put in categories.
			//     before entering the loop, find the oldest fruit.
			//-----------------------------------------------------------------------------

			lf = lfru[open_bolls];		        // 1st active element in fruit array, oldest immature fruit
			//psc      if(lf.eq.0)lf=isq-isow-10 ! start in fruno 10 days before 1st square
			if ((lf == 0)) lf = isq - 10; 		// start in fruno 10 days before 1st square             'const

			//psc      do 200 l=lf,i-isow-1 ! loop from oldest growing boll to previous day
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

                    fruit_wt_shed = fruwt[cohort] * frmark[cohort, age7] / fruno[cohort];	// adjust wt sheds
                    frudw_shed = frudw_shed + fruit_wt_shed;
                    fruwt[cohort] = fruwt[cohort] - fruit_wt_shed;					// adjust wt sheds
                    fruno[cohort] = fruno[cohort] - frmark[cohort, age7];			// remove marked fruit
                }

                //
                //------  increase in weight of bolls (seed cotton) ---------------------------
                if (cohort <= lfru[flowers])   // for cohorts older (smaller cohort#) than flowers: ie. bolls
                {
                    fruwt[cohort] = fruwt[cohort] + fruno[cohort] * bollgr;     // increase today for cohort day bolls
                }

                //
                //------ shed fruit not growing ------------------------------------------------
                //
                //  If Category 5 and 6 bolls (med and large bolls) are not effectively growing (very small size), they should be shed tomorrow
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
                if ((n_def > 0))  	            // develop faster after defoliation 
                {
                    bpsum[cohort] = bpsum[cohort] / 0.99;
                }

                // frost or last few late opening bolls
                if ((iend == 2 | iend == 6))   // frost or green bolls < 1/mtr with open bolls (crop complete)
                {
                    if ((bpsum[cohort] >= 0.9)) bpsum[cohort] = 1.0; 	// crop finishing; opens phys mat bolls
                }

                // carbon supply stress or water stress  (code added by MPBange & SMilroy in 2002 'fix' code)
                if ((bload > carcap_c) | (f_limiting < 1.0))         // carbon or water stress
                {
                    bpsum[cohort] = bpsum[cohort] / 0.98;           // accelerate bolls in this cohort
                    if (bpsum[cohort] >= 0.9) bpsum[cohort] = 1.0;  // force open near physiologically mature bolls
                }

                    // commented out code replaced by code following  -  dbj 29/8/2007 (code developed for stand-alone OZCOT)
                 
                    //    //---- sort fruit and marked fruit into age categories  -----------------------

                    //    //         ! determine which fruit age category this cohort is in
                    //    for (cat = 1; cat <= max_categories - 1; cat++)
                    //    {     // ! stay in loop until category found
                    //        if ((cat < flowers))  // if yes, squares
                    //        {
                    //            if ((fyzage[cohort] < frudd[cat])) goto L204;
                    //        }
                    //        else                  // no, therefore flowers or bolls
                    //        {
                    //            if ((bpsum[cohort] < bltme[cat])) goto L204;
                    //        }
                    //    }

                    //    //------- if last loop completed, fruit are open bolls -------------------------

                    //    if ((cohort <= lfru[open_bolls])) goto L200; // this days bolls already open?
                    //    lfru[open_bolls] = cohort;			         // this days bolls open today, reset marker
                    //    //   if(i.le.idate)go to 202   ! use actual counts or not?
                    //    openz = openz + fruno[cohort];				 // simulated bolls open added to total
                    //    openwt = openwt + fruwt[cohort];
                    //    frucat[cat] = frucat[cat] + fruno[cohort];   // sum fruit nos in categories

                    //    //202       continue

                    //    fruno[cohort] = 0.0;				     // delete open bolls from array
                    //    fruwt[cohort] = 0.0;
                    //    goto L200;
                    //L204:

                    //    continue;
                    //    if ((cohort <= lfru[cat])) goto L206; 	// this days fruit in category cat  yet?
                    //    lfru[cat] = cohort;				        // this days fruit into category cat today.
                    //    if ((cat != flowers)) goto L206; 		// category is flowers?
                    //    ndayfl = ndayfl + 1;
                    //L206:
                    //    // count this days flowering
                    //    //          if(i.gt.idate)go to 206   ! using actual counts or not?
                    //    //          fruno(l)=0.0              ! clear for actual counts
                    //    //          go to 200

                    //    continue;
                    //    frucat[cat] = frucat[cat] + fruno[cohort];		        // sum fruit nos in categories
                    //    if ((cat >= flowers)) frudw = frudw + fruwt[cohort]; 	// sum dry wt of fruit
                    //    if ((cat > large_bolls)) break;

                    //    for (age = 2; age <= 6; age++)
                    //    {  					    // loop thro marked fruit
                    //        fmkcat[cat, age] = fmkcat[cat, age] + frmark[cohort, age];	// sum marked fruit
                    //        sfmcat[cat] = sfmcat[cat] + frmark[cohort, age];            // sum marked fruit for bload
                    //    }
                    //} // end of cohort loop from oldest growing boll to previous day

                    ////---- total squares, green (growing) bolls, open bolls---------------------

                    //squarz = 0.0;     // reset
                    //bollz = 0.0;
                    //bload = 0.0;
        			
                    //for (cat = 1; cat <= max_categories - 1; cat++) {
                    //    if ((cat <= large_sqz)) squarz = squarz + frucat[cat]; 	                    // total squares
                    //    if ((cat >= flowers & cat <= inedible_bolls)) bollz = bollz + frucat[cat]; 	// total bolls
                    //    bload = bload + (frucat[cat] - sfmcat[cat]) * wt[cat];                      // boll load
                    //}


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
                if ((cohort > lfru[cat]))
                {
                    // cohort has changed categories and sets a new top marker for the category
                    lfru[cat] = cohort;

                    //  count the number of cohorts that become flowers today
                    //     count needed for use with fruit counts - yet to be re-implemented (dbj)
                    if ((cat == flowers)) cohortsInFlower = cohortsInFlower + 1;

                    //  if fruit just opened as a boll, then add this to the 'opened' bucket which is not reset each day and so needs to be added at point of change
                    if ((cat == open_bolls))
                    {
                        openz = openz + fruno[cohort]; 	// total open bolls
                        openwt = openwt + fruwt[cohort];
                        //  following lines commented out 16/07/02 by dbj as fruit counts are needed for lint quality analysis
                        //  !c  fruno(cohort)=0.0     ! delete open bolls from array
                        //  !c  cgrfruitweight(cohort)=0.0
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
            if ((firstFlowerDOY == 0) & (bollz > (ppm / 2.0)))
            {
                // mark first flower as date DAS (50% of plants)
                firstFlowerDOY = jdate;
                firstFlowerDAS = DAS;
            }

            // Open Bolls
            if ((firstOpenBollDOY == 0 & openz > (ppm / 2.0)))
            {
                // mark first open boll as date DAS (50% of plants)
                firstOpenBollDOY = jdate;
                firstOpenBollDAS = DAS;
            }


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

                bload = bload + fruitCount * wt[idxFruitCat];   //  boll load = fruit num * wt (rel_C_Demand of category, varietal parameter)
            }


        //************* end of rehashed code  **********************************
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
			
            // Calculate this day's production of fruit (sites)
            fruno[DAS] = ozcot_frugen(DAS);
			// ozcot_frugen is function generating squares
			//      end if

			//---- mark this day's fruit for physiological shedding      -----------------

            // surv = ozcot_survive(carcap_c, bload);  // calculate background shedding (shedding = 1 - survival rate)
            surv = ozcot_survive();  // calculate background shedding (shedding = 1 - survival rate)
			// square survival rate

			//      if(jdate.lt.59) surv = 0.1       ! for krs with delayed protection
			//      surv =surv*0.33                  ! for pest damage in 1972/3 namoi

			//      frmark(i-isow,1)=fruno(i-isow)*(1.-surv)
			//      if(frmark(i-isow,1).lt.0.) frmark(i-isow,1)=0.0
			//      fmkcat(1,1)=fmkcat(1,1)+frmark(i-isow,1)
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

            //TODO: dbj  Following values may need/have been adjusted if fruit counts are being used
			sites = sites + fruno[DAS];
			squarz = squarz + fruno[DAS];
			frucat[small_sqz] = frucat[small_sqz] + fruno[DAS];

            //TODO:  dbj  Check if maturity date is set correctly elsewhere (or needs to be here as per stand-alone version)
                    ////       !!
                    ////       !!  set day of maturity if it has not been set and we have reached the % of open bolls
                    ////       !!  ----------------------------------------------------------------------------------
                    //if ((riozimaturitydoy == 0))
                    //{
                    //    if ((cgropenbollcnt / (cgrgrnbollcnt + cgropenbollcnt) > rrozrdefoliateperc / 100.0 & riozidefoliantspraycnt == 0))
                    //    {
                    //        riozimaturitydoy = utcurrentdate.doy;
                    //        // day ozrdefoliateperc percentage of bolls are open
                    //    }
                    //}


            return; 
		}





        // ====================================================================
        //      subroutine harvest(iend)
        public void ozcot_harvest()
		{
			// ====================================================================

			//     this subroutine simulates defoliation and picking
			//     use n_def, n_pick and j_pick for cost of defoliation:       nb action
            //
            //     subjects for future development in a more comprehensive whole farm
			//     management model.



			//- implementation section ----------------------------------

			//jh skip row correction
			alai_row = alai;
			if (nskip > 0) alai_row = alai * rs; 		// lai in hedgerow

			if ((openz / (bollz + openz) > open_def / 100.0) & (n_def == 0))
			{
				j_def = iday;				     // day open_def. open
				//jh skip row correction
				if ((alai_row < 0.5))
				{
				}
                else if ((n_def == 0)) {
					n_def = 1;					// 1st defoliant spray     'const
					i_def = iday;				// day of 1st defol.
                    Console.WriteLine("{0} {1} {2} (Day of year={3}), cotton: ", Today.Day, Today.ToString("MMMM"), Today.Year, DOY);
                    Console.WriteLine("         Defoliant spray   {0}", n_def);
				}
                  
				else
				{
				}
			}
            else if ((n_def == 1 & iday - i_def == 10)) {	// 10 days since 1st defoliation?
				//jh skip row correction
				if ((alai_row > 0.2))
				{
					n_def = 2;					// 2nd defoliant spray      
					i_def2 = iday;				// day of 2nd defol
                    Console.WriteLine("{0} {1} {2} (Day of year={3}), cotton: ", Today.Day, Today.ToString("MMMM"), Today.Year, DOY);
                    Console.WriteLine("         Defoliant spray   {0}", n_def);
                }
				else
				{
					j_pick = jdate;				// date of picking
					//jh need rs correction   ??? TODO:  check for correction
					if ((bollz < 10))          // 10 bolls worth picking
					{
						n_pick = 1;			   // count picks 
                    }
					//write #1,")"first pick ", iday," days from sowin "                      +  "there are not enough bolls for a 2nd pick."
					else                      // 10 bolls worth picking
					{
						n_pick = 2;			  // count picks 
						//write #1,")"first pick ", iday," days from sowin "                      +  "there are enough bolls for a 2nd pick."
					}
				}
			}
            else if ((n_def == 2 & iday - i_def2 == 10)) {
				j_pick = jdate;				// date of picking
				//jh need rs correction  ???  TODO: check for correction
				if ((bollz < 10))           // 10 bolls worth picking
				{
					n_pick = 1;				// count picks
				}
				//write #1,")"first pick ", iday," days from sowin "                   +  "there are not enough bolls for a 2nd pick."
				else
				{
					n_pick = 2;				// count picks
					//write #1,")"first pick ", iday," days from sowin "                   +  "there are enough bolls for a 2nd pick."
				}
			}

			//jh      if(j_pick.ne.0 .and. bollz.lt.1.0) then
			if ((j_pick != 0))
			{
				iend = 6; 				// terminate crop.  BYPASS 2nd Pick logic as obsolete!   
                Console.WriteLine("{0} {1} {2} (Day of year={3}), cotton: ", Today.Day, Today.ToString("MMMM"), Today.Year, DOY);
                Console.WriteLine("         Crop to be picked and terminated.");
            }

			return; 
		}


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


			//      real ddisq

			//- implementation section ----------------------------------

			//jh      if(idate.ne.0) go to 40

			//     no counts - simulate first square

			//      if(iszn.ne.nszn) then                ! is this a new season?
			//          iszn = nszn                      ! reset flag
			//          delay = 0.0                      ! delay in day degrees
			//      end if

			//psc    add delay to common block

			if (smi < smi_delay_crit)           // water stress delays squaring
			{
				delay = delay + (1.0 - (smi / smi_delay_crit)) * hunits;
			}

			// cold shock constable (pers. comm)
            if (tempmn < cold_shock_delay_crit)
            {
                delay = delay + cold_shock_delay;
            }
			
			//      if(hail) delay = delay + tipout + hail_lag    ! tipping out delay
			if (sumdd < ddisq + delay)
			{
				return; 
			}
			else
			{
			}


			//      fruno(i-isow)=1.0*ppm  ! average plant has 1 square
			//      fruno(i-isow)=0.5      ! as in 1983/84 version
			//  fruno[DAS] = 1.0 * ppm;			// average plant has 1 square
			//  fruno[DAS] = 0.5 * ppm;			// 50% plants have 1 square

            // TODO:  check for correction for row spacing
			//jh need rs correction ??? this has 0.5 fruit/m2 regardless of rowspacing
			fruno[DAS] = 0.5;			    // as in 1983/84 version


			//     square & site production on first day

			//      squarz=squarz+fruno(i-isow) ! sum squares
			//      sites=sites+fruno(i-isow) ! sum sites
			//      isq=i                     ! should be isq=iday see above
			squarz = squarz + fruno[DAS];		// sum squares
			sites = sites + fruno[DAS];			// sum sites
			isq = DAS;			                // should be isq=iday see above

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
            //     areas are square m leaf per square m land (alai & dlai)
            //     or square m per site (dlds & dlds_max)

            //     revised by abh april 1995

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
           // int idx;
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
                //        dlai(iemrg-isow)=acotyl*ppm            ! initial area
                //        alai=dlai(iemrg-isow)
                //        lastlf=iemrg-isow                      ! set index for oldest leaf
                acotyl_m2 = acotyl / 1000000.0;  // convert mm2 to m2
                dlai[iemrg] = acotyl_m2 * ppm;    // initial area
                alai = dlai[iemrg];
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
                alaix = alai;              // save previous lai
                //index = ifix(dd);          // index for do loop below to grow leaf
                index = (int)Math.Truncate(dd);          // index for do loop below to grow leaf

                for (lin = 1; lin <= index; lin++)
                {
                    alaix = alaix * (1.0 + actrgr);   // grow leaf without water stress
                }
                dlai_pot = alaix - alai;              // days increase without water stress

                actrgr = actrgr * flfsmi;             // water stress
                alaix = alai;                         // save previous lai again

                for (lin = 1; lin <= index; lin++)
                {
                    alaix = alaix * (1.0 + actrgr);   // grow leaf with water stress
                }
                dlai[iday] = alaix - alai;            // days increase with water
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
            alai = alai + dlai[iday];

            //*******senescence ***************************************************

            // ddleaf = ozcot_senlf(bload, alai, carcap_c, smi);
            ddleaf = ozcot_senlf();
            if (n_def == 1 & (iday - i_def) > 7) ddleaf = ddleaf * 0.33;      // 1st defol"n
            if (n_def == 2 & (iday - i_def) > 7) ddleaf = ddleaf * 0.0;       // 2nd defol"n

            shedlf = 0.0;       // initialise for this day
            leaf_res = 0.0;

            if (lastlf > 0)           // called after measured lai finished
            {
                for (l = lastlf; l <= DAS; l++)        // loop thro unshed leaves
                {
                    if ((fyzage[l] < ddleaf)) break;   // are this days leaves shed today?
                    alai = alai - dlai[l];                // reduce lai
                    shedlf = shedlf + dlai[l];            // sum area of shed leaves
                    dlai[l] = 0.0;                        // day,s area now zero
                    dw_leaf = dw_leaf - ddw_l[l];         // reduce leaf dry matter
                    leaf_res = leaf_res + ddw_l[l];       // sum this day's residues
                    ddw_l[l] = 0.0;                       // this day"s leaf wt now zero
                    lastlf = l + 1;                       // set index for oldest remaining leaves.
                }
            }

            leaf_res_n = leaf_res * leaf_res_n_conc;   // n content of leaf residues
            leaf_n = leaf_n - leaf_res_n;              // estimate of N content of leaf

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

            //- implementation section ----------------------------------

            //         **** read met data  ***************************************

            //       read(1,151,end=153) jdate,imyr,
            //     *  rain,epan,tempmx,tempmn,tempdy,tempwt,wind,solrad
            //151    format(4x,i3,5x,i4,f5.0,f4.1,2f4.0,2f4.1,f4.0,f4.0) ! new


            //psc        solrad = solrad / 0.04186
            //psc        rain = rain / 10.0
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

            if ((rain <= 0.1))
            {
                if ((nsince < 1)) nsince = 1;
                nsince = nsince + 1;
            }
            else if ((nsince > 1))
            {
                nsince = 1;                       // 1st day of rain period, light rain
                if ((rain > 10.0)) nsince = 0;    // heavy rain
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
            double functionReturnValue = 0.0;
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
            functionReturnValue = Math.Pow(10.0, ew);

            return functionReturnValue;
        }




        // ====================================================================
        public double ozcot_senlf()
        {
            double functionReturnValue = 0.0;
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

            functionReturnValue = 833.0 + 277.0 * fb * fw * fn;

            //     proposal 3/1/97 try when convenient abh

            fn = ozcot_stress(0.75, 1.0, 1.0, fnstrs);            // nitrogen stress 30 dec 1992
            
            //jh skip row correction ??  TODO:  check for skip row correction
            alai_row = alai;
            //      if(nskigt.0)alai_row = alai*rs        ! lai in hedgerow

            if ((alai_row > 3.0)) fl = 1.0 - (alai_row - 3.0) / 3.0;    // effect of lai > 3
            if ((fl > 1.0)) fl = 1.0;
            if ((fl < 0.0)) fl = 0.0;

            f1 = Math.Min((fb * fw), fn);
            f2 = Math.Min(f1, fl);
            //      senlf=833.+277.*f2

            // TODO:  calcs on fl are not used.  Check validity

            return functionReturnValue;

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

            //------- reset initial soil water ------------------------------------------

            //------ remove water from layers 1&2 during land preparation -------------------

            ozcot_evapotransp();            // uses smi, gives ep & et
            //jh      call swbal_et(i)               ! removes ep & es from layers
            ozcot_swbal_et();               // removes ep & es from layers
            ozcot_sum_sw();                 // sums sw and gives def
            //jh      call irrigate (rainsi)         ! uses def
            //jh      call runoff (rainsi,rainef)    ! uses sw, gives q
            //jh      call swbal_add (rainef)        ! adds rain & irrigation to layers
            //jh      call sum_sw                    ! sums sw and gives def
            //jh      call water_info (i)
            ozcot_indices();            //gives smi & wli
            //jh      if (wli.gt.watlog_n) call drainage
            if (nskip > 0.0) ozcot_skipwater();    // for skip row
            
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
                smi_row = 1 + (smi - 1) * rs;      // smi in plant row
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
                    smi_row = 1 + (smi - 1) * rs;   // smi in plant row
                    smi = smi_row;                  // effective smi
                }
                else
                {
                    smi = Math.Min(smi, smi_row);   // no, drying continues
                    //    smi = smi*f_smi            ! adjust smi when using water in skip
                }
            }

            wli = 1 - rs * (1 - wli);              // water logging index in row

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

			alai_row = alai;
			if (nskip > 0) alai_row = alai * rs; 	// lai in hedgerow - abh 5/11/96

			if (alai_row > 3.0)
			{
				ep = eo - es;
			}
            else 
                if (alai_row > 1.6)
                {
				    ep = (0.08 + 0.3066 * alai_row) * eo;	// l.mateos 1987, abh 1988
			    }
			    else
			    {
			    	ep = (1.0 - Math.Exp(-0.5186 * alai_row)) * eo;	// l.mateos 1987, abh 1988
			    }

			if (alai_row == 0.0) ep = 0.0; 
			if (ep < 0.0) ep = 0.0; 

			if (nskip > 0)
			{
				ep = ep / rs;				       // ep on ground area basis
				//jh  alai = alai/rs               ! restore lai to ground area basis
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

            //jh        smi=smi_rt   !debug
            smi = Math.Min(1.0, smi);
            //jh      print*, smi_rt, rtdep
            //jh        wli = sw/ul
            //jhtemp
            //jh        wli = smi_rt
            wli = smi;
            //        smi = 0.86 !debug
            //        wli = 0.86 !debug

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
            double stressFactor;
            double functionReturnValue = 0.0;
            // ====================================================================

            //       computes or adjusts a factor.
            //       input is state variable strs with upper and lower limits, high,low.
            //       output is between 0 and 1.
            //       a =  1 gives factor a linear fn of ratio of strs to high - low
            //       a gt 1 accentuates effect of strs on value
            //       a lt 1 damps effect.


            //- implementation section ----------------------------------

            stressFactor = (strs - low) / (high - low);
            if ((stressFactor > 1.0))
            {
                functionReturnValue = 1.0;
            }
            else if ((stressFactor <= 0.0))
            {
                functionReturnValue = 0.0;
            }
            else
            {
                functionReturnValue = (1.0 - Math.Pow((1.0 - stressFactor), a));
            }
            return functionReturnValue;
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


            double ux;
            double epcoef;
            double uob;
            double sum;
            //jh v2001      real stran
            double depth;
            double tdepth;
            double epd;
            double swlr;
            double dswmx;
            int l;

            //- implementation section ----------------------------------


            //       percol=rainef
            //       exes=es

              //    IF(swrSoilMoistIdx.GE.0.5)lrEPCoeff=3.051       !  lrW*N
              //    IF(swrSoilMoistIdx.LT.0.5)lrEPCoeff=2.436       !  82/83


            ux = 0.0;
            if (smi >= epcoef_smi_crit)
            {
                epcoef = epcoef1;      //  w*n   
            }
            else
            {
                epcoef = epcoef2;       //(smi < epcoef_smi_crit))  82/83   
            }
            uob = ep / (1.0 - Math.Exp(-epcoef));
            sum = 0.0;
            //jh v2001        stran=0. ! sum of trans(l) down profile
            depth = 0.0;
            tdepth = 0.0;
            epd = 0.0;
            //       w=0.

            //***********************************************************************

            for (l = 1; l <= nlayr; l++)
            {
                //psc
                //psc          swlayr(l)=swdep(l)/10./dlayr_cm(l)
                //psc     *                 -(duldep(l)/10./dlayr_cm(l)-ullayr(l))
                //psc
                //jh v2001          trans(l)=0.
                swlr = swlayr[l] * dlayr_cm[l];
                //           swmax=ullayr(l)*dlayr_cm(l)
                //           if(percol.eq.0..and.exes.eq.0.)go to 2

                //**** rain percolates layer 'l'

                //       swlr=swlr+percol
                //       percol=0.
                //       if(swlr.le.swmax)go to 1
                //       percol=swlr-swmax ! surplus percolates to next layer
                //       swlr=swmax        ! this layer full

                //**** extract es from layer 'l'

                // 1     swlr=swlr-exes    ! extract es from this layer
                //       exes=0.
                //       if(swlr.ge.0.)go to 2
                //       exes=-swlr        ! es carried down to next layer
                //       swlr=0.

                //**** extract ep from this layer

                //2        continue
                depth = depth + dlayr_cm[l];              // moved here 29/6/88
                //         if(i.lt.iemrg) depth=0.0       !       ditto
                if ((DAS < iemrg)) depth = 0.0;           //      ditto     //TODO: dbj check logic ???? DAS never < 0 AND iemrg ALWAYS < DAS ??
                if (rtdep > tdepth)                      //      ditto
                {
                    if ((rtdep < depth)) depth = rtdep;       //      ditto
                    sum = uob * (1.0 - Math.Exp(-epcoef * depth / rtdep));
                    dswmx = sum - ux;
                    swlr = swlr - dswmx;                     // extract ep from this layer
                    if ((swlr < 0.0))
                    {
                        epd = swlr;
                        swlr = 0.0;
                    }

                    //jh v2001          dswmx=dlayr_cm(l)*swlayr(l)
                    //jh v2001          trans(l)=dswmx
                    tdepth = depth;
                    ux = sum + epd;     // epd corrects ux if layer is dry
                    epd = 0.0;
                }
           
                swlayr[l] = swlr / dlayr_cm[l];
            }
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
        //      subroutine yield(nszn,iend)
        public void ozcot_yield()
        {
            // ====================================================================

            //     estimates yield and gross margin at end of season

            //- implementation section ----------------------------------

            //     calculate yield **********************************************************

            alint = openwt * 10.0 * pcLint_frac;              // g sc/m to kg lint/ha
            //psc      alint = alint/rs                   ! adjust for row spacing 2/5/90

            //      plntz = uptakn                     ! nitrogen uptake
            //pc   bollsc = 0.
            //pc   if(openz.gt.0.) bollsc = openwt/openz ! g sc/boll
            //jh v2001       rrig(rain_pre_post_boll) = rrig(rain_preboll)
            //jh v2001      :                           + rrig(rain_postboll)  ! commulative rain pre + post boll

            ozcot_residues();
            // to calculate stem and root residues
            //     calculate gross margin ***************************************************

            //     currently redundant!!!!

            //      cot_price = 2.                     ! $ per kg
            //      wat_cost = 12.                     ! $ per ml pumped
            //      spray_cost =2.                     ! $ day of protection

            //      cot_price = 2.                      ! cotton price $ per kg
            //      gro_cost = 1200.                    ! growing costs - irrigated
            //      gro_cost = 450.                     ! growing costs - rain grown
            //      wat_cost  = 12.                     ! $ per ml (pumping & carges)
            //      spray_cost = 2.                     ! $ per day of protection
            //      cot_inc = alint*cot_price           ! cotton income
            //      wat_exp = rrig(2)/10.*wat_cost      ! water expenditure
            //      spray_save = (150-ilaiz)*spray_cost ! saving of spray cost
            //      gross_marg = cot_inc - gro_cost - wat_exp -  spray_save ! gross margin

            //     sowing date for output ***************************************************

            //      jsow = isow+imet-1                  ! sowing date in j time
            //      mdpy_prev = 365
            //      if((imyr-1)/4*4 .eq. imyr_1) mdpy_prev = 366
            //      if(jsow.gt.mdpy_prev) jsow = jsow - mdpy_prev

            //     this section for when crop not sown **************************************

            //      if(iend.ge.3) then
            //          jsow = 0                        ! crop not sown, window passed or fallow
            //          gross_marg = 0.0                ! gross margin nil
            //      end if

            //     this section does stuff needed with 1st call of yield ********************


            //      if (imyr.gt.1800) then              ! year in 10s or 100s?
            //          imyr = imyr-1800                ! change year to 10s
            //          if(imyr.ge.100) imyr=imyr-100   ! for 1900s
            //      endif                               ! above done for output

            //      if(nszn.eq.1) then                     ! first season

            //          open(3,file='yield.out',status='unknown')
            //          write(3,7772) title

            //          if(mode.le.1) then                  ! validation or calibration
            //              write(3,600)
            //          else                                ! simulation for strategy etc
            //              if(defirr(2).gt.0.) then        ! irrigated crop
            //                  write(3,700)
            //              else                            ! rain-fed crop
            //                  write(3,800)
            //              endif
            //          endif

            //          if(jdate.lt.244) then               ! last year was year of sowing
            //              iyr1 = imyr-1                   ! year sown in first season
            //              iyr2 = imyr                     ! year harvested first season
            //          else                                ! this year was year of sowing,
            //              iyr1 = imyr                     ! year of sowing
            //              iyr2 = imyr+1                   ! year of harvest
            //          endif

            //      else                                    ! not first season

            //              iyr1 = iyr1+1                   ! year of sowing of this season
            //              iyr2 = iyr2+1                   ! year of harvest of this season


            //      endif

            //      if(iyr1.eq.100) iyr1 = 0                 ! new century
            //      if(iyr2.eq.100) iyr2 = 0                 ! new century

            //    fallow soil water *********************************************************


            //      if(jsow.gt.0) then    ! crop sown; deal with fallow sw gain
            //          gain = rrig(2)-rrig(6) ! gain in sw during fallow
            //          fraction = 0.0    ! fraction of rainfall conserved by fallow
            //          if(rrig(5).ne.0.) fraction = gain/rrig(5)
            //          write(4,999) iyr1,iyr2,jsow,rrig(6),rrig(2),gain,rrig(5),fraction
            //999       format(3i4,5f8.2) ! yrs, sow d, initial sw, final sw, gain, rain, fraction
            //          rrig(5) = 0.0     ! reset fallow rain. do not reset in reset!
            //          rrig(6) = sw      ! initial sw for next fallow. do not reset in reset!
            //      endif

            //     write output *************************************************************

            //      if(mode.le.1) then                  ! validation or calibration
            //          write(3,7770) iyr1,iyr2,jsow,openz,alint,bollsc,
            //     *    alaiz,plntz,ilaiz,sqzx,isqzx
            //          write(2,7770) iyr1,iyr2,jsow,openz,alint,bollsc,
            //     *    alaiz,plntz,ilaiz,sqzx,isqzx
            //      else                                ! simulation for strategy etc
            //          if(defirr(2).gt.0.) then        ! irrigated crop
            //              write(3,7771) iyr1,iyr2,jsow,openz,alint,bollsc,
            //     *        alaiz,plntz,ilaiz,ifix(rrig(1)),rrig(2),rrig(7),rrig(8)
            //     *        alaiz,plntz,ilaiz,ifix(rrig(1)),rrig(2),rrig(7),def_last ! norn d
            //              write(2,7771) iyr1,iyr2,jsow,openz,alint,bollsc,
            //     *        alaiz,plntz,ilaiz,ifix(rrig(1)),rrig(2),rrig(7),rrig(8)
            //     *        alaiz,plntz,ilaiz,ifix(rrig(1)),rrig(2),rrig(7),def_last ! norn d
            //          else                            ! rain-fed crop
            //              write(3,7771) iyr1,iyr2,jsow,openz,alint,bollsc,
            //     *        alaiz,plntz,ilaiz,ifix(rrig(1)),rrig(2),rrig(3),rrig(4)
            //              write(2,7771) iyr1,iyr2,jsow,openz,alint,bollsc,
            //     *        alaiz,plntz,ilaiz,ifix(rrig(1)),rrig(2),rrig(3),rrig(4)
            //          endif
            //      endif

            return; 

            //600   format(' year sown  bolls/m  lint sc/boll max_lai  n_uptk day
            //     *   sqz day')
            //700   format(' year sown  bolls/m  lint sc/boll max_lai  n_uptk day
            //     * no  water  rain  cum_et')
            //     * no  water  rain def_l')                                    ! for norm d
            //800   format(' year sown  bolls/m  lint sc/boll max_lai  n_uptk day
            //     * no  water rain1  rain2')
            //7770  format(x,2i2,i4,f8.1,f8.0,2f8.2,f8.0,i4,f6.1,i4)
            //7771  format(x,2i2,i4,f8.1,f8.0,2f8.2,f8.0,i4,i3,3f7.1)
            //7772  format(15a4)

        }



        // ====================================================================
        public void ozcot_plant_n()
        {
            // ====================================================================
            //     call from pltgrw before ozcot_cropn
            //     calculates nitrogen content of dry matter increments and sums them
            //     adjusts n increments as soil n supply diminishes
            //     variable ddw_leaf etc from s/r dry_matter
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


            //data conc_l /0.04/
            //data conc_s /0.02/
            //data conc_r /0.02/
            //data conc_b /0.015/

            //- implementation section ----------------------------------

            //jhadded
            supply_n = Math.Min(2.0, Math.Min(uptakn / 10.0 - total_n, tsno3 / 10.0) / 5.0);
            // max uptake of 2 g/m2. 5 days to takeup avail n.
            //      supply_n = min(1.0, uptakn/240.0)   ! max uptake of 240 kg/ha specified in n_fertilise

            //      calculate daily increment for components of dry matter

            dn_leaf = ddw_leaf * conc_l;            // leaf nitrogen
            dn_stem = ddw_stem * conc_s;            // stem nitrogen
            dn_root = ddw_root * conc_r;            // root nitrogen
            dn_boll = ddw_boll * conc_b;            // boll nitrogen

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


            //      write(4,222) iday,supply_n,ldn_plant,total_n,uptakn,dw_total
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

            stem_res = dw_stem;                // stem residues dry matter
            stem_res_n = dw_stem * conc_res;   // n content of stem residues
            root_res = dw_root;                // root residues dry matter
            root_res_n = dw_root * conc_res;   // n content of root residues

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
            //       assimilate new dry matter passed daily from s/r assimilation
            //       ddw_boll   day's increase in boll dry weight
            //       ddw_leaf   day's increase in leaf dry weight
            //       ddw_stem   day's increase in stem dry weight
            //       ddw_root   day's increase in root dry weight
            //       ddw_root_max   maximum value of increase in root dry weight
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
            //      real ddw_root_max


            //- implementation section ----------------------------------

            //------------------------------------------------------------------------------
            //     initialise leaf, stem and root dry matter at start of new crop
            //------------------------------------------------------------------------------

            if ((dw_leaf == 0.0))      // leaf weight initialise to zero?
            {
                dw_leaf = embryo * f_leaf * ppm;           // initial leaf dry weight per m
                dw_stem = embryo * f_stem * ppm;           // initial stem dry weight per m
                dw_root = embryo * f_root * ppm;           // initial root dry weight per m
            }

            //------------------------------------------------------------------------------
            //     calculate demand (potential growth) for leaf, stem and root
            //------------------------------------------------------------------------------

            ddw_leaf = dlai_pot * specific_lw;                                    // leaf demand
            ddw_stem = mvDataFunctions.divide_check(a_stem_leaf * ddw_leaf * dw_stem, dw_leaf, 0.0);    // ditto for stem
            ddw_root = mvDataFunctions.divide_check(a_root_leaf * ddw_leaf * dw_root, dw_leaf, 0.0);    // ditto for root

            //------------------------------------------------------------------------------
            //     feed back of leaf weight/area ratio
            //------------------------------------------------------------------------------

            if (dlai[iday] > 0.0)    // leaf growth today?
            {
                wt_area = mvDataFunctions.divide_check(ddw_leaf, dlai[iday], 0.0);   // leaf weight/area ratio
                if ((wt_area > wt_area_max))                   // too thick
                {
                    ddw_leaf = dlai[iday] * wt_area_max;      // reduce weight
                    //          else if(wt_area.lt.wt_area_min) then            ! too thin
                    //              dlai(iday) = ddw_leaf/wt_area_min           ! reduce area
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
                ddw_boll = bollz * bollgr;                                      // boll demand - potential growth

            }

            //------------------------------------------------------------------------------
            //   determine supply of assimilate
            //------------------------------------------------------------------------------
            assimilate = 0.0;
            ozcot_assimilation();            // day's assimilate
            supply = assimilate + reserve;             // compute supply
            reserve = 0.0;                             // reserve used

            //------------------------------------------------------------------------------
            //   compare total demand with supply to partition assimilate
            //------------------------------------------------------------------------------

            demand = ddw_leaf + ddw_boll + ddw_stem + ddw_root;   // compute total demand

            if ((supply >= demand))         // demand met, potential growth achieved
            {
                reserve = reserve + supply - demand;              // excess becomes reserve
                if ((reserve > res_cap)) reserve = res_cap;       // limit to reserve
                sd_ratio = 1.0;                                   // supply,demand ratio for leaf,stem,boll
                sd_root = 1.0;                                    // supply,demand for root
            }
            else                           // demand not met, reduce potential growth
            {
                demand = demand - ddw_root;                       // demand for leaf, stem and fruit
                if ((supply >= demand))        // their potential growth achieved
                {
                    sd_ratio = 1.0;                                      // supply,demand ratio for leaf,stem,boll          'const
                    sd_root = mvDataFunctions.divide_check((supply - demand), ddw_root, 0.0);  // supply,demand for root
                    ddw_root = supply - demand;                          // rest to root
                }
                else                          // leaf, stem and fruit demand not met
                {
                    sd_ratio = mvDataFunctions.divide_check(supply, demand, 0.0);            // supply,demand ratio
                    ddw_leaf = ddw_leaf * sd_ratio;                    // actual leaf growth
                    ddw_boll = ddw_boll * sd_ratio;                    // actual fruit growth
                    ddw_stem = ddw_stem * sd_ratio;                    // actual stem growth
                    ddw_root = 0.0;                                    // no root growth
                    sd_root = 0.0;                                     // supply,demand for root
                    bollgr = bollgr * sd_ratio;                        // adjust boll growth rate
                }
            }

            //------------------------------------------------------------------------------
            //     grow crop by updating dry weights for leaf, stem, bolls and roots
            //------------------------------------------------------------------------------

            dw_leaf = dw_leaf + ddw_leaf;                        // total leaf dry weight
            dw_stem = dw_stem + ddw_stem;                        // total stem dry weight
            dw_boll = dw_boll + ddw_boll;                        // total boll dry weight
            dw_root = dw_root + ddw_root;                        // total root dry weight
            dw_total = dw_leaf + dw_stem + dw_boll + dw_root;    // total dry weight

            ddw_l[iday] = ddw_leaf;                              // this day"s leaf dry wt
            //      alai = alai+dlai(iday)                           ! update lai with increase

            //------------------------------------------------------------------------------
            //     feed back from root grow to root depth
            //------------------------------------------------------------------------------
            if ((iday == 1)) ddw_root_max = ddw_root;            // initialise max root rate

            if ((ddw_root > ddw_root_max))
            {
                ddw_root_max = ddw_root;                         // save maximum root rate
                root_feedback = 1.0;                             // feedback of dw on depth
            }
            else
            {
                if ((ddw_root_max == 0.0))
                {
                    root_feedback = 1.0;
                }
                else
                {
                    root_feedback = mvDataFunctions.divide_check(ddw_root, ddw_root_max, 0.0);  // feedback of dw on depth
                }
            }

            root_feedback = Math.Min(root_feedback, sd_root);            // feedback of dw on depth
            if ((root_feedback > 0.0))
            {
                root_feedback = Math.Pow(root_feedback, 0.333);       // cubic to linear
            }

            // Calculation to update estimated plant height

            if (num_height == 0)
            {
                height = 900.0;                  // no data specified,  use default
            }
            else
            {
                height = mvDataFunctions.linear_interp_real(mvDataFunctions.divide(dw_stem, pp, 0.0),
                                                              x_stem_wt,
                                                              y_height,
                                                              num_height
                                                            );
            }

            return; 
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

            //jh      alight = 1.-exp(-1.*alai)      ! light interception, beer's law.
            //jh         changed to the following to accommodate skip row
            alai_row = alai;
            if ((nskip > 0)) alai_row = alai * rs;                  // lai in hedgerow
            alight = (1.0 - Math.Exp(-ozcot_kvalue * alai_row));    // original code  - now gives interception in
            if ((nskip > 0))
            {
                alight = alight / rs;                // interception on ground area basis
                //jh         alai = alai/rs         ! restore lai to ground area basis
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

            if (num_co2_fert > 0) 
            {
                co2FertValue = mvDataFunctions.linear_interp_real(co2, x_co2_fert, y_co2_fert, num_co2_fert);
            }
            else
            {
                co2FertValue = 1.0;
            }

            return co2FertValue;

        }

    }
}
