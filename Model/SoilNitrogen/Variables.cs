﻿using System;
using System.Reflection;
using System.Collections.Generic;
using System.Xml.Serialization;
using System.Xml;
using System.Linq;
using System.Text;
using ModelFramework;
using CSGeneral;

/// <summary>
/// This partial class contains most of the variables and input properties of SoilNitrogen
/// </summary>

public partial class SoilNitrogen
{

	#region Links to other modules

	/// <summary>
	/// Link to APSIM's clock
	/// </summary>
	[Link]
	public Clock Clock = null;

	/// <summary>
	/// Link to APSIM's metFile (weather data)
	/// </summary>
	[Link]
	public MetFile MetFile = null;

	/// <summary>
	/// Link to container paddock
	/// </summary>
	[Link]
	public Paddock Paddock;		// not sure why it is here

	#endregion

	#region Parameters and inputs provided by the user or APSIM

	#region Parameters added by RCichota

	// whether to use new functions to compute temp and moist factors
	private bool useNewSTFFunction = false;  // for stf
	private bool useNewSWFFunction = false;  // for swf
	private bool useNewProcesses = false;    // for processes
	[Param]
	public string useAllNewFunctions
	{
		set
		{
			useNewSTFFunction = value.ToLower().Contains("yes");
			useNewSWFFunction = useNewSTFFunction;
			useNewProcesses = useNewSTFFunction;
		}
	}
	[Param]
	public string useNewFunction4TF
	{ set { useNewSTFFunction = value.ToLower().Contains("yes"); } } // for stf
	[Param]
	public string useNewFunction4WF
	{ set { useNewSWFFunction = value.ToLower().Contains("yes"); } } // for swf

	// whether to use single temp and moist factors for SOM and FOM mineralisation ir separated
	private bool useSingleMinerFactors = true;
	[Param]
	public string useSingleFactors4Miner
	{ set { useSingleMinerFactors = value.ToLower().Contains("yes"); } }

	// whether calculate one set of mineralisation factors (stf and swf) or one for each pool
	private bool useFactorsBySOMpool = false;
	[Param]
	public string useMultiFactors4MinerSOM
	{ set { useFactorsBySOMpool = value.ToLower().Contains("yes"); } }
	private bool useFactorsByFOMpool = false;
	[Param]
	public string useMultiFactors4MinerFOM
	{ set { useFactorsByFOMpool = value.ToLower().Contains("yes"); } }

	[Param]
	public string NPartitionApproach;

	#endregion

	//*Following parameters might be better merged into other regions but it is clear to have it separtately, FLi 
	#region ALTERNATIVE Params for alternarive nitrification/denitrification processes

	// soil texture by layer: COARSE = 1.0;/MEDIUM = 2.0; FINE = 3.0; VERYFINE = 4.0;
	double[] SoilTextureID;
	[Param(IsOptional = true, MinVal = 1.0, MaxVal = 4.0)]
	[Input(IsOptional = true)]
	public double[] texture
	{
		set
		{
			double IDvalue = 2.0;  // default texture is medium
			for (int layer = 0; layer < dlayer.Length; layer++)
			{
				if (value != null)
					IDvalue = value[layer];
				SoilTextureID[layer] = IDvalue;
			}
		}
	}

	//Alternative N2O emission
	[Param(MinVal = 0, MaxVal = 10)]
	public int n2o_approach = 0;           // Approches used for nitri/denitri process for n2o emission 

	//WNMM
	[Param(MinVal = 0.0, MaxVal = 1.0)]
	public double wnmm_n_alpha = 0.002;             // maximum fraction of nitrification rate as N2O

	[Param(MinVal = 0.0, MaxVal = 1.0)]
	public double wnmm_dn_alpha = 0.5;            // maximum fraction of denitrification rate at wfps = 0.8

	//NWMIS
	[Param(MinVal = 0.0, MaxVal = 50.0)]
	public double nemis_dn_km = 22;              // half-saturation consntant for NO3 reduction (unit ppm = mgN/kg)

	[Param(MinVal = 0.0, MaxVal = 50.0)]
	public double nemis_dn_pot = 7.194; 	        // default = 7.194; potential denitrification rate at 20C, on undisturbed soil 
	// saturated with water in the lab and placed at a nitrate content near to 200 mgN/kg
	//CENTURY
	[Param(MinVal = 0.0, MaxVal = 60.0)]
	public double cent_n_soilt_ave = 15;             // average soil surface temperature

	[Param(MinVal = 0.0, MaxVal = 60.0)]
	public double cent_n_maxt_ave = 25; 	            // long term average maximum monthly temperature of the hottest month	

	[Param(MinVal = 0.0, MaxVal = 1.0)]
	public double cent_n_wfps_ave = 0.7;              // default = 0.7; average wfps in top nitrifyDepth of soil

	[Param(MinVal = 0.0, MaxVal = 1.0)]
	public double cent_n_max_rate = 0.1;              // default = 0.1, maximum fraction of ammonium to NO3 during nitrification (gN/m2)
	#endregion

	#region Parameters used on initialisation only

	#region General setting parameters

	/// <summary>
	/// Soil parameterisation set to use
	/// </summary>
	/// <remarks>
	/// Used to determine which node of xml file will be used to read [Param]'s
	/// </remarks>
	private string SoilCNParameterSet = "standard";
	[Param(IsOptional = true)]
	public string soiltype
	{
		get { return SoilCNParameterSet; }
		set { SoilCNParameterSet = value.Trim(); }
	}

	/// <summary>
	/// Indicates whether simpleSoilTemp is allowed
	/// </summary>
	/// <remarks>
	/// When 'yes', soil temperature may be computed internally, if an external value is not supplied.
	/// If 'no', a value for soil temperature must be supplied or an fatal error will occur.
	/// </remarks>
	private bool AllowsimpleSoilTemp = false;
	[Param]
	public string allow_simpleSoilTemp
	{
		get { return (AllowsimpleSoilTemp) ? "yes" : "no"; }
		set { AllowsimpleSoilTemp = value.ToLower().Contains("yes"); }
	}

	/// <summary>
	/// Indicates whether soil profile reduction is allowed (from erosion)
	/// </summary> 
	private bool AllowProfileReduction = false;
	[Param]
	public string profile_reduction
	{
		get { return (AllowProfileReduction) ? "yes" : "no"; }
		set { AllowProfileReduction = value.ToLower().StartsWith("on"); }
	}

	/// <summary>
	/// Indicates whether organic solutes are to be simulated
	/// </summary>
	/// <remarks>
	/// Always false as this is not implemented yet
	/// </remarks>
	private bool useOrganicSolutes = false;
	[Param(IsOptional = true)]
	public string use_organic_solutes
	{
		get { return (useOrganicSolutes) ? "yes" : "no"; }
		set { useOrganicSolutes = value.ToLower().StartsWith("on"); }
	}

	/// <summary>
	/// Minimum allowable Urea content (ppm)
	/// </summary>
	[Param(MinVal = 0.0, MaxVal = 1000.0)]
	public double ureappm_min;

	/// <summary>
	/// Minimum allowable NH4 content (ppm)
	/// </summary>
	[Param(MinVal = 0.0, MaxVal = 1000.0)]
	public double nh4ppm_min;

	/// <summary>
	/// Minimum allowable NO3 content (ppm)
	/// </summary>
	[Param(MinVal = 0.0, MaxVal = 1000.0)]
	public double no3ppm_min;

	/// <summary>
	/// Minimum allowable FOM content (kg/ha)
	/// </summary>
	[Param(MinVal = 0.0, MaxVal = 1.0)]
	public double fom_min;

	/// <summary>
	/// FOM type for initalisation
	/// </summary>
	public string ini_FOMtype = "default";

	/// <summary>
	/// Factor to convert from OC to OM
	/// </summary>
	[Param(MinVal = 0.0, MaxVal = 3.0)]
	public double oc2om_factor;

	/// <summary>
	/// Default weight fraction of C in carbohydrates
	/// </summary>
	/// <remarks>
	/// Used to convert FOM amount into fom_c
	/// </remarks>
	private double defaultFOMCarbonContent = 0.4;
	[Param(IsOptional = true, MinVal = 0.0, MaxVal = 1.0)]
	public double c_in_fom
	{
		get { return defaultFOMCarbonContent; }
		set { defaultFOMCarbonContent = value; }
	}

	/// <summary>
	/// Defaul value for initialising soil pH
	/// </summary>
	[Param()]
	public double ini_pH;

	/// <summary>
	/// Minimum relative area (fraction of paddock) for any patch
	/// </summary>
	private double MinimumPatchArea = 1.0;
	[Param(IsOptional = true, MinVal = 0.0, MaxVal = 1.0)]
	public double minPatchArea
	{
		get { return MinimumPatchArea; }
		set { MinimumPatchArea = value; }
	}

	/// <summary>
	/// Absolute threshold value to trigger a warning message when negative values are detected
	/// </summary>
	[Param()]
	public double WarningThreshold;

	/// <summary>
	/// Absolute threshold value to trigger a fatal error when negative values are detected
	/// </summary>
	[Param()]
	public double FatalThreshold;

	#endregion

	#region Parameters for handling soil loss process

	/// <summary>
	/// Coefficient a of the enrichment equation
	/// </summary>
	[Param()]
	public double enr_a_coeff;

	/// <summary>
	/// Coefficient b of the enrichment equation
	/// </summary>
	[Param()]
	public double enr_b_coeff;

	#endregion

	#region Parameters for setting up soil organic matter

	/// <summary>
	/// The total OC amount at initialisation, used also on reset
	/// </summary>
	private double[] InitialOC = null;

	/// <summary>
	/// The C:N ratio of the soil humus (active + inert)
	/// </summary>
	/// <remarks>
	/// Remains fixed throughout the simulation
	/// </remarks>
	private double hum_cn = 0.0;
	/// <summary>
	/// The C:N ratio of the soil OM, from xml/GUI (actually of humus)
	/// </summary>
	[Param(MinVal = 1.0, MaxVal = 25.0)]
	public double soil_cn
	{
		get { return hum_cn; }
		set { hum_cn = value; }
	}

	/// <summary>
	/// The C:N ratio of microbial biomass
	/// </summary>
	/// <remarks>
	/// Remains fixed throughout the simulation
	/// </remarks>
	private double biom_cn = 8.0;
	[Param(IsOptional = true, MinVal = 1.0, MaxVal = 50.0)]
	public double mcn
	{
		get { return biom_cn; }
		set { biom_cn = value; }
	}

	/// <summary>
	/// Proportion of biomass-C in the initial mineralizable humic-C (0-1)
	/// </summary>
	[Param(MinVal = 0.0, MaxVal = 1.0)]
	public double[] fbiom;

	/// <summary>
	/// Proportion of the initial total soil C that is inert, not subject to mineralisation (0-1)
	/// </summary>
	[Param(MinVal = 0.0, MaxVal = 1.0)]
	public double[] finert;

	#endregion

	#region Parameters for setting fresh organic matter (FOM)

	/// <summary>
	/// Initial weight of fom in the soil (kgDM/ha)
	/// </summary>
	private double iniFomWt = 0.0;
	[Param(MinVal = 0.0, MaxVal = 100000.0)]
	public double root_wt
	{
		get { return iniFomWt; }
		set { iniFomWt = value; }
	}

	/// <summary>
	/// Initial depth over which fom is distributed within the soil profile (mm)
	/// </summary>
	/// <remarks>
	/// If not given fom will be distributed over the whole soil profile
	/// Distribution is homogenous over this depth
	/// </remarks>
	private double iniFomDepth = 0.0;
	[Param(IsOptional = true, MinVal = 0.0, MaxVal = 5000.0)]
	public double root_depth
	{
		get { return iniFomDepth; }
		set { iniFomDepth = value; }
	}

	/// <summary>
	/// Initial C:N ratio of roots (actually FOM)
	/// </summary>
	private double iniFomCNratio = 0.0;
	[Param(MinVal = 0.1, MaxVal = 750.0)]
	public double root_cn
	{
		get { return iniFomCNratio; }
		set { iniFomCNratio = value; }
	}

	/// <summary>
	/// Initial C:N ratio of each of the three fom composition pools (carbohydrate, cellulose, and lignin)
	/// </summary>
	/// <remarks>
	/// Case not given, iniFomCNratio is used
	/// </remarks>
	private double[] fomPoolsCNratio = null;
	[Param(IsOptional = true, MinVal = 0.0, MaxVal = 1000.0)]
	public double[] root_cn_pool
	{
		get { return fomPoolsCNratio; }
		set { fomPoolsCNratio = value; }
	}

	/// <summary>
	/// List of available FOM types names
	/// </summary>
	[Param(Name = "fom_type")]
	[XmlArray("fom_type")]
	public String[] fom_types;

	/// <summary>
	/// Fraction of carbohydrate in FOM (0-1), for each FOM type
	/// </summary>
	[Param(MinVal = 0.0, MaxVal = 1.0)]
	public double[] fract_carb;

	/// <summary>
	/// Fraction of cellulose in FOM (0-1), for each FOM type
	/// </summary>
	[Param(MinVal = 0.0, MaxVal = 1.0)]
	public double[] fract_cell;

	/// <summary>
	/// Fraction of lignin in FOM (0-1), for each FOM type
	/// </summary>
	[Param(MinVal = 0.0, MaxVal = 1.0)]
	public double[] fract_lign;

	#endregion

	#region Parameters for FOM and SurfaceOM mineralisation process

	#region Surface OM

	/// <summary>
	/// Fraction of residue C mineralised retained in system (0-1)
	/// </summary>
	[Param(MinVal = 0.0, MaxVal = 1.0)]
	public double ef_res;

	/// <summary>
	/// Fraction of retained residue C transferred to biomass (0-1)
	/// </summary>
	/// <remarks>
	/// Remaining will got into humus
	/// </remarks>
	[Param(MinVal = 0.0, MaxVal = 1.0)]
	public double fr_res_biom;

	/// <summary>
	/// Depth from which mineral N can be immobilised when decomposing surface residues (mm)
	/// </summary>
	[Param(MinVal = 0.0, MaxVal = 1000.0)]
	public double min_depth;

	#endregion

	#region Fresh OM

	/// <summary>
	/// Optimum rate constant for decomposition of FOM pools [carbohydrate component] (0-1)
	/// </summary>
	[Param(MinVal = 0.0, MaxVal = 1.0)]
	public double[] rd_carb;

	/// <summary>
	/// Optimum rate constant for decomposition of FOM pools [cellulose component] (0-1)
	/// </summary>
	[Param(MinVal = 0.0, MaxVal = 1.0)]
	public double[] rd_cell;

	/// <summary>
	/// Optimum rate constant for decomposition of FOM pools [lignin component] (0-1)
	/// </summary>
	[Param(MinVal = 0.0, MaxVal = 1.0)]
	public double[] rd_lign;

	/// <summary>
	/// Fraction of FOM C mineralised retained in system (0-1) 
	/// </summary>
	[Param(MinVal = 0.0, MaxVal = 1.0)]
	public double ef_fom;

	/// <summary>
	/// Fraction of retained FOM C transferred to biomass (0-1)
	/// </summary>
	[Param(MinVal = 0.0, MaxVal = 1.0)]
	public double fr_fom_biom;

	#region Old parameters

	/// <summary>
	/// Coeff. to determine the magnitude of C:N effects on decomposition of FOM
	/// </summary>
	[Param(MinVal = 0.0, MaxVal = 10.0)]
	public double cnrf_coeff;

	/// <summary>
	/// C:N above which decomposition rate of FOM declines
	/// </summary>
	[Param(MinVal = 5.0, MaxVal = 100.0)]
	public double cnrf_optcn;

	#endregion

	#region New parameters

	/// <summary>
	/// Data for calculating the temperature effect on FOM mineralisation
	/// </summary>
	private BendingStickData TempFactorData_MinerFOM = new BendingStickData();


	/// <summary>
	/// Optimum temperature for mineralisation of FOM
	/// </summary>
	[Param]
	public double[] stfMinerFOM_Topt
	{ set { TempFactorData_MinerFOM.xValueForOptimum = value; } }

	/// <summary>
	/// Temperature factor for mineralisation of FOM at zero degrees
	/// </summary>
	[Param]
	public double[] stfMinerFOM_FctrZero
	{ set { TempFactorData_MinerFOM.yValueAtZero = value; } }

	/// <summary>
	/// Curve exponent for temperature factor for mineralisation of FOM
	/// </summary>
	[Param]
	public double[] stfMinerFOM_CvExp
	{ set { TempFactorData_MinerFOM.CurveExponent = value; } }

	/// <summary>
	/// Parameters for calculating the soil moisture factor for FOM mineralisation
	/// </summary>
	private BrokenStickData MoistFactorData_MinerFOM = new BrokenStickData();

	/// <summary>
	/// Values of modified soil water content at which the moisture factor is given
	/// </summary>
	[Param]
	public double[] swfMinerFOM_x
	{ set { MoistFactorData_MinerFOM.xVals = value; } }

	/// <summary>
	/// Moiture factor values for the given modified soil water content
	/// </summary>
	[Param]
	public double[] swfMinerFOM_y
	{ set { MoistFactorData_MinerFOM.yVals = value; } }

	/// <summary>
	/// Optimum C:N ratio, below which mineralisation of FOM is unlimited
	/// </summary>
	private double CNFactorMinerFOM_OptCN;
	[Param]
	public double cnfMinerFOM_OptCN
	{ set { CNFactorMinerFOM_OptCN = value; } }

	/// <summary>
	/// Decrease for the CN factor when C:N is greater then optimum
	/// </summary>
	private double CNFactorMinerFOM_RateCN;
	[Param]
	public double cnfMinerFOM_RateCN
	{ set { CNFactorMinerFOM_RateCN = value; } }

	#endregion

	#endregion

	#endregion

	#region Parameters for SOM mineralisation/immobilisation process

	/// <summary>
	/// Potential rate of soil biomass mineralisation (fraction per day)
	/// </summary>
	[Param(MinVal = 0.0, MaxVal = 1.0)]
	public double[] rd_biom;

	/// <summary>
	/// Fraction of biomass C mineralised retained in system (0-1)
	/// </summary>
	[Param(MinVal = 0.0, MaxVal = 1.0)]
	public double ef_biom;

	/// <summary>
	/// Fraction of retained biomass C returned to biomass (0-1)
	/// </summary>
	[Param(MinVal = 0.0, MaxVal = 1.0)]
	public double fr_biom_biom;

	/// <summary>
	/// Potential rate of humus mineralisation (per day)
	/// </summary>
	[Param(MinVal = 0.0, MaxVal = 1.0)]
	public double[] rd_hum;

	/// <summary>
	/// Fraction of humic C mineralised retained in system (0-1)
	/// </summary>
	[Param(MinVal = 0.0, MaxVal = 1.0)]
	public double ef_hum;

	#region Old parameters

	/// <summary>
	/// Soil temperature above which there is no further effect on mineralisation and nitrification (oC)
	/// </summary>
	[Param(MinVal = 5.0, MaxVal = 100.0)]
	public double[] opt_temp;

	/// <summary>
	/// Index specifying water content for computing moisture factor for mineralisation
	/// </summary>
	[Param(MinVal = 0.0, MaxVal = 2.0)]
	public double[] wfmin_index;

	/// <summary>
	/// Value of moisture factor (for mineralisation) function at given index values
	/// </summary>
	[Param(MinVal = 0.0, MaxVal = 1.0)]
	public double[] wfmin_values;

	#endregion

	#region New parameters

	/// <summary>
	/// Data to calculate the temperature effect on SOM mineralisation
	/// </summary>
	private BendingStickData TempFactorData_MinerSOM = new BendingStickData();

	/// <summary>
	/// Optimum temperature for OM mineralisation
	/// </summary>
	[Param]
	public double[] stfMiner_Topt
	{
		get { return TempFactorData_MinerSOM.xValueForOptimum; }
		set { TempFactorData_MinerSOM.xValueForOptimum = value; }
	}

	/// <summary>
	/// Temperature factor for OM mineralisation at zero degree
	/// </summary>
	[Param]
	public double[] stfMiner_FctrZero
	{
		get { return TempFactorData_MinerSOM.yValueAtZero; }
		set { TempFactorData_MinerSOM.yValueAtZero = value; }
	}

	/// <summary>
	/// Curve exponent to calculate temperature factor for OM mineralisation
	/// </summary>
	[Param]
	public double[] stfMiner_CvExp
	{
		get { return TempFactorData_MinerSOM.CurveExponent; }
		set { TempFactorData_MinerSOM.CurveExponent = value; }
	}

	/// <summary>
	/// Parameters to calculate soil moisture factor for OM mineralisation
	/// </summary>
	/// <remarks>
	/// These are pairs of points representing a broken stick function
	/// </remarks>
	private BrokenStickData MoistFactorData_MinerSOM = new BrokenStickData();

	/// <summary>
	/// Values of the modified soil water content at which misture factor is know
	/// </summary>
	[Param]
	public double[] swfMiner_x
	{
		get { return MoistFactorData_MinerSOM.xVals; }
		set { MoistFactorData_MinerSOM.xVals = value; }
	}

	/// <summary>
	/// Values of the moisture factor at the given modified water content
	/// </summary>
	[Param]
	public double[] swfMiner_y
	{
		get { return MoistFactorData_MinerSOM.yVals; }
		set { MoistFactorData_MinerSOM.yVals = value; }
	}

	#region Parameters for each OM type

	#region Humic pool

	/// <summary>
	/// Parameters to calculate the temperature effects on mineralisation - humus
	/// </summary>
	private BendingStickData TempFactorData_MinerSOM_Hum = new BendingStickData();

	/// <summary>
	/// Optimum temperature for mineralisation of humus
	/// </summary>
	[Param]
	public double[] stfMinerHum_Topt
	{
		get { return TempFactorData_MinerSOM_Hum.xValueForOptimum; }
		set { TempFactorData_MinerSOM_Hum.xValueForOptimum = value; }
	}

	/// <summary>
	/// Temperature factor for mineralisation of humus at zero degrees
	/// </summary>
	[Param]
	public double[] stfMinerHum_FctrZero
	{
		get { return TempFactorData_MinerSOM_Hum.yValueAtZero; }
		set { TempFactorData_MinerSOM_Hum.yValueAtZero = value; }
	}

	/// <summary>
	/// Curve exponent for calculating the temperature factor for mineralisation of humus
	/// </summary>
	[Param]
	public double[] stfMinerHum_CvExp
	{
		get { return TempFactorData_MinerSOM_Hum.CurveExponent; }
		set { TempFactorData_MinerSOM_Hum.CurveExponent = value; }
	}

	/// <summary>
	/// Parameters to calculate the soil moisture factor for mineralisation of humus
	/// </summary>
	private BrokenStickData MoistFactorData_MinerSOM_Hum = new BrokenStickData();

	/// <summary>
	/// Values of the modified soil water content at which the moisture factor is know
	/// </summary>
	[Param]
	public double[] swfMinerHum_x
	{
		get { return MoistFactorData_MinerSOM_Hum.xVals; }
		set { MoistFactorData_MinerSOM_Hum.xVals = value; }
	}

	/// <summary>
	/// Values of the moisture factor at given water content values
	/// </summary>
	[Param]
	public double[] swfMinerHum_y
	{
		get { return MoistFactorData_MinerSOM_Hum.yVals; }
		set { MoistFactorData_MinerSOM_Hum.yVals = value; }
	}

	# endregion

	#region M biomass pool

	/// <summary>
	/// Parameters to calculate the temperature effects on mineralisation - biom
	/// </summary>
	private BendingStickData TempFactorData_MinerSOM_Biom = new BendingStickData();

	/// <summary>
	/// Optimum temperature for mineralisation of biom
	/// </summary>
	[Param]
	public double[] stfMinerBiom_Topt
	{
		get { return TempFactorData_MinerSOM_Biom.xValueForOptimum; }
		set { TempFactorData_MinerSOM_Biom.xValueForOptimum = value; }
	}

	/// <summary>
	/// Temperature factor for mineralisation of biom at zero degrees
	/// </summary>
	[Param]
	public double[] stfMinerBiom_FctrZero
	{
		get { return TempFactorData_MinerSOM_Biom.yValueAtZero; }
		set { TempFactorData_MinerSOM_Biom.yValueAtZero = value; }
	}

	/// <summary>
	/// Curve exponent for calculating the temperature factor for mineralisation of biom
	/// </summary>
	[Param]
	public double[] stfMinerBiom_CvExp
	{
		get { return TempFactorData_MinerSOM_Biom.CurveExponent; }
		set { TempFactorData_MinerSOM_Biom.CurveExponent = value; }
	}

	/// <summary>
	/// Parameters to calculate the soil moisture factor for mineralisation of biom
	/// </summary>
	private BrokenStickData MoistFactorData_MinerSOM_Biom = new BrokenStickData();

	/// <summary>
	/// Values of the modified soil water content at which the moisture factor is know
	/// </summary>
	[Param]
	public double[] swfMinerBiom_x
	{
		get { return MoistFactorData_MinerSOM_Biom.xVals; }
		set { MoistFactorData_MinerSOM_Biom.xVals = value; }
	}

	/// <summary>
	/// Values of the moisture factor at given water content values
	/// </summary>
	[Param]
	public double[] swfMinerBiom_y
	{
		get { return MoistFactorData_MinerSOM_Biom.yVals; }
		set { MoistFactorData_MinerSOM_Biom.yVals = value; }
	}

	# endregion

	#endregion

	#endregion


	#endregion

	#region Parameters for urea hydrolisys process

	/// <summary>
	/// Parameters to calculate the temperature effect on urea hydrolysis
	/// </summary>
	private BendingStickData TempFactorData_UHydrol = new BendingStickData();

	/// <summary>
	/// Optimum temperature for urea hydrolisys
	/// </summary>
	[Param]
	public double[] stfHydrol_Topt
	{
		get { return TempFactorData_UHydrol.xValueForOptimum; }
		set { TempFactorData_UHydrol.xValueForOptimum = value; }
	}

	/// <summary>
	/// Temperature factor for urea hydrolisys at zero degrees
	/// </summary>
	[Param]
	public double[] stfHydrol_FctrZero
	{
		get { return TempFactorData_UHydrol.yValueAtZero; }
		set { TempFactorData_UHydrol.yValueAtZero = value; }
	}

	/// <summary>
	/// Curve exponent to calculate the temperature factor for urea hydrolisys
	/// </summary>
	[Param]
	public double[] stfHydrol_CvExp
	{
		get { return TempFactorData_UHydrol.CurveExponent; }
		set { TempFactorData_UHydrol.CurveExponent = value; }
	}

	/// <summary>
	/// Parameters to calculate the moisture effect on urea hydrolysis
	/// </summary>
	private BrokenStickData MoistFactorData_UHydrol = new BrokenStickData();

	/// <summary>
	/// Values of the modified soil water content at which factor is known
	/// </summary>
	[Param]
	public double[] swfHydrol_x
	{
		get { return MoistFactorData_UHydrol.xVals; }
		set { MoistFactorData_UHydrol.xVals = value; }
	}

	/// <summary>
	/// Values of the modified moisture factor at given water content
	/// </summary>
	[Param]
	public double[] swfHydrol_y
	{
		get { return MoistFactorData_UHydrol.yVals; }
		set { MoistFactorData_UHydrol.yVals = value; }
	}

	/// Parameters for calculating the potential urea hydrolisys
	/// <summary>
	/// Minimum value for hydrolysis rate
	/// </summary>
	[Param]
	public double potHydrol_min;

	/// <summary>
	/// Paramter A of the function determining potential urea hydrolysis
	/// </summary>
	[Param]
	public double potHydrol_parmA;

	/// <summary>
	/// Paramter B of the function determining potential urea hydrolysis
	/// </summary>
	[Param]
	public double potHydrol_parmB;

	/// <summary>
	/// Paramter C of the function determining potential urea hydrolysis
	/// </summary>
	[Param]
	public double potHydrol_parmC;

	/// <summary>
	/// Paramter D of the function determining potential urea hydrolysis
	/// </summary>
	[Param]
	public double potHydrol_parmD;

	#endregion

	#region Parameters for nitrification process

	/// <summary>
	/// Maximum potential nitrification (ppm/day)
	/// </summary>
	/// <remarks>
	/// This is the parameter M on Michaelis-Menten equation
	/// r = MC/(k+C)
	/// </remarks>
	[Param(MinVal = 0.0, MaxVal = 100.0)]
	public double nitrification_pot;

	/// <summary>
	/// NH4 conc. at half potential rate (ppm)
	/// </summary>
	/// <remarks>
	/// This is the parameter k on Michaelis-Menten equation
	/// r = MC/(k+C)
	/// </remarks>
	[Param(MinVal = 0.0, MaxVal = 200.0)]
	public double nh4_at_half_pot;

	#region Old parameters

	/// <summary>
	/// Index specifying water content for water factor for nitrification
	/// </summary>
	[Param(MinVal = 0.0, MaxVal = 2.0)]
	public double[] wfnit_index;

	/// <summary>
	/// Value of water factor (for nitrification) function at given index values
	/// </summary>
	[Param(MinVal = 0.0, MaxVal = 1.0)]
	public double[] wfnit_values;

	/// <summary>
	/// pH values for specifying pH factor for nitrification
	/// </summary>
	[Param(MinVal = 0.0, MaxVal = 14.0)]
	public double[] pHf_nit_pH;

	/// <summary>
	/// Value of pH factor (for nitrification) function for given pH values
	/// </summary>
	[Param(MinVal = 0.0, MaxVal = 1.0)]
	public double[] pHf_nit_values;

	#endregion

	#region New parameters

	/// <summary>
	/// Parameters to calculate the temperature effect on nitrification
	/// </summary>
	private BendingStickData TempFactorData_Nitrif = new BendingStickData();

	/// <summary>
	/// Optimum temperature for nitrification
	/// </summary>
	[Param]
	public double[] stfNitrif_Topt
	{
		get { return TempFactorData_Nitrif.xValueForOptimum; }
		set { TempFactorData_Nitrif.xValueForOptimum = value; }
	}

	/// <summary>
	/// Temperature factor for nitrification at zero degrees
	/// </summary>
	[Param]
	public double[] stfNitrif_FctrZero
	{
		get { return TempFactorData_Nitrif.yValueAtZero; }
		set { TempFactorData_Nitrif.yValueAtZero = value; }
	}

	/// <summary>
	/// Curve exponent for calculating the temperature factor for nitrification
	/// </summary>
	[Param]
	public double[] stfNitrif_CvExp
	{
		get { return TempFactorData_Nitrif.CurveExponent; }
		set { TempFactorData_Nitrif.CurveExponent = value; }
	}

	/// <summary>
	/// Parameters to calculate the soil moisture factor for nitrification
	/// </summary>
	private BrokenStickData MoistFactorData_Nitrif = new BrokenStickData();

	/// <summary>
	/// Values of the modified soil water content at which the moisture factor is known
	/// </summary>
	[Param]
	public double[] swfNitrif_x
	{
		get { return MoistFactorData_Nitrif.xVals; }
		set { MoistFactorData_Nitrif.xVals = value; }
	}

	/// <summary>
	/// Values of the moisture factor at given water content
	/// </summary>
	[Param]
	public double[] swfNitrif_y
	{
		get { return MoistFactorData_Nitrif.yVals; }
		set { MoistFactorData_Nitrif.yVals = value; }
	}

	/// <summary>
	/// Parameters to calculate the soil pH factor for nitrification
	/// </summary>
	private BrokenStickData pHFactorData_Nitrif = new BrokenStickData();

	/// <summary>
	/// Values of pH at which factors is known
	/// </summary>
	[Param]
	public double[] sphfNitrif_x
	{
		get { return pHFactorData_Nitrif.xVals; }
		set { pHFactorData_Nitrif.xVals = value; }
	}

	/// <summary>
	/// Values of pH factor ar given pH values
	/// </summary>
	[Param]
	public double[] sphfNitrif_y
	{
		get { return pHFactorData_Nitrif.yVals; }
		set { pHFactorData_Nitrif.yVals = value; }
	}

	#endregion

	#endregion

	#region Parameters for denitrification and N2O emission processes

	/// <summary>
	/// Denitrification rate coefficient (kg/mg)
	/// </summary>
	[Param(MinVal = 0.0, MaxVal = 1.0)]
	public double dnit_rate_coeff;

	/// <summary>
	/// Fraction of nitrification lost as denitrification
	/// </summary>
	[Param(MinVal = 0.0, MaxVal = 1.0)]
	public double dnit_nitrf_loss;

	/// <summary>
	/// Parameter k1 from Thorburn et al (2010) for N2O model
	/// </summary>
	[Param(MinVal = 0.0, MaxVal = 100.0)]
	public double dnit_k1;

	#region Old parameters

	/// <summary>
	/// Power term to calculate water factor for denitrification
	/// </summary>
	[Param(MinVal = 0.0, MaxVal = 5.0)]
	public double dnit_wf_power;

	/// <summary>
	/// Values of WFPS for calculating the N2O fraction of denitrification
	/// </summary>
	[Param(MinVal = 0.0, MaxVal = 100.0)]
	public double[] dnit_wfps;

	/// <summary>
	/// Values of WFPS factor for N2O fraction of denitrification
	/// </summary>
	[Param(MinVal = 0.0, MaxVal = 100.0)]
	public double[] dnit_n2o_factor;

	#endregion

	#region New parameters

	/// <summary>
	/// Parameter A to compute active carbon (for denitrification)
	/// </summary>
	[Param]
	public double actC_parmB;

	/// <summary>
	/// Parameter B to compute active carbon (for denitrification)
	/// </summary>
	[Param]
	public double actC_parmA;

	/// <summary>
	/// Parameters to calculate the temperature effect on denitrification
	/// </summary>
	private BendingStickData TempFactorData_Denit = new BendingStickData();

	/// <summary>
	/// Optimum temperature for denitrification
	/// </summary>
	[Param]
	public double[] stfDenit_Topt
	{
		get { return TempFactorData_Denit.xValueForOptimum; }
		set { TempFactorData_Denit.xValueForOptimum = value; }
	}

	/// <summary>
	/// Temperature factor for denitrification at zero degrees
	/// </summary>
	[Param]
	public double[] stfDenit_FctrZero
	{
		get { return TempFactorData_Denit.yValueAtZero; }
		set { TempFactorData_Denit.yValueAtZero = value; }
	}

	/// <summary>
	/// Curve exponent for calculating the temperature factor for denitrification
	/// </summary>
	[Param]
	public double[] stfDenit_CvExp
	{
		get { return TempFactorData_Denit.CurveExponent; }
		set { TempFactorData_Denit.CurveExponent = value; }
	}

	/// <summary>
	/// Parameters to calculate the soil moisture factor for denitrification
	/// </summary>
	private BrokenStickData MoistFactorData_Denit = new BrokenStickData();

	/// <summary>
	/// Values of modified soil water content at which the moisture factor is known
	/// </summary>
	[Param]
	public double[] swfDenit_x
	{
		get { return MoistFactorData_Denit.xVals; }
		set { MoistFactorData_Denit.xVals = value; }
	}

	/// <summary>
	/// Values of the moisture factor at given water content values
	/// </summary>
	[Param]
	public double[] swfDenit_y
	{
		get { return MoistFactorData_Denit.yVals; }
		set { MoistFactorData_Denit.yVals = value; }
	}

	/// <summary>
	/// Parameter A in the N2N2O function
	/// </summary>
	[Param]
	public double N2N2O_parmA;

	/// <summary>
	/// Parameter B in the N2N2O function
	/// </summary>
	[Param]
	public double N2N2O_parmB;

	/// <summary>
	/// Parameters to calculate the soil moisture factor for denitrification gas ratio
	/// </summary>
	private BrokenStickData WFPSFactorData_N2N2O = new BrokenStickData();

	/// <summary>
	/// Values of modified soil water content at which the moisture factor is known
	/// </summary>
	[Param]
	public double[] wfpsN2N2O_x
	{
		get { return WFPSFactorData_N2N2O.xVals; }
		set { WFPSFactorData_N2N2O.xVals = value; }
	}

	/// <summary>
	/// Values of the moisture factor at given water content values
	/// </summary>
	[Param]
	public double[] wfpsN2N2O_y
	{
		get { return WFPSFactorData_N2N2O.yVals; }
		set { WFPSFactorData_N2N2O.yVals = value; }
	}

	#endregion

	#endregion

	#endregion

	#region Parameters that do or may change during simulation

	#region Soil physics data

	/// <summary>
	/// Soil layers' thichness (mm)
	/// </summary>
	[Input]
	[Units("mm")]
	private double[] dlayer;

	/// <summary>
	/// Soil bulk density for each layer (g/cm3)
	/// </summary>
	[Input]
	[Units("g/cm^3")]
	private double[] bd;
	//private float[] bd;
	//private double[] SoilDensity;

	/// <summary>
	/// Soil water amount at saturation (mm)
	/// </summary>
	[Input]
	[Units("mm")]
	private double[] sat_dep;

	/// <summary>
	/// Soil water amount at drainage upper limit (mm)
	/// </summary>
	[Input]
	[Units("mm")]
	private double[] dul_dep;

	/// <summary>
	/// Soil water amount at drainage lower limit (mm)
	/// </summary>
	[Input]
	[Units("mm")]
	private double[] ll15_dep;

	/// <summary>
	/// Today's soil water amount (mm)
	/// </summary>
	[Input]
	[Units("mm")]
	private double[] sw_dep;

	/// <summary>
	/// Soil albedo (0-1)
	/// </summary>
	[Input]
	private double salb;
	//{ get; private set; }

	/// <summary>
	/// Soil temperature (oC), as computed by an external module (SoilTemp)
	/// </summary>
	[Input(IsOptional = true)]
	[Units("oC")]
	private double[] ave_soil_temp;

	#endregion

	#region Soil pH data

	/// <summary>
	/// pH of soil (assumed equivalent to a 1:1 soil-water slurry)
	/// </summary>
	[Param(IsOptional = true, MinVal = 3.5, MaxVal = 11.0)]
	[Input(IsOptional = true)]
	public double[] ph;

	#endregion

	#region Values for soil organic matter (som)

	/// <summary>
	/// Stores initial OC values until dlayer is available, can be used for a Reset operation
	/// </summary>
	private double[] OC_reset;

	/// <summary>
	/// Total soil organic carbon content (%)
	/// </summary>
	[Param]
	[Output]
	[Units("%")]
	[Description("Soil organic carbon (exclude FOM)")]
	public double[] oc
	{
		get
		{
			double[] result;
			if (initDone)
			{
				result = new double[dlayer.Length];
				for (int k = 0; k < Patch.Count; k++)
					for (int layer = 0; layer < dlayer.Length; ++layer)
						result[layer] += Patch[k].oc[layer] * Patch[k].RelativeArea;
			}
			else
			{
				// no value has been asigned yet, return null
				result = OC_reset;
			}
			return result;
		}
		set
		{
			if (initDone)
			{
				Console.WriteLine(" Attempt to assign values for OC during simulation, "
								 + "this operation is not valid and will be ignored");
			}
			else
			{
				// Store initial values, initialisation of C pools is done on InitCalc. Can be used OnReset
				OC_reset = value;
			}
		}
	}

	#endregion

	#region Values for soil mineral N

	/// <summary>
	/// Stores initial values until dlayer is available, can be used for a Reset operation
	/// </summary>
	private double[] ureappm_reset;

	/// <summary>
	/// Soil urea nitrogen content (ppm)
	/// </summary>
	[Param(IsOptional = true, MinVal = 0.0, MaxVal = 10000.0)]
	[Output]
	[Units("mg/kg")]
	[Description("Soil urea nitrogen content")]
	public double[] ureappm
	{
		get
		{
			double[] result;
			if (initDone)
			{
				result = new double[dlayer.Length];
				for (int k = 0; k < Patch.Count; k++)
					for (int layer = 0; layer < dlayer.Length; ++layer)
						result[layer] += Patch[k].urea[layer] * convFactor_kgha2ppm(layer) * Patch[k].RelativeArea;
			}
			else
				result = ureappm_reset;
			return result;
		}
		set
		{
			if (initDone)
			{
				double sumOld = MathUtility.Sum(urea);      // original amount

				for (int layer = 0; layer < value.Length; ++layer)
					value[layer] = MathUtility.Divide(value[layer], convFactor_kgha2ppm(layer), 0.0);       //Convert from ppm to kg/ha
				for (int k = 0; k < Patch.Count; k++)
					Patch[k].urea = value;

				if (!inReset)
					SendExternalMassFlowN(MathUtility.Sum(urea) - sumOld);

			}
			else
				ureappm_reset = value;
		}
	}

	/// <summary>
	/// Soil urea nitrogen amount (kgN/ha)
	/// </summary>
	[Output]
	[Units("kg/ha")]
	[Description("Soil urea nitrogen amount")]
	public double[] urea
	{
		get
		{
			double[] result = new double[dlayer.Length];
			for (int k = 0; k < Patch.Count; k++)
				for (int layer = 0; layer < dlayer.Length; ++layer)
					result[layer] += Patch[k].urea[layer] * Patch[k].RelativeArea;
			return result;
		}
		set  // should this be private?
		{
			double sumOld = MathUtility.Sum(urea);

			for (int k = 0; k < Patch.Count; k++)
				Patch[k].urea = value;

			SendExternalMassFlowN(MathUtility.Sum(urea) - sumOld);
		}
	}

	/// <summary>
	/// Stores initial values until dlayer is available, can be used for a Reset operation
	/// </summary>
	private double[] nh4ppm_reset;
	/// <summary>
	/// Soil ammonium nitrogen content (ppm)
	/// </summary>
	[Param(IsOptional = true, MinVal = 0.0, MaxVal = 10000.0)]
	[Output]
	[Units("mg/kg")]
	[Description("Soil ammonium nitrogen content")]
	public double[] nh4ppm
	{
		get
		{
			double[] result;
			if (initDone)
			{
				result = new double[dlayer.Length];
				for (int k = 0; k < Patch.Count; k++)
					for (int layer = 0; layer < dlayer.Length; ++layer)
						result[layer] += Patch[k].nh4[layer] * convFactor_kgha2ppm(layer) * Patch[k].RelativeArea;
			}
			else
				result = nh4ppm_reset;
			return result;
		}
		set
		{
			if (initDone)
			{
				double sumOld = MathUtility.Sum(nh4);   // original values

				for (int layer = 0; layer < value.Length; ++layer)
					value[layer] = MathUtility.Divide(value[layer], convFactor_kgha2ppm(layer), 0.0);       //Convert from ppm to kg/ha
				for (int k = 0; k < Patch.Count; k++)
					Patch[k].nh4 = value;

				if (!inReset)
					SendExternalMassFlowN(MathUtility.Sum(nh4) - sumOld);
			}
			else
				nh4ppm_reset = value;
		}
	}

	/// <summary>
	/// Soil ammonium nitrogen amount (kg/ha)
	/// </summary>
	[Output]
	[Units("kg/ha")]
	[Description("Soil ammonium nitrogen amount")]
	public double[] nh4
	{
		get
		{
			double[] result = new double[dlayer.Length];
			for (int k = 0; k < Patch.Count; k++)
				for (int layer = 0; layer < dlayer.Length; ++layer)
					result[layer] += Patch[k].nh4[layer] * Patch[k].RelativeArea;
			return result;
		}
		set  // should this be private?
		{
			double sumOld = MathUtility.Sum(nh4);

			for (int k = 0; k < Patch.Count; k++)
				Patch[k].nh4 = value;

			SendExternalMassFlowN(MathUtility.Sum(nh4) - sumOld);
		}
	}

	/// <summary>
	/// Stores initial values until dlayer is available, can be used for a Reset operation
	/// </summary>
	private double[] no3ppm_reset;

	/// <summary>
	/// Soil nitrate nitrogen content (ppm)
	/// </summary>
	[Param(IsOptional = true, MinVal = 0.0, MaxVal = 10000.0)]
	[Output]
	[Units("mg/kg")]
	[Description("Soil nitrate nitrogen content")]
	public double[] no3ppm
	{
		get
		{
			double[] result;
			if (initDone)
			{
				result = new double[dlayer.Length];
				for (int k = 0; k < Patch.Count; k++)
					for (int layer = 0; layer < dlayer.Length; ++layer)
						result[layer] += Patch[k].no3[layer] * convFactor_kgha2ppm(layer) * Patch[k].RelativeArea;
			}
			else
				result = no3ppm_reset;
			return result;
		}
		set
		{
			if (initDone)
			{
				double sumOld = MathUtility.Sum(no3);   // original values
				for (int layer = 0; layer < value.Length; ++layer)
					value[layer] = MathUtility.Divide(value[layer], convFactor_kgha2ppm(layer), 0.0);       //Convert from ppm to kg/ha
				for (int k = 0; k < Patch.Count; k++)
					Patch[k].no3 = value;

				if (!inReset)
					SendExternalMassFlowN(MathUtility.Sum(no3) - sumOld);
			}
			else
				no3ppm_reset = value;
		}
	}

	/// <summary>
	/// Soil nitrate nitrogen amount (kgN/ha)
	/// </summary>
	[Output]
	[Units("kg/ha")]
	[Description("Soil nitrate nitrogen amount")]
	public double[] no3
	{
		get
		{
			double[] result = new double[dlayer.Length];
			for (int layer = 0; layer < dlayer.Length; ++layer)
				for (int k = 0; k < Patch.Count; k++)
					result[layer] += Patch[k].no3[layer] * Patch[k].RelativeArea;
			return result;
		}
		set  // should this be private? or not exist at all?
		{
			double sumOld = MathUtility.Sum(no3);
			for (int k = 0; k < Patch.Count; k++)
				Patch[k].no3 = value;

			SendExternalMassFlowN(MathUtility.Sum(no3) - sumOld);
		}
	}

	#endregion

	#region Soil loss data

	/// <summary>
	/// Indicates whether soil profile reduction is allowed (from erosion)
	/// </summary>
	private bool allowProfileReduction = false;
	[Input(IsOptional = true)]
	private string n_reduction
	{ set { allowProfileReduction = value.StartsWith("on"); } }

	/// <summary>
	/// Soil loss, due to erosion (?)
	/// </summary>
	[Input(IsOptional = true)]
	[Units("t/ha")]
	private double soil_loss;

	#endregion

	#region Pond data

	/// <summary>
	/// Indicates whether pond is active or not
	/// </summary>
	private Boolean isPondActive = false;
	[Input(IsOptional = true)]
	private string pond_active
	{ set { isPondActive = (value == "yes"); } }

	/// <summary>
	/// Amount of C decomposed in pond that is added to soil m. biomass
	/// </summary>
	[Input(IsOptional = true)]
	[Units("kg/ha")]
	private double pond_biom_C;
	//{ set {PondC_to_BiomC}; }

	/// <summary>
	/// Amount of C decomposed in pond that is added to soil humus
	/// </summary>	
	[Input(IsOptional = true)]
	[Units("kg/ha")]
	private double pond_hum_C;
	//{ set {PondC_to_HumC}; }

	#endregion

	#region Inhibitors data

	// factor reducing urea hydrolysis due to the presence of an inhibitor - not implemented yet
	private double[] InhibitionFactor_UHydrolysis = null;
	[Input(IsOptional = true)]
	[Units("0-1")]
	private double[] hydrolysis_inhibition
	{
		get { return InhibitionFactor_UHydrolysis; }
		set
		{
			InhibitionFactor_UHydrolysis = new double[dlayer.Length];
			for (int layer = 0; layer < dlayer.Length; layer++)
			{
				if (layer < value.Length)
				{
					InhibitionFactor_UHydrolysis[layer] = value[layer];
					if (InhibitionFactor_UHydrolysis[layer] < 0.0)
					{
						InhibitionFactor_UHydrolysis[layer] = 0.0;
						Console.WriteLine("Value for hydrolysis inhibition is below lower limit, value will be adjusted to 0.0");
					}
					else if (InhibitionFactor_UHydrolysis[layer] > 1.0)
					{
						InhibitionFactor_UHydrolysis[layer] = 1.0;
						Console.WriteLine("Value for hydrolysis inhibition is above upper limit, value will be adjusted to 1.0");
					}
				}
				else
					InhibitionFactor_UHydrolysis[layer] = 0.0;
			}
		}
	}

	/// <summary>
	/// Factor reducing nitrification due to the presence of a inhibitor
	/// </summary>
	private double[] InhibitionFactor_Nitrification = null;
	[Input(IsOptional = true)]
	[Units("0-1")]
	double[] nitrification_inhibition
	{
		set
		{
			InhibitionFactor_Nitrification = new double[dlayer.Length];
			for (int layer = 0; layer < dlayer.Length; layer++)
			{
				if (layer < value.Length)
				{
					InhibitionFactor_Nitrification[layer] = value[layer];
					if (InhibitionFactor_Nitrification[layer] < 0.0)
					{
						InhibitionFactor_Nitrification[layer] = 0.0;
						Console.WriteLine("Value for nitrification inhibition is below lower limit, value will be adjusted to 0.0");
					}
					else if (InhibitionFactor_Nitrification[layer] > 1.0)
					{
						InhibitionFactor_Nitrification[layer] = 1.0;
						Console.WriteLine("Value for nitrification inhibition is above upper limit, value will be adjusted to 1.0");
					}
				}
				else
					InhibitionFactor_Nitrification[layer] = 0.0;
			}
		}
	}

	// factor reducing urea hydrolysis due to the presence of an inhibitor - not implemented yet
	private double[] InhibitionFactor_Denitrification = null;
	[Input(IsOptional = true)]
	[Units("0-1")]
	private double[] Denitrification_inhibition
	{
		get { return InhibitionFactor_UHydrolysis; }
		set
		{
			InhibitionFactor_Denitrification = new double[dlayer.Length];
			for (int layer = 0; layer < dlayer.Length; layer++)
			{
				if (layer < value.Length)
				{
					InhibitionFactor_Denitrification[layer] = value[layer];
					if (InhibitionFactor_Denitrification[layer] < 0.0)
					{
						InhibitionFactor_Denitrification[layer] = 0.0;
						Console.WriteLine("Value for denitrification inhibition is below lower limit, "
							+ "value will be adjusted to 0.0");
					}
					else if (InhibitionFactor_Denitrification[layer] > 1.0)
					{
						InhibitionFactor_Denitrification[layer] = 1.0;
						Console.WriteLine("Value for denitrification inhibition is above upper limit, "
							+ "value will be adjusted to 1.0");
					}
				}
				else
					InhibitionFactor_Denitrification[layer] = 0.0;
			}
		}
	}

	// factor reducing mineralisation processes due to the presence of an inhibitor - not implemented yet
	private double[] InhibitionFactor_Mineralisation = null;
	[Input(IsOptional = true)]
	[Units("0-1")]
	private double[] mineralisation_inhibition
	{
		get { return InhibitionFactor_Mineralisation; }
		set
		{
			InhibitionFactor_Mineralisation = new double[dlayer.Length];
			for (int layer = 0; layer < dlayer.Length; layer++)
			{
				if (layer < value.Length)
				{
					InhibitionFactor_Mineralisation[layer] = value[layer];
					if (InhibitionFactor_Mineralisation[layer] < 0.0)
					{
						InhibitionFactor_Mineralisation[layer] = 0.0;
						Console.WriteLine("Value for mineralisation inhibition is below lower limit, value will be adjusted to 0.0");
					}
					else if (InhibitionFactor_Mineralisation[layer] > 1.0)
					{
						InhibitionFactor_Mineralisation[layer] = 1.0;
						Console.WriteLine("Value for mineralisation inhibition is above upper limit, value will be adjusted to 1.0");
					}
				}
				else
					InhibitionFactor_Mineralisation[layer] = 0.0;
			}
		}
	}

	#endregion

	#endregion

	#region Settable variables

	#region Mineral nitrogen

	/// <summary>
	/// Variations in ureappm as given by another component
	/// </summary>
	//[Input(IsOptional = true)]
	[Output]
	[Units("mg/kg")]
	private double[] dlt_ureappm
	{
		set
		{
			// for now any incoming dlt is passed to all patches, this will have to be handled differently in the future
			for (int layer = 0; layer < value.Length; ++layer)
				value[layer] = MathUtility.Divide(value[layer], convFactor_kgha2ppm(layer), 0.0);  // convert from ppm to kg/ha
			for (int k = 0; k < Patch.Count; k++)
				Patch[k].dlt_urea = value;
		}
	}

	/// <summary>
	/// Variations in urea as given by another component
	/// </summary>
	//[Input(IsOptional = true)]
	[Output]
	[Units("kg/ha")]
	private double[] dlt_urea
	{
		set
		{
			// for now any incoming dlt is passed to all patches, this will have to be handled differently in the future
			for (int k = 0; k < Patch.Count; k++)
				Patch[k].dlt_urea = value;
		}
	}

	/// <summary>
	/// Variations in nh4ppm as given by another component
	/// </summary>
	//[Input(IsOptional = true)]
	[Output]
	[Units("mg/kg")]
	private double[] dlt_nh4ppm
	{
		set
		{
			// for now any incoming dlt is passed to all patches, this will have to be handled differently in the future
			for (int layer = 0; layer < value.Length; ++layer)
				value[layer] = MathUtility.Divide(value[layer], convFactor_kgha2ppm(layer), 0.0);  // convert from ppm to kg/ha
			for (int k = 0; k < Patch.Count; k++)
				Patch[k].dlt_nh4 = value;
		}
	}

	/// <summary>
	/// Variations in nh4 as given by another component
	/// </summary>
	//[Input(IsOptional = true)]
	[Output]
	[Units("kg/ha")]
	private double[] dlt_nh4
	{
		set
		{
			// for now any incoming dlt is passed to all patches, this will have to be handled differently in the future
			for (int k = 0; k < Patch.Count; k++)
				Patch[k].dlt_nh4 = value;
		}
	}

	/// <summary>
	/// Variations in no3ppm as given by another component
	/// </summary>
	//[Input(IsOptional = true)]
	[Output]
	[Units("mg/kg")]
	private double[] dlt_no3ppm
	{
		set
		{
			// for now any incoming dlt is passed to all patches, this will have to be handled differently in the future
			for (int layer = 0; layer < value.Length; ++layer)
				value[layer] = MathUtility.Divide(value[layer], convFactor_kgha2ppm(layer), 0.0);  // convert from ppm to kg/ha
			for (int k = 0; k < Patch.Count; k++)
				Patch[k].dlt_no3 = value;
		}
	}

	/// <summary>
	/// Variations in no3 as given by another component
	/// </summary>
	//[Input(IsOptional = true)]
	[Output]
	[Units("kg/ha")]
	private double[] dlt_no3
	{
		set
		{
			// for now any incoming dlt is passed to all patches, this will have to be handled differently in the future
			for (int k = 0; k < Patch.Count; k++)
				Patch[k].dlt_no3 = value;
		}
	}

	#endregion

	#region Organic N and C

	/// <summary>
	/// Variations in org_n as given by another component
	/// </summary>
	//[Input(IsOptional = true)]
	[Output]
	[Units("kg/ha")]
	private double[] dlt_org_n
	{
		set
		{
			// for now any incoming dlt is passed to all patches, this might have to be handled differently in the future
			for (int k = 0; k < Patch.Count; k++)
				Patch[k].dlt_org_n = value;
		}
	}

	/// <summary>
	/// Variations in org_c_pool1 as given by another component
	/// </summary>
	//[Input(IsOptional = true)]
	[Output]
	[Units("kg/ha")]
	private double[] dlt_org_c_pool1
	{
		set
		{
			// for now any incoming dlt is passed to all patches, this might have to be handled differently in the future
			for (int k = 0; k < Patch.Count; k++)
				Patch[k].dlt_org_c_pool1 = value;
		}
	}

	/// <summary>
	/// Variations in org_c_pool2 as given by another component
	/// </summary>
	//[Input(IsOptional = true)]
	[Output]
	[Units("kg/ha")]
	private double[] dlt_org_c_pool2
	{
		set
		{
			// for now any incoming dlt is passed to all patches, this might have to be handled differently in the future
			for (int k = 0; k < Patch.Count; k++)
				Patch[k].dlt_org_c_pool2 = value;
		}
	}

	/// <summary>
	/// Variations in org_c_pool3 as given by another component
	/// </summary>
	//[Input(IsOptional = true)]
	[Output]
	[Units("kg/ha")]
	private double[] dlt_org_c_pool3
	{
		set
		{
			// for now any incoming dlt is passed to all patches, this might have to be handled differently in the future
			for (int k = 0; k < Patch.Count; k++)
				Patch[k].dlt_org_c_pool3 = value;
		}
	}

	#endregion

	#endregion

	#endregion

	#region Outputs we make available to other components

	#region Values that other components can get or set

	/// <summary>
	/// Amount of C in pool1 of FOM - doesn't seem to be fully implemented
	/// </summary>
	[Output]
	[Units("kg/ha")]
	[Description("Not fully implemented")]
	double[] org_c_pool1
	{
		get
		{
			double[] result = new double[dlayer.Length];
			for (int layer = 0; layer < dlayer.Length; ++layer)
				for (int k = 0; k < Patch.Count; k++)
					result[layer] += Patch[k].fom_c_pool1[layer] * Patch[k].RelativeArea;
			return result;
		}
		set
		{
			if (value.Length == dlayer.Length)
			{
				for (int layer = 0; layer < value.Length; ++layer)
				{
					if (value[layer] < 0)
						throw new Exception("Value given for fom_c_pool1 is negative");
					else
						for (int k = 0; k < Patch.Count; k++)
							Patch[k].fom_c_pool1[layer] = value[layer];
				}
			}
		}
	}

	/// <summary>
	/// Amount of C in pool2 of FOM - doesn't seem to be fully implemented
	/// </summary>
	[Output]
	[Units("kg/ha")]
	[Description("Not fully implemented")]
	double[] org_c_pool2
	{
		get
		{
			double[] result = new double[dlayer.Length];
			for (int layer = 0; layer < dlayer.Length; ++layer)
				for (int k = 0; k < Patch.Count; k++)
					result[layer] += Patch[k].fom_c_pool2[layer] * Patch[k].RelativeArea;
			return result;
		}
		set
		{
			if (value.Length == dlayer.Length)
			{
				for (int layer = 0; layer < value.Length; ++layer)
				{
					if (value[layer] < 0)
						throw new Exception("Value given for fom_c_pool2 is negative");
					else
						for (int k = 0; k < Patch.Count; k++)
							Patch[k].fom_c_pool2[layer] = value[layer];
				}
			}
		}
	}

	/// <summary>
	/// Amount of C in pool3 of FOM - doesn't seem to be fully implemented
	/// </summary>
	[Output]
	[Units("kg/ha")]
	[Description("Not fully implemented")]
	double[] org_c_pool3
	{
		get
		{
			double[] result = new double[dlayer.Length];
			for (int layer = 0; layer < dlayer.Length; ++layer)
				for (int k = 0; k < Patch.Count; k++)
					result[layer] += Patch[k].fom_c_pool3[layer] * Patch[k].RelativeArea;
			return result;
		}
		set
		{
			if (value.Length == dlayer.Length)
			{
				for (int layer = 0; layer < value.Length; ++layer)
				{
					if (value[layer] < 0)
						throw new Exception("Value given for fom_c_pool3 is negative");
					else
						for (int k = 0; k < Patch.Count; k++)
							Patch[k].fom_c_pool3[layer] = value[layer];
				}
			}
		}
	}

	/// <summary>
	/// Amount of N in FOM - doesn't seem to be fully implemented
	/// </summary>
	[Output]
	[Units("kg/ha")]
	[Description("Not fully implemented")]
	double[] org_n
	{
		get
		{
			double[] result = new double[dlayer.Length];
			for (int layer = 0; layer < dlayer.Length; ++layer)
				for (int k = 0; k < Patch.Count; k++)
					result[layer] += Patch[k].fom_n[layer] * Patch[k].RelativeArea;
			return result;
		}
		set
		{
			if (value.Length == dlayer.Length)
			{
				for (int layer = 0; layer < value.Length; ++layer)
				{
					if (value[layer] < 0)
						throw new Exception("Value given for fom_n is negative");
					else
						for (int k = 0; k < Patch.Count; k++)
							Patch[k].fom_n[layer] = value[layer];

				}
			}
		}
	}

	#endregion

	#region Values that other components can only get

	#region Outputs for Nitrogen

	#region General values

	/// <summary>
	/// Minimum allowable urea amount in each layer
	/// </summary>
	[Output]
	[Units("kg/ha")]
	[Description("Minimum allowable urea")]
	double[] urea_min;

	/// <summary>
	/// Minimum allowable NH4 amount in each layer
	/// </summary>
	[Output]
	[Units("kg/ha")]
	[Description("Minimum allowable NH4")]
	double[] nh4_min;

	/// <summary>
	/// Minimum allowable NO3 amount in each layer
	/// </summary>
	[Output]
	[Units("kg/ha")]
	[Description("Minimum allowable NO3")]
	double[] no3_min;

	#endregion

	#region Changes for today - deltas

	/// <summary>
	/// N carried out in sediment via runoff/erosion
	/// </summary>
	[Output]
	[Units("kg")]
	[Description("N loss carried in sediment")]
	double dlt_n_loss_in_sed
	{
		get
		{
			double result = 0.0;
			for (int k = 0; k < Patch.Count; k++)
				result += Patch[k].dlt_n_loss_in_sed * Patch[k].RelativeArea;
			return result;
		}
	}

	/// <summary>
	/// Net nh4 change today
	/// </summary>
	[Output]
	[Units("kg/ha")]
	[Description("Net NH4 change today")]
	double[] dlt_nh4_net
	{
		get
		{
			double[] result = new double[dlayer.Length];
			for (int layer = 0; layer < dlayer.Length; ++layer)
				for (int k = 0; k < Patch.Count; k++)
					result[layer] += Patch[k].dlt_nh4_net[layer] * Patch[k].RelativeArea;
			return result;
		}
	}

	/// <summary>
	/// Net NH4 transformation today
	/// </summary>
	[Output]
	[Units("kg/ha")]
	[Description("Net NH4 transformation")]
	double[] nh4_transform_net
	{
		get
		{
			double[] result = new double[dlayer.Length];
			for (int layer = 0; layer < dlayer.Length; ++layer)
				for (int k = 0; k < Patch.Count; k++)
					result[layer] += Patch[k].nh4_transform_net[layer] * Patch[k].RelativeArea;
			return result;
		}
	}

	/// <summary>
	/// Net no3 change today
	/// </summary>
	[Output]
	[Units("kg/ha")]
	[Description("Net NO3 change today")]
	double[] dlt_no3_net
	{
		get
		{
			double[] result = new double[dlayer.Length];
			for (int layer = 0; layer < dlayer.Length; ++layer)
				for (int k = 0; k < Patch.Count; k++)
					result[layer] += Patch[k].dlt_no3_net[layer] * Patch[k].RelativeArea;
			return result;
		}
	}

	/// <summary>
	/// Net NO3 transformation today
	/// </summary>
	[Output]
	[Units("kg/ha")]
	[Description("Net NO3 transformation")]
	double[] no3_transform_net
	{
		get
		{
			double[] result = new double[dlayer.Length];
			for (int layer = 0; layer < dlayer.Length; ++layer)
				for (int k = 0; k < Patch.Count; k++)
					result[layer] += Patch[k].no3_transform_net[layer] * Patch[k].RelativeArea;
			return result;
		}
	}

	/// <summary>
	/// Net mineralisation today
	/// </summary>
	[Output]
	[Units("kg/ha")]
	[Description("Net N mineralised in soil")]
	double[] dlt_n_min
	{
		get
		{
			double[] result = new double[dlayer.Length];
			for (int layer = 0; layer < dlayer.Length; layer++)
				for (int k = 0; k < Patch.Count; k++)
					result[layer] += Patch[k].dlt_n_min[layer] * Patch[k].RelativeArea;
			return result;
		}
	}

	/// <summary>
	/// Net N mineralisation from residue decomposition
	/// </summary>
	[Output]
	[Units("kg/ha")]
	[Description("Net N mineralisation from residue decomposition")]
	double[] dlt_n_min_res
	{
		get
		{
			double[] result = new double[dlayer.Length];
			for (int layer = 0; layer < dlayer.Length; layer++)
				for (int k = 0; k < Patch.Count; k++)
					result[layer] += Patch[k].dlt_n_min_res[layer] * Patch[k].RelativeArea;
			return result;
		}
	}

	/// <summary>
	/// Net NH4 mineralisation from residue decomposition
	/// </summary>
	[Output]
	[Units("kg/ha")]
	[Description("Net convertion of NH4 for residue mineralisation/immobilisation")]
	double[] dlt_res_nh4_min
	{
		get
		{
			double[] result = new double[dlayer.Length];
			for (int layer = 0; layer < dlayer.Length; ++layer)
				for (int k = 0; k < Patch.Count; k++)
					result[layer] += Patch[k].dlt_nh4_decomp[layer] * Patch[k].RelativeArea;
			return result;
		}
	}

	/// <summary>
	/// Net NO3 mineralisation from residue decomposition
	/// </summary>
	[Output]
	[Units("kg/ha")]
	[Description("Net convertion of NO3 for residue mineralisation/immobilisation")]
	double[] dlt_res_no3_min
	{
		get
		{
			double[] result = new double[dlayer.Length];
			for (int layer = 0; layer < dlayer.Length; ++layer)
				for (int k = 0; k < Patch.Count; k++)
					result[layer] += Patch[k].dlt_no3_decomp[layer] * Patch[k].RelativeArea;
			return result;
		}
	}

	/// <summary>
	/// Net fom N mineralised (negative for immobilisation)
	/// </summary>
	[Output]
	[Units("kg/ha")]
	[Description("Net FOM N mineralised, negative for immobilisation")]
	double[] dlt_fom_n_min
	{
		get
		{
			double[] result = new double[dlayer.Length];
			for (int layer = 0; layer < dlayer.Length; ++layer)
				for (int k = 0; k < Patch.Count; k++)
					result[layer] += Patch[k].dlt_n_fom_2_min[layer] * Patch[k].RelativeArea;
			return result;
		}
	}

	/// <summary>
	/// Net N mineralised for humic pool
	/// </summary>
	[Output]
	[Units("kg/ha")]
	[Description("Net humic N mineralised, negative for immobilisation")]
	double[] dlt_hum_n_min
	{
		get
		{
			double[] result = new double[dlayer.Length];
			for (int layer = 0; layer < dlayer.Length; ++layer)
				for (int k = 0; k < Patch.Count; k++)
					result[layer] += Patch[k].dlt_n_hum_2_min[layer] * Patch[k].RelativeArea;
			return result;
		}
	}

	/// <summary>
	/// Net N mineralised from m. biomass pool
	/// </summary>
	[Output]
	[Units("kg/ha")]
	[Description("Net biomass N mineralised")]
	double[] dlt_biom_n_min
	{
		get
		{
			double[] result = new double[dlayer.Length];
			for (int layer = 0; layer < dlayer.Length; ++layer)
				for (int k = 0; k < Patch.Count; k++)
					result[layer] += Patch[k].dlt_n_biom_2_min[layer] * Patch[k].RelativeArea;
			return result;
		}
	}

	/// <summary>
	/// Total net N mineralised (residues plus soil OM)
	/// </summary>
	[Output]
	[Units("kg/ha")]
	[Description("Total net N mineralised (soil plus residues)")]
	double[] dlt_n_min_tot
	{
		get
		{
			double[] result = new double[dlayer.Length];
			for (int layer = 0; layer < dlayer.Length; layer++)
				for (int k = 0; k < Patch.Count; k++)
					result[layer] += Patch[k].dlt_n_min_tot[layer] * Patch[k].RelativeArea;
			return result;
		}
	}

	/// <summary>
	/// Nitrogen coverted by hydrolysis (from urea to NH4)
	/// </summary>
	[Output]
	[Units("kg/ha")]
	[Description("Nitrogen coverted by hydrolysis")]
	double[] dlt_urea_hydrol
	{
		get
		{
			double[] result = new double[dlayer.Length];
			for (int layer = 0; layer < dlayer.Length; layer++)
				for (int k = 0; k < Patch.Count; k++)
					result[layer] += Patch[k].dlt_urea_hydrolised[layer] * Patch[k].RelativeArea;
			return result;
		}
	}

	/// <summary>
	/// Nitrogen coverted by nitrification (from NH4 to either NO3 or N2O) - alias of nitrification
	/// </summary>
	[Output]
	[Units("kg/ha")]
	[Description("Nitrogen coverted by nitrification")]
	double[] dlt_rntrf
	{
		get
		{
			double[] result = new double[dlayer.Length];
			for (int layer = 0; layer < dlayer.Length; ++layer)
				for (int k = 0; k < Patch.Count; k++)
					result[layer] += Patch[k].dlt_nitrification[layer] * Patch[k].RelativeArea;
			return result;
		}
	}

	/// <summary>
	/// Nitrogen coverted by nitrification (from NH4 to either NO3 or N2O)
	/// </summary>
	[Output]
	[Units("kg/ha")]
	[Description("Nitrogen coverted by nitrification")]
	double[] nitrification
	{
		get
		{
			double[] result = new double[dlayer.Length];
			for (int layer = 0; layer < dlayer.Length; ++layer)
				for (int k = 0; k < Patch.Count; k++)
					result[layer] += Patch[k].dlt_nitrification[layer] * Patch[k].RelativeArea;
			return result;
		}
	}

	/// <summary>
	/// Effective, or net, nitrogen coverted by nitrification (from NH4 to NO3)
	/// </summary>
	[Output]
	[Units("kg/ha")]
	[Description("Effective nitrogen coverted by nitrification")]
	double[] effective_nitrification
	{
		get
		{
			double[] result = new double[dlayer.Length];
			for (int layer = 0; layer < dlayer.Length; ++layer)
				for (int k = 0; k < Patch.Count; k++)
					result[layer] += Patch[k].effective_nitrification[layer] * Patch[k].RelativeArea;
			return result;
		}
	}

	/// <summary>
	/// NH4 N denitrified
	/// </summary>
	[Output]
	[Units("kg/ha")]
	[Description("NH4 N denitrified")]
	double[] dlt_nh4_dnit
	{
		get
		{
			double[] result = new double[dlayer.Length];
			for (int layer = 0; layer < dlayer.Length; ++layer)
				for (int k = 0; k < Patch.Count; k++)
					result[layer] += Patch[k].dlt_nh4_dnit[layer] * Patch[k].RelativeArea;
			return result;
		}
	}

	/// <summary>
	/// NO3 N denitrified
	/// </summary>
	[Output]
	[Units("kg/ha")]
	[Description("NO3 N denitrified")]
	double[] dlt_no3_dnit
	{
		get
		{
			double[] result = new double[dlayer.Length];
			for (int layer = 0; layer < dlayer.Length; ++layer)
				for (int k = 0; k < Patch.Count; k++)
					result[layer] += Patch[k].dlt_no3_dnit[layer] * Patch[k].RelativeArea;
			return result;
		}
	}

	/// <summary>
	/// Total N2O amount produced today
	/// </summary>
	[Output]
	[Units("kg/ha")]
	[Description("Amount of N2O produced")]
	double[] n2o_atm
	{
		get
		{
			double[] result = new double[dlayer.Length];
			for (int layer = 0; layer < dlayer.Length; ++layer)
				for (int k = 0; k < Patch.Count; k++)
					result[layer] += Patch[k].n2o_atm[layer] * Patch[k].RelativeArea;
			return result;
		}
	}

	/// <summary>
	/// Amount of N2O produced by nitrification
	/// </summary>
	[Output]
	[Units("kg/ha")]
	[Description("Amount of N2O produced by nitrification")]
	double[] n2o_atm_nitrification
	{
		get
		{
			double[] result = new double[dlayer.Length];
			for (int layer = 0; layer < dlayer.Length; ++layer)
				for (int k = 0; k < Patch.Count; k++)
					result[layer] += Patch[k].dlt_nh4_dnit[layer] * Patch[k].RelativeArea;
			return result;
		}
	}

	/// <summary>
	/// Amount of N2O produced by denitrification
	/// </summary>
	[Output]
	[Units("kg/ha")]
	[Description("Amount of N2O produced by dentrification")]
	double[] n2o_atm_dentrification
	{
		get
		{
			double[] result = new double[dlayer.Length];
			for (int layer = 0; layer < dlayer.Length; ++layer)
				for (int k = 0; k < Patch.Count; k++)
					result[layer] += (Patch[k].n2o_atm[layer] - Patch[k].dlt_nh4_dnit[layer]) * Patch[k].RelativeArea;
			return result;
		}
	}

	/// <summary>
	/// Amount of N2 produced
	/// </summary>
	[Output]
	[Units("kg/ha")]
	[Description("Amount of N2 produced")]
	double[] n2_atm
	{
		get
		{
			double[] result = new double[dlayer.Length];
			for (int layer = 0; layer < dlayer.Length; ++layer)
				for (int k = 0; k < Patch.Count; k++)
					result[layer] += Patch[k].n2_atm[layer] * Patch[k].RelativeArea;
			return result;
		}
	}

	/// <summary>
	/// N converted by denitrification
	/// </summary>
	[Output]
	[Units("kg/ha")]
	[Description("N converted by denitrification")]
	double[] dnit
	{
		get
		{
			double[] result = new double[dlayer.Length];
			for (int layer = 0; layer < dlayer.Length; layer++)
				for (int k = 0; k < Patch.Count; k++)
					result[layer] += Patch[k].dnit[layer] * Patch[k].RelativeArea;
			return result;
		}
	}

	/// <summary>
	/// Excess N required above NH4 supply (for immobilisation)
	/// </summary>
	[Output]
	[Units("kg/ha")]
	[Description("NH4 deficit for immobilisation")]
	double[] nh4_deficit_immob
	{
		get
		{
			double[] result = new double[dlayer.Length];
			for (int layer = 0; layer < dlayer.Length; ++layer)
				for (int k = 0; k < Patch.Count; k++)
					result[layer] += Patch[k].nh4_deficit_immob[layer] * Patch[k].RelativeArea;
			return result;
		}
	}

	#endregion

	#region Amounts in various pools

	/// <summary>
	/// Total nitrogen in FOM
	/// </summary>
	[Output]
	[Units("kg/ha")]
	[Description("Nitrogen in FOM")]
	double[] fom_n
	{
		get
		{
			double[] result = new double[dlayer.Length];
			for (int layer = 0; layer < dlayer.Length; ++layer)
				for (int k = 0; k < Patch.Count; k++)
					result[layer] += Patch[k].fom_n[layer] * Patch[k].RelativeArea;
			return result;
		}
	}

	/// <summary>
	/// Nitrogen in FOM pool 1
	/// </summary>
	[Output]
	[Units("kg/ha")]
	[Description("Nitrogen in FOM pool 1")]
	double[] fom_n_pool1
	{
		get
		{
			double[] result = new double[dlayer.Length];
			for (int layer = 0; layer < dlayer.Length; ++layer)
				for (int k = 0; k < Patch.Count; k++)
					result[layer] += Patch[k].fom_n_pool1[layer] * Patch[k].RelativeArea;
			return result;
		}
	}

	/// <summary>
	/// Nitrogen in FOM pool 2
	/// </summary>
	[Output]
	[Units("kg/ha")]
	[Description("Nitrogen in FOM pool 2")]
	double[] fom_n_pool2
	{
		get
		{
			double[] result = new double[dlayer.Length];
			for (int layer = 0; layer < dlayer.Length; ++layer)
				for (int k = 0; k < Patch.Count; k++)
					result[layer] += Patch[k].fom_n_pool2[layer] * Patch[k].RelativeArea;
			return result;
		}
	}

	/// <summary>
	/// Nitrogen in FOM pool 3
	/// </summary>
	[Output]
	[Units("kg/ha")]
	[Description("Nitrogen in FOM pool 3")]
	double[] fom_n_pool3
	{
		get
		{
			double[] result = new double[dlayer.Length];
			for (int layer = 0; layer < dlayer.Length; ++layer)
				for (int k = 0; k < Patch.Count; k++)
					result[layer] += Patch[k].fom_n_pool3[layer] * Patch[k].RelativeArea;
			return result;
		}
	}

	/// <summary>
	/// Soil humic N
	/// </summary>
	[Output]
	[Units("kg/ha")]
	[Description("Soil humic nitrogen")]
	double[] hum_n
	{
		get
		{
			double[] result = new double[dlayer.Length];
			for (int layer = 0; layer < dlayer.Length; ++layer)
				for (int k = 0; k < Patch.Count; k++)
					result[layer] += Patch[k].hum_n[layer] * Patch[k].RelativeArea;
			return result;
		}
	}

	/// <summary>
	/// Soil biomass nitrogen
	/// </summary>
	[Output]
	[Units("kg/ha")]
	[Description("Soil biomass nitrogen")]
	double[] biom_n
	{
		get
		{
			double[] result = new double[dlayer.Length];
			for (int layer = 0; layer < dlayer.Length; ++layer)
				for (int k = 0; k < Patch.Count; k++)
					result[layer] += Patch[k].biom_n[layer] * Patch[k].RelativeArea;
			return result;
		}
	}

	/// <summary>
	/// Total N in soil
	/// </summary>
	[Output]
	[Units("kg/ha")]
	[Description("Total N in soil")]
	double[] nit_tot
	{
		get
		{
			double[] result = null;
			if (dlayer != null)
			{
				result = new double[dlayer.Length];
				for (int layer = 0; layer < dlayer.Length; ++layer)
					for (int k = 0; k < Patch.Count; k++)
						result[layer] += Patch[k].nit_tot[layer] * Patch[k].RelativeArea;
			}
			return result;
		}
	}

	#endregion

	#region Nitrogen balance

	/// <summary>
	/// Balance of nitrogen: deltaN - losses
	/// </summary>
	[Output]
	[Description("Nitrogen balance")]
	double nitrogenbalance
	{
		get
		{
			double deltaN = SumDoubleArray(nit_tot) - dailyInitialN;  // variation in N today
			double losses = SumDoubleArray(dlt_nh4_dnit) + SumDoubleArray(dlt_no3_dnit);
			return -(losses + deltaN);
			// why leaching losses are not accounted and what about the inputs?
		}
	}

	#endregion

	#endregion

	#region Outputs for Carbon

	#region General values

	/// <summary>
	/// Number of fom types read - is this really needed?
	/// </summary>
	[Output]
	[Description("Number of FOM types")]
	private int num_fom_types
	{ get { return fom_types.Length; } }

	/// <summary>
	/// Carbohydrate fraction of FOM (0-1)
	/// </summary>
	[Output]
	[Description("Fraction of carbohydrate in FOM")]
	private double fr_carb
	{ get { return fract_carb[fom_type]; } }

	/// <summary>
	/// Cellulose fraction of FOM (0-1)
	/// </summary>
	[Output]
	[Description("Fraction of cellulose in FOM")]
	private double fr_cell
	{ get { return fract_cell[fom_type]; } }

	/// <summary>
	/// Lignin fraction of FOM (0-1)
	/// </summary>
	[Output]
	[Description("Fraction of lignin in FOM")]
	private double fr_lign
	{ get { return fract_lign[fom_type]; } }

	#endregion

	#region Changes for today - deltas

	/// <summary>
	/// Carbon loss in sediment, via runoff/erosion
	/// </summary>
	[Output]
	[Units("kg")]
	[Description("Carbon loss in sediment")]
	double dlt_c_loss_in_sed
	{
		get
		{
			double result = 0.0;
			for (int k = 0; k < Patch.Count; k++)
				result += Patch[k].dlt_c_loss_in_sed * Patch[k].RelativeArea;
			return result;
		}
	}

	/// <summary>
	/// Amount of C converted from FOM to humic (kg/ha)
	/// </summary>
	[Output]
	[Units("kg/ha")]
	[Description("FOM C converted to humic")]
	double[] dlt_fom_c_hum
	{
		get
		{
			double[] result = new double[dlayer.Length];
			for (int layer = 0; layer < dlayer.Length; ++layer)
				for (int k = 0; k < Patch.Count; k++)
					result[layer] += Patch[k].dlt_fom_c_hum[layer] * Patch[k].RelativeArea;
			return result;
		}
	}

	/// <summary>
	/// Amount of C converted from FOM to m. biomass (kg/ha)
	/// </summary>
	[Output]
	[Units("kg/ha")]
	[Description("FOM C converted to biomass")]
	double[] dlt_fom_c_biom
	{
		get
		{
			double[] result = new double[dlayer.Length];
			for (int layer = 0; layer < dlayer.Length; ++layer)
				for (int k = 0; k < Patch.Count; k++)
					result[layer] += Patch[k].dlt_fom_c_biom[layer] * Patch[k].RelativeArea;
			return result;
		}
	}

	/// <summary>
	/// Amount of C lost to atmosphere from FOM
	/// </summary>
	[Output]
	[Units("kg/ha")]
	[Description("FOM C lost to atmosphere")]
	double[] dlt_fom_c_atm
	{
		get
		{
			double[] result = new double[dlayer.Length];
			for (int layer = 0; layer < dlayer.Length; ++layer)
				for (int k = 0; k < Patch.Count; k++)
					result[layer] += Patch[k].dlt_fom_c_atm[layer] * Patch[k].RelativeArea;
			return result;
		}
	}

	/// <summary>
	/// Humic C converted to biomass
	/// </summary>
	[Output]
	[Units("kg/ha")]
	[Description("Humic C converted to biomass")]
	double[] dlt_hum_c_biom
	{
		get
		{
			double[] result = new double[dlayer.Length];
			for (int layer = 0; layer < dlayer.Length; ++layer)
				for (int k = 0; k < Patch.Count; k++)
					result[layer] += Patch[k].dlt_c_hum_2_biom[layer] * Patch[k].RelativeArea;
			return result;
		}
	}

	/// <summary>
	/// Humic C lost to atmosphere
	/// </summary>
	[Output]
	[Units("kg/ha")]
	[Description("Humic C lost to atmosphere")]
	double[] dlt_hum_c_atm
	{
		get
		{
			double[] result = new double[dlayer.Length];
			for (int layer = 0; layer < dlayer.Length; ++layer)
				for (int k = 0; k < Patch.Count; k++)
					result[layer] += Patch[k].dlt_c_hum_2_atm[layer] * Patch[k].RelativeArea;
			return result;
		}
	}

	/// <summary>
	/// Biomass C converted to humic
	/// </summary>
	[Output]
	[Units("kg/ha")]
	[Description("Biomass C converted to humic")]
	double[] dlt_biom_c_hum
	{
		get
		{
			double[] result = new double[dlayer.Length];
			for (int layer = 0; layer < dlayer.Length; ++layer)
				for (int k = 0; k < Patch.Count; k++)
					result[layer] += Patch[k].dlt_c_biom_2_hum[layer] * Patch[k].RelativeArea;
			return result;
		}
	}

	/// <summary>
	/// Biomass C lost to atmosphere
	/// </summary>
	[Output]
	[Units("kg/ha")]
	[Description("Biomass C lost to atmosphere")]
	double[] dlt_biom_c_atm
	{
		get
		{
			double[] result = new double[dlayer.Length];
			for (int layer = 0; layer < dlayer.Length; ++layer)
				for (int k = 0; k < Patch.Count; k++)
					result[layer] += Patch[k].dlt_c_biom_2_atm[layer] * Patch[k].RelativeArea;
			return result;
		}
	}

	/// <summary>
	/// Carbon from residues converted to biomass C
	/// </summary>
	[Output]
	[Units("kg/ha")]
	[Description("Carbon from residues converted to biomass")]
	double[] dlt_res_c_biom
	{
		get
		{
			double[] result = new double[dlayer.Length];
			for (int layer = 0; layer < dlayer.Length; ++layer)
				for (int k = 0; k < Patch.Count; k++)
					result[layer] += Patch[k].dlt_res_c_biom[layer] * Patch[k].RelativeArea;
			return result;
		}
	}

	/// <summary>
	/// Carbon from residues converted to humic C
	/// </summary>
	[Output]
	[Units("kg/ha")]
	[Description("Carbon from residues converted to humic")]
	double[] dlt_res_c_hum
	{
		get
		{
			double[] result = new double[dlayer.Length];
			for (int layer = 0; layer < dlayer.Length; ++layer)
				for (int k = 0; k < Patch.Count; k++)
					result[layer] += Patch[k].dlt_res_c_hum[layer] * Patch[k].RelativeArea;
			return result;
		}
	}

	/// <summary>
	/// Carbon from residues lost to atmosphere
	/// </summary>
	[Output]
	[Units("kg/ha")]
	[Description("Carbon from residues lost to atmosphere")]
	double[] dlt_res_c_atm
	{
		get
		{
			double[] result = new double[dlayer.Length];
			for (int layer = 0; layer < dlayer.Length; ++layer)
				for (int k = 0; k < Patch.Count; k++)
					result[layer] += Patch[k].dlt_res_c_atm[layer] * Patch[k].RelativeArea;
			return result;
		}
	}

	/// <summary>
	/// Delta C in pool 1 of FOM
	/// </summary>
	[Output]
	[Units("kg/ha")]
	[Description("Delta FOM C pool in fraction 1")]
	double[] dlt_fom_c_pool1
	{
		get
		{
			double[] result = new double[dlayer.Length];
			for (int layer = 0; layer < dlayer.Length; ++layer)
				for (int k = 0; k < Patch.Count; k++)
					result[layer] += Patch[k].dlt_fom_c_pool1[layer] * Patch[k].RelativeArea;
			return result;
		}
	}

	/// <summary>
	/// Delta C in pool 2 of FOM
	/// </summary>
	[Output]
	[Units("kg/ha")]
	[Description("Delta FOM C pool in fraction 2")]
	double[] dlt_fom_c_pool2
	{
		get
		{
			double[] result = new double[dlayer.Length];
			for (int layer = 0; layer < dlayer.Length; ++layer)
				for (int k = 0; k < Patch.Count; k++)
					result[layer] += Patch[k].dlt_fom_c_pool2[layer] * Patch[k].RelativeArea;
			return result;
		}
	}

	/// <summary>
	/// Delta C in pool 3 of FOM
	/// </summary>
	[Output]
	[Units("kg/ha")]
	[Description("Delta FOM C pool in fraction 3")]
	double[] dlt_fom_c_pool3
	{
		get
		{
			double[] result = new double[dlayer.Length];
			for (int layer = 0; layer < dlayer.Length; ++layer)
				for (int k = 0; k < Patch.Count; k++)
					result[layer] += Patch[k].dlt_fom_c_pool3[layer] * Patch[k].RelativeArea;
			return result;
		}
	}

	/// <summary>
	/// Carbon lost from all residues to atmosphere
	/// </summary>
	[Output]
	[Units("kg/ha")]
	[Description("Carbon from all residues to atmosphere")]
	double[] soilp_dlt_res_c_atm
	{
		get
		{
			double[] result = new double[dlayer.Length];
			for (int layer = 0; layer < dlayer.Length; ++layer)
				for (int k = 0; k < Patch.Count; k++)
					result[layer] += Patch[k].soilp_dlt_res_c_atm[layer] * Patch[k].RelativeArea;
			return result;
		}
	}

	/// <summary>
	/// Carbon from all residues to humic pool
	/// </summary>
	[Output]
	[Units("kg/ha")]
	[Description("Carbon from all residues to humic")]
	double[] soilp_dlt_res_c_hum
	{
		get
		{
			double[] result = new double[dlayer.Length];
			for (int layer = 0; layer < dlayer.Length; ++layer)
				for (int k = 0; k < Patch.Count; k++)
					result[layer] += Patch[k].soilp_dlt_res_c_hum[layer] * Patch[k].RelativeArea;
			return result;
		}
	}

	/// <summary>
	/// Carbon from all residues to m. biomass
	/// </summary>
	[Output]
	[Units("kg/ha")]
	[Description("Carbon from all residues to biomass")]
	double[] soilp_dlt_res_c_biom
	{
		get
		{
			double[] result = new double[dlayer.Length];
			for (int layer = 0; layer < dlayer.Length; ++layer)
				for (int k = 0; k < Patch.Count; k++)
					result[layer] += Patch[k].soilp_dlt_res_c_biom[layer] * Patch[k].RelativeArea;
			return result;
		}
	}

	#endregion

	#region Amounts in various pools

	/// <summary>
	/// Fresh organic C - FOM
	/// </summary>
	[Output]
	[Units("kg/ha")]
	[Description("Soil FOM C")]
	double[] fom_c
	{
		get
		{
			double[] result = new double[dlayer.Length];
			for (int layer = 0; layer < dlayer.Length; ++layer)
				for (int k = 0; k < Patch.Count; k++)
					result[layer] += Patch[k].fom_c[layer] * Patch[k].RelativeArea;
			return result;
		}
	}

	/// <summary>
	/// Amount of C in pool 1 of FOM
	/// </summary>
	[Output]
	[Units("kg/ha")]
	[Description("FOM C in pool 1")]
	double[] fom_c_pool1
	{
		get
		{
			double[] result = new double[dlayer.Length];
			for (int layer = 0; layer < dlayer.Length; ++layer)
				for (int k = 0; k < Patch.Count; k++)
					result[layer] += Patch[k].fom_c_pool1[layer] * Patch[k].RelativeArea;
			return result;
		}
	}

	/// <summary>
	/// Amount of C in pool 2 of FOM
	/// </summary>
	[Output]
	[Units("kg/ha")]
	[Description("FOM C in pool 2")]
	double[] fom_c_pool2
	{
		get
		{
			double[] result = new double[dlayer.Length];
			for (int layer = 0; layer < dlayer.Length; ++layer)
				for (int k = 0; k < Patch.Count; k++)
					result[layer] += Patch[k].fom_c_pool2[layer] * Patch[k].RelativeArea;
			return result;
		}
	}

	/// <summary>
	/// Amount of C in pool 3 of FOM
	/// </summary>
	[Output]
	[Units("kg/ha")]
	[Description("FOM C in pool 3")]
	double[] fom_c_pool3
	{
		get
		{
			double[] result = new double[dlayer.Length];
			for (int layer = 0; layer < dlayer.Length; ++layer)
				for (int k = 0; k < Patch.Count; k++)
					result[layer] += Patch[k].fom_c_pool3[layer] * Patch[k].RelativeArea;
			return result;
		}
	}

	/// <summary>
	/// Amount of C in humic pool
	/// </summary>
	[Output]
	[Units("kg/ha")]
	[Description("Soil humic C")]
	double[] hum_c
	{
		get
		{
			double[] result = new double[dlayer.Length];
			for (int layer = 0; layer < dlayer.Length; ++layer)
				for (int k = 0; k < Patch.Count; k++)
					result[layer] += Patch[k].hum_c[layer] * Patch[k].RelativeArea;
			return result;
		}
	}

	/// <summary>
	/// Amount of C in inert humic pool
	/// </summary>
	[Output]
	[Units("kg/ha")]
	[Description("Soil humic inert C")]
	double[] inert_c
	{
		get
		{
			double[] result = new double[dlayer.Length];
			for (int layer = 0; layer < dlayer.Length; ++layer)
				for (int k = 0; k < Patch.Count; k++)
					result[layer] += Patch[k].inert_c[layer] * Patch[k].RelativeArea;
			return result;
		}
	}
	
	/// <summary>
	/// Amount of C in m. biomass pool
	/// </summary>
	[Output]
	[Units("kg/ha")]
	[Description("Soil biomass C")]
	double[] biom_c
	{
		get
		{
			double[] result = new double[dlayer.Length];
			for (int layer = 0; layer < dlayer.Length; ++layer)
				for (int k = 0; k < Patch.Count; k++)
					result[layer] += Patch[k].biom_c[layer] * Patch[k].RelativeArea;
			return result;
		}
	}

	/// <summary>
	/// Total carbon amount in the soil
	/// </summary>
	[Output]
	[Units("kg/ha")]
	[Description("Total soil carbon")]
	double[] carbon_tot
	{
		get
		{
			double[] result = null;
			if (dlayer != null)
			{
				result = new double[dlayer.Length];
				for (int layer = 0; layer < dlayer.Length; ++layer)
					for (int k = 0; k < Patch.Count; k++)
						result[layer] += Patch[k].carbon_tot[layer] * Patch[k].RelativeArea;
			}
			return result;
		}
	}

	#endregion

	#region Carbon Balance

	/// <summary>
	/// Balance of C in soil: deltaC - losses
	/// </summary>
	[Output]
	[Description("Carbon balance")]
	double carbonbalance
	{
		get
		{
			double deltaC = SumDoubleArray(carbon_tot) - dailyInitialC;     // variation in C today
			double losses = SumDoubleArray(dlt_res_c_atm) + SumDoubleArray(dlt_fom_c_atm) + SumDoubleArray(dlt_hum_c_atm) + SumDoubleArray(dlt_biom_c_atm);
			return -(losses + deltaC);
		}
	}

	#endregion

	#endregion

	#region Factors and other outputs

	/// <summary>
	/// amount of P coverted by residue mineralisation
	/// </summary>
	[Output]
	[Units("kg/ha")]
	[Description("P coverted by residue mineralisation (needed by SoilP)")]
	double[] soilp_dlt_org_p
	{
		get
		{
			double[] result = new double[dlayer.Length];
			for (int layer = 0; layer < dlayer.Length; ++layer)
				for (int k = 0; k < Patch.Count; k++)
					result[layer] += Patch[k].soilp_dlt_org_p[layer] * Patch[k].RelativeArea;
			return result;
		}
	}
	
	/// <summary>
	/// Soil temperature (oC), values actually used in the model
	/// </summary>
	private double[] Tsoil;

	/// <summary>
	/// SoilN's simple soil temperature
	/// </summary>
	[Output]
	[Units("oC")]
	[Description("Soil temperature")]
	double[] st
	{
		get
		{
			double[] Result = new double[0];
			if (!use_external_st)
				Result = Tsoil;
			return Result;
		}
	}

	/// <summary>
	/// Temperature factor for nitrification and mineralisation
	/// </summary>
	[Output]
	[Description("Temperature factor for nitrification and mineralisation")]
	double[] tf
	{
		get
		{
			double[] result = new double[dlayer.Length];
			// RCichota: deactivated
			//int index = (!is_pond_active) ? 1 : 2;
			//for (int layer = 0; layer < dlayer.Length; layer++)
			//    result[layer] = (soiltype == "rothc") ? RothcTF(layer, index) : TF(layer, index);
			return result;
		}
	}

	/// <summary>
	/// Number of internal patches
	/// </summary>
	[Output]
	[Description("Number of internal patches")]
	int numPatches
	{ get { return Patch.Count; } }

	/// <summary>
	/// Relative area of each internal patch
	/// </summary>
	[Output]
	[Description("Relative area of each internal patch")]
	double[] PatchArea
	{
		get
		{
			double[] result = new double[Patch.Count];
			for (int k = 0; k < Patch.Count; k++)
				result[k] = Patch[k].RelativeArea;
			return result;
		}
	}


	#endregion

	#endregion

	#endregion

	#region Useful constants

	/// <summary>
	/// Value to evaluate precision against floating point variables
	/// </summary>
	private double EPSILON = Math.Pow(2, -24);

	#endregion

	#region Internal variables

	#region Components

	/// <summary>
	/// List of all existing patches (internal instances of C and N processes)
	/// </summary>
	List<soilCNPatch> Patch;

	/// <summary>
	/// The internal soil temp module - to be avoided (deprecated)
	/// </summary>
	private simpleSoilTemp simpleST;

	#endregion

	#region Decision auxiliary variables

	/// <summary>
	/// Marker for whether initialisation has been finished or not
	/// </summary>
	private bool initDone = false;

	/// <summary>
	/// Marker for whether a reset is going on
	/// </summary>
	private bool inReset = false;

	/// <summary>
	/// Marker for whether external soil temperature is supplied, otherwise use internal
	/// </summary>
	private bool use_external_st = false;

	/// <summary>
	/// Marker for whether external ph is supplied, otherwise default is used
	/// </summary>
	private bool use_external_ph = false;

	/// <summary>
	/// Marker for whether there is pond water, decomposition of surface OM will be done by that model
	/// </summary>
	private bool is_pond_active = false;

	#endregion

	#region Miscelaneous

	/// <summary>
	/// Total C content at the beginning of the day
	/// </summary>
	private double dailyInitialC;

	/// <summary>
	/// Total N content at the beginning of the day
	/// </summary>
	private double dailyInitialN;

	/// <summary>
	/// Type of fom
	/// </summary>
	private int fom_type;

	/// <summary>
	/// Number of surface residues whose decomposition is being calculated
	/// </summary>
	private int num_residues = 0;

	#endregion

	#region Parameters related to computing approaches

	/// <summary>
	/// Approach to be used when computing urea hydrolysis
	/// </summary>
	UreaHydrolysisApproaches UreaHydrolysisApproach = UreaHydrolysisApproaches.APSIMdefault;

	/// <summary>
	/// Approach to be used when computing nitrification
	/// </summary>
	NitrificationApproaches NitrificationApproach = NitrificationApproaches.APSIMdefault;

	/// <summary>
	/// Approach to be used when computing denitrification
	/// </summary>
	DenitrificationApproaches DenitrificationApproach = DenitrificationApproaches.APSIMdefault;

	#endregion

	#endregion

	#region Types and structures

	/// <summary>
	/// List with patch ids to merge
	/// </summary>
	private struct PatchIDs
	{
	
		/// <summary>
		/// IDs of disappearing patches
		/// </summary>
		public List<int> disappearing;
		/// <summary>
		/// IDs of patches receiving the area and status of disappearing patches
		/// </summary>
		public List<int> recipient;
	}

	/// <summary>
	/// The parameters to compute a exponential type function (used for example for temperature factor)
	/// </summary>
	private struct BendingStickData
	{
		// this is a bending stick type data

		/// <summary>
		/// Optimum temperature, when factor is equal to one
		/// </summary>
		public double[] xValueForOptimum;
		/// <summary>
		/// Value of factor when temperature is equal to zero celsius
		/// </summary>
		public double[] yValueAtZero;
		/// <summary>
		/// Exponent defining the curvature of the factors
		/// </summary>
		public double[] CurveExponent;
	}

	/// <summary>
	/// Lists of x and y values used to describe certain a 'broken stick' function (e.g. moisture factor)
	/// </summary>
	private struct BrokenStickData
	{
		/// <summary>
		/// The values in the x-axis
		/// </summary>
		public double[] xVals;
		/// <summary>
		/// The values in the y-axis
		/// </summary>
		public double[] yVals;
	}

	/// <summary>
	/// List of approaches available for computing urea hydrolysis
	/// </summary>
	private enum UreaHydrolysisApproaches { APSIMdefault, RCichota };

	/// <summary>
	/// List of approaches available for computing nitrification
	/// </summary>
	private enum NitrificationApproaches { APSIMdefault, RCichota };

	/// <summary>
	/// List of approaches available for computing denitrification
	/// </summary>
	private enum DenitrificationApproaches { APSIMdefault, RCichota };

	#endregion

}
