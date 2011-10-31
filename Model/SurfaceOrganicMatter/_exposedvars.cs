using System;
using System.Collections.Generic;
using System.Text;
using ModelFramework;
using System.Linq;
using CSGeneral;

public partial class SurfaceOM : Instance
{
    [Link]
    Paddock mypaddock;

    #region BensParams

    [Param]
    public ResiduesType residue_types;

    [Param]
    public SOMTillageType tillage;

    [Param]
    [Units("")]
    public string PoolName;

    [Param]
    [Units("")]
    public string type;

    [Param]
    [Units("")]
    public string mass;

    [Param]
    [Units("")]
    public string standing_fraction;

    [Param]
    [Units("")]
    public string cpr;

    [Param(IsOptional = true)]
    [Units("")]
    public string cnr;

    #endregion

    #region Params

    /// <summary>
    /// critical residue weight below which Thorburn"s cover factor equals one
    /// </summary>
    [Param]
    [Units("")]
    public float crit_residue_wt;

    /// <summary>
    /// temperature at which decomp reaches optimum (oC)
    /// </summary>
    [Param]
    [Units("")]
    public float opt_temp;

    /// <summary>
    /// cumeos at which decomp rate becomes zero. (mm H2O)
    /// </summary>
    [Param]
    [Units("")]
    public float cum_eos_max;

    /// <summary>
    /// coeff for rate of change in decomp with C:N
    /// </summary>
    [Param]
    [Units("")]
    public float cnrf_coeff;

    /// <summary>
    /// C:N above which decomp is slowed
    /// </summary>
    [Param]
    [Units("")]
    public float cnrf_optcn;

    /// <summary>
    /// 
    /// </summary>
    [Param]
    [Units("")]
    public float c_fract;

    /// <summary>
    /// total amount of "leaching" rain to remove all soluble N from surfom
    /// </summary>
    [Param]
    [Units("")]
    public float leach_rain_tot;

    /// <summary>
    /// threshold rainfall amount for leaching to occur
    /// </summary>
    [Param]
    [Units("")]
    public float min_rain_to_leach;

    /// <summary>
    /// critical minimum org C below which potential decomposition rate is 100% (to avoid numerical imprecision)
    /// </summary>
    [Param]
    [Units("")]
    public float crit_min_surfom_orgC;

    /// <summary>
    /// Default C:P ratio
    /// </summary>
    [Param]
    [Units("")]
    public float default_cpr;

    /// <summary>
    /// Default fraction of residue isolated from soil (standing up)
    /// </summary>
    [Param]
    [Units("")]
    public float default_standing_fraction;

    /// <summary>
    /// extinction coefficient for standing residues
    /// </summary>
    [Param]
    [Units("")]
    public float standing_extinct_coeff;

    /// <summary>
    /// fraction of incoming faeces to add
    /// </summary>
    [Param(IsOptional = true, MinVal = 0.0, MaxVal = 0.0)]
    [Units("0-1")]
    public double fractionFaecesAdded = 0.5;

    public string[] fom_types;
    public int num_fom_types;
    public int[] cf_contrib = new int[max_residues];            //determinant of whether a residue type contributes to the calculation of contact factor (1 or 0)

    public float[] C_fract = new float[max_residues];	        //Fraction of Carbon in plant material (0-1)

    public float[,] fr_pool_C = new float[MaxFr, max_residues];	//carbohydrate fraction in fom C pool (0-1)
    public float[,] fr_pool_N = new float[MaxFr, max_residues];	//carbohydrate fraction in fom N pool (0-1)
    public float[,] fr_pool_P = new float[MaxFr, max_residues];	//carbohydrate fraction in fom P pool (0-1)

    public float[] nh4ppm = new float[max_residues];	        //ammonium component of residue (ppm)
    public float[] no3ppm = new float[max_residues];	        //nitrate component of residue (ppm)
    public float[] po4ppm = new float[max_residues];	        //ammonium component of residue (ppm)
    public float[] specific_area = new float[max_residues];	    //specific area of residue (ha/kg)

    #endregion
    
    #region Inputs

    [Input]
    [Units("mm")]
    float eos = float.NaN;

    [Input]
    [Units("mm")]
    float[] dlayer = null;

    [Input(IsOptional = true)]
    [Units("")]
    string pond_active = null;

    [Input(IsOptional = true)]
    [Units("")]
    float[] labile_p = null;

    [Input(IsOptional = true)]
    [Units("")]
    float irrigation = float.NaN;

    [Input]
    int today { set { Today = DateUtility.JulianDayNumberToDateTime(value); } get { return DateUtility.dateTimeToJulianDayNumber(Today); } }
    DateTime Today;


    /*
    [Input(true)]
    [Units("")]
    float f_incorp { get { return f_incorp_val; } set { f_incorp_val = value; f_incorp_written = true; } }
    float f_incorp_val = float.NaN;

    [Input(true)]
    [Units("")]
    float tillage_depth { get { return tillage_depth_val; } set { tillage_depth_val = value; tillage_depth_written = true; } }
    float tillage_depth_val = float.NaN;
    */


    //[Input()]
    //[Units("")]
    //float Crit_residue_wt;

    #endregion

    #region Outputs

    ///<summary>
    ///Total mass of all surface organic materials
    ///</summary>
    [Output]
    [Units("kg/ha")]
    public float surfaceom_wt { get { return SumSurfOMStandingLying(g.SurfOM, x => x.amount); } }

    public float carbonbalance { get { return 0 - (surfaceom_c - g.DailyInitialC); } }

    public float nitrogenbalance { get { return 0 - (surfaceom_n - g.DailyInitialN); } }

    ///<summary>
    ///Total mass of all surface organic carbon
    ///</summary>
    [Output]
    [Units("kg/ha")]
    public float surfaceom_c { get { return SumSurfOMStandingLying(g.SurfOM, x => x.C); } }

    ///<summary>
    ///Total mass of all surface organic nitrogen
    ///</summary>
    [Output]
    [Units("kg/ha")]
    public float surfaceom_n { get { return SumSurfOMStandingLying(g.SurfOM, x => x.N); } }

    ///<summary>
    ///Total mass of all surface organic phosphor
    ///</summary>
    [Output]
    [Units("kg/ha")]
    public float surfaceom_p { get { return SumSurfOMStandingLying(g.SurfOM, x => x.P); } }

    [Output]
    [Units("")]
    public float surfaceom_ashalk { get { return SumSurfOMStandingLying(g.SurfOM, x => x.P); } }

    ///<summary>
    ///Total mass of nitrate
    ///</summary>
    [Output]
    [Units("kg/ha")]
    public float surfaceom_no3 { get { return SumSurfOM(g.SurfOM, x => x.no3); } }

    ///<summary>
    ///Total mass of ammonium
    ///</summary>
    [Output]
    [Units("kg/ha")]
    public float surfaceom_nh4 { get { return SumSurfOM(g.SurfOM, x => x.nh4); } }


    ///<summary>
    ///Total mass of labile phosphorus
    ///</summary>
    [Output]
    [Units("kg/ha")]
    public float surfaceom_labile_p { get { return SumSurfOM(g.SurfOM, x => x.po4); } }

    ///<summary>
    ///Fraction of ground covered by all surface OMs
    ///</summary>
    [Output]
    [Units("m^2/m^2")]
    public float surfaceom_cover { get { return surfom_cover_total(); } }

    ///<summary>
    ///Temperature factor for decomposition
    ///</summary>
    [Output]
    [Units("0-1")]
    public float tf { get { return surfom_tf(); } }

    ///<summary>
    ///Contact factor for decomposition
    ///</summary>
    [Output]
    [Units("0-1")]
    public float cf { get { return surfom_cf(); } }

    [Output]
    [Units("0-1")]
    public float wf { get { return surfom_wf(); } }

    [Output]
    [Units("")]
    public float leaching_fr { get { return g.leaching_fr; } }

    ///<summary>
    ///Mass of organic matter named wheat
    ///</summary>
    [Output]
    [Units("")]
    public float surfaceom_wt_rice { get { return surfaceom_wt_("rice", x => x.amount); } }

    ///<summary>
    ///Mass of organic matter named algae
    ///</summary>
    [Output]
    [Units("")]
    public float surfaceom_wt_algae { get { return surfaceom_wt_("algae", x => x.amount); } }

    ///<summary>
    ///Mass of organic matter in all pools
    ///</summary>
    [Output]
    [Units("kg/ha")]
    public float[] surfaceom_wt_all
    {
        get
        {
            return g.SurfOM.Select<SurfOrganicMatterType, float>
                (
                x => 
                    SumOMFractionType(x.Standing, y => y.amount) + 
                    SumOMFractionType(x.Lying, y => y.amount)
                ).ToArray<float>();
        }
    }

    ///<summary>
    ///Mass of organic carbon in all pools
    ///</summary>
    [Output]
    [Units("kg/ha")]
    public float[] surfaceom_c_all
    {
        get
        {
            return g.SurfOM.Select<SurfOrganicMatterType, float>
                (
                x =>
                    SumOMFractionType(x.Standing, y => y.C) +
                    SumOMFractionType(x.Lying, y => y.C)
                ).ToArray<float>();
        }
    }

    ///<summary>
    ///Mass of organic nitrogen in all pools
    ///</summary>
    [Output]
    [Units("kg/ha")]
    public float[] surfaceom_n_all
    {
        get
        {
            return g.SurfOM.Select<SurfOrganicMatterType, float>
                (
                x =>
                    SumOMFractionType(x.Standing, y => y.N) +
                    SumOMFractionType(x.Lying, y => y.N)
                ).ToArray<float>();
        }
    }

    ///<summary>
    ///Mass of organic phosphor in all pools
    ///</summary>
    [Output]
    [Units("kg/ha")]
    public float[] surfaceom_p_all
    {
        get
        {
            return g.SurfOM.Select<SurfOrganicMatterType, float>
                (
                x =>
                    SumOMFractionType(x.Standing, y => y.P) +
                    SumOMFractionType(x.Lying, y => y.P)
                ).ToArray<float>();
        }
    }

    [Output]
    [Units("")]
    public float[] surfaceom_ashalk_all
    {
        get
        {
            return g.SurfOM.Select<SurfOrganicMatterType, float>
                (
                x =>
                    SumOMFractionType(x.Standing, y => y.AshAlk) +
                    SumOMFractionType(x.Lying, y => y.AshAlk)
                ).ToArray<float>();
        }
    }

    ///<summary>
    ///Mass of nitrate in all pools
    ///</summary>
    [Output]
    [Units("")]
    public float[] surfaceom_no3_all
    {
        get
        {
            return g.SurfOM.Select<SurfOrganicMatterType, float>(x => x.no3).ToArray<float>();
        }
    }

    ///<summary>
    ///Mass of ammonium in all pools
    ///</summary>
    [Output]
    [Units("")]
    public float[] surfaceom_nh4_all
    {
        get
        {
            return g.SurfOM.Select<SurfOrganicMatterType, float>(x => x.nh4).ToArray<float>();
        }
    }

    ///<summary>
    ///Mass of labile phosphorus in all pools
    ///</summary>
    [Output]
    [Units("")]
    public float[] surfaceom_labile_p_all
    {
        get
        {
            return g.SurfOM.Select<SurfOrganicMatterType, float>(x => x.nh4).ToArray<float>();
        }
    }



    ///<summary>
    ///Potential organic C decomposition in all pools
    ///</summary>
    [Output]
    [Units("")]
    public float[] pot_c_decomp_all
    {
        get
        {
            float[] c, n, p;
            surfom_Pot_Decomp(out c, out n, out p);

            return c;
        }
    }

    ///<summary>
    ///Potential organic N decomposition in all pools
    ///</summary>
    [Output]
    [Units("")]
    public float[] pot_n_decomp_all
    {
        get
        {
            float[] c, n, p;
            surfom_Pot_Decomp(out c, out n, out p);

            return n;
        }
    }

    ///<summary>
    ///Potential organic P decomposition in all pools
    ///</summary>
    [Output]
    [Units("")]
    public float[] pot_p_decomp_all
    {
        get
        {
            float[] c, n, p;
            surfom_Pot_Decomp(out c, out n, out p);

            return p;
        }
    }

    ///<summary>
    ///Fraction of all pools which is inert, ie not in contact with the ground
    ///</summary>
    [Output]
    [Units("")]
    public float[] standing_fr_all
    {
        get
        {
            return g.SurfOM.Select<SurfOrganicMatterType, float>(x => divide(x.Standing[0].amount, x.Standing[0].amount + x.Lying[0].amount, 0)).ToArray<float>();
        }
    }

    ///<summary>
    ///Fraction of ground covered by all pools
    ///</summary>
    [Output]
    [Units("m^2/m^2")]
    public float[] surfaceom_cover_all
    {
        get
        {
            float[] result = new float[g.SurfOM.Count];
            for (int i = 0; i < result.Length; i++)
                result[i] = surfom_cover(i);

            return result;
        }
    }

    ///<summary>
    ///C:N ratio factor for decomposition for all pools
    ///</summary>
    [Output]
    [Units("")]
    public float[] cnrf_all
    {
        get
        {
            float[] result = new float[g.SurfOM.Count];
            for (int i = 0; i < result.Length; i++)
                result[i] = surfom_cnrf(i);

            return result;
        }
    }

    //[Output]
    //[Units("")]
    //public float[] dlt_no3;// { get; private set; }

    //[Output]
    //[Units("")]
    //public float[] dlt_nh4;// { get; private set; }

    //[Output]
    //[Units("")]
    //public float[] dlt_labile_p;// { get; private set; }


    #endregion
}
