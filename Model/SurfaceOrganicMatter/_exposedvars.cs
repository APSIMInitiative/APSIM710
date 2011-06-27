using System;
using System.Collections.Generic;
using System.Text;

public partial class SurfaceOrganicMatter : Instance
{
    #region BensParams

    [Link]
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
    public float mass;

    [Param]
    [Units("")]
    public float standing_fraction;

    [Param]
    [Units("")]
    public float cpr;

    [Param(true)]
    [Units("")]
    public float cnr;

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

    [Input(true)]
    [Units("")]
    string pond_active = null;

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
    public float surfaceom_wt
    {
        get
        {
            float temp = 0;

            //if (g.num_surfom > 0)
                //Console.WriteLine("Success");

            for (int i = 0; i < g.num_surfom; i++)
                for (int j = 0; j < g.SurfOM[i].Standing.Length; j++)
                    temp += g.SurfOM[i].Standing[j].amount + g.SurfOM[i].Lying[j].amount;
            return temp;
        }
    }

    ///<summary>
    ///Total mass of all surface organic carbon
    ///</summary>
    [Output]
    [Units("kg/ha")]
    public float surfaceom_c
    {
        get
        {
            float temp = 0;
            for (int i = 0; i < g.num_surfom; i++)
                for (int j = 0; j < g.SurfOM[i].Standing.Length; j++)
                    temp += g.SurfOM[i].Standing[j].C + g.SurfOM[i].Lying[j].C;
            return temp;
        }
    }

    ///<summary>
    ///Total mass of all surface organic nitrogen
    ///</summary>
    [Output]
    [Units("kg/ha")]
    public float surfaceom_n
    {
        get
        {
            float temp = 0;
            for (int i = 0; i < g.num_surfom; i++)
                for (int j = 0; j < g.SurfOM[i].Standing.Length; j++)
                    temp += g.SurfOM[i].Standing[j].N + g.SurfOM[i].Lying[j].N;
            return temp;
        }
    }

    ///<summary>
    ///Total mass of all surface organic phosphor
    ///</summary>
    [Output]
    [Units("kg/ha")]
    public float surfaceom_p
    {
        get
        {
            float temp = 0;
            for (int i = 0; i < g.num_surfom; i++)
                for (int j = 0; j < g.SurfOM[i].Standing.Length; j++)
                    temp += g.SurfOM[i].Standing[j].P + g.SurfOM[i].Lying[j].P;
            return temp;
        }
    }

    [Output]
    [Units("")]
    public float surfaceom_ashalk
    {
        get
        {
            float temp = 0;
            for (int i = 0; i < g.num_surfom; i++)
                for (int j = 0; j < g.SurfOM[i].Standing.Length; j++)
                    temp += g.SurfOM[i].Standing[j].AshAlk + g.SurfOM[i].Lying[j].AshAlk;
            return temp;
        }
    }

    ///<summary>
    ///Total mass of nitrate
    ///</summary>
    [Output]
    [Units("kg/ha")]
    public float surfaceom_no3
    {
        get
        {
            float temp = 0;
            for (int i = 0; i < g.num_surfom; i++)
                temp += g.SurfOM[i].no3;
            return temp;
        }
    }

    ///<summary>
    ///Total mass of ammonium
    ///</summary>
    [Output]
    [Units("kg/ha")]
    public float surfaceom_nh4
    {
        get
        {
            float temp = 0;
            for (int i = 0; i < g.num_surfom; i++)
                temp += g.SurfOM[i].nh4;
            return temp;
        }
    }

    ///<summary>
    ///Total mass of labile phosphorus
    ///</summary>
    [Output]
    [Units("kg/ha")]
    public float surfaceom_labile_p
    {
        get
        {
            float temp = 0;
            for (int i = 0; i < g.num_surfom; i++)
                temp += g.SurfOM[i].po4;
            return temp;
        }
    }

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
    [Units("")]
    public float tf { get { return surfom_tf(); } }

    ///<summary>
    ///Contact factor for decomposition
    ///</summary>
    [Output]
    [Units("")]
    public float cf { get { return surfom_cf(); } }

    [Output]
    [Units("")]
    public float wf { get { return surfom_wf(); } }

    [Output]
    [Units("")]
    public float leaching_fr { get { return g.leaching_fr; } }

    ///<summary>
    ///Mass of organic matter named wheat
    ///</summary>
    [Output]
    [Units("")]
    public float surfaceom_wt_rice
    {
        get
        {
            float temp = 0;
            int SOMNo = surfom_number("rice");
            if (SOMNo > 0)
                for (int i = 0; i < g.SurfOM[SOMNo].Standing.Length;i++ )
                    temp += g.SurfOM[SOMNo].Standing[i].amount + g.SurfOM[SOMNo].Lying[i].amount;
            else
                throw new Exception("No organic matter called 'rice' present");

            return temp;
        }
    }

    ///<summary>
    ///Mass of organic matter named algae
    ///</summary>
    [Output]
    [Units("")]
    public float surfaceom_wt_algae     
    {
        get
        {
            float temp = 0;
            int SOMNo = surfom_number("algae");
            if (SOMNo > 0)
                for (int i = 0; i < g.SurfOM[SOMNo].Standing.Length;i++ )
                    temp += g.SurfOM[SOMNo].Standing[i].amount + g.SurfOM[SOMNo].Lying[i].amount;
            else
                throw new Exception("No organic matter called 'rice' present");

            return temp;
        }
    }

    ///<summary>
    ///Mass of organic matter in all pools
    ///</summary>
    [Output]
    [Units("")]
    public float[] surfaceom_wt_all
    {
        get
        {
            float[] temp = new float[g.num_surfom];
            for (int i = 0; i < g.num_surfom; i++)
            {
                temp[i] = 0;
                for (int j = 0; j < g.SurfOM[i].Standing.Length; j++)
                    temp[i] += g.SurfOM[i].Standing[j].amount + g.SurfOM[i].Lying[j].amount;
            }

            return temp;
        }
    }

    ///<summary>
    ///Mass of organic carbon in all pools
    ///</summary>
    [Output]
    [Units("")]
    public float[] surfaceom_c_all
    {
        get
        {
            float[] temp = new float[g.num_surfom];
            for (int i = 0; i < g.num_surfom; i++)
            {
                temp[i] = 0;
                for (int j = 0; j < g.SurfOM[i].Standing.Length; j++)
                    temp[i] += g.SurfOM[i].Standing[j].C + g.SurfOM[i].Lying[j].C;
            }

            return temp;
        }
    }

    ///<summary>
    ///Mass of organic nitrogen in all pools
    ///</summary>
    [Output]
    [Units("")]
    public float[] surfaceom_n_all     
    {
        get
        {
            float[] temp = new float[g.num_surfom];
            for (int i = 0; i < g.num_surfom; i++)
            {
                temp[i] = 0;
                for (int j = 0; j < g.SurfOM[i].Standing.Length; j++)
                    temp[i] += g.SurfOM[i].Standing[j].N + g.SurfOM[i].Lying[j].N;
            }

            return temp;
        }
    }

    ///<summary>
    ///Mass of organic phosphor in all pools
    ///</summary>
    [Output]
    [Units("")]
    public float[] surfaceom_p_all
    {
        get
        {
            float[] temp = new float[g.num_surfom];
            for (int i = 0; i < g.num_surfom; i++)
            {
                temp[i] = 0;
                for (int j = 0; j < g.SurfOM[i].Standing.Length; j++)
                    temp[i] += g.SurfOM[i].Standing[j].P + g.SurfOM[i].Lying[j].P;
            }

            return temp;
        }
    }

    [Output]
    [Units("")]
    public float[] surfaceom_ashalk_all
    {
        get
        {
            float[] temp = new float[g.num_surfom];
            for (int i = 0; i < g.num_surfom; i++)
            {
                temp[i] = 0;
                for (int j = 0; j < g.SurfOM[i].Standing.Length; j++)
                    temp[i] += g.SurfOM[i].Standing[j].AshAlk + g.SurfOM[i].Lying[j].AshAlk;
            }

            return temp;
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
            float[] temp = new float[g.num_surfom];
            for (int i = 0; i < g.num_surfom; i++)
                    temp[i] = g.SurfOM[i].no3;

            return temp;
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
            float[] temp = new float[g.num_surfom];
            for (int i = 0; i < g.num_surfom; i++)
                temp[i] = g.SurfOM[i].nh4;

            return temp;
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
            float[] temp = new float[g.num_surfom];
            for (int i = 0; i < g.num_surfom; i++)
                temp[i] = g.SurfOM[i].po4;

            return temp;
        }
    }

    ///<summary>
    ///Mass of organic carbon in all pools in fpool1
    ///</summary>
    [Output]
    [Units("")]
    public float[] surfaceom_c1_all
    {
        get
        {
            float[] temp = new float[g.num_surfom];
            for (int i = 0; i < g.num_surfom; i++)
                temp[i] = g.SurfOM[i].Standing[1].C + g.SurfOM[i].Lying[1].C;

            return temp;
        }
    }

    ///<summary>
    ///Mass of organic carbon in all pools in fpool2
    ///</summary>
    [Output]
    [Units("")]
    public float[] surfaceom_c2_all 
    {
        get
        {
            float[] temp = new float[g.num_surfom];
            for (int i = 0; i < g.num_surfom; i++)
                temp[i] = g.SurfOM[i].Standing[2].C + g.SurfOM[i].Lying[2].C;

            return temp;
        }
    }

    ///<summary>
    ///Mass of organic carbon in all pools in fpool3
    ///</summary>
    [Output]
    [Units("")]
    public float[] surfaceom_c3_all
    {
        get
        {
            float[] temp = new float[g.num_surfom];
            for (int i = 0; i < g.num_surfom; i++)
                temp[i] = g.SurfOM[i].Standing[3].C + g.SurfOM[i].Lying[3].C;

            return temp;
        }
    }

    ///<summary>
    ///Mass of organic nitrogen in all pools in fpool1
    ///</summary>
    [Output]
    [Units("")]
    public float[] surfaceom_n1_all
    {
        get
        {
            float[] temp = new float[g.num_surfom];
            for (int i = 0; i < g.num_surfom; i++)
                temp[i] = g.SurfOM[i].Standing[1].N + g.SurfOM[i].Lying[1].N;

            return temp;
        }
    }
    
    ///<summary>
    ///Mass of organic nitrogen in all pools in fpool2
    ///</summary>
    [Output]
    [Units("")]
    public float[] surfaceom_n2_all
    {
        get
        {
            float[] temp = new float[g.num_surfom];
            for (int i = 0; i < g.num_surfom; i++)
                temp[i] = g.SurfOM[i].Standing[2].N + g.SurfOM[i].Lying[2].N;

            return temp;
        }
    }

    ///<summary>
    ///Mass of organic nitrogen in all pools in fpool3
    ///</summary>
    [Output]
    [Units("")]
    public float[] surfaceom_n3_all
    {
        get
        {
            float[] temp = new float[g.num_surfom];
            for (int i = 0; i < g.num_surfom; i++)
                temp[i] = g.SurfOM[i].Standing[3].N + g.SurfOM[i].Lying[3].N;

            return temp;
        }
    }

    ///<summary>
    ///Mass of organic phosphorous in all pools in fpool1
    ///</summary>
    [Output]
    [Units("")]
    public float[] surfaceom_p1_all
    {
        get
        {
            float[] temp = new float[g.num_surfom];
            for (int i = 0; i < g.num_surfom; i++)
                temp[i] = g.SurfOM[i].Standing[1].P + g.SurfOM[i].Lying[1].P;

            return temp;
        }
    }

    ///<summary>
    ///Mass of organic phosphorous in all pools in fpool2
    ///</summary>
    [Output]
    [Units("")]
    public float[] surfaceom_p2_all
    {
        get
        {
            float[] temp = new float[g.num_surfom];
            for (int i = 0; i < g.num_surfom; i++)
                temp[i] = g.SurfOM[i].Standing[2].P + g.SurfOM[i].Lying[2].P;

            return temp;
        }
    }

    ///<summary>
    ///Mass of organic phosphorous in all pools in fpool3
    ///</summary>
    [Output]
    [Units("")]
    public float[] surfaceom_p3_all
    {
        get
        {
            float[] temp = new float[g.num_surfom];
            for (int i = 0; i < g.num_surfom; i++)
                temp[i] = g.SurfOM[i].Standing[3].P + g.SurfOM[i].Lying[3].P;

            return temp;
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
            float[] result = new float[g.num_surfom];
            for (int SOMNo = 0; SOMNo < g.num_surfom;SOMNo++ )
                result[SOMNo] = divide(g.SurfOM[SOMNo].Standing[1].amount, (g.SurfOM[SOMNo].Standing[1].amount + g.SurfOM[SOMNo].Lying[1].amount), 0);

            return result;
        }
    }

    ///<summary>
    ///Fraction of ground covered by all pools
    ///</summary>
    [Output]
    [Units("")]
    public float[] surfaceom_cover_all
    {
        get
        {
            float[] result = new float[g.num_surfom];
            for (int SOMNo = 0; SOMNo < g.num_surfom; SOMNo++)
                result[SOMNo] = surfom_cover(SOMNo);

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
            float[] result = new float[g.num_surfom];
            for (int SOMNo = 0; SOMNo < g.num_surfom; SOMNo++)
                result[SOMNo] = surfom_cnrf(SOMNo);

            return result;
        }
    }

    [Output]
    [Units("")]
    public float[] dlt_no3;// { get; private set; }

    [Output]
    [Units("")]
    public float[] dlt_nh4;// { get; private set; }

    [Output]
    [Units("")]
    public float[] dlt_labile_p;// { get; private set; }


    #endregion
}
