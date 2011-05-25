using System;
using System.Collections.Generic;
using System.Text;

public partial class SurfaceOrganicMatter : Instance
{

    [EventHandler]
    public void OnTick() { }
    
    [EventHandler]
    public void OnTillage() { }
    
    [EventHandler]
    public void OnTillage_single() { }
    
    [EventHandler]
    public void OnAdd_surfaceom() { surfom_add_surfom(); }
    
    [EventHandler]
    public void OnInit1() { surfom_Reset(); }
    
    [EventHandler]
    public void OnReset() { surfom_Reset(); }
    
    [EventHandler]
    public void OnCreate() { }
    
    [EventHandler]
    public void OnSum_report() { surfom_Sum_Report(); }

    /// <summary>
    /// Calculates surfom removal as a result of remove_surfom message
    /// </summary>
    /// <param name="variant"></param>
    [EventHandler]
    public void OnRemove_surfaceOM(SurfaceOrganicMatterType SOM)
    {
        string Err_string = "";
        int SOMNo;                  //specific system number for this residue name;
        int som_index;              //index into SOM;
        int pool;

        //- Implementation Section ----------------------------------

        //APSIM THING
        //SOM = unpack_SurfaceOrganicMatter(variant);

        for (som_index = 0; som_index < g.num_surfom; som_index++)
        {

            //Determine which residue pool corresponds to this index in the array;
            SOMNo = surfom_number(SOM.Pool[som_index].Name);

            if (SOMNo < 0)
            {
                //This is an unknown type - error;
               Console.WriteLine( "Attempting to remove Surface Organic Matter from unknown " + SOM.Pool[som_index].Name + " Surface Organic Matter name." + Environment.NewLine);
            }
            else
            {
                //This type already exists;

                //        g.SurfOM[SOMNo]; = g.SurfOM[SOMNo]; - g.SOM[SOMNo];

                //          Check if too much removed ?
                for (pool = 0; pool < MaxFr; pool++)
                {
                    if (g.SurfOM[SOMNo].Lying[pool].amount >= SOM.Pool[SOMNo].LyingFraction[pool].amount)
                    {
                        g.SurfOM[SOMNo].Lying[pool].amount = g.SurfOM[SOMNo].Lying[pool].amount - SOM.Pool[SOMNo].LyingFraction[pool].amount;
                    }
                    else
                    {
                        throw new Exception(
                            "Attempting to remove more dm from " + SOM.Pool[som_index].Name + " lying Surface Organic Matter pool " + pool + " than available" + Environment.NewLine
                            + "Removing " + SOM.Pool[SOMNo].LyingFraction[pool].amount + " (kg/ha) " + "from " + g.SurfOM[SOMNo].Lying[pool].amount + " (kg/ha) available."
                        );
                    }

                    g.SurfOM[SOMNo].Lying[pool].C = g.SurfOM[SOMNo].Lying[pool].C - SOM.Pool[SOMNo].LyingFraction[pool].C;
                    g.SurfOM[SOMNo].Lying[pool].N = g.SurfOM[SOMNo].Lying[pool].N - SOM.Pool[SOMNo].LyingFraction[pool].N;
                    g.SurfOM[SOMNo].Lying[pool].P = g.SurfOM[SOMNo].Lying[pool].P - SOM.Pool[SOMNo].LyingFraction[pool].P;
                    g.SurfOM[SOMNo].Lying[pool].AshAlk = g.SurfOM[SOMNo].Lying[pool].AshAlk - SOM.Pool[SOMNo].LyingFraction[pool].AshAlk;

                    if (g.SurfOM[SOMNo].Standing[pool].amount >= SOM.Pool[SOMNo].StandingFraction[pool].amount)
                    {
                        g.SurfOM[SOMNo].Standing[pool].amount = g.SurfOM[SOMNo].Standing[pool].amount - SOM.Pool[SOMNo].StandingFraction[pool].amount;
                    }
                    else
                    {
                        Console.WriteLine(
                            "Attempting to remove more dm from " + SOM.Pool[som_index].Name + " standing Surface Organic Matter pool " + pool + " than available" + Environment.NewLine
                            + "Removing " + SOM.Pool[SOMNo].LyingFraction[pool].amount + " (kg/ha) " + "from " + g.SurfOM[SOMNo].Lying[pool].amount + " (kg/ha) available."
                       );
                    }

                    g.SurfOM[SOMNo].Standing[pool].C = g.SurfOM[SOMNo].Standing[pool].C - SOM.Pool[SOMNo].StandingFraction[pool].C;
                    g.SurfOM[SOMNo].Standing[pool].N = g.SurfOM[SOMNo].Standing[pool].N - SOM.Pool[SOMNo].StandingFraction[pool].N;
                    g.SurfOM[SOMNo].Standing[pool].P = g.SurfOM[SOMNo].Standing[pool].P - SOM.Pool[SOMNo].StandingFraction[pool].P;
                    g.SurfOM[SOMNo].Standing[pool].AshAlk = g.SurfOM[SOMNo].Standing[pool].AshAlk - SOM.Pool[SOMNo].StandingFraction[pool].AshAlk;
                }

                g.SurfOM[SOMNo].no3 = g.SurfOM[SOMNo].no3 - SOM.Pool[SOMNo].no3;
                g.SurfOM[SOMNo].nh4 = g.SurfOM[SOMNo].nh4 - SOM.Pool[SOMNo].nh4;
                g.SurfOM[SOMNo].po4 = g.SurfOM[SOMNo].po4 - SOM.Pool[SOMNo].po4;
            }

            //Report Removals;
            if (p.report_removals == "yes")
            {
                float samount = 0, sN = 0, sP = 0, lamount = 0, lN = 0, lP = 0;
                for (int i = 0; i < MaxFr; i++)
                {
                    lamount += SOM.Pool[SOMNo].LyingFraction[i].amount;
                    lN += SOM.Pool[SOMNo].LyingFraction[i].N;
                    lP += SOM.Pool[SOMNo].LyingFraction[i].P;

                    samount += SOM.Pool[SOMNo].StandingFraction[i].amount;
                    sN += SOM.Pool[SOMNo].StandingFraction[i].N;
                    sP += SOM.Pool[SOMNo].StandingFraction[i].P;
                }
                Console.WriteLine(
                    "Removed SurfaceOM" + Environment.NewLine
                    + "    SurfaceOM name         = " + SOM.Pool[SOMNo].Name + Environment.NewLine
                    + "    SurfaceOM Type         = " + SOM.Pool[SOMNo].OrganicMatterType + Environment.NewLine
                    + "    Amount Removed (kg/ha): " + Environment.NewLine
                    + "           Lying: " + Environment.NewLine
                    + "                 Amount = " + lamount + Environment.NewLine
                    + "                 N      = " + lN + Environment.NewLine
                    + "                 P      = " + lP + Environment.NewLine
                    + "           Standing: " + Environment.NewLine
                    + "                 Amount = " + samount + Environment.NewLine
                    + "                 N      = " + sN + Environment.NewLine
                    + "                 P      = " + sP + Environment.NewLine
                    );
            }
            else
            {
                //The user has asked for no reports for removals of surfom;
                //in the summary file.
            }

            //APSIM THING
            /*
         surfom_Send_SOM_removed_Event (SOM.Pool[SOMNo].OrganicMatterType   
                                               , SOM.Pool[SOMNo].OrganicMatterType   
                                               , sum(SOM.Pool[SOMNo].LyingFraction[1:MaxFr].amount) + sum(SOM.Pool[SOMNo].StandingFraction[1:MaxFr].amount)   
                                               , sum(SOM.Pool[SOMNo].LyingFraction[1:MaxFr].N) + sum(SOM.Pool[SOMNo].StandingFraction[1:MaxFr].N)   
                                               , sum(SOM.Pool[SOMNo].LyingFraction[1:MaxFr].P) + sum(SOM.Pool[SOMNo].StandingFraction[1:MaxFr].P));
             * */
        }

    }

    [EventHandler]
    public void OnNewmet(NewMetType newmetdata)
    {
        g.MetData = newmetdata;
    }
   
    [EventHandler]
    public void OnIrrigated() { surfom_ONirrigated(); }
    
    [EventHandler]
    public void OnCrop_chopped() { Console.WriteLine("Crop chopped"); surfom_ON_Crop_chopped(); }

    /// <summary>
    /// Get information on surfom added from the crops
    /// </summary>
    /// <param name="variant"></param>
    [EventHandler]
    public void OnBiomassRemoved(BiomassRemovedType BiomassRemoved)
    {

        float surfom_added;	//amount of residue added (kg/ha)
        float surfom_N_added;	//amount of residue N added (kg/ha)
        float surfom_P_added;	//amount of residue N added (kg/ha)

        float sum_temp = 0;
        for (int i = 0; i < BiomassRemoved.fraction_to_residue.Length; i++)
            sum_temp += BiomassRemoved.fraction_to_residue[i];

        if (sum_temp == 0)
        {
            //no surfom in this stuff;
        }
        else
        {
            //Find the amount of surfom to be added today;
            surfom_added = 0;
            for (int i = 0; i < BiomassRemoved.fraction_to_residue.Length; i++)
                surfom_added += BiomassRemoved.dlt_crop_dm[i] * BiomassRemoved.fraction_to_residue[i];

            if (surfom_added > 0.0)
            {
                //Find the amount of N & added in surfom today;

                surfom_N_added = 0;
                surfom_P_added = 0;

                for (int i = 0; i < BiomassRemoved.dlt_dm_p.Length; i++)
                {
                    surfom_N_added += BiomassRemoved.dlt_dm_p[i] * BiomassRemoved.fraction_to_residue[i];
                    surfom_N_added += BiomassRemoved.dlt_dm_n[i] * BiomassRemoved.fraction_to_residue[i];
                }

                AddSurfaceOM(surfom_added, surfom_N_added, surfom_P_added, BiomassRemoved.crop_type);

            }
            else
            {
                //nothing to add;
            }

        }

    }
    
    [EventHandler]
    public void OnProcess()
    {
        surfom_get_other_variables();

        surfom_Process();
        //catch (Exception e) { }
    }
   
    [EventHandler]
    public void OnPost() { }

    [EventHandler]
    public void OnActualresiduedecompositioncalculated(SurfaceOrganicMatterDecompType SOMDecomp)
    {

            //  Local Variables;
            //int numvals;                    //number of values from postbox;
            //int layer;                      //layer counter;
            int num_surfom;                    //local surfom counter from received event;
            int residue_no;                 //Index into the global array;
            float[] c_pot_decomp = new float[max_residues];	//pot amount of C to decompose (kg/ha)
            float[] n_pot_decomp = new float[max_residues];	//pot amount of N to decompose (kg/ha)
            float[] p_pot_decomp = new float[max_residues];	//pot amount of P to decompose (kg/ha)
            float tot_c_decomp;	//total amount of c to decompose;
            float tot_n_decomp;	//total amount of c to decompose;
            float tot_p_decomp;	//total amount of c to decompose;
            float[] residue_incorp_fraction = new float[max_layer];
            //float dlt_residue_fraction;
            float[] dlt_res_c_decomp = new float[max_layer];
            //  int deepest_layer;

            float SOMcnr = 0;
            float SOMc = 0;
            float SOMn = 0;
            //string Err_string;

            //APSIM THING - Handled in method signature
            //unpack_SurfaceOrganicMatterDecomp(variant, SOMDecomp);

            num_surfom = SOMDecomp.Pool.Length;


            //calculate potential decompostion of C, N, and P;
            surfom_Pot_Decomp(out c_pot_decomp, out n_pot_decomp, out p_pot_decomp);

            for (int counter = 0; counter < num_surfom; counter++)
            {

                //Determine which residue pool corresponds to this index in the array;
                residue_no = surfom_number(SOMDecomp.Pool[counter].Name);

                //Collect actual decompostion of C and N from supplying module (soiln2)
                tot_c_decomp = SOMDecomp.Pool[counter].FOM.C;
                tot_n_decomp = SOMDecomp.Pool[counter].FOM.N;

                Bound_check_real_var(tot_n_decomp, 0.0f, n_pot_decomp[residue_no], "total n decompostion");

                //check if C:N ratio of decomposed material is correct;
                for (int i = 0; i < g.SurfOM[residue_no].Standing.Length; i++)
                {
                    SOMc += g.SurfOM[residue_no].Standing[i].C + g.SurfOM[residue_no].Lying[i].C;
                    SOMn += g.SurfOM[residue_no].Standing[i].N + g.SurfOM[residue_no].Lying[i].N;
                }

                SOMcnr = divide(SOMc, SOMn, 0.0f);

                if (reals_are_equal(tot_c_decomp, 0.0f) && reals_are_equal(tot_n_decomp, 0.0f))
                {
                    //all OK - nothing happening;
                }
                else if (tot_c_decomp > c_pot_decomp[residue_no] + acceptableErr)
                {
                    throw new Exception("SurfaceOM - C decomposition exceeds potential rate");
                }
                else if (tot_n_decomp > n_pot_decomp[residue_no] + acceptableErr)
                {
                    throw new Exception("SurfaceOM - N decomposition exceeds potential rate");
                    //NIH - If both the following tests are empty then they can both be deleted.
                }
                else if (reals_are_equal(divide(tot_c_decomp, tot_n_decomp, 0.0f), SOMcnr))
                {
                    //all ok - decomposition and residue pool have same C:N;
                }
                else
                {
                    //              call fatal_error (Err_internal,
                    //    :                 "C:N ratio of decomposed residues is not valid")
                }

                //calculate total p decomposition;
                tot_p_decomp = tot_c_decomp * divide(p_pot_decomp[residue_no], c_pot_decomp[residue_no], 0.0f);

                //Do actual decomposing - update pools for C, N, and P;
                surfom_Decomp(tot_c_decomp, tot_n_decomp, tot_p_decomp, residue_no);
            }

    }

    [EventHandler]
    public void OnProp_up()
    {
        surfom_prop_up();
    }

   
}

