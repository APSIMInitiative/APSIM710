using System;
using System.Collections.Generic;
using System.Text;

public partial class SurfaceOM2 : Instance
{
    /*
            /// <summary>
     /// Get new met data
     /// </summary>
     /// <param name="variant"></param>
     private void surfom_ONnewmet(int variant)
     {
         //APSIM THING
         //unpack_newmet(variant, g.MetData);

     }

            private void surfom_decompose_surfom(int variant)
     {


         //  Local Variables;
         int numvals;                    //number of values from postbox;
         int layer;                      //layer counter;
         int num_surfom;                    //local surfom counter from received event;
         int residue_no;                 //Index into the global array;
         float[] c_pot_decomp = new float[max_residues];	//pot amount of C to decompose (kg/ha)
         float[] n_pot_decomp = new float[max_residues];	//pot amount of N to decompose (kg/ha)
         float[] p_pot_decomp = new float[max_residues];	//pot amount of P to decompose (kg/ha)
         float tot_c_decomp;	//total amount of c to decompose;
         float tot_n_decomp;	//total amount of c to decompose;
         float tot_p_decomp;	//total amount of c to decompose;
         float[] residue_incorp_fraction = new float[max_layer];
         float dlt_residue_fraction;
         float[] dlt_res_c_decomp = new float[max_layer];
         int deepest_layer;
         SurfaceOrganicMatterDecompType SOMDecomp = new SurfaceOrganicMatterDecompType();
         float SOMcnr = 0;
         float SOMc = 0;
         float SOMn = 0;
         string Err_string;

         //APSIM THING
         //unpack_SurfaceOrganicMatterDecomp(variant, SOMDecomp);
         num_surfom = SOMDecomp.Pool.Length;

         //calculate potential decompostion of C, N, and P;
         DecompReturnStruct d = surfom_Pot_Decomp();

         for (int counter = 1; counter < num_surfom; counter++)
         {

             //Determine which residue pool corresponds to this index in the array;
             residue_no = surfom_number(SOMDecomp.Pool[counter].Name);

             //Collect actual decompostion of C and N from supplying module (soiln2)
             tot_c_decomp = SOMDecomp.Pool[counter].FOM.C;
             tot_n_decomp = SOMDecomp.Pool[counter].FOM.N;

             Bound_check_real_var(tot_n_decomp, 0.0f, d.N_decomp[residue_no], "total n decompostion");

             //check if C:N ratio of decomposed material is correct;
             for (int i = 0; i < MaxFr; i++)
             {
                 SOMc = g.SurfOM[residue_no].Standing[i].C + g.SurfOM[residue_no].Lying[i].C;
                 SOMn = g.SurfOM[residue_no].Standing[i].N + g.SurfOM[residue_no].Lying[i].N;
             }

             SOMcnr = divide(SOMc, SOMn, 0.0f);

             if (reals_are_equal(tot_c_decomp, 0.0f) && reals_are_equal(tot_n_decomp, 0.0f))
             {
                 //all OK - nothing happening;
             }
             else if (tot_c_decomp > c_pot_decomp[residue_no] + float.Epsilon)
             {
                 throw new Exception("SurfaceOM2 - C decomposition exceeds potential rate");
             }
             else if (tot_n_decomp > n_pot_decomp[residue_no] + float.Epsilon)
             {
                 throw new Exception("SurfaceOM2 - N decomposition exceeds potential rate");
                 //NIH - If both the following tests are empty { they can both be deleted.
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
     * */
}
