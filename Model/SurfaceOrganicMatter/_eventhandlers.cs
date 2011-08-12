using System;
using System.Collections.Generic;
using System.Text;

public partial class SurfaceOrganicMatter : Instance
{

    [EventHandler]
    public void OnTick()
    {
        g.DailyInitialC = 0;
        g.DailyInitialN = 0;
        for (int i = 0; i < g.num_surfom; i++)
            for (int j = 0; j < g.SurfOM[i].Standing.Length; j++)
            {
                g.DailyInitialC += g.SurfOM[i].Standing[j].C + g.SurfOM[i].Lying[j].C;
                g.DailyInitialN += g.SurfOM[i].Standing[j].N + g.SurfOM[i].Lying[j].N;
            }
    }
    
    [EventHandler]
    public void OnTillage(TillageType data) 
    {
        surfom_tillage(data);
    }

    [EventHandler]
    public void OnTillage_single(Tillage_singleType data) 
    {
        surfom_tillage_single(data);
    }

    [EventHandler]
    public void OnAdd_surfaceom(Add_SurfaceOMType data) { surfom_add_surfom(data); }
    
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
        surfom_remove_surfom(SOM);
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
        surfom_decompose_surfom(SOMDecomp);
    }


    [EventHandler]
    public void OnProp_up()
    {
        surfom_prop_up();
    }

   
}

