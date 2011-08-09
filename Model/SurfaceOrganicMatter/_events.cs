using System;
using System.Collections.Generic;
using System.Text;

public partial class SurfaceOrganicMatter : Instance
{

    [Event]
    public event ExternalMassFlowDelegate ExternalMassFlow;
    private void publish_ExternalMassFlow(ExternalMassFlowType massBalanceChange)
    {
        if (ExternalMassFlow != null)
            ExternalMassFlow.Invoke(massBalanceChange);
    }

    [Event]
    public event SurfaceOrganicMatterDecompDelegate PotentialResidueDecompositionCalculated;

    private void publish_SurfaceOrganicMatterDecomp(SurfaceOrganicMatterDecompType SOMDecomp)
    {
        if (PotentialResidueDecompositionCalculated != null)
            PotentialResidueDecompositionCalculated.Invoke(SOMDecomp);
    }

   /* [Event]
    public event IncorpFomDelegate IncorpFOMPool;
    private void publish_FOMPool(FOMPoolType data)
    {
        IncorpFomType event_data = new IncorpFomType();
        event_data.
    }*/

    [Event]
    public event SurfaceOrganicMatterDelegate SurfaceOrganicMatterState;
    private void publish_SurfaceOrganicMatter(SurfaceOrganicMatterType SOM)
    {
        if (SurfaceOrganicMatterState != null)
            SurfaceOrganicMatterState.Invoke(SOM);
    }

    [Event]
    public event Residue_addedDelegate Residue_added;
    /// <summary>
    /// Notify other modules of residue added to residue pool.
    /// </summary>
    /// <param name="residue_type"></param>
    /// <param name="dm_type"></param>
    /// <param name="dlt_residue_wt"></param>
    /// <param name="dlt_residue_N_wt"></param>
    /// <param name="dlt_residue_P_wt"></param>
    private void residue2_Send_Res_added_Event(string residue_type, string dm_type, float dlt_residue_wt, float dlt_residue_N_wt, float dlt_residue_P_wt)
    {
        if(Residue_added != null)
        {
            Residue_addedType data = new Residue_addedType();
            data.residue_type = residue_type;
            data.dm_type = dm_type;
            data.dlt_residue_wt = dlt_residue_wt;
            data.dlt_dm_n = dlt_residue_N_wt;
            data.dlt_dm_p = dlt_residue_P_wt;

            Residue_added.Invoke(data);
        }		
    }

    [Event]
    public event Residue_removedDelegate Residue_removed;
    /// <summary>
    /// Notify other modules of residue removed from residue pool
    /// </summary>
    /// <param name="residue_removed_action"></param>
    /// <param name="dlt_residue_fraction"></param>
    /// <param name="residue_incorp_fraction"></param>
    /// <param name="deepest_layer"></param>
    private void residue2_Send_Res_removed_Event(string residue_removed_action, float dlt_residue_fraction, float[] residue_incorp_fraction, int deepest_layer)
    {
        if (Residue_removed != null)
        {
            Residue_removedType data = new Residue_removedType();

            data.residue_removed_action = residue_removed_action;
            data.dlt_residue_fraction = dlt_residue_fraction;
            data.residue_incorp_fraction = residue_incorp_fraction;

            Residue_removed.Invoke(data);
        }
    }

    [Event]
    public event SurfaceOM_removedDelegate SurfaceOM_removed;
    /// <summary>
    /// Notify other modules of residue added to residue pool
    /// </summary>
    /// <param name="residue_type"></param>
    /// <param name="dm_type"></param>
    /// <param name="dlt_residue_wt"></param>
    /// <param name="dlt_residue_N_wt"></param>
    /// <param name="dlt_residue_P_wt"></param>
    private void surfom_Send_SOM_removed_Event(string residue_type, string dm_type, float dlt_residue_wt, float dlt_residue_N_wt, float dlt_residue_P_wt)
    {
        if (SurfaceOM_removed != null)
        {
            SurfaceOM_removedType data = new SurfaceOM_removedType();

            data.SurfaceOM_type = residue_type;
            data.SurfaceOM_dm_type = dm_type;
            data.dlt_SurfaceOM_wt = dlt_residue_wt;
            data.SurfaceOM_dlt_dm_n = dlt_residue_N_wt;
            data.SurfaceOM_dlt_dm_p = dlt_residue_P_wt;

            SurfaceOM_removed.Invoke(data);
        }
    }

   

}

