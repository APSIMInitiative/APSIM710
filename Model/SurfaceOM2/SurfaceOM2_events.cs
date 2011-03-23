using System;
using System.Collections.Generic;

using System.Text;


public partial class SurfaceOM2 : Instance
{

    /*
    <message4 to=".MasterPM.paddock" msgtype="Register" ack="false" kind="event" name="potentialresiduedecompositioncalculated" type="type   field name='Pool' array='T'   element   field name='Name' kind='string'    field name='OrganicMatterType' kind='string'    field name='FOM'   field name='amount' kind='single' unit='kgha'    field name='C' kind='single' unit='kgha'    field name='N' kind='single' unit='kgha'    field name='P' kind='single' unit='kgha'    field name='AshAlk' kind='single' unit='kgha'    field   element   field   type"/>
    <message4 to=".MasterPM.paddock" msgtype="Register" ack="false" kind="event" name="surfaceorganicmatterstate" type="type   field name='Pool' array='T'   element   field name='Name' kind='string'    field name='OrganicMatterType' kind='string'    field name='PotDecompRate' kind='single' unit='day^-1'    field name='no3' kind='single' unit='kgha'    field name='nh4' kind='single' unit='kgha'    field name='po4' kind='single' unit='kgha'    field name='StandingFraction' array='T'   element   field name='amount' kind='single' unit='kgha'    field name='C' kind='single' unit='kgha'    field name='N' kind='single' unit='kgha'    field name='P' kind='single' unit='kgha'    field name='AshAlk' kind='single' unit='kgha'    element   field   field name='LyingFraction' array='T'   element   field name='amount' kind='single' unit='kgha'    field name='C' kind='single' unit='kgha'    field name='N' kind='single' unit='kgha'    field name='P' kind='single' unit='kgha'    field name='AshAlk' kind='single' unit='kgha'    element   field   element   field   type"/>
    <message4 to=".MasterPM.paddock" msgtype="Register" ack="false" kind="event" name="IncorpFOMPool" type="type   field name='Layer' array='T'   element   field name='thickness' kind='single' units='mm'    field name='no3' kind='single' unit='kgha'    field name='nh4' kind='single' unit='kgha'    field name='po4' kind='single' unit='kgha'    field name='Pool' array='T'   element   field name='amount' kind='single' unit='kgha'    field name='C' kind='single' unit='kgha'    field name='N' kind='single' unit='kgha'    field name='P' kind='single' unit='kgha'    field name='AshAlk' kind='single' unit='kgha'    element   field   element   field   type"/>
    <message4 to=".MasterPM.paddock" msgtype="Register" ack="false" kind="event" name="residue_added" type="type   field name='param1_name' kind='string'    field name='param1_numbytes' kind='integer4'    field name='param1_code' kind='integer4'    field name='param1_isarray' kind='boolean'    field name='param1_value' kind='string' array='T'    field name='param2_name' kind='string'    field name='param2_numbytes' kind='integer4'    field name='param2_code' kind='integer4'    field name='param2_isarray' kind='boolean'    field name='param2_value' kind='string' array='T'    field name='param3_name' kind='string'    field name='param3_numbytes' kind='integer4'    field name='param3_code' kind='integer4'    field name='param3_isarray' kind='boolean'    field name='param3_value' kind='string' array='T'    field name='param4_name' kind='string'    field name='param4_numbytes' kind='integer4'    field name='param4_code' kind='integer4'    field name='param4_isarray' kind='boolean'    field name='param4_value' kind='string' array='T'    field name='param5_name' kind='string'    field name='param5_numbytes' kind='integer4'    field name='param5_code' kind='integer4'    field name='param5_isarray' kind='boolean'    field name='param5_value' kind='string' array='T'    field name='param6_name' kind='string'    field name='param6_numbytes' kind='integer4'    field name='param6_code' kind='integer4'    field name='param6_isarray' kind='boolean'    field name='param6_value' kind='string' array='T'    field name='param7_name' kind='string'    field name='param7_numbytes' kind='integer4'    field name='param7_code' kind='integer4'    field name='param7_isarray' kind='boolean'    field name='param7_value' kind='string' array='T'    type"/>
    <message4 to=".MasterPM.paddock" msgtype="Register" ack="false" kind="event" name="residue_removed" type="type   field name='param1_name' kind='string'    field name='param1_numbytes' kind='integer4'    field name='param1_code' kind='integer4'    field name='param1_isarray' kind='boolean'    field name='param1_value' kind='string' array='T'    field name='param2_name' kind='string'    field name='param2_numbytes' kind='integer4'    field name='param2_code' kind='integer4'    field name='param2_isarray' kind='boolean'    field name='param2_value' kind='string' array='T'    field name='param3_name' kind='string'    field name='param3_numbytes' kind='integer4'    field name='param3_code' kind='integer4'    field name='param3_isarray' kind='boolean'    field name='param3_value' kind='string' array='T'    field name='param4_name' kind='string'    field name='param4_numbytes' kind='integer4'    field name='param4_code' kind='integer4'    field name='param4_isarray' kind='boolean'    field name='param4_value' kind='string' array='T'    field name='param5_name' kind='string'    field name='param5_numbytes' kind='integer4'    field name='param5_code' kind='integer4'    field name='param5_isarray' kind='boolean'    field name='param5_value' kind='string' array='T'    field name='param6_name' kind='string'    field name='param6_numbytes' kind='integer4'    field name='param6_code' kind='integer4'    field name='param6_isarray' kind='boolean'    field name='param6_value' kind='string' array='T'    field name='param7_name' kind='string'    field name='param7_numbytes' kind='integer4'    field name='param7_code' kind='integer4'    field name='param7_isarray' kind='boolean'    field name='param7_value' kind='string' array='T'    type"/>
    <message4 to=".MasterPM.paddock" msgtype="Register" ack="false" kind="event" name="surfaceom_removed" type="type   field name='param1_name' kind='string'    field name='param1_numbytes' kind='integer4'    field name='param1_code' kind='integer4'    field name='param1_isarray' kind='boolean'    field name='param1_value' kind='string' array='T'    field name='param2_name' kind='string'    field name='param2_numbytes' kind='integer4'    field name='param2_code' kind='integer4'    field name='param2_isarray' kind='boolean'    field name='param2_value' kind='string' array='T'    field name='param3_name' kind='string'    field name='param3_numbytes' kind='integer4'    field name='param3_code' kind='integer4'    field name='param3_isarray' kind='boolean'    field name='param3_value' kind='string' array='T'    field name='param4_name' kind='string'    field name='param4_numbytes' kind='integer4'    field name='param4_code' kind='integer4'    field name='param4_isarray' kind='boolean'    field name='param4_value' kind='string' array='T'    field name='param5_name' kind='string'    field name='param5_numbytes' kind='integer4'    field name='param5_code' kind='integer4'    field name='param5_isarray' kind='boolean'    field name='param5_value' kind='string' array='T'    field name='param6_name' kind='string'    field name='param6_numbytes' kind='integer4'    field name='param6_code' kind='integer4'    field name='param6_isarray' kind='boolean'    field name='param6_value' kind='string' array='T'    field name='param7_name' kind='string'    field name='param7_numbytes' kind='integer4'    field name='param7_code' kind='integer4'    field name='param7_isarray' kind='boolean'    field name='param7_value' kind='string' array='T'    type"/>
    <message4 to=".MasterPM.paddock" msgtype="Register" ack="false" kind="event" name="decomposed" type="type"/>
    <message4 to=".MasterPM.paddock" msgtype="Register" ack="false" kind="event" name="externalmassflow" type="type   field name='PoolClass' kind='string' unit='-'    field name='FlowType' kind='string' unit='-'    field name='C' kind='single' unit='kgha'    field name='N' kind='single' unit='kgha'    field name='P' kind='single' unit='kgha'    field name='DM' kind='single' unit='kgha'    field name='SW' kind='single' unit='mm'    type"/>
     */
    [Event]
    public event ExternalMassFlowDelegate ExternalMassFlow;
    private void publish_ExternalMassFlow(ExternalMassFlowType massBalanceChange)
    {
        if (ExternalMassFlow != null)
            ExternalMassFlow.Invoke(massBalanceChange);
    }

    [Event]
    public event SurfaceOrganicMatterDecompDelegate SurfaceOrganicMatterDecomp;
    private void publish_SurfaceOrganicMatterDecomp(SurfaceOrganicMatterDecompType SOMDecomp)
    {
        if (SurfaceOrganicMatterDecomp != null)
            SurfaceOrganicMatterDecomp.Invoke(SOMDecomp);
    }

    [Event]
    public event SurfaceOrganicMatterDelegate SurfaceOrganicMatter;
    private void publish_SurfaceOrganicMatter(SurfaceOrganicMatterType SOM)
    {
        if (SurfaceOrganicMatter != null)
            SurfaceOrganicMatter.Invoke(SOM);
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

