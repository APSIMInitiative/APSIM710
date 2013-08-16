using System;
using System.Collections.Generic;
using System.Text;
using ModelFramework;
using CSGeneral;
using System.Reflection;
using System.Collections;


public class OilPalmUnderstory
{
    [Link]
    Component My = null;
    public string Name { get { return My.Name; } }
    [Output]
    public string plant_status = "out";
    [Link]
    public Paddock MyPaddock; // Can be used to dynamically get access to simulation structure and variables
    [Input]
    DateTime Today;   // Equates to the value of the current simulation date - value comes from CLOCK
     [Output]
    public string Crop_Type = "OilPalmUnderstory";
    [Output]
    double height = 300.0;
    [Output]
    double cover_tot = 0.0;
    [Output]
    double cover_green = 0.0;
    [Output]
    double RootDepth = 300.0;

    double kl = 0.04;
    [Input]
    double[] ll15_dep = null;
    [Input]
    double[] sw_dep = null;
    [Input]
    double[] dlayer = null;
    [Input]
    double[] no3 = null;
    [Input]
    double Radn = 0.0;
    [Input]
    double eo = 0.0;
    [Output]
    double[] PotSWUptake;
    [Output]
    double[] SWUptake;
    [Output]
    double PEP = 0.0;
    [Output]
    double EP = 0.0;
    [Output]
    double DltDM = 0.0;
    [Output]
    double FW = 0.0;

    double[] PotNUptake;
    double[] NUptake;
    [Output]
    double NFixation = 0.0;

    // The following event handler will be called once at the beginning of the simulation
    [EventHandler]
    public void OnInitialised()
    {
        //MyPaddock.Parent.ChildPaddocks
        PotSWUptake = new double[ll15_dep.Length];
        SWUptake = new double[ll15_dep.Length];
        NUptake = new double[ll15_dep.Length];
        PotNUptake = new double[ll15_dep.Length];
    }
    [Event]
    public event BiomassRemovedDelegate BiomassRemoved;
    [Event]
    public event NewCropDelegate NewCrop;
    [Event]
    public event NullTypeDelegate Sowing;

    [EventHandler]
    public void OnSow(SowPlant2Type Sow)
    {
        plant_status = "alive";

        if (NewCrop != null)
        {
            NewCropType Crop = new NewCropType();
            Crop.crop_type = Crop_Type;
            Crop.sender = Name;
            NewCrop.Invoke(Crop);
        }

        if (Sowing != null)
            Sowing.Invoke();

    }

    [EventHandler]
    public void OnProcess()
    {

        DoWaterBalance();
        DoGrowth();
        DoNBalance();

        // Now add today's growth to the soil - ie assume plants are in steady state.
        BiomassRemovedType BiomassRemovedData = new BiomassRemovedType();
        BiomassRemovedData.crop_type = Crop_Type;
        BiomassRemovedData.dm_type = new string[1] { "litter" };
        BiomassRemovedData.dlt_crop_dm = new float[1] { (float)(DltDM * 10) };
        BiomassRemovedData.dlt_dm_n = new float[1] { (float)( NFixation+MathUtility.Sum(NUptake)) };
        BiomassRemovedData.dlt_dm_p = new float[1] { 0 };
        BiomassRemovedData.fraction_to_residue = new float[1] { 1 };
        BiomassRemoved.Invoke(BiomassRemovedData);


    }

    private void DoGrowth()
    {
        double OPCover = 0;
        if (!MyPaddock.Get("OilPalm.cover_green", out OPCover))
            OPCover = 0.0;
        double RUE = 1.3;
        DltDM = RUE * Radn * cover_green * (1 - OPCover) * FW;

    }

    private void DoWaterBalance()
    {
        double OPCover = 0;
        if (!MyPaddock.Get("OilPalm.cover_green", out OPCover))
            OPCover = 0.0;

        cover_green = 0.40 *(1-OPCover);
        PEP = eo * cover_green * (1 - OPCover);


        for (int j = 0; j < ll15_dep.Length; j++)
            PotSWUptake[j] = Math.Max(0.0,RootProportion(j, RootDepth)* kl * (sw_dep[j] - ll15_dep[j]));

        double TotPotSWUptake = MathUtility.Sum(PotSWUptake);

        EP = 0.0;
        for (int j = 0; j < ll15_dep.Length; j++)
        {
            SWUptake[j] = PotSWUptake[j] * Math.Min(1.0, PEP / TotPotSWUptake);
            EP += SWUptake[j];
            sw_dep[j] = sw_dep[j] - SWUptake[j];

        }
        if (!MyPaddock.Set("Soil Water.sw_dep", sw_dep))
            throw new Exception("Unable to set sw_dep");
        if (PEP > 0.0)
        {
            FW = EP / PEP;
        }
        else
        {
            FW = 1.0;
        }

    }

    private void DoNBalance()
    {
        double Ndemand = DltDM * 10 * 0.021;
        NFixation = Math.Max(0.0, Ndemand * .44);

        for (int j = 0; j < ll15_dep.Length; j++)
        {
            PotNUptake[j] = Math.Max(0.0, RootProportion(j, RootDepth) * no3[j]);
        }

        double TotPotNUptake = MathUtility.Sum(PotNUptake);
        double Fr = Math.Min(1.0, (Ndemand-NFixation) / TotPotNUptake);

        for (int j = 0; j < ll15_dep.Length; j++)
        {
            NUptake[j] = PotNUptake[j] * Fr;
            no3[j] = no3[j] - NUptake[j];

        }
        if (!MyPaddock.Set("Soil Nitrogen.no3", no3))
            throw new Exception("Unable to set no3");

        //NFixation = Math.Max(0.0, Ndemand - MathUtility.Sum(NUptake));

    }


   
    
    private double RootProportion(int layer, double root_depth)
    {
        double depth_to_layer_bottom = 0;   // depth to bottom of layer (mm)
        double depth_to_layer_top = 0;      // depth to top of layer (mm)
        double depth_to_root = 0;           // depth to root in layer (mm)
        double depth_of_root_in_layer = 0;  // depth of root within layer (mm)
        // Implementation Section ----------------------------------
        for (int i = 0; i <= layer; i++)
            depth_to_layer_bottom += dlayer[i];
        depth_to_layer_top = depth_to_layer_bottom - dlayer[layer];
        depth_to_root = Math.Min(depth_to_layer_bottom, root_depth);
        depth_of_root_in_layer = Math.Max(0.0, depth_to_root - depth_to_layer_top);

        return depth_of_root_in_layer / dlayer[layer];
    }
    private int LayerIndex(double depth)
    {
        double CumDepth = 0;
        for (int i = 0; i < dlayer.Length; i++)
        {
            CumDepth = CumDepth + dlayer[i];
            if (CumDepth >= depth) { return i; }
        }
        throw new Exception("Depth deeper than bottom of soil profile");
    }

}
