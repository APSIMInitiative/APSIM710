using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using ModelFramework;
using CSGeneral;

public class SimpleTree
{
    [Link]
    Paddock MyPaddock; // Can be used to dynamically get access to simulation structure and variables
    [Input]
    DateTime Today;   // Equates to the value of the current simulation date - value comes from CLOCK
    [Output]
    RootSystemType RootSystem;
    [Output]
    double CoverLive = 0.5;
    [Output]
    string plant_status = "alive";
    [Output]
    double sw_demand = 0;

    private int NumPlots;
    private double[] dlayer;
    private Component SoilWat;

    // The following event handler will be called once at the beginning of the simulation
    [EventHandler]
    public void OnInitialised()
    {
        NumPlots = MyPaddock.Parent.ChildPaddocks.Count; 
        RootSystem = new RootSystemType();
    }

    [EventHandler]
    public void OnPrepare()
    {
        RootSystem.Zone = new RootSystemZoneType[NumPlots];

        for (int i = 0; i < NumPlots; i++)
        {
            RootSystem.Zone[i] = new RootSystemZoneType();
            SoilWat = (Component)MyPaddock.LinkByType("SoilWat");
            SoilWat.Get("dlayer", out RootSystem.Zone[i].dlayer);
            RootSystem.Zone[i].ZoneName = MyPaddock.Parent.ChildPaddocks[i].Name;
            RootSystem.Zone[i].RootDepth = 550;
            RootSystem.Zone[i].kl = new double[RootSystem.Zone[i].dlayer.Length];
            RootSystem.Zone[i].ll = new double[RootSystem.Zone[i].dlayer.Length];

            for (int j = 0; j < RootSystem.Zone[i].dlayer.Length; j++)
            {
                RootSystem.Zone[i].kl[j] = 0.02;
                RootSystem.Zone[i].ll[j] = 0.15;
            }
        }
        GetPotSWUptake();
    }

    private void GetPotSWUptake()
    {
        double TotPotSWUptake = 0;
        double[] SWDep;
        double[] LL15Dep;
        double[][] PotSWUptake = new double[RootSystem.Zone.Length][];

        for (int i = 0; i < RootSystem.Zone.Length; i++)
        {
            PotSWUptake[i] = new double[RootSystem.Zone[i].dlayer.Length];
            SoilWat.Get("sw_dep", out SWDep);
            SoilWat.Get("ll15_dep", out LL15Dep);
            for (int j = 0; j < SWDep.Length; j++)
            {
                PotSWUptake[i][j] = Math.Max(0.0, RootProportion(j, RootSystem.Zone[i].RootDepth, RootSystem.Zone[i].dlayer) * RootSystem.Zone[i].kl[j] * (SWDep[j] - LL15Dep[j])) * 1; // 1 is the area of the zone - disabled for now
            }
        }

        foreach (double[] i in PotSWUptake)
            foreach (double d in i)
                TotPotSWUptake += d;
        RootSystem.SWDemand = TotPotSWUptake;
        sw_demand = TotPotSWUptake;
    }

    private double RootProportion(int layer, double root_depth, double[] dlayer)
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
}
