using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using ModelFramework;
using CSGeneral;

public class SoilArbitrator
{
    [Link]
    Simulation paddock;
    [Input]
    DateTime Today;   // Equates to the value of the current simulation date - value comes from CLOCK
    [Output]
    float no_water = 1.0f;

    float[] kl;
    float[] cropLL;
    double[] dlayer;
    float swDemand;
    double[,] PotSWUptake;  // Potential SW uptake from each layer x zone (L)
    double[,] SWUptake;
    double TotPotSWUptake;
    double[] swdep;
    float rootDepth;
    Paddock[] MyPaddocks;
    Component[] MySoilWats;
    Component rootModel;
    RootSystemType rootData;

    // The following event handler will be called once at the beginning of the simulation
    [EventHandler]
    public void OnInitialised()
    {
        MyPaddocks = new Paddock[paddock.ChildPaddocks.Count];
        MySoilWats = new Component[paddock.ChildPaddocks.Count];

        for (int i = 0; i < paddock.ChildPaddocks.Count; i++)
        {
            MySoilWats[i] = (Component)paddock.ChildPaddocks[i].LinkByType("SoilWat");
            foreach (Component c in paddock.ChildPaddocks[i].Children) //find tree model. should go in crops eventually.
            {
                if (c.Name.ToLower().Contains("simpletree"))
                    rootModel = c;
            }
        }
    }

    // The following event handler will be called each day at the beginning of the day
    [EventHandler]
    public void OnPrepare()
    {
    }

    [EventHandler]
    public void OnProcess()
    {
        MySoilWats[0].Get("dlayer", out dlayer); //assume that all zones will have the same dlayers
        for (int i = 0; i < paddock.ChildPaddocks.Count; i++)
        {
            foreach (Component c in paddock.ChildPaddocks[i].Crops)
            {
                if (c.Name.ToLower().Contains("maize"))
                {
                    WaterUptake(c, MySoilWats[i]);
                    //c.Set("my_dlt", );
                }
            }
        }

        object temp;
        Console.WriteLine("mark");
        rootModel.Get("rootSystem", out temp); Console.WriteLine(temp);
        rootData = (RootSystemType)temp;

        for (int i = 0; i < rootData.Zone.Length; i++)
        {
            Console.WriteLine("DATS " + rootData.Zone[i].kl + " " + rootData.Zone[i].ll + " " + rootData.Zone[i].rootDepth);
        }
    }

    private void WaterUptake(Component crop, Component soil)
    {
        crop.Get("sw_demand", out swDemand);
        crop.Get("root_depth", out rootDepth);
        crop.Get("ll", out cropLL);
        crop.Get("kl", out kl);
        soil.Get("sw_dep", out swdep);

        PotSWUptake = new double[paddock.ChildPaddocks.Count, dlayer.Length];
        SWUptake = new double[paddock.ChildPaddocks.Count, dlayer.Length];

        for (int i = 0; i < paddock.ChildPaddocks.Count; i++)
            for (int j = 0; j < dlayer.Length; j++)
            {
                //            Console.WriteLine("RP: " + RootProportion(j, rootDepth) + " RD: " + rootDepth + " kl: " + kl[i] + " ll: " + cropLL[j] + " swdep :" + swdep[j]);
                PotSWUptake[i, j] = Math.Max(0.0, RootProportion(j, rootDepth) * kl[i] * (swdep[j] - cropLL[j])) * paddock.ChildPaddocks.Count;
            }

        TotPotSWUptake = MathUtility.Sum(PotSWUptake);
        //      Console.WriteLine("Demand: " + swDemand + " PotUptake: " + TotPotSWUptake);      
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

}