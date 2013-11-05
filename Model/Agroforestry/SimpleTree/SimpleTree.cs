using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using ModelFramework;

public class SimpleTree
{
    [Link]
    Paddock MyPaddock; // Can be used to dynamically get access to simulation structure and variables
    [Input]
    DateTime Today;   // Equates to the value of the current simulation date - value comes from CLOCK
    [Output]
    RootSystemType rootSystem;

    private int numPlots;
    private double[] dlayer;
    private Component soilWat;

    // The following event handler will be called once at the beginning of the simulation
    [EventHandler]
    public void OnInitialised()
    {
        numPlots = MyPaddock.Parent.ChildPaddocks.Count;
        soilWat = (Component)MyPaddock.LinkByType("SoilWat");
        rootSystem = new RootSystemType();
        soilWat.Get("dlayer", out dlayer);
    }

    [EventHandler]
    public void OnPrepare()
    {
        /********************************************************************************************************
         * This code will loop through the available paddocks (in the same order they appear in the sim tree)
         * and generate soil water data for each. This is packed into a List that can be picked up by SoilArbitrator
         ********************************************************************************************************/
        rootSystem.Zone = new RootSystemZoneType[numPlots];
        for (int i = 0; i < numPlots; i++)
        {
            rootSystem.Zone[i] = new RootSystemZoneType();
            rootSystem.Zone[i].rootDepth = 550;
            rootSystem.Zone[i].kl = new double[dlayer.Length];
            rootSystem.Zone[i].ll = new double[dlayer.Length];

            for (int j = 0; j < dlayer.Length; j++)
            {
                rootSystem.Zone[i].kl[j] = 0.05;
                rootSystem.Zone[i].ll[j] = 0.15;
            }
        }
    }
}
