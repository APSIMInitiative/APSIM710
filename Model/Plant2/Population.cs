using System;
using System.Collections.Generic;
using System.Text;


public class Population
{
    [Link]
    Plant Plant;

    [Param]
    [Output("Population")]
    public double Value;

    [EventHandler]
    public void OnSow(SowPlant2Type Sow)
    {
        Value = Plant.SowingData.Population;
    }

}

