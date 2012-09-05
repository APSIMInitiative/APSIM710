using System;
using System.Collections.Generic;
using System.Text;
using ModelFramework;


public class Population
{
    [Link]
    Plant Plant = null;
    
    [Output("Population")]
    public double Density
    {
        get { return Plant.SowingData.Population; }
    }

}

