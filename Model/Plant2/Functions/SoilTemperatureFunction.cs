using System;
using System.Collections.Generic;
using System.Text;
using CSGeneral;

class SoilTemperatureFunction : Function
{
    [Param]
    private double Depth = 0;
    [Input]
    private double[] ave_soil_temp = null;
    [Input]
    public double[] dlayer = null; // Depth of each soil layer in mm         

    [Output]
    public override double Value
    {
        get
        {
            double cummDepth = 0.0; // Cummulative depth
            for (int i = 0; i < dlayer.Length; i++)
            { 
                cummDepth += dlayer[i];
                if (cummDepth >= Depth)
                {
                    return ave_soil_temp[i];                  
                }
            }
            throw new Exception("SoilTemperatureFunction: Specified soil depth of " + Depth.ToString() + " mm is greater than profile depth of " + cummDepth.ToString() + " mm"); 
        }
    }

}
