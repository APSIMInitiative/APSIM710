using System;
using System.Collections.Generic;
using System.Text;
using System.Reflection;


public class FinalNodeNumber : Instance
   {
    [Link]
    Plant Plant = null;
    [Param]
    [Description("Maximum Final Leaf Number ")]
    public double MaxNodeNo = 0;
    [Param]
    [Description("Initial number of leaf primordia")]
    protected double InitialLeafPrimordia = 0;
    [Param]
    //[Description("Final Node Number")]
    //protected double FinalNodeNoEstimate = 0;
    [Link]
    protected TemperatureFunction ThermalTime = null;

    private double _PrimordiaNumber = 0;

    //ublic override void Initialising()
    //{
    //    _PrimordiaNumber = InitialLeafPrimordia;
    //}

    [Output]
    public double ApexThermaltime
    {
        get
        {
            return ThermalTime.Value;
        }
    }

    public double MaximumNodeNumber()
    {
        return MaxNodeNo;
    }
    
    public double PrimordiaNumber() 
    {
           if (_PrimordiaNumber == 0.0)
                _PrimordiaNumber = InitialLeafPrimordia;
           double DeltaPrimordiaNo = 0;    
           Function NodeInitiationRate = (Function)Children["NodeInitiationRate"];
           if (NodeInitiationRate.Value > 0.0)
               DeltaPrimordiaNo = ThermalTime.Value / NodeInitiationRate.Value;
           _PrimordiaNumber = _PrimordiaNumber + DeltaPrimordiaNo;
           return Math.Min(_PrimordiaNumber, MaxNodeNo);
    }
   }
