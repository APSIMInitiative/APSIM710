using System;
using System.Collections.Generic;
using System.Text;
using System.Reflection;


public class FinalNodeNumber : Instance
   {

    //Class Linkages
    [Link]
    protected TemperatureFunction ThermalTime = null;
    [Link(IsOptional.Yes)]
    protected TerminateFinalNodeNumber TerminateFinalNodeNumber = null;
    [Link(IsOptional.Yes)]
    protected Function NodeInitiationRate = null;

    //Class Parameters
    [Param]
    [Description("Maximum Final Leaf Number ")]
    private double MaxNodeNo = 0;
    [Param]
    [Description("Initial number of leaf primordia")]
    private double InitialLeafPrimordia = 0;

    //Class data members
    private double _PrimordiaNumber = 0;

    public void UpdateFinalNodeVariables()
    {
        if (TerminateFinalNodeNumber != null)
            TerminateFinalNodeNumber.UpdateTerminateNodeVariables();
    }

    public double MaximumNodeNumber()
    {
        return MaxNodeNo;
    }
    
    public double FinalLeafNumber() 
    {
        double FLN = 0;
        if (TerminateFinalNodeNumber == null)
         FLN = Math.Min(_PrimordiaNumber, MaxNodeNo);
        else FLN = Math.Max(InitialLeafPrimordia, Math.Min(_PrimordiaNumber, TerminateFinalNodeNumber.TerminatedFinalNodeNumber));

        return FLN;
    }

    public double PrimordiaNumber()
    {

        if (NodeInitiationRate != null)
        {
            if (_PrimordiaNumber == 0.0)
                _PrimordiaNumber = InitialLeafPrimordia;
            double DeltaPrimordiaNo = 0;
            //Function NodeInitiationRate = (Function)Children["NodeInitiationRate"];
            if (NodeInitiationRate.Value > 0.0)
                DeltaPrimordiaNo = ThermalTime.Value / NodeInitiationRate.Value;
            _PrimordiaNumber = _PrimordiaNumber + DeltaPrimordiaNo;
        }
        else _PrimordiaNumber = MaxNodeNo;

        return _PrimordiaNumber;
    }
   }
