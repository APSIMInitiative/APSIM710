using System;
using System.Collections.Generic;
using System.Text;
using System.Reflection;

public class FinalNodeNumber
{
    //Class Linkages
    [Link]
    protected Function ThermalTime = null;
    [Link(IsOptional = true)]
    protected TerminateFinalNodeNumber TerminateFinalNodeNumber = null;
    [Link(IsOptional = true)]
    protected Function NodeInitiationRate = null;
    [Link(IsOptional = true)]
    protected Function PhotoperiodFactor = null;
    //Class Parameters
    [Param]
    [Description("Maximum Final Leaf Number ")]
    private double MaxNodeNo = 0;
    [Param]
    [Description("Initial number of leaf primordia")]
    private double InitialLeafPrimordia = 0;

    //Class data members
    private double _PrimordiaNumber = 0;
    private double _FinalLeafNumber = 0;

    public void Clear()
    {
        _FinalLeafNumber = 0;
        _PrimordiaNumber = 0;
    }
    public double MaximumNodeNumber
    {
        get { return MaxNodeNo; }
    }
    public double PrimordiaNumber
    {
        get { return _PrimordiaNumber; }
    }
    public double FinalLeafNumber
    {
        get { return _FinalLeafNumber; }
    }

    public void Calculate()
    {
        CalculatePrimordiaNumber();
        UpdateFinalNodeVariables();
        CalculateFinalLeafNumber();
    }

    public void PreEmergenceCalculate()
    {
        if (TerminateFinalNodeNumber != null)
        {
            TerminateFinalNodeNumber.AttainableFinalNodeNumber();
        }
        CalculateFinalLeafNumber();
    }
    private void UpdateFinalNodeVariables()
    {
        if (TerminateFinalNodeNumber != null)
            TerminateFinalNodeNumber.UpdateTerminateNodeVariables();
    }
    public void CalculateFinalLeafNumber()
    {
        if (TerminateFinalNodeNumber != null)
            //_FinalLeafNumber = Math.Max(InitialLeafPrimordia, Math.Min(_PrimordiaNumber, TerminateFinalNodeNumber.TerminatedFinalNodeNumber));
            _FinalLeafNumber = Math.Max(InitialLeafPrimordia, TerminateFinalNodeNumber.TargetFinalNodeNumber);
        else if (PhotoperiodFactor != null)
            _FinalLeafNumber = PhotoperiodFactor.Value * MaxNodeNo;
        else
            _FinalLeafNumber = Math.Min(_PrimordiaNumber, MaxNodeNo);
    }
    private void CalculatePrimordiaNumber()
    {
        if (NodeInitiationRate != null)
        {
            if (_PrimordiaNumber == 0.0)
                _PrimordiaNumber = InitialLeafPrimordia;
            else if (NodeInitiationRate.Value > 0.0)
                _PrimordiaNumber += ThermalTime.Value / NodeInitiationRate.Value;
            _PrimordiaNumber = Math.Min(_PrimordiaNumber, MaxNodeNo);
        }
        else _PrimordiaNumber = MaxNodeNo;
    }

}
