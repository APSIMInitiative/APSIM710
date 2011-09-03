using System;
using System.Collections.Generic;
using System.Text;
using System.Reflection;


public class TerminateFinalNodeNumber : Instance
{
 #region Setup
    [Link]
    Plant Plant = null;
    [Link]
    Leaf Leaf = null;

    //Class Parameters
    [Param]
    private double VernalisationType = 0;
    [Param]
    private double VernalisationIntercept = 0;
    [Param]
    private double VernalisationSlope = 0;
    [Param]
    public double PhotoperiodSensitivity = 0;
    [Param]
    public double SaturationPhotoperiod = 0;
    //Class data members
    private double MaxT = 0;
    private double MinT = 0;
    private double MeanT = 0;
    public double PhotoPeriod = 0;
    private double _VernalisationIncrement = 0;
    private double _VernalisationIndex = 0;
    public bool _CropIsVernalised = false;
    public double _AttainableFinalNodeNumber = 0;
    public double _VernalisationFinalNodeNumber = 0;
    public double _PhotoperiodFinalNodeNumber = 0;
    public double _CommitPrimordia = 0;
    public double _TerminatedFinalNodeNumber = 0;

    [Output]
    public double VernalisationIncrement { get { return _VernalisationIncrement; } }
    [Output]
    public double VernalisationIndex { get { return _VernalisationIndex; } }
    //VernalisationIndex is 0 if the crop is not vernalised, 1 if it is fully vernalised and somewhere inbetween if the vernalisation response is partially saturated
    [Output]
    public double VernalisationFinalNodeNumber { get { return _VernalisationFinalNodeNumber; } }
    //LongDayFinalNodeNumber is the number of leaves that the crop will produce if grown in long photoperiod (greater than the saturating photoperiod)
    //This number increases each day as new primordia are initiated on the apex and is fixed when the crop is vernalised
    [Output]
    public double PhotoperiodFinalNodeNumber { get { return _PhotoperiodFinalNodeNumber; } }
    //DayLengthFinalNodeNumber is the final number of leaves that a fully vernalised crop would produce if it committed its terminated node number on the given day.  
    //This adds leaves to the LongDayFinalNodeNumber depending on how much the photoperiod is below saturating photoperiod and the photoperiod sensitivity of the variety
    [Output]
    public double TerminatedFinalNodeNumber { get { return _TerminatedFinalNodeNumber; } }
    //This is the ultimate number of leaves that the crop will produce.  It it changes with DayLengthFinalNodeNumber until the number of primordia equals DayLengthFinalNodeNumber + 4 and then is fixed.
    [Output]
    public double CommitPrimordia { get { return _CommitPrimordia; } }
    //This is the primordia threshold at which the final node number is fixed when primordia number first exceeds this value.
    [Output]
    public double TargetFinalNodeNumber
    {
        get
        {
            return Math.Max(_AttainableFinalNodeNumber, _TerminatedFinalNodeNumber); ;
        }
    }
    //This returns an estimate of the final node number that the crop will committ to starting off with maximum node number, decreasing as the crop is vernalised and then changing in response to photoperiod 
    //until TerminatedFinalNodeNumber is fixed 

    //Set Photoperiod value at the start of each day
    [EventHandler]
    public void OnNewMet(NewMetType NewMet)
    {
        MaxT = NewMet.maxt;
        MinT = NewMet.mint;
        MeanT = (MaxT + MinT) / 2;
        if (_VernalisationIndex < 1.0)
            IncrementVernalisation();
        CropIsernalised();
    }

    public void UpdateTerminateNodeVariables()
    {
        VernalisationFinalNodeNumberFunction();
        PhotoperiodFinalNodeNumberFunction();
        CommitPrimordiaFunction();
        TerminatedFinalNodeNumberFunction();
    }

    [EventHandler]
    public void OnSow(SowPlant2Type Sow)
    {
        _VernalisationIndex = VernalisationType;
    }

 #endregion

 #region vernalisation functions

    /// <summary>
    /// Calculate progress toward vernalisation saturation today.
    /// This function calculates a vernalisation index which increases from 0 when the crop is unvernalised to 1 when vernalisation response is saturated
    /// </summary>
    public void IncrementVernalisation()
    {
        if ((MeanT > 0.0) && (MeanT <= 15.0))
            _VernalisationIncrement = VernalisationIntercept + VernalisationSlope * MeanT;
        else if ((MeanT > 15.0) && (MeanT <= 18.0))
            _VernalisationIncrement = (MeanT - 15.0) * (VernalisationIntercept + VernalisationSlope * 15.0);
        else
            _VernalisationIncrement = 0.0;

        _VernalisationIndex += _VernalisationIncrement;
    }

    /// <summary>
    /// This function determines if the crop has been vernalised either from saturation of vernalisation index or primordia number reaching the AttainableFinalNodeNumber
    /// </summary>
     public void CropIsernalised()
     {
         if ((_VernalisationIndex >= 1) || (Leaf.PrimordiaNo >= AttainableFinalNodeNumber))
            _CropIsVernalised = true;
     }
 #endregion

 #region Final node number functions
     /// <summary>
     /// AttainableFinalNodeNumber is the final leaf number that the crop may committ to 
     /// It decreases from the MaximumNodeNumber toward a minimum depending on the extent of vernalisation
     /// </summary>
     public double AttainableFinalNodeNumber
     {
         get
         {
             _AttainableFinalNodeNumber = Leaf.MaxNodeNo * (1 - _VernalisationIndex);
             return _AttainableFinalNodeNumber;
         }
     }

     public void VernalisationFinalNodeNumberFunction()
    {
        if ((Leaf.CohortNo >= 3.0) && (_CropIsVernalised == true))
        { } // do nothing
        else
        _VernalisationFinalNodeNumber = Leaf.PrimordiaNo;
    }

     public void PhotoperiodFinalNodeNumberFunction()
     {
         Function Photoperiod = (Function)Children["Photoperiod"];
         double PhotoPeriodResponse = 0;
         if (Photoperiod.Value <SaturationPhotoperiod)
             PhotoPeriodResponse = PhotoperiodSensitivity * (SaturationPhotoperiod - Photoperiod.Value);
         _PhotoperiodFinalNodeNumber = _VernalisationFinalNodeNumber + PhotoPeriodResponse;
     }

     public void CommitPrimordiaFunction()
     {
         _CommitPrimordia = _TerminatedFinalNodeNumber + 4;
     }

    public void TerminatedFinalNodeNumberFunction()
    {
        if (Leaf.PrimordiaNo <= _CommitPrimordia)
           _TerminatedFinalNodeNumber = _PhotoperiodFinalNodeNumber;
    }
 #endregion
}
