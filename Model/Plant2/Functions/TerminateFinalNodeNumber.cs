using System;
using System.Collections.Generic;
using System.Text;
using System.Reflection;


public class TerminateFinalNodeNumber
{
 #region Setup
    [Link]
    Leaf Leaf = null;

    [Link]
    Function Photoperiod = null;

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
    public double _AttainableFinalNodeNumber = 0;
    public double _VernalisationFinalNodeNumber = 0;
    public double _PhotoperiodFinalNodeNumber = 0;
    public double _CommitPrimordia = 0;
    public double _TerminatedFinalNodeNumber = 0;

    [Output]
    public double VernalisationIncrement { get { return _VernalisationIncrement; } }
    [Output]
    public double JuvenileDevelopmentIndex 
    { 
        get 
        {
            if (Leaf.PrimordiaNo == 0)
                return _VernalisationIndex;
            else if ((_VernalisationIndex >= 1) || (Leaf.PrimordiaNo >= _AttainableFinalNodeNumber))
                return 1.0;
            else
                return Math.Max(Leaf.PrimordiaNo / _AttainableFinalNodeNumber, _VernalisationIndex);
        } 
    }
    //JuvenileDevelopmentIndex is a combination of progress toward vernalisation saturation and vegetative development toward final leaf number
    [Output]
    public double VernalisationIndex { get { return _VernalisationIndex; } }
    //Vernalisation index is 1 if fully saturated, 0 if not vernalised at all and some where in between is vernalisation is partly completed.
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
            return (int)Math.Max(_AttainableFinalNodeNumber, _TerminatedFinalNodeNumber); ;
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
    }

    public void UpdateTerminateNodeVariables()
    {
        AttainableFinalNodeNumber();
        VernalisationFinalNodeNumberFunction();
        PhotoperiodFinalNodeNumberFunction();
        CommitPrimordiaFunction();
        TerminatedFinalNodeNumberFunction();
    }

    [EventHandler]
    public void OnSow(SowPlant2Type Sow)
    {
        _VernalisationIndex = VernalisationType;
        _AttainableFinalNodeNumber = Leaf.MaxNodeNo;
    }

 #endregion
    
 #region vernalisation functions

    /// <summary>
    /// Calculate progress toward vernalisation saturation today.
    /// This function calculates a vernalisation index which increases from 0 when the crop is unvernalised to 1 when vernalisation response is saturated
    /// </summary>
    public void IncrementVernalisation()
    {
        if ((MeanT > 0.0) && (MeanT <= 11.0))
            _VernalisationIncrement = VernalisationIntercept + VernalisationSlope * MeanT;
        else if ((MeanT > 11.0) && (MeanT <= 15.0))
            _VernalisationIncrement = (1 - (MeanT - 11.0)/(15.0 - 11.0)) * (VernalisationIntercept + VernalisationSlope * 11.0);
        else
            _VernalisationIncrement = 0.0;

        _VernalisationIndex += _VernalisationIncrement;
    }

 #endregion

 #region Final node number functions
     /// <summary>
     /// AttainableFinalNodeNumber is the final leaf number that the crop may committ to 
     /// It decreases from the MaximumNodeNumber toward a minimum depending on the extent of vernalisation
     /// </summary>
    public void AttainableFinalNodeNumber()
     {
             _AttainableFinalNodeNumber = Leaf.MaxNodeNo * (1 - _VernalisationIndex);
     }

    public void VernalisationFinalNodeNumberFunction()
    {
        if ((Leaf.AppearedCohortNo >= 3.0) && (JuvenileDevelopmentIndex >= 1.0))
        { } // do nothing
        else
        _VernalisationFinalNodeNumber = (Leaf.PrimordiaNo);
    }

    public void PhotoperiodFinalNodeNumberFunction()
     {
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
