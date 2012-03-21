using System;
using System.Collections.Generic;
using System.Text;
using System.Reflection;

[Description("This Function uses the Jamieson and Brooking phenology model to determine the final main-stem leaf number of a cereal plant")]
public class CerealFinalNodeNumber : Function
{
 #region Setup
    [Link]
    Leaf Leaf = null;
    [Link]
    Structure Structure = null;
    [Link]
    Function Photoperiod = null;
    [Link]
    Phenology Phenology = null;
    
    //Class Parameters
    [Param]
    public double PhotoperiodSensitivity = 0;
    [Param]
    public double SaturationPhotoperiod = 0;
    [Param]
    public double MaximumMainStemNodeNumber = 0;
    [Param]
    public double MinimumMainStemNodeNumber = 0;
    //Class data members
    public double PhotoPeriod = 0;
    public double _AttainableFinalNodeNumber = 0;
    public double _VernalisationFinalNodeNumber = 0;
    public double _PhotoperiodFinalNodeNumber = 0;
    public double _CommitHaunStage = 0;
    public double _TerminatedFinalNodeNumber = 0;
    public double _FinalNodeNumber = 0;
 #endregion

 [Output]
 public override double Value
 {
        get
        {
            return _FinalNodeNumber;
        }
 }

 #region Output variables
    //Attainable final leaf number is the number of leaves that may be aquired given the current extent of vernalisation staturation
    [Output]
    public override double AttainableFinalNodeNumber { get { return _AttainableFinalNodeNumber; } }
    
    //VernalisationFinalNodeNumber is the number of leaves that the crop will produce if grown in long photoperiod (greater than the saturating photoperiod)
    //This number increases each day as new primordia are initiated on the apex and is fixed when the crop completes its juvenile phase
    [Output]
    public override double VernalisationFinalNodeNumber { get { return _VernalisationFinalNodeNumber; } }
    
    //PhotoperiodFinalNodeNumber is the final number of leaves that a fully vernalised crop would produce if it committed its terminated node number on the given day.  
    //This adds leaves to the LongDayFinalNodeNumber depending on how much the photoperiod is below saturating photoperiod and the photoperiod sensitivity of the variety
    [Output]
    public override double PhotoperiodFinalNodeNumber { get { return _PhotoperiodFinalNodeNumber; } }
    
    //This is the ultimate number of leaves that the crop will produce.  It it changes with DayLengthFinalNodeNumber until the number of primordia equals DayLengthFinalNodeNumber + 4 and then is fixed.
    [Output]
    public override double TerminatedFinalNodeNumber { get { return _TerminatedFinalNodeNumber; } }
    
    //This is the primordia threshold at which the final node number is fixed when primordia number first exceeds this value.
    [Output]
    public override double CommitHaunStage { get { return _CommitHaunStage; } }
    
    [Output]
    public override double TargetFinalNodeNumber  { get {return Math.Max(AttainableFinalNodeNumber, TerminatedFinalNodeNumber); } }
    
 #endregion

 #region Final node number functions
    
    public void AttainableFinalNodeNumberFunction()
     {
         _AttainableFinalNodeNumber = (MaximumMainStemNodeNumber - MinimumMainStemNodeNumber) * (1 - Phenology.AccumulatedVernalisation) + MinimumMainStemNodeNumber;
     }

    public void VernalisationFinalNodeNumberFunction()
    {
        if (((Leaf.ExpandedNodeNo >= 2.0) && (Phenology.JuvenileDevelopmentIndex >= 1.0)) | ((Leaf.ExpandedNodeNo >= 2.0) && (AttainableFinalNodeNumber <= Structure.MainStemPrimordiaNo)))
        { } // do nothing
        else
        _VernalisationFinalNodeNumber = (Structure.MainStemPrimordiaNo);
    }

    public void PhotoperiodFinalNodeNumberFunction()
     {
         if (Leaf.CohortsInitialised)
         {
             double PhotoPeriodResponse = 0;
             if (Photoperiod.Value < SaturationPhotoperiod)
                 PhotoPeriodResponse = PhotoperiodSensitivity * (SaturationPhotoperiod - Photoperiod.Value);
             _PhotoperiodFinalNodeNumber = VernalisationFinalNodeNumber + PhotoPeriodResponse;
         }
     }

    public void CommitHaunStageFunction()
    {
         _CommitHaunStage = PhotoperiodFinalNodeNumber / 2;
    }

    public void TerminatedFinalNodeNumberFunction()
    {
        if (Leaf.ExpandedNodeNo <= CommitHaunStage)
        _TerminatedFinalNodeNumber = PhotoperiodFinalNodeNumber;
    }

    public void FinalNodeNumberFunction()
    {
        if (Leaf.CohortsInitialised)
        {
            _FinalNodeNumber = Math.Max(Structure.MainStemNodeNo, TargetFinalNodeNumber);
        }
    }
    
    public override void SetFinalNodeNumber()
    {
        _FinalNodeNumber = MaximumMainStemNodeNumber;
    }

    public override void UpdateVariables()
    {
        if (Phenology.CurrentPhaseName == "Emerging")
        {
            AttainableFinalNodeNumberFunction();
        }
        else
        {
            AttainableFinalNodeNumberFunction();
            VernalisationFinalNodeNumberFunction();
            PhotoperiodFinalNodeNumberFunction();
            CommitHaunStageFunction();
            TerminatedFinalNodeNumberFunction();
            FinalNodeNumberFunction();
         }
    }
    
    public void Clear()
    {
        _FinalNodeNumber = 0;
    }
    
    [EventHandler]
    public void OnSow(SowPlant2Type Sow)
    {
        _AttainableFinalNodeNumber = MaximumMainStemNodeNumber;
        _FinalNodeNumber = MinimumMainStemNodeNumber;
    }
    
 #endregion
}
