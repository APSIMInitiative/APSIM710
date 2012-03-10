using System;
using System.Collections.Generic;
using System.Text;
using ModelFramework;

[Description("Keeps Track of Plants Structural Development")]
public class Structure
{
    #region Inputs
    [Link]
    protected Function ThermalTime = null;
    [Link]
    Leaf Leaf = null;
    [Link]
    public Phenology Phenology = null;
    
    [Link]
    public Population Population = null;
    [Param]
    [Output]
    [Description("Primary Bud")]
    public double PrimaryBudNo = 1;

    [Output]
    [Param]
    [Description("The stage name that leaves get initialised.")]
    public string InitialiseStage = "";
    
    //[Link(NamePath = "MainStemInitialPrimordiaNumber")]
    //public Function MainStemInitialPrimordiaNumber = null;
    //[Link(NamePath = "MainStemPrimordiaInitiationRate")]
    //public Function MainStemPrimordiaInitiationRate = null;
    //[Link(NamePath = "MainStemNodeAppearanceRate")]
    //[Output]
    //public Function MainStemNodeAppearanceRate = null;
    //[Link(NamePath = "MainStemFinalNodeNumber")]
    //public Function FinalNodeNumber = null;
    public double _MainStemPrimordiaNumber = 0;
    public double _MainStemNodeNumber = 0;
    public double DeltaNodeNumber = 0;
    public double _ThermalTime = 0;
        
    [Link(NamePath = "Height")]
    public Function HeightModel = null;

    [Link(NamePath = "BranchingRate")]
    public Function Branching = null;
    [Link(NamePath = "ShadeInducedBranchMortality")]
    public Function ShadeInducedBranchMortality = null;
    [Link(NamePath = "DroughtInducedBranchMortality")]
    public Function DroughtInducedBranchMortality = null;
    #endregion

    [Output]
    [Units("/m2")]
    [Description("Number of Mainstem units per meter")]
    public double MainStemPopn
    {
        get { return Population.Value * PrimaryBudNo; }
    }
    
    #region Structural variables
    public double MainStemPrimordiaNumber
    {
        get
        {
            return _MainStemPrimordiaNumber;
        }
    }
    public double MainStemNodeNumber
    {
        get
        {
            return _MainStemNodeNumber;
        }
    }
    //public double MainStemFinalNodeNumber
    //{
    //    get
    //    {
    //        return FinalNodeNumber.Value;
    //    }
    //}
    public double Height 
    {
        get
        {
            return HeightModel.Value;
        }
    }
    public double BranchingRate
    {
        get
        {
            return Branching.Value;
        }
    }
    public double ProportionStemMortality
    {
        get
        {
            return DroughtInducedBranchMortality.Value + ShadeInducedBranchMortality.Value;
        }
    }
    #endregion

    #region Functions
    public void DoPotentialGrowth()
    {
        /*   if ((Leaf.AppearedCohortNo == (int)MainStemFinalNodeNumber) && (Leaf.AppearedCohortNo > 0.0) && (Leaf.AppearedCohortNo < Leaf.MaxNodeNo)) //If last interger leaf has appeared set the fraction of the final part leaf.
           {
               //Leaf.FinalLeafFraction = MainStemFinalNodeNumber - Leaf.AppearedCohortNo;
           }
        
         //Calculate primordia number
           if (Phenology.OnDayOf(InitialiseStage)) // We have no leaves set up and nodes have just started appearing - Need to initialise Leaf cohorts
                _MainStemPrimordiaNumber = MainStemInitialPrimordiaNumber.Value;
           else if (MainStemPrimordiaInitiationRate.Value > 0.0)
               _MainStemPrimordiaNumber += ThermalTime.Value / MainStemPrimordiaInitiationRate.Value;
           _MainStemPrimordiaNumber = Math.Min(_MainStemPrimordiaNumber, MainStemFinalNodeNumber);

        //Calculate Node number
           if (MainStemNodeNumber > 0) //If statement needs to go
           {
               DeltaNodeNumber = 0;
               if (MainStemNodeAppearanceRate.Value > 0)
                   DeltaNodeNumber = _ThermalTime / MainStemNodeAppearanceRate.Value;
               _MainStemNodeNumber += DeltaNodeNumber;
               _MainStemNodeNumber = Math.Min(_MainStemNodeNumber, MainStemFinalNodeNumber);
           }

           // We have no leaves set up and nodes have just started appearing - Need to initialise Leaf cohorts
           if (Phenology.OnDayOf(InitialiseStage))
           {
               //Leaf.InitialLeafSetup();
           }

           //When primordia number is 1 more than current Leaf cohort number produce a new cohort
           if (MainStemPrimordiaNumber >= Leaf.InitialisedCohortNo + Leaf.FinalLeafFraction)
           {
               //Leaf.InitialiseNewCohort();
           }*/
    }
   
    [EventHandler]
    public void OnNewMet(NewMetType NewMet)
    {
        //This is a fudge until we get around to making canopy temperature drive phenology dirrectly.
        if ((Leaf.DroughtInducedSenAcceleration != null) && (Leaf.DroughtInducedSenAcceleration.Value > 1.0))
            _ThermalTime = ThermalTime.Value * Leaf.DroughtInducedSenAcceleration.Value;
        else _ThermalTime = ThermalTime.Value;
    }

    [EventHandler]
    public void Clear()
    {
        _MainStemNodeNumber = 0;
        _MainStemPrimordiaNumber = 0;
    }
    #endregion
}

