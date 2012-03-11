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
    //[Link]
    //public FinalNodeNumber FinalNodeNumber = null;
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

    #region Output Variables
    [Output]
    [Units("/m2")]
    [Description("Number of mainstems per meter")]
    public double MainStemPopn
    {
        get { return Population.Value * PrimaryBudNo; }
    }
    [Output]
    [Units("/m2")]
    [Description("Number of stems per meter including main and branch stems")]
    public double TotalStemPopn
    {
        get
        {
            double n = 0;
            foreach (LeafCohort L in Leaf.Leaves)
                n = Math.Max(n, L.Population);
            return n;
        }
    }
    [Output]
    [Description("Number of mainstem primordia initiated")] //Note: PrimordiaNo is a double that increase gradually each day
    public double MainStemPrimordiaNo { get { return Leaf.PrimordiaNo; } }
    [Output]
    [Description("Number of mainstem nodes appeared")] //Note: AppearedNodeNo is a double that increase gradually each day
    public double MainStemNodeNo = 0;

    //Plant leaf number state variables
    [Output]
    [Units("/plant")]
    [Description("Number of leaves appeared per plant including all main stem and branch leaves")]
    public double PlantMainStemNodeNo
    {
        get
        {
            double n = 0;
            foreach (LeafCohort L in Leaf.Leaves)
                if (L.IsAppeared)
                    n += L.Population;
            return n / Population.Value;
        }
    }
    [Output]
    [Units("/PrimaryBud")]
    [Description("Number of appeared leaves per primary bud unit including all main stem and branch leaves")]
    public double PrimaryBudMainStemNodeNo
    {
        get { return PlantMainStemNodeNo / PrimaryBudNo; }
    }
    
    //Leaf State Variables regarding final leaf number
    [Output]
    [Description("Number of leaves that will appear on the mainstem before it terminates")]
    public double MainStemFinalNodeNo { get { return Leaf.FinalLeafNo; } } //FIXME  For consistency with the naming convention FinalLeafNumber should be called FinalNodeNumber but this will require renaming the finalNodeNumber Object which is a job for another day
    [Output]
    [Units("0-1")]
    [Description("Relative progress toward final leaf")]
    public double RelativeNodeApperance { get { return MainStemNodeNo / MainStemFinalNodeNo; } }
    [Output]
    [Description("Number of leaves yet to appear")]
    public double RemainingNodeNo { get { return MainStemFinalNodeNo - MainStemNodeNo; } }

    /*public double MainStemPrimordiaNumber
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
    } */
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

