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
    
    [Output]
    [Param]
    [Description("The stage name that leaves get initialised.")]
    public string InitialiseStage = "";
    public double DeltaNodeNumber = 0;
    public double _ThermalTime = 0;    

    [Link(NamePath = "MainStemPrimordiaInitiationRate")]
    public Function MainStemPrimordiaInitiationRate = null;
    [Link(NamePath = "MainStemNodeAppearanceRate")]
    public Function MainStemNodeAppearanceRate = null;
    [Link(NamePath = "MainStemFinalNodeNumber")]
    public Function MainStemFinalNodeNumber = null;
    [Link(NamePath = "Height")]
    public Function HeightModel = null;
    [Link(NamePath = "BranchingRate")]
    public Function Branching = null;
    [Link(NamePath = "ShadeInducedBranchMortality")]
    public Function ShadeInducedBranchMortality = null;
    [Link(NamePath = "DroughtInducedBranchMortality")]
    public Function DroughtInducedBranchMortality = null;
    [Link(NamePath = "DensityFactor", IsOptional = true)]
    public Function DensityFactor = null;
    #endregion

    #region Class Members
    double _CurrentPopulation = 0;
    double _CurrentTotalStemPopn = 0;
    public double MaximumNodeNumber = 0;

    [Output]
    [Description("Number of plants per meter2")]
    [Units("/m2")]
    public double Population
    {
        get
        {
            return _CurrentPopulation;
        }
        set
        {
            _CurrentPopulation = value;
        }

    }
    
    [Output]
    [Description("Number of mainstem units per plant")]
    [Units("/plant")]
    public double PrimaryBudNo = 1;
    
    [Output("Number of mainstems per meter")]
    [Units("/m2")]
    public double MainStemPopn
    {
        get {
            double OrigPop = Population * PrimaryBudNo;
            if (DensityFactor == null)
             return OrigPop;
            else
                return OrigPop *(1- DensityFactor.Value);
        }
    }
    
    [Output]
    [Description("Number of stems per meter including main and branch stems")] 
    [Units("/m2")]
    public double TotalStemPopn
    {
        get
        {
            //double n = 0;
            //foreach (LeafCohort L in Leaf.Leaves)
            //    n = Math.Max(n, L._Population);
            //return n;
            return _CurrentTotalStemPopn;
        }
        set
        {
            _CurrentTotalStemPopn = value;
        }
    }
    [Output]
    [Description("Number of mainstem primordia initiated")] 
    public double MainStemPrimordiaNo = 0;
    [Output]
    [Description("Number of mainstem nodes appeared")] 
    public double MainStemNodeNo = 0;

    //Plant leaf number state variables
    [Output]
    [Units("/plant")]
    [Description("Number of leaves appeared per plant including all main stem and branch leaves")]
    public double PlantMainStemNodeNo  //Fixme.  This is named wrong and should be called PlantTotalNodeNo
    {
        get
        {
            double n = 0;
            foreach (LeafCohort L in Leaf.Leaves)
                if (L.IsAppeared)
                    n += L._Population;
            return n / Population;
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
    public double MainStemFinalNodeNo { get { return MainStemFinalNodeNumber.Value; } } //FIXME  For consistency with the naming convention FinalLeafNumber should be called FinalNodeNumber but this will require renaming the finalNodeNumber Object which is a job for another day
    [Output]
    [Units("0-1")]
    [Description("Relative progress toward final leaf")]
    public double RelativeNodeApperance 
    { 
        get 
        {
            if (Leaf.CohortsInitialised == false) //FIXME introduced to removed colateral damage during testing.  Need to remove and fix max leaf area parameterisation in potato.xml
                return 0;
            else
                return MainStemNodeNo / MainStemFinalNodeNo; 
        } 
    }
    [Output]
    [Description("Number of leaves yet to appear")]
    public double RemainingNodeNo { get { return MainStemFinalNodeNo - MainStemNodeNo; } }
   /* public double AttainableFinalNodeNumber
    {
        get
        {
            return MainStemFinalNodeNumber.AttainableFinalNodeNumber;
        }
    }*/
    
    //Structural Variables
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
        if (Phenology.OnDayOf(InitialiseStage) == false) // We have no leaves set up and nodes have just started appearing - Need to initialise Leaf cohorts
           if (MainStemPrimordiaInitiationRate.Value > 0.0)
            {
            MainStemPrimordiaNo += ThermalTime.Value / MainStemPrimordiaInitiationRate.Value;
            }
       
            double StartOfDayMainStemNodeNo = (int)MainStemNodeNo;
            
            MainStemFinalNodeNumber.UpdateVariables();
            MainStemPrimordiaNo = Math.Min(MainStemPrimordiaNo, MaximumNodeNumber);
            
            if (MainStemNodeNo > 0)  //If statement needs to go
            {
               DeltaNodeNumber = 0;
               if (MainStemNodeAppearanceRate.Value > 0)
                   DeltaNodeNumber = ThermalTime.Value / MainStemNodeAppearanceRate.Value;
               MainStemNodeNo += DeltaNodeNumber;
               MainStemNodeNo = Math.Min(MainStemNodeNo, MainStemFinalNodeNo);
            }

            //Set stem population at emergence
            if (Phenology.OnDayOf(InitialiseStage))
            {
                _CurrentTotalStemPopn = MainStemPopn;
            }

            //Increment total stem number if main-stem node number has increased by one.
            if ((MainStemNodeNo - StartOfDayMainStemNodeNo) >= 1.0)
            {
                _CurrentTotalStemPopn += BranchingRate * MainStemPopn;
            }

            //Reduce stem number incase of mortality
            if (ProportionStemMortality > 0)
            {
                double DeltaPopn = Math.Min(ProportionStemMortality * (TotalStemPopn - MainStemPopn), TotalStemPopn - Population);
                _CurrentTotalStemPopn -= DeltaPopn;
            }
    }

    public void ResetStemPopn()
    {
        _CurrentTotalStemPopn = MainStemPopn;
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
        MainStemNodeNo = 0;
        MainStemPrimordiaNo = 0;
        _CurrentTotalStemPopn = 0;
    }

    [EventHandler]
    public void OnInitialised()
    {
        MainStemFinalNodeNumber.SetFinalNodeNumber();
        MaximumNodeNumber = MainStemFinalNodeNumber.Value;
    }
    [EventHandler]
    public void OnSow(SowPlant2Type Sow)
    {
        if (Sow.MaxCover <= 0.0)
            throw new Exception("MaxCover must exceed zero in a Sow event.");
        PrimaryBudNo = Sow.BudNumber;
        //MaxNodeNo = MaximumNodeNumber;
        Population = Sow.Population;

    }
    #endregion
}

