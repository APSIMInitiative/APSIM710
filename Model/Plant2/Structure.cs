using System;
using System.Collections.Generic;
using System.Text;
using ModelFramework;

[Description("Keeps Track of Plants Structural Development")]
public class Structure
{
    #region Paramater Input Classes
    [Link]
    protected Function ThermalTime = null;
    [Link]
    Leaf Leaf = null;
    [Link]
    public Phenology Phenology = null;
    [Link(NamePath = "MainStemPrimordiaInitiationRate")]
    public Function MainStemPrimordiaInitiationRate = null;
    [Link(NamePath = "MainStemNodeAppearanceRate")]
    public Function MainStemNodeAppearanceRate = null;
    [Link(NamePath = "MainStemFinalNodeNumber")]
    public Function MainStemFinalNodeNumberFunction = null;
    [Link(NamePath = "Height")]
    public Function HeightModel = null;
    [Link(NamePath = "BranchingRate")]
    public Function Branching = null;
    [Link(NamePath = "ShadeInducedBranchMortality")]
    public Function ShadeInducedBranchMortality = null;
    [Link(NamePath = "DroughtInducedBranchMortality")]
    public Function DroughtInducedBranchMortality = null;
    [Link(NamePath = "PlantMortality", IsOptional = true)]
    public Function PlantMortality = null;
    #endregion

    #region Class Fields
    [Output]
    [Param]
    [Description("The stage name that leaves get initialised.")]
    public string InitialiseStage = "";
    public double DeltaNodeNumber = 0;
    public double _ThermalTime = 0;  
    double _Population = 0;
    double _TotalStemPopn = 0;
    double _ProportionPlantMortality = 0;
    double _ProportionStemMortality = 0;
    double _ProportionTotalStemMortality = 0;
    public double MaximumNodeNumber = 0;
    #endregion

    #region Class Properties
    //Population state variables
    [Output]
    [Description("Number of plants per meter2")]
    [Units("/m2")]
    public double Population
    {
        get
        {
            return _Population;
        }
        set
        {
            _Population = value;
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
        get 
        {
           return Population * PrimaryBudNo;
        }
    }
    [Output]
    [Description("Number of stems per meter including main and branch stems")] 
    [Units("/m2")]
    public double TotalStemPopn
    {
        get
        {
             return _TotalStemPopn;
        }
        set
        {
            _TotalStemPopn = value;
        }
    }

    //Plant leaf number state variables
    [Output]
    [Description("Number of mainstem primordia initiated")] 
    public double MainStemPrimordiaNo = 0;
    [Output]
    [Description("Number of mainstem nodes appeared")] 
    public double MainStemNodeNo = 0;
    [Output]
    [Units("/plant")]
    [Description("Number of leaves appeared per plant including all main stem and branch leaves")]
    public double PlantTotalNodeNo  
    {
        get
        {
            double n = 0;
            foreach (LeafCohort L in Leaf.Leaves)
                if (L.IsAppeared)
                    n += L.CohortPopulation;
            return n / Population;
        }
    }
    [Output]
    [Units("/PrimaryBud")]
    [Description("Number of appeared leaves per primary bud unit including all main stem and branch leaves")]
    public double PrimaryBudTotalNodeNo
    {
        get { return PlantTotalNodeNo / PrimaryBudNo; }
    }
    [Output]
    [Description("Number of leaves that will appear on the mainstem before it terminates")]
    public double MainStemFinalNodeNo 
    { 
        get 
        { 
            return MainStemFinalNodeNumberFunction.Value; 
        } 
    } 
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
    public double RemainingNodeNo 
    { 
        get 
        { 
            return MainStemFinalNodeNo - MainStemNodeNo; 
        } 
    }
    
    //Utility Variables
    [Output("Height")]
    [Units("mm")]
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
    public double ProportionBranchMortality
    {
        get
        {
            return _ProportionStemMortality;
        }
        set
        {
            _ProportionStemMortality = value;
        }
    }
    public double ProportionPlantMortality
    {
        get
        {
            return _ProportionPlantMortality;
        }
        set
        {
            _ProportionPlantMortality = value;
        }
    }
   /* public double ProportionTotalStemMortality
    {
        get
        {
            return _ProportionTotalStemMortality;
        }
        set
        {
            _ProportionTotalStemMortality = value;
        }
    }*/
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
            
            MainStemFinalNodeNumberFunction.UpdateVariables("");
            MainStemPrimordiaNo = Math.Min(MainStemPrimordiaNo, MaximumNodeNumber);
            
            if (MainStemNodeNo > 0)  //If statement needs to go
            {
               DeltaNodeNumber = 0;
               if (MainStemNodeAppearanceRate.Value > 0)
                   DeltaNodeNumber = ThermalTime.Value / MainStemNodeAppearanceRate.Value;
               MainStemNodeNo += DeltaNodeNumber;
               MainStemNodeNo = Math.Min(MainStemNodeNo, MainStemFinalNodeNo);
            }

        //Fixme  This is redundant now and could be removed
            //Set stem population at emergence
            if (Phenology.OnDayOf(InitialiseStage))
            {
                TotalStemPopn = MainStemPopn;
            }

            double InitialStemPopn = TotalStemPopn;

            //Increment total stem population if main-stem node number has increased by one.
            if ((MainStemNodeNo - StartOfDayMainStemNodeNo) >= 1.0)
            {
                TotalStemPopn += BranchingRate * MainStemPopn;
            }

            //Reduce plant population incase of mortality
            if (PlantMortality != null)
            {
                double DeltaPopn = Population * PlantMortality.Value;
                Population -= DeltaPopn;
                TotalStemPopn -= DeltaPopn;
                ProportionPlantMortality = PlantMortality.Value;
            }
        
            //Reduce stem number incase of mortality
            double PropnMortality = 0;
            if (DroughtInducedBranchMortality != null)
                PropnMortality = DroughtInducedBranchMortality.Value;
            if (ShadeInducedBranchMortality != null)
                PropnMortality += ShadeInducedBranchMortality.Value;
            {
                double DeltaPopn = Math.Min(PropnMortality * (TotalStemPopn - MainStemPopn), TotalStemPopn - Population);
                TotalStemPopn -= DeltaPopn;
                ProportionBranchMortality = PropnMortality;
            }
            //ProportionTotalStemMortality = (InitialStemPopn - TotalStemPopn) / InitialStemPopn;
    }
    public void UpdateHeight()
    {
        HeightModel.UpdateVariables("");
    }
    public void ResetStemPopn()
    {
        TotalStemPopn = MainStemPopn;
    }
    #endregion

    #region Event Handlers
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
        TotalStemPopn = 0;
    }
    [EventHandler]
    public void OnInitialised()
    {
        string initial = "yes";
        MainStemFinalNodeNumberFunction.UpdateVariables(initial);
        MaximumNodeNumber = MainStemFinalNodeNumberFunction.Value;
    }
    [EventHandler]
    public void OnSow(SowPlant2Type Sow)
    {
        if (Sow.MaxCover <= 0.0)
            throw new Exception("MaxCover must exceed zero in a Sow event.");
        PrimaryBudNo = Sow.BudNumber;
        Population = Sow.Population;
    }
    #endregion
}

