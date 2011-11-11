using System;
using System.Collections.Generic;
using System.Text;
using ModelFramework;
using VBMet;
using CSGeneral;

[Model]
public class Plant
{
    [Link(IsOptional=true)]
    Phenology Phenology = null;

    [Link(IsOptional=true)]
    Arbitrator Arbitrator = null;

    [Link]
    ModelEnvironment ModelEnvironment = null;

    public string Name { get { return ModelEnvironment.Name; } }


    private List<Organ> _Organs = new List<Organ>();
    public List<Organ> Organs
    {
        // Public property to return our organs to caller. Used primarily for unit testing.
        get { return _Organs; }
    }
    
    [Input(IsOptional = true)]
    Single swim3 = 0;

 #region Outputs
    [Output("Crop_Type")]
    [Param]
    public string CropType = "";
    [Output]
    private double WaterSupplyDemandRatio = 0;
    [Output]
    public string PlantStatus
    {
        get
        { return "in"; }
    }
    [Output]
    [Units("mm")]
    private double WaterDemand   // Needed for SWIM2
    {
        get
        {
            double Demand = 0;
            foreach (Organ o in Organs)
                Demand += o.WaterDemand;
            return Demand;
        }
    }

    public string FullName
    {
        get { return ModelEnvironment.FullName; }
    }
 #endregion

 #region Plant functions
    private void DoPhenology()
    {
        if (Phenology != null)
            Phenology.DoTimeStep();
    }
    public void DoPotentialGrowth()
    {
        foreach (Organ o in Organs)
            o.DoPotentialGrowth();
    }
    private void DoWater()
    {
        if (swim3 == 0)
        {
            double Supply = 0;
            double Demand = 0;
            foreach (Organ o in Organs)
            {
                Supply += o.WaterSupply;
                Demand += o.WaterDemand;
            }

            if (Demand > 0)
                WaterSupplyDemandRatio = Supply / Demand;
            else
                WaterSupplyDemandRatio = 1;

            double fraction = 1;
            if (Demand > 0)
                fraction = Math.Min(1.0, Supply / Demand);

            foreach (Organ o in Organs)
            {
                if (o.WaterDemand > 0)
                    o.WaterAllocation = fraction * o.WaterDemand;
            }

            double FractionUsed = 0;
            if (Supply > 0)
                FractionUsed = Math.Min(1.0, Demand / Supply);

            foreach (Organ o in Organs)
                o.DoWaterUptake(FractionUsed * Supply);
        }
        else
        {
            double Uptake = 0;
            double Demand = 0;
            double Supply = 0;
            foreach (Organ o in Organs)
            {
                Supply += o.WaterSupply;
                Uptake += o.WaterUptake;
                Demand += o.WaterDemand;
            }
            // It is REALLY dodgy that we need to do this at all
            if (Demand > 0)
                WaterSupplyDemandRatio = Supply / Demand;
            else
                WaterSupplyDemandRatio = 1;

            double fraction = 1;
            if (Demand > 0)
                fraction = Uptake / Demand;
            if (fraction > 1.001)
                throw new Exception("Water uptake exceeds total crop demand.");

            foreach (Organ o in Organs)
            {
                if (o.WaterDemand > 0)
                    o.WaterAllocation = fraction * o.WaterDemand;
            }

            //throw new Exception("Cannot talk to swim3 yet");
        }
    }
    private void DoArbitrator()
    {
        if (Arbitrator != null)
        {
            Arbitrator.DoDM(Organs);
            Arbitrator.DoN(Organs);
        }
    }
    public void DoActualGrowth()
    {
        foreach (Organ o in Organs)
            o.DoActualGrowth();
    }
    public object GetPlantVariable(string VariablePath)
    {
        return ModelEnvironment.Get(VariablePath);
    }
 #endregion

 #region Event handlers and publishers
    [Event]
    public event NewCropDelegate NewCrop;
    [Event]
    public event NullTypeDelegate Sowing;
    [Event]
    public event NullTypeDelegate Cutting;
    [Event]
    public event NewCropDelegate CropEnding;
    [EventHandler]
    public void OnSow(SowPlant2Type Sow)
    {
        // Go through all our children and find all organs.
        foreach (string ChildName in ModelEnvironment.ChildNames())
        {
            Organ Child = ModelEnvironment.Link<Organ>(ChildName);
            if (Child != null)
                Organs.Add((Organ)Child);
        }

        if (NewCrop != null)
        {
            NewCropType Crop = new NewCropType();
            Crop.crop_type = CropType;
            Crop.sender = Name;
            NewCrop.Invoke(Crop);
        } 
        
        if (Sowing != null)
            Sowing.Invoke();
    }
    [EventHandler]
    public void OnProcess()
    {
        DoPhenology();
        DoPotentialGrowth();
        DoWater();
        DoArbitrator();
        DoActualGrowth();
    }
    [EventHandler]
    private void OnEndCrop()
    {
        NewCropType Crop = new NewCropType();
        Crop.crop_type = CropType;
        Crop.sender = Name;
        CropEnding.Invoke(Crop);
    }
    [EventHandler]
    private void OnCut()
    {
        Cutting.Invoke();
    }
 #endregion

}
   
