using System;
using System.Collections.Generic;
using System.Text;
using ModelFramework;
using CSGeneral;
using System.Reflection;
using System.Collections;


public class Plant
{
    [Link(IsOptional=true)]
    Phenology Phenology = null;

    [Link(IsOptional=true)]
    Arbitrator Arbitrator = null;

    [Link(IsOptional = true)]
    Structure Structure = null;

    [Link]
    Component My = null;

    public string Name { get { return My.Name; } }
    public SowPlant2Type SowingData;

    private List<Organ> _Organs = new List<Organ>();
    [Output]
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
    public string plant_status
    {
        get
        {
            // What should be returned here?
            // The old "plant" component returned either "out", "alive"
            // How to determine "dead"?
            return "alive";
        }
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
        get { return My.FullName; }
    }
    [Output]
    public AvailableToAnimalType AvailableToAnimal
    {
        get
        {
            AvailableToAnimalType avail = new AvailableToAnimalType();
            avail.element = new AvailableToAnimalelementType[12];
            int cohort = 0;
            foreach (Organ O in Organs)
            {
                if (O is AboveGround)
                {
                    avail.element[cohort].CohortID = Name;
                    avail.element[cohort].Organ = O.Name;
                    avail.element[cohort].AgeID = "green";
                    avail.element[cohort].Bottom = 0.0;
                    avail.element[cohort].Top = Structure.Height;
                    avail.element[cohort].Chem = "digestible";
                    avail.element[cohort].Weight = O.Live.Wt * 0.1;
                    avail.element[cohort].N = O.Live.N;
                    avail.element[cohort].P = 0.0;
                    avail.element[cohort].S = 0.0;
                    avail.element[cohort].AshAlk = 0.0;

                    cohort++;

                    avail.element[cohort].CohortID = Name;
                    avail.element[cohort].Organ = O.Name;
                    avail.element[cohort].AgeID = "senesced";
                    avail.element[cohort].Bottom = 0.0;
                    avail.element[cohort].Top = Structure.Height;
                    avail.element[cohort].Chem = "digestible";
                    avail.element[cohort].Weight = O.Dead.Wt * 0.1;
                    avail.element[cohort].N = O.Dead.N;
                    avail.element[cohort].P = 0.0;
                    avail.element[cohort].S = 0.0;
                    avail.element[cohort].AshAlk = 0.0;

                    cohort++;
                }
            } 
            return avail; 
        }
    }
 #endregion

 #region Plant functions
    private void DoPhenology()
    {
        if (Phenology != null)
            Phenology.DoTimeStep();
    }
    public void DoDMSetUp()
    {
        if (Structure != null)
            Structure.DoPotentialDM();
        foreach (Organ o in Organs)
            o.DoPotentialDM();
    }
    public void DoNutrientSetUp()
    {
        foreach (Organ o in Organs)
            o.DoPotentialNutrient();
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
                if (o.WaterDemand > 0)
                    o.WaterAllocation = fraction * o.WaterDemand;

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
                if (o.WaterDemand > 0)
                    o.WaterAllocation = fraction * o.WaterDemand;

            //throw new Exception("Cannot talk to swim3 yet");
        }
    }
    public void DoActualGrowth()
    {
        if (Structure != null)
            Structure.DoActualGrowth();
        foreach (Organ o in Organs)
            o.DoActualGrowth();
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
    [Event]
    public event BiomassRemovedDelegate BiomassRemoved;

    [EventHandler]
    public void OnSow(SowPlant2Type Sow)
    {
        SowingData = Sow;

        // Go through all our children and find all organs.
        Organs.Clear();
        foreach (object ChildObject in My.ChildrenAsObjects)
        {
            Organ Child = ChildObject as Organ;
            if (Child != null)
                Organs.Add(Child);
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

        // tell all our children about sow
        foreach (Organ Child in Organs)
            Child.OnSow(Sow);
    }
    [EventHandler]
    public void OnProcess()
    {
        DoPhenology();
        DoDMSetUp();
        DoWater();  //Fixme Do water should go before do DMsetup
        if (Arbitrator != null)
            Arbitrator.DoDMArbitration(Organs);
        DoNutrientSetUp();
        if (Arbitrator != null)
            Arbitrator.DoNutrientArbitration(Organs);
        DoActualGrowth();
    }
    [EventHandler]
    public void OnHarvest()
    {
        // tell all our children about sow
        foreach (Organ Child in Organs)
            Child.OnHarvest();

   // I cannot end call end crop, however can call onharvest?
   // temp
   //     OnEndCrop();

    }
    [EventHandler]
    public void OnEndCrop()
    {
        NewCropType Crop = new NewCropType();
        Crop.crop_type = CropType;
        Crop.sender = Name;
        if (CropEnding != null)
            CropEnding.Invoke(Crop);

        BiomassRemovedType BiomassRemovedData = new BiomassRemovedType();
        BiomassRemovedData.crop_type = CropType;
        BiomassRemovedData.dm_type = new string[Organs.Count];
        BiomassRemovedData.dlt_crop_dm = new float[Organs.Count];
        BiomassRemovedData.dlt_dm_n = new float[Organs.Count];
        BiomassRemovedData.dlt_dm_p = new float[Organs.Count];
        BiomassRemovedData.fraction_to_residue = new float[Organs.Count];
        int i = 0;
        foreach (Organ O in Organs)
        {
            if (O is AboveGround)
            {
                BiomassRemovedData.dm_type[i] = O.Name;
                BiomassRemovedData.dlt_crop_dm[i]=(float)(O.Live.Wt + O.Dead.Wt)*10f;
                BiomassRemovedData.dlt_dm_n[i] = (float)(O.Live.N + O.Dead.N) * 10f;
                BiomassRemovedData.dlt_dm_p[i] = 0f;
                BiomassRemovedData.fraction_to_residue[i] = 1f;
            }
            else
            {
                BiomassRemovedData.dm_type[i] = O.Name;
                BiomassRemovedData.dlt_crop_dm[i] = 0f;
                BiomassRemovedData.dlt_dm_n[i] = 0f;
                BiomassRemovedData.dlt_dm_p[i] = 0f;
                BiomassRemovedData.fraction_to_residue[i] = 0f;
            }
            i++;
        }
        BiomassRemoved.Invoke(BiomassRemovedData);

        // tell all our children about sow
        foreach (Organ Child in Organs)
            Child.OnEndCrop();
    }
    [EventHandler]
    private void OnCut()
    {
        Cutting.Invoke();
    }

 #endregion

}
   
