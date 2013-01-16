using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

abstract public class BaseOrgan1 : Organ1
{
    private Biomass _Live = null;
    private Biomass _Dead = null;

    [Link]
    public Biomass Live
    {
        get
        {
            return _Live;
        }
        protected set
        {
            if (_Live == null)
                _Live = value;
            else
                _Live.SetTo(value);
        }
    }

    [Link]
    public Biomass Dead
    {
        get
        {
            return _Dead;
        }
        protected set
        {
            if (_Dead == null)
                _Dead = value;
            else
                _Dead.SetTo(value);
        }
    }

    public abstract string Name { get; }
    public abstract Biomass Senescing { get; protected set; }
    public abstract Biomass Retranslocation { get; protected set; }
    public abstract Biomass Growth { get; protected set; }
    public abstract Biomass Detaching { get; protected set; }

    // Soil water
    public abstract double SWSupply { get; }
    public abstract double SWDemand { get; }
    public abstract double SWUptake { get; }
    public abstract void DoSWDemand(double Supply);
    public abstract void DoSWUptake(double SWDemand);


    // dry matter
    public abstract double DMSupply { get; }
    public abstract double DMRetransSupply { get; }
    public abstract double dltDmPotRue { get; }
    public abstract double DMGreenDemand { get; }
    public abstract double DMDemandDifferential { get; }
    public abstract void DoDMDemand(double DMSupply);
    public abstract void DoDmRetranslocate(double dlt_dm_retrans_to_fruit, double demand_differential_begin);
    public abstract void GiveDmGreen(double Delta);
    public abstract void DoSenescence();
    public abstract void DoDetachment();

    // nitrogen
    public abstract double NSupply { get; }
    public abstract double NDemand { get; }
    public abstract double NUptake { get; }
    public abstract double SoilNDemand { get; }
    public abstract double NCapacity { get; }
    public abstract double NDemandDifferential { get; }
    public abstract double AvailableRetranslocateN { get; }
    public abstract double DltNSenescedRetrans { get; }
    public abstract void DoNDemand(bool IncludeRetranslocation);
    public abstract void DoNDemand1Pot(double dltDmPotRue);
    public abstract void DoSoilNDemand();
    public abstract void DoNSupply();
    public abstract void DoNRetranslocate(double availableRetranslocateN, double GrainNDemand);
    public abstract void DoNSenescence();
    public abstract void DoNSenescedRetranslocation(double navail, double n_demand_tot);
    public abstract void DoNPartition(double GrowthN);
    public abstract void DoNFixRetranslocate(double NFixUptake, double nFixDemandTotal);
    public abstract void DoNConccentrationLimits();
    public abstract void ZeroDltNSenescedTrans();
    public abstract void DoNUptake(double PotNFix);

    // cover
    public abstract double CoverGreen { get; protected set; }
    public abstract double CoverSen { get; protected set; }
    public abstract void DoPotentialRUE();
    public abstract double interceptRadiation(double incomingSolarRadiation);
    public abstract void DoCover();

    // update
    public abstract void Update();

    // events
    public abstract void OnPrepare();
    public abstract void OnHarvest(HarvestType Harvest, BiomassRemovedType BiomassRemoved);
    public abstract void OnEndCrop(BiomassRemovedType BiomassRemoved);
}
