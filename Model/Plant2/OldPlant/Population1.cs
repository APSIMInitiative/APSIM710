﻿using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using CSGeneral;

public class Population1
{
    [Link]
    Plant15 Plant = null;

    double _Plants;

    public double Density { get { return _Plants; } }

    [EventHandler]
    public void OnSow(SowPlant2Type Sow)
    {
        _Plants = Sow.Population;
    }

    [Link]
    Phenology Phenology = null;

    [Link]
    SWStress SWStress = null;

    [Link]
    Leaf1 Leaf = null;

    [Link(IsOptional = true)]
    Function CropFailureStressPeriod = null;

    [Link]
    Function DeathHighTemperatureDuringEmergence = null;

    [Param]
    double LeafNumberCritical = 0;

    [Param]
    double TTEmergenceLimit = 0;

    [Param]
    double DaysToGerminationLimit = 0;

    [Param]
    double SWStressPhenoLimit = 0;

    [Param]
    double SWStressPhotoLimit = 0;

    [Param]
    double SWStressPhotoRate = 0;

    private int das;
    private double dlt_plants_failure_germ;
    private double dlt_plants_failure_emergence;
    private double dlt_plants_death_seedling;
    private double dlt_plants_failure_leaf_sen;
    private double dlt_plants_failure_phen_delay;
    private double dlt_plants_death_drought;
    private double dlt_plants;
    private double CumSWStressPheno = 0;
    private double CumSWStressPhoto = 0;
    private double dlt_plants_death_external;

    /// <summary>
    /// This is only called from a Plant1 process method - not used in Plant2.
    /// </summary>
    internal bool PlantDeath()
    {
        das++;
        if (Phenology.InPhase("sowing"))
            dlt_plants_failure_germ = CropFailureGermination();

        else
            dlt_plants_failure_germ = 0.0;

        if (Phenology.InPhase("germination"))
            dlt_plants_failure_emergence = CropFailureEmergence();
        else
            dlt_plants_failure_emergence = 0.0;

        dlt_plants_death_seedling = 0.0;
        if (Phenology.OnDayOf("emergence"))
            dlt_plants_death_seedling = DeathSeedling();

        /*XXXX this needs to be coupled with dlt_leaf_area_sen, c_sen_start_stage  FIXME*/
        if (!Phenology.InPhase("SowingToGermination") && !Phenology.InPhase("GerminationToEmergence"))
            dlt_plants_failure_leaf_sen = CropFailureLeafSen();
        else
            dlt_plants_failure_leaf_sen = 0.0;

        if (CropFailureStressPeriod.Value == 1)
            dlt_plants_failure_phen_delay = CropFailurePhenDelay();
        else
            dlt_plants_failure_phen_delay = 0.0;

        if (CropFailureStressPeriod.Value == 1)
            dlt_plants_death_drought = DeathDrought();
        else
            dlt_plants_death_drought = 0.0;

        DeathActual();
        Util.Debug("Population.dlt_plants=%f", dlt_plants);
        if (MathUtility.FloatsAreEqual(dlt_plants + Density, 0.0))
        {
            //!!!!! fix problem with deltas in update when change from alive to dead ?zero deltas

            double biomass = 0;
            foreach (Organ1 Organ in Plant.Tops)
                biomass += (Organ.Green.Wt + Organ.Senesced.Wt) * Conversions.gm2kg / Conversions.sm2ha;

            // report
            string msg = "Plant death. standing above-ground dm = " + biomass.ToString("f2") + " (kg/ha)";
            Console.WriteLine(msg);
            return true;
            // XX Needs to signal a need to call zero_variables here...
            // Present method is to rely on calling zero_xx at tomorrow's prepare() event.. :(
        }
        return false;
    }

    internal void Update()
    {
        if (CropFailureStressPeriod.Value == 1)
        {
            CumSWStressPheno += (1 - SWStress.Pheno);
            CumSWStressPhoto += (1 - SWStress.Photo);
        }
        _Plants += dlt_plants;
        Util.Debug("Population.Density=%f", Density);
    }

    /// <summary>
    /// Crop failure from lack of germination within a specific maximum number of days.
    /// </summary>
    private double CropFailureGermination()
    {
        if (das >= DaysToGerminationLimit)
        {
            Console.WriteLine("      crop failure because of lack of\r\n" +
                              "         germination within " + DaysToGerminationLimit.ToString() +
                              " days of sowing");
            return -1.0 * Density;
        }
        return 0.0;
    }

    /// <summary>
    /// Crop failure from lack of emergence within a specific maximum
    /// thermal time sum from germination.
    /// </summary>
    private double CropFailureEmergence()
    {
        if (Phenology.CurrentPhase is GerminatingPhase && Phenology.CurrentPhase.TTinPhase > TTEmergenceLimit)
        {
            Console.WriteLine(" failed emergence due to deep planting");
            return -1.0 * Density;
        }
        return 0.0;
    }

    /// <summary>
    /// Determine plant death from prolonged phenology delay.
    /// </summary>
    private double CropFailurePhenDelay()
    {
        if (CumSWStressPheno >= SWStressPhenoLimit)
        {
            Console.WriteLine("Crop failure because of prolonged phenology delay through water stress.");
            return -1.0 * Density;
        }
        return 0.0;
    }

    /// <summary>
    /// Determine plant population death from leaf area senescing
    /// </summary>
    private double CropFailureLeafSen()
    {
        double leaf_area = MathUtility.Divide(Leaf.LAI, Density, 0.0); // leaf area per plant

        if (MathUtility.FloatsAreEqual(leaf_area, 0.0, 1.0e-6))
        {
            Console.WriteLine("Crop failure because of total leaf senescence.");
            return -1.0 * Density;
        }
        return 0.0;
    }

    /// <summary>
    /// Determine seedling death rate due to high soil temperatures during emergence.
    /// </summary>
    private double DeathSeedling()
    {
        // Calculate fraction of plants killed by high temperature
        double killfr = DeathHighTemperatureDuringEmergence.Value;
        double dlt_plants = -Density * killfr;

        if (killfr > 0.0)
        {
            string msg = "Plant kill. ";
            msg = msg + (killfr * Conversions.fract2pcnt).ToString("f2");
            msg = msg + "% failure because of high soil surface temperatures.";
            Console.WriteLine(msg);
        }
        return dlt_plants;
    }

    /// <summary>
    /// Determine plant death rate due to drought
    /// </summary>
    private double DeathDrought()
    {
        double killfr;                                 // fraction of crop population to kill
        double dlt_plants = 0.0;                       // population to kill

        if (Leaf.LeafNumber < LeafNumberCritical &&
            CumSWStressPhoto > SWStressPhotoLimit &&
            !MathUtility.FloatsAreEqual(SWStress.Photo, 1.0))
        {
            killfr = SWStressPhotoRate * (CumSWStressPhoto - SWStressPhotoLimit);
            killfr = MathUtility.Constrain(killfr, 0.0, 1.0);
            dlt_plants = -Density * killfr;

            string msg = "Plant kill. ";
            msg = msg + (killfr * Conversions.fract2pcnt).ToString("f2");
            msg = msg + "% failure because of water stress.";
            Console.WriteLine(msg);
        }
        return dlt_plants;
    }

    /// <summary>
    /// Determine plant death rate due to a range of given processes
    /// </summary>
    private void DeathActual()
    {
        // dlt's are negative so take minimum.
        double pmin = dlt_plants_failure_germ;             // Progressive minimum
        pmin = Math.Min(pmin, dlt_plants_failure_emergence);
        pmin = Math.Min(pmin, dlt_plants_failure_leaf_sen);
        pmin = Math.Min(pmin, dlt_plants_failure_phen_delay);
        pmin = Math.Min(pmin, dlt_plants_death_drought);
        pmin = Math.Min(pmin, dlt_plants_death_seedling);
        pmin = Math.Min(pmin, dlt_plants_death_external);
        dlt_plants = pmin;
        dlt_plants_death_external = 0.0;                //Ugly hack here??
    }


    public double DyingFractionPlants
    {
        get
        {
            double dying_fract_plants = MathUtility.Divide(-dlt_plants, Density, 0.0);
            return MathUtility.Constrain(dying_fract_plants, 0.0, 1.0);
        }
    }






        
}

