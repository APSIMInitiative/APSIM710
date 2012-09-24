using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using CSGeneral;
using ModelFramework;

public abstract class Organ1
{
    protected static void ZeroArray(double[] Arr)
    {
        for (int i = 0; i < Arr.Length; i++)
            Arr[i] = 0;
    }
    /// <summary>
    /// Find the first element of an array where a given value
    /// is contained with the cumulative sum_of of the elements.
    /// If sum_of is not reached by the end of the array, then it
    /// is ok to set it to the last element. This will take
    /// account of the case of the number of levels being 0.
    /// </summary>
    protected static int GetCumulativeIndex(double cum_sum, double[] array, int NumElements)
    {
        double progressive_sum = 0.0;
        int indx;

        for (indx = 0; indx < NumElements; indx++)
        {
            progressive_sum = progressive_sum + array[indx];
            if (progressive_sum >= cum_sum)
                break;
        }
        if (indx == NumElements)
            return (indx - 1); // last element in array
        return indx;
    }

    /// <summary>
    /// Accumulates a value in an array, at the specified index.
    /// If the increment in index value changes to a new index, the value
    /// is distributed proportionately between the two indices of the array.
    /// </summary>
    /// <param name="value">value to add to array</param>
    /// <param name="array">array to split</param>
    /// <param name="p_index">current p_index no</param>
    /// <param name="dlt_index">increment in p_index no</param>
    protected static void Accumulate(double value, double[] array, double p_index, double dlt_index)
    {
        double fract_in_old;           // fraction of value in last index
        int new_index;                 // number of index just starting ()
        double portion_in_new;         // portion of value in next index
        double portion_in_old;         // portion of value in last index

        int current_index = (int) Math.Truncate(p_index);

        // make sure the index is something we can work with
        if (current_index >= 0)
        {
            // fraction_of of current index elapsed ()
            double index_devel = p_index - Math.Truncate(p_index) + dlt_index;
            if (index_devel >= 1.0)
            {
                // now we need to divvy
                new_index = (int)(p_index + Math.Min(1.0, dlt_index));
                if (MathUtility.FloatsAreEqual(Math.IEEERemainder(p_index, 1.0), 0.0))
                {
                    fract_in_old = 1.0 - MathUtility.Divide(index_devel - 1.0, dlt_index, 0.0);
                    portion_in_old = fract_in_old * (value + array[current_index]) -
                                         array[current_index];
                }
                else
                {
                    fract_in_old = 1.0f - MathUtility.Divide(index_devel - 1.0, dlt_index, 0.0);
                    portion_in_old = fract_in_old * value;
                }
                portion_in_new = value - portion_in_old;
                array[current_index] = array[current_index] + portion_in_old;
                array[new_index] = array[new_index] + portion_in_new;
            }
            else
            {
                array[current_index] = array[current_index] + value;
            }
        }

        else
            throw new Exception("Accumulate index < 0!!");
    }

    protected static int IncreaseSizeOfBiomassRemoved(BiomassRemovedType BiomassRemoved)
    {
        // Make sure the BiomassRemoved structure has enough elements in it.
        if (BiomassRemoved.dm_type == null)
        {
            BiomassRemoved.dm_type = new string[1];
            BiomassRemoved.fraction_to_residue = new float[1];
            BiomassRemoved.dlt_crop_dm = new float[1];
            BiomassRemoved.dlt_dm_n = new float[1];
            BiomassRemoved.dlt_dm_p = new float[1];
        }
        else
        {
            int NewSize = BiomassRemoved.dm_type.Length + 1;
            Array.Resize(ref BiomassRemoved.dm_type, NewSize);
            Array.Resize(ref BiomassRemoved.fraction_to_residue, NewSize);
            Array.Resize(ref BiomassRemoved.dlt_crop_dm, NewSize);
            Array.Resize(ref BiomassRemoved.dlt_dm_n, NewSize);
            Array.Resize(ref BiomassRemoved.dlt_dm_p, NewSize);
        }
        return BiomassRemoved.dm_type.Length - 1;
    }

    [Link]
    public Component My;

    [Link]
    public Plant15 Plant;


    public string Name { get { return My.Name; } }

    internal virtual void DoPotentialRUE() { }
    internal virtual void DoSWDemand(double Supply) { }
    internal virtual void DoDMDemand(double DMSupply) { }
    internal virtual void DoNDemand1Pot(double dltDmPotRue) { }
    internal virtual double DMSupply { get { return 0.0; } }
    internal virtual double dltDmPotRue { get { return 0.0; } }
    internal virtual double DMGreenDemand { get { return 0.0; } }
    internal virtual void GiveDmGreen(double Delta) { }
    internal virtual double interceptRadiation(double incomingSolarRadiation) { return 0; }
    internal virtual double DMRetransSupply { get { return 0; } }       // leaf and stem should override.
    internal virtual void ZeroDltDmGreen() { }
    internal virtual void ZeroDltNSenescedTrans() { }

    internal virtual double DMDemandDifferential { get { return 0; } }  // fruit parts should override.
    internal virtual void DoDmRetranslocate(double dlt_dm_retrans_to_fruit, double demand_differential_begin) { } // fruit parts should override.
    internal virtual void DoSenescence() { }
    internal virtual void DoNDemand(bool IncludeRetranslocation) { }
    internal virtual void DoSoilNDemand() { }
    internal virtual void DoCover() { }
    internal abstract void DoNRetranslocate(double availableRetranslocateN, double GrainNDemand);
    internal abstract void DoDetachment();
    internal abstract void DoNSenescence();
    internal abstract void DoNSenescedRetranslocation(double navail, double n_demand_tot);
    internal abstract void Update();
    internal virtual void OnPrepare() { }
    internal virtual void OnHarvest(HarvestType Harvest, BiomassRemovedType BiomassRemoved) { }

    internal abstract Biomass Green { get; }
    internal abstract Biomass Senesced { get; }
    internal abstract Biomass Senescing { get; }
    internal abstract Biomass Retranslocation { get; }
    internal abstract Biomass Growth { get; }
    internal abstract Biomass Detaching { get; }
    internal abstract double NCrit { get; }
    internal abstract double NMin { get; }
    internal abstract double NDemand { get; }
    internal abstract double SoilNDemand { get; }

    internal abstract double SWDemand { get; }
    internal abstract double NCapacity { get; }
    internal abstract double NDemandDifferential { get; }
    internal abstract double AvailableRetranslocateN { get ; }
    internal abstract double DltNSenescedRetrans { get; }

    internal virtual double CoverGreen { get { return 0; } }
    internal virtual double CoverSen { get { return 0; } }

    internal void DoNPartition(double GrowthN)
    {
        Growth.StructuralN = GrowthN;
    }
    internal void DoNFixRetranslocate(double NFixUptake, double nFixDemandTotal)
    {
        Growth.StructuralN += NFixUptake * MathUtility.Divide(NDemandDifferential, nFixDemandTotal, 0.0);
    }
    internal abstract void DoNConccentrationLimits();

    protected static void CalcNDemand(double dltDm, double dltDmPotRue, double n_conc_crit, double n_conc_max,
                                     Biomass Growth, Biomass Green, double RetranslocationN, 
                                     double n_deficit_uptake_fraction,
                                     ref double NDemand, ref double NMax)
    {
        double part_fract = MathUtility.Divide(Growth.Wt, dltDm, 0.0);
        double dlt_dm_pot = dltDmPotRue * part_fract;         // potential dry weight increase (g/m^2)
        dlt_dm_pot = MathUtility.Constrain(dlt_dm_pot, 0.0, dltDmPotRue);

        if (Green.Wt > 0.0)
        {
            // get N demands due to difference between
            // actual N concentrations and critical N concentrations
            double N_crit = Green.Wt * n_conc_crit;         // critical N amount (g/m^2)
            double N_potential = Green.Wt * n_conc_max;     // maximum N uptake potential (g/m^2)

            // retranslocation is -ve for outflows
            double N_demand_old = N_crit                       // demand for N by old biomass (g/m^2)
                               - (Green.N + RetranslocationN);
            if (N_demand_old > 0.0)                             // Don't allow demand to satisfy all deficit
                N_demand_old *= n_deficit_uptake_fraction;

            double N_max_old = N_potential                  // N required by old biomass to reach  N_conc_max  (g/m^2)
                               - (Green.N + RetranslocationN);
            if (N_max_old > 0.0)
                N_max_old *= n_deficit_uptake_fraction;         // Don't allow demand to satisfy all deficit

            // get potential N demand (critical N) of potential growth
            double N_demand_new = dlt_dm_pot * n_conc_crit;     // demand for N by new growth (g/m^2)
            double N_max_new = dlt_dm_pot * n_conc_max;      // N required by new growth to reach N_conc_max  (g/m^2)

            NDemand = N_demand_old + N_demand_new;
            NMax = N_max_old + N_max_new;

            NDemand = MathUtility.Constrain(NDemand, 0.0, double.MaxValue);
            NMax = MathUtility.Constrain(NMax, 0.0, double.MaxValue);
        }
        else
            NDemand = NMax = 0.0;
    }


}
