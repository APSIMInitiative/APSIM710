using System;
using System.Reflection;
using System.Collections.Generic;
using System.Xml.Serialization;
using System.Xml;
using System.Linq;
using ModelFramework;
using CSGeneral;

/// <remarks>
/// This partial class contains most of the soil processes of SoilNitrogen
/// </remarks>
public partial class SoilNitrogen
{

    /// <summary>
    /// Performs the soil C and N balance processes, at APSIM timestep.
    /// </summary>
    /// <remarks>
    /// The processes considered, in order, are:
    ///     - Decomposition of surface residues
    ///     - Urea hydrolysis
    ///     - Denitrification + N2O production
    ///     - SOM mineralisation (humus then m. biomass)
    ///     - Decomposition of FOM
    ///     - Nitrification + N2O production
    /// Note: potential surface organic matter decomposition is given by SurfaceOM module, only N balance is considered here
    ///  If there is a pond then surfaceOM is inactive, the decomposition of OM is done wholly by the pond module
    ///  Also, different parameters are used for some processes when pond is active
    /// </remarks>
    private void EvaluateProcesses()
    {
        int nLayers = dlayer.Length;    // number of soil layers

        // 1. surface residues decomposition
        // 1.1. clear dome deltas
        Array.Clear(dlt_c_res_to_biom, 0, dlt_c_res_to_biom.Length);
        Array.Clear(dlt_c_res_to_hum, 0, dlt_c_res_to_hum.Length);
        Array.Clear(dlt_c_res_to_atm, 0, dlt_c_res_to_atm.Length);
        Array.Clear(dlt_res_nh4_min, 0, dlt_res_nh4_min.Length);
        Array.Clear(dlt_res_no3_min, 0, dlt_res_no3_min.Length);
        // 1.2. get the amounts of C decomposed
        if (isPondActive)
        {
            // There is a pond in the system, the POND module will decompose residues - not SoilNitrogen
            //   the pond module computes the amounts of C added to the soil, here these are added to the top layer
            //   with the C:N ratio of the respective SOM pools (Not sure how N balance is kept here)

            // zero deltas by assigning new array
            dlt_c_decomp = new double[1][];
            dlt_n_decomp = new double[1][];
            dlt_c_decomp[0] = new double[nLayers];
            dlt_n_decomp[0] = new double[nLayers];

            dlt_c_res_to_biom[0] += pond_biom_C;   // humic material from breakdown of residues in pond
            dlt_c_res_to_hum[0] += pond_hum_C;     // biom material from breakdown of residues in pond
        }
        else
        {
            // compute the C and N balance for residue decomposition 
            DecomposeResidues();
        }

        // take each layer in turn and compute the soil C and N processes
        for (int layer = 0; layer < nLayers; layer++)
        {
            // 1.3. add/remove C and N from residue decomposition into appropriate pools

            // organic C pools
            biom_c[layer] += dlt_c_res_to_biom[layer];
            hum_c[layer] += dlt_c_res_to_hum[layer];

            // organic N balance
            hum_n[layer] = MathUtility.Divide(hum_c[layer], HumusCNr, 0.0);
            biom_n[layer] = MathUtility.Divide(biom_c[layer], MBiomassCNr, 0.0);

            // update soil mineral N
            _nh4[layer] += dlt_res_nh4_min[layer];
            _no3[layer] += dlt_res_no3_min[layer];

            // 2. get the urea hydrolysis
            dlt_urea_hydrolysis[layer] = UreaHydrolysis(layer);

            // update soil mineral N
            _urea[layer] -= dlt_urea_hydrolysis[layer];
            _nh4[layer] += dlt_urea_hydrolysis[layer];

            // 3. get the denitrification
            dlt_no3_dnit[layer] = Denitrification(layer);

            // update soil mineral N
            _no3[layer] -= dlt_no3_dnit[layer];

            // N2O loss to atmosphere due to denitrification
            double N2N2O = Denitrification_Nratio(layer);
            dlt_n2o_dnit[layer] = dlt_no3_dnit[layer] / (N2N2O + 1.0);

            // 4. transformations of soil organic matter pools
            // 4.1. mineralisation of humic pool
            MineraliseHumus(layer);

            // 4.2. mineralisation of m. biomass pool
            MineraliseMBiomass(layer);

            // 5. decomposition of FOM pools
            DecomposeFOM(layer);

            // update SOM pools
            biom_c[layer] += dlt_hum_c_biom[layer] - dlt_biom_c_hum[layer] - dlt_biom_c_atm[layer] +
                           dlt_c_fom_to_biom[0][layer] + dlt_c_fom_to_biom[1][layer] + dlt_c_fom_to_biom[2][layer];
            hum_c[layer] += dlt_biom_c_hum[layer] - dlt_hum_c_biom[layer] - dlt_hum_c_atm[layer] +
                           dlt_c_fom_to_hum[0][layer] + dlt_c_fom_to_hum[1][layer] + dlt_c_fom_to_hum[2][layer];

            biom_n[layer] = MathUtility.Divide(biom_c[layer], MBiomassCNr, 0.0);
            hum_n[layer] = MathUtility.Divide(hum_c[layer], HumusCNr, 0.0);

            // update FOM pools
            for (int pool = 0; pool < 3; pool++)
            {
                fom_c_pool[pool][layer] -= (dlt_c_fom_to_biom[pool][layer] + dlt_c_fom_to_hum[pool][layer] + dlt_c_fom_to_atm[pool][layer]);
                fom_n_pool[pool][layer] -= dlt_n_fom[pool][layer];
            }

            // update soil mineral N after mineralisation/immobilisation
            // starts with nh4
            _nh4[layer] += dlt_hum_n_min[layer] + dlt_biom_n_min[layer] + dlt_fom_n_min[layer];
            if (_nh4[layer] < -epsilon)
            {
                nh4_deficit_immob[layer] = -_nh4[layer];
                _nh4[layer] = 0.0;
            }
            else
                nh4_deficit_immob[layer] = 0.0;

            // now change no3
            _no3[layer] -= nh4_deficit_immob[layer];
            if (_no3[layer] < -epsilon)
                throw new Exception("N immobilisation resulted in mineral N in layer(" + (layer + 1).ToString() + ") to go below minimum");
            // note: tests for adequate mineral N for immobilisation have been made so this no3 should not go below no3_min


            // 6. get the nitrification of ammonium-N
            dlt_nitrification[layer] = Nitrification(layer);

            // N2O loss to atmosphere during nitrification
            dlt_n2o_nitrif[layer] = N2OProducedDuringNitrification(layer);

            // update soil mineral N
            _no3[layer] += dlt_nitrification[layer] - dlt_nh4_dnit[layer];
            _nh4[layer] -= dlt_nitrification[layer];

            // 7. check whether values are ok
            // 7.1. Organic forms
            for (int pool = 0; pool < 3; pool++)
            {
                CheckNegativeValues(ref fom_c_pool[pool][layer], layer, "fom_c_pool" + (pool + 1).ToString(), "EvaluateProcesses");
                CheckNegativeValues(ref fom_n_pool[pool][layer], layer, "fom_n_pool" + (pool + 1).ToString(), "EvaluateProcesses");
            }
            CheckNegativeValues(ref biom_c[layer], layer, "biom_c", "EvaluateProcesses");
            CheckNegativeValues(ref hum_c[layer], layer, "hum_c", "EvaluateProcesses");
            CheckNegativeValues(ref biom_n[layer], layer, "biom_n", "EvaluateProcesses");
            CheckNegativeValues(ref hum_n[layer], layer, "hum_n", "EvaluateProcesses");

            // 7.2. Mineral forms
            CheckNegativeValues(ref _urea[layer], layer, "urea", "EvaluateProcesses");
            CheckNegativeValues(ref _nh4[layer], layer, "nh4", "EvaluateProcesses");
            CheckNegativeValues(ref _no3[layer], layer, "no3", "EvaluateProcesses");
        }
    }


    #region >>  The soil C and N processes

    #region »   OM processes

    /// <summary>
    /// Calculate rate of nitrogen mineralization/immobilization of surface residues
    /// </summary>
    /// <remarks>
    /// This will test to see whether adequate mineral nitrogen is available to sustain potential rate of decomposition of
    /// surface residues, which was somputed by SurfaceOM. It aslo calculates net rate of nitrogen mineralization/immobilization
    /// </remarks>
    private void DecomposeResidues()
    {
        int nLayers = dlayer.Length;                            // number of layers in the soil
        int nResidues = residueName.Length;                     // number of residues being considered
        double[] no3_available = new double[nLayers];           // no3 available for mineralisation
        double[] nh4_available = new double[nLayers];           // nh4 available for mineralisation
        int ImmobilisationLayer = getCumulativeIndex(ImmobilisationDepth, dlayer);  // soil layer down to which N is available for mineralisation
        double[] fracLayer = FractionLayer(ImmobilisationDepth);          // fraction of each layer that is within mineralisation depth
        double[] dlt_c_to_biom = new double[nResidues];         // C mineralized converted to biomass
        double[] dlt_c_to_hum = new double[nResidues];          // C mineralized converted to humus

        // 1. zero deltas by assigning new array
        dlt_c_decomp = new double[nResidues][];
        dlt_n_decomp = new double[nResidues][];
        for (int residue = 0; residue < nResidues; residue++)
        {
            dlt_c_decomp[residue] = new double[nLayers];
            dlt_n_decomp[residue] = new double[nLayers];
        }

        // check whether there is any potential residue decompostion
        if (SumDoubleArray(pot_c_decomp) > epsilon)
        {  // there is some decomposition, verify C-N balance

            // 2. get the available mineral N in the soil close to surface (mineralisation depth)
            for (int layer = 0; layer <= ImmobilisationLayer; layer++)
            {
                no3_available[layer] = Math.Max(0.0, _no3[layer]) * fracLayer[layer];
                nh4_available[layer] = Math.Max(0.0, _nh4[layer]) * fracLayer[layer];
            }

            // 3. get the potential transfers to m. biomass and humic pools
            for (int residue = 0; residue < nResidues; residue++)
            {
                dlt_c_to_biom[residue] = pot_c_decomp[residue] * (1.0 - ResiduesRespirationFactor) * ResiduesFractionIntoBiomass;
                dlt_c_to_hum[residue] = pot_c_decomp[residue] * (1.0 - ResiduesRespirationFactor) * (1.0 - ResiduesFractionIntoBiomass);
            }

            // 4. test whether there is adequate N available to meet immobilization demand

            // 4.1. potential N demanded for conversion of FOM into soil OM
            double n_demand = MathUtility.Divide(SumDoubleArray(dlt_c_to_biom), MBiomassCNr, 0.0) +
                              MathUtility.Divide(SumDoubleArray(dlt_c_to_hum), HumusCNr, 0.0);
            // 4.2. total available N for this process
            double n_min_available = SumDoubleArray(nh4_available) + SumDoubleArray(no3_available);
            double n_available = n_min_available + SumDoubleArray(pot_n_decomp);

            // 4.3. factor to reduce mineralization rate if insufficient N is available
            double ReductionFactor = 1.0;
            if (n_demand > n_available)
            {
                ReductionFactor = MathUtility.Divide(n_min_available, n_demand - SumDoubleArray(pot_n_decomp), 0.0);
                ReductionFactor = Math.Max(0.0, Math.Min(1.0, ReductionFactor));
            }

            // 5. partition the additions of C and N to layers
            double dlt_n_decomp_tot = 0.0;
            double dlt_c_atm = 0.0;
            double fractionIntoLayer = 1.0;
            for (int layer = 0; layer <= ImmobilisationLayer; layer++)
            {
                // 5.1. fraction of mineralised stuff going in this layer
                fractionIntoLayer = MathUtility.Divide(dlayer[layer] * fracLayer[layer], ImmobilisationDepth, 0.0);

                // 5.2. adjust C and N amounts for each residue and add to soil OM pools
                for (int residue = 0; residue < nResidues; residue++)
                {
                    dlt_c_decomp[residue][layer] = pot_c_decomp[residue] * ReductionFactor * fractionIntoLayer;
                    dlt_n_decomp[residue][layer] = pot_n_decomp[residue] * ReductionFactor * fractionIntoLayer;
                    dlt_n_decomp_tot += dlt_n_decomp[residue][layer];

                    dlt_c_res_to_biom[layer] += dlt_c_to_biom[residue] * ReductionFactor * fractionIntoLayer;
                    dlt_c_res_to_hum[layer] += dlt_c_to_hum[residue] * ReductionFactor * fractionIntoLayer;
                    dlt_c_atm = pot_c_decomp[residue] * Math.Max(0.0, ResiduesRespirationFactor);
                    dlt_c_res_to_atm[layer] += dlt_c_atm * ReductionFactor * fractionIntoLayer;
                }
            }

            // 6. get the net N mineralised/immobilised (hg/ha) - positive means mineralisation, negative is immobilisation
            double dlt_mineral_n = dlt_n_decomp_tot - n_demand * ReductionFactor;

            // 7. partition mineralised/immobilised N into mineral forms
            if (dlt_mineral_n > epsilon)
            {
                // 7.1. we have mineralisation into NH4, distribute it over the layers
                for (int layer = 0; layer <= ImmobilisationLayer; layer++)
                {
                    fractionIntoLayer = MathUtility.Divide(dlayer[layer] * fracLayer[layer], ImmobilisationDepth, 0.0);
                    dlt_res_nh4_min[layer] = dlt_mineral_n * fractionIntoLayer;
                }
            }
            else if (dlt_mineral_n < -epsilon)
            {
                // 7.2. we have immobilisation, soak up any N required from NH4 then NO3
                for (int layer = 0; layer <= ImmobilisationLayer; layer++)
                {
                    dlt_res_nh4_min[layer] = -Math.Min(nh4_available[layer], Math.Abs(dlt_mineral_n));
                    dlt_mineral_n -= dlt_res_nh4_min[layer];
                }

                for (int layer = 0; layer <= ImmobilisationLayer; layer++)
                {
                    dlt_res_no3_min[layer] = -Math.Min(no3_available[layer], Math.Abs(dlt_mineral_n));
                    dlt_mineral_n -= dlt_res_no3_min[layer];
                }

                // 7.3. check that there is no remaining immobilization demand
                if (Math.Abs(dlt_mineral_n) >= epsilon)
                    throw new Exception("Value for remaining immobilization is out of range");
            }
            // else, there is no net N transformation
        }
        // else, there is no residue decomposition
    }

    /// <summary>
    /// Calculate the transformations of the the soil humic pool, mineralisation (+ve) or immobilisation (-ve)
    /// </summary>
    /// <remarks>
    /// It is assumed that the inert_C component of the humic pool is not subject to mineralisation
    /// some constants have different values when there's a pond, as anaerobic conditions dominate
    /// </remarks>
    /// <param name="layer">the node number representing soil layer for which calculations will be made</param>
    private void MineraliseHumus(int layer)
    {
        // index = 0 for aerobic conditions, 1 for anaerobic conditions
        int index = (!isPondActive) ? 0 : 1;

        // get the potential mineralisation
        double pot_miner = (hum_c[layer] - inert_c[layer]) * AHumusTurnOverRate[index];

        if (pot_miner >= epsilon)
        {
            // get the soil temperature factor
            double stf = SoilTempFactor(layer, index, TempFactorData_MinerSOM);

            // get the soil water factor
            double swf = SoilMoistFactor(layer, index, MoistFactorData_MinerSOM);

            // compute the mineralization amounts of C and N from the humic pool
            double dlt_c_min_tot = pot_miner * stf * swf;
            double dlt_n_min_tot = MathUtility.Divide(dlt_c_min_tot, HumusCNr, 0.0);

            // distribute the mineralised N and C
            dlt_hum_c_biom[layer] = dlt_c_min_tot *(1.0 - AHumusRespirationFactor);
            dlt_hum_c_atm[layer] = dlt_c_min_tot * AHumusRespirationFactor;

            // calculate net mineralization
            dlt_hum_n_min[layer] = dlt_n_min_tot - MathUtility.Divide(dlt_hum_c_biom[layer], MBiomassCNr, 0.0);
        }
        else
        {
            // there is no mineralisation - only reset the delta variables
            dlt_hum_c_biom[layer] = 0.0;
            dlt_hum_c_atm[layer] = 0.0;
            dlt_hum_n_min[layer] = 0.0;
        }
    }

    /// <summary>
    /// Calculate the transformations of the soil biomass pool, mineralisation (+ve) or immobilisation (-ve)
    /// </summary>
    /// <param name="layer">the node number representing soil layer for which calculations will be made</param>
    private void MineraliseMBiomass(int layer)
    {
        // index = 0 for aerobic and 0 for anaerobic conditions
        int index = (!isPondActive) ? 0 : 1;

        // get the potential mineralisation
        double pot_miner = biom_n[layer] * MBiomassTurnOverRate[index];

        if (pot_miner >= epsilon)
        {
            // get the soil temperature factor
            double stf = SoilTempFactor(layer, index, TempFactorData_MinerSOM);

            // get the soil water factor
            double swf = SoilMoistFactor(layer, index, MoistFactorData_MinerSOM);

            // compute the mineralization amounts of C and N from the m. biomass pool
            double dlt_n_min_tot = pot_miner * stf * swf;
            double dlt_c_min_tot = dlt_n_min_tot * MBiomassCNr;

            // distribute the mineralised N and C
            dlt_biom_c_hum[layer] = dlt_c_min_tot * (1.0 - MBiomassRespirationFactor) * (1.0 - MBiomassFractionIntoBiomass);
            dlt_biom_c_atm[layer] = dlt_c_min_tot * MBiomassRespirationFactor;

            // calculate net mineralization
            dlt_biom_n_min[layer] = dlt_n_min_tot - MathUtility.Divide(dlt_biom_c_hum[layer], HumusCNr, 0.0) -
                               MathUtility.Divide((dlt_c_min_tot - dlt_biom_c_atm[layer] - dlt_biom_c_hum[layer]), MBiomassCNr, 0.0);
        }
        else
        {
            // there is no mineralisation - only reset the delta variables
            dlt_biom_c_hum[layer] = 0.0;
            dlt_biom_c_atm[layer] = 0.0;
            dlt_biom_n_min[layer] = 0.0;
        }
    }

    /// <summary>
    /// Calculate the decomposition of the soil Fresh OM, mineralisation (+ve) or immobilisation (-ve)
    /// </summary>
    /// <remarks>
    /// - parameters are given in pairs, for aerobic and anaerobic conditions (with pond)
    /// </remarks>
    /// <param name="layer">the node number representing soil layer for which calculations will be made</param>
    private void DecomposeFOM(int layer)
    {
        // index = 0 for aerobic and 1 for anaerobic conditions
        int index = (!isPondActive) ? 0 : 1;

        // get total available mineral N (kg/ha)
        double mineralN_available = Math.Max(0.0, _no3[layer] + _nh4[layer]);

        // calculate gross amount of C & N released due to mineralisation of the fresh organic matter.
        if (fom_c[layer] >= epsilon)
        {
            double dlt_n_fom_gross_miner = 0.0; // amount of fresh organic N mineralized across fpools (kg/ha)
            double dlt_c_fom_gross_miner = 0.0; // total C mineralized (kg/ha) summed across fpools
            double[] dlt_n_gross_decomp = new double[3]; // amount of fresh organic N mineralized in each pool (kg/ha)
            double[] dlt_c_gross_decomp = new double[3]; // amount of C mineralized (kg/ha) from each pool

            // get the soil temperature factor
            double stf = SoilTempFactor(layer, index, TempFactorData_DecompFOM);

            // get the soil water factor
            double swf = SoilMoistFactor(layer, index, MoistFactorData_DecompFOM);

            // ratio of C in fresh OM to N available for decay
            double cnr = MathUtility.Divide(fom_c[layer], fom_n[layer] + mineralN_available, 0.0);

            // calculate the C:N ratio factor
            double cnrf = CNratioFactor(layer, index, cnrf_CNthreshold, cnrf_ReductionCoeff);

            // C:N ratio of fom
            double fom_cn = MathUtility.Divide(fom_c[layer], fom_n[layer], 0.0);

            // get the decomposition of carbohydrate-like, cellulose-like and lignin-like fractions (fom pools) in turn.
            for (int pool = 0; pool < 3; pool++)
            {
                // get the max decomposition rate for each fpool
                double drate = FractRDFom(pool)[index] * cnrf * stf * swf;

                // calculate the gross amount of fresh organic carbon mineralised (kg/ha)
                dlt_c_gross_decomp[pool] = drate * fom_c_pool[pool][layer];

                // calculate the gross amount of N released from fresh organic matter (kg/ha)
                dlt_n_gross_decomp[pool] = drate * fom_n_pool[pool][layer];

                // sum up values
                dlt_c_fom_gross_miner += dlt_c_gross_decomp[pool];
                dlt_n_fom_gross_miner += dlt_n_gross_decomp[pool];
            }

            // calculate potential transfers of C mineralised to biomass
            double dlt_c_biom_tot = dlt_c_fom_gross_miner * (1.0 -FOMRespirationFactor) * FOMFractionIntoBiomass;

            // calculate potential transfers of C mineralised to humus
            double dlt_c_hum_tot = dlt_c_fom_gross_miner * (1.0 - FOMRespirationFactor) * (1.0 - FOMFractionIntoBiomass);

            // test whether there is adequate N available to meet immobilisation demand
            double n_demand = MathUtility.Divide(dlt_c_biom_tot, MBiomassCNr, 0.0) +
                              MathUtility.Divide(dlt_c_hum_tot, HumusCNr, 0.0);
            double n_available = mineralN_available + dlt_n_fom_gross_miner;

            // factor to reduce mineralisation rates if insufficient N to meet immobilisation demand
            double reductionFactor = 1.0;
            if (n_demand > n_available)
                reductionFactor = Math.Max(0.0, Math.Min(1.0, MathUtility.Divide(mineralN_available, n_demand - dlt_n_fom_gross_miner, 0.0)));

            // now adjust carbon transformations etc. and similarly for N pools
            for (int fractn = 0; fractn < 3; fractn++)
            {
                double dlt_c_act_decomp = dlt_c_gross_decomp[fractn] * reductionFactor;
                dlt_c_fom_to_biom[fractn][layer] = dlt_c_act_decomp * (1.0 -FOMRespirationFactor) * FOMFractionIntoBiomass;
                dlt_c_fom_to_hum[fractn][layer] = dlt_c_act_decomp * (1.0 -FOMRespirationFactor) * (1.0 - FOMFractionIntoBiomass);
                dlt_c_fom_to_atm[fractn][layer] = dlt_c_act_decomp * FOMRespirationFactor;
                dlt_n_fom[fractn][layer] = dlt_n_gross_decomp[fractn] * reductionFactor;
            }
            dlt_fom_n_min[layer] = (dlt_n_fom_gross_miner - n_demand) * reductionFactor;
        }
        else
        {
            // tehre is no decomposition - only reset the delta variables
            for (int fractn = 0; fractn < 3; fractn++)
            {
                dlt_c_fom_to_biom[fractn][layer] = 0.0;
                dlt_c_fom_to_hum[fractn][layer] = 0.0;
                dlt_c_fom_to_atm[fractn][layer] = 0.0;
                dlt_n_fom[fractn][layer] = 0.0;
            }
            dlt_fom_n_min[layer] = 0.0;
        }
    }

    #endregion OM processes

    #region »   N processes

    /// <summary>
    /// Calculate the amount of urea converted to NH4 via hydrolysis (kgN/ha)
    /// </summary>
    /// <remarks>
    /// - very small amounts of urea are hydrolysed promptly, regardless the hydrolysis settings
    /// - parameters are given in pairs, for aerobic and anaerobic conditions (with pond)
    /// </remarks>
    /// <param name="layer">the node number representing soil layer for which calculations will be made</param>
    /// <returns>delta N coverted from urea into NH4</returns>
    private double UreaHydrolysis(int layer)
    {
        double result;

        // index = 0 for aerobic and 1 for anaerobic conditions
        int index = (!isPondActive) ? 0 : 1;

        //if (_urea[layer]< 0.1)
        if (MathUtility.Divide(_urea[layer], SoilDensity[layer] * dlayer[layer], 0.0) < 0.0001) // 0.01ppm
        {
            // urea amount is too small, all will be hydrolised
            result = _urea[layer];
        }
        else
        {
            // potential fraction of urea being hydrolysed
            double totalC = (hum_c[layer] + biom_c[layer]) * convFactor[layer] / 10000;  // (100/1000000) = convert to ppm and then to %
            double pot_hydrol_rate = potHydrol_parmA + potHydrol_parmB * totalC +
                     potHydrol_parmC * ph[layer] + potHydrol_parmD * totalC * ph[layer];
            pot_hydrol_rate = Math.Max(potHydrol_min, Math.Min(1.0, pot_hydrol_rate));

            if (pot_hydrol_rate >= epsilon)
            {
                // get the soil temperature factor
                double stf = SoilTempFactor(layer, index, TempFactorData_UHydrol);

                // get the soil water factor
                double swf = SoilMoistFactor(layer, index, MoistFactorData_UHydrol);

                // actual amount hydrolysed;
                result = Math.Max(0.0, Math.Min(_urea[layer], pot_hydrol_rate * _urea[layer] * Math.Min(swf, stf)));
            }
            else
                result = 0.0;
        }

        return result;
    }

    /// <summary>
    /// Calculate the amount of NH4 converted to NO3 via nitrification
    /// </summary>
    /// <remarks>
    /// - This routine is much simplified from original CERES code
    /// - pH effect on nitrification is not used as pH is not simulated
    /// - parameters are given in pairs, for aerobic and anaerobic conditions (with pond)
    /// </remarks>
    /// <param name="layer">the node number representing soil layer for which calculations will be made</param>
    /// <returns>delta N coverted from NH4 into NO3</returns>
    private double Nitrification(int layer)
    {
        double result;

        // index = 0 for aerobic and 1 for anaerobic conditions
        int index = (!isPondActive) ? 0 : 1;

        // get the potential rate of nitrification for layer
        double nh4ppm = _nh4[layer] * convFactor[layer];
        double pot_nitrif_rate_ppm = MathUtility.Divide(nitrification_pot * nh4ppm, nh4ppm + nh4_at_half_pot, 0.0);

        if (pot_nitrif_rate_ppm >= epsilon)
        {
            // get the soil temperature factor
            double stf = SoilTempFactor(layer, index, TempFactorData_Nitrif);

            // get the soil water factor
            double swf = SoilMoistFactor(layer, index, MoistFactorData_Nitrif);

            // get the soil pH factor
            double phf = SoilpHFactor(layer, index, pHFactorData_Nitrif);

            // get most limiting factor
            double pni = Math.Min(swf, Math.Min(stf, phf));

            // get the actual rate of nitrification
            double nitrif_rate = pot_nitrif_rate_ppm * pni * Math.Max(0.0, 1.0 - InhibitionFactor_Nitrification[layer]);
            result = MathUtility.Divide(nitrif_rate, convFactor[layer], 0.0);      // convert back to kg/ha

        }
        else
            result = 0.0;

        return result;
    }

    /// <summary>
    /// Calculate the amount of N2O produced during nitrification
    /// </summary>
    /// <param name="layer">the soil layer index for which calculations will be made</param>
    /// <returns>delta N coverted into N2O during nitrification</returns>
    /// <returns></returns>
    private double N2OProducedDuringNitrification(int layer)
    {
        double result = dlt_nitrification[layer] * n2oLossFactor;
        return result;
    }

    /// <summary>
    /// Calculate amount of NO3 transformed via denitrification
    /// </summary>
    /// <remarks>
    /// - parameters are given in pairs, for aerobic and anaerobic conditions (with pond)
    /// </remarks>
    /// <param name="layer">the soil layer index for which calculations will be made</param>
    /// <returns>delta N coverted from NO3 into gaseous forms</returns>
    private double Denitrification(int layer)
    {
        // Notes:
        //     Denitrification will happend whenever: 
        //         - the soil water in the layer > the drained upper limit (Godwin et al., 1984),
        //         - the NO3 nitrogen concentration > 1 mg N/kg soil,
        //         - the soil temperature >= a minimum temperature.

        // + Assumptions
        //     That there is a root system present.  Rolston et al. say that the denitrification rate coeffficient (dnit_rate_coeff) of non-cropped
        //       plots was 0.000168 and for cropped plots 3.6 times more (dnit_rate_coeff = 0.0006). The larger rate coefficient was required
        //       to account for the effects of the root system in consuming oxygen and in adding soluble organic C to the soil.

        //+  Notes
        //       Reference: Rolston DE, Rao PSC, Davidson JM, Jessup RE (1984). "Simulation of denitrification losses of Nitrate fertiliser applied
        //        to uncropped, cropped, and manure-amended field plots". Soil Science Vol 137, No 4, pp 270-278.
        //
        //       Reference for Carbon availability factor: Reddy KR, Khaleel R, Overcash MR (). "Carbon transformations in land areas receiving 
        //        organic wastes in relation to nonpoint source pollution: A conceptual model".  J.Environ. Qual. 9:434-442.

        double result;
        int index = 0; // denitrification calcs are not different whether there is pond or not. use 0 as default

        // get available carbon from soil organic pools
        double totalC = (hum_c[layer] + fom_c[layer]) * convFactor[layer];
        double active_c = actC_parmA + actC_parmB * totalC;
        // Note: Ceres wheat has active_c = 0.4* fom_C_pool1 + 0.0031 * 0.58 * hum_C_conc + 24.5

        // get the potential denitrification rate
        double pot_denit_rate = DenitRateCoefficient * active_c;

        if (pot_denit_rate >= epsilon)
        {
            // get the soil temperature factor
            double stf = SoilTempFactor(layer, index, TempFactorData_Denit);

            // get the soil water factor
            double swf = SoilMoistFactor(layer, index, MoistFactorData_Denit);

            // calculate denitrification rate  - kg/ha
            result = pot_denit_rate * _no3[layer] * swf * stf;
        }
        else
            result = 0.0;

        return result;
    }

    /// <summary>
    /// Calculate the N2 to N2O ratio during denitrification
    /// </summary>
    /// <remarks>
    /// parameters are given in pairs, for aerobic and anaerobic conditions (with pond)
    /// </remarks>
    /// <param name="layer">the soil layer index for which calculations will be made</param>
    /// <returns>The ratio between N2 and N2O (0-1)</returns>
    private double Denitrification_Nratio(int layer)
    {
        double result;
        int index = 0; // denitrification calcs are not different whether there is pond or not. use 0 as default

        // the water filled pore space (%)
        double WFPS = sw_dep[layer] / sat_dep[layer] * 100.0;

        // CO2 production today (kgC/ha)
        double CO2_prod = co2_atm[layer];

        // calculate the terms for the formula from Thornburn et al (2010)
        bool didInterpolate;
        double CO2effect = 0.0;
        if (CO2_prod > epsilon)
            CO2effect = Math.Exp(N2N2O_parmB * (_no3[layer] / CO2_prod));
        CO2effect = Math.Max(N2N2O_parmA, CO2effect);
        double WFPSeffect = MathUtility.LinearInterpReal(WFPS, WFPSFactorData_Denit.xVals, WFPSFactorData_Denit.yVals, out didInterpolate);
        result = Math.Max(0.0, dnit_k1 * CO2effect * WFPSeffect);

        return result;
    }

    #endregion

    #region Old FOM auxiliary functions

    private double[] FractRDFom(int fract)
    {
        switch (fract)
        {
            case 0: return FOMCarbTurnOverRate;
            case 1: return FOMCellTurnOverRate;
            case 2: return FOMLignTurnOverRate;
            default: throw new Exception("Coding error: bad fraction in FractRDFom");
        }
    }

    #endregion  old functions

    #region >>  Environmental factors

    /// <summary>
    /// Calculate a temperature factor (0-1) for C and N processes
    /// </summary>
    /// <param name="layer">The soil layer to calculate</param>
    /// <param name="index">Parameter indication whether pond exists</param>
    /// <param name="Parameters">Parameter data</param>
    /// <returns>Temperature limiting factor (0-1)</returns>
    private double SoilTempFactor(int layer, int index, BentStickData Parameters)
    {
        // + Assumptions
        //     index = 0 for aerobic conditions, 1 for anaerobic

        if (index > Parameters.xValueForOptimum.Length - 1)
            throw new Exception("SoilNitrogen.SoilTempFactor - invalid value for \"index\" parameter");

        double Toptimum = Parameters.xValueForOptimum[index];
        double Fzero = Parameters.yValueAtZero[index];
        double CurveN = Parameters.CurveExponent[index];
        double AuxV = Math.Pow(Fzero, 1 / CurveN);
        double Tzero = Toptimum * AuxV / (AuxV - 1);
        double beta = 1 / (Toptimum - Tzero);

        return Math.Min(1.0, Math.Pow(beta * Math.Max(0.0, Tsoil[layer] - Tzero), CurveN));
    }

    /// <summary>
    /// Calculate a soil moist factor (0-1) for C and N processes
    /// </summary>
    /// <param name="layer">The soil layer to calculate</param>
    /// <param name="index">Parameter indication whether pond exists</param>
    /// <param name="Parameters">Parameter data</param>
    /// <returns>Soil moisture limiting factor (0-1)</returns>
    private double SoilMoistFactor(int layer, int index, BrokenStickData Parameters)
    {
        // + Assumptions
        //     index = 0 for aerobic conditions, 1 for anaerobic

        if (index == 0)
        {
            bool didInterpolate;

            // get the modified soil water variable
            double[] yVals = { 0.0, 1.0, 2.0, 3.0 };
            double[] xVals = { 0.0, ll15_dep[layer], dul_dep[layer], sat_dep[layer] };
            double myX = MathUtility.LinearInterpReal(sw_dep[layer], xVals, yVals, out didInterpolate);

            // get the soil moist factor
            return MathUtility.LinearInterpReal(myX, Parameters.xVals, Parameters.yVals, out didInterpolate);
        }
        else if (index == 1) // if pond is active
            return 1.0;
        else
            throw new Exception("SoilNitrogen.SoilMoistFactor - invalid value for \"index\" parameter");
    }

    /// <summary>
    /// Calculate a water filled pore space factor for denitrification processes
    /// </summary>
    /// <param name="layer">The soil layer to calculate</param>
    /// <param name="index">Parameter indication whether pond exists</param>
    /// <param name="Parameters">Parameter data</param>
    /// <returns>limiting factor due to water filled pore space (0-1)</returns>
    private double WaterFilledPoreSpaceFactor(int layer, int index, BrokenStickData Parameters)
    {
        // + Assumptions
        //     index = 0 for aerobic conditions, 1 for anaerobic

        if (index == 0)
        {
            bool didInterpolate;

            // get the WFPS value (%)
            double WFPS = sw_dep[layer] / sat_dep[layer] * 100.0;

            // get the WFPS factor
            return MathUtility.LinearInterpReal(WFPS, Parameters.xVals, Parameters.yVals, out didInterpolate);
        }
        else if (index == 1) // if pond is active
            return 1.0;
        else
            throw new Exception("SoilNitrogen.SoilMoistFactor - invalid value for \"index\" parameter");
    }

    /// <summary>
    /// Calculate a pH factor for C and N processes
    /// </summary>
    /// <param name="layer">The soil layer to calculate</param>
    /// <param name="index">Parameter indication whether pond exists</param>
    /// <param name="Parameters">Parameter data</param>
    /// <returns>Soil pH limiting factor (0-1)</returns>
    private double SoilpHFactor(int layer, int index, BrokenStickData Parameters)
    {
        bool DidInterpolate;
        return MathUtility.LinearInterpReal(ph[layer], Parameters.xVals, Parameters.yVals, out DidInterpolate);
    }

    /// <summary>
    /// Calculate a C:N ratio factor for C and N processes
    /// </summary>
    /// <param name="layer">The soil layer to calculate</param>
    /// <param name="index">Parameter indication whether pond exists</param>
    /// <param name="OptCN">The optimum CN ration, below which there is no limitations</param>
    /// <param name="rateCN">A rate factor to increase limitation as function of increasing CN ratio</param>
    /// <returns>The CN ratio limiting factor</returns>
    private double CNratioFactor(int layer, int index, double OptCN, double rateCN)
    {
        // get total available mineral N (kg/ha)
        double nitTot = Math.Max(0.0, _no3[layer] + _nh4[layer]);

        // get the amounts of fresh organic carbon and nitrogen (kg/ha)
        double fomC = 0.0;
        double fomN = 0.0;
        for (int pool = 0; pool < 3; pool++)
        {
            fomC += fom_c_pool[pool][layer];
            fomN += fom_n_pool[pool][layer];
        }

        // ratio of C in fresh OM to N available for decay
        double cnr = MathUtility.Divide(fomC, fomN + nitTot, 0.0);

        return Math.Max(0.0, Math.Min(1.0, Math.Exp(-rateCN * (cnr - OptCN) / OptCN)));
    }

    #endregion Envmt factors

    #endregion C and N processes

}
