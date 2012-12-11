using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace SoilCN
{
     class Nprocesses
    {
        #region Parameters added by RCichota

        // whether to use new functions to compute temp and moist factors
        public bool useNewSTFFunction = false;
        public bool useNewSWFFunction = false;
        public bool useNewProcesses = false;

        public UreaHydrolysisApproach UreaHydrolysisApproach = UreaHydrolysisApproach.APSIMdefault;

        #endregion

        #region Parameters used on initialisation only

        #region General setting parameters

        #endregion

        #endregion


        #region Parameters for urea hydrolisys process

        // optimum temperature for urea hydrolisys
        private static BendingStickData TempFactor_UHydrol = new BendingStickData();
        [Param]
        public double[] stfHydrol_Topt
        { set { TempFactor_UHydrol.xValueAtOptimum = value; } }

        // temperature factor for urea hydrolisys at zero degrees
        [Param]
        public double[] stfHydrol_FctrZero
        { set { TempFactor_UHydrol.yValueAtZero = value; } }

        // curve exponent for temperature factor for urea hydrolisys
        [Param]
        public double[] stfHydrol_CvExp
        { set { TempFactor_UHydrol.CurveExponent = value; } }

        // parameters for soil moisture factor for hydrolisys
        private static BrokenStickData MoistFactor_UHydrol = new BrokenStickData();
        [Param]
        public double[] swfHydrol_x
        { set { MoistFactor_UHydrol.xVals = value; } }
        [Param]
        public double[] swfHydrol_y
        { set { MoistFactor_UHydrol.yVals = value; } }

        // parameters for computing potential urea hydrolisys - after CERES, see GoodWin and Jones (1991)
        private static double MinPotentialHydrolisys;
        [Param(MinVal = 0.0, MaxVal = 1.0)]
        [Units("0-1")]
        public double potHydrol_min
        { set { MinPotentialHydrolisys = value; } }

        [Param]
        public static double potHydrol_parmA;

        [Param]
        public static double potHydrol_parmB;

        [Param]
        public static double potHydrol_parmC;

        [Param]
        public static double potHydrol_parmD;

        #endregion

        #region Parameters for nitrification process

        // Maximum potential nitrification at optimum conditions () - Michaelis-Menten equation
        private double MaxPotentialNitrificationRate;
        [Param(MinVal = 0.0, MaxVal = 100.0)]
        [Units("mg/kg/d")]
        public double nitrification_pot
        { set { MaxPotentialNitrificationRate = value; } }

        // nh4 concentration at half potential nitrification (ppm) - Michaelis-Menten equation
        private double NH4atHalfPotentialNitrification;
        [Param(MinVal = 0.0, MaxVal = 200.0)]
        [Units("mg/kg")]
        public double nh4_at_half_pot
        { set { NH4atHalfPotentialNitrification = value; } }

        // Fraction of nitrification lost as denitrification
        private double NitrousFractionOnDenit;
        [Param(MinVal = 0.0, MaxVal = 1.0)]
        [Units("0-1")]
        public double dnit_nitrf_loss
        { set { NitrousFractionOnDenit = value; } }

        #region Old parameters

        [Param(MinVal = 0.0, MaxVal = 2.0)]
        public double[] wfnit_index;        // index specifying water content for water factor for nitrification

        [Param(MinVal = 0.0, MaxVal = 1.0)]
        public double[] wfnit_values;       // value of water factor(nitrification) function at given index values

        [Param(MinVal = 0.0, MaxVal = 14.0)]
        public double[] pHf_nit_pH;         // pH values for specifying pH factor for nitrification

        [Param(MinVal = 0.0, MaxVal = 1.0)]
        public double[] pHf_nit_values;     // value of pH factor(nitrification) function for given pH values

        #endregion

        #region New parameters

        // optimum temperature for nitrification
        private BendingStickData TempFactor_Nitrif1 = new BendingStickData();
        [Param]
        private double[] stfNitrif_Topt
        { set { TempFactor_Nitrif1.xValueAtOptimum = value; } }

        // temperature factor for nitrification at zero degrees
        [Param]
        private double[] stfNitrif_FctrZero
        { set { TempFactor_Nitrif1.yValueAtZero = value; } }

        // curve exponent for temperature factor for nitrification
        [Param]
        private double[] stfNitrif_CvExp
        { set { TempFactor_Nitrif1.CurveExponent = value; } }

        // parameters for soil moisture factor for nitrification
        private BrokenStickData MoistFactor_Nitrif1 = new BrokenStickData();
        [Param]
        private double[] swfNitrif_x
        { set { MoistFactor_Nitrif1.xVals = value; } }
        [Param]
        private double[] swfNitrif_y
        { set { MoistFactor_Nitrif1.yVals = value; } }

        // parameters for soil pH factor for nitrification
        private BrokenStickData pHFactor_Nitrif1 = new BrokenStickData();
        [Param]
        private double[] sphfNitrif_x
        { set { pHFactor_Nitrif1.xVals = value; } }
        [Param]
        private double[] sphfNitrif_y
        { set { pHFactor_Nitrif1.yVals = value; } }

        #endregion

        #endregion

        #region Parameters for denitrification and N2O emission processes

        // Denitrification rate coefficient at optimum conditions (fraction per ppm C per day)
        private double DenitRateCoefficient;
        [Param(MinVal = 0.0, MaxVal = 1.0)]
        [Units("/mg/kg/d")]
        public double dnit_rate_coeff
        { set { DenitRateCoefficient = value; } }

        // Parameter K1 from Thorburn et al (2010) for N2O model
        private double N2N2O_parmK1;
        [Param(MinVal = 0.0, MaxVal = 100.0)]
        public double dnit_k1
        { set { N2N2O_parmK1 = value; } }

        #region Old parameters

        [Param(MinVal = 0.0, MaxVal = 5.0)]
        public double dnit_wf_power;        // denitrification water factor power term

        [Param(MinVal = 0.0, MaxVal = 100.0)]
        public double[] dnit_wfps;            // WFPS for calculating the n2o fraction of denitrification

        [Param(MinVal = 0.0, MaxVal = 100.0)]
        public double[] dnit_n2o_factor;      // WFPS factor for n2o fraction of denitrification

        #endregion

        #region New parameters

        // parameter A to compute water soluble organic carbon (for denitrification)
        [Param]
        public double actC_parmA;

        // parameter B to compute water soluble organic carbon (for denitrification)
        [Param]
        public double actC_parmB;

        // optimum temperature for denitrification
        private BendingStickData TempFactor_Denit1 = new BendingStickData();
        [Param]
        public double[] stfDenit_Topt
        { set { TempFactor_Denit1.xValueAtOptimum = value; } }

        // temperature factor for denitrification at zero degrees
        [Param]
        public double[] stfDenit_FctrZero
        { set { TempFactor_Denit1.yValueAtZero = value; } }

        // curve exponent for temperature factor for denitrification
        [Param]
        public double[] stfDenit_CvExp
        { set { TempFactor_Denit1.CurveExponent = value; } }

        // parameters for soil moisture factor for denitrification
        private BrokenStickData MoistFactor_Denit1 = new BrokenStickData();
        [Param]
        public double[] swfDenit_x
        { set { MoistFactor_Denit1.xVals = value; } }
        [Param]
        public double[] swfDenit_y
        { set { MoistFactor_Denit1.yVals = value; } }

        // parameter A in the N2N2O function
        [Param]
        public double N2N2O_parmA;

        // parameter B in the N2N2O function
        [Param]
        public double N2N2O_parmB;

        // parameters for soil moisture factor for denitrification
        private BrokenStickData WFPSFactor_N2N2O1 = new BrokenStickData();
        [Param]
        public double[] wfpsN2N2O_x
        { set { WFPSFactor_N2N2O1.xVals = value; } }
        [Param]
        public double[] wfpsN2N2O_y
        { set { WFPSFactor_N2N2O1.yVals = value; } }

        #endregion

        #endregion

        double[] _urea;
        double[] dlayer;
        /// <summary>
        /// + Purpose:
        ///     Calculate the amount of urea converted to NH4 via hydrolysis (kgN/ha)
        /// </summary>
        /// <remarks>
        /// + Assumptions:
        ///     - very small amounts of urea are hydrolysed promptly, regardless the hydrolysis approach
        ///     - the actual hydrolysis is computed in another method according to the approach chosen
        ///     - parameters are given in pairs, for aerobic and anaerobic conditions (with pond)
        /// </remarks>
        /// <documentation>
        /// 
        /// </documentation>
        /// <param name="layer">the node number representing soil layer for which calculations will be made</param>
        /// <returns>delta N coverted from urea into NH4</returns>
        private double UreaHydrolysis(int layer)
        {
            double result;

            if (_urea[layer] > 0.0)
            {
                // we have urea, so can do some hydrolysis
                if (!useNewProcesses)
                {
                    // using old APSIM-SoilN method
                    result = UreaHydrolysis_ApsimSoilN(layer);
                }
                else
                {
                    // get the minimum urea amount we bother to calc hydrolysis
                    double LowUrea = 0.1 * dlayer[layer] / 200;
                    //  its original value was 0.1 kg/ha, assuming 'typical' thickness as 20cm it was 0.005

                    if (_urea[layer] < LowUrea)
                    {
                        // urea amount is too small, all will be hydrolised
                        result = _urea[layer];
                    }
                    else
                    {
                        switch (UreaHydrolysisApproach)
                        {
                            case UreaHydrolysisApproach.APSIMdefault:
                                // use default soilNitrogen function
                                result = UreaHydrolysis_ApsimSoilNitrogen(layer);
                                break;
                            case UreaHydrolysisApproach.RCichota:
                                // use function define by RCichota
                                result = 0;
                                break;
                            default:
                                throw new Exception("Method for urea hydrolysis not valid");
                        }
                    }
                }
            }
            else
                result = 0.0;
            return result;
        }

        /// <summary>
        /// + Purpose:
        ///     Compute the hydrolysis of urea using the approach from APSIM-SoilN
        /// </summary>
        /// <remarks>
        /// + Assumptions:
        ///     - parameters are given in pairs, for aerobic and anaerobic conditions (with pond)
        /// </remarks>
        /// <documentation>
        /// This approach was used in APSIM-SoilN module, and has been adapted from CERES. See Godwin, D.C. and Jones, C.A. (1991). Nitrogen dynamics in
        ///  soil-plant systems. In: Hanks, J. and Ritchie, J.T. Modeling plant and soil systems. pp. 287-321.
        /// This has not been tested especifically in APSIM
        /// </documentation>
        /// <param name="layer">the node number representing soil layer for which calculations will be made</param>
        /// <returns>delta N coverted from urea into NH4</returns>
        private double UreaHydrolysis_ApsimSoilN(int layer)
        {

            double result = 0.0;
            //if (_urea[layer] < 0.1)
            //    // urea amount is too small, all will be hydrolised
            //    result = _urea[layer];
            //else
            //{
            //    // get the index for aerobic/anaerobic conditions
            //    int index = (!is_pond_active) ? 1 : 2;

            //    // get the soil water factor
            //    double swf = Math.Max(0.0, Math.Min(1.0, WF(layer, index) + 0.20));

            //    // get the soil temperature factor
            //    double stf = Math.Max(0.0, Math.Min(1.0, (st[layer] / 40.0) + 0.20));

            //    // note (jngh) oc & ph are not updated during simulation
            //    //      mep    following equation would be better written using oc(layer) = (hum_C(layer) + biom_C(layer))

            //    // get potential fraction of urea for hydrolysis
            //    double ak = -1.12 + 1.31 * OC_reset[layer] + 0.203 * ph[layer] - 0.155 * OC_reset[layer] * ph[layer];
            //    ak = Math.Max(0.25, Math.Min(1.0, ak));

            //    //get amount hydrolysed;
            //    result = Math.Max(0.0, Math.Min(_urea[layer], ak * _urea[layer] * Math.Min(swf, stf)));
            //}

            return result;
        }

        /// <summary>
        /// + Purpose:
        ///     Compute the hydrolysis of urea using the approach from APSIM-SoilNitrogen
        /// </summary>
        /// <remarks>
        /// + Assumptions:
        ///     - parameters are given in pairs, for aerobic and anaerobic conditions (with pond)
        /// </remarks>
        /// <documentation>
        /// This approach is an updated version of original used in APSIM-SoilN module. Initially based on CERES, see Godwin, D.C. and Jones, C.A. (1991).
        ///  Nitrogen dynamics in soil-plant systems. In: Hanks, J. and Ritchie, J.T. Modeling plant and soil systems. pp. 287-321.
        /// Major differences include renaming some of the variables and allowing paramater values to be changed by user. Also organic carbon is updated
        ///  at each time step.
        /// This has not been tested especifically in APSIM
        /// </documentation>
        /// <param name="layer">the node number representing soil layer for which calculations will be made</param>
        /// <returns>delta N coverted from urea into NH4</returns>
        private double UreaHydrolysis_ApsimSoilNitrogen(int layer)
        {
            //// get the index for aerobic/anaerobic conditions
            //int index = (!is_pond_active) ? 1 : 2;

            //// get the soil water factor
            //double swf = Math.Max(0.0, Math.Min(1.0, WF(layer, index) + 0.20));
            //if (useNewSWFFunction)
            //    swf = SoilMoistFactor(layer, index, MoistFactor_Hydrol);

            //// get the soil temperature factor
            //double stf = Math.Max(0.0, Math.Min(1.0, (st[layer] / 40.0) + 0.20));
            //if (useNewSTFFunction)
            //    stf = SoilTempFactor(layer, index, TempFactor_Hydrol);

            //// get the total C amount
            //double totalC = OC_reset[layer];
            //if (useNewProcesses)
            //    totalC = hum_c[layer] + biom_c[layer] * convFactor_kgha2ppm(layer) / 10000;  // (100/1000000) = convert to ppm and then to %
            //// RCichota: why not FOM?

            //// get potential fraction of urea for hydrolysis
            //double ak = potHydrol_parmA +
            //        potHydrol_parmB * totalC +
            //        potHydrol_parmC * ph[layer] +
            //        potHydrol_parmD * totalC * ph[layer];
            //ak = Math.Max(potHydrol_min, Math.Min(1.0, ak));
            ////original eq.: double ak = Math.Max(0.25, Math.Min(1.0, -1.12 + 1.31 * OC_reset[layer] + 0.203 * ph[layer] - 0.155 * OC_reset[layer] * ph[layer]));

            ////get amount N hydrolysed;
            //double result = Math.Max(0.0, Math.Min(_urea[layer], ak * _urea[layer] * Math.Min(swf, stf)));
            //return result;
            return 0.0;
        }




    }
}
