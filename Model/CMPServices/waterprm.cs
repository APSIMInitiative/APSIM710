namespace CMPServices
{
    using System;
    using CMPServices;


    public enum ParticleSize { tcClay, tcSilt, tcFineSand, tcCoarseSand };
    /// <summary>
    /// Pedotransfer functions for the soil moisture characteristic
    /// </summary>
    public enum TPTMethod { ptfDefault, ptfSaxton };

    /// <summary>
    /// Parameter class for the GRAZPLAN soil moisture budget
    /// </summary>
    public class SoilWaterParams
    {
        private const double ROCKDENSITY = 2.65; // Mg/m^3    
        private const double PSI_LL15 = 15000.0;  // m suction 
        private const double PSI_DUL = 100.0;  // m suction 
        private const double MAX_EVAP_AWC = 30.0;  // mm               Maximum AWC and depth for computing   
        private const double MAX_EVAP_DEPTH = 300.0;  // mm            depth of soil evaporation            
        private const double DEF_ALBEDO = 0.17;


        ///<summary>Number of soil layers </summary>
        public int LastLayer;
        ///<summary>Depth of each soil layer (mm)</summary>
        public double[] Layer_MM = new double[GrazType.MaxSoilLayers + 1];      // [0] = surface
        ///<summary>Depth from surface to layer bottoms (mm)</summary>
        public double[] Bottom_MM = new double[GrazType.MaxSoilLayers + 1];     // [0] = surface
        ///<summary>Bulk density of each layer (g/cm3)</summary>
        public double[] BulkDensity = new double[GrazType.MaxSoilLayers + 1];   // [0] = surface    
        ///<summary>SW content at saturation (mm/mm)</summary>
        public double[] SW_SAT = new double[GrazType.MaxSoilLayers + 1];        // [0] = surface
        ///<summary>SW content at field capacity (mm/mm)</summary>
        public double[] SW_DUL = new double[GrazType.MaxSoilLayers + 1];        // [0] = surface
        ///<summary>SW content at wilting point (mm/mm)</summary>
        public double[] SW_WP = new double[GrazType.MaxSoilLayers + 1];         // [0] = surface
        ///<summary>Saturated hydraulic conductivities (mm/d)</summary>
        public double[] SatCond = new double[GrazType.MaxSoilLayers + 1];       // [0] = surface
        ///<summary>Particle Size Distribution</summary>
        public double[,] PSD = new double[GrazType.MaxSoilLayers, 4]; // layer, particle size
        ///<summary>Soil evaporation constant (mm/d^0.5)</summary>
        public double EvapAlpha;
        ///<summary>Last layer for soil evaporation</summary>
        public int LastSoilELayer;
        ///<summary>Soil albedo (0-1)</summary>
        public double Albedo;
        ///<summary>SCS runoff curve number</summary>
        public double CN_BareSoil;

        ///<summary>Rain intercepted /area index  (kg/m^2)</summary>
        public double InterceptCapacity;
        ///<summary>Melting rate for snow (mm/oC)</summary>
        public double SnowMeltRate;
        ///<summary>Temperature threshold for snowmelt (oC)</summary>
        public double SnowMeltBase;

        ///<summary>Conversion factor between kg/ha and ppm</summary>
        public double[] ConcFactor;

        ///<summary>Soil moisture at saturation (mm)</summary>
        public double[] SoilM_SAT = new double[GrazType.MaxSoilLayers + 1];     // [0] = surface
        ///<summary>Soil moisture at field capacity (mm)</summary>
        public double[] SoilM_DUL = new double[GrazType.MaxSoilLayers + 1];     // [0] = surface
        ///<summary>Soil moisture at wilting point (mm)</summary>
        public double[] SoilM_WP = new double[GrazType.MaxSoilLayers + 1];      // [0] = surface
        ///<summary>Weighting factors for retention in runoff</summary> 
        public double[] RetainWeight = new double[GrazType.MaxSoilLayers + 1];  // [0] = surface
        ///<summary>Total stage 1 soil evaporation (mm)</summary>
        public double EvapU;
        ///<summary>Factor for hydraulic conductivity (-)</summary>
        public double[] Beta = new double[GrazType.MaxSoilLayers + 1];          // [0] = surface


        public SoilWaterParams()
        {

        }

        public SoilWaterParams(SoilWaterParams srcParams)
        {
            LastLayer = srcParams.LastLayer;
            Layer_MM = srcParams.Layer_MM;
            BulkDensity = srcParams.BulkDensity;
            SW_SAT = srcParams.SW_SAT;
            SW_DUL = srcParams.SW_DUL;
            SW_WP = srcParams.SW_WP;
            SatCond = srcParams.SatCond;
            PSD = srcParams.PSD;
            EvapAlpha = srcParams.EvapAlpha;
            Albedo = srcParams.Albedo;
            CN_BareSoil = srcParams.CN_BareSoil;

            deriveParams();
            LastSoilELayer = srcParams.LastSoilELayer;
        }

        public double DefaultSAT(double BD)
        {
            return 0.93 * (1.0 - BD / ROCKDENSITY);
        }

        public double DefaultBD(double SAT)
        {
            return Math.Max(0.8, ROCKDENSITY * (1.0 - SAT / 0.93));
        }

        /// <summary>
        /// 
        /// </summary>
        /// <param name="iLayer">Layer array index 1-n</param>
        /// <returns></returns>
        public double fPorosity(int iLayer)
        {
            return 1.0 - BulkDensity[iLayer] / ROCKDENSITY;
        }

        /// <summary>
        /// 
        /// </summary>
        /// <param name="iLayer">Layer array index 1-n</param>
        /// <returns></returns>
        public double fAirDry(int iLayer)
        {
            return Math.Min(SW_WP[iLayer], 0.004 + 0.184 * SW_WP[iLayer]);
        }

        /// <summary>
        /// 
        /// </summary>
        /// <param name="iLayer">Layer array index 1-n</param>
        /// <returns></returns>
        public double fCampbellParam(int iLayer)
        {
            if (iLayer <= LastLayer)
                return Math.Log(PSI_LL15 / PSI_DUL) / Math.Log(SW_DUL[iLayer] / SW_WP[iLayer]);
            else
                return 0.0;
        }

        public double fDefaultSoilEDepth()
        {
            double fLayerAWC;
            double fLayerFract;
            double fCumulAWC;
            int L;

            fCumulAWC = 0.0;
            double Result = 0.0;
            for (L = 1; L <= LastLayer; L++)
            {
                fLayerAWC = SoilM_DUL[L] - SoilM_WP[L];
                if (fLayerAWC > 0.0)
                    fLayerFract = Math.Max(0.0, Math.Min((MAX_EVAP_AWC - fCumulAWC) / fLayerAWC, 1.0));
                else
                    fLayerFract = 1.0;
                fCumulAWC = fCumulAWC + fLayerAWC;
                Result = Result + fLayerFract * Layer_MM[L];
            }
            Result = Math.Min(10.0 * Math.Truncate(Result / 10.0 + 0.99999), MAX_EVAP_DEPTH);          // Round up to next 10 mm                
            return Result;
        }

        public void deriveParams()
        {
            double fSoilEDepth;
            double TotalWeight;
            int L;

            SnowMeltRate = 4.57;
            SnowMeltBase = 0.0;
            InterceptCapacity = 0.55;

            BulkDensity[GrazType.SURFACE] = 0.0;
            ConcFactor[GrazType.SURFACE] = 0.0;
            SoilM_SAT[GrazType.SURFACE] = 0.0;
            SoilM_DUL[GrazType.SURFACE] = 0.0;
            SoilM_WP[GrazType.SURFACE] = 0.0;
            Layer_MM[GrazType.SURFACE] = 0.0;

            if (Layer_MM[1] == 0.0)
                for (L = 1; L <= LastLayer; L++)
                {
                    Layer_MM[L] = Bottom_MM[L] - Bottom_MM[L - 1];
                }
            else
                for (L = 1; L <= LastLayer; L++)
                    Bottom_MM[L] = Bottom_MM[L - 1] + Layer_MM[L];

            if (BulkDensity[1] == 0.0)
                for (L = 1; L <= LastLayer; L++)
                    BulkDensity[L] = DefaultBD(SW_SAT[L]);
            else if (SW_SAT[1] == 0.0)
                for (L = 1; L <= LastLayer; L++)
                {
                    SW_SAT[L] = DefaultSAT(BulkDensity[L]);
                    if (SW_SAT[L] <= SW_DUL[L])
                        SW_SAT[L] = 0.5 * (SW_DUL[L] + fPorosity(L));
                }

            if (Albedo == 0.0)
                Albedo = DEF_ALBEDO;

            for (L = 1; L <= LastLayer; L++)
            {
                ConcFactor[L] = 100.0 / (Layer_MM[L] * BulkDensity[L]);                   // Conversion factor between kg/ha and   
                SoilM_SAT[L] = Layer_MM[L] * SW_SAT[L];                                  //  mg/kg                                
                SoilM_DUL[L] = Layer_MM[L] * SW_DUL[L];
                SoilM_WP[L] = Layer_MM[L] * SW_WP[L];
            }

            fSoilEDepth = fDefaultSoilEDepth();                                         // Find the last layer for soil          
            LastSoilELayer = 1;                                                         //   evaporation                         
            while ((LastSoilELayer < LastLayer) && (Bottom_MM[LastSoilELayer] < fSoilEDepth - GrazType.VerySmall))
                LastSoilELayer++;

            TotalWeight = 0.0;
            for (L = 1; L <= LastLayer; L++)
            {
                RetainWeight[L] = Math.Pow(0.01, Bottom_MM[L] / Bottom_MM[LastLayer]);
                TotalWeight = TotalWeight + RetainWeight[L];
            }
            for (L = 1; L <= LastLayer; L++)
                RetainWeight[L] = RetainWeight[L] / TotalWeight;

            EvapU = 9.0 * Math.Pow(Math.Max(EvapAlpha - 3.0, 0.0), 0.42);

            for (L = 1; L <= LastLayer; L++)
                Beta[L] = Math.Log(0.0022) / Math.Log(SoilM_DUL[L] / SoilM_SAT[L]);
        }
    }
}

