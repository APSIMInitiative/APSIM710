using System;
using System.Text;
using CMPServices;

namespace Uptake
{
    /// <summary>
    /// Plant information
    /// </summary>
    public class PlantUptake
    {
        /// <summary>Parameters of the uptake model </summary>
        public double[] UptakeK = new double[GrazType.MaxSoilLayers];       // LayerArray
        /// <summary>Root radius                 (m)</summary>
        public double[] RootRadius = new double[GrazType.MaxSoilLayers];    // LayerArray
        /// <summary>Root length density profile (m/m^3)</summary>
        public double[] RootLengthD = new double[GrazType.MaxSoilLayers];   // LayerArray
        /// <summary>Demand for water            (kg/m^2/d)</summary>
        public double Demand;                   
        /// <summary>Supply of water             (kg/m^2/d)</summary>
        public double[] Supply = new double[GrazType.MaxSoilLayers];        // LayerArray    
    }


    /// <summary>
    /// Water Allocator base class
    /// </summary>
    public class WaterAllocator
    {
        // Soil information -------------------------------------------------------
        /// <summary>Depth of each soil layer    (m)</summary>
        protected double[] FLayerDepth_M = new double[GrazType.MaxSoilLayers];
        protected int FNoLayers;
        // Plant information -------------------------------------------------------
        protected int FNoPlantParams;
        protected int FNoPlants;
        protected PlantUptake[] FPlantInfo = new PlantUptake[GrazType.MaxPlantSpp];
        // Space for results -------------------------------------------------------
        protected double[,] FWaterUptake = new double[GrazType.MaxPlantSpp, GrazType.MaxSoilLayers];
        protected double[,] FSoilFract = new double[GrazType.MaxPlantSpp, GrazType.MaxSoilLayers];

        public WaterAllocator()
        {

        }
        #region private and protected members
        private void checkPlantIndex(int Idx)
        {
            if (Idx > FNoPlants)   //Allocate the correct amount of storage
                throw new Exception("TWaterAllocator: invalid plant index = " + Idx.ToString());
            else if (Idx == FNoPlants)
                setNoPlants(Idx + 1);
        }

        /// <summary>
        /// Size structures for iValue number of plants
        /// </summary>
        /// <param name="iValue"></param>
        protected virtual void setNoPlants(int iValue)
        {
            FNoPlants = iValue;

            Array.Resize(ref FPlantInfo, FNoPlants);
            ExtMath.ResizeArray(FWaterUptake, FNoPlants, GrazType.MaxSoilLayers);
            ExtMath.ResizeArray(FSoilFract, FNoPlants, GrazType.MaxSoilLayers);
        }
        #endregion
        /// <summary>
        /// On construction, soil parameters required by the allocator are computed from 
        /// values stored by SoilParams.                                                 
        /// * Note that units (especially length units) have to be converted at times    
        /// </summary>
        /// <param name="SoilParams"></param>
        public WaterAllocator(SoilWaterParams SoilParams)
        {
            FNoLayers = SoilParams.LastLayer;
            for (int Ldx = 0; Ldx <= FNoLayers - 1; Ldx++)
            {
                FLayerDepth_M[Ldx] = 0.001 * SoilParams.Layer_MM[Ldx + 1];   // Convert layer depth from mm to m     
            }
        }

        /// <summary>
        /// Zeroes the plant information in case an index is left unused
        /// </summary>
        public virtual void initTimeStep()
        {
            FPlantInfo = new PlantUptake[GrazType.MaxPlantSpp];
            for (int Idx = 0; Idx <= FNoPlants - 1; Idx++)
            {
                FPlantInfo[Idx] = new PlantUptake();
            }
            FWaterUptake = new double[GrazType.MaxPlantSpp, GrazType.MaxSoilLayers];
            FSoilFract = new double[GrazType.MaxPlantSpp, GrazType.MaxSoilLayers];
        }
        /// <summary>
        /// Populate the allocator with information about each plant species (or cohort).
        /// These routines should be called for each "species" in each time step before  
        /// computing the water uptake rate.                                             
        /// </summary>
        /// <param name="Idx">Index number for this "species"</param>
        /// <param name="UptakeParams">Parameters of the q(r) function</param>
        public virtual void setPlantParams(int Idx, double[] UptakeParams)
        {
            checkPlantIndex(Idx);
            for (int Jdx = 0; Jdx <= FNoPlantParams - 1; Jdx++)
                FPlantInfo[Idx].UptakeK[Jdx] = UptakeParams[Jdx];
        }
        /// <summary>
        /// Populate the allocator with information about each plant species (or cohort).
        /// These routines should be called for each "species" in each time step before  
        /// computing the water uptake rate.                                             
        /// </summary>
        /// <param name="Idx">Index number for this "species"</param>
        /// <param name="WaterDemand">Potential water uptake kg/m^2/d = mm/d</param>
        public virtual void setPlantDemand(int Idx, double WaterDemand)
        {
            checkPlantIndex(Idx);
            if (FPlantInfo[Idx] == null)
                FPlantInfo[Idx] = new PlantUptake();
            FPlantInfo[Idx].Demand = WaterDemand;       //WaterDemand is already in kg/m^2/d 
        }

        /// <summary>
        /// Populate the allocator with information about each plant species (or cohort).
        /// These routines should be called for each "species" in each time step before  
        /// computing the water uptake rate.                                             
        /// </summary>
        /// <param name="Idx">Index number for this "species"</param>
        /// <param name="Supply">Potential water supply kg/m^2/d = mm/d</param>
        public virtual void setPlantSupply(int Idx, double[] Supply)
        {
            checkPlantIndex(Idx);
            FPlantInfo[Idx].Supply = Supply;
        }
        /// <summary>
        /// Populate the allocator with information about each plant species (or cohort).
        /// These routines should be called for each "species" in each time step before  
        /// computing the water uptake rate.                                             
        /// </summary>
        /// <param name="Idx">Index number for this "species"</param>
        /// <param name="RLD">Root length density in each layer    m/m^3</param>
        public virtual void setPlantRLD(int Idx, double[] RLD)
        {
            checkPlantIndex(Idx);
            FPlantInfo[Idx].RootLengthD = RLD;                                          // RLD is already in m/m^3               
        }

        /// <summary>
        /// Populate the allocator with information about each plant species (or cohort).
        /// These routines should be called for each "species" in each time step before  
        /// computing the water uptake rate.                                             
        /// </summary>
        /// <param name="Idx">Index number for this "species"</param>
        /// <param name="RootRadius">Root radius in each layer  m</param>
        public virtual void setPlantRootR(int Idx, double[] RootRadius)
        {
            checkPlantIndex(Idx);
            FPlantInfo[Idx].RootRadius = RootRadius;
        }

        /// <summary>
        /// -
        /// </summary>
        /// <param name="Idx">Index denoting a species or cohort</param>
        /// <param name="UptakeParams">Parameters of the q(r) function</param>
        /// <param name="RootRadius">Root radius profile         (mm)</param>
        /// <param name="RLD">Root length density profile (m/m^3)</param>
        /// <param name="WaterDemand">Total water demand          (mm/d)</param>
        public virtual void setPlantInfo(int Idx, double[] UptakeParams, double[] RootRadius, double[] RLD, double WaterDemand)
        {
            if (Idx > FNoPlants)                                                        // Allocate the correct amount of storage
                throw new Exception("TWaterAllocator: invalid plant index = " + Idx.ToString());
            else if (Idx == FNoPlants)
                setNoPlants(Idx + 1);

            for (int Jdx = 0; Jdx <= FNoPlantParams - 1; Jdx++)
                FPlantInfo[Idx].UptakeK[Jdx] = UptakeParams[Jdx];
            for (int Ldx = 0; Ldx <= FNoLayers - 1; Ldx++)                              // Convert root radius from mm to m      
                FPlantInfo[Idx].RootRadius[Ldx] = RootRadius[Ldx];
            FPlantInfo[Idx].RootLengthD = RLD;                                          // RLD is already in m/m^3               
            FPlantInfo[Idx].Demand = WaterDemand;                                       // WaterDemand is already in kg/m^2/d    
        }

        /// <summary>
        /// Main logic routine for the water uptake calulations                          
        /// Assumption:                                                                  
        /// Plant-specific information for this time step has been provided through    
        ///   calls to setPlantInfo().                                                   
        /// </summary>
        /// <param name="PotentialET">Potential evaporation rate  mm/d</param>
        /// <param name="Theta">Volumetric water content    (v/v)</param>
        public virtual void computeWaterUptake(double PotentialET, double[] Theta)
        {

        }
        /// <summary>
        /// Provides the computed water uptake rate for a given "species".
        /// </summary>
        /// <param name="Idx">Index denoting a species or cohort</param>
        /// <param name="fUptake">Water supply rate profile   (mm/d)</param>
        public virtual void returnWaterUptake(int Idx, ref double[] fUptake)
        {
            for (int i = 0; i <= FNoLayers - 1; i++)
                fUptake[i] = FWaterUptake[Idx, i];
        }

        /// <summary>
        /// Provides the soil fraction for nutrient uptake for a given "species".
        /// </summary>
        /// <param name="Idx">Index denoting a species or cohort</param>
        /// <param name="fSoilPropn">Fraction of soil volume     (v/v)</param>
        public virtual void returnSoilPropn(int Idx, ref double[] fSoilPropn)
        {
            for (int i = 0; i <= FNoLayers - 1; i++)
                fSoilPropn[i] = FSoilFract[Idx, i];
        }
    }

    public struct TWaterParam
    {
        public double Q_max;
        public double Psi_max;
        public double Psi_zero;
        public double Curvature;
        public double PiRLD;
        public double RootRadius;
    }
    /// <summary>
    /// New water uptake model
    /// </summary>
    public class WaterAllocator2 : WaterAllocator
    {
        private const double GRAVITY = 9.801;                       // Acceleration due to gravity    m/s^2
        private const double DENSITY = 1.0;                         // Density of water               Mg/m^3
        private const double KPA_2_M = -1.0 / (GRAVITY * DENSITY);  // Converts kPa to m suction
        private const double PSI_WP = -1500;                        // Water potential at wilting pt  kPa

        // Soil information --------------------------------------------------------
        /// <summary>
        /// Water content at air-dry    (v/v)     
        /// </summary>
        private double[] FTheta_DRY = new double[GrazType.MaxSoilLayers];    // LayerArray
        /// <summary>
        /// Water content at saturation (v/v)     
        /// </summary>
        private double[] FTheta_SAT = new double[GrazType.MaxSoilLayers];    // LayerArray
        /// <summary>
        /// Air entry potential         (m)       
        /// </summary>
        private double[] FPsi_Entry = new double[GrazType.MaxSoilLayers];    // LayerArray
        /// <summary>
        /// Exponent in theta-psi fn              
        /// </summary>
        private double[] FPsi_B = new double[GrazType.MaxSoilLayers];        // LayerArray
        /// <summary>
        /// K when psi=1 kPa            (kg/m^2/d)
        /// </summary>
        private double[] FCond_1KPA = new double[GrazType.MaxSoilLayers];    // LayerArray

        // Space for computations --------------------------------------------------
        private TWaterParam[] FParams;
        // Space for computations --------------------------------------------------
        protected double[,] FWaterSupply = new double[GrazType.MaxPlantSpp, GrazType.MaxSoilLayers];
        // Soil information --------------------------------------------------------
        /// <summary>
        /// Exponent in psi-K function            
        /// </summary>
        protected double[] FCond_N = new double[GrazType.MaxSoilLayers];

        public WaterAllocator2()
        {

        }

        public WaterAllocator2(SoilWaterParams SoilParams)
            : base(SoilParams)
        {
            FNoPlantParams = 4;

            checkLayerInfo(SoilParams);

            for (int Ldx = 0; Ldx <= FNoLayers - 1; Ldx++)
            {
                FTheta_DRY[Ldx] = SoilParams.fAirDry(Ldx + 1);
                FTheta_SAT[Ldx] = SoilParams.SW_SAT[Ldx + 1];
                FPsi_B[Ldx] = SoilParams.fCampbellParam(Ldx + 1);
                FCond_N[Ldx] = SoilParams.Beta[Ldx + 1] / FPsi_B[Ldx];
                FPsi_Entry[Ldx] = PSI_WP * Math.Pow(SoilParams.SW_WP[Ldx + 1] / SoilParams.SW_SAT[Ldx + 1],
                                                        FPsi_B[Ldx]);                       // Air entry potential (kPa)             
                FCond_1KPA[Ldx] = SoilParams.SatCond[Ldx + 1] * Math.Pow(-1.0 / FPsi_Entry[Ldx], -FCond_N[Ldx]);
            }
        }
        #region private and protected members

        /// <summary>
        /// Restricts the total uptake rate so that soil water is not reduced below      
        /// air-dry content 
        /// </summary>
        /// <param name="fTheta">-</param>
        private void limitWaterUptake(double[] fTheta)
        {
            double dMaxLayerUptake;
            double dLayerUptake;
            int Idx, Ldx;

            for (Ldx = 0; Ldx <= FNoLayers - 1; Ldx++)                                                 // Uptake cannot reduce water content below air-dry content               
            {
                dMaxLayerUptake = 1000.0 * FLayerDepth_M[Ldx] * Math.Max(0.0, fTheta[Ldx] - FTheta_DRY[Ldx]);
                dLayerUptake = 0.0;
                for (Idx = 0; Idx <= FNoPlants - 1; Idx++)
                    dLayerUptake = dLayerUptake + FWaterUptake[Idx, Ldx];

                if (dLayerUptake > dMaxLayerUptake)
                {
                    for (Idx = 0; Idx <= FNoPlants - 1; Idx++)
                        FWaterUptake[Idx, Ldx] = FWaterUptake[Idx, Ldx] * (dMaxLayerUptake / dLayerUptake);
                }
            }
        }

        /// <summary>
        /// Psi, Psi_max and Psi_zero are all in kPa and are negative                
        /// </summary>
        /// <param name="Kdx"></param>
        /// <param name="_Psi"></param>
        /// <param name="Qr"></param>
        /// <param name="dQr_dPsi"></param>
        private void ComputeQr(int Kdx, double _Psi, ref double Qr, ref double dQr_dPsi)
        {
            if (_Psi >= FParams[Kdx].Psi_max)
            {
                Qr = FParams[Kdx].Q_max;
                dQr_dPsi = 0.0;
            }
            else if (_Psi <= FParams[Kdx].Psi_zero)
            {
                Qr = 0.0;
                dQr_dPsi = 0.0;
            }
            else
            {
                Qr = FParams[Kdx].Q_max * Math.Pow(_Psi / FParams[Kdx].Psi_max, -FParams[Kdx].Curvature) * (_Psi - FParams[Kdx].Psi_zero) / (FParams[Kdx].Psi_max - FParams[Kdx].Psi_zero);
                dQr_dPsi = -Qr * (FParams[Kdx].Curvature / _Psi + 1.0 / (FParams[Kdx].Psi_zero - _Psi));
            }
        }

        /// <summary>
        /// Campbell water potential function
        /// </summary>
        /// <param name="dTheta"></param>
        /// <param name="Ldx"></param>
        /// <returns></returns>
        protected double Psi(double dTheta, int Ldx)
        {
            return FPsi_Entry[Ldx] * Math.Pow(dTheta / FTheta_SAT[Ldx], -FPsi_B[Ldx]);
        }

        /// <summary>
        /// Campbell conductivity function
        /// </summary>
        /// <param name="fPsi"></param>
        /// <param name="Ldx"></param>
        /// <returns></returns>
        protected double Conductivity(double fPsi, int Ldx)
        {
            return FCond_1KPA[Ldx] * Math.Pow(Math.Abs(fPsi), -FCond_N[Ldx]);
        }

        /// <summary>
        /// Computes the actual uptake rate once the water supply rates are known
        /// </summary>
        /// <param name="Idx">-</param>
        protected virtual void computePlantUptake(int Idx)
        {
            double dProfileSupply;
            double dDayFract;
            double dProfileUptake;
            int Ldx;

            dProfileSupply = 0.0;
            for (Ldx = 0; Ldx <= FNoLayers - 1; Ldx++)
                dProfileSupply = dProfileSupply + FWaterSupply[Idx, Ldx];

            if (dProfileSupply < Math.PI / 2.0 * FPlantInfo[Idx].Demand)               // Assume a sinusoidal pattern of water  
            {                                                                          //   demand through the day              
                dDayFract = 2.0 / Math.PI * Math.Asin(2.0 / Math.PI * dProfileSupply / FPlantInfo[Idx].Demand);
                dProfileUptake = (1.0 - Math.Cos(Math.PI / 2.0 * dDayFract)) * FPlantInfo[Idx].Demand
                                + (1.0 - dDayFract) * dProfileSupply;
            }
            else
                dProfileUptake = FPlantInfo[Idx].Demand;

            for (Ldx = 0; Ldx <= FNoLayers - 1; Ldx++)                                 // Weight the final total uptake in      
            {
                if (dProfileSupply > 0)                                                //   proportion to supply rate           
                    FWaterUptake[Idx, Ldx] = dProfileUptake * (FWaterSupply[Idx, Ldx] / dProfileSupply);
                else
                    FWaterUptake[Idx, Ldx] = 0.0;
            }
        }

        /// <summary>
        /// Size structures for iValue number of plants
        /// </summary>
        /// <param name="iValue"></param>
        protected override void setNoPlants(int iValue)
        {
            base.setNoPlants(iValue);

            Array.Resize(ref FParams, iValue);
            ExtMath.ResizeArray(FWaterSupply, iValue, GrazType.MaxSoilLayers);

        }

        #endregion

        /// <summary>
        /// -
        /// </summary>
        /// <param name="Ldx">-</param>
        /// <param name="fTheta">-</param>
        /// <param name="fPotentialET">-</param>
        public void computeWaterSupply(int Ldx, double fTheta, double fPotentialET)
        {
            //const double BOUND_APPROACH = 0.95;
            double Psi_s;

            Boolean bWetEnough;
            double[,] Alpha;
            double[] Beta;
            double[] Deltas;
            int iLayerPlants;
            int[] iPlantMap = new int[99];

            double N;
            double d2PiOnRhoG;
            double dK_Psi_Soil;
            double dK_Surface;

            double[] SurfacePsi = new double[99];
            double[] dPrev_Psi = new double[99];
            double dMaxError;
            //double dPrev_Error;

            double[] Qr = new double[99];
            double[] dQr_dPsi = new double[99];
            double[] Qs = new double[99];
            double[] dExtractRadius = new double[99];

            int iIterations;
            double dQ2Radius;
            double dLnTerm;
            Boolean bDone;
            double[] dDeltaScale = new double[99];
            //Boolean bFirstTime;
            Boolean bRadiusOK;
            int Idx, Kdx, Ndx;


            Psi_s = Psi(fTheta, Ldx);                                                 // Water potential in this soil layer    
            // units of kPa, negative              
            iLayerPlants = 0;
            for (Idx = 0; Idx <= FNoPlants - 1; Idx++)
            {
                if (fPotentialET > 0.0)
                    bWetEnough = (Psi_s > FPlantInfo[Idx].UptakeK[3] / fPotentialET);
                else
                    bWetEnough = true;

                if (bWetEnough && (FPlantInfo[Idx].RootLengthD[Ldx] > 0.0))
                {
                    iPlantMap[iLayerPlants] = Idx;
                    iLayerPlants++;
                }
            }

            if (iLayerPlants > 0)                                                    // Only run the water uptake model if    
            {                                                                        //   uptake will be non-negative         
                N = FCond_N[Ldx];
                d2PiOnRhoG = (2.0 * Math.PI) / (GRAVITY * DENSITY);
                dK_Psi_Soil = Conductivity(Psi_s, Ldx) * Psi_s;

                for (Kdx = 0; Kdx <= iLayerPlants - 1; Kdx++)
                {
                    Idx = iPlantMap[Kdx];
                    FParams[Kdx].Q_max = FPlantInfo[Idx].UptakeK[0]                               // Convert max. uptake/root area to max. 
                                               * 2.0 * Math.PI * FPlantInfo[Idx].RootRadius[Ldx];            //   uptake/root length                  
                    FParams[Kdx].Psi_max = FPlantInfo[Idx].UptakeK[1];
                    FParams[Kdx].Curvature = FPlantInfo[Idx].UptakeK[2];
                    FParams[Kdx].Psi_zero = FPlantInfo[Idx].UptakeK[3] / Math.Max(fPotentialET, 0.00001);
                    FParams[Kdx].PiRLD = Math.PI * FPlantInfo[Idx].RootLengthD[Ldx];
                    FParams[Kdx].RootRadius = FPlantInfo[Idx].RootRadius[Ldx];
                }
                Alpha = new double[iLayerPlants, iLayerPlants];
                Beta = new double[iLayerPlants];
                Deltas = new double[iLayerPlants];
                iIterations = 0;

                for (Kdx = 0; Kdx <= iLayerPlants - 1; Kdx++)                                           // Initial guesses for SurfacePsi        
                    SurfacePsi[Kdx] = Psi_s;
                // Multi-dimensional Newton-Raphson      
                do                                                                     //  procedure, with modifications        
                {
                    dQ2Radius = 0.0;                                                        // Use Q2Radius to compute the current   
                    for (Kdx = 0; Kdx <= iLayerPlants - 1; Kdx++)                                         //    r(s) values                        
                    {
                        ComputeQr(Kdx, SurfacePsi[Kdx], ref Qr[Kdx], ref dQr_dPsi[Kdx]);
                        dQ2Radius = dQ2Radius + FParams[Kdx].PiRLD * Math.Pow(Qr[Kdx], 2);
                    }
                    dQ2Radius = Math.Sqrt(dQ2Radius);

                    for (Kdx = 0; Kdx <= iLayerPlants - 1; Kdx++)                                         // We need all the r(s) values before    
                    {
                        if (Qr[Kdx] > 0.0)                                                 //   computing any element of Alpha      
                            dExtractRadius[Kdx] = Qr[Kdx] / dQ2Radius;
                        else
                            dExtractRadius[Kdx] = FParams[Kdx].RootRadius;
                    }

                    for (Kdx = 0; Kdx <= iLayerPlants - 1; Kdx++)
                    {
                        dK_Surface = Conductivity(SurfacePsi[Kdx], Ldx);
                        dLnTerm = Math.Log(dExtractRadius[Kdx] / FParams[Kdx].RootRadius);
                        Qs[Kdx] = d2PiOnRhoG / (N - 1.0) * (dK_Surface * SurfacePsi[Kdx] - dK_Psi_Soil) / dLnTerm;

                        for (Ndx = 0; Ndx <= iLayerPlants - 1; Ndx++)
                        {
                            Alpha[Kdx, Ndx] = FParams[Kdx].PiRLD * dExtractRadius[Kdx] * dExtractRadius[Ndx] * dQr_dPsi[Ndx];
                            if (Kdx == Ndx)
                                Alpha[Kdx, Ndx] = Alpha[Kdx, Ndx] - d2PiOnRhoG * dK_Surface - (dLnTerm + 1.0) * dQr_dPsi[Kdx];
                        }
                        Beta[Kdx] = (Qr[Kdx] - Qs[Kdx]) * dLnTerm;
                    }

                    dMaxError = 0.0;
                    for (Kdx = 0; Kdx <= iLayerPlants - 1; Kdx++)
                        dMaxError = Math.Max(dMaxError, Math.Abs(Qr[Kdx] - Qs[Kdx]) / FParams[Kdx].Q_max);
                    bDone = (dMaxError <= 1.0E-6);

                    if (!bDone)
                    {
                        for (Kdx = 0; Kdx <= iLayerPlants - 1; Kdx++)
                            dPrev_Psi[Kdx] = SurfacePsi[Kdx];

                        ExtMath.SolveLinear(Alpha, ref Deltas, Beta);                                    // Here is the Newton-Raphson step that  
                        //   computes the deltas                 
                        for (Kdx = 0; Kdx <= iLayerPlants - 1; Kdx++)
                            dDeltaScale[Kdx] = 1.0;                                             // Enforce approach from above, i.e.     
                        do                                                                 //   keep the SurfacePsi values between  
                        {
                            for (Kdx = 0; Kdx <= iLayerPlants - 1; Kdx++)                                     //   Psi_s and the solution              
                                SurfacePsi[Kdx] = dPrev_Psi[Kdx] + dDeltaScale[Kdx] * Deltas[Kdx];

                            dQ2Radius = 0.0;
                            for (Kdx = 0; Kdx <= iLayerPlants - 1; Kdx++)
                            {
                                ComputeQr(Kdx, SurfacePsi[Kdx], ref Qr[Kdx], ref dQr_dPsi[Kdx]);
                                dQ2Radius = dQ2Radius + FParams[Kdx].PiRLD * Math.Pow(Qr[Kdx], 2);
                            }
                            dQ2Radius = Math.Sqrt(dQ2Radius);

                            bRadiusOK = true;
                            for (Kdx = 0; Kdx <= iLayerPlants - 1; Kdx++)
                            {
                                if (dQ2Radius > 0.0)
                                    dExtractRadius[Kdx] = Qr[Kdx] / dQ2Radius;
                                else
                                    dExtractRadius[Kdx] = 0.0;

                                if (dExtractRadius[Kdx] < FParams[Kdx].RootRadius)
                                {
                                    bRadiusOK = false;
                                    dDeltaScale[Kdx] = 0.5 * dDeltaScale[Kdx];
                                }
                                else
                                {
                                    dK_Surface = Conductivity(SurfacePsi[Kdx], Ldx);
                                    dLnTerm = Math.Log(dExtractRadius[Kdx] / FParams[Kdx].RootRadius);
                                    Qs[Kdx] = d2PiOnRhoG / (N - 1.0) * (dK_Surface * SurfacePsi[Kdx] - dK_Psi_Soil) / dLnTerm;
                                    if ((Qr[Kdx] - Qs[Kdx]) / FParams[Kdx].Q_max < -1.0E-6)
                                    {
                                        bRadiusOK = false;
                                        dDeltaScale[Kdx] = 0.5 * dDeltaScale[Kdx];
                                    }
                                }

                                if (dDeltaScale[Kdx] < 1.0E-8)
                                    throw new Exception("Numerical failure in water uptake model");
                            }
                        }
                        while (!bRadiusOK);
                    } //_ if not bDone

                    iIterations++;
                    if (iIterations > 10000)
                        throw new Exception("Numerical failure in water uptake model");
                }
                while (!bDone);

                //commented code removed

                for (Kdx = 0; Kdx <= iLayerPlants - 1; Kdx++)                                // Compute the final water supply rate   
                {                                                                      //   from this layer for each species    
                    Idx = iPlantMap[Kdx];
                    FWaterSupply[Idx, Ldx] = Qr[Kdx]
                                             * FPlantInfo[Idx].RootLengthD[Ldx]
                                             * FLayerDepth_M[Ldx];
                    FSoilFract[Idx, Ldx] = Math.PI * FPlantInfo[Idx].RootLengthD[Ldx] * Math.Pow(dExtractRadius[Kdx], 2);
                }
            } //_ if iLayerPlants > 0 
        }



        /// <summary>
        /// -
        /// </summary>
        /// <param name="SoilParams">-</param>
        protected virtual void checkLayerInfo(SoilWaterParams SoilParams)
        {
            int Ldx;

            for (Ldx = 0; Ldx <= FNoLayers - 1; Ldx++)
            {
                if (SoilParams.SW_WP[Ldx + 1] >= SoilParams.SW_DUL[Ldx + 1])
                    throw new Exception("DUL must be strictly greater than WP in layer " + Ldx.ToString());
                else if (SoilParams.SatCond[Ldx + 1] <= 0.0)
                    throw new Exception("Saturated conductivity must be positive in layer " + Ldx.ToString());
                else if (SoilParams.SatCond[Ldx + 1] * Math.Pow(-1.0 / FPsi_Entry[Ldx], -FCond_N[Ldx]) < 1.0E-10)
                    throw new Exception("DUL and WP are too close together in layer " + Ldx.ToString());
            }
        }

        /// <summary>
        /// Zeroes the plant information in case an index is left unused
        /// </summary>
        public override void initTimeStep()
        {
            base.initTimeStep();
            FWaterSupply = new double[FNoPlants, GrazType.MaxSoilLayers];
        }

        /// <summary>
        /// Main logic routine for the water uptake calulations                          
        /// Assumption:                                                                  
        /// Plant-specific information for this time step has been provided through    
        ///   calls to setPlantInfo().                                                   
        /// </summary>
        /// <param name="PotentialET">Potential evaporation rate  mm/d</param>
        /// <param name="Theta">Volumetric water content    (v/v)</param>
        public override void computeWaterUptake(double PotentialET, double[] Theta)
        {
            int Idx, Ldx;

            for (Ldx = 0; Ldx <= FNoLayers - 1; Ldx++)                                                  // Compute water supply rates layer      
            {
                if (Theta[Ldx] > 0.0)                                                  //   by layer                            
                    computeWaterSupply(Ldx, Theta[Ldx], PotentialET);
            }

            for (Idx = 0; Idx <= FNoPlants - 1; Idx++)                                   // Now balance supply and demand         
                computePlantUptake(Idx);

            limitWaterUptake(Theta);
        }
    }

    public class TRoot
    {
        public double[] fLL;            // LayerArray
        public double[] fKL;            // LayerArray
        public double[] fPsi_min;       // LayerArray
        public double[] fPsi_max;       // LayerArray
        public double[] fGamma;         // LayerArray
        public double[] fLambda;        // LayerArray

        public double fRel_Psi;
        public double[] fMonoSupply;    // LayerArray
        public double[] fQ;             // LayerArray
        public double fPrev_Rel_Psi;
        public double[] fPrev_Fract;    // LayerArray    

        public TRoot()
        {
            fLL = new double[GrazType.MaxSoilLayers];            // LayerArray
            fKL = new double[GrazType.MaxSoilLayers];            // LayerArray
            fPsi_min = new double[GrazType.MaxSoilLayers];       // LayerArray
            fPsi_max = new double[GrazType.MaxSoilLayers];       // LayerArray
            fGamma = new double[GrazType.MaxSoilLayers];         // LayerArray
            fLambda = new double[GrazType.MaxSoilLayers];        // LayerArray
            fMonoSupply = new double[GrazType.MaxSoilLayers];    // LayerArray
            fQ = new double[GrazType.MaxSoilLayers];             // LayerArray
            fPrev_Fract = new double[GrazType.MaxSoilLayers];    // LayerArray    
        }
    }

    /// <summary>
    /// Demand vs supply allocator
    /// </summary>
    public class WaterAllocator3 : WaterAllocator2
    {
        private TRoot[] FRootData = new TRoot[GrazType.MaxPlantSpp];

        public WaterAllocator3(SoilWaterParams SoilParams)
            : base(SoilParams)
        {
            FNoPlantParams = 0;
        }

        #region Private and protected members

        /// <summary>
        /// -
        /// </summary>
        /// <param name="Q1">-</param>
        /// <param name="Q2">-</param>
        /// <returns>-</returns>
        private double fSpaceProportion(double Q1, double Q2)
        {
            double dRatio;
            double dBoundary;
            double Result = 0;

            if (Q1 == 0.0)
                Result = 0.0;
            else if (Q2 == 0.0)
                Result = 1.0;
            else
            {
                dRatio = Q1 / Q2;
                if (Math.Abs(dRatio - 1.0) < 1E-7)
                    Result = 0.5;
                else
                {
                    dBoundary = (Math.Sqrt(Math.Pow(dRatio, 2) - dRatio + 1.0) - 1.0) / (dRatio - 1.0);
                    if (dBoundary <= 0.5)
                        Result = 2.0 * Math.Pow(dBoundary, 2);
                    else
                        Result = 1.0 - 2.0 * Math.Pow(1.0 - dBoundary, 2);
                }
            }
            return Result;
        }

        /// <summary>
        /// -
        /// </summary>
        /// <param name="Theta">-</param>
        private void computeSoilFraction(double[] Theta)
        {
            double[] dPsi_Soil = new double[GrazType.MaxSoilLayers];
            double dPsiMinFactor;
            double dSupplySum;
            double dPsi_Root;
            double dSlope;
            double fDeltaRelPsi;
            Boolean bConverged;
            double dSum;
            int iIterCount;
            int Idx, Jdx, Ldx;

            // Preliminary computations
            for (Ldx = 0; Ldx <= FNoLayers - 1; Ldx++)
                dPsi_Soil[Ldx] = Psi(Theta[Ldx], Ldx);

            for (Idx = 0; Idx <= FNoPlants - 1; Idx++)
            {
                if (FPlantInfo[Idx].Demand > 0.0)
                {
                    dSupplySum = 0.0;
                    for (Ldx = 0; Ldx <= FNoLayers - 1; Ldx++)
                        dSupplySum = dSupplySum + FPlantInfo[Idx].Supply[Ldx];

                    for (Ldx = 0; Ldx <= FNoLayers - 1; Ldx++)
                    {
                        if (FPlantInfo[Idx].Supply[Ldx] > 0.0)
                        {
                            FRootData[Idx].fPsi_min[Ldx] = Psi(FRootData[Idx].fLL[Ldx], Ldx);
                            dPsiMinFactor = (1.0 - Math.Pow(FRootData[Idx].fPsi_min[Ldx] / dPsi_Soil[Ldx], 1.0 - FCond_N[Ldx]));

                            if (dSupplySum <= FPlantInfo[Idx].Demand)
                                FRootData[Idx].fPsi_max[Ldx] = FRootData[Idx].fPsi_min[Ldx];
                            else
                                FRootData[Idx].fPsi_max[Ldx] = dPsi_Soil[Ldx] * Math.Pow(1.0 - FPlantInfo[Idx].Demand / dSupplySum * dPsiMinFactor,
                                                                                        1.0 / (1.0 - FCond_N[Ldx]));
                            if (FRootData[Idx].fPsi_min[Ldx] < dPsi_Soil[Ldx])
                                FRootData[Idx].fGamma[Ldx] = FPlantInfo[Idx].Supply[Ldx] / dPsiMinFactor;
                            else
                                FRootData[Idx].fGamma[Ldx] = 0.0;
                        }
                        else
                        {
                            FRootData[Idx].fPsi_min[Ldx] = dPsi_Soil[Ldx];
                            FRootData[Idx].fPsi_max[Ldx] = dPsi_Soil[Ldx];
                            FRootData[Idx].fGamma[Ldx] = 0.0;
                        }
                    }
                }
                else
                {
                    for (Ldx = 0; Ldx <= FNoLayers - 1; Ldx++)
                    {
                        FRootData[Idx].fPsi_min[Ldx] = 0.0;
                        FRootData[Idx].fPsi_max[Ldx] = 0.0;
                        FRootData[Idx].fGamma[Ldx] = 0.0;
                    }
                }
            }

            for (Ldx = 0; Ldx <= FNoLayers - 1; Ldx++)
            {
                dSum = 0.0;
                for (Idx = 0; Idx <= FNoPlants - 1; Idx++)
                {
                    if (FPlantInfo[Idx].Supply[Ldx] > 0.0)
                        dSum = dSum + FRootData[Idx].fKL[Ldx];
                }

                for (Idx = 0; Idx <= FNoPlants - 1; Idx++)
                {
                    if ((dSum > 0.0) && (FPlantInfo[Idx].Supply[Ldx] > 0.0))
                        FRootData[Idx].fLambda[Ldx] = FRootData[Idx].fKL[Ldx] / dSum;
                    else
                        FRootData[Idx].fLambda[Ldx] = 0.0;
                }
            }

            // Initial values
            for (Idx = 0; Idx <= FNoPlants - 1; Idx++)
            {
                FRootData[Idx].fRel_Psi = 0.0;
                for (Ldx = 0; Ldx <= FNoLayers - 1; Ldx++)
                    FSoilFract[Idx, Ldx] = FRootData[Idx].fLambda[Ldx];
            }

            iIterCount = 0;
            do
            {
                // store previous values for convergence testing
                for (Idx = 0; Idx <= FNoPlants - 1; Idx++)
                {
                    FRootData[Idx].fPrev_Rel_Psi = FRootData[Idx].fRel_Psi;
                    for (Ldx = 0; Ldx <= FNoLayers - 1; Ldx++)
                        FRootData[Idx].fPrev_Fract[Ldx] = FSoilFract[Idx, Ldx];
                }

                // pre-compute expensive calculation
                for (Idx = 0; Idx <= FNoPlants - 1; Idx++)
                {
                    for (Ldx = 0; Ldx <= FNoLayers - 1; Ldx++)
                    {
                        dPsi_Root = FRootData[Idx].fPsi_min[Ldx] + FRootData[Idx].fRel_Psi * (FRootData[Idx].fPsi_max[Ldx] - FRootData[Idx].fPsi_min[Ldx]);
                        if (dPsi_Root < dPsi_Soil[Ldx])
                            FRootData[Idx].fMonoSupply[Ldx] = FRootData[Idx].fGamma[Ldx]
                                                               * (1.0 - Math.Pow(dPsi_Root / dPsi_Soil[Ldx], 1.0 - FCond_N[Ldx]));
                        else
                            FRootData[Idx].fMonoSupply[Ldx] = 0.0;
                    }
                }

                // update the P(i,k)
                for (Idx = 0; Idx <= FNoPlants - 1; Idx++)
                {
                    dSupplySum = 0.0;
                    for (Ldx = 0; Ldx <= FNoLayers - 1; Ldx++)
                    {
                        FWaterSupply[Idx, Ldx] = FSoilFract[Idx, Ldx] * FRootData[Idx].fMonoSupply[Ldx];
                        dSupplySum = dSupplySum + FWaterSupply[Idx, Ldx];
                    }

                    for (Ldx = 0; Ldx <= FNoLayers - 1; Ldx++)
                    {
                        if ((FRootData[Idx].fLambda[Ldx] == 0.0) || (dSupplySum == 0.0))
                            FRootData[Idx].fQ[Ldx] = 0.0;
                        else if (dSupplySum < FPlantInfo[Idx].Demand)
                            FRootData[Idx].fQ[Ldx] = FWaterSupply[Idx, Ldx] / FRootData[Idx].fLambda[Ldx];
                        else
                            FRootData[Idx].fQ[Ldx] = FPlantInfo[Idx].Demand
                                       * (FWaterSupply[Idx, Ldx] / dSupplySum) / FRootData[Idx].fLambda[Ldx];
                    }
                } //_ for Idx 

                for (Ldx = 0; Ldx <= FNoLayers - 1; Ldx++)
                {
                    for (Idx = 0; Idx <= FNoPlants - 1; Idx++)
                    {
                        if (FRootData[Idx].fLambda[Ldx] > 0.0)
                        {
                            dSum = 0.0;
                            for (Jdx = 0; Jdx <= FNoPlants - 1; Jdx++)
                                dSum = dSum + FRootData[Jdx].fLambda[Ldx]
                                               * fSpaceProportion(FRootData[Idx].fQ[Ldx],
                                                                   FRootData[Jdx].fQ[Ldx]);
                            FSoilFract[Idx, Ldx] = 2.0 * FRootData[Idx].fLambda[Ldx] * dSum;
                        }
                    }
                }

                // recompute supply rates and update the root surface potentials
                for (Idx = 0; Idx <= FNoPlants - 1; Idx++)
                {
                    if (FPlantInfo[Idx].Demand > 0.0)
                    {
                        dSupplySum = 0.0;               // Newton's method:
                        dSlope = 0.0;               //   dSlope = d(total supply)/d(relative psi)
                        for (Ldx = 0; Ldx <= FNoLayers - 1; Ldx++)
                        {
                            FWaterSupply[Idx, Ldx] = FSoilFract[Idx, Ldx] * FRootData[Idx].fMonoSupply[Ldx];
                            dSupplySum = dSupplySum + FWaterSupply[Idx, Ldx];

                            dPsi_Root = FRootData[Idx].fPsi_min[Ldx] + FRootData[Idx].fRel_Psi * (FRootData[Idx].fPsi_max[Ldx] - FRootData[Idx].fPsi_min[Ldx]);
                            if (dPsi_Root < 0.0)
                                dSlope = dSlope + (FCond_N[Ldx] - 1.0)
                                                     * (FSoilFract[Idx, Ldx] * FRootData[Idx].fGamma[Ldx] - FWaterSupply[Idx, Ldx])
                                                     * (FRootData[Idx].fPsi_max[Ldx] - FRootData[Idx].fPsi_min[Ldx])
                                                     / dPsi_Root;
                        }

                        if (dSlope != 0.0)
                            fDeltaRelPsi = (FPlantInfo[Idx].Demand - dSupplySum) / dSlope;
                        else
                            fDeltaRelPsi = 0.0;

                        if (FRootData[Idx].fRel_Psi + fDeltaRelPsi >= 1.0)
                            FRootData[Idx].fRel_Psi = FRootData[Idx].fRel_Psi + 0.75 * (1.0 - FRootData[Idx].fRel_Psi);
                        else if (FRootData[Idx].fRel_Psi + fDeltaRelPsi <= 0.0)
                            FRootData[Idx].fRel_Psi = 0.0;
                        else
                            FRootData[Idx].fRel_Psi = FRootData[Idx].fRel_Psi + fDeltaRelPsi;
                    }
                }

                // complete?
                bConverged = true;
                for (Idx = 0; Idx <= FNoPlants - 1; Idx++)
                {
                    bConverged = (bConverged && (Math.Abs(FRootData[Idx].fPrev_Rel_Psi - FRootData[Idx].fRel_Psi) < 1.0E-5));
                    for (Ldx = 0; Ldx <= FNoLayers - 1; Ldx++)
                        bConverged = (bConverged && (Math.Abs(FRootData[Idx].fPrev_Fract[Ldx] - FSoilFract[Idx, Ldx]) < 1.0E-5));
                }
                iIterCount++;
            }
            while (!bConverged && (iIterCount <= 100 * Math.Pow(FNoPlants, 2)));

            // Finally, normalize the soil fractions (this should be a small change)
            for (Ldx = 0; Ldx <= FNoLayers - 1; Ldx++)
            {
                dSum = 0.0;
                for (Idx = 0; Idx <= FNoPlants - 1; Idx++)
                    dSum = dSum + FSoilFract[Idx, Ldx];

                if (dSum > 0.0)
                {
                    for (Idx = 0; Idx <= FNoPlants - 1; Idx++)
                        FSoilFract[Idx, Ldx] = FSoilFract[Idx, Ldx] / dSum;
                }
                else
                {
                    dSum = 0.0;
                    for (Idx = 0; Idx <= FNoPlants - 1; Idx++)
                        dSum = dSum + FRootData[Idx].fKL[Ldx];

                    for (Idx = 0; Idx <= FNoPlants - 1; Idx++)
                    {
                        if (dSum > 0.0)
                            FSoilFract[Idx, Ldx] = FRootData[Idx].fKL[Ldx] / dSum;
                        else
                            FSoilFract[Idx, Ldx] = 0.0;
                    }
                }
            }
        }

        /// <summary>
        /// Computes the actual uptake rate once the water supply rates are known
        /// </summary>
        /// <param name="Idx">-</param>
        protected override void computePlantUptake(int Idx)
        {
            double[] dAvailSupply = new double[GrazType.MaxSoilLayers];
            double dProfileSupply;
            double dProfileUptake;
            int Ldx;

            dProfileSupply = 0.0;
            for (Ldx = 0; Ldx <= FNoLayers - 1; Ldx++)
            {
                dAvailSupply[Ldx] = FSoilFract[Idx, Ldx] * FPlantInfo[Idx].Supply[Ldx];
                dProfileSupply = dProfileSupply + dAvailSupply[Ldx];
            }

            dProfileUptake = Math.Min(dProfileSupply, FPlantInfo[Idx].Demand);

            for (Ldx = 0; Ldx <= FNoLayers - 1; Ldx++)                                 // Weight the final total uptake in     
            {
                if (dProfileUptake < FPlantInfo[Idx].Demand)                           // proportion to supply rate           
                    FWaterUptake[Idx, Ldx] = dAvailSupply[Ldx];
                else if (dProfileSupply > 0.0)
                    FWaterUptake[Idx, Ldx] = dAvailSupply[Ldx] * dProfileUptake / dProfileSupply;
                else
                    FWaterUptake[Idx, Ldx] = 0.0;
            }
        }

        /// <summary>
        /// -
        /// </summary>
        /// <param name="SoilParams">Soil parameters</param>
        protected override void checkLayerInfo(SoilWaterParams SoilParams)
        {
            int Ldx;

            for (Ldx = 0; Ldx <= FNoLayers - 1; Ldx++)
            {
                if (SoilParams.SW_WP[Ldx + 1] >= SoilParams.SW_DUL[Ldx + 1])
                    throw new Exception("DUL must be strictly greater than WP in layer " + Ldx.ToString());
            }
        }

        /// <summary>
        /// -
        /// </summary>
        /// <param name="iValue">Number of plants</param>
        protected override void setNoPlants(int iValue)
        {
            base.setNoPlants(iValue);

            Array.Resize(ref FRootData, iValue);

        }

        #endregion

        /// <summary>
        /// 
        /// </summary>
        /// <param name="Idx"></param>
        /// <param name="fKL"></param>
        /// <param name="fLL"></param>
        public void setRootParams(int Idx, double[] fKL, double[] fLL)
        {
            for (int i = 0; i <= fKL.Length - 1; i++)    //for each layer
            {
                if (FRootData[Idx] == null)
                    FRootData[Idx] = new TRoot();
                FRootData[Idx].fKL[i] = fKL[i];
                FRootData[Idx].fLL[i] = fLL[i];
            }
        }

        /// <summary>
        /// Main logic routine for the water uptake calulations                          
        /// Assumption:                                                                  
        /// Plant-specific information for this time step has been provided through    
        ///   calls to setPlantInfo().                                                   
        /// </summary>
        /// <param name="PotentialET">Potential evaporation rate  mm/d</param>
        /// <param name="Theta">Volumetric water content (v/v)</param>
        public override void computeWaterUptake(double PotentialET, double[] Theta)
        {
            int Idx;

            computeSoilFraction(Theta);
            for (Idx = 0; Idx <= FNoPlants - 1; Idx++)                                               // Now balance supply and demand         
                computePlantUptake(Idx);
        }
    }
}



