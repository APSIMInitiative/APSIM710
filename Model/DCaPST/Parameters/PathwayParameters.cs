using DCAPST.Interfaces;

namespace DCAPST
{   
    public class PathwayParameters : IPathwayParameters
    {
        public double IntercellularToAirCO2Ratio { get; set; }
        public double FractionOfCyclicElectronFlow { get; set; }        
        public double RespirationSLNRatio { get; set; }        
        public double MaxRubiscoActivitySLNRatio { get; set; }        
        public double MaxElectronTransportSLNRatio { get; set; }        
        public double MaxPEPcActivitySLNRatio { get; set; }
        public double MesophyllCO2ConductanceSLNRatio { get; set; }
        public double MesophyllElectronTransportFraction { get; set; }
        public double ATPProductionElectronTransportFactor { get; set; }
       
        public double ExtraATPCost { get; set; }

        public TemperatureResponseValues RubiscoCarboxylation { get; set; }
        public TemperatureResponseValues RubiscoOxygenation { get; set; }
        public TemperatureResponseValues RubiscoCarboxylationToOxygenation { get; set; }
        public TemperatureResponseValues RubiscoActivity { get; set; }

        public TemperatureResponseValues PEPc { get; set; }
        public TemperatureResponseValues PEPcActivity { get; set; }
        public TemperatureResponseValues Respiration { get; set; }

        public LeafTemperatureParameters ElectronTransportRateParams { get; set; }
        public TemperatureResponseValues MesophyllCO2ConductanceParams { get; set; }

        public double SpectralCorrectionFactor { get; set; }
        public double PS2ActivityFraction { get; set; }
        public double PEPRegeneration { get; set; }
        public double BundleSheathConductance { get; set; }       
    }

    public struct TemperatureResponseValues
    {
        /// <summary>
        /// The value of the temperature response factor for a given parameter
        /// </summary>
        public double Factor;

        /// <summary>
        /// The value of the temperature response factor at 25 degrees
        /// </summary>
        public double At25;
    }
}
