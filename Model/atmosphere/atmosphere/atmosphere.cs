using System;
using ModelFramework;

public class atmosphere
    {
        
        //gas
        [Output]
        [Units("kg")]
        public double CCH4S =0;
        [Output]
        [Units("kg")]
        public double CCO2_S=0;
        [Output]
        [Units("kg")]
        public double CGas=0;
        [Output]
        [Units("kg")]
        public double ENH3=0;
        [Output]
        [Units("kg")]
        public double CH4EM = 0;
        [Output]
        [Units("kg")]
        public double NN2O = 0;
        [Output]
        [Units("kg")]
        public double NH3Emission = 0;
        [Output]
        [Units("kg")]
        public double N2Emission = 0;
        [Output]
        [Units("kg")]
        public double N2OEmission = 0;
        [Output]
        [Units("kg")]
        public double CH4Emission = 0;
        [Output]
        [Units("kg")]
        public double CO2Emission = 0;

        [EventHandler]
        public void OnInitialised()
        {
  
        }
        [EventHandler]
        public void OnPrepare()
        {
          
        }

        [EventHandler]
        public void OnCCH4SEvent(DoubleType CCH4S)
        {
            this.CCH4S += CCH4S.Value;
        }
        [EventHandler]
        public void OnCCO2_SEvent(DoubleType CCO2_S)
        {
            this.CCO2_S += CCO2_S.Value;
        }
        [EventHandler]
        public void OnCGasEvent(DoubleType CGas)
        {
            this.CGas += CGas.Value;
        }
        [EventHandler]
        public void OnENH3Event(DoubleType ENH3)
        {
            this.ENH3 += ENH3.Value;
       
        }
        [EventHandler]
        public void OnNN2OEvent(DoubleType NN2O)
        {
            this.NN2O += NN2O.Value;
        }
        [EventHandler]
        public void OnCH4EMEvent(DoubleType CH4EM)
        {
            this.CH4EM += CH4EM.Value;
        }
        [EventHandler]
        public void OnNH3EmissionEvent(DoubleType NH3Emission)
        {
            this.NH3Emission += NH3Emission.Value;
        }
        [EventHandler]
        public void OnCN2OEmissionEvent(DoubleType N2OEmission)
        {
            this.N2OEmission += N2OEmission.Value;
        }
        [EventHandler]
        public void OnCH4EmissionEvent(DoubleType CH4Emission)
        {
            this.CH4Emission += CH4Emission.Value;
        }
        [EventHandler]
        public void OnCO2EmissionEvent(DoubleType CO2Emission)
        {
            this.CO2Emission += CO2Emission.Value;
        }
        [EventHandler]
        public void OnProcess()
        {
            
        }

    
}
