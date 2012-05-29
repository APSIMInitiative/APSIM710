using System;
using ModelFramework;
using System.Collections.Generic;

    public class SlurryTank
    {
        double Water = 0;
        double Ash = 0;
        double Tan = 0;

        double N_Inert = 0;
        double NFast = 0;
        double C_Inert = 0;
        double C_Lignin = 0;

        double CSlow = 0;
        double CVFA = 0;

        double CFast = 0;
        double HFast = 0;
        double HSlow = 0;
        double OFast = 0;
        double OSlow = 0;
        double SFast = 0;
        double S_S04 = 0;
        double CGas=0;
        double pH = 7.0;

        //const
        double E = 2.718281828459045;
        double RA = 0.2;
        //from Files
        [Param]
        double ThetaA = 0;
        [Param]
        double ThetaB = 0;
        [Param]
        double ThetaC = 0;

        [Param]
        double ThetaAM = 0;
        [Param]
        double ThetaBM = 0;
        [Param]
        double ThetaCM = 0;


        [Param]
        double GInert = 0;
        [Param]
        double b = 0;

        [Param]
        double pHmin = 0;
        [Param]
        double pHopt_lo = 0;
        [Param]
        double pHmax = 0;
        [Param]
        double pHopt_hi = 0;

        //gas
  
        //other


        double HVFA = 0.0;
        double OVFA = 0.0;

        double slurrypH = 0;
        double HInert = 0;
        double OInert=0;
        double S2_S = 0;
        double CCH4S = 0;
        double NN2O=0;
        double CH4EM = 0;
        double CCO2_S=0;
        double ENH3 = 0;
        //water
        [Link]
        private Paddock MyPaddock;

        double CO2M = 0;
        //from File
        [Param]
         string TankName = "nothing";

     

        [Param]
        double CCH4 = 0;

        [Param]
        double temperatureInKelvin = 283.0;
        [Param]
        double k1 = 0;
        [Param]
        double k2 = 0;
        [Param]
        double k3 = 0;
        [Param]
        double k4 = 0;
        [Param]
        double surfaceArea = 0;

        [Event]
        public event DoubleDelegate CCH4SEvent;
        [Event]
        public event DoubleDelegate CCO2_SEvent;
        [Event]
        public event DoubleDelegate CGasEvent;
        [Event]
        public event DoubleDelegate ENH3Event;
        [Event]
        public event DoubleDelegate CH4EMEvent;
        [Event]
        public event DoubleDelegate NN2OEvent;
        [Event]
        public event StringDelegate panic;
        [Event]
        public event AddFaecesDelegate add_faeces;
        [EventHandler]
        public void OnInitialised()
        {
            Console.Out.WriteLine("Entered OnInitialised in Slurry Tank");
        }
        [EventHandler]
        public void OnPrepare()
        {
            Console.Out.WriteLine("Entered OnPrepare in Slurry Tank");
        }

        private double GetOM()
        {
            double OM = N_Inert + NFast + OInert + OFast + OSlow + HFast + HSlow + HInert;
            return OM;
        }

        private double GetMass()
        {
            double mass = Water + Ash + GetOM() + Tan;
            return mass;
        }

        [EventHandler]
        public void OnAddSlurry(ManureType input)
        {
          
                slurrypH = input.pH;
                double amount = input.amount;
                double DM = amount * input.DM;
                Ash += DM * input.Ash; // formel 1.2 AshContent  //kg
                double TanAdded = amount * input.Tan; //1.3  // kg
                Tan += TanAdded;
                double RP = DM * input.RP;
                //org N is obtained by dividing CP by 6.25
                double orgNAdded = RP / 6.25; // 1.5	//kg
                //Partition org N between inert and Fast pools
                double N_InertAdded = input.fInert * orgNAdded; //1.4
                N_Inert += N_InertAdded; //kg //fInert should be between 0 and 1
                double NFastAdded = (1 - input.fInert) * orgNAdded; // 1.5
                NFast += NFastAdded; //kg
                //! calculate the carbon content of the inert pool
                double C_InertAdded = 10 * input.fInert * orgNAdded;
                C_Inert += C_InertAdded; //1.6 //kg
                HInert += 0.055 * C_InertAdded;//1.34
                OInert += 0.444 * C_InertAdded;// 1.35
                //! Calculate the carbon content of the raw protein
                double CIn_RP = 4.28 * NFastAdded; //1.6 //kg
                //! Calculate the carbon content of the lignin
                double CIn_Lignin = DM * 0.55 * input.ADL; //1.8 //kg
                C_Lignin += CIn_Lignin;
                //! Calculate the carbon content of the slow pool
                double CIn_Slow = DM *0.44 * (input.NDF - input.ADL); //1.9
                CSlow += CIn_Slow;  //kg
                //! Calculate the carbon content of the raw lipid
                double CIn_RL = DM * 0.77 * input.RL; //1.10 //kg
                //! calculate the carbon content of the volatile fatty acids
                double CIn_VFA = DM * 0.4 * input.VFA; //1.11 //kg
                CVFA += CIn_VFA;
                double HIn_VFA = DM * 0.167 * input.VFA;//1.16 // kg
                HVFA += HIn_VFA;
                double OIn_VFA = DM * 0.889 * input.VFA;//1.19 // kg
                OVFA += OIn_VFA;
                //! Calculate the carbon content of the starch and sugar
                double CIn_Starch = DM * 0.44 * input.Rem; //1.12 //kg
                //! Calculate the carbon content of the Fast pool
                CFast += CIn_RP + CIn_RL + CIn_Starch; //1.13
                //! Calculate the hydrogen in the Fast and Slow pools
                HFast += 0.117 * CIn_RP + 0.152 * CIn_RL + 0.139 * CIn_Starch;//1.14
                HSlow += 0.139 * CIn_Slow;//1.15
                //! Calculate the oxygen in the Fast and Slow
                OFast += 0.533 * CIn_RP + 0.14 * CIn_RL + 1.111 * CIn_Starch;//1.17
                OSlow += 1.111 * CSlow;//1.18
                //! Calculate the sulphur in the Fast and Sulphate pools
                SFast += amount * (input.TotalS - (input.SulphS + input.SulphS));//1.20
                S_S04 += amount * input.TotalS;//1.21
                S2_S += amount * input.TotalS; // 1.22
                Water += amount * (1 - input.DM); // 1.1 //kg


             
        }

        [EventHandler]
        public void OnProcess()
        {

            if (Water > 0)
            {
                temperatureInKelvin = 283.0;
                double temperatureInCelsius = temperatureInKelvin - 273.15;
                //! Calculate the normalised temperature effect
                double FTheta = Math.Pow(E, ThetaA + ThetaB * temperatureInCelsius * (1 - 0.5 * (temperatureInCelsius / ThetaC)));  //1.26
                double FpH = 1.0; //new equations, appear after 1.27
                if (pH <= pHmin)
                    FpH = 0;
                if ((pH > pHmin) && (pH < pHopt_lo))
                    FpH = (pHopt_lo - pH) / (pHopt_lo - pHmin);
                if ((pH >= pHopt_lo) && (pH <= pHopt_hi))
                    FpH = 1.0;
                if ((pH > pHopt_hi) && (pH < pHmax))
                    FpH = (pH - pHopt_hi) / (pHmax - pHopt_hi);
                if (pH >= pHmax)
                    FpH = 0;
                //! Calculate the degradation rates of the Fast and Slow pools
                double k1act = FpH * FTheta * k1;//1.25 //should be 0 to 1
                double k2act = FpH * FTheta * k2;//1.25

                double hydrolysedCpool = k1act * CSlow + k2act * CFast;
                double hydrolysedHpool = k1act * HSlow + k2act * HFast;
                double hydrolysedOpool = k1act * OSlow + k2act * OFast;

                double FS = Math.Pow(E, -b * (S_S04 / Water));	//1.31
                double FThetaM = Math.Pow(E, ThetaAM + ThetaBM * temperatureInCelsius * (1 - 0.5 * (temperatureInCelsius / ThetaCM)));
                double k3act = FThetaM * FpH * FS * k3; //1.32
                double k4act = FTheta * FpH * (1 - FS) * k4; //1.33

                //calculate C in CH4S. This will be instantaneously lost as CH4-C
                CCH4S = 0.375 * k2act * SFast; //1.28
                //calculate the H in CH4S
                double HCH4S = 0.333 * CCH4S;//1.39

                CGas = k3act * CVFA;//1.35	//anaerobic degradation of VFA by methanogens

                double Hgas = k3act * HVFA; //1.44
                double Ogas = k3act * OVFA;//1.45
                double CHCH4M = 12 * (CGas / 24 + Hgas / 8 - Ogas / 64); //1.46

                CCO2_S = k4act * CVFA; //1.34  //CO2-C from oxidation by SO4
                //calculate the H utilised during oxidation of VFA by SO4. Oxygen is not budgetted here.
                double HSO4 = 0.167 * CCO2_S;//1.37

                CO2M = CGas - CHCH4M;//1.47	
                CCH4 = CCH4S + CHCH4M; //1.48	

                double KN = Math.Pow(10, -0.09018 - (2729.92 / temperatureInKelvin)); //1.53
                double KH = Math.Pow(10, -1.69 + 1447.7 / temperatureInKelvin); //1.55

                if (Tan > 0.0)
                    ENH3 = 24 * 60 * 60 * surfaceArea * Tan / (Water * KH * (1 + Math.Pow(10, -slurrypH) / KN) * RA); //1.57
                else
                    ENH3 = 0.0;

                //update C state variables
                //! Update the carbon in the Inert pool
                //GInert=0.0;
                C_Inert += GInert * hydrolysedCpool; //1.27

                //! Update the values of the carbon in the Fast and Slow pools is
                CSlow *= (1 - k1act); //1.24
                CFast *= (1 - k2act); //1.24 
                double CAddVFA = (1 - GInert) * hydrolysedCpool - CCH4S;//1.29
                if (CAddVFA < 0.0)
                {
                    StringType message = new StringType("Negative VFA addition");
                    panic.Invoke(message);
                }
                CVFA = (1 - (k3act + k4act)) * CVFA + CAddVFA; //1.30

                //update the H and O
                HFast *= (1 - k2act);//1.36
                HSlow *= (1 - k1act);//1.36
                OFast *= (1 - k2act);//1.36
                OSlow *= (1 - k1act);//1.36
                double HAddInert = 0.055 * GInert * hydrolysedHpool;
                HInert += HAddInert; //1.37
                double OAddInert = 0.044 * GInert * hydrolysedOpool;
                OInert += OAddInert;//1.38
                HVFA += hydrolysedHpool - (HAddInert + HCH4S + HSO4 + Hgas); //1.42
                OVFA += hydrolysedOpool - (OAddInert + Ogas);//1.43

                //update S in Fast	
                SFast *= (1 - k2act);//1.59
                S_S04 -= 2.667 * CCO2_S; //1.60
                double SAddS2 = 2.667 * (CCO2_S + CCH4S);
                S2_S += SAddS2; //1.61

                //update the N
                double NAddInert = 0.1 * GInert * hydrolysedCpool;
                N_Inert += NAddInert;//1.51
                Tan += k2act * NFast - (NAddInert + ENH3);// 1.52
                NFast *= (1 - k2act); //1.50

                if (Tan < 0.0)
                {

                    StringType message = new StringType("Not enough TAN to enable the inert to be created or too much volatilisation");
                    panic.Invoke(message);
                }

                DoubleType value = new DoubleType();
                value.Value = CCH4S;
                CCH4SEvent.Invoke(value);
                value.Value = CCO2_S;
                CCO2_SEvent.Invoke(value);
                value.Value = CGas;
                CGasEvent.Invoke(value);
                value.Value = ENH3;
                ENH3Event.Invoke(value);
                value.Value = CH4EM;
                CH4EMEvent.Invoke(value);
                value.Value = NN2O;
                NN2OEvent.Invoke(value);
                for (int i = 0; i < MyPaddock.Children.Count; i++)
                {
                    Component item = MyPaddock.Children[i];

                    if (item.Name.CompareTo("SurfaceOrganicMatter") == 0)
                    {
                        Console.WriteLine("Sending manure to soil");

                        AddFaecesType faeces = new AddFaecesType();
                        faeces.VolumePerDefaecation = GetMass();
                        faeces.AreaPerDefaecation = 0.0;
                        faeces.NO3N = 0;
                        faeces.AreaPerDefaecation = 0;
                        faeces.Defaecations = 0;
                        faeces.Eccentricity = 0;
                        faeces.NH4N = Tan;
                        faeces.NO3N = 0;
                        faeces.OMAshAlk = 0;
                        faeces.OMN = N_Inert + NFast;
                        faeces.OMP = 0;
                        faeces.OMS = SFast;
                        faeces.OMWeight = GetOM();
                        faeces.POXP = 0;
                        faeces.SO4S = S_S04;
                        item.Publish("add_faeces", faeces);
                    }
                }
                /*
        AddFaecesType faeces1 = new AddFaecesType();
        faeces1.NO3N = 0;
        faeces1.AreaPerDefaecation = 0;
        faeces1.Defaecations = 0;
        faeces1.Eccentricity = 0;
        faeces1.NH4N = 0;
        faeces1.NO3N = 0;
        faeces1.OMAshAlk = 0;
        faeces1.OMN = 0;
        faeces1.OMP = 0;
        faeces1.OMS = 0;
        faeces1.OMWeight = 0;
        faeces1.POXP = 0;
        faeces1.SO4S = 0;
        faeces1.VolumePerDefaecation = 0;
        add_faeces.Invoke(faeces1);*/
                NN2O = 0;

            }
        }

    
}
