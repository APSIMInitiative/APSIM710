using System;
using ModelFramework;
using System.Collections.Generic;

public class SlurryTank
{
    [Input]
    int day;
    [Output]
    double Water = 0;
    [Output]
    double Ash = 0;
    [Output]
    double Tan = 0;
    [Output]
    double N_Inert = 0;
    [Output]
    double NFast = 0;
    [Output]
    double C_Inert = 0;
    [Output]
    double C_Lignin = 0;
    [Output]
    double CSlow = 0;
    [Output]
    double CVFA = 0;
    [Output]
    double CFast = 0;
    [Output]
    double HFast = 0;
    [Output]
    double HSlow = 0;
    [Output]
    double OFast = 0;
    [Output]
    double OSlow = 0;
    [Output]
    double SFast = 0;
    [Output]
    double S_S04 = 0;

    //const
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

    [Output]
    double CGas = 0;
    [Output]
    double TotalNInput = 0;
    [Output]
    double TotalNOutput = 0;
    [Output]
    double TotalCInput = 0;
    [Output]
    double TotalCOutput = 0;
    [Output]
    double HVFA = 0.0;
    [Output]
    double OVFA = 0.0;
    [Output]
    double slurrypH = 0;
    [Output]
    double HInert = 0;
    [Output]
    double OInert = 0;
    [Output]
    double S2_S = 0;
    [Output]
    double CCH4S = 0;
    [Output]
    double NN2O = 0;
    [Output]
    double CH4EM = 0;
    [Output]
    double CCO2_S = 0;
    [Output]
    double ENH3 = 0;
    [Output]
    int daysSinceFilled = 0;
    [Output]
    double CO2M = 0;
    [Output]
    double CHCH4M = 0;
    [Output]
    double SlurryOM = 0;
    [Output]
    double CH4CPerHrPerVS = 0;
    [Output]
    double cumCH4litresPerkgVS = 0;
    [Output]
    double Finc = 0;
    [Output]
    double FTheta = 0;
    [Output]
    double FpH = 0;
    [Output]
    double FThetaM = 0;
    [Output]
    double FS = 0;

    [Link]
    private Paddock MyPaddock;

    
    //this is not a parameter Jonas!
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
    [Param]
    double dayOfEmpty = -1;

    [Param]
    double lag_alpha = 0;
    [Param]
    double lag_beta = -1;

    double initialOM = 0;

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
        Console.WriteLine("OnInit");
    }
    [EventHandler]
    public void OnPrepare()
    {
        Console.WriteLine("OnPrep");

    }

    private double GetOM()
    {
        double OM = N_Inert + NFast + OInert + OFast + OSlow + HFast + HSlow + HInert + C_Inert + C_Lignin/0.55 + CSlow + CFast +
            CVFA + OVFA + HVFA;
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
        //TotalNInput += TanAdded;
        Tan += TanAdded;
        double RP = DM * input.RP;
        //org N is obtained by dividing CP by 6.25
        double orgNAdded = RP / 6.25; // 1.5	//kg
        //TotalNInput += orgNAdded;
        //Partition org N between inert and Fast pools
        double N_InertAdded = input.fInert * orgNAdded; //1.4
        N_Inert += N_InertAdded; //kg //fInert should be between 0 and 1
        double NFastAdded = (1 - input.fInert) * orgNAdded; // 1.5
        NFast += NFastAdded; //kg
        //! calculate the carbon content of the inert pool
        double C_InertAdded = 10 * input.fInert * orgNAdded;
        //TotalCInput += C_InertAdded;
        C_Inert += C_InertAdded; //1.6 //kg
        HInert += 0.055 * C_InertAdded;//1.34
        OInert += 0.444 * C_InertAdded;// 1.35
        //! Calculate the carbon content of the raw protein
        double CIn_RP = 4.28 * NFastAdded; //1.6 //kg
        //! Calculate the carbon content of the lignin
        double CIn_Lignin = DM * 0.55 * input.ADL; //1.8 //kg
        //TotalCInput += CIn_Lignin;
        C_Lignin += CIn_Lignin;
        //! Calculate the carbon content of the slow pool
        double CIn_Slow = DM * 0.44 * (input.NDF - input.ADL); //1.9
        //TotalCInput += CIn_Slow;
        CSlow += CIn_Slow;  //kg
        //! Calculate the carbon content of the raw lipid
        double CIn_RL = DM * 0.77 * input.RL; //1.10 //kg
        //! calculate the carbon content of the volatile fatty acids
        double CIn_VFA = DM * 0.4 * input.VFA; //1.11 //kg
        //TotalCInput += CIn_VFA;
        CVFA += CIn_VFA;
        double HIn_VFA = DM * 0.167 * input.VFA;//1.16 // kg
        HVFA += HIn_VFA;
        double OIn_VFA = DM * 0.889 * input.VFA;//1.19 // kg
        OVFA += OIn_VFA;
        //! Calculate the carbon content of the starch and sugar
        double CIn_Starch = DM * 0.44 * input.Rem; //1.12 //kg
        //! Calculate the carbon content of the Fast pool
        CFast += CIn_RP + CIn_RL + CIn_Starch; //1.13
        //TotalCInput += CIn_RP + CIn_RL + CIn_Starch;
        //! Calculate the hydrogen in the Fast and Slow pools
        HFast += 0.117 * CIn_RP + 0.152 * CIn_RL + 0.139 * CIn_Starch;//1.14
        HSlow += 0.139 * CIn_Slow;//1.15
        //! Calculate the oxygen in the Fast and Slow
        OFast += 0.533 * CIn_RP + 0.14 * CIn_RL + 1.111 * CIn_Starch;//1.17
        OSlow += 1.111 * CSlow;//1.18
        //! Calculate the sulphur in the Fast and Sulphate pools
        //correct form is this!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
       // S_S04 += amount * input.SulphateS;//1.21
        //S2_S += amount * input.SulphideS; // 1.22
        //SFast += amount * (input.TotalS - (input.SulphateS + input.SulphideS));//1.20
        SFast += amount * (input.TotalS - input.SulphS)/1000.0;//1.20
        S_S04 += amount * input.SulphS / 1000.0;//1.21
        S2_S += 0; // 1.22
        Water += amount * (1 - input.DM); // 1.1 //kg
        initialOM = GetOM();
    }

    [EventHandler]
    public void OnProcess()
    {

        if (Water > 0)
        {
            daysSinceFilled++;
            temperatureInKelvin = 283.0;
            double temperatureInCelsius = temperatureInKelvin - 273.15;
            //Calculate lag effect after cleaning
            //lag_alpha and lag_beta are constants
            Finc = 1 / (1 + Math.Exp(-(lag_alpha * daysSinceFilled - lag_beta)));
            //! Calculate the normalised temperature effect
            FTheta = Math.Exp(ThetaA + ThetaB * temperatureInCelsius * (1 - 0.5 * (temperatureInCelsius / ThetaC)));  //1.26
            FpH = 1.0; //new equations, appear after 1.27
            if (slurrypH <= pHmin)
                FpH = 0;
            if ((slurrypH > pHmin) && (slurrypH < pHopt_lo))
                FpH = (pHopt_lo - slurrypH) / (pHopt_lo - pHmin);
            if ((slurrypH >= pHopt_lo) && (slurrypH <= pHopt_hi))
                FpH = 1.0;
            if ((slurrypH > pHopt_hi) && (slurrypH < pHmax))
                FpH = (slurrypH - pHopt_hi) / (pHmax - pHopt_hi);
            if (slurrypH >= pHmax)
                FpH = 0;
            //! Calculate the degradation rates of the Fast and Slow pools
            double k1act = FpH * FTheta * k1;//1.25 //should be 0 to 1
            double k2act = FpH * FTheta * k2;//1.25

            double hydrolysedCpool = k1act * CSlow + k2act * CFast;
            double hydrolysedHpool = k1act * HSlow + k2act * HFast;
            double hydrolysedOpool = k1act * OSlow + k2act * OFast;

            FS = Math.Exp(-b * (S_S04 / Water));	//1.31
            FThetaM = Math.Exp(ThetaAM + ThetaBM * temperatureInCelsius * (1 - 0.5 * (temperatureInCelsius / ThetaCM)));
            double k3act = Finc * FThetaM * FpH * FS * k3; //1.32
            double k4act = FTheta * FpH * (1 - FS) * k4; //1.33

            //calculate C in CH4S. This will be instantaneously lost as CH4-C
            CCH4S = 0.375 * k2act * SFast; //1.28
            //TotalCOutput += CCH4S;
            //calculate the H in CH4S
            double HCH4S = 0.333 * CCH4S;//1.39

            CGas = k3act * CVFA;//1.35	//anaerobic degradation of VFA by methanogens
            //TotalCOutput += CGas;
            double Hgas = k3act * HVFA; //1.44
            double Ogas = k3act * OVFA;//1.45
            CHCH4M = 12 * (CGas / 24 + Hgas / 8 - Ogas / 64); //1.46

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
            //TotalNOutput += ENH3;
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

            SlurryOM = GetOM();
            CH4CPerHrPerVS = 1000000.0 * CCH4 / (24 * initialOM);
            //litres of methane per kg VS; = kg CH4-C * grams per kg * litres per mol/gram mol
            cumCH4litresPerkgVS += CCH4 * 1000.0 * 22.4 / (16 * initialOM);

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
            if (daysSinceFilled==dayOfEmpty)
            {
                for (int i = 0; i < MyPaddock.Children.Count; i++)
                {
                    Component item = MyPaddock.Children[i];

                    if (item.Name.CompareTo("SurfaceOrganicMatter") == 0)
                    {


                        AddFaecesType faeces = new AddFaecesType();
                        faeces.VolumePerDefaecation = GetMass();
                        faeces.AreaPerDefaecation = 0.0;
                        faeces.NO3N = 0;
                        faeces.AreaPerDefaecation = 0;
                        faeces.Defaecations = 0;
                        faeces.Eccentricity = 0;
                        faeces.NH4N = Tan;
                        Tan = 0;
                        faeces.NO3N = 0;
                        faeces.OMAshAlk = 0;
                        faeces.OMN = N_Inert + NFast;
                        N_Inert = 0;
                        NFast = 0;
                        faeces.OMP = 0;
                        faeces.OMS = SFast;
                        faeces.OMWeight = GetOM();
                        faeces.POXP = 0;
                        faeces.SO4S = S_S04;
                        item.Publish("add_faeces", faeces);
                    }
                }
                Water = 0;
                Ash = 0;
                Tan = 0;

                N_Inert = 0;
                NFast = 0;
                C_Inert = 0;
                C_Lignin = 0;

                CSlow = 0;
                CVFA = 0;

                CFast = 0;
                HFast = 0;
                HSlow = 0;
                OFast = 0;
                OSlow = 0;
                SFast = 0;
                S_S04 = 0;
                CGas = 0;
                HVFA = 0.0;
                OVFA = 0.0;

                slurrypH = 0;
                HInert = 0;
                OInert = 0;
                S2_S = 0;
                CCH4S = 0;
                NN2O = 0;
                CH4EM = 0;
                CCO2_S = 0;
                ENH3 = 0;
                daysSinceFilled = 0;
                cumCH4litresPerkgVS = 0;
                CH4CPerHrPerVS = 0;
                SlurryOM = GetOM();
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
