using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
public class WASHWATERClass
{
    [Output]
    ManureType WASHWATER;
    [Param]
    double ADL=0;
    [Param]
    double amount = 0;
    [Param]
    double Ash = 0;
    [Param]
    double C_content = 0;
    [Param]
    double DM = 0;
    [Param]
    double fInert = 0;
    [Param]
    double K_conent = 0;
    [Param]
    double NDF = 0;
    [Param]
    double NH4 = 0;
    [Param]
    double NO3_content = 0;
    [Param]
    double orgN_conent = 0;
    [Param]
    double p_conent = 0;
    [Param]
    double P_digest = 0;
    [Param]
    double pH = 0;
    [Param]
    double Rem = 0;
    [Param]
    double RL = 0;
    [Param]
    double RP = 0;
    [Param]
    double SulphS = 0;
    [Param]
    double Tan = 0;
    [Param]
    double TotalS = 0;
    [Param]
    double VFA = 0;
     [EventHandler]
    public void OnInitialised()
    {
        WASHWATER.ADL = ADL;
        WASHWATER.amount = amount;
        WASHWATER.Ash = Ash;
        WASHWATER.C_content = C_content;      
        WASHWATER.DM = DM;
        WASHWATER.fInert = fInert;
        WASHWATER.K_content = K_conent;
        WASHWATER.NDF = NDF;
        WASHWATER.NH4_content = NH4;
        WASHWATER.NO3_content = NO3_content;
        WASHWATER.orgN_content = orgN_conent;
        WASHWATER.P_content = p_conent;
        WASHWATER.P_digest = P_digest;
        WASHWATER.pH = pH;
        WASHWATER.Rem = Rem;
        WASHWATER.RL = RL;
        WASHWATER.RP = RP;
        WASHWATER.SulphS = SulphS;
        WASHWATER.Tan = Tan;
        WASHWATER.TotalS = TotalS;
        WASHWATER.VFA = VFA;
        Console.Out.WriteLine("Entered OnInitialised in animalSection");
    }

}

