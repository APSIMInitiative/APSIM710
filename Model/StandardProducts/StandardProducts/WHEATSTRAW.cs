using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
public class WHEATSTRAWClass
{
    [Output]
    ManureType WHEATSTRAW;
    [Param]
    double ADL = 0;
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
        WHEATSTRAW.ADL = ADL;
        WHEATSTRAW.amount = amount;
        WHEATSTRAW.Ash = Ash;
        WHEATSTRAW.C_content = C_content;
        WHEATSTRAW.DM = DM;
        WHEATSTRAW.fInert = fInert;
        WHEATSTRAW.K_content = K_conent;
        WHEATSTRAW.NDF = NDF;
        WHEATSTRAW.NH4_content = NH4;
        WHEATSTRAW.NO3_content = NO3_content;
        WHEATSTRAW.orgN_content = orgN_conent;
        WHEATSTRAW.P_content = p_conent;
        WHEATSTRAW.P_digest = P_digest;
        WHEATSTRAW.pH = pH;
        WHEATSTRAW.Rem = Rem;
        WHEATSTRAW.RL = RL;
        WHEATSTRAW.RP = RP;
        WHEATSTRAW.SulphS = SulphS;
        WHEATSTRAW.Tan = Tan;
        WHEATSTRAW.TotalS = TotalS;
        WHEATSTRAW.VFA = VFA;
        Console.Out.WriteLine("Entered OnInitialised in animalSection");
    }

}

