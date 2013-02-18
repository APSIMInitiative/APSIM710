using System;
using System.Collections.Generic;
using System.Text;
using CSGeneral;

public class MolecularPhenology
{
    [Link]
    Phenology Phenology = null;

    [Link]
    Structure Structure = null;

    [Link]
    Function ThermalTime = null;

    [Link]
    Function PhotoperiodFunction = null;
    
    [Link]
    Function Vrn1rate = null;
    
    [Link]
    Function Vrn2rate = null;
    
    [Link]
    Function Vrn3rate = null;

    [Output]
    public double AccumulatedVernalisation = 0;


    //Set up class variables
    [Output]
    double AccumTt = 0;
    [Output]
    double Vrn1Lag = 0;
    [Output]
    double Vrn1 = 0;
    [Output]
    double Vrn2 = 0;
    [Output]
    double Vrn3 = 0;
    [Output]
    double Vrn4 = 0;
    [Output]
    double Vrn1Target = 0;
    [Output]
    double Pp = 0;
    [Output]
    double Tt = 0;
    [Output]
    double MeanT = 0;
    [Output]
    double HaunStageYesterday = 0;
    [Output]
    double DeltaHaunStage = 0;
    [Output]
    double FIHS = 0;
    [Output]
    double TSHS = 0;
    [Output]
    double FLN = 0;
    //[Output]
    //bool IsGerminated = false;
    [Output]
    bool IsPreVernalised = false;
    [Output]
    bool IsVernalised = false;
    [Output]
    bool IsInduced = false;
    [Output]
    bool IsReproductive = false;

    public double VrnRateAt0 = 1.6;
    public double VrnRateAt30 = 0.08;
    public double VrnRateCurve = -0.19;
    public double BaseVrn1Target = 0.74;

    //Event procedures
    [EventHandler]
    public void OnInitialised()
    {
        Vrn4 = 1.0;
        Vrn1Target = 0.74;
    }

    [EventHandler]
    public void OnPrepare()
    {
        if (Phenology.CurrentPhaseName == "Emerging")
            DeltaHaunStage = Tt / 90; //Fixme, need to do something better than this
        else
            DeltaHaunStage = Structure.DeltaNodeNumber;
        
        //Pre-Vernalisation lag, determine the repression of Vrn4
        if (IsPreVernalised == false)
        {
            Vrn4 -= Vrn1rate.Value * DeltaHaunStage;
            Vrn4 = Math.Max(Vrn4, 0.0);
            if (Vrn4 == 0.0)
                IsPreVernalised = true;
        }
        
        //Vernalisation, determine extent of Vrn1 expression when Vrn 4 is suppressed
        if ((IsPreVernalised) && (IsVernalised == false))
        {
            Vrn1 += Vrn1rate.Value * DeltaHaunStage;
            Vrn1 = Math.Min(1.0, Vrn1);
        }
        
        //Update Vernalisation target to reflect photoperiod conditions and determine Vernalisation status
        if ((IsVernalised == false) && (Vrn1Target <= 1.0))
        {
            if (Structure.MainStemNodeNo >= 1.1)
            {
                Vrn2 += Vrn2rate.Value * DeltaHaunStage;
                Vrn1Target = Math.Min(1.0, BaseVrn1Target + Vrn2);
            }
            if (Vrn1 >= Vrn1Target)
                IsVernalised = true;
        }
        //If Vernalisation is complete begin expressing Vrn3
        if ((IsVernalised) && (IsReproductive == false))
        {
            Vrn3 += Vrn3rate.Value * DeltaHaunStage;
            Vrn3 = Math.Min(1.0, Vrn3);
        }

        //Set timings of floral initiation, terminal spiklet and FLN in response to Vrn3 expression
        if ((Vrn3 >= 0.3) && (IsInduced == false))
        {
            IsInduced = true;
            FIHS = Structure.MainStemNodeNo;
        }
        if ((Vrn3 >= 1.0) && (IsReproductive == false))
        {
            IsReproductive = true;
            TSHS = Structure.MainStemNodeNo + 1.0;
            FLN = 2.86 + 1.1 * TSHS;
        }
    }
}
   
