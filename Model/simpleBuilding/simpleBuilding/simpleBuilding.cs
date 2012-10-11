using System;
using ModelFramework;

public class simpleBuilding
{
    [Link]
    private Paddock MyPaddock;
    [Param]
    public double changeDM = 0.01;
    [Param]
    public string SlurryTo = "nothing";
    [Param]
    int batch = 1;
    [Param]
    double slurry_pH = 0;
    [Param]
    double slurry_Ash = 0;
    [Param]
    double slurry_TAN = 0;
    [Param]
    double slurry_RP = 0;
    [Param]
    double slurry_NDF = 0;
    [Param]
    double slurry_RL = 0;
    [Param]
    double slurry_VFA = 0;
    [Param]
    double slurry_ADL = 0;
    [Param]
    double slurry_DM = 0;
    [Param]
    double slurry_TotalS = 0;
    [Param]
    double slurry_SulphS = 0;
    [Param]
    double slurry_fInert = 0;
    [Event]
    public event ManureDelegate AddSlurry;
    private double DM = 0;

    void produceSlurry()
    {
        DM += changeDM;

        ManureType ting = new ManureType();

        ting.amount = 1000.0; //kg
        ting.Ash=slurry_Ash;//  0.11; //proportion of DM
        ting.Tan = slurry_TAN;// 0.001; //proportion of FW
        ting.RP = slurry_RP;// 0.188; //proportion of DM
        ting.fInert = slurry_fInert;// 0.0; //proportion of organic N in an inert form
        ting.NDF = slurry_NDF;// 0.271; //
        ting.RL = slurry_RL;// 0.039;
        ting.VFA = slurry_VFA;// 0.0152;
        ting.TotalS = slurry_TotalS;// 0.69;//g per kg fresh weight
        ting.SulphS = slurry_SulphS;// 0.24; //g per kg fresh weight
        //Jonas please add
        // ting.SulphideS = 0.24; //g per kg fresh weight
        // ting.SulphateS = 0.24; //g per kg fresh weight
        ting.ADL = slurry_ADL;// 0.115;
        ting.Rem = 1 - (ting.RL + ting.RP + ting.NDF + ting.VFA + ting.Ash);
        ting.DM = slurry_DM;// 0.1;
        ting.pH = slurry_pH;
        for (int i = 0; i < MyPaddock.Children.Count; i++)
        {
            Component item = MyPaddock.Children[i];

            if (item.Name.CompareTo(SlurryTo) == 0)
            {
                item.Publish("AddSlurry", ting);
            }
        }

    }
    [EventHandler]
    public void OnInitialised()
    {
        if (batch == 1)
            produceSlurry();
    }

    [EventHandler]
    public void OnProcess()
    {
        if (batch == 0)
            produceSlurry();
    }
}
