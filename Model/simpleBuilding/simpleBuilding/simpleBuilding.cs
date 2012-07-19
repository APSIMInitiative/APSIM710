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
    // [Param]
    int batch = 1;
    [Event]
    public event ManureDelegate AddSlurry;
    private double DM = 0;

    void produceSlurry()
    {
        DM += changeDM;

        ManureType ting = new ManureType();

        ting.amount = 10000.0; //kg
        ting.Ash = 0.011; //proportion of DM
        ting.Tan = 0.001; //proportion of FW
        ting.RP = 0.188; //proportion of DM
        ting.fInert = 0.0; //proportion of organic N in an inert form
        ting.NDF = 0.271; //
        ting.RL = 0.039;
        ting.VFA = 0.0152;
        ting.TotalS = 0.69;//g per kg fresh weight
        ting.SulphS = 0.24; //g per kg fresh weight
        //Jonas please add
        // ting.SulphideS = 0.24; //g per kg fresh weight
        // ting.SulphateS = 0.24; //g per kg fresh weight
        ting.ADL = 0.115;
        ting.DM = 0.1;
        ting.pH = 7.6;
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
