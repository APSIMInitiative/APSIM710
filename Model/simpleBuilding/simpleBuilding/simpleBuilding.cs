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
        [Event]
        public event ManureDelegate AddSlurry;
        private double DM=0;
        [EventHandler]
        public void OnInitialised()
        {

      

        }

        [EventHandler]
        public void OnProcess()
        {


            DM += changeDM;
            
            ManureType ting = new ManureType();

            ting.amount = 1000.0;
            ting.Ash=0.011; //proportion of DM
            ting.Tan = 0.001; //proportion of FW
            ting.RP = 0.188; //proportion of DM
            ting.fInert=0.05;
            ting.NDF = 0.271;
            ting.RL = 0.039;
            ting.VFA = 0.0152;
            ting.TotalS = 0;
            ting.SulphS = 0;
            ting.ADL = 0.115;
            ting.DM = 0.1;
            ting.pH = 7;
            for (int i = 0; i < MyPaddock.Children.Count; i++)
            {
                Component item = MyPaddock.Children[i];

                if (item.Name.CompareTo(SlurryTo) == 0)
                {
                    item.Publish("AddSlurry", ting);
                }
            }

        }

    
    

    
}
