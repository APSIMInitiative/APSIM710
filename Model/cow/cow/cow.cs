using System;
using ModelFramework;

public class cow
    {

    [Event]
    public event ManureDelegate AddSlurry;

    [EventHandler]
        public void OnInitialised()
        {
  
        }
        [EventHandler]
        public void OnPrepare()
        {
          
        }

       
        [EventHandler]
        public void OnProcess()
        {
        ManureType manure = new ManureType();

        manure.amount = 1000.0; //kg
        manure.Ash =   0.11; //proportion of DM
        manure.Tan =  0.001; //proportion of FW
        manure.RP =  0.188; //proportion of DM
        manure.fInert =  0.0; //proportion of organic N in an inert form
        manure.NDF =  0.271; //
        manure.RL =  0.039;
        manure.VFA =  0.0152;
        manure.TotalS =  0.69;//g per kg fresh weight
        manure.SulphS =  0.24; //g per kg fresh weight
 
        manure.Rem = 1 - (manure.RL + manure.RP + manure.NDF + manure.VFA + manure.Ash);
        manure.DM = 1;
        manure.pH =7 ;

        AddSlurry.Invoke( manure);
           
    }

    
}
