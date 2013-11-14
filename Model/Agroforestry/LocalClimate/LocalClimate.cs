using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using CMPServices;
using ModelFramework;

public class LocalClimate
{
    [Input]
    DateTime Today;   // Equates to the value of the current simulation date - value comes from CLOCK
    [Output]
    float mint { get; set; }
    [Output]
    float maxt { get; set; }
    [Output]
    float radn { get; set; }
    [Output]
    float rain { get; set; }
    [Output]
    float vp { get; set; }

    [EventHandler]
    public void OnInitialised()
    {
    }

    // The following event handler will be called each day at the beginning of the day
    [EventHandler]
    public void OnPrepare()
    {
        NewMetType metData = new NewMetType();
        metData.mint = mint;
        metData.maxt = maxt;
        metData.radn = radn;
        metData.rain = 0;//rain;
        metData.vp = vp;
        metData.today = Today.Date.ToOADate(); //Not sure if this is what the NewMetType uses, but it's the only one that returns a double.
        newMet.Invoke(metData);
    }

    [Event]
    public event NewMetDelegate newMet;
}
