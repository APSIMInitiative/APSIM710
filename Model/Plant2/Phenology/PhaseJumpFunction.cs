using System;
using System.Collections.Generic;
using System.Text;
using ModelFramework;

class PhaseJumpFunction
{
    [Link]
    Phenology Phenology = null;

    [Param]
    private string Start = "";
    [Param]
    private string End = "";
    [Param]
    private string PhaseNameToJumpTo = "";
    [Param]
    private string Event = "";
    [Link]
    private Paddock MyPaddock
    = null;

    [EventHandler]
    public void OnInitialised()
    {
        MyPaddock.Subscribe(Event, OnEvent);
    }

    public void OnEvent()
    {
        if (Phenology.Between(Start, End))
        {
            Phenology.CurrentPhaseName = PhaseNameToJumpTo;
        }
    }
}
   
