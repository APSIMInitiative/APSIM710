using System;
using System.Collections.Generic;
using System.Text;
#if !fulldotnet  
    using ModelFramework;
#endif

class PhaseJumpFunction : Instance
{
    [Link]
    Plant ParentPlant = null;

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
    private Paddock MyPaddock;
    public override void Initialised()
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
   
