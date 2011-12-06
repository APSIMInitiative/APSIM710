using System;
using System.Collections.Generic;
using System.Text;
using ModelFramework;

class OnEventFunction : Function
{
    [Param]
    private string Event = "";
    private double _Value = 0;


    [Link]
    private Paddock MyPaddock = null;

    [EventHandler] 
    public void OnInitialised()
    {
        MyPaddock.Subscribe(Event, OnEvent);
    }

    public void OnEvent()
    {
        List<object> Children = My.ChildrenAsObjects;
        
        if (Children.Count == 0)
            throw new Exception("Cannot find function in function: " + Name);

        Function F = Children[0] as Function;
        _Value = F.Value;
    }

    [Output]
    public override double Value
    {
        get
        {
            return _Value;
        }
    }

}
   
