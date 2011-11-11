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
    private ModelEnvironment MyPaddock = null;

    [EventHandler] 
    public void OnInitialised()
    {
        MyPaddock.Subscribe(Event, OnEvent);
    }

    public void OnEvent()
    {
        string[] Children = MyPaddock.ChildNames();
        
        if (Children.Length == 0)
            throw new Exception("Cannot find function in function: " + Name);

        Function F = MyPaddock.Link<Function>(Children[0]);
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
   
