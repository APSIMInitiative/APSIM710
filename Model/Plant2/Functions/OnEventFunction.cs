using System;
using System.Collections.Generic;
using System.Text;
using ModelFramework;

[Description("Returns the value of PreEventValue child function from Initialisation to SetEvent, PostEventValue from ReSetEvent and PreEventValue again from ReSetEvent to the next SetEvent")]
class OnEventFunction : Function
{
    private double _Value = 0;
    
    [Param]
    private string SetEvent = "";
    [Param]
    private string ReSetEvent = "";
    
    [Link]
    private Paddock MyPaddock = null;

    [Link]
    Function PreEventValue = null;

    [Link]
    Function PostEventValue = null; 

    [EventHandler] 
    public void OnInitialised()
    {
        MyPaddock.Subscribe(SetEvent, OnSetEvent);
        MyPaddock.Subscribe(ReSetEvent, OnReSetEvent);
    }

    [EventHandler]
    public void OnInit()
    {
        _Value = PreEventValue.Value;
    }

    [EventHandler]
    public void OnReSetEvent()
    {
        _Value = PreEventValue.Value;
    }

    public void OnSetEvent()
    {
           _Value = PostEventValue.Value;
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
   
