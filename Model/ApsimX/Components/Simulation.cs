using System;
using System.Collections.Generic;
using System.Text;

[CanHaveChildren]
public class Simulation
{
    [Event]
    public event NullTypeDelegate TimeStep;

    [Event]
    public event NullTypeDelegate Initialised;

    public void InvokeInitialised()
    {
        if (Initialised != null)
            Initialised.Invoke();
    }

    public void InvokeTimeStep()
    {
        if (TimeStep != null)
            TimeStep.Invoke();
    }

}

[CanHaveChildren]
public class Area
{

}

public class SummaryFile
{

}

