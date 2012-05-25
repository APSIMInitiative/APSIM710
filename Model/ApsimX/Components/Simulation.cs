using System;
using System.Collections.Generic;
using System.Text;

public class Simulation
{
    [Event]
    public event NullTypeDelegate TimeStep;

    [Event]
    public event NullTypeDelegate Initialised;

    [Param]
    public string Name;

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

public class Folder
{

}
public class Area
{

}

[Serializable]
public class SummaryFile
{

}

