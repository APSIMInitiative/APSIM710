using System;
using System.Collections.Generic;
using System.Text;
using System.Xml;
using System.IO;
using System.Reflection;
using CSGeneral;


public class FinishedException : Exception
   {
   public FinishedException(string msg)
      : base(msg)
      
      {
      }
   }

public class Engine
{
    /// <summary>
    /// Main program entry point.
    /// </summary>
    static int Main(string[] args)
    {
        try
        {
            if (args.Length != 1)
                throw new Exception("Usage: FastApsim .ApsimFileName");

            Engine ApsimEngine = new Engine();
            ApsimEngine.Load(args[0]);
            ApsimEngine.Run();
            return 0;
        }
        catch (Exception err)
        {
            Console.WriteLine(err.Message);
            return 1;
        }
    }

    // Some data for our engine.
    private ModelInstance Simulation;

    /// <summary>
    /// Load a simulation from the specified file.
    /// </summary>
    public void Load(string FileName)
    {
        if (!File.Exists(FileName))
            throw new Exception("Cannot find file: " + FileName);

        XmlDocument Doc = new XmlDocument();
        Doc.Load(FileName);

        // Find the <simulation> element.
        XmlNode SimulationNode = XmlHelper.FindByType(Doc.DocumentElement, "Simulation");
        if (SimulationNode == null)
            throw new Exception("Cannot find a simulation to run");

        Simulation = ModelInstance.CreateModelInstance(SimulationNode);
        Simulation Sim = (Simulation)Simulation.TheModel;
        Simulation.UpdateValues(); 
        Sim.InvokeInitialised();
    }

    /// <summary>
    /// Load a simulation from the specified XML
    /// </summary>
    public void LoadXml(string XML)
    {
        XmlDocument Doc = new XmlDocument();
        Doc.LoadXml(XML);

        Simulation = ModelInstance.CreateModelInstance(Doc.DocumentElement);
        Simulation Sim = (Simulation)Simulation.TheModel;
        Sim.InvokeInitialised();
        Simulation.UpdateValues();
    }

    /// <summary>
    /// Run the simulation previously loaded into memory. Returns 1 on on receiving a finish.
    /// </summary>
    public int RunSingleTimeStep()
    {
        try
        {
            Simulation Sim = (Simulation)Simulation.TheModel;
            Sim.InvokeTimeStep();
        }
        catch (FinishedException exc)
        {
            Console.WriteLine(exc.Message);
            return 1;
        }

        return 0;
    }

    /// <summary>
    /// Run entire simulation.
    /// </summary>
    private void Run()
    {
        Simulation.UpdateValues();
        while (RunSingleTimeStep() == 0) ;
    }

    /// <summary>
    /// Find an instance of a model with the specified name.
    /// Usefull for unit tests.
    /// </summary>
    public object FindModel(string ModelName)
    {
        return Simulation.FindModel(ModelName);
    }

}
