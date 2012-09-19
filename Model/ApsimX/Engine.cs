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

public class ApsimX
{
    /// <summary>
    /// Main program entry point.
    /// </summary>
    static int Main(string[] args)
    {
        if (args.Length < 1)
        {
            Console.WriteLine("Usage: ApsimX .ApsimFileName");
            return 1;
        }

        string SumFileName = Path.ChangeExtension(args[0], ".sum");
        StreamWriter sw = new StreamWriter(SumFileName);
        Console.SetOut(sw);
        try
        {
            ModelInstance Simulation = Load(args[0]);

            Simulation.Run();
        }
        catch (Exception err)
        {
            if (err.InnerException is FinishedException)
            {
                Console.WriteLine(err.InnerException.Message);
            }
            else
                Console.WriteLine(err.ToString());
        }
        finally
        {
            sw.Close();
        }
        return 0;
    }

    /// <summary>
    /// Load a simulation from the specified file.
    /// </summary>
    public static ModelInstance Load(string FileName)
    {
        if (!File.Exists(FileName))
            throw new Exception("Cannot find file: " + FileName);

        StreamReader In = new StreamReader(FileName);

        ModelInstance Simulation = CreateModelInstance(In);
        return Simulation;
    }

    /// <summary>
    /// Create instances of all objects specified by the XmlNode. Returns the top 
    /// level instance.
    /// </summary>
    public static ModelInstance CreateModelInstance(TextReader Reader)
    {
        ModelInstance RootInstance = XmlSerialiser.Deserialise(Reader);
        RootInstance.Initialise();
        return RootInstance;
    }

}
