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
        string SumFileName = Path.ChangeExtension(args[0], ".sum");
        StreamWriter sw = new StreamWriter(SumFileName);
        Console.SetOut(sw);
        try
        {
            if (args.Length < 1)
                throw new Exception("Usage: ApsimX .ApsimFileName");

            ModelInstance Simulation = Load(args[0]);

            Simulation.Run();
            return 0;
        }
        catch (Exception err)
        {
            if (err.InnerException is FinishedException)
            {
                Console.WriteLine(err.InnerException.Message);
                return 0;
            }
            Console.WriteLine(err.ToString());
            return 1;
        }
        finally
        {
            sw.Close();
        }
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
    private static ModelInstance CreateModelInstance(TextReader Reader)
    {
        ModelInstance RootInstance = XmlSerialiser.Deserialise(Reader);
        RootInstance.Initialise();
        return RootInstance;
    }

}
