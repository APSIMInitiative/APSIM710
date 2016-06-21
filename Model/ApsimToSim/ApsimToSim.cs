using System;
using System.IO;
using System.Collections;
using System.Collections.Specialized;
using System.Runtime.InteropServices;
using System.Reflection;
using System.Reflection.Emit;
using CSGeneral;
using System.Xml;
using ApsimFile;
using System.Collections.Generic;

class ApsimToSimExe
{

    [STAThread]
    static int Main(string[] args)
    {
        // Main entry point into application.
        // Firstly parse all arguments.
        try
        {
            string ApsimFileName = null;
            if (args.Length > 0)
                ApsimFileName = args[0].Replace("\"", "");
            else
                throw new Exception("No .apsim file specified on the command line");

            Dictionary<string, string> Macros = new Dictionary<string, string>();
            for (int i = 1; i < args.Length; i++)
            {
                int pos = args[i].IndexOf('=');
                if (pos > 0)
                {
                    string name = args[i].Substring(0, pos).Replace("\"", "");
                    string value = args[i].Substring(pos + 1).Replace("\"", "");
                    Macros.Add(name, value);
                }
            }
            string simName = "";

            if (Macros.ContainsKey("Simulation"))
                simName = Macros["Simulation"];

            ApsimToSimExe SimCreator = new ApsimToSimExe();
            SimCreator.ConvertApsimToSim(ApsimFileName, simName);
        }
        catch (Exception err)
        {
            Console.WriteLine(err.Message);
            return 1;
        }
        return 0;
    }

    private void ConvertApsimToSim(string ApsimFileName, string SimName)
    {
        //if the filename is not 'rooted' then assume that the user intends to use the current working directory as the root
        ApsimFileName = ApsimFileName.Replace("\"", "");
        if (!Path.IsPathRooted(ApsimFileName))
            ApsimFileName = Path.Combine(Directory.GetCurrentDirectory(), ApsimFileName);

        //double-check file actually exists before proceeding
        if (!File.Exists(ApsimFileName))
            throw new Exception("Error: Specified APSIM File does not exist!\n\t" + ApsimFileName);

        PlugIns.LoadAll();

        Directory.SetCurrentDirectory(Path.GetDirectoryName(ApsimFileName));

        // convert the specified simulations in the specified apsim file name
        // into a separate .sim file for each.
        ApsimFile.ApsimFile Apsim = new ApsimFile.ApsimFile();
        Apsim.OpenFile(ApsimFileName);

        if (Apsim.FactorComponent == null )
			FindSimsAndConvert(Apsim.RootComponent, SimName);
        else
            {
            bool factorialActive = XmlHelper.Value(Apsim.FactorComponent.ContentsAsXML, "active") == "1";
            if (factorialActive)
            {
                List<string> simulationPaths = new List<string>();
                ApsimFile.ApsimFile.ExpandSimsToRun(Apsim.RootComponent, ref simulationPaths);
                foreach (string simXmlPath in simulationPaths)
                {
                    FactorBuilder b = new FactorBuilder(Apsim.FactorComponent);
                    Component Simulation = Apsim.Find(simXmlPath);
                    List<string> allFactorials = Factor.CalcFactorialList(Apsim, simXmlPath);
                    int totalCount = allFactorials.Count;
                    for (int instanceCount = 1; instanceCount <= totalCount; instanceCount++)
                    {
                        string rootName = Simulation.Name;
                        if (b.SaveExtraInfoInFilename)
                            rootName += ";" + allFactorials[instanceCount - 1];
                        else
                        {
                            //write a spacer to list sims in order eg: 01 or 001 or 0001 depending on count
                            string sPad = "";
                            double tot = Math.Floor(Math.Log10(totalCount) + 1);
                            double file = Math.Floor(Math.Log10(instanceCount) + 1);
                            for (int i = 0; i < (int)(tot - file); ++i)
                                sPad += "0";

                            rootName += "_" + sPad + instanceCount;
                        }

                        string fullSimPath = simXmlPath + "@factorial=" + allFactorials[instanceCount - 1];
                        Factor.CreateSimFiles(Apsim, new string[] { fullSimPath }, Directory.GetCurrentDirectory());
                    }
                }
            }
            else if (SimName.Contains("@factorial="))
                foreach (string simFileName in Factor.CreateSimFiles(Apsim, new string[] { SimName }, Directory.GetCurrentDirectory()))
                    Console.Error.WriteLine("Written " + simFileName);
            else
                FindSimsAndConvert(Apsim.RootComponent, SimName);
        } 
    }

    private void FindSimsAndConvert(ApsimFile.Component Apsim, string SimPath)
    {
        // Iterate through all nested simulations and convert them to
        // .sim format if necessary.
        foreach (ApsimFile.Component Child in Apsim.ChildNodes)
        {
            if (Child.Type.ToLower() == "simulation" && Child.Enabled)
            {
				if (SimPath == "" || string.Equals(Child.FullPath, SimPath, StringComparison.CurrentCultureIgnoreCase))
                {
                    string SimFileName = ApsimToSim.WriteSimFile(Child);
					Console.Error.WriteLine("Written " + SimFileName);
                }
            } else if (Child.Type.ToLower() == "folder") 
                FindSimsAndConvert(Child, SimPath);
        }
    }
}

