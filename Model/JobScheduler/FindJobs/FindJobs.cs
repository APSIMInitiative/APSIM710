using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Text;
using System.IO;
using System.Net;
using System.Net.Sockets;
using System.Threading;
using System.Xml;
using CSGeneral;
using System.Reflection;
using System.Xml.Serialization;
using System.Text.RegularExpressions;

class Program
{
    /// <summary>
    /// This program locates files and adds them as jobs to the JobScheduler.
    /// </summary>
    static int Main(string[] args)
    {
        try
        {
            Go(args);
        }
        catch (Exception err)
        {
            Console.WriteLine(err.Message);
            return 1;
        }
        return 0;
    }

    /// <summary>
    /// Go find all files matching the specified filespec.
    /// </summary>
    private static void Go(string[] args)
    {
        // Parse the command line.
        Dictionary<string, string> Macros = Utility.ParseCommandLine(args);
        if (!Macros.ContainsKey("Server") || !Macros.ContainsKey("Port") || !Macros.ContainsKey("FileSpec") || !Macros.ContainsKey("TargetName"))
            throw new Exception("Usage FindJobs Filespec=*.apsim TargetName=Tests Server=bob.apsim.info Port=13000");

        string DirFileSpec = Macros["Filespec"].Replace("\"", "");

        string ApsimExe = "%APSIM%/Model/Apsim.exe";
        string ApsimXExe = "%APSIM%/Model/ApsimX.exe";

        string RootDirectory = Path.GetDirectoryName(DirFileSpec);

        // Scan a directory.
        // 1. For each filename found, add a job for each simulation in each file.
        string FileSpec = Path.GetFileName(DirFileSpec);
        List<string> FileNames = new List<string>();
        Utility.FindFiles(RootDirectory, FileSpec, ref FileNames, false);

        Target Target = new Target();
        Target.Name = Macros["TargetName"];
        foreach (string FileName in FileNames)
        {
            if (File.Exists(FileName))
            {
                string Exe = ApsimExe;
                List<string> SimulationNames = GetSimulationNamesFrom(FileName);

                if (Path.GetExtension(FileName).ToLower() == ".apsimx")
                    Exe = ApsimXExe;

                foreach (string SimulationName in SimulationNames)
                {
                    Job J = new Job();
                    J.Name = FileName + ":" + SimulationName;
                    J.CommandLine = StringManip.DQuote(Exe) + " " + StringManip.DQuote(FileName) + " " + StringManip.DQuote("Simulation=" + SimulationName);
                    J.WorkingDirectory = Path.GetDirectoryName(FileName);
                    Target.Jobs.Add(J);
                }
            }
        }
        XmlSerializer x = new XmlSerializer(typeof(Target));
        StringWriter s = new StringWriter();
        x.Serialize(s, Target);

        Utility.SocketSend(Macros["Server"], Convert.ToInt32(Macros["Port"]), 
                           "AddTarget~" + s.ToString());
    }

    private static List<string> GetSimulationNamesFrom(string FileName)
    {
        StreamReader In = new StreamReader(FileName);
        string Contents = In.ReadToEnd();
        In.Close();
        
        string Pattern;
        if (Path.GetExtension(FileName).ToLower() == ".con")
            Pattern = "^\\[(.+)\\]";
        else if (Path.GetExtension(FileName) == ".apsim" || Path.GetExtension(FileName) == ".apsimx")
            Pattern = "<simulation name=\"(.+)\"";
        else
            throw new Exception("Cannot find simulations in file: " + FileName);

        List<string> Matches = new List<string>();
        Regex rgx = new Regex(Pattern, RegexOptions.IgnoreCase | RegexOptions.Multiline);
        foreach (Match match in rgx.Matches(Contents))
            if (match.Groups.Count == 2)
                Matches.Add(match.Groups[1].Value);
        
        return Matches;
    }


}

