using System;
using System.Linq;
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
            Console.WriteLine("Added " + Go(args) + " Jobs");
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
    private static int Go(string[] args)
    {
        // Parse the command line.
        Dictionary<string, string> Macros = Utility.ParseCommandLine(args);
        if (!Macros.ContainsKey("Server") || !Macros.ContainsKey("Port") || !Macros.ContainsKey("FileSpec") || !Macros.ContainsKey("TargetName"))
            throw new Exception("Usage FindJobs Filespec=*.apsim TargetName=Tests Server=bob.apsim.info Port=13000");

        string DirFileSpec = Macros["Filespec"].Replace("\"", "");

        string RootDirectory = Path.GetDirectoryName(DirFileSpec);

        // Scan a directory.
        // 1. For each filename found, add a job for each simulation in each file.
        string FileSpec = Path.GetFileName(DirFileSpec);
        List<string> FileNames = new List<string>();
        Utility.FindFiles(RootDirectory, FileSpec, ref FileNames, true, false);

        Target Target = new Target();
        Target.Name = Macros["TargetName"];
        foreach (string FileName in FileNames)
        {
            if (File.Exists(FileName))
            {
                string Exe = "";
                if (Path.GetExtension(FileName).ToLower() == ".con")
                    Exe = "%APSIM%/Model/ConToSim.exe";
                else if (Path.GetExtension(FileName).ToLower() == ".apsim")
                    Exe = "%APSIM%/Model/ApsimToSim.exe";
                Job convJob = new Job();
                convJob.Name = "";
                if (Exe != "")
                {
                    convJob.Name = "Convert " + FileName;
                    convJob.CommandLine = StringManip.DQuote(Exe) + " " + StringManip.DQuote(FileName);
                    convJob.WorkingDirectory = Path.GetDirectoryName(FileName);
                    Target.Jobs.Add(convJob);
                }

                foreach (string SimulationName in GetSimulationNamesFrom(FileName))
                {
                    Job J = new Job();
                    J.Name = FileName + ":" + SimulationName;
                    if (Path.GetExtension(FileName).ToLower() == ".apsim")
                    {
                        J.CommandLine = StringManip.DQuote("%APSIM%/Model/ApsimModel.exe") + " " +
                                         StringManip.DQuote(SimulationName + ".sim");
                        J.DependsOn = new List<DependsOn>();
                        J.DependsOn.Add(new DependsOn(convJob.Name));
                        J.StdOutFilename = Path.Combine(Path.GetDirectoryName(FileName), SimulationName + ".sum");
                    }
                    else if (Path.GetExtension(FileName).ToLower() == ".con")
                    {
                        string simfile = Path.ChangeExtension(FileName, "." + SimulationName + ".sim");
                        J.CommandLine = StringManip.DQuote("%APSIM%/Model/ApsimModel.exe") + " " +
                                         StringManip.DQuote(simfile);
                        J.DependsOn = new List<DependsOn>();
                        J.DependsOn.Add(new DependsOn(convJob.Name));
                        J.StdOutFilename = Path.Combine(Path.GetDirectoryName(FileName), Path.ChangeExtension(simfile, ".sum"));
                    }
                    else
                    {
                        J.CommandLine = StringManip.DQuote("%APSIM%/Model/ApsimX.exe") + " " +
                                      StringManip.DQuote(FileName) + " " +
                                      StringManip.DQuote("Simulation=" + SimulationName);
                    }
                    J.WorkingDirectory = Path.GetDirectoryName(FileName);
                    Target.Jobs.Add(J);
                }
            }
        }

        //rearrange run jobs so that the longest run first.
        List<Job> convertJobs = new List<Job>();
        List<Job> runJobs = new List<Job>();

        foreach (Job j in Target.Jobs)
        {
            if (j.CommandLine.Contains("ApsimToSim.exe"))
                convertJobs.Add(j);
            else
                runJobs.Add(j);
        }
        Target.Jobs.Clear();
        Target.Jobs.AddRange(convertJobs);

        StreamReader dataFile = new StreamReader(@"Build\report.txt");
        string line;
        Dictionary<string, int> times = new Dictionary<string, int>();
        while ((line = dataFile.ReadLine()) != null)
        {
            if (line.Contains("[Pass] C:"))
            {
                string[] split = line.Split(new char[] { '[' }, StringSplitOptions.RemoveEmptyEntries);
                times.Add(split[1].Substring(6).Trim(), Convert.ToInt32(split[2].Replace("sec]", "")));
            }
        }

        List<Job> runList = new List<Job>();

        List<KeyValuePair<string, int>> jobList = times.ToList();
        jobList.Sort((first, second) =>
            {
                return first.Value.CompareTo(second.Value);
            });

        jobList.Reverse();
        //add jobs from longest to shortest
        foreach (var kvp in jobList)
        {
            for (int i = runJobs.Count - 1;i>=0 ; i--)
            {
                if (runJobs[i].Name.ToLower().Contains(kvp.Key.ToLower()))
                {
                    runList.Add(runJobs[i]);
                    runJobs.RemoveAt(i);
                    break;
                }
            }
        }

        //add any jobs that were not in list
        runList.Reverse();
        runList.AddRange(runJobs);
        runList.Reverse();

        Target.Jobs.AddRange(runList);

        XmlSerializer x = new XmlSerializer(typeof(Target));
        StringWriter s = new StringWriter();
        x.Serialize(s, Target);
        
        //debug code
        string st = s.ToString();
        using (TextWriter writer = File.CreateText(@"c:\temp\list.txt"))
        {
            writer.Write(st);
        }
        
        Utility.SocketSend(Macros["Server"], Convert.ToInt32(Macros["Port"]),
                           "AddTarget~" + s.ToString());
        return (Target.Jobs.Count);
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
            {
                string sim = match.Groups[1].Value;
                if (sim.IndexOf("enabled=\"no") < 0)
                  Matches.Add(StringManip.RemoveAfter(sim, '\"'));
            }

        return Matches;
    }


}

