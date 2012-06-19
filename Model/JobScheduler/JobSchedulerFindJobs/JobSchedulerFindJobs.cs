using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Text;
using System.IO;
using System.Net;
using System.Net.Sockets;
using System.Threading;
using System.Xml;
using ApsimFile;
using CSGeneral;

class Program
{
    /// <summary>
    /// This program locates files and adds them as jobs to the JobScheduler.
    /// The first argument is the filespec (including directory path) e.g. 
    ///   c:\Apsim\Examples\*.apsim, or "c:\Apsim\Examples\Continuous Wheat.apsim"
    /// The second argument is a "NodePath". This is a reference to a node
    /// in the JobScheduler tree under which this program is to add the new
    /// jobs. When this program opens a socket connection to the JobScheduler,
    /// this NodePath is sent as an argument.
    /// </summary>
    static int Main(string[] args)
    {
        try
        {
            if (args.Length != 2)
                throw new Exception("Usage: JobSchedulerFindJobs FileSpec NodePath");
            Go(args[0].Replace("\"", ""), args[1].Replace("\"", ""));
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
    private static void Go(string DirFileSpec, string NodePath)
    {
        string RootDirectory = Path.GetDirectoryName(DirFileSpec);

        if (File.Exists(DirFileSpec))
        {
            XmlDocument Doc = new XmlDocument();
            XmlNode Node = Doc.AppendChild(Doc.CreateElement("dummy"));
            if (Path.GetExtension(DirFileSpec).ToLower() == ".con")
            {
                // Run ConToSim, find the .sim files created, and insert them back into the tree
                CreateConJobXML(DirFileSpec, Node);
            }
            else if (Path.GetExtension(DirFileSpec).ToLower() == ".apsim" || Path.GetExtension(DirFileSpec).ToLower() == ".apsimx")
            {
                CreateApsimJobXML(DirFileSpec, Node);
            }
            else if (Path.GetExtension(DirFileSpec).ToLower() == ".sim")
            {
                CreateSimJobXML(DirFileSpec, Node);
            }
            else
            {
                CreateShellExecuteJobXML(DirFileSpec, Node);
            }
            string XML = "AddXML~" + NodePath + "/RunApsim~" + Doc.DocumentElement.OuterXml;
            TalkToJobScheduler(XML);
            Console.WriteLine("xml=" + XML);
        }
        else
        {
            // Scan a directory.
            // 1. For each filename found, add a job for each simulation in each file.
            string FileSpec = Path.GetFileName(DirFileSpec);
            List<string> FileNames = new List<string>();
            Utility.FindFiles(RootDirectory, FileSpec, ref FileNames, false);
            foreach (string FileName in FileNames)
            {
                // 2. Create a structure to send back to the Scheduler
                XmlDocument Doc = new XmlDocument();
                XmlNode RootNode = Doc.AppendChild(Doc.CreateElement("dummy"));
                string XML;
                if (Path.GetExtension(FileName).ToLower() == ".con" || Path.GetExtension(FileName).ToLower() == ".apsim" || Path.GetExtension(FileName).ToLower() == ".sim")
                {
                    // Book in another instance to call Create[Con|Apsim]Job
                    ScheduleJobXML(FileName, NodePath, RootNode);
                    XML = "AddXML~" + NodePath + "/CreateSims~" + Doc.DocumentElement.OuterXml;
                }
                else if (Path.GetExtension(FileName).ToLower() == ".apsimx")
                {
                    string SumFileName = Path.ChangeExtension(FileName, ".sum");
                    XmlDocument DocX = new XmlDocument();
                    XmlNode Node = DocX.AppendChild(DocX.CreateElement("dummy"));
                    XmlNode JobNode = Node.AppendChild(Node.OwnerDocument.CreateElement("Job"));
                    XmlHelper.SetName(JobNode, "ApsimX " + FileName);
                    XmlHelper.SetValue(JobNode, "WorkingDirectory", Path.GetDirectoryName(FileName));
                    XmlHelper.SetValue(JobNode, "CommandLine", ReplaceEnvironmentVariables(StringManip.DQuote(Path.Combine("%APSIM%", "Model", "ApsimX.exe")) + " " + StringManip.DQuote(FileName) + " > " + StringManip.DQuote(SumFileName)));
                    XmlHelper.SetValue(JobNode, "CommandLineUnix", ReplaceEnvironmentVariables(StringManip.DQuote(Path.Combine("%APSIM%", "Model", "ApsimX.x")) + " " + StringManip.DQuote(FileName) + " > " + StringManip.DQuote(SumFileName)));
                    XML = "AddXML~" + NodePath + "/RunApsim~" + DocX.OuterXml;
                }
                else
                {
                    CreateShellExecuteJobXML(FileName, RootNode);
                    XML = "AddXML~" + NodePath + "/~" + Doc.DocumentElement.OuterXml;
                }
                TalkToJobScheduler(XML);
            }
        }

    }
    /// <summary>
    /// Create a job to convert a [.con or .apsim] file.
    /// </summary>
    private static void ScheduleJobXML(string FileName, string NodePath, XmlNode RootNode)
    {
        XmlNode JobNode = RootNode.AppendChild(RootNode.OwnerDocument.CreateElement("Job"));

        XmlHelper.SetName(JobNode, "Convert To Sim " + FileName);
        XmlHelper.SetValue(JobNode, "WorkingDirectory", Path.GetDirectoryName(FileName));

        string commandLine = ReplaceEnvironmentVariables(StringManip.DQuote(Path.Combine("%APSIM%", "Model", "JobSchedulerFindJobs.exe")) + " " + StringManip.DQuote(FileName) + " " + NodePath);
        XmlHelper.SetValue(JobNode, "CommandLine", commandLine);
        XmlHelper.SetValue(JobNode, "CommandLineUnix", commandLine);

    }

    /// <summary>
    /// For APSIM simulations, create a special type of job that not only runs
    /// ApsimToSim (or ConToSim) but also APSIM.
    /// </summary>
    private static void CreateApsimJobXML(string FileName, XmlNode Node)
    {
        // Add multiple nodes to the RunApsim folder.
        ApsimFile.ApsimFile Apsim = new ApsimFile.ApsimFile();
        PlugIns.LoadAll();
        Apsim.OpenFile(FileName);
        
        foreach (ApsimFile.Component Child in Apsim.RootComponent.ChildNodes)
        {
            CreateApsimJobXML(Child, Node, FileName);
        }
    }
    private static void CreateApsimJobXML(Component Comp, XmlNode Node, string FileName)
    {
        if (Comp.Type.ToLower() == "simulation" && Comp.Enabled)
        {
            bool IsApsimX = Path.GetExtension(FileName).ToLower() == ".apsimx";
            string SimFileName;
            string Executable;
            string LinuxExecutable;
            if (IsApsimX)
            {
                SimFileName = FileName;
                Executable = "ApsimX.exe";
                LinuxExecutable = "ApsimX.x";
            }
            else
            {
                SimFileName = ApsimToSim.WriteSimFile(Comp);
                Executable = "Apsim.exe";
                LinuxExecutable = "Apsim.x";
            }
            if (Path.DirectorySeparatorChar == '\\') 
                SimFileName = SimFileName.Replace('/', '\\'); 
            else 
                SimFileName = SimFileName.Replace('\\', '/');
            string SumFileName = Path.ChangeExtension(SimFileName, ".sum");
            XmlNode JobNode = Node.AppendChild(Node.OwnerDocument.CreateElement("Job"));
            XmlHelper.SetName(JobNode, Executable + " " + SimFileName);
            XmlHelper.SetValue(JobNode, "WorkingDirectory", Path.GetDirectoryName(SimFileName));
            XmlHelper.SetValue(JobNode, "CommandLine", ReplaceEnvironmentVariables(StringManip.DQuote(Path.Combine("%APSIM%", "Model", Executable)) + " " + StringManip.DQuote(SimFileName) + " > " + StringManip.DQuote(SumFileName)));
            XmlHelper.SetValue(JobNode, "CommandLineUnix", ReplaceEnvironmentVariables(StringManip.DQuote(Path.Combine("%APSIM%", "Model", LinuxExecutable)) + " " + StringManip.DQuote(SimFileName) + " > " + StringManip.DQuote(SumFileName)));
        }
        else if (Comp.Type.ToLower() == "folder" && Comp.Enabled)
        {
            foreach (Component Child in Comp.ChildNodes)
               if (Child.Type == "simulation" || Child.Type == "folder")
                   CreateApsimJobXML(Child, Node, FileName);
        }
    }
    /// <summary>
    /// For APSIM simulations, create a job that runs APSIM.
    /// </summary>
    private static void CreateConJobXML(string FileName, XmlNode Node)
    {
        List<string> SimFileNames = GetSimsInConFile(FileName);

        // Add multiple nodes to the RunApsim folder.
        foreach (string SimFileName in SimFileNames)
        {
            string SumFileName = Path.ChangeExtension(SimFileName, ".sum");
            XmlNode JobNode = Node.AppendChild(Node.OwnerDocument.CreateElement("Job"));
            XmlHelper.SetName(JobNode, "Apsim " + SimFileName);
            XmlHelper.SetValue(JobNode, "WorkingDirectory", Path.GetDirectoryName(FileName));
            XmlHelper.SetValue(JobNode, "CommandLine", ReplaceEnvironmentVariables(StringManip.DQuote(Path.Combine("%APSIM%", "Model", "Apsim.exe")) + " " + StringManip.DQuote(SimFileName) + " > " + StringManip.DQuote(SumFileName)));
            XmlHelper.SetValue(JobNode, "CommandLineUnix", ReplaceEnvironmentVariables(StringManip.DQuote(Path.Combine("%APSIM%", "Model", "Apsim.x")) + " " + StringManip.DQuote(SimFileName) + " > " + StringManip.DQuote(SumFileName)));
        }
    }

    /// <summary>
    /// Find the simulations in a .con file
    /// </summary>
    public static List<string> GetSimsInConFile(string FileName)
    {
        Process P = new Process();
        if (Configuration.getArchitecture() == Configuration.architecture.unix)
            P.StartInfo.FileName = ReplaceEnvironmentVariables(Path.Combine("%APSIM%", "Model", "ConToSim.x"));
        else
            P.StartInfo.FileName = ReplaceEnvironmentVariables(Path.Combine("%APSIM%", "Model", "ConToSim.exe"));
        P.StartInfo.UseShellExecute = false;
        P.StartInfo.ErrorDialog = false;
        P.StartInfo.RedirectStandardOutput = true;
        P.StartInfo.RedirectStandardError = true;
        P.StartInfo.RedirectStandardInput = false;
        P.StartInfo.Arguments = StringManip.DQuote(FileName);
        P.StartInfo.WorkingDirectory = Path.GetDirectoryName(FileName);
        P.Start();
        P.WaitForExit();

        List<string> SimsInConFile = new List<string>();
        if (P.ExitCode == 0)
        {
            string Line = P.StandardError.ReadLine();
            while (Line != null)
            {
                if (Line.Length > 8 && Line.Substring(0, 8) == "Written ")
                {
                    string SimFileName = Line.Substring(8);
                    if (Path.DirectorySeparatorChar == '\\') SimFileName = SimFileName.Replace('/', '\\'); else SimFileName = SimFileName.Replace('\\', '/');
                    SimsInConFile.Add(SimFileName);
                }
                Line = P.StandardError.ReadLine();
            }
            P.StandardError.Close();
        }
        return SimsInConFile;
    }

    /// <summary>
    /// .sim files need no special processing
    /// </summary>
    private static void CreateSimJobXML(string SimFileName, XmlNode Node)
    {
        if (Path.DirectorySeparatorChar == '\\') SimFileName = SimFileName.Replace('/', '\\'); else SimFileName = SimFileName.Replace('\\', '/');
        string SumFileName = Path.ChangeExtension(SimFileName, ".sum");

        XmlNode JobNode = Node.AppendChild(Node.OwnerDocument.CreateElement("Job"));
        XmlHelper.SetName(JobNode, "Apsim " + SimFileName);
        XmlHelper.SetValue(JobNode, "WorkingDirectory", Path.GetDirectoryName(SimFileName));
        XmlHelper.SetValue(JobNode, "CommandLine", ReplaceEnvironmentVariables(StringManip.DQuote(Path.Combine("%APSIM%", "Model", "Apsim.exe")) + " " + StringManip.DQuote(SimFileName) + " > " + StringManip.DQuote(SumFileName)));
        XmlHelper.SetValue(JobNode, "CommandLineUnix", ReplaceEnvironmentVariables(StringManip.DQuote(Path.Combine("%APSIM%", "Model", "Apsim.x")) + " " + StringManip.DQuote(SimFileName) + " > " + StringManip.DQuote(SumFileName)));
    }

    /// <summary>
    /// .sim files need no special processing
    /// </summary>
    private static void CreateShellExecuteJobXML(string shellCommand, XmlNode Node)
    {
        if (Path.DirectorySeparatorChar == '\\') shellCommand = shellCommand.Replace('/', '\\'); else shellCommand = shellCommand.Replace('\\', '/');

        XmlNode JobNode = Node.AppendChild(Node.OwnerDocument.CreateElement("Job"));
        XmlHelper.SetName(JobNode, shellCommand);
        XmlHelper.SetValue(JobNode, "WorkingDirectory", Path.GetDirectoryName(shellCommand));
        XmlHelper.SetValue(JobNode, "CommandLine", ReplaceEnvironmentVariables(shellCommand));
        XmlHelper.SetValue(JobNode, "CommandLineUnix", ReplaceEnvironmentVariables(shellCommand));
    }
    
    /// <summary>
    /// Look through the specified string for an environment variable name surrounded by
    /// % characters. Replace them with the environment variable value.
    /// </summary>
    public static string ReplaceEnvironmentVariables(string CommandLine)
    {
        int PosPercent = CommandLine.IndexOf('%');
        while (PosPercent != -1)
        {
            string Value = null;
            int EndVariablePercent = CommandLine.IndexOf('%', PosPercent + 1);
            if (EndVariablePercent != -1)
            {
                string VariableName = CommandLine.Substring(PosPercent + 1, EndVariablePercent - PosPercent - 1);
                Value = System.Environment.GetEnvironmentVariable(VariableName);
                if (Value == null)
                    Value = System.Environment.GetEnvironmentVariable(VariableName, EnvironmentVariableTarget.User);
            }

            if (Value != null)
            {
                CommandLine = CommandLine.Remove(PosPercent, EndVariablePercent - PosPercent + 1);
                CommandLine = CommandLine.Insert(PosPercent, Value);
                PosPercent = PosPercent + 1;
            }

            else
                PosPercent = PosPercent + 1;

            if (PosPercent >= CommandLine.Length)
                PosPercent = -1;
            else
                PosPercent = CommandLine.IndexOf('%', PosPercent);
        }
        return CommandLine;
    }

    /// <summary>
    /// A static helper method to let other classes talk to this Job Scheduler via a socket connection.
    /// The response from the JobScheduler is returned.
    /// </summary>
    public static string TalkToJobScheduler(string Data)
    {
        // Open a socket connection to JobScheduler.
        int PortNumber = 13000;  // Some arbitary number.
        IPAddress localAddr = IPAddress.Parse("127.0.0.1");
        IPEndPoint ipe = new IPEndPoint(localAddr, PortNumber);
        Socket S = new Socket(ipe.AddressFamily, SocketType.Stream, ProtocolType.Tcp);
        S.Connect(ipe);
        if (!S.Connected)
            throw new Exception("Cannot connect to JobScheduler via socket");

        // Send our XML to JobScheduler.
        Byte[] bytes = Encoding.ASCII.GetBytes(Data);
        S.Send(bytes);

        // Now wait for a response.
        S.Receive(bytes);
        S.Close();
        return bytes.ToString();
    }

}

