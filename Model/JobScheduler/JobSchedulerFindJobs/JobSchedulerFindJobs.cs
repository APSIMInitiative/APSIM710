using System;
using System.Collections.Generic;
using System.Text;
using System.IO;
using CSGeneral;
using System.Net;
using System.Net.Sockets;
using System.Threading;
using System.Xml;
using ApsimFile;

class Program
{
    /// <summary>
    /// This program locates files and adds them as jobs to the JobScheduler.
    /// The first argument is the filespec (including directory path) e.g. 
    ///   c:\Apsim\Examples\*.apsim.   
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
        string FileSpec = Path.GetFileName(DirFileSpec);

        List<string> FileNames = new List<string>();
        Utility.FindFiles(RootDirectory, FileSpec, ref FileNames, false);

        // For each filename found, add a job for each simulation in each file.
        string XML = "AddXML~" + NodePath + "~";
        XmlDocument Doc = new XmlDocument();
        Doc.AppendChild(Doc.CreateElement("Build"));
        foreach (string FileName in FileNames)
        {
            try
            {
                if (Path.GetExtension(FileName).ToLower() == ".con" ||
                    Path.GetExtension(FileName).ToLower() == ".apsim")
                    CreateApsimJobXML(FileName, Doc.DocumentElement);
                else
                {
                    XmlNode NewJobNode = Doc.CreateElement("Job");
                    Doc.DocumentElement.AppendChild(NewJobNode);
                    XmlHelper.SetName(NewJobNode, StringManip.DQuote(FileName));
                    XmlHelper.SetValue(NewJobNode, "WorkingDirectory", Path.GetDirectoryName(FileName));
                    XmlHelper.SetValue(NewJobNode, "CommandLine", Path.GetFileName(FileName));
                }
            }
            catch (Exception err)
            {
                throw new Exception(err.Message + ". File name = " + FileName);
            }
        }
        XML += Doc.DocumentElement.OuterXml;
        TalkToJobScheduler(XML);
    }

    /// <summary>
    /// For APSIM simulations, create a special type of job that not only runs
    /// ApsimToSim (or ConToSim) but also APSIM.
    /// </summary>
    private static void CreateApsimJobXML(string FileName, XmlNode RootNode)
    {
        XmlNode CreateSimsNode = XmlHelper.Find(RootNode, "CreateSims");
        XmlNode RunApsimNode = XmlHelper.Find(RootNode, "RunApsim");
        if (CreateSimsNode == null)
        {
            CreateSimsNode = RootNode.OwnerDocument.CreateElement("Folder");
            RootNode.AppendChild(CreateSimsNode);
            XmlHelper.SetName(CreateSimsNode, "CreateSims");

            RunApsimNode = RootNode.OwnerDocument.CreateElement("Folder");
            RootNode.AppendChild(RunApsimNode);
            XmlHelper.SetName(RunApsimNode, "RunApsim");
            //XmlHelper.SetAttribute(RunApsimNode, "wait", "yes");
        }

        string SimConvTool;
        List<string> SimFileNames = new List<string>();
        if (Path.GetExtension(FileName).ToLower() == ".con")
        {
            SimFileNames = ConFile.GetSimsInConFile(FileName);
            SimConvTool = "ConToSim.exe";
            for (int i = 0; i < SimFileNames.Count; i++)
                SimFileNames[i] = Path.GetFileNameWithoutExtension(FileName)
                                  + "." + SimFileNames[i];

        }
        else
        {
            SimConvTool = "ApsimToSim.exe";
            SimFileNames = ApsimFile.ApsimFile.GetSimNamesInApsimFile(FileName);
        }
        for (int i = 0; i < SimFileNames.Count; i++)
            SimFileNames[i] += ".sim";

        // Add a node to CreateSims folder
        XmlNode JobNode = CreateSimsNode.AppendChild(RootNode.OwnerDocument.CreateElement("Job"));
        XmlHelper.SetName(JobNode, SimConvTool + " " + FileName);
        XmlHelper.SetValue(JobNode, "WorkingDirectory", "%Apsim%\\Model");
        XmlHelper.SetValue(JobNode, "CommandLine", SimConvTool + " " + StringManip.DQuote(FileName));

        // Add multiple nodes to the RunApsim folder.
        foreach (string SimFileName in SimFileNames)
        {
            string FullSimFileName = Path.GetDirectoryName(FileName) + "\\" + SimFileName;
            string SumFileName = Path.ChangeExtension(FullSimFileName, ".sum");

            JobNode = RunApsimNode.AppendChild(RootNode.OwnerDocument.CreateElement("Job"));
            XmlHelper.SetName(JobNode, "Apsim.exe " + FullSimFileName);
            XmlHelper.SetValue(JobNode, "WorkingDirectory", "%Apsim%\\Model");
            XmlHelper.SetValue(JobNode, "CommandLine", "Apsim.exe " + StringManip.DQuote(FullSimFileName) + "> " + StringManip.DQuote(SumFileName));
        }
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
   