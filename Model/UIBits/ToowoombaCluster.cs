using Microsoft.VisualBasic;
using System;
using System.Collections;
using System.Collections.Generic;
using System.Data;
using System.Diagnostics;
using System.IO;
using System.Xml;
using ApsimFile;
using CSGeneral;
using System.Net.Mail;
using System.Windows.Forms;


public class ToowoombaCluster
{
    public delegate void ProgressNotifier(int Percent, string Text);

    public static void RunOnCluster(List<string> FilesToRun, string FinalFolder, bool ZipAllFiles, bool ConverToSim, string ApsimVersion, ProgressNotifier Notifier)
    {
        Directory.CreateDirectory(FinalFolder);
        string DestFolder = FinalFolder;
        if (ZipAllFiles)
            DestFolder = Path.GetTempPath() + "\\Cluster";
        Directory.CreateDirectory(DestFolder);

        // Create a RunApsim.bat file
        string RunApsimFileName = DestFolder + "\\Job.bat";
        CreateRunApsimFile(RunApsimFileName, ApsimVersion);

        // Create a .sub file
        StreamWriter SubWriter = new StreamWriter(DestFolder + "\\Job.su~");
        SubWriter.WriteLine("universe = vanilla");
        SubWriter.WriteLine("requirements = (OpSys == \"WINNT51\" || OpSys == \"WINNT52\") && APSIM_INSTALLED =?= True");
        SubWriter.WriteLine("log = Job.condorlog");
        SubWriter.WriteLine("nice_user = True");
        SubWriter.WriteLine("executable = Job.bat");

        double i = 0;
        foreach (string FileName in FilesToRun)
        {
            Notifier((int) (i / FilesToRun.Count * 100.0), "Analysing " + FileName);
            i++;

            // Create a .sub file + sim files.
            CreateSubFile(FileName, DestFolder, SubWriter, ConverToSim);
        }
        SubWriter.Close();

        File.Move(DestFolder + "\\Job.su~", DestFolder + "\\Job.sub");

        // Zip the whole lot up.
        if (ZipAllFiles)
        {
            Notifier(100, "Zipping all files...");
            string ZipFileName = FinalFolder + "\\" + CalcZipFileName();

            string[] FilesToZip = Directory.GetFiles(DestFolder, "*.*");
            UIUtility.Zip.ZipFiles(FilesToZip, ZipFileName, "");

            // Remove all unwanted files
            Notifier(100, "Removing all temporary files...");

            Directory.Delete(DestFolder, true);
        }

        Notifier(100, "Done.");
    }


    private static void ModifyApsimFile(XmlNode RootNode, string FolderName, string DestFolder)
    {

        List<XmlNode> FileNameNodes = new List<XmlNode>();
        XmlHelper.FindAllRecursively(RootNode, "FileName", ref FileNameNodes);
        foreach (XmlNode FileNameNode in FileNameNodes)
        {
            if (FileNameNode.ParentNode.Name != "summaryfile" && FileNameNode.ParentNode.Name != "outputfile" &&
                !FileNameNode.ParentNode.Name.Contains("ApsimFileReader"))
            {
                string ReferencedFileName =  Path.Combine(FolderName, Configuration.RemoveMacros(FileNameNode.InnerText));
                string DestFileName = Path.Combine(DestFolder, Path.GetFileName(ReferencedFileName));
                if (!File.Exists(DestFileName))
                    File.Copy(ReferencedFileName, DestFileName);
                FileNameNode.InnerText = Path.GetFileName(FileNameNode.InnerText);
            }
        }
    }

    public static void CreateRunApsimFile(string FileName, string ApsimVersion)
    {
        StreamWriter Bat = new StreamWriter(FileName);
        Bat.WriteLine("if [%~x1] == [.apsim] (");
        Bat.WriteLine("   \"c:\\Program files\\" + ApsimVersion.Replace(".", "") + "\\Model\\ApsimToSim.exe\" .\\\\%1");
        Bat.WriteLine("   )");
        Bat.WriteLine("for %%f in (*.sim) do \"c:\\Program files\\" + ApsimVersion + "\\Model\\Apsim.exe\" \"%%f\"");
        Bat.Close();
    }


    public static void CreateSubFile(string FileName, string DestFolder, StreamWriter SubWriter, bool ConvertToSim)
    {
        if (ConvertToSim)
        {
            XmlDocument Doc = new XmlDocument();
            Doc.Load(FileName);

            // Get a list of simulation paths in the .apsim file.
            ApsimFile.ApsimFile F = new ApsimFile.ApsimFile();
            F.Open(Doc.DocumentElement);
            List<string> SimulationPaths = new List<string>();
            ApsimFile.ApsimFile.ExpandSimsToRun(F.RootComponent, ref SimulationPaths);

            // For each simulation write a section to a sub file.
            foreach (string SimulationPath in SimulationPaths)
            {

                Component Simulation = F.Find(SimulationPath);
                string SimFileName = ApsimToSim.WriteSimFile(Simulation, DestFolder);
                //if (SimFileName.Contains(" "))
                //{
                //    string NewSimFileName = SimFileName.Replace(" ", "");
                //    if (File.Exists(NewSimFileName))
                //        File.Delete(NewSimFileName);
                //    File.Move(SimFileName, NewSimFileName);
                //    SimFileName = NewSimFileName;
                //}

                // Analyse the .sim file looking for <FileName> elements. If these elements are in other directories then change
                // the sim file to make them local files.
                AnalyseFile(SimFileName, Path.GetDirectoryName(FileName), DestFolder, SubWriter);
            }
        }
        else
        {
            string DestFileName = DestFolder + "\\" + Path.GetFileName(FileName);
            File.Copy(FileName, DestFileName, true);
            AnalyseFile(DestFileName, Path.GetDirectoryName(FileName), DestFolder, SubWriter);
        }

    }

    private static void AnalyseFile(string FileName, string SourceDirectory, string DestFolder, StreamWriter SubWriter)
    {
        string InputFiles = "Job.bat," + Path.GetFileName(FileName);

        XmlDocument Doc = new XmlDocument();
        Doc.Load(FileName);
        List<XmlNode> FileNameNodes = new List<XmlNode>();
        XmlHelper.FindAllRecursively(Doc.DocumentElement, "FileName", ref FileNameNodes);
        foreach (XmlNode FileNameNode in FileNameNodes)
        {
            if (FileNameNode.ParentNode.Name != "summaryfile" && FileNameNode.ParentNode.Name != "outputfile" &&
                !FileNameNode.ParentNode.Name.Contains("ApsimFileReader"))
            {
                string ReferencedFileName = Configuration.RemoveMacros(FileNameNode.InnerText);
                ReferencedFileName = Path.Combine(SourceDirectory, ReferencedFileName);
                if (!File.Exists(ReferencedFileName))
                    throw new Exception("Cannot find file: " + ReferencedFileName);

                string DestFileName = Path.Combine(DestFolder, Path.GetFileName(ReferencedFileName));
                if (!File.Exists(DestFileName))
                    File.Copy(ReferencedFileName, DestFileName);

                FileNameNode.InnerText = Path.GetFileName(FileNameNode.InnerText);
                InputFiles = InputFiles + "," + FileNameNode.InnerText;
            }
        }
        Doc.Save(FileName);

        // Write to sub file.
        string BaseFileNameNoSpaces = Path.GetFileName(FileName).Replace(" ", "");
        SubWriter.WriteLine("arguments = \" '" + Path.GetFileName(FileName) + "' \"");
        SubWriter.WriteLine("output = " + Path.ChangeExtension(BaseFileNameNoSpaces, ".sum"));
        SubWriter.WriteLine("error = " + Path.ChangeExtension(BaseFileNameNoSpaces, ".stderr"));
        SubWriter.WriteLine("transfer_input_files = " + InputFiles);
        SubWriter.WriteLine("queue");
        SubWriter.WriteLine("");
    }

    public static void CreateJobFile(ApsimFile.ApsimFile F, string JobFileName, string EmailAddress, string ApsimVersion)
    {
        string UserName = EmailAddress;
        if (!UserName.Contains("@"))
        {
            throw new Exception("Invalid email address: " + EmailAddress);
        }
        UserName = UserName.Remove(UserName.IndexOf("@"));

        XmlDocument Doc = new XmlDocument();
        XmlNode JobNode = Doc.AppendChild(Doc.CreateElement("Job"));
        XmlHelper.SetName(JobNode, UserName);

        // Create <localcommand> node
        XmlHelper.SetValue(JobNode, "LocalCommand/CommandLine", "\"c:\\Program files\\Apsim" + ApsimVersion.Replace(".", "") + "\\Model\\ApsimToSim.exe\" \"%jobfolder%\\" + Path.GetFileName(F.FileName) + "\"");

        // Create <cluster> node
        XmlHelper.SetValue(JobNode, "Cluster/CommandLine", "Job.bat %f");
        XmlHelper.SetValue(JobNode, "Cluster/NiceUser", "Yes");
        XmlNode ClusterNode = XmlHelper.Find(JobNode, "Cluster");
        XmlHelper.SetAttribute(ClusterNode, "foreach", "*.sim");

        // zip up the outputs.
        string ZipFileName = CalcZipFileName();
        XmlHelper.SetValue(JobNode, "Zip/To", ZipFileName);

        // ftp the zip outputs to apsrunet2.
        XmlHelper.SetValue(JobNode, "Ftp/From", ZipFileName);
        XmlHelper.SetValue(JobNode, "Ftp/ToSite", "apsrunet2.apsim.info");
        XmlHelper.SetValue(JobNode, "Ftp/ToFolder", "Cluster");

        // Create an email node.
        XmlHelper.SetValue(JobNode, "Email/Subject", "Your cluster job has been completed");
        XmlHelper.SetValue(JobNode, "Email/To", EmailAddress);
        string Url = "http://www.apsim.info/Cluster/" + ZipFileName;
        XmlHelper.SetValue(JobNode, "Email/Body", "To access your job outputs, goto <a href=\"" + Url + "\">" + Url + "</a>");

        // Create a log node.
        XmlHelper.SetValue(JobNode, "Log/Description", "Cluster run by " + EmailAddress + ", Cluster");

        Doc.Save(JobFileName);
    }

    private static string CalcZipFileName()
    {
        string ZipFileName = DateAndTime.Now.ToShortDateString() + "(" + DateAndTime.Now.ToShortTimeString() + ").zip";
        ZipFileName = ZipFileName.Replace(" ", ".");
        ZipFileName = ZipFileName.Replace("/", ".");
        ZipFileName = ZipFileName.Replace(":", ".");

        return ZipFileName;
    }

    private static void SendToWebService(string ZipFileName, string Password)
    {
        FileInfo fInfo = new FileInfo(ZipFileName);
        long numBytes = fInfo.Length;

        FileStream fStream = new FileStream(ZipFileName, FileMode.Open, FileAccess.Read);
        BinaryReader br = new BinaryReader(fStream);
        byte[] Data = br.ReadBytes((int)numBytes);
        br.Close();

        UIBits.ClusterService.Cluster s = new UIBits.ClusterService.Cluster();
        s.Timeout = 300000;
        string msg = s.UploadFile(Data, Path.GetFileName(ZipFileName), Password);
        if (msg != "OK")
        {
            throw new Exception("Error: " + msg);
        }
    }

}