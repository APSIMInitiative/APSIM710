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


public class ToowoombaCluster
   {
   public static List<string> RunOnCluster(string FolderName, string Password, string EmailAddress, string ApsimVersion)
      {
      List<string> FilesToRun = new List<string>();
      Utility.FindFiles(FolderName, "*.apsim", ref FilesToRun, false);
      foreach (string FileName in FilesToRun)
         {
         ApsimFile.ApsimFile f = new ApsimFile.ApsimFile(FileName);
         List<string> SimPaths = new List<string>();
         ApsimFile.ApsimFile.ExpandSimsToRun(f.RootComponent, ref SimPaths);
         RunOnCluster(f, SimPaths, Password, EmailAddress, ApsimVersion);
         }
      return FilesToRun;
      }


   public static void RunOnCluster(ApsimFile.ApsimFile F, List<string> SimulationPaths, string Password, string EmailAddress, string ApsimVersion)
      {
      string UserName = EmailAddress;
      if (!UserName.Contains("@"))
         throw new Exception("Invalid email address: " + EmailAddress);
      UserName = UserName.Remove(UserName.IndexOf("@"));


      // Keep track of all files that need to go to the cluster.
      List<string> FilesToSend = new List<string>();

      // Create a .sub file + sim files.
      string SubFileName = Path.GetTempPath() + "Job.su~";
      CreateSubFile(F, SubFileName, SimulationPaths, ref FilesToSend);

      // Create a job file.
      string JobFileName = Path.GetTempPath() + "Job.xml";
      CreateJobFile(JobFileName, EmailAddress, ref FilesToSend);

      // Create a RunApsim.bat file
      string RunApsimFileName = Path.GetTempPath() + "Job.ba~";
      CreateRunApsimFile(RunApsimFileName, ApsimVersion, ref FilesToSend);

      // Zip the whole lot up.
      string ZipFileName = CalcZipFileName(UserName);
      UIUtility.Zip.ZipFiles(FilesToSend, ZipFileName, "");

      // Email the file to the run machine.
      //SendEmailToRunMachine(ZipFileName, EmailAddress)
      SendToWebService(ZipFileName, Password);

      // Remove temporary files.
      File.Delete(SubFileName);
      File.Delete(JobFileName);
      File.Delete(RunApsimFileName);
      File.Delete(ZipFileName);
      foreach (string SimFileName in Directory.GetFiles(Path.GetDirectoryName(F.FileName), "*.sim"))
         {
         File.Delete(SimFileName);
         }
      }

   public static void CreateRunApsimFile(string FileName, string ApsimVersion, ref List<string> FilesToSend)
      {
      FilesToSend.Add(FileName);
      StreamWriter Bat = new StreamWriter(FileName);
      Bat.WriteLine("\"c:\\Program files\\Apsim" + ApsimVersion.Replace(".", "") + "\\Model\\Apsim.exe\" \"%1\"");
      Bat.Close();
      }


   public static void CreateSubFile(ApsimFile.ApsimFile F, string SubFileName, List<string> SimulationPaths, ref List<string> FilesToSend)
      {
      FilesToSend.Add(SubFileName);

      StreamWriter SubWriter = new StreamWriter(SubFileName);
      SubWriter.WriteLine("universe = vanilla");
      SubWriter.WriteLine("requirements = (OpSys == \"WINNT51\" || OpSys == \"WINNT52\") && APSIM_INSTALLED =?= True");
      SubWriter.WriteLine("log = Job.condorlog");
      SubWriter.WriteLine("nice_user = True");
      SubWriter.WriteLine("executable = Job.bat");

      // For each simulation write a section to a sub file.
      foreach (string SimulationPath in SimulationPaths)
         {
         string InputFiles = "Job.bat";

         Component Simulation = F.Find(SimulationPath);
         string SimFileName = ApsimToSim.WriteSimFile(Simulation);
         FilesToSend.Add(SimFileName);
         InputFiles = InputFiles + "," + Path.GetFileName(SimFileName);

         // Analyse the .sim file looking for <FileName> elements. If these elements are in other directories then change
         // the sim file to make them local files.
         XmlDocument Doc = new XmlDocument();
         Doc.Load(SimFileName);
         List<XmlNode> FileNameNodes = new List<XmlNode>();
         XmlHelper.FindAllRecursively(Doc.DocumentElement, "FileName", ref FileNameNodes);
         foreach (XmlNode FileNameNode in FileNameNodes)
            {
            string ReferencedFileName = Configuration.RemoveMacros(FileNameNode.InnerText);
            if (!FilesToSend.Contains(ReferencedFileName))
               FilesToSend.Add(ReferencedFileName);
            FileNameNode.InnerText = Path.GetFileName(FileNameNode.InnerText);
            InputFiles = InputFiles + "," + FileNameNode.InnerText;
            }
         Doc.Save(SimFileName);


         SubWriter.WriteLine("arguments = \" '" + Path.GetFileName(SimFileName) + "' \"");
         SubWriter.WriteLine("output = " + Path.GetFileName(Path.ChangeExtension(SimFileName, ".sum")));
         SubWriter.WriteLine("error = " + Path.GetFileName(Path.ChangeExtension(SimFileName, ".stderr")));
         SubWriter.WriteLine("transfer_input_files = " + InputFiles);
         SubWriter.WriteLine("queue");
         SubWriter.WriteLine("");
         }
      SubWriter.Close();
      }

   public static void CreateJobFile(string JobFileName, string EmailAddress, ref List<string> FilesToSend)
      {
      FilesToSend.Add(JobFileName);

      string UserName = EmailAddress;
      if (!UserName.Contains("@"))
         {
         throw new Exception("Invalid email address: " + EmailAddress);
         }
      UserName = UserName.Remove(UserName.IndexOf("@"));

      XmlDocument Doc = new XmlDocument();
      XmlNode JobNode = Doc.AppendChild(Doc.CreateElement("Job"));
      XmlHelper.SetName(JobNode, UserName);

      // Create <cluster> node
      XmlHelper.SetValue(JobNode, "Cluster/SubFile", "Job.su~");

      // zip up the outputs.
      string ZipFileName = CalcZipFileName(UserName); 
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

   private static string CalcZipFileName(string UserName)
      {
      string ZipFileName = UserName + "(" + DateAndTime.Now.ToShortDateString() + " " + DateAndTime.Now.ToShortTimeString() + ").zip";
      ZipFileName = ZipFileName.Replace(" ", "_");
      ZipFileName = ZipFileName.Replace("/", ".");
      ZipFileName = ZipFileName.Replace(":", ".");

      return ZipFileName;
      }

   public static void SendToWebService(string ZipFileName, string Password)
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