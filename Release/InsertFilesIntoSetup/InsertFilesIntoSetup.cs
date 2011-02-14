using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Xml;
using System.IO;
using CSGeneral;
using System.Collections.Specialized;
using ApsimFile;

class InsertFilesIntoSetup
   {
   private int LastID = 0;
   private string[] Exclusions;

   /// <summary>
   /// Main program. Returns 1 on error. Args:
   ///    .vdproj    Wildcard    Directory
   /// The WildCard can be either a valid directory name or a wildcard.
   /// TargetDirectory is the target folder name starting with a %Apsim%
   ///    e.g. %Apsim%\UserInterface
   /// </summary>
   static int Main(string[] args)
      {
      try
         {
         if (args.Length != 3 && args.Length != 4)
            throw new Exception("Args: SetupFileName  [/WithExclusions] WildCard  TargetDirectory");
         if (!File.Exists(args[0]))
            throw new Exception("Cannot find file: " + args[0]);

         InsertFilesIntoSetup Tool = new InsertFilesIntoSetup();

         Tool.Go(args);

         return 0;
         }
      catch (Exception e)
         {
         Console.Write("Error: " + e.Message);
         return 1;
         }
      }

   /// <summary>
   /// Go do work
   /// </summary>
   private void Go(string[] args)
      {
      string SourceFileName = args[0];
      string ExclusionsFileName = "";
      string WildCard;
      string TargetDirectory;
      if (args.Length == 4)
         {
         ExclusionsFileName = "Exclusions.txt";
         WildCard = args[2];
         TargetDirectory = args[3];
         }
      else
         {
         WildCard = args[1];
         TargetDirectory = args[2];
         }

      // read .vdproj into an XML document.
      StreamReader In = new StreamReader(SourceFileName);
      string RootObjName = In.ReadLine().Replace("\"", "");
      XmlDocument Doc = new XmlDocument();
      ReadObjectIntoXml(In, Doc, RootObjName);
      In.Close();

      // Read in exclusions list.
      if (ExclusionsFileName != "")
         {
         In = new StreamReader(ExclusionsFileName);
         string Contents = In.ReadToEnd();
         Exclusions = Contents.Split("\r\n".ToCharArray(), StringSplitOptions.RemoveEmptyEntries);
         }

      // Write to XML file - for debugging purposes.
      string XmlFileName = Path.ChangeExtension(SourceFileName, ".xml");
      Doc.Save("Test.xml");

      InsertFilesIntoXML(WildCard, Doc.DocumentElement, TargetDirectory);

      // Update the ProductCode and ProductVersion.
      UpdateVersionInfo(Doc.DocumentElement);

      // write to .vdproj
      //string NewFileName = Path.ChangeExtension(SourceFileName, ".vdprojnew");
      string NewFileName = SourceFileName;
      StreamWriter Out = new StreamWriter(NewFileName);
      WriteVDProj(Doc.DocumentElement, Out, 0);
      Out.Close();
      }

   /// <summary>
   /// Insert files that match the specified WildCard into the XML document.
   /// </summary>
   private void InsertFilesIntoXML(string WildCard, XmlNode RootNode, string TargetDirectory)
      {
      if (Directory.Exists(WildCard))
         {
         if ((File.GetAttributes(WildCard) & FileAttributes.Hidden) != FileAttributes.Hidden)
            {
            // Directory - need to recurse.
            InsertFilesIntoXML(WildCard + "\\*.*", RootNode, TargetDirectory);
            foreach (string SubDirectory in Directory.GetDirectories(WildCard))
               InsertFilesIntoXML(SubDirectory, RootNode, 
                                  TargetDirectory + "\\" + 
                                  Path.GetFileName(SubDirectory)); // recursion
            }
         }
      else
         {
         // Wildcard - no recursion.
         foreach (string FileName in Directory.GetFiles(Path.GetDirectoryName(WildCard),
                                                        Path.GetFileName(WildCard)))
            InsertFileIntoXML(FileName, RootNode, TargetDirectory);
         }
      }

   /// <summary>
   /// Insert the specified file into the XML.
   /// </summary>
   private void InsertFileIntoXML(string FileName, XmlNode RootNode, string TargetDirectory)
      {
      // Get the FileNode in the XML. This will be the parent for our new file object.
      XmlNode FileNode = XmlHelper.Find(RootNode, "Deployable/File");
      if (FileNode == null)
         throw new Exception("Cannot find <File> node");

      // See if this file is in our exclusion list.
      bool ExcludeFile = false;
      if (Exclusions != null)
         {
         foreach (string ExcludeFileName in Exclusions)
            {
            ExcludeFile = FileName.Contains(ExcludeFileName);
            if (ExcludeFile)
               break;
            }
         }
      
      if (!ExcludeFile &&
          !FileAlreadyExists(FileName, RootNode, TargetDirectory) && 
          Path.GetExtension(FileName) != ".out" &&
          Path.GetExtension(FileName) != ".sum" &&
          Path.GetExtension(FileName) != ".csv" &&
          Path.GetExtension(FileName) != ".nc")
         {

         // Get the ID of the target directory object.
         string FolderID = GetFolderID(RootNode, TargetDirectory);


         // Create and populate an object for our file.
         string ID = CreateNewID();
         XmlNode ObjectNode = FileNode.AppendChild(FileNode.OwnerDocument.CreateElement("Object"));
         XmlHelper.SetName(ObjectNode, "1FB2D0AE-D3B9-43D4-B9DD-F88EC61E35DE");
         XmlHelper.SetAttribute(ObjectNode, "id", ID);
         XmlHelper.SetValue(ObjectNode, "SourcePath", "8:" + FileName.Replace("\\", "\\\\"));
         XmlHelper.SetValue(ObjectNode, "TargetName", "8:" + Path.GetFileName(FileName));
         XmlHelper.SetValue(ObjectNode, "Tag", "8:");
         XmlHelper.SetValue(ObjectNode, "Folder", "8:" + FolderID);
         XmlHelper.SetValue(ObjectNode, "Condition", "8:");
         XmlHelper.SetValue(ObjectNode, "Transitive", "11:FALSE");
         XmlHelper.SetValue(ObjectNode, "Vital", "11:TRUE");
         XmlHelper.SetValue(ObjectNode, "ReadOnly", "11:FALSE");
         XmlHelper.SetValue(ObjectNode, "Hidden", "11:FALSE");
         XmlHelper.SetValue(ObjectNode, "System", "11:FALSE");
         XmlHelper.SetValue(ObjectNode, "Permanent", "11:FALSE");
         XmlHelper.SetValue(ObjectNode, "SharedLegacy", "11:FALSE");
         XmlHelper.SetValue(ObjectNode, "PackageAs", "3:1");
         XmlHelper.SetValue(ObjectNode, "Register", "3:1");
         XmlHelper.SetValue(ObjectNode, "Exclude", "11:FALSE");
         XmlHelper.SetValue(ObjectNode, "IsDependency", "11:FALSE");
         XmlHelper.SetValue(ObjectNode, "IsolateTo", "8:");

         // Add and entry to "Hierarchy"
         XmlNode Hierarchy = XmlHelper.Find(RootNode, "Hierarchy");
         XmlNode Entry = Hierarchy.AppendChild(Hierarchy.OwnerDocument.CreateElement("Entry"));
         XmlHelper.SetValue(Entry, "MsmKey", "8:" + ID);
         XmlHelper.SetValue(Entry, "OwnerKey", "8:_UNDEFINED");
         XmlHelper.SetValue(Entry, "MsmSig", "8:_UNDEFINED");
         }
      }

   /// <summary>
   /// Return true if the file is already in the install list for the specified TargetDirectory.
   /// </summary>
   private bool FileAlreadyExists(string FileName, XmlNode RootNode, string TargetDirectory)
      {
      XmlNode FileNode = XmlHelper.Find(RootNode, "Deployable/File");
      string FolderID = GetFolderID(RootNode, TargetDirectory);

      string TargetName = Path.GetFileName(FileName).ToLower();
      bool Found = false;
      foreach (XmlNode FileObject in XmlHelper.ChildNodes(FileNode, "Object"))
         {
         if (XmlHelper.Value(FileObject, "Folder") == "8:" + FolderID)
            {
            string NameToMatch;

            if (XmlHelper.Value(FileObject, "TargetName") == "8:") // A auto-dependency.
               NameToMatch = XmlHelper.Value(FileObject, "SourcePath");
            else
               NameToMatch = XmlHelper.Value(FileObject, "TargetName");
            // remove the 8: from in front of the nametomatch
            NameToMatch = NameToMatch.Substring(2);
            Found = Path.GetFileName(NameToMatch).ToLower() == Path.GetFileName(FileName).ToLower();
            if (Found) break;
            }
         }
      return Found;
      }

   /// <summary>
   /// Go locate and if necessary create, our TargetDirectory. Returns the ID to caller.
   /// TargetDirectory should be something like %Apsim%\UserInterface
   /// </summary>
   private string GetFolderID(XmlNode RootNode, string TargetDirectory)
      {
      XmlNode FolderNode = XmlHelper.Find(RootNode, "Deployable/Folder");
      if (FolderNode == null)
         throw new Exception("Cannot find <Folder> node");

      // Find the target folder.
      XmlNode TargetDirNode = null;
      foreach (XmlNode Child in FolderNode.ChildNodes)
         {
         if (XmlHelper.Value(Child, "Property") == "8:TARGETDIR")
            TargetDirNode = Child;
         }
      if (TargetDirNode == null)
         throw new Exception("Cannot find folder object with a property of: 8:TARGETDIR");

      if (!TargetDirectory.Contains("%Apsim%"))
         throw new Exception("TargetDirectory argument must start with %Apsim%");
      TargetDirectory = TargetDirectory.Replace("%Apsim%", "");
      if (TargetDirectory != "" && TargetDirectory[0] == '\\')
         TargetDirectory = TargetDirectory.Remove(0, 1);  // remove leading /
      return FindOrCreate(TargetDirNode, TargetDirectory);
      }

   /// <summary>
   /// Go find or create the specified ChildDirectory under DirNode.
   /// ChildDirectory should be something like: UserInterface i.e. relative to 
   /// the DirNode passed in.
   /// </summary>
   private string FindOrCreate(XmlNode DirNode, string ChildDirectory)
      {
      // If the child directory is empty then return DirNode's ID.
      if (ChildDirectory == "")
         return XmlHelper.Attribute(DirNode, "id");

      // Get the folders node.
      XmlNode FoldersNode = XmlHelper.Find(DirNode, "Folders");
      if (FoldersNode == null)
         throw new Exception("Cannot find <Folders> node under: " + XmlHelper.Value(DirNode, "Property"));

      string Remainder = "";
      int PosDelimiter = ChildDirectory.IndexOf('\\');
      if (PosDelimiter != -1)
         {
         Remainder = ChildDirectory.Substring(PosDelimiter + 1);
         ChildDirectory = ChildDirectory.Substring(0, PosDelimiter);
         }

      // Look for our child directory.
      XmlNode ChildDirectoryNode = null;
      foreach (XmlNode Child in XmlHelper.ChildNodes(FoldersNode, "Object"))
         {
         string Name = XmlHelper.Value(Child, "Name");
         int PosColon = Name.IndexOf(':');
         if (PosColon != -1 && Name.Substring(PosColon + 1).ToLower() == ChildDirectory.ToLower())
            {
            ChildDirectoryNode = Child;
            break;
            }
         }

      // If the child directory wasn't found then add one.
      if (ChildDirectoryNode == null)
         {
         ChildDirectoryNode = FoldersNode.AppendChild(FoldersNode.OwnerDocument.CreateElement("Object"));
         XmlHelper.SetName(ChildDirectoryNode, "9EF0B969-E518-4E46-987F-47570745A589");
         XmlHelper.SetAttribute(ChildDirectoryNode, "id", CreateNewID());
         XmlHelper.SetValue(ChildDirectoryNode, "Name", "8:" + ChildDirectory);
         XmlHelper.SetValue(ChildDirectoryNode, "AlwaysCreate", "11:FALSE");
         XmlHelper.SetValue(ChildDirectoryNode, "Condition", "8:");
         XmlHelper.SetValue(ChildDirectoryNode, "Transitive", "11:FALSE");
         XmlHelper.SetValue(ChildDirectoryNode, "Property", "8:" + CreateNewID());
         ChildDirectoryNode.AppendChild(FoldersNode.OwnerDocument.CreateElement("Folders"));
         }

      // If we have a remainder then recurse to create the rest of the path.
      if (Remainder != "")
         return FindOrCreate(ChildDirectoryNode, Remainder);
      else
         return XmlHelper.Attribute(ChildDirectoryNode, "id");
      }

   /// <summary>
   /// Create a new unique ID and return it.
   /// </summary>
   private string CreateNewID()
      {
      LastID++;
      return "_" + LastID;
      }

   /// <summary>
   /// Read the next object from the input stream and store under the
   /// specified parent XML node.
   /// </summary>
   private XmlNode ReadObjectIntoXml(StreamReader In, XmlNode ParentNode, string ObjName)
      {
      XmlNode ObjNode = null;
      if (ParentNode is XmlDocument)
         ObjNode = ParentNode.AppendChild(((XmlDocument)ParentNode).CreateElement(ObjName));
      else
         ObjNode = ParentNode.AppendChild(ParentNode.OwnerDocument.CreateElement(ObjName));
      // Next line should be a {
      if (In.ReadLine().Trim() != "{")
         throw new Exception("Was expecting a { character");

      string Line = In.ReadLine().Trim();
      while (Line != "}")
         {
         if (Line.Contains("="))
            {
            Line = Line.Replace("\\\"", "~");
            StringCollection LineBits = StringManip.SplitStringHonouringQuotes(Line, "=");
            if (LineBits.Count != 2)
               throw new Exception("Unknown type of line: " + Line);
            LineBits[0] = LineBits[0].Replace("~", "\\\"");
            LineBits[1] = LineBits[1].Replace("~", "\\\"");

            // Strip of a leading and trailing " chars.
            LineBits[0] = CleanName(LineBits[0]);
            LineBits[1] = CleanName(LineBits[1]);
            XmlHelper.SetValue(ObjNode, LineBits[0], LineBits[1]);
            }
         else if (Line.Contains(":"))
            {
            String[] LineBits = Line.Split(":".ToCharArray(), 
                                           StringSplitOptions.RemoveEmptyEntries);
            if (LineBits.Length != 2)
               throw new Exception("Unknown type of line: " + Line);

            XmlNode ChildNode = ReadObjectIntoXml(In, ObjNode, "Object");

            string ChildObjName = LineBits[0].Replace("\"", "").Replace("{", "").Replace("}", "");
            string ID = LineBits[1].Replace("\"", "").Replace("{", "").Replace("}", "");

            int IDNum;
            if (Int32.TryParse(ID.Substring(1), out IDNum))
               LastID = Math.Max(LastID, IDNum);

            XmlHelper.SetAttribute(ChildNode, "name", ChildObjName);
            XmlHelper.SetAttribute(ChildNode, "id", ID);            
            }
         else
            {
            string ChildObjName = Line.Replace("\"", "");
            ReadObjectIntoXml(In, ObjNode, ChildObjName);
            }
         Line = In.ReadLine().Trim();
         }
      return ObjNode;
      }

   private string CleanName(string Name)
      {
      if (Name[0] == '\"')
         Name = Name.Substring(1);
      if (Name[Name.Length - 1] == '\"')
         Name = Name.Remove(Name.Length - 1);
      return Name;
      }

   /// <summary>
   /// Write a .vdproj file from the specified XMLNode.
   /// </summary>
   private void WriteVDProj(XmlNode Node, StreamWriter Out, int Level)
      {
      string Indentation = new String(' ', Level * 4);
      string NodeName;
      if (Node.Name == "Object")
         NodeName = VDProjObjectName(Node);
      else
         NodeName = Node.Name;

      Out.WriteLine(Indentation + StringManip.DQuote(NodeName));
      Out.WriteLine(Indentation + "{");
      foreach (XmlNode Child in Node.ChildNodes)
         {
         if (Child.ChildNodes.Count == 1 && 
             (Child.ChildNodes[0].NodeType.ToString() == "Text" ||
             Child.ChildNodes[0].NodeType.ToString() == "CDATA"))
            {
            // property.
            Out.WriteLine(Indentation + StringManip.DQuote(Child.Name) +
                          " = " +
                          StringManip.DQuote(Child.InnerText));
            }
         else
            {
            WriteVDProj(Child, Out, Level + 1);
            }


         }
      Out.WriteLine(Indentation + "}");
      }

   /// <summary>
   /// Create a object name to write to the .vdproj file.
   /// </summary>
   private string VDProjObjectName(XmlNode Node)
      {
      string FirstBit = XmlHelper.Name(Node);
      string SecondBit = XmlHelper.Attribute(Node, "id");
      if (FirstBit.Contains("-"))
         FirstBit = "{" + FirstBit + "}";
      if (SecondBit.Contains("-"))
         SecondBit = "{" + SecondBit + "}";
      return FirstBit + ":" + SecondBit;
      }

   /// <summary>
   /// Update the ProductCode and ProductVersion
   /// </summary>
   private void UpdateVersionInfo(XmlNode Node)
      {
      string ProductName = "APSIM " + Configuration.Instance.ApsimVersion()
                              + " " +
                              Configuration.Instance.ApsimBuildNumber().Replace(" ", "");
      string ProductVersion = Configuration.Instance.ApsimVersion().Replace(".", "");
      string ProductCode = Guid.NewGuid().ToString().ToUpper();
      string DefaultLocation = "[ProgramFilesFolder]\\\\Apsim" + 
                               Configuration.Instance.ApsimVersion().Replace(".", "")
                               + "-" +
                               Configuration.Instance.ApsimBuildNumber().Replace(" ", "");

      XmlHelper.SetValue(Node, "Deployable/Product/ProductName", "8:" + ProductName);
      XmlHelper.SetValue(Node, "Deployable/Product/ProductVersion", "8:" + ProductVersion);
      XmlHelper.SetValue(Node, "Deployable/Folder/3C67513D-01DD-4637-8A68-80971EB9504F/DefaultLocation",
                               "8:" + DefaultLocation);
      XmlHelper.SetValue(Node, "Deployable/Folder/1525181F-901A-416C-8A58-119130FE478E/Folders/9EF0B969-E518-4E46-987F-47570745A589/Name",
                               "8:" + ProductName);

      // We also need to update the ProductCode to get around the problem of the installation
      // thinking there's a previous version already installed on users computer.
      XmlHelper.SetValue(Node, "Deployable/Product/ProductCode", "8:{" + ProductCode + "}");     
      
      } 


   }
