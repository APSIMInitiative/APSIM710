namespace ApsimFile
    {
    using System;
    using System.Collections;
    using System.Collections.Generic;
    using System.Collections.Specialized;
    using System.Text;
    using System.Xml;
    using System.IO;
    using CSGeneral;
    using System.Diagnostics;


   public class ApsimFile
      {
      // ---------------------------------------------
      // This class encapsulates a .apsim file.
      // ---------------------------------------------

      // private stuff
      private Component MyRootNode = null;
      private Component MyFactorNode = null;
      private bool Dirty = false;
      private bool ReadOnly = false;
      private string MyFileName = "";
      private int DisabledEventCount = 0;

      // events
      public delegate void FileDelegate<T>(T arg);
      public delegate void EmptyDelegate();
      public event FileDelegate<Component> ComponentChangedEvent;
      public event FileDelegate<Component> ContentChangedEvent;
      public event FileDelegate<bool> DirtyChanged;
      public event FileDelegate<string> FileNameChanged;
      public event EmptyDelegate BeforeSave;

      public void PublishComponentChanged(Component Comp)
         {
         // If you are not already executing a ComponentChangedEvent 
         if (DisabledEventCount == 0 && ComponentChangedEvent != null)
            ComponentChangedEvent.Invoke(Comp);         //raise a ComponentChangedEvent
         SetDirty(true);
         }
      internal void PublishContentChanged(Component Comp)
         {
         if (Comp != null && DisabledEventCount == 0 && ContentChangedEvent != null)
            ContentChangedEvent.Invoke(Comp);
         SetDirty(true);
         }
      internal void BeginUpdate()
         {
         DisabledEventCount++;
         }
      internal void EndUpdate()
         {
         DisabledEventCount--;
         }

      private void SetDirty(bool Dirty)
         {
         if (this.Dirty != Dirty)
            {
            this.Dirty = Dirty;
            if (DisabledEventCount == 0 && DirtyChanged != null)
               DirtyChanged.Invoke(Dirty);
            }
         }
      private void SetFileName(string FileName)
         {
         MyFileName = FileName;
         if (Path.GetDirectoryName(FileName) != "" && Path.GetExtension(FileName) != ".xml")
            Directory.SetCurrentDirectory(Path.GetDirectoryName(FileName));
         if (DisabledEventCount == 0 && FileNameChanged != null)
            FileNameChanged.Invoke(FileName);
         }

      // properties
      public bool IsDirty
      { get { return Dirty; } }
      public bool IsReadOnly { get { return ReadOnly; } }
      public Component RootComponent
         {
         // ------------------------------------------------------
         // Return the root node to the caller.
         // ------------------------------------------------------

         get { return MyRootNode; }
         }
      public Component FactorComponent
      {
          // ------------------------------------------------------
          // Return the root node to the caller.
          // ------------------------------------------------------

          get { return MyFactorNode; }
      }
       public void CreateFactorComponent()
       {
           MyFactorNode = new Component(this, RootComponent);
           DisabledEventCount++;
           RootComponent.ChildNodes.Add(MyFactorNode);
           MyFactorNode.Name = "Factorials";  //setting Name cause a ComponentChanged event
           MyFactorNode.Type = "factorial";   //setting Type cause a ComponentChanged event
           DisabledEventCount--;
           PublishComponentChanged(MyFactorNode);
       }
      public string FileName
      { get { return MyFileName; } }

      public ApsimFile()
         {
         // ------------------------------------------------------
         // Constructor
         // ------------------------------------------------------
         }

      public ApsimFile(string FileName)
         {
         // ------------------------------------------------------
         // Constructor
         // ------------------------------------------------------
         OpenFile(FileName);
         }

      public Component Find(string FullPath)
         {
         // ------------------------------------------------------
         // Locates a component with the specified full path
         // e.g. /RootComponent/Child/SubChild
         // ------------------------------------------------------

         if (FullPath == "")
            throw new Exception("Cannot call Find with a blank path");
         if (FullPath[0] != Component.Delimiter)
            throw new Exception("Path must be fully qualified: " + FullPath);

         int Pos = FullPath.IndexOf(Component.Delimiter, 1);
         string RootName, NamePath;
         if (Pos == -1)
            {
            RootName = FullPath.Substring(1);
            NamePath = "";
            }
         else
            {
            RootName = FullPath.Substring(1, Pos - 1);
            NamePath = FullPath.Substring(Pos + 1);
            }
         if (RootName.ToLower() != RootComponent.Name.ToLower())
            return null;
         if (NamePath == "")
            return RootComponent;
         else
            return RootComponent.Find(NamePath);
         }
      public void New()
         {
         New("<folder/>");
         }
      public void New(string Xml)
         {
         // Create a new .apsim file in memory.
         XmlDocument Doc = new XmlDocument();
         Doc.LoadXml(Xml);
         Open(Doc.DocumentElement);
         }
      public void NewFromFile(string FileName)
         {
         // Create a new .apsim file in memory.
         XmlDocument Doc = new XmlDocument();
         Doc.Load(FileName);
         Open(Doc.DocumentElement);
         }
      public bool Open(XmlNode Node)
         {
         bool UpgradeOccurred = APSIMChangeTool.Upgrade(Node);
         this.ReadOnly = false;

         DisabledEventCount++;
         MyRootNode = new Component(this, null);
         MyRootNode.Read(Node);
         MyRootNode.ResolveShortcuts();

         //FactorialNode should be in the first level
         MyFactorNode = null;
         foreach (Component comp in MyRootNode.ChildNodes)
         {
             if (comp.Type.ToLower() == "factorial")
                 MyFactorNode = comp;
         }

         DisabledEventCount--;
         PublishComponentChanged(MyRootNode);
         SetFileName("Untitled");
         return UpgradeOccurred;
         }
      public void OpenFile(string FileName)
         {
         if (Path.GetDirectoryName(FileName) == "")
            FileName = Path.Combine(Directory.GetCurrentDirectory(), FileName);

         DisabledEventCount++;
         XmlDocument doc = new XmlDocument();
         doc.Load(FileName);
         if (Open(doc.DocumentElement))
            doc.Save(FileName);
         List<string> ReadOnlyFileNamesList = Configuration.Instance.Settings("ReadOnlyFiles");
         string[] ReadOnlyFileNames = new string[ReadOnlyFileNamesList.Count];
         ReadOnlyFileNamesList.CopyTo(ReadOnlyFileNames);
         ReadOnly = (CSGeneral.StringManip.IndexOfCaseInsensitive(ReadOnlyFileNames, Path.GetFileName(FileName)) != -1);
         DisabledEventCount--;
         PublishComponentChanged(MyRootNode);
         SetDirty(false);
         if (!ReadOnly)
            SetFileName(FileName);
         }

      public bool Save()
         {
         // ---------------------------------------------------------
         // Save the contents of this apsim document to the specified 
         // file - formatted nicely.
         // ---------------------------------------------------------
         if (!ReadOnly && RootComponent != null && FileName != "Untitled")
            {
            if (BeforeSave != null)
               BeforeSave.Invoke();
            XmlDocument doc = new XmlDocument();
            XmlNode RootNode = doc.AppendChild(doc.CreateElement(RootComponent.Type));
            XmlHelper.SetAttribute(RootNode, "version", APSIMChangeTool.CurrentVersion.ToString());
            RootComponent.Write(RootNode);
            doc.Save(FileName);
            Dirty = true;
            SetDirty(false);
            return true;
            }
         return false;
         }
      public bool SaveAs(string FileName)
         {
         ReadOnly = false;
         SetFileName(FileName);
         return Save();
         }

      public void ReloadFromFile()
         {
         OpenFile(FileName);
         }

      public static List<string> GetSimsInApsimFile(string FileName)
         {
         List<string> SimsToRun = new List<string>();
         ApsimFile Apsim = new ApsimFile(FileName);
         ExpandSimsToRun(Apsim.RootComponent, ref SimsToRun);
         return SimsToRun;
         }

      public static void ExpandSimsToRun(Component Comp, ref List<string> SimsToRun)
         {
         // ------------------------------------------------
         // Go looking for simulations to run. Look at the
         // currently selected nodes first and progressively
         // their parents until some simulations are found.
         // ------------------------------------------------

         while (Comp.Type != "simulation" && Comp.Type != "folder" && Comp.Type != "simulations")
            Comp = Comp.Parent;

         if (Comp.Type == "simulation" & Comp.Enabled)
            SimsToRun.Add(Comp.FullPath);

         else if (Comp.Type == "folder")
            {
            foreach (Component Child in Comp.ChildNodes)
               {
               if (Child.Type == "simulation" || Child.Type == "folder")
                  ExpandSimsToRun(Child, ref SimsToRun); // recursion
               }

            if (SimsToRun.Count == 0)
               {
               // Current selection must be in a folder inside a simulation step up to parent
               // looking for the parent simulation
               while ((Comp != null) && Comp.Type != "simulation")
                  Comp = Comp.Parent;

               if ((Comp != null) && Comp.Type == "simulation" && Comp.Enabled)
                  SimsToRun.Add(Comp.FullPath);
               }
            }
         }

      public static List<string> GetSimNamesInApsimFile(string FileName)
         {
         XmlDocument Doc = new XmlDocument();
         Doc.Load(FileName);
         List<string> SimNames = new List<string>();
         List<XmlNode> SimNodes = new List<XmlNode>();
         XmlHelper.FindAllRecursivelyByType(Doc.DocumentElement, "simulation", ref SimNodes);
         foreach (XmlNode SimNode in SimNodes)
            {
            if (XmlHelper.Attribute(SimNode, "enabled") != "no")
               SimNames.Add(XmlHelper.Name(SimNode));
            }
         return SimNames;
         }



      }
    }

