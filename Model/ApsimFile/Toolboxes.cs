using System.Collections.Generic;
using System.IO;
using ApsimFile;

public class Toolboxes
   {
   private static Toolboxes Singleton = null;
   private List<string> ToolBoxesFromPlugIns = new List<string>();
   private List<string> ToolBoxesFromUsers = new List<string>();
   public static Toolboxes Instance
      {
      get
         {
         if (Singleton == null)
            Singleton = new Toolboxes();
         return Singleton;
         }
      }

   public Toolboxes()
      {
      // ---------------------------------------------------
      // Constructor.
      // ---------------------------------------------------
      Clear();
      }

   public void Clear()
      {
      // ---------------------------------------------------
      // Refresh our list of toolboxes.
      // ---------------------------------------------------
      ToolBoxesFromUsers = Configuration.Instance.Settings("ToolBox");
      ToolBoxesFromPlugIns.Clear();
      }
   public List<string> AllToolBoxes
      {
      // ---------------------------------------------------
      // Return a complete list of toolboxes to caller.
      // ---------------------------------------------------
      get
         {
         List<string> Files = new List<string>();
         Files.AddRange(ToolBoxesFromPlugIns);
         Files.AddRange(ToolBoxesFromUsers);
         return Files;
         }
      }
   public List<string> UserToolBoxes
      {
      // ---------------------------------------------------
      // Return a list of user defined toolboxes. Also allow
      // setting the user defined toolboxes.
      // ---------------------------------------------------
      get
         {
         return AllToolBoxes;
         }
      set
         {
         ToolBoxesFromUsers = value;
         Configuration.Instance.SetSettings("ToolBox", value);
         }
      }
   public void CreateUserToolBox(string Filename)
      {
      // --------------------------------------------------- 
      // create a new empty toolbox at the specified file 
      // --------------------------------------------------- 
      StreamWriter sr = File.CreateText(Filename);
      sr.WriteLine("<folder name=\"" + Path.GetFileNameWithoutExtension(Filename) + "\">");
      sr.WriteLine("</folder>");
      sr.Close();
      }
   public void AddPlugInToolBox(string FileName)
      {
      // --------------------------------------------------- 
      // A plugin wants to add a toolbox.
      // --------------------------------------------------- 
      ToolBoxesFromPlugIns.Add(Configuration.RemoveMacros(FileName));
      }

   }
