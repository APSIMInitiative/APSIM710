using System;
using System.Collections.Generic;
using System.Text;
using ApsimFile;
using System.IO;
using System.Xml;
using System.Collections;
using CSGeneral;

public class ApsimToSim
   {
   /// <summary>
   /// Writes a sim file for the specified component. Will throw on error.
   /// </summary>
   /// 
   public static string WriteSimFile(Component Child)
      {
      TestUniqueNamesUnderPaddock(Child);           //test to see if the .apsim file was valid before writing sim file.
      string SimFileName = Child.Name + ".sim";
      XmlDocument SimXML = new XmlDocument();
      SimXML.LoadXml(WriteSimScript(Child));
      SortSimContents(SimXML.DocumentElement);
      SimXML.Save(SimFileName);
      return Path.GetFullPath(SimFileName);
      }
   private static string WriteSimScript(Component Child)
      {
      // Write and return the .sim file contents for the specified 
      // Child component.
      if (Child.Enabled)
         {
         XmlNode ApsimToSim = Types.Instance.ApsimToSim(Child.Type);
         if (ApsimToSim != null)
            {
            string ApsimToSimContents = ApsimToSim.InnerXml;

            // Replace any occurrences of our macros with appropriate text.
            // e.g. [Soil.] is replaced by the appropriate soil value.
            //      [Model] is replaced by the contents of the [Model] node i.e. the ini contents.
            //      [Dll] is replaced by the name of the model dll.
            //      [Children] is replaced by the sim script for all children of this component.
            //      [InstanceName] is replaced by the instance name.
            ApsimToSimContents = ApsimToSimContents.Replace("[InstanceName]", Child.Name);
            ApsimToSimContents = ReplaceSoilMacros(ApsimToSimContents, Child);
            ApsimToSimContents = ReplaceModelMacro(ApsimToSimContents, Child);
            ApsimToSimContents = ReplaceDllMacro(ApsimToSimContents, Child);
            ApsimToSimContents = ReplaceDllExtMacro(ApsimToSimContents, Child);
            ApsimToSimContents = ReplaceChildrenMacro(ApsimToSimContents, Child);

            // Any other macros in the <ApsimToSim> will be removed by using the 
            // APSIM macro language.
            XmlDocument ChildValues = new XmlDocument();
            ChildValues.LoadXml(Child.Contents);

            // Add in any child components that don't have anything in their <ApsimToSim>
            foreach (Component SubChild in Child.ChildNodes)
               RecursivelyAddChildContent(SubChild, ChildValues.DocumentElement);

            Macro Macro = new Macro();
            return Macro.Go(ChildValues.DocumentElement, XmlHelper.FormattedXML(ApsimToSimContents));
            }
         }
      return "";
      }
   private static void RecursivelyAddChildContent(Component C, XmlNode ContentNode)
      {
      // Recursively add in any child components that don't have anything in their <ApsimToSim>
      XmlNode ApsimToSim = Types.Instance.ApsimToSim(C.Type);
      if (ApsimToSim == null)
         {
         XmlNode NewNode = ContentNode.AppendChild(ContentNode.OwnerDocument.ImportNode(C.ContentsAsXML, true));
         foreach (Component Child in C.ChildNodes)
            RecursivelyAddChildContent(Child, NewNode);
         }
      }
   private static string ReplaceDllMacro(string ApsimToSimContents, Component ApsimComponent)
      {
      // Replace all occurrences of [Dll] with the name of the model dll.
      string Dll = Types.Instance.MetaData(ApsimComponent.Type, "dll");
      Dll = Configuration.AddMacros(Dll);
      return ApsimToSimContents.Replace("[dll]", Dll);
      }

   private static string ReplaceDllExtMacro(string ApsimToSimContents, Component ApsimComponent)
      {
      // Replace all occurrences of %dllext%
      if (ApsimFile.Configuration.amRunningOnUnix()) 
         return(ApsimToSimContents.Replace("%dllext%", "so"));
            
      return (ApsimToSimContents.Replace("%dllext%", "dll"));
      }
   private static string ReplaceModelMacro(string ApsimToSimContents, Component ApsimComponent)
      {
      // Replace all occurrences of [Model] with the contents of the model configuration.
      while (ApsimToSimContents.Contains("[Model"))
         {
         // If the user has an ini child under
         string ModelContents = "";
         foreach (Component Child in ApsimComponent.ChildNodes)
            {
            if (Child.Type == "ini")
               {
               // Get the name of the model file.
               XmlDocument IniComponent = new XmlDocument();
               IniComponent.LoadXml(Child.Contents);
               string ModelFileName = Configuration.RemoveMacros(XmlHelper.Value(IniComponent.DocumentElement, "filename"));

               if (Path.GetExtension(ModelFileName) == ".xml")
                  {
                  // Find the <Model> node in the model file.
                  XmlDocument ModelFile = new XmlDocument();
                  ModelFile.Load(ModelFileName);
                  ModelContents += FindModelContents(ModelFile.DocumentElement, ApsimComponent.Type);
                  }
               else
                  ModelContents += "<include>" + ModelFileName + "</include>";
               }
            }

         // See if there is something after [Model e.g. [Model SoilWat]. SoilWat is the ModelType
         int PosModel = ApsimToSimContents.IndexOf("[Model");
         int PosEndModel = ApsimToSimContents.IndexOf(']', PosModel);
         if (ModelContents == "")
            {
            int PosStartModelType = PosModel + "[Model".Length;
            string ModelType = ApsimToSimContents.Substring(PosStartModelType, PosEndModel - PosStartModelType).Trim();
            if (ModelType == "")
               ModelContents = Types.Instance.ModelContents(ApsimComponent.Type);
            else
               ModelContents = Types.Instance.ModelContents(ApsimComponent.Type, ModelType);
            }
         ApsimToSimContents = ApsimToSimContents.Remove(PosModel, PosEndModel - PosModel + 1);
         ApsimToSimContents = ApsimToSimContents.Insert(PosModel, ModelContents);
         }
      return ApsimToSimContents;
      }
   private static string FindModelContents(XmlNode Node, string TypeName)
      {
      // Given the XmlNode passed in, try and find the <Model> node.
      // The node passed in could be a <type> node in which case the <Model>
      // node will be immediately under it.
      // Alternatively, the node passed in could be a <plugin> node, so the 
      // <Model> node will be under the <type name="xxx"> node.
      XmlNode ModelNode = XmlHelper.Find(Node, "Model");
      if (ModelNode == null)
         ModelNode = XmlHelper.Find(Node, TypeName + "/Model");
      if (ModelNode == null)
         return "";
      else
         return ModelNode.InnerXml;
      }
   private static string ReplaceSoilMacros(string ApsimToSimContents, Component ApsimComponent)
      {
      // Replace the [soil.] macros will values.         
      if (ApsimToSimContents.Contains("[soil."))
         {
         // Firstly we need to locate the nearest soil.
         Component SoilComponent = null;
         if (ApsimComponent.Type == "soil")
            SoilComponent = ApsimComponent;
         else
            {
            foreach (Component Sibling in ApsimComponent.Parent.ChildNodes)
               {
               if (Sibling.Type.ToLower() == "soil")
                  SoilComponent = Sibling;
               }
            }

         if (SoilComponent != null)
            {
            // Create a soil XML node.
            XmlDocument Doc = new XmlDocument();
            Doc.LoadXml(SoilComponent.FullXMLNoShortCuts());
            XmlNode SoilNode = Doc.DocumentElement;

            // Now do all soil macro replacements.
            ApsimToSimContents = Soil.ReplaceSoilMacros(SoilNode, ApsimToSimContents);
            }
         }
      return ApsimToSimContents;
      }
   private static string ReplaceChildrenMacro(string ApsimToSimContents, Component ApsimComponent)
      {
      // Replace the [Children] macro with child sim script.

      int PosStartMacro = ApsimToSimContents.IndexOf("[Children");
      while (PosStartMacro != -1)
         {
         int PosEndMacro = ApsimToSimContents.IndexOf(']', PosStartMacro);
         if (PosEndMacro != -1)
            {
            string ChildType = "";
            string[] MacroWords = ApsimToSimContents.Substring(PosStartMacro + 1, PosEndMacro - PosStartMacro - 1)
                                  .Split(" ".ToCharArray(), StringSplitOptions.RemoveEmptyEntries);
            if (MacroWords.Length == 2)
               ChildType = MacroWords[1];

            string ChildSimContents = "";
            foreach (Component Child in ApsimComponent.ChildNodes)
               {
               if (ChildType == "" || Child.Type.ToLower() == ChildType.ToLower())
                  ChildSimContents += WriteSimScript(Child);
               }
            ApsimToSimContents = ApsimToSimContents.Remove(PosStartMacro, PosEndMacro - PosStartMacro + 1);
            ApsimToSimContents = ApsimToSimContents.Insert(PosStartMacro, ChildSimContents);
            }
         PosStartMacro = ApsimToSimContents.IndexOf("[Children", PosStartMacro + 1);
         }
      if (ApsimToSimContents.Contains("[HasChildren"))
         {

         string ChildSimContents = "";
         foreach (Component Child in ApsimComponent.ChildNodes)
            ChildSimContents += WriteSimScript(Child);
         if (ChildSimContents == "")
            ApsimToSimContents = ApsimToSimContents.Replace("[HasChildren]", "");
         else
            ApsimToSimContents = ApsimToSimContents.Replace("[HasChildren]", "Yes");
         ApsimToSimContents = ApsimToSimContents.Replace("[Children]", ChildSimContents);
         }
      return ApsimToSimContents;
      }

   #region Sim Sorting code
   private static void SortSimContents(XmlNode Node)
      {
      // go through all paddocks and sort all component nodes.

      XmlHelper.Sort(Node, new ComponentSorter());
      foreach (XmlNode System in XmlHelper.ChildNodes(Node, "system"))
         SortSimContents(System); // recursion
      }
   private class ComponentSorter : IComparer
      {
      private CaseInsensitiveComparer StringComparer = new CaseInsensitiveComparer();
      private List<string> Components = new List<string>();
      public ComponentSorter()
         {
         Components = Configuration.Instance.ComponentOrder();
         }
      int IComparer.Compare(object x, object y)
         {
         XmlNode Data1 = (XmlNode)x;
         XmlNode Data2 = (XmlNode)y;
         string ModuleName1 = Path.GetFileNameWithoutExtension(XmlHelper.Attribute(Data1, "executable")).ToLower();
         string ModuleName2 = Path.GetFileNameWithoutExtension(XmlHelper.Attribute(Data2, "executable")).ToLower();

         if (x == y)
            return 0;
         if (Data1.Name == "executable")
            return -1;
         if (Data2.Name == "executable")
            return 1;
         if (ModuleName1 == ModuleName2)
            {
            int ChildIndex1 = Array.IndexOf(XmlHelper.ChildNames(Data1.ParentNode, ""), XmlHelper.Name(Data1));
            int ChildIndex2 = Array.IndexOf(XmlHelper.ChildNames(Data2.ParentNode, ""), XmlHelper.Name(Data2));
            if (ChildIndex1 < ChildIndex2)
               return -1;
            else
               return 1;
            }
         if (XmlHelper.Type(Data1) == "title")
            return -1;
         for (int i = 0; i != Components.Count; i++)
            {
            if (StringManip.StringsAreEqual(Components[i], ModuleName1))
               return -1;
            if (StringManip.StringsAreEqual(Components[i], ModuleName2))
               return 1;
            }
         // Neither are in list so keep original order intact i.e. Node1 comes before Node2!!
         // Find the relative positions of data1 and data2 in the parent list.
         int Data1Pos = 0;
         int Data2Pos = 0;
         for (int i = 0; i != Data1.ParentNode.ChildNodes.Count; i++)
            {
            if (Data1.ParentNode.ChildNodes[i] == Data1)
               Data1Pos = i;
            if (Data1.ParentNode.ChildNodes[i] == Data2)
               Data2Pos = i;
            }
         if (Data1Pos < Data2Pos)
            return -1;
         else
            return 1;
         }
      }
   #endregion


   #region Testing .apsim file is valid code


   private static void TestUniqueNamesUnderPaddock(Component Simulation)
   {
       // -------------------------------------------------------------
       // There should not be any two components with the same name under the paddock level.
       // This will test to see if there is any with the same name and if so, return an error.
       // -------------------------------------------------------------
       string SameName;

       List<Component> Paddocks;
       Paddocks = ApsimFile.ComponentUtility.FindPaddsInSim(Simulation);

       foreach (Component Paddock in Paddocks)
       {
           SameName = ApsimFile.ComponentUtility.FindSameNameRecursively(Paddock);
           if (SameName != "")
           {
               throw new Exception("You can not have two components with the same name in a paddock." + "\r\n" + "Simulation: " + Simulation.Name + "\r\n" + "Component Name: " + SameName);
           }
       }
   }

   #endregion

   }
