using System;
using System.IO;
using System.Collections;
using System.Collections.Specialized;
using System.Runtime.InteropServices;
using System.Reflection;
using System.Reflection.Emit;
using CSGeneral;
using System.Xml;
using ApsimFile;
using System.Collections.Generic;

namespace ApsimToSim
   {
   class ApsimToSim
      {
      [STAThread] static int Main(string[] args)
         {
         // Main entry point into application.
         // Firstly parse all arguments.
         string ApsimFileName = null;
         string[] SimPaths = new string[args.Length - 1];
         for (int i = 0; i != args.Length; i++)
            {
            if (i == 0)
               ApsimFileName = args[i];
            else
               SimPaths[i - 1] = args[i];
            }

         Assembly.Load("Actions");

         if (ApsimFileName == null)
            Console.WriteLine("No .apsim file specified on the command line");

         try
            {
            ApsimToSim SimCreator = new ApsimToSim();
            SimCreator.ConvertApsimToSim(ApsimFileName, SimPaths);
            }
         catch (Exception err)
            {
            Console.WriteLine(err.Message);
            return 1;
            }
         return 0;
         }

      private void ConvertApsimToSim(string ApsimFileName, string[] SimNames)
         {
         Directory.SetCurrentDirectory(Path.GetDirectoryName(ApsimFileName));

         PlugIns.LoadAll();

         // convert the specified simulations in the specified apsim file name
         // into a separate .sim file for each.
         ApsimFile.ApsimFile Apsim = new ApsimFile.ApsimFile();
         Apsim.OpenFile(ApsimFileName);

         // In case the file is now dirty due to .apsim file converter then save it
         if (Apsim.IsDirty)
            Apsim.Save();

         FindSimsAndConvert(Apsim.RootComponent, SimNames);
         }
      private void FindSimsAndConvert(ApsimFile.Component Apsim, string[] SimPaths)
         {
         // Iterate through all nested simulations and convert them to
         // .sim format if necessary.
         foreach (ApsimFile.Component Child in Apsim.ChildNodes)
            {
            if (Child.Type.ToLower() == "simulation")
               {
               string SimName = Child.Name;
               string SimPath = Child.FullPath;
               bool convertSim = (SimPaths.Length == 0 || Array.IndexOf(SimPaths, SimPath) != -1);
               if (convertSim)
                  {
                  try
                     {
                     XmlDocument SimXML = new XmlDocument();
                     SimXML.LoadXml(WriteSimScript(Child));
                     SortSimContents(SimXML.DocumentElement);
                     SimXML.Save(SimName + ".sim");
                     }
                  catch (Exception err)
                     {
                     throw new Exception(SimName + ": " + err.Message);
                     }
                  }
               }
            if (Child.Type.ToLower() == "folder")
               FindSimsAndConvert(Child, SimPaths);
            }
         }
      private string WriteSimScript(Component Child)
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
               ApsimToSimContents = ReplaceSoilMacros(ApsimToSimContents, Child);
               ApsimToSimContents = ReplaceModelMacro(ApsimToSimContents, Child);
               ApsimToSimContents = ReplaceDllMacro(ApsimToSimContents, Child);
               ApsimToSimContents = ReplaceChildrenMacro(ApsimToSimContents, Child);

               // Any other macros in the <ApsimToSim> will be removed by using the 
               // APSIM macro language.
               XmlDocument ChildValues = new XmlDocument();
               ChildValues.LoadXml(Child.Contents);
               Macro Macro = new Macro();
               return Macro.Go(ChildValues.DocumentElement, XmlHelper.FormattedXML(ApsimToSimContents));
               }
            }
         return "";
         }
      private string ReplaceDllMacro(string ApsimToSimContents, Component ApsimComponent)
         {
         // Replace all occurrences of [Dll] with the name of the model dll.
         string Dll = Types.Instance.MetaData(ApsimComponent.Type, "dll");
         Dll = Configuration.AddMacros(Dll);
         return ApsimToSimContents.Replace("[dll]", Dll);
         }
      private string ReplaceModelMacro(string ApsimToSimContents, Component ApsimComponent)
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
      private string FindModelContents(XmlNode Node, string TypeName)
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
      private string ReplaceSoilMacros(string ApsimToSimContents, Component ApsimComponent)
         {
         // Replace the [Soil.] macros will values.         
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

            // Create an instance of the soil class so that we can ask it for the values we need.
            if (SoilComponent != null)
               {
               Soil SoilInPaddock = new Soil(SoilComponent.ContentsAsXML);

               // Soil / Crop variables
               string CropName = ApsimComponent.Name;
               ApsimToSimContents = ApsimToSimContents.Replace("[soil.ll]", WriteLayeredSoilValues(SoilInPaddock.LL(CropName)));
               ApsimToSimContents = ApsimToSimContents.Replace("[soil.kl]", WriteLayeredSoilValues(SoilInPaddock.KL(CropName)));
               ApsimToSimContents = ApsimToSimContents.Replace("[soil.xf]", WriteLayeredSoilValues(SoilInPaddock.XF(CropName)));

               // Soil water variables.
               ApsimToSimContents = ApsimToSimContents.Replace("[soil.Thickness]", WriteLayeredSoilValues(SoilInPaddock.Thickness));
               ApsimToSimContents = ApsimToSimContents.Replace("[soil.Sat]", WriteLayeredSoilValues(SoilInPaddock.SAT));
               ApsimToSimContents = ApsimToSimContents.Replace("[soil.Dul]", WriteLayeredSoilValues(SoilInPaddock.DUL));
               ApsimToSimContents = ApsimToSimContents.Replace("[soil.LL15]", WriteLayeredSoilValues(SoilInPaddock.LL15));
               ApsimToSimContents = ApsimToSimContents.Replace("[soil.AirDry]", WriteLayeredSoilValues(SoilInPaddock.Airdry));
               ApsimToSimContents = ApsimToSimContents.Replace("[soil.SWCon]", WriteLayeredSoilValues(SoilInPaddock.SWCON));
               ApsimToSimContents = ApsimToSimContents.Replace("[soil.BD]", WriteLayeredSoilValues(SoilInPaddock.BD));
               ApsimToSimContents = ApsimToSimContents.Replace("[soil.MWCon]", WriteLayeredSoilValues(SoilInPaddock.MWCON));
               ApsimToSimContents = ApsimToSimContents.Replace("[soil.KS]", WriteLayeredSoilValues(SoilInPaddock.KS));

               // Initial water and nitrogen variables.
               foreach (Component SoilChild in ApsimComponent.ChildNodes)
                  {
                  if (SoilChild.Type == "InitWater")
                     {
                     InitWater Water = new InitWater(SoilChild.ContentsAsXML, SoilInPaddock);
                     ApsimToSimContents = ApsimToSimContents.Replace("[soil.SW]", SoilComponentUtility.LayeredToString(Water.SWMapedToSoil));
                     }
                  else if (SoilChild.Type.ToLower() == "soilsample")
                     {
                     SoilSample Sample = new SoilSample(SoilChild.ContentsAsXML, SoilInPaddock);
                     if (MathUtility.ValuesInArray(Sample.SW))
                        ApsimToSimContents = ApsimToSimContents.Replace("[soil.SW]", SoilComponentUtility.LayeredToString(Sample.SWMapedToSoil));
                     if (MathUtility.ValuesInArray(Sample.NO3))
                        {
                        ApsimToSimContents = ApsimToSimContents.Replace("[soil.NO3]", SoilComponentUtility.LayeredToString(Sample.NO3MapedToSoil));
                        ApsimToSimContents = ApsimToSimContents.Replace("[soil.NH4]", SoilComponentUtility.LayeredToString(Sample.NH4MapedToSoil));
                        }
                     }
                  else if (SoilChild.Type == "InitNitrogen")
                     {
                     InitNitrogen Nitrogen = new InitNitrogen(SoilChild.ContentsAsXML, SoilInPaddock);
                     ApsimToSimContents = ApsimToSimContents.Replace("[soil.NO3]", SoilComponentUtility.LayeredToString(Nitrogen.NO3MapedToSoil));
                     ApsimToSimContents = ApsimToSimContents.Replace("[soil.NH4]", SoilComponentUtility.LayeredToString(Nitrogen.NH4MapedToSoil));
                     }
                  }

               // Soil nitrogen variables.
               ApsimToSimContents = ApsimToSimContents.Replace("[soil.OC]", WriteLayeredSoilValues(SoilInPaddock.OC));
               ApsimToSimContents = ApsimToSimContents.Replace("[soil.PH]", WriteLayeredSoilValues(SoilInPaddock.PH));
               ApsimToSimContents = ApsimToSimContents.Replace("[soil.FBIOM]", WriteLayeredSoilValues(SoilInPaddock.FBIOM));
               ApsimToSimContents = ApsimToSimContents.Replace("[soil.FINERT]", WriteLayeredSoilValues(SoilInPaddock.FINERT));
               ApsimToSimContents = ApsimToSimContents.Replace("[soil.Rocks]", WriteLayeredSoilValues(SoilInPaddock.Rocks));

               // Soil phosphorus variables.
               ApsimToSimContents = ApsimToSimContents.Replace("[soil.LabileP]", WriteLayeredSoilValues(SoilInPaddock.LabileP));
               ApsimToSimContents = ApsimToSimContents.Replace("[soil.BandedP]", WriteLayeredSoilValues(SoilInPaddock.BandedP));
               ApsimToSimContents = ApsimToSimContents.Replace("[soil.RockP]", WriteLayeredSoilValues(SoilInPaddock.RockP));
               ApsimToSimContents = ApsimToSimContents.Replace("[soil.Sorption]", WriteLayeredSoilValues(SoilInPaddock.Sorption));

               ApsimToSimContents = ApsimToSimContents.Replace("[soil.RootCP]", SoilInPaddock.RootCP.ToString());
               ApsimToSimContents = ApsimToSimContents.Replace("[soil.RateDissolRock]", SoilInPaddock.RateDissolRock.ToString());
               ApsimToSimContents = ApsimToSimContents.Replace("[soil.RateLossAvail]", SoilInPaddock.RateLossAvail.ToString());

               }
            }
         return ApsimToSimContents;
         }
      private string WriteLayeredSoilValues(double[] Values)
         {
         if (MathUtility.ValuesInArray(Values))
            return SoilComponentUtility.LayeredToString(Values);
         else
            return "";
         }
      private string ReplaceChildrenMacro(string ApsimToSimContents, Component ApsimComponent)
         {
         // Replace the [Children] macro will child sim script.

         if (ApsimToSimContents.Contains("[Children]") || ApsimToSimContents.Contains("[HasChildren]"))
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
      private void SortSimContents(XmlNode Node)
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


      }
   }
