using System;
using CSGeneral;
using System.Xml;
using System.Text.RegularExpressions;
using System.Collections.Generic;
using System.Windows.Forms;

namespace ApsimFile
   {
   // ------------------------------------------
   // This class converts an APSIM file from one 
   // version to the 'current' version
   // ------------------------------------------
   public class APSIMChangeTool
      {
      public static int CurrentVersion = 28;
      private delegate void UpgraderDelegate(XmlNode Data);

      public static void Upgrade(XmlNode Data)
         {
         // ------------------------------------------
         // Upgrade the specified data
         // to the 'current' version. Returns true
         // if something was upgraded.
         // ------------------------------------------
         UpgradeToVersion(Data, CurrentVersion);
         }

      public static void UpgradeToVersion(XmlNode Data, int ToVersion)
         {
         // ------------------------------------------
         // Upgrade the specified data
         // to the specified version. Returns true
         // if something was upgraded.
         // ------------------------------------------

         UpgraderDelegate[] Upgraders = { null,
                                          new UpgraderDelegate(ToVersion2), 
                                          new UpgraderDelegate(ToVersion3),
                                          new UpgraderDelegate(ToVersion4),
                                          new UpgraderDelegate(ToVersion5),
                                          new UpgraderDelegate(ToVersion6),
                                          new UpgraderDelegate(ToVersion7),
                                          new UpgraderDelegate(ToVersion8),
                                          new UpgraderDelegate(ToVersion9),
                                          new UpgraderDelegate(ToVersion10),
                                          new UpgraderDelegate(ToVersion11),
                                          new UpgraderDelegate(ToVersion12),
                                          new UpgraderDelegate(ToVersion13),
                                          new UpgraderDelegate(ToVersion14),
                                          new UpgraderDelegate(ToVersion15),
                                          new UpgraderDelegate(ToVersion16),
                                          new UpgraderDelegate(ToVersion17),
                                          new UpgraderDelegate(ToVersion18),
                                          new UpgraderDelegate(ToVersion19),
                                          new UpgraderDelegate(ToVersion20),
                                          new UpgraderDelegate(ToVersion21),
                                          new UpgraderDelegate(ToVersion22),
                                          new UpgraderDelegate(ToVersion23),
                                          new UpgraderDelegate(ToVersion24),
                                          new UpgraderDelegate(ToVersion25),
                                          new UpgraderDelegate(ToVersion26),
                                          new UpgraderDelegate(ToVersion27),
                                          new UpgraderDelegate(ToVersion28)
                                       };
         if (Data != null)
            {
            // Get version number of data.
            int DataVersion = 1;
            if (XmlHelper.Attribute(Data, "version") != "")
               DataVersion = Convert.ToInt32(XmlHelper.Attribute(Data, "version"));

            while (DataVersion < ToVersion)
               {
               Upgrade(Data, Upgraders[DataVersion]);
               DataVersion++;
               }

            // All finished upgrading - write version number out.
            XmlHelper.SetAttribute(Data, "version", ToVersion.ToString());
            }
         }

      private static void Upgrade(XmlNode Data, UpgraderDelegate Upgrader)
         {
         // ------------------------------------------------
         // Upgrade the data using the specified 'upgrader'
         // ------------------------------------------------

         foreach (XmlNode Child in XmlHelper.ChildNodes(Data, ""))
            {
            Upgrader(Child);
            if (Child.Name.ToLower() == "area"
               || Child.Name.ToLower() == "folder"
               || Child.Name.ToLower() == "simulation"
               || Child.Name.ToLower() == "manager"
               || Child.Name.ToLower() == "outputfile"
               || Child.Name.ToLower() == "graph"
               || Child.Name.ToLower() == "data"
               || Child.Name.ToLower() == "tclmanager"
               || Child.Name.ToLower() == "farmmanager"
               || Child.Name.ToLower() == "paddockmanager")
               Upgrade(Child, Upgrader);  // recurse
            }
         }

      #region Version2
      private static void ToVersion2(XmlNode Data)
         {
         // ---------------------------------------------------------------------------
         // Upgrade the data to file version 2. This file version was used in APSIM 4.2
         // ---------------------------------------------------------------------------
         if (XmlHelper.Type(Data).ToLower() == "soil")
            ToVersion2Soil(Data);
         else if (XmlHelper.Type(Data).ToLower() == "registrations")
            XmlHelper.SetName(Data, "global");
         else if (XmlHelper.Type(Data).ToLower() == "outputfile")
            {
            ToVersion2Variables(Data, "OutputFileDescription/Variables", "variable");
            ToVersion2Variables(Data, "OutputFileDescription/Events", "event");
            }
         }
      private static void ToVersion2Soil(XmlNode Data)
         {
         XmlNode Water = XmlHelper.Find(Data, "Water");
         XmlNode Nitrogen = XmlHelper.Find(Data, "Nitrogen");
         XmlNode InitWater = XmlHelper.Find(Data, "InitWater");
         XmlNode InitNitrogen = XmlHelper.Find(Data, "InitNitrogen");

         if (InitWater == null)
            {
            InitWater = Data.AppendChild(XmlHelper.CreateNode(Data.OwnerDocument, "InitWater", ""));
            MoveVersion2SoilInfo(Water, "sw", InitWater);
            }
         else
            DeleteVersion2SoilInfo(Water, "Sw");

         if (InitNitrogen == null)
            {
            InitNitrogen = Data.AppendChild(XmlHelper.CreateNode(Data.OwnerDocument, "InitNitrogen", ""));
            MoveVersion2SoilInfo(Nitrogen, "no3", InitNitrogen);
            MoveVersion2SoilInfo(Nitrogen, "nh4", InitNitrogen);
            }
         else
            {
            DeleteVersion2SoilInfo(Nitrogen, "no3");
            DeleteVersion2SoilInfo(Nitrogen, "nh4");
            }
         }
      private static void DeleteVersion2SoilInfo(XmlNode FromNode, string InfoName)
         {
         foreach (XmlNode Layer in XmlHelper.ChildNodes(FromNode, "Layer"))
            {
            XmlNode Value = XmlHelper.Find(Layer, InfoName);
            if (Value != null)
               Layer.RemoveChild(Value);
            }
         }
      private static void MoveVersion2SoilInfo(XmlNode FromNode, string InfoName, XmlNode ToNode)
         {
         // Make sure we have enough layers in the ToNode.
         int NumLayers = XmlHelper.ChildNodes(FromNode, "Layer").Count;
         for (int LayerNumber = 1; LayerNumber <= NumLayers; LayerNumber++)
            if (XmlHelper.Find(ToNode, LayerNumber.ToString()) == null)
               ToNode.AppendChild(XmlHelper.CreateNode(ToNode.OwnerDocument, "layer", LayerNumber.ToString()));

         foreach (XmlNode Layer in XmlHelper.ChildNodes(FromNode, "Layer"))
            {
            XmlNode Value = XmlHelper.Find(Layer, InfoName);
            if (Value != null)
               {
               Layer.RemoveChild(Value);
               XmlNode ToLayer = XmlHelper.Find(ToNode, XmlHelper.Name(Layer));
               ToLayer.AppendChild(Value);
               }
            }
         }
      private static void ToVersion2Variables(XmlNode Data, string NodeToOperateOn, string ChildType)
         {
         // ------------------------------------------
         // Remove all 'data outside paddock' from all 
         // children of specified data.
         // ------------------------------------------
         XmlNode Variables = XmlHelper.FindByType(Data, NodeToOperateOn);

         if (Variables != null)
            {
            foreach (XmlNode Child in XmlHelper.ChildNodes(Variables, ChildType))
               {
               if (XmlHelper.Attribute(Child, "module").ToLower() == "data outside paddock")
                  XmlHelper.SetAttribute(Child, "module", "global");
               if (XmlHelper.Name(Child).ToLower().IndexOf("data outside paddock.") == 0)
                  XmlHelper.SetName(Child, Child.Name.Remove(0, 21));
               }
            }
         }
      #endregion

      private static void ToVersion3(XmlNode Data)
         {
         // ---------------------------------------------------------------------------
         // Upgrade the data to file version 3. This file version was used in APSIM 5.0
         // NB: APSIM 5.0 and 5.1 didn't actually do any conversions due to the converter
         // never being called. There was also another bug where it was looking for
         // 'sample' nodes rather than 'soilsample'. Only YieldProphet ever used
         // soil samples at this time.
         // ---------------------------------------------------------------------------

         if (XmlHelper.Type(Data).ToLower() == "soil")
            {
            XmlNode SoilSample = XmlHelper.FindByType(Data, "soilsample");
            if (SoilSample != null)
               {
               string SWUnit = XmlHelper.Value(SoilSample, "swunit");
               if (SWUnit != "")
                  {
                  string WaterFormat;
                  if (SWUnit.ToLower() == "volumetric")
                     WaterFormat = "VolumetricPercent";
                  else
                     WaterFormat = "GravimetricPercent";
                  XmlNode NewNode = SoilSample.OwnerDocument.CreateElement("WaterFormat");
                  XmlHelper.SetValue(NewNode, "", WaterFormat);
                  SoilSample.ReplaceChild(NewNode, XmlHelper.Find(SoilSample, "swunit"));
                  }

               XmlNode Nitrogen = XmlHelper.Find(SoilSample, "Nitrogen");
               XmlNode Other = XmlHelper.Find(SoilSample, "Other");
               if (Other == null)
                  Other = SoilSample.AppendChild(SoilSample.OwnerDocument.CreateElement("Other"));
               MoveVersion2SoilInfo(Nitrogen, "oc", Other);
               MoveVersion2SoilInfo(Nitrogen, "ph", Other);
               }
            }
         }

      private static void ToVersion4(XmlNode Data)
         {
         if (XmlHelper.Type(Data).ToLower() == "rule")
            {
            foreach (XmlNode category in XmlHelper.ChildNodes(Data, "category"))
               {
               foreach (XmlNode property in XmlHelper.ChildNodes(category, "property"))
                  {
                  XmlNode NewProperty = XmlHelper.CreateNode(category.OwnerDocument, XmlHelper.Name(property), "");
                  XmlHelper.SetAttribute(NewProperty, "type", XmlHelper.Attribute(property, "type"));

                  if (XmlHelper.Attribute(property, "croppropertyname") != "")
                     XmlHelper.SetAttribute(NewProperty, "croppropertyname", XmlHelper.Attribute(property, "croppropertyname"));

                  if (XmlHelper.Attribute(property, "listvalues") != "")
                     XmlHelper.SetAttribute(NewProperty, "listvalues", XmlHelper.Attribute(property, "listvalues"));

                  XmlHelper.SetAttribute(NewProperty, "description", XmlHelper.Attribute(property, "description"));
                  NewProperty.InnerText = XmlHelper.Attribute(property, "value");
                  category.ReplaceChild(NewProperty, property);
                  }
               }
            }
         }

      private static void ToVersion5(XmlNode Data)
         {
         if (XmlHelper.Attribute(Data, "shortcut") != "" && XmlHelper.Attribute(Data, "name") == "")
            XmlHelper.SetName(Data, XmlHelper.Attribute(Data, "shortcut"));

         // get rid of <filename>
         if (XmlHelper.Type(Data).ToLower() == "outputfile")
            {
            XmlNode FileNameNode = XmlHelper.Find(Data, "filename");
            if (FileNameNode != null)
               Data.RemoveChild(FileNameNode);
            }

         if (XmlHelper.Type(Data).ToLower() == "outputfiledescription")
            {
            XmlNode outputfiledescription = Data;
            if (XmlHelper.Attribute(outputfiledescription, "shortcut") == "")
               {
               string[] VGNames = XmlHelper.ChildNames(outputfiledescription, "variables");
               foreach (string VGName in VGNames)
                  {
                  XmlNode VariablesGroup = XmlHelper.Find(outputfiledescription, VGName);

                  string[] VNames = XmlHelper.ChildNames(VariablesGroup, "variable");
                  foreach (string VName in VNames)
                     {
                     XmlNode Variable = XmlHelper.Find(VariablesGroup, VName);
                     if (XmlHelper.Attribute(Variable, "name") != XmlHelper.Attribute(Variable, "variablename"))
                        XmlHelper.SetName(Variable, XmlHelper.Attribute(Variable, "variablename") + " as " + XmlHelper.Attribute(Variable, "name"));
                     if (XmlHelper.Attribute(Variable, "arrayspec").Trim() != "")
                        XmlHelper.SetName(Variable, XmlHelper.Name(Variable) + XmlHelper.Attribute(Variable, "arrayspec"));
                     string ComponentName = XmlHelper.Attribute(Variable, "module");
                     if (ComponentName.ToLower() == "global")
                        ComponentName = "";
                     if (ComponentName != "" && XmlHelper.Attribute(Variable, "ModuleType") != "soil")
                        XmlHelper.SetName(Variable, ComponentName + "." + XmlHelper.Name(Variable));
                     XmlHelper.SetAttribute(Variable, "array", "?");
                     XmlHelper.DeleteAttribute(Variable, "ModuleType");
                     XmlHelper.DeleteAttribute(Variable, "arrayspec");
                     XmlHelper.DeleteAttribute(Variable, "module");
                     XmlHelper.DeleteAttribute(Variable, "variablename");
                     }
                  XmlHelper.SetName(VariablesGroup, XmlHelper.Name(outputfiledescription));
                  VariablesGroup.ParentNode.ParentNode.AppendChild(VariablesGroup);
                  }

               string[] EGNames = XmlHelper.ChildNames(outputfiledescription, "events");
               foreach (string EGName in EGNames)
                  {
                  XmlNode EventsGroup = XmlHelper.Find(outputfiledescription, EGName);
                  string[] EventNames = XmlHelper.ChildNames(EventsGroup, "event");
                  foreach (string EventName in EventNames)
                     {
                     XmlNode Event = XmlHelper.Find(EventsGroup, EventName);
                     string ComponentName;
                     string NewEventName;

                     if (XmlHelper.Name(Event).IndexOf('.') != -1)
                        {
                        ComponentName = XmlHelper.Name(Event).Substring(0, XmlHelper.Name(Event).IndexOf('.'));
                        NewEventName = XmlHelper.Name(Event).Substring(XmlHelper.Name(Event).IndexOf('.') + 1);
                        }
                     else
                        {
                        NewEventName = XmlHelper.Name(Event);
                        ComponentName = XmlHelper.Attribute(Event, "module");
                        }

                     if (ComponentName.ToLower() == "global")
                        ComponentName = "";

                     if (ComponentName != "")
                        XmlHelper.SetName(Event, ComponentName + "." + NewEventName);
                     else
                        XmlHelper.SetName(Event, NewEventName);

                     XmlHelper.DeleteAttribute(Event, "ModuleType");
                     XmlHelper.DeleteAttribute(Event, "module");
                     XmlHelper.DeleteAttribute(Event, "eventname");
                     }
                  XmlHelper.SetName(EventsGroup, XmlHelper.Name(outputfiledescription) + " Events");
                  EventsGroup.ParentNode.ParentNode.AppendChild(EventsGroup);
                  }
               }
            else
               {
               XmlNode VariablesGroup = Data.ParentNode.AppendChild(XmlHelper.CreateNode(Data.OwnerDocument, "variables", XmlHelper.Attribute(outputfiledescription, "shortcut")));
               XmlHelper.SetAttribute(VariablesGroup, "shortcut", XmlHelper.Attribute(outputfiledescription, "shortcut"));

               XmlNode EventsGroup = Data.ParentNode.AppendChild(XmlHelper.CreateNode(Data.OwnerDocument, "events", XmlHelper.Attribute(outputfiledescription, "shortcut") + " Events"));
               XmlHelper.SetAttribute(EventsGroup, "shortcut", XmlHelper.Attribute(outputfiledescription, "shortcut") + " Events");
               }
            outputfiledescription.ParentNode.RemoveChild(outputfiledescription);
            }
         }

      private static void ToVersion6(XmlNode Data)
         {
         if (XmlHelper.Type(Data).ToLower() == "logic")
            {
            foreach (XmlNode script in XmlHelper.ChildNodes(Data, "script"))
               {
               string text = script.InnerText;
               script.InnerText = "";
               string eventName = XmlHelper.Name(script);
               eventName = eventName.Replace("startofday", "start_of_day");
               eventName = eventName.Replace("endofday", "end_of_day");
               XmlHelper.SetValue(script, "event", eventName);
               XmlHelper.SetValue(script, "text", text);
               XmlHelper.DeleteAttribute(script, "name");
               }
            }
         }

      #region Version7
      private static void ToVersion7(XmlNode Data)
         {
         // ---------------------------------------------------------
         // Version 7 soils now have a 'profile' element. Go create
         // one and populate it with all children of 
         // <Water>, <Nitrogen> and <Other> elements.
         // SoilCrop nodes are turned into:
         //     <ll name="Barley">0.460</ll>
         //     <kl name="Barley">0</kl>
         //     <xf name="Barley">0</xf>
         // for each layer under profile.
         // ---------------------------------------------------------

         if (XmlHelper.Type(Data).ToLower() == "soil" || XmlHelper.Type(Data).ToLower() == "soilsample")
            {
            foreach (XmlNode Child in XmlHelper.ChildNodes(Data, ""))
               {
               if (XmlHelper.Type(Child).ToLower() == "water" ||
                   XmlHelper.Type(Child).ToLower() == "nitrogen" ||
                   XmlHelper.Type(Child).ToLower() == "other" ||
                   XmlHelper.Type(Child).ToLower() == "soilcrop" ||
                   XmlHelper.Type(Child).ToLower() == "initwater" ||
                   XmlHelper.Type(Child).ToLower() == "initnitrogen")
                  ToSoilProfile(Child);
               else if (XmlHelper.Type(Child).ToLower() == "soilsample")
                  ToVersion7(Child);
               }
            foreach (XmlNode Child in XmlHelper.ChildNodes(Data, ""))
               {
               if (XmlHelper.Type(Child).ToLower() == "water" ||
                   XmlHelper.Type(Child).ToLower() == "nitrogen" ||
                   XmlHelper.Type(Child).ToLower() == "other" ||
                   XmlHelper.Type(Child).ToLower() == "soilcrop" ||
                   XmlHelper.Type(Child).ToLower() == "waterformat")
                  Data.RemoveChild(Child);
               }
            }
         }
      private static void ToSoilProfile(XmlNode Data)
         {
         XmlNode Profile;
         if (XmlHelper.Type(Data).ToLower() == "soilsample" ||
             XmlHelper.Type(Data).ToLower() == "initwater" ||
             XmlHelper.Type(Data).ToLower() == "initnitrogen")
            Profile = XmlHelper.EnsureNodeExists(Data, "profile");
         else
            Profile = XmlHelper.EnsureNodeExists(Data.ParentNode, "profile");

         int NumLayers = XmlHelper.ChildNodes(Data, "layer").Count;
         if (NumLayers == 0)
            return;
         XmlHelper.EnsureNumberOfChildren(Profile, "layer", "", NumLayers);

         int LayerNumber = 0;
         foreach (XmlNode Child in XmlHelper.ChildNodes(Data, ""))
            {
            if (XmlHelper.Type(Child).ToLower() == "layer")
               {
               LayerNumber++;
               XmlNode Layer = XmlHelper.ChildNodes(Profile, "layer")[LayerNumber - 1];
               foreach (XmlNode Value in XmlHelper.ChildNodes(Child, ""))
                  {
                  if (Value.InnerText != MathUtility.MissingValue.ToString())
                     {
                     XmlNode LayerData = Layer.AppendChild(Value);

                     // truncates to 3 dec places.
                     if (Value.InnerText.IndexOf('.') != -1)
                        {
                        double DoubleValue = Convert.ToDouble(Value.InnerText);
                        LayerData.InnerText = DoubleValue.ToString("f3");
                        }

                     if (XmlHelper.Type(Data).ToLower() == "soilcrop")
                        XmlHelper.SetName(LayerData, XmlHelper.Name(Data));
                     }
                  }
               Data.RemoveChild(Child);
               }
            else if (XmlHelper.Type(Child).ToLower() != "profile")
               Data.ParentNode.AppendChild(Child);
            }
         }
      #endregion

      #region Version8
      private static void ToVersion8(XmlNode Data)
         {
         // --------------------------------------------------------------
         // Put a <thickness> into <InitWater> and <InitNitrogen> elements
         // when they have layered info.
         // --------------------------------------------------------------

         if (XmlHelper.Type(Data).ToLower() == "soil")
            {
            XmlNode WaterProfile = XmlHelper.Find(Data, "Profile");
            if (WaterProfile != null)
               {
               foreach (XmlNode Child in XmlHelper.ChildNodes(Data, ""))
                  {
                  if (XmlHelper.Type(Child).ToLower() == "initwater" ||
                      XmlHelper.Type(Child).ToLower() == "initnitrogen")
                     {
                     int LayerNumber = 1;
                     XmlNode Profile = XmlHelper.Find(Child, "Profile");
                     foreach (XmlNode Layer in XmlHelper.ChildNodes(Profile, "layer"))
                        {
                        string Thickness = GetSoilProfileInfo(WaterProfile, "thickness", LayerNumber);
                        XmlHelper.SetValue(Layer, "thickness", Thickness);
                        LayerNumber++;
                        }
                     }
                  }
               }
            }
         }
      private static string GetSoilProfileInfo(XmlNode Profile, string InfoName, int LayerNumber)
         {
         if (XmlHelper.ChildNodes(Profile, "layer").Count >= LayerNumber)
            {
            XmlNode Layer = XmlHelper.ChildNodes(Profile, "layer")[LayerNumber - 1];
            return XmlHelper.Value(Layer, InfoName);
            }
         return "";
         }
      #endregion

      private static void ToVersion9(XmlNode Data)
         {
         if (XmlHelper.Type(Data).ToLower() == "stockherbageconverter")
            {
            string[] TypesToDelete = {"proportion_legume", "dmdValue", "p_conc_green_leaf_default",
                                          "p_conc_green_stem_default", "p_conc_senesced_leaf_default",
                                          "p_conc_senesced_stem_default", "p_conc_dead_leaf_default",
                                          "p_conc_dead_stem_default", "ash_alk_green_leaf_default",
                                          "ash_alk_green_stem_default", "ash_alk_senesced_leaf_default",
                                          "ash_alk_senesced_stem_default", "ash_alk_dead_leaf_default",
                                          "ash_alk_dead_stem_default", "ns_ratio_green_leaf_default",
                                          "ns_ratio_green_stem_default", "ns_ratio_senesced_leaf_default",
                                          "ns_ratio_senesced_stem_default", "ns_ratio_dead_leaf_default",
                                          "ns_ratio_dead_stem_default", "np_ratio_green_leaf_default",
                                          "np_ratio_green_stem_default", "np_ratio_senesced_leaf_default",
                                          "np_ratio_senesced_stem_default", "np_ratio_dead_leaf_default",
                                          "np_ratio_dead_stem_default", "dmd_green_leaf",
                                          "dmd_green_stem", "dmd_senesced_leaf",
                                          "dmd_senesced_stem", "dmd_dead_leaf",
                                          "dmd_dead_stem", "KQ5Leaf",
                                          "KQ5Stem", "KQ4",
                                          "cp_n_ratio"};

            foreach (string Type in TypesToDelete)
               {
               XmlNode Child = XmlHelper.Find(Data, Type);
               if (Child != null)
                  Data.RemoveChild(Child);
               }
            }
         }

      private static void ToVersion10(XmlNode Data)
         {
         // ---------------------------------------------------------------
         // This conversion doesn't really work.
         // ---------------------------------------------------------------

         string[] OkDataTypes = {"apsimfilereader", "xmlfilereader", "probability", "filter",
                                    "cumulative", "depth", "diff", "frequency", "kwtest",
                                    "predobs", "regression", "stats", "soi", "rems", 
                                    "excelreader", "recordfilter"};

         if (XmlHelper.Type(Data).ToLower() == "data")
            {
            foreach (XmlNode Child in XmlHelper.ChildNodes(Data, ""))
               foreach (XmlNode SubChild in XmlHelper.ChildNodes(Child, ""))
                  {
                  if (Array.IndexOf(OkDataTypes, XmlHelper.Type(SubChild).ToLower()) != -1)
                     {
                     // Add a source node to our data node.
                     XmlNode NewNode = SubChild.AppendChild(XmlHelper.CreateNode(SubChild.OwnerDocument, "source", ""));
                     XmlHelper.SetValue(NewNode, "", XmlHelper.Name(SubChild.ParentNode));

                     // Move data node to parent.
                     Data.AppendChild(SubChild);
                     }
                  }
            }
         }

      #region Version11
      private static void ToVersion11(XmlNode Data)
         {
         // ---------------------------------------------------------------
         // Shortcut paths are now full paths. e.g.
         //     <manager name="SharedLogic" shortcut="/untitled/shared/SharedLogic" />
         // ---------------------------------------------------------------

         string ShortcutPath = XmlHelper.Attribute(Data, "shortcut");

         if (ShortcutPath != "" && ShortcutPath[0] != '/')
            {
            ShortcutPath = "/" + XmlHelper.Name(Data.OwnerDocument.DocumentElement)
                               + "/shared/" + ShortcutPath.Replace("\\", "/");
            if (XmlHelper.FullPath(Data) != ShortcutPath)
               XmlHelper.SetAttribute(Data, "shortcut", ShortcutPath);
            else
               XmlHelper.DeleteAttribute(Data, "shortcut");
            XmlNode RealNode = XmlHelper.Find(Data, ShortcutPath);
            MakeNodeShortcuts(Data, RealNode);
            }
         }
      private static void MakeNodeShortcuts(XmlNode ShortCutNode, XmlNode RealNode)
         {
         XmlHelper.SetName(ShortCutNode, XmlHelper.Name(RealNode));
         foreach (XmlNode Child in XmlHelper.ChildNodes(RealNode, ""))
            {
            if (Types.Instance.IsVisible(Child.Name) || Child.Name == "rule")
               {
               XmlNode NewNode = ShortCutNode.AppendChild(ShortCutNode.OwnerDocument.CreateElement(Child.Name));
               string ShortCutPath = XmlHelper.FullPath(RealNode) + "/" + XmlHelper.Name(Child);
               XmlNode RealChildNode = XmlHelper.Find(RealNode, ShortCutPath);
               XmlHelper.SetAttribute(NewNode, "shortcut", ShortCutPath);
               MakeNodeShortcuts(NewNode, Child);
               }
            }
         }
      #endregion

      private static void ToVersion12(XmlNode Data)
         {
         // -----------------------------------------------------------
         // <rule> and <logic> nodes are now called manager.
         // -----------------------------------------------------------
         string nodeName = Data.Name.ToLower();
         if (nodeName == "manager" && XmlHelper.Name(Data) != "Manager folder")
            {
            // Change <manager> to <folder name="Manager folder">
            XmlNode NewNode = XmlHelper.ChangeType(Data, "folder");
            XmlHelper.SetName(NewNode, "Manager folder");
            foreach (XmlNode Rule in XmlHelper.ChildNodes(NewNode, ""))
               ToVersion12(Rule);
            }

         else if (nodeName == "rule" || nodeName == "logic")
            {
            // Change <rule> to <manager>
            XmlNode NewNode = XmlHelper.ChangeType(Data, "manager");

            foreach (XmlNode Condition in XmlHelper.ChildNodes(NewNode, "condition"))
               {
               XmlNode ScriptNode = XmlHelper.CreateNode(NewNode.OwnerDocument, "script", "");
               NewNode.AppendChild(ScriptNode);
               XmlHelper.SetValue(ScriptNode, "text", Condition.InnerText);
               XmlHelper.SetValue(ScriptNode, "event", XmlHelper.Name(Condition));
               NewNode.RemoveChild(Condition);
               }
            if (XmlHelper.ChildNodes(NewNode, "category").Count > 0)
               {
               XmlNode UI = NewNode.AppendChild(NewNode.OwnerDocument.CreateElement("ui"));
               foreach (XmlNode Category in XmlHelper.ChildNodes(NewNode, "category"))
                  {
                  XmlNode CategoryNode = UI.AppendChild(UI.OwnerDocument.CreateElement("category"));
                  XmlHelper.SetName(CategoryNode, XmlHelper.Name(Category));
                  foreach (XmlNode Prop in XmlHelper.ChildNodes(Category, ""))
                     UI.AppendChild(UI.OwnerDocument.ImportNode(Prop, true));
                  NewNode.RemoveChild(Category);
                  }
               }
            }
         else if (nodeName == "tclmanager" || nodeName == "farmmanager" || nodeName == "paddockmanager")
            {
            foreach (XmlNode Rule in XmlHelper.ChildNodes(Data, "rule"))
               {
               if (XmlHelper.Attribute(Rule, "shortcut") != "")
                  {
                  string ShortCutPath = XmlHelper.Attribute(Rule, "shortcut");
                  XmlHelper.SetAttribute(Rule, "shortcut", ShortCutPath);
                  }
               else
                  {
                  foreach (XmlNode Condition in XmlHelper.ChildNodes(Rule, "condition"))
                     {
                     XmlNode ScriptNode = XmlHelper.CreateNode(Rule.OwnerDocument, "script", "");
                     Rule.AppendChild(ScriptNode);
                     XmlHelper.SetValue(ScriptNode, "text", Condition.InnerText);
                     XmlHelper.SetValue(ScriptNode, "event", XmlHelper.Name(Condition));
                     }
                  }
               Data.AppendChild(Rule);
               }
            }
         }

      private static void ToVersion13(XmlNode Variables)
         {
         ApplyConversionsFile(Variables, Configuration.Instance.ConversionsNode("5.4"));

         if (XmlHelper.Type(Variables) == "tclgroup")
            {
            // Clone this node with a new "type"
            XmlNode NewNode = Variables.ParentNode.AppendChild(Variables.OwnerDocument.CreateElement("tclmanager"));
            XmlHelper.SetName(NewNode, XmlHelper.Name(Variables));
            foreach (XmlNode Child in XmlHelper.ChildNodes(Variables, ""))
               {
               if (XmlHelper.Type(Child) == "initscript")
                  {
                  // make this an explicit "logic" component
                  XmlNode InitScriptNode = XmlHelper.CreateNode(Variables.OwnerDocument, "rule", "Initialisation logic");
                  XmlNode LogicScriptNode = XmlHelper.CreateNode(Variables.OwnerDocument, "script", "init");
                  XmlHelper.SetValue(LogicScriptNode, "text", Child.InnerText);
                  XmlHelper.SetValue(LogicScriptNode, "event", "init");
                  InitScriptNode.AppendChild(LogicScriptNode);
                  NewNode.AppendChild(InitScriptNode);
                  }
               else
                  {
                  NewNode.AppendChild(NewNode.OwnerDocument.ImportNode(Child, true));
                  }
               }
            Variables.ParentNode.ReplaceChild(NewNode, Variables);
            }
         }

      private static void ToVersion14(XmlNode Variables)
         {
         ApplyConversionsFile(Variables, Configuration.Instance.ConversionsNode("6.1"));
         }

      private static void ApplyConversionsFile(XmlNode Variables, XmlNode Conversions)
         {
         if (Variables.Name.ToLower() == "variables")
            {
            foreach (XmlNode Conversion in XmlHelper.ChildNodes(Conversions, "conversion"))
               {
               string[] Bits = XmlHelper.Value(Conversion, "description").Split(' ');
               if (Bits.Length == 5 && Bits[0] == "Renamed")
                  {
                  string OldName = Bits[2].ToLower();
                  string NewName = Bits[4];
                  foreach (XmlNode Variable in XmlHelper.ChildNodes(Variables, "Variable"))
                     {
                     string VariableLine = XmlHelper.Name(Variable);

                     // Do replacement where a module name was specified.
                     int Pos = VariableLine.ToLower().IndexOf("." + OldName);
                     if (Pos != -1)
                        {
                        Pos += OldName.Length + 1;
                        if (Pos == VariableLine.Length || VariableLine[Pos] == ' ')
                           {
                           Pos -= OldName.Length + 1;
                           VariableLine = VariableLine.Substring(0, Pos)
                                         + "." + NewName
                                         + VariableLine.Substring(Pos + OldName.Length + 1);
                           }
                        }
                     else if (VariableLine.Length >= OldName.Length && VariableLine.ToLower().Substring(0, OldName.Length) == OldName.ToLower())
                        {
                        VariableLine = NewName;
                        if (NewName.Length < VariableLine.Length)
                           VariableLine += VariableLine.Substring(NewName.Length);
                        }
                     XmlHelper.SetName(Variable, VariableLine);
                     }
                  }
               else if (Bits.Length == 3 && Bits[0] == "Removed")
                  {
                  string NameToDelete = Bits[2].ToLower();
                  foreach (XmlNode Variable in XmlHelper.ChildNodes(Variables, "Variable"))
                     {
                     string VariableLine = XmlHelper.Name(Variable).ToLower();
                     int PosSpace = VariableLine.IndexOf(' ');
                     if (PosSpace == -1)
                        PosSpace = VariableLine.Length;
                     int PosPeriod = VariableLine.IndexOf('.');

                     // get the variable name
                     string VariableName;
                     if (PosPeriod != -1 && PosPeriod < PosSpace)
                        VariableName = VariableLine.Substring(PosPeriod, PosSpace - PosPeriod - 1);
                     else
                        VariableName = VariableLine.Substring(0, PosSpace);

                     // Do we want to delete this variable?
                     if (VariableName == NameToDelete)
                        Variables.RemoveChild(Variable);
                     }

                  }

               }
            }
         else if (Variables.Name.ToLower() == "manager")
            {
            foreach (XmlNode Conversion in XmlHelper.ChildNodes(Conversions, "conversion"))
               {
               string[] Bits = XmlHelper.Value(Conversion, "description").Split(' ');
               if (Bits.Length == 5 && Bits[0] == "Renamed")
                  {
                  string OldName = Bits[2].ToLower();
                  string NewName = Bits[4];
                  foreach (XmlNode Script in XmlHelper.ChildNodes(Variables, "script"))
                     {
                     string Text = XmlHelper.Value(Script, "text");
                     Text = Regex.Replace(Text, "(\\W)" + OldName + "(\\W)", "$1" + NewName + "$2");
                     XmlHelper.SetValue(Script, "text", Text);
                     }
                  }
               }
            }
         }

      private static void ToVersion15(XmlNode Node)
         {
         if (XmlHelper.Type(Node) == "irrigation")
            {
            XmlNode no3_conc = Node.AppendChild(Node.OwnerDocument.CreateElement("default_no3_conc"));
            XmlHelper.SetAttribute(no3_conc, "type", "text");
            XmlHelper.SetAttribute(no3_conc, "description", "Nitrate concentration (ppm N)");
            XmlHelper.SetValue(no3_conc, "", "0.0");

            XmlNode nh4_conc = Node.AppendChild(Node.OwnerDocument.CreateElement("default_nh4_conc"));
            XmlHelper.SetAttribute(nh4_conc, "type", "text");
            XmlHelper.SetAttribute(nh4_conc, "description", "Ammonium concentration (ppm N)");
            XmlHelper.SetValue(nh4_conc, "", "0.0");

            XmlNode cl_conc = Node.AppendChild(Node.OwnerDocument.CreateElement("default_cl_conc"));
            XmlHelper.SetAttribute(cl_conc, "type", "text");
            XmlHelper.SetAttribute(cl_conc, "description", "Chloride concentration (ppm Cl)");
            XmlHelper.SetValue(cl_conc, "", "0.0");
            }
         }

      private static void ToVersion16(XmlNode Node)
         {
         // ---------------------------------------------------------------
         // Converts:
         //     <SplitReport name="Probability Exceedence">
         //       <Page name="Data" Left="0" Top="0" Width="829" Height="199">
         //         <ApsimFileReader name="OutputFile" Visible="yes" Left="0" Top="0" Width="318" Height="186">
         //           <BySeries>yes</BySeries>
         //           <FileName>
         //           </FileName>
         //         </ApsimFileReader>
         //         <Probability Left="489" Top="3" Width="128" Height="186">
         //           <FieldName>yield</FieldName>
         //           <Exceedence>Yes</Exceedence>
         //           <source>SeriesSplitter</source>
         //         </Probability>
         //         <SeriesSplitter Left="346" Top="0" Width="113" Height="189">
         //           <source>OutputFile</source>
         //           <FieldName>Title</FieldName>
         //         </SeriesSplitter>
         //       </Page>
         //       <Page name="Report" Left="0" Top="202" Width="829" Height="295">
         //         <Chart Left="18" Top="10" RightMargin="5" BottomMargin="5" Width="586" Height="478">
         //           <source>Probability</source>
         //           <XY>
         //             <X>*</X>
         //             <Y>Probability</Y>
         //             <SeriesType>Line</SeriesType>
         //             <PointType>None</PointType>
         //           </XY>
         //           <Properties>
         //             <Axes>
         //               <LeftAxis>
         //                 <Maximum>100</Maximum>
         //                 <Minimum>0</Minimum>
         //               </LeftAxis>
         //               <BottomAxis />
         //               <TopAxis />
         //               <RightAxis />
         //             </Axes>
         //           </Properties>
         //         </Chart>
         //       </Page>
         //      </SplitReport>
         //  
         //  To:
         //     <Graph name="Probability Exceedence">
         //        <Plot name="">
         //          <SeriesType>Solid line</SeriesType>
         //          <PointType>None</PointType>
         //          <Y>Probability</Y>
         //          <GDProbability>
         //            <GDApsimFileReader name="ApsimFileReader"/>
         //            <Exceedence>Yes</Exceedence>
         //          </GDProbability>
         //        </Plot>
         //      </Graph>
         //  ---------------------------------------------------------------

         if (Node.Name.ToLower() == "splitreport" || Node.Name.ToLower() == "tabbedreport")
            {
            XmlNode Data = XmlHelper.Find(Node, "data");
            if (Data != null)
               {
               string ChartName = XmlHelper.Name(Node);
               XmlHelper.SetName(Node, "zzzzzzzzzzzzzzz");
               foreach (XmlNode Page in XmlHelper.ChildNodes(Node, "page"))
                  {
                  XmlNode Chart = null;
                  if (XmlHelper.ChildNodes(Page, "chart").Count > 1)
                     {
                     // Turn page into a report.
                     Chart = ProcessReport(Page, Data);
                     }
                  else if (XmlHelper.ChildNodes(Page, "chart").Count == 1)
                     {
                     // Simple chart.
                     Chart = ProcessChart(XmlHelper.Find(Page, "chart"), Page.ParentNode.ParentNode, Data);
                     }
                  if (Chart != null)
                     {
                     XmlHelper.SetName(Chart, ChartName);
                     XmlHelper.EnsureNodeIsUnique(Chart);
                     }
                  }
               }
            Node.ParentNode.RemoveChild(Node);
            }
         }

      private static void ToVersion17(XmlNode Node)
         {
         // ---------------------------------------------------------------
         // Converts:
         //     <memo>e1xydGYxXGFuc2lcYW5zaWNwZzEyNTJcZGVmZjB</memo>
         //  
         //  To:
         //     <memo>This is some text</memo>
         //  ---------------------------------------------------------------

         if (Node.Name.ToLower() == "memo")
            {
            System.Windows.Forms.RichTextBox RichText = new System.Windows.Forms.RichTextBox();
            RichText.Rtf = CSGeneral.Utility.EncodeBase64ToString(Node.InnerXml);
            XmlHelper.SetValue(Node, "", RichText.Text.Replace("\n", "\r\n"));
            }
         }


      private static XmlNode ProcessReport(XmlNode Page, XmlNode Data)
         {
         XmlNode Report = Page.ParentNode.ParentNode.AppendChild(Page.OwnerDocument.CreateElement("GraphReport"));
         int ChartNumber = 1;
         foreach (XmlNode Chart in XmlHelper.ChildNodes(Page, "chart"))
            {
            XmlNode Graph = ProcessChart(Chart, Report, Data);
            XmlHelper.SetName(Graph, "Graph" + ChartNumber.ToString());
            ChartNumber++;
            }
         return Report;
         }


      private static XmlNode ProcessChart(XmlNode Node, XmlNode ParentForChart, XmlNode Data)
         {
         XmlNode Chart = ParentForChart.AppendChild(Node);
         Chart = XmlHelper.ChangeType(Chart, "Graph");

         // If there is a <source> element under this chart then go put it into all <xy> nodes.
         XmlNode Source = XmlHelper.Find(Chart, "source");
         if (Source != null)
            {
            foreach (XmlNode XY in XmlHelper.ChildNodes(Chart, "XY"))
               XY.AppendChild(Source.CloneNode(true));
            if (XmlHelper.ChildNodes(Chart, "XY").Count > 0)
               Chart.RemoveChild(Source);
            }

         // recursively go resolve all <source> nodes.
         ResolveSourceNodes(Chart, Data);

         CleanUpGraphNode(Chart);
         return Chart;
         }

      private static void ResolveSourceNodes(XmlNode Node, XmlNode Data)
         {
         // --------------------------------------------------------------------
         // Go looking for all <source> nodes and replace them with the 
         // appropriate nodes under the Data node.
         // --------------------------------------------------------------------
         
         foreach (XmlNode Source in XmlHelper.ChildNodes(Node, "source"))
            {
            string SourceDataName = Source.InnerText;
            if (SourceDataName != "")
               {
               XmlNode SourceDataNode = XmlHelper.Find(Data, SourceDataName);
               if (SourceDataNode != null && SourceDataNode.Name == "SeriesSplitter")
                  {
                  SourceDataName = XmlHelper.Value(SourceDataNode, "source");
                  SourceDataNode = XmlHelper.Find(Data, SourceDataName);
                  }

               if (SourceDataNode != null)
                  {
                  XmlNode NewSource = SourceDataNode.CloneNode(true);
                  Node.ReplaceChild(NewSource, Source);
                  ResolveSourceNodes(NewSource, Data);
                  }
               }
            }

         // Recurse through all the <XY> nodes.
         foreach (XmlNode XY in XmlHelper.ChildNodes(Node, "xy"))
            ResolveSourceNodes(XY, Data);
         }

      private static void CleanUpGraphNode(XmlNode Node)
         {
         // --------------------------------------------------------------------
         // Cleanup all unwanted cruft from the Node.
         // --------------------------------------------------------------------
         
         string[] NodesToDelete = { "BySeries", "Colour", "Properties", "Phase" };

         string[] OldNames = {"XY", "ApsimFileReader", "Probability", "SOIData", "Depth", "Filter",  };
         string[] NewNames = { "Plot", "GDApsimFileReader", "GDProbability", "GDSOI", "GDDepth", "GDFilter" };
         
         string NodeName = XmlHelper.Name(Node);
         Node.Attributes.RemoveAll();
         XmlHelper.SetName(Node, NodeName);

         // If we have a value of '*' then delete ourselves and go no further
         if (Node.InnerText == "*" || (Node.InnerText.Length > 0 && Node.InnerText.Substring(0, 1) == "!"))
            {
            Node.ParentNode.RemoveChild(Node);
            return;
            }

         if (Node.InnerText == "SeriesName")
            Node.InnerText = "Title";

         // See if there are child nodes we can delete.
         foreach (XmlNode Child in XmlHelper.ChildNodes(Node, ""))
            {
            if (Array.IndexOf(NodesToDelete, Child.Name) != -1)
               Child.ParentNode.RemoveChild(Child);
            }

         // Change month string to month number.
         if (Node.Name == "Month")
            {
            string[] Months = { "January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December" };
            int MonthIndex = Array.IndexOf(Months, Node.InnerText);
            if (MonthIndex == -1)
               Node.ParentNode.RemoveChild(Node);
            else
               {
               MonthIndex++;
               Node.InnerText = MonthIndex.ToString();
               }
            return;
            }

         // If this is a <depth> node then extract the child <filter> node
         if (Node.Name == "Depth")
            ProcessDepthGraph(Node);

         // If this is a <depth> node then extract the child <filter> node
         if (Node.Name == "SOIData")
            {
            XmlNode FileName = XmlHelper.Find(Node, "filename");
            if (FileName != null)
               Node.RemoveChild(FileName);
            }

         //See if we need to rename ourselves.
         int Index = Array.IndexOf(OldNames, Node.Name);
         if (Index != -1)
            Node = XmlHelper.ChangeType(Node, NewNames[Index]);

         // Now recurse down to clean up all children.
         foreach (XmlNode Child in XmlHelper.ChildNodes(Node, ""))
            CleanUpGraphNode(Child);

         }

      private static void ProcessDepthGraph(XmlNode Node)
         {
         // --------------------------------------------------------------------
         // Need to convert: 
         //    <GDDepth>
         //      <GDFilter>
         //        <FilterString>Date='10/10/1942'</FilterString>
         //        <GDApsimFileReader name="OutputFile">
         //          <FileName>
         //          </FileName>
         //        </GDApsimFileReader>
         //      </GDFilter>
         //    </GDDepth>
         // To:
         //    <GDDepth>
         //      <Date>10/10/1942</Date>
         //      <GDApsimFileReader name="OutputFile">
         //        <FileName>
         //        </FileName>
         //      </GDApsimFileReader>
         //    </GDDepth>
         // --------------------------------------------------------------------

         XmlNode Filter = XmlHelper.Find(Node, "filter");
         List<string> DateStrings = XmlHelper.Values(Filter, "FilterString");

         for (int i = 0; i != DateStrings.Count; i++)
            {
            DateStrings[i] = DateStrings[i].Replace("Date='", "");
            DateStrings[i] = DateStrings[i].Replace("'", "");
            }
         XmlHelper.SetValues(Node, "Date", DateStrings);
         foreach (XmlNode FilterChild in XmlHelper.ChildNodes(Filter, ""))
            {
            if (FilterChild.Name != "FilterString")
               Node.AppendChild(FilterChild);
            }
         Node.RemoveChild(Filter);
         }


      private static void ToVersion18(XmlNode Node)
         {
         // ---------------------------------------------------------------
         // Removes:
         //     <registrations name="global" />
         // from all paddocks
         //  ---------------------------------------------------------------

         if (Node.Name.ToLower() == "area")
            {
            XmlNode RegistrationsNode = XmlHelper.FindByType(Node, "registrations");
            if (RegistrationsNode != null)
               Node.RemoveChild(RegistrationsNode);
            }
         }

      private static void ToVersion19(XmlNode Node)
         {
         // ----------------------------------------------------------------
         // Renames phos type to soilp:
         //     <phos />   to <soilp />
         //  ---------------------------------------------------------------

         if (Node.Name.ToLower() == "phos")
            {
            XmlHelper.ChangeType(Node, "soilp");
            }
         }
      private static void ToVersion20(XmlNode Node)
         {
         // ----------------------------------------------------------------
         // Rework the soil <profile> node into separate child nodes.
         //  ---------------------------------------------------------------

         if (Node.Name.ToLower() == "soil")
            {
            // If there is a <Phosphorus> node then get rid of it. It is old and not used.
            XmlNode OldPhosphorusNode = XmlHelper.FindByType(Node, "Phosphorus");
            if (OldPhosphorusNode != null)
               Node.RemoveChild(OldPhosphorusNode);

            XmlNode ProfileNode = XmlHelper.Find(Node, "profile");
            if (ProfileNode != null)
               {
               XmlNode WaterNode = Node.AppendChild(Node.OwnerDocument.CreateElement("Water"));
               XmlNode SoilWatNode = Node.AppendChild(Node.OwnerDocument.CreateElement("SoilWat"));
               XmlNode SOMNode = Node.AppendChild(Node.OwnerDocument.CreateElement("SoilOrganicMatter"));
               XmlNode LABNode = Node.AppendChild(Node.OwnerDocument.CreateElement("Lab"));

               XmlNode CountryNode = AnnotateNode(Node, "Country", "", "");
               Node.InsertBefore(CountryNode, Node.FirstChild);

               AnnotateNode(Node, "State", "", "");
               AnnotateNode(Node, "Region", "", "");
               AnnotateNode(Node, "NearestTown", "", "Nearest town");
               AnnotateNode(Node, "Site", "", "");
               AnnotateNode(Node, "ApsoilNumber", "", "Apsoil number");
               AnnotateNode(Node, "SoilType", "", "Classification");
               AnnotateNode(Node, "Latitude", "", "Latitude (WGS84)");
               AnnotateNode(Node, "Langitude", "", "Longitude (WGS84)");
               AnnotateNode(Node, "LocationAccuracy", "", "Location accuracy");
               AnnotateNode(Node, "NaturalVegetation", "", "Natural vegetation");
               AnnotateNode(Node, "DataSource", "multiedit", "Data source");
               AnnotateNode(Node, "Comment", "multiedit", "Comments");

               XmlNode ConaNode = XmlHelper.Find(Node, "CONA");
               if (ConaNode != null)
                  {
                  XmlHelper.SetValue(SoilWatNode, "SummerCona", ConaNode.InnerText);
                  XmlHelper.SetValue(SoilWatNode, "WinterCona", ConaNode.InnerText);
                  Node.RemoveChild(ConaNode);
                  XmlNode UNode = XmlHelper.Find(Node, "U");
                  if (UNode != null)
                     {
                     XmlHelper.SetValue(SoilWatNode, "SummerU", UNode.InnerText);
                     XmlHelper.SetValue(SoilWatNode, "WinterU", UNode.InnerText);
                     Node.RemoveChild(UNode);
                     }
                  }
               else
                  {
                  MoveSoilNode(Node, "SummerCona", SoilWatNode);
                  MoveSoilNode(Node, "SummerU", SoilWatNode);
                  MoveSoilNode(Node, "SummerDate", SoilWatNode);
                  MoveSoilNode(Node, "WinterCona", SoilWatNode);
                  MoveSoilNode(Node, "WinterU", SoilWatNode);
                  MoveSoilNode(Node, "WinterDate", SoilWatNode);
                  }
               if (XmlHelper.Value(SoilWatNode, "SummerDate") == "")
                  XmlHelper.SetValue(SoilWatNode, "SummerDate", "1-Nov");
               if (XmlHelper.Value(SoilWatNode, "WinterDate") == "")
                  XmlHelper.SetValue(SoilWatNode, "WinterDate", "1-Apr");

               MoveSoilNode(Node, "DiffusConst", SoilWatNode);
               MoveSoilNode(Node, "DiffusSlope", SoilWatNode);
               MoveSoilNode(Node, "SALB", SoilWatNode);
               MoveSoilNode(Node, "CN2Bare", SoilWatNode);
               MoveSoilNode(Node, "CNRed", SoilWatNode);
               MoveSoilNode(Node, "CNCov", SoilWatNode);
               MoveSoilNode(Node, "CNCanopyFact", SoilWatNode);
               MoveSoilNode(Node, "DiffusConst", SoilWatNode);
               MoveSoilNode(Node, "RootCN", SoilWatNode);
               MoveSoilNode(Node, "RootWT", SoilWatNode);
               MoveSoilNode(Node, "SoilCN", SoilWatNode);
               MoveSoilNode(Node, "EnrACoeff", SoilWatNode);
               MoveSoilNode(Node, "EnrBCoeff", SoilWatNode);
               foreach (XmlNode LayerNode in XmlHelper.ChildNodes(ProfileNode, "Layer"))
                  {
                  XmlNode WaterLayerNode = WaterNode.AppendChild(Node.OwnerDocument.CreateElement("Layer"));
                  XmlNode SoilWatLayerNode = SoilWatNode.AppendChild(Node.OwnerDocument.CreateElement("Layer"));
                  XmlNode SOMLayerNode = SOMNode.AppendChild(Node.OwnerDocument.CreateElement("Layer"));
                  XmlNode LABLayerNode = LABNode.AppendChild(Node.OwnerDocument.CreateElement("Layer"));

                  SetValue(WaterLayerNode, "Thickness", XmlHelper.Value(LayerNode, "Thickness"), "mm");
                  SetValue(SoilWatLayerNode, "Thickness", XmlHelper.Value(LayerNode, "Thickness"), "mm");
                  SetValue(SOMLayerNode, "Thickness", XmlHelper.Value(LayerNode, "Thickness"), "mm");
                  SetValue(LABLayerNode, "Thickness", XmlHelper.Value(LayerNode, "Thickness"), "mm");

                  SetValue(WaterLayerNode, "BD", XmlHelper.Value(LayerNode, "BD"), "g/cc");
                  SetValue(WaterLayerNode, "AirDry", XmlHelper.Value(LayerNode, "AirDry"), "mm/mm");
                  SetValue(WaterLayerNode, "LL15", XmlHelper.Value(LayerNode, "LL15"), "mm/mm");
                  SetValue(WaterLayerNode, "DUL", XmlHelper.Value(LayerNode, "DUL"), "mm/mm");
                  SetValue(WaterLayerNode, "SAT", XmlHelper.Value(LayerNode, "SAT"), "mm/mm");
                  SetValue(WaterLayerNode, "KS", XmlHelper.Value(LayerNode, "KS"), "mm/day");
                  foreach (XmlNode LLNode in XmlHelper.ChildNodes(LayerNode, "ll"))
                     {
                     string CropName = XmlHelper.Name(LLNode);
                     XmlNode CropNode = XmlHelper.Find(WaterNode, CropName);
                     if (CropNode == null)
                        {
                        CropNode = WaterNode.AppendChild(Node.OwnerDocument.CreateElement("SoilCrop"));
                        XmlHelper.SetName(CropNode, CropName);
                        }
                     XmlNode CropLayerNode = CropNode.AppendChild(Node.OwnerDocument.CreateElement("Layer"));
                     SetValue(CropLayerNode, "Thickness", XmlHelper.Value(LayerNode, "Thickness"), "mm");
                     SetValue(CropLayerNode, "LL", LLNode.InnerText, "mm/mm");

                     if (XmlHelper.ChildByNameAndType(LayerNode, CropName, "KL") != null)
                        SetValue(CropLayerNode, "KL", XmlHelper.ChildByNameAndType(LayerNode, CropName, "KL").InnerText, "/day");
                     if (XmlHelper.ChildByNameAndType(LayerNode, CropName, "XF") != null)
                        SetValue(CropLayerNode, "XF", XmlHelper.ChildByNameAndType(LayerNode, CropName, "XF").InnerText, "0-1");
                     }

                  SetValue(SoilWatLayerNode, "SWCON", XmlHelper.Value(LayerNode, "SWCON"), "0-1");
                  SetValue(SoilWatLayerNode, "MWCON", XmlHelper.Value(LayerNode, "MWCON"), "0-1");

                  SetValue(SOMLayerNode, "OC", XmlHelper.Value(LayerNode, "OC"), "Total %");
                  SetValue(SOMLayerNode, "FBiom", XmlHelper.Value(LayerNode, "FBiom"), "0-1");
                  SetValue(SOMLayerNode, "FInert", XmlHelper.Value(LayerNode, "FInert"), "0-1");

                  SetValue(LABLayerNode, "Rocks", XmlHelper.Value(LayerNode, "Rocks"), "%");
                  SetValue(LABLayerNode, "Texture", XmlHelper.Value(LayerNode, "Texture"), "");
                  SetValue(LABLayerNode, "EC", XmlHelper.Value(LayerNode, "EC"), "1:5 dS/m");
                  SetValue(LABLayerNode, "PH", XmlHelper.Value(LayerNode, "PH"), "1:5 water");
                  SetValue(LABLayerNode, "CL", XmlHelper.Value(LayerNode, "CL"), "mg/kg");
                  SetValue(LABLayerNode, "Boron", XmlHelper.Value(LayerNode, "Boron"), "mg/kg");
                  SetValue(LABLayerNode, "CEC", XmlHelper.Value(LayerNode, "CEC"), "cmol+/kg");
                  SetValue(LABLayerNode, "Ca", XmlHelper.Value(LayerNode, "Ca"), "cmol+/kg");
                  SetValue(LABLayerNode, "Mg", XmlHelper.Value(LayerNode, "Mg"), "cmol+/kg");
                  SetValue(LABLayerNode, "Na", XmlHelper.Value(LayerNode, "Na"), "cmol+/kg");
                  SetValue(LABLayerNode, "K", XmlHelper.Value(LayerNode, "K"), "cmol+/kg");
                  SetValue(LABLayerNode, "ESP", XmlHelper.Value(LayerNode, "ESP"), "%");
                  SetValue(LABLayerNode, "Mn", XmlHelper.Value(LayerNode, "Mn"), "mg/kg");
                  SetValue(LABLayerNode, "Al", XmlHelper.Value(LayerNode, "Al"), "meq/100g");
                  SetValue(LABLayerNode, "ParticleSizeSand", XmlHelper.Value(LayerNode, "ParticleSizeSand"), "%");
                  SetValue(LABLayerNode, "ParticleSizeSilt", XmlHelper.Value(LayerNode, "ParticleSizeSilt"), "%");
                  SetValue(LABLayerNode, "ParticleSizeClay", XmlHelper.Value(LayerNode, "ParticleSizeClay"), "%");
                  }

               // Move phosphorus stuff if necessary.
               if (XmlHelper.Value(Node, "RootCP") != "" && ProfileNode != null)
                  {
                  XmlNode PhosphorusNode = Node.AppendChild(Node.OwnerDocument.CreateElement("Phosphorus"));
                  MoveSoilNode(Node, "RootCP", PhosphorusNode);
                  MoveSoilNode(Node, "RateDissolRock", PhosphorusNode);
                  MoveSoilNode(Node, "RateLossAvail", PhosphorusNode);
                  foreach (XmlNode LayerNode in ProfileNode)
                     {
                     XmlNode PhosphorusLayerNode = PhosphorusNode.AppendChild(Node.OwnerDocument.CreateElement("Layer"));
                     SetValue(PhosphorusLayerNode, "Thickness", XmlHelper.Value(LayerNode, "Thickness"), "mm");
                     SetValue(PhosphorusLayerNode, "LabileP", XmlHelper.Value(LayerNode, "LabileP"), "mg/kg");
                     SetValue(PhosphorusLayerNode, "BandedP", XmlHelper.Value(LayerNode, "BandedP"), "kg/ha");
                     SetValue(PhosphorusLayerNode, "RockP", XmlHelper.Value(LayerNode, "RockP"), "kg/ha");
                     SetValue(PhosphorusLayerNode, "Sorption", XmlHelper.Value(LayerNode, "Sorption"), "-");
                     }
                  }

               Node.RemoveChild(ProfileNode);
               }

            // Turn the <InitNitrogen> element into a sample node.  
            XmlNode InitNitrogenNode = XmlHelper.Find(Node, "InitNitrogen");
            if (InitNitrogenNode != null)
               ConvertSampleNode(Node, InitNitrogenNode, "Initial nitrogen");

            // Turn the <InitWater> element into a sample node IF it has layered values.
            XmlNode InitWaterNode = XmlHelper.Find(Node, "InitWater");
            if (InitWaterNode != null)
               ConvertSampleNode(Node, InitWaterNode, "Initial water");
            
            // Change all <SoilSample> nodes to <Sample>
            foreach (XmlNode Child in XmlHelper.ChildNodes(Node, "SoilSample"))
               ConvertSampleNode(Node, Child, XmlHelper.Name(Child));
            }
         else if (Node.Name.ToLower() == "soilp")
            Node.ParentNode.RemoveChild(Node);
         }

      private static void ConvertSampleNode(XmlNode SoilNode, XmlNode OldSampleNode, string NewNodeName)
         {
         if (XmlHelper.Attribute(OldSampleNode, "shortcut") != "")
            {
            // Remove the <InitWater> node as it's a shortcut that will be readded later.
            OldSampleNode.ParentNode.RemoveChild(OldSampleNode);
            }
         else
            {
            XmlNode ProfileNode = XmlHelper.Find(OldSampleNode, "Profile");
            if (ProfileNode == null)
               {
               XmlHelper.SetName(OldSampleNode, NewNodeName);
               if (!OldSampleNode.HasChildNodes)
                  XmlHelper.SetValue(OldSampleNode, "PercentMethod/Percent", "0");
               }
            else
               {
               XmlNode SampleNode = SoilNode.AppendChild(SoilNode.OwnerDocument.CreateElement("Sample"));
               XmlHelper.SetName(SampleNode, NewNodeName);
               AnnotateNode(SampleNode, "Date", "date", "Sample date:");

               foreach (XmlNode LayerNode in ProfileNode.ChildNodes)
                  {
                  XmlNode NewLayerNode = SampleNode.AppendChild(SoilNode.OwnerDocument.CreateElement("Layer"));
                  SetValue(NewLayerNode, "Thickness", XmlHelper.Value(LayerNode, "Thickness"), "mm");
                  if (XmlHelper.Value(OldSampleNode, "WaterFormat") == "GravimetricPercent")
                     SetValue(NewLayerNode, "SW", XmlHelper.Value(LayerNode, "sw"), "grav. mm/mm");
                  else
                     SetValue(NewLayerNode, "SW", XmlHelper.Value(LayerNode, "sw"), "mm/mm");
                  SetValue(NewLayerNode, "NO3", XmlHelper.Value(LayerNode, "no3"), "ppm");
                  SetValue(NewLayerNode, "NH4", XmlHelper.Value(LayerNode, "nh4"), "ppm");
                  SetValue(NewLayerNode, "OC", XmlHelper.Value(LayerNode, "oc"), "Total %");
                  SetValue(NewLayerNode, "EC", XmlHelper.Value(LayerNode, "ec"), "1:5 dS/m");
                  SetValue(NewLayerNode, "PH", XmlHelper.Value(LayerNode, "ph"), "CaCl2");
                  SetValue(NewLayerNode, "CL", XmlHelper.Value(LayerNode, "cl"), "mg/kg");
                  }

               // Remove old <InitWater> node.
               SoilNode.RemoveChild(OldSampleNode);
               }
            }
         }


      private static void ToVersion21(XmlNode Node)
         {
         // ----------------------------------------------------------------
         // Rework the soil nodes that have shortcuts.
         // ---------------------------------------------------------------

         if (Node.Name.ToLower() == "soil" && XmlHelper.Attribute(Node, "shortcut") != "")
            {
            string ShortCutPath = XmlHelper.Attribute(Node, "shortcut");
            XmlNode ShortcutSourceNode = XmlHelper.Find(Node.OwnerDocument.DocumentElement, ShortCutPath);
            if (ShortcutSourceNode != null)
               ToVersion21(ShortcutSourceNode);  // recursion
            CreateChildWithShortcut(Node, ShortCutPath, "Water", "Water");
            CreateChildWithShortcut(Node, ShortCutPath, "SoilWat", "SoilWat");
            CreateChildWithShortcut(Node, ShortCutPath, "SoilOrganicMatter", "SoilOrganicMatter");
            CreateChildWithShortcut(Node, ShortCutPath, "Lab", "Lab");
            CreateChildWithShortcut(Node, ShortCutPath, "Initial Water", "InitWater");
            CreateChildWithShortcut(Node, ShortCutPath, "Initial Water", "Sample");
            CreateChildWithShortcut(Node, ShortCutPath, "Initial Nitrogen", "Sample");
            CreateChildWithShortcut(Node, ShortCutPath, "Phosphorus", "Phosphorus");

            // Now shortcut all crop nodes.
            XmlNode ShortcutSourceWaterNode = XmlHelper.Find(Node.OwnerDocument.DocumentElement, ShortCutPath + "/Water");
            XmlNode WaterNode = XmlHelper.Find(Node, "Water");
            if (ShortcutSourceWaterNode != null && WaterNode != null)
               {
               foreach (string CropName in XmlHelper.ChildNames(ShortcutSourceWaterNode, "SoilCrop"))
                  CreateChildWithShortcut(WaterNode, ShortCutPath + "/Water", CropName, "SoilCrop");
               }
            }
         }

      /// <summary>
      /// Create a child node with a shortcut attribute if the child doesn't already exist
      /// and the shortcut source code does have the child.
      /// </summary>
      private static void CreateChildWithShortcut(XmlNode Node, string ShortCutPath, string ChildName, string ChildType)
         {
         XmlNode ShortcutSourceNode = XmlHelper.Find(Node.OwnerDocument.DocumentElement, ShortCutPath);
         if (ShortcutSourceNode != null && XmlHelper.Find(Node, ChildName) == null)
            {
            XmlNode ShortcutSourceNodeChild = XmlHelper.Find(ShortcutSourceNode, ShortCutPath + "/" + ChildName);
            if (ShortcutSourceNodeChild != null && XmlHelper.Type(ShortcutSourceNodeChild) == ChildType)
               {
               XmlNode Child = Node.AppendChild(Node.OwnerDocument.CreateElement(ChildType));
               if (ChildName != ChildType)
                  XmlHelper.SetName(Child, ChildName);
               XmlHelper.SetAttribute(Child, "shortcut", ShortCutPath + "/" + ChildName);
               }
            }
         }

      private static void SetValue(XmlNode Node, string NodeName, string Value, string Units)
         {
         if (Value != "")
            {
            XmlHelper.SetValue(Node, NodeName, Value);
            // Only put the units on the first layer.
            XmlNode FirstChild = XmlHelper.ChildNodes(Node.ParentNode, "Layer")[0];
            if (Node == FirstChild)
               XmlHelper.SetAttribute(XmlHelper.Find(Node, NodeName), "units", Units);
            }
         }

      private static XmlNode AnnotateNode(XmlNode Node, string NodeName, string Type, string Description)
         {
         XmlNode NodeToAnnotate = XmlHelper.EnsureNodeExists(Node, NodeName);
         if (NodeToAnnotate.Name != NodeName)
            {
            // Must be different case - fix.
            NodeToAnnotate = XmlHelper.ChangeType(NodeToAnnotate, NodeName);
            }

         if (Type != "")
            XmlHelper.SetAttribute(NodeToAnnotate, "type", Type);
         if (Description != "")
            XmlHelper.SetAttribute(NodeToAnnotate, "description", Description);

         return NodeToAnnotate;
         }

      private static void MoveSoilNode(XmlNode Node, string NodeName, XmlNode NodeToMoveTo)
         {
         XmlNode NodeToMove = XmlHelper.Find(Node, NodeName);
         if (NodeToMove != null)
            NodeToMoveTo.AppendChild(NodeToMove);
         }
      
      private static void ToVersion22(XmlNode Node)
         {
         // ----------------------------------------------------------------
         // Rework the soil nodes that have shortcuts.
         // ---------------------------------------------------------------

         if (Node.Name.ToLower() == "soil" && XmlHelper.Attribute(Node, "shortcut") == "")
            {
            XmlNode SoilWatNode = XmlHelper.Find(Node, "SoilWat");
            if (SoilWatNode != null)
               {
               XmlHelper.SetValue(SoilWatNode, "Slope", "");
               XmlHelper.SetValue(SoilWatNode, "DischargeWidth", "");
               XmlHelper.SetValue(SoilWatNode, "CatchmentArea", "");
               XmlHelper.SetValue(SoilWatNode, "MaxPond", "");
               }
            XmlNode SoilOrganicMatterNode = XmlHelper.Find(Node, "SoilOrganicMatter");
            if (SoilOrganicMatterNode != null)
               {
               MoveSoilNode(Node, "SoilWat/RootCN", SoilOrganicMatterNode);
               MoveSoilNode(Node, "SoilWat/RootWT", SoilOrganicMatterNode);
               MoveSoilNode(Node, "SoilWat/SoilCN", SoilOrganicMatterNode);
               MoveSoilNode(Node, "SoilWat/EnrACoeff", SoilOrganicMatterNode);
               MoveSoilNode(Node, "SoilWat/EnrBCoeff", SoilOrganicMatterNode);
               }
            }
         }

      private static void ToVersion23(XmlNode Node)
         {
         // ----------------------------------------------------------------
         // Rework the clock start and end date nodes so that they have a
         // format specifier.
         // ---------------------------------------------------------------

         if (Node.Name.ToLower() == "clock" && XmlHelper.Attribute(Node, "shortcut") == "")
            {
            foreach (XmlNode Child in XmlHelper.ChildNodes(Node, ""))
               {
               string Description = XmlHelper.Attribute(Child, "description");
               StringManip.SplitOffBracketedValue(ref Description, '(', ')');
               XmlHelper.SetAttribute(Child, "description", Description);
               XmlHelper.SetAttribute(Child, "type", "date");
               }
            }
         }

      private static void ToVersion24(XmlNode Node)
         {
         // ----------------------------------------------------------------
         // Add SorptionCoeff property to Phosphorus nodes.
         // ---------------------------------------------------------------

         if (Node.Name.ToLower() == "soil" && XmlHelper.Attribute(Node, "shortcut") == "")
            {
            XmlNode PhosphorusNode = XmlHelper.Find(Node, "Phosphorus");
            if (PhosphorusNode != null && XmlHelper.Attribute(PhosphorusNode, "shortcut") == "")
               {
               XmlHelper.SetValue(PhosphorusNode, "SorptionCoeff", "0.7");
               }
            }
         }

      private static void ToVersion25(XmlNode Node)
         {
         // ----------------------------------------------------------------
         // Make sure the soil nodes are complete and in the right order.
         // ----------------------------------------------------------------
         if (Node.Name.ToLower() == "soil")
            {
            if (XmlHelper.Attribute(Node, "shortcut") != "")
               {
               XmlNode LabChild = XmlHelper.Find(Node, "Lab");
               if (LabChild != null && XmlHelper.Attribute(LabChild, "shortcut") != "")
                  {
                  string ShortCut = XmlHelper.Attribute(LabChild, "shortcut").Replace("/Lab", "/Analysis");
                  XmlNode AnalysisChild = XmlHelper.ChangeType(LabChild, "Analysis");
                  XmlHelper.SetAttribute(AnalysisChild, "shortcut", ShortCut);
                  }
               }
            else
               {
               string[] SoilProperties = {"Country", "Site", "Region", "LocalName", "SoilType",
                                   "NearestTown",
                                   "NaturalVegetation",
                                   "State",
                                   "ApsoilNumber",
                                   "Latitude",
                                   "Longitude",
                                   "LocationAccuracy",
                                   "DataSource",
                                   "Comments"};
               SetPropertiesOrder(Node, SoilProperties, null, false);

               // Order the nodes under <Water>
               XmlNode WaterNode = XmlHelper.Find(Node, "Water");
               if (WaterNode != null)
                  {
                  string[] Variables = { "Thickness", "KS", "BD", "AirDry", "LL15", "DUL", "SAT" };
                  string[] VariableUnits = { "mm", "mm/day", "g/cc", "mm/mm", "mm/mm", "mm/mm", "mm/mm" };
                  SetLayeredOrder(WaterNode, Variables, VariableUnits);

                  // Order the variables in <SoilCrop>
                  string[] CropVariables = { "Thickness", "LL", "KL", "XF" };
                  string[] CropUnits = { "mm", "mm/mm", "/day", "0-1" };

                  foreach (XmlNode CropNode in XmlHelper.ChildNodes(WaterNode, "SoilCrop"))
                     {
                     SetLayeredOrder(CropNode, CropVariables, CropUnits);
                     }
                  }

               // Order the nodes under <SoilWat>
               XmlNode SoilWatNode = XmlHelper.Find(Node, "SoilWat");
               if (SoilWatNode != null)
                  {
                  string[] Properties = { "SummerCona", "SummerU", "SummerDate", "WinterCona",
                                       "WinterU", "WinterDate", "DiffusConst", "DiffusSlope",
                                       "Salb", "Cn2Bare", "CnRed", "CnCov", "CnCanopyFact",
                                       "Slope", "DischargeWidth", "CatchmentArea", "MaxPond"};
                  SetPropertiesOrder(SoilWatNode, Properties, null, false);
                  string[] Variables = { "Thickness", "SWCON", "MWCON", "KLAT" };
                  string[] Units = { "mm", "0-1", "0-1", "mm/d" };
                  SetLayeredOrder(SoilWatNode, Variables, Units);
                  }

               // Order the nodes under <SoilOrganicMatter>
               XmlNode SOMNode = XmlHelper.Find(Node, "SoilOrganicMatter");
               if (SOMNode != null)
                  {
                  string[] Properties = { "RootCN", "RootWt", "SoilCn", "EnrACoeff", "EnrBCoeff" };
                  SetPropertiesOrder(SOMNode, Properties, null, false);

                  string[] Variables = { "Thickness", "OC", "FBiom", "FInert" };
                  string[] Units = { "mm", "Total %", "0-1", "0-1" };
                  SetLayeredOrder(SOMNode, Variables, Units);
                  }
               // Order the nodes under <Lab>
               XmlNode LabNode = XmlHelper.Find(Node, "Lab");
               if (LabNode != null)
                  {
                  string[] Variables = { "Thickness", "Rocks", "Texture", "MunsellColour", "EC", "PH", "CL", "Boron", "CEC",
                                      "Ca", "Mg", "Na", "K", "ESP", "Mn", "Al",
                                      "ParticleSizeSand", "ParticleSizeSilt", "ParticleSizeClay"};



                  string[] Units =     { "mm", "%", "", "", "1:5 dS/m", "1:5 water", "mg/kg", "Hot water mg/kg", "cmol+/kg",
                                      "cmol+/kg", "cmol+/kg", "cmol+/kg", "cmol+/kg", "%", "mg/kg", "cmol+/kg",
                                      "%", "%", "%"};
                  SetLayeredOrder(LabNode, Variables, Units);
                  XmlHelper.ChangeType(LabNode, "Analysis");
                  }
               // Order the nodes under <Sample>
               foreach (XmlNode SampleNode in XmlHelper.ChildNodes(Node, "Sample"))
                  {
                  string[] Variables = { "Thickness", "NO3", "NH4", "SW" };
                  string[] Units = { "mm", "ppm", "ppm", "mm/mm" };
                  SetLayeredOrder(SampleNode, Variables, Units);
                  }
               // Order the nodes under <Phosphorus>
               XmlNode PNode = XmlHelper.Find(Node, "Phosphorus");
               if (PNode != null)
                  {
                  string[] Properties = { "RootCP", "RateDissolRock", "RateLossAvail", "SorptionCoeff" };
                  SetPropertiesOrder(PNode, Properties, null, false);

                  string[] Variables = { "Thickness", "LabileP", "BandedP", "RockP", "Sorption" };
                  string[] Units = { "mm", "mg/kg", "kg/ha", "kg/ha", "" };
                  SetLayeredOrder(PNode, Variables, Units);
                  }
               }
            }
         }

      /// <summary>
      /// Fix the order of the properties of the specified parent xml node to that 
      /// giveen in ChildNodeNames
      /// </summary>
      private static void SetPropertiesOrder(XmlNode ParentNode, string[] ChildNodeNames, string[] Units, bool CheckUnits)
         {
         for (int i = 0; i < ChildNodeNames.Length; i++)
            {
            XmlNode Child = XmlHelper.Find(ParentNode, ChildNodeNames[i]);
            if (Child == null)
               {
               Child = ParentNode.OwnerDocument.CreateElement(ChildNodeNames[i]);
               if (Units != null && Units[i] != "")
                  XmlHelper.SetAttribute(Child, "units", Units[i]);
               }
            Child = ParentNode.InsertBefore(Child, ParentNode.ChildNodes[i]);
            if (CheckUnits && XmlHelper.Attribute(Child, "units") == "")
               XmlHelper.SetAttribute(Child, "units", Units[i]);
            }
         }
      /// <summary>
      /// Fix the order of the layered variables of the specified parent xml node to that 
      /// giveen in ChildNodeNames
      /// </summary>
      private static void SetLayeredOrder(XmlNode ProfileNode, string[] ChildNodeNames, string[] Units)
         {
         bool First = true;
         foreach (XmlNode LayerNode in XmlHelper.ChildNodes(ProfileNode, "Layer"))
            {
            if (XmlHelper.Find(LayerNode, "Depth") != null)
               RemoveDepthNodes(XmlHelper.Find(LayerNode, "Depth"));
            SetPropertiesOrder(LayerNode, ChildNodeNames, Units, First);
            if (First)
               First = false;
            }
         }

      /// <summary>
      /// Change the depth nodes: <depth>0-10</depth>
      /// to thickness nodes : <thickness>100</thickness>
      /// </summary>
      private static void RemoveDepthNodes(XmlNode DepthNode)
         {
         string[] DepthStringBits = DepthNode.InnerText.Split("-".ToCharArray(), StringSplitOptions.RemoveEmptyEntries);

         int Thickness = 100;
         if (DepthStringBits.Length == 2)
            {
            int Depth1;
            int Depth2;
            if (Int32.TryParse(DepthStringBits[0], out Depth1))
               if (Int32.TryParse(DepthStringBits[1], out Depth2))
                  {
                  Thickness = Depth2 - Depth1;
                  string Units = XmlHelper.Attribute(DepthNode, "units");
                  if (Units == "cm")
                     Thickness *= 10;
                  }
            }
         XmlNode ThicknessNode = XmlHelper.ChangeType(DepthNode, "Thickness");
         ThicknessNode.InnerText = Thickness.ToString();
         XmlHelper.SetAttribute(ThicknessNode, "units", "mm");
         }

      private static void ToVersion26(XmlNode Node)
         {
         // ----------------------------------------------------------------
         // 1. Remove an unwanted "croppropertyname" attribute from <cultivar>
         // 2. change the name attribute of <category> to description.
         // ---------------------------------------------------------------

         if (Node.Name.ToLower() == "ui")
            {
            foreach (XmlNode Category in XmlHelper.ChildNodes(Node, "category"))
               {
               XmlHelper.SetAttribute(Category, "type", "category");
               if (XmlHelper.Attribute(Category, "name") != "")
                  XmlHelper.SetAttribute(Category, "description", XmlHelper.Attribute(Category, "name"));
               XmlHelper.DeleteAttribute(Category, "name");
               }

            foreach (XmlNode Cultivar in XmlHelper.ChildNodes(Node, "cultivar"))
               XmlHelper.DeleteAttribute(Cultivar, "croppropertyname");
            }
         }

      private static void ToVersion27(XmlNode Node)
         {
         // ----------------------------------------------------------------
         // 1. Change Boron units from mg/kg to Hot water mg/kg
         // 2. Remove CnCanopyFact from SoilWat
         // ---------------------------------------------------------------

         if (Node.Name.ToLower() == "soil")
            {
            XmlNode BoronNode = XmlHelper.Find(Node, "Analysis/Layer/Boron");
            if (BoronNode != null && XmlHelper.Attribute(BoronNode, "units") == "mg/kg")
               XmlHelper.SetAttribute(BoronNode, "units", "Hot water mg/kg");

            XmlNode CnCanopyFactNode = XmlHelper.Find(Node, "SoilWat/CnCanopyFact");
            if (CnCanopyFactNode != null)
               CnCanopyFactNode.ParentNode.RemoveChild(CnCanopyFactNode);            
            }
         }
      private static void ToVersion28(XmlNode Node)
         {
         // Ensure that each report component has an filename tag that
         // matches the .apsim file's paddock/file structure. From code in 
         // ComponentUtility.CalcFileName()
         if (Node.Name.ToLower() == "outputfile")
             {
             string simulationName = null;
             string paddockName = null;
             XmlNode d = Node;
             while (d.ParentNode != null)
                {
                 d = d.ParentNode;
                 if (d.Name.ToLower() == "area")
                     paddockName = XmlHelper.Attribute(d, "name");
                 else if (d.Name.ToLower() == "simulation")
                     simulationName = XmlHelper.Attribute(d, "name");
                }
             string fqname = XmlHelper.FullPath(Node);
             string fileName = simulationName;
             if ((paddockName != null) && paddockName.ToLower() != "paddock")
                 fileName = fileName + " " + paddockName;

             if (XmlHelper.Attribute(Node, "name") != "" && 
                 XmlHelper.Attribute(Node, "name").ToLower() != "outputfile")
                 fileName = fileName + " " + XmlHelper.Attribute(Node, "name");

             XmlHelper.SetValue(Node, "filename", fileName + ".out");
             XmlNode fileNameNode = XmlHelper.Find(Node, "filename");
             XmlHelper.SetAttribute(fileNameNode, "output", "yes");

             // move title from /outputfile/variables/constants to /outputfile
             MoveSoilNode(Node, "variables/constants/title", Node);
             if ((XmlHelper.Find(Node, "title")) == null)
                 XmlHelper.SetValue(Node, "title", fileName);
             }
         else  if (Node.Name.ToLower() == "metfile")
             {
             XmlNode fileNameNode;
             if ((fileNameNode = XmlHelper.Find(Node, "filename")) != null)
                 XmlHelper.SetAttribute(fileNameNode, "input", "yes");
             else
                 {
                 // Must be a shortcut. Create a linked filename object
                 string ShortCut = XmlHelper.Attribute(Node, "shortcut");
                 if (ShortCut != null)
                     {
                     fileNameNode = XmlHelper.EnsureNodeExists(Node, "filename");
                     XmlHelper.SetAttribute(fileNameNode, "shortcut", ShortCut + "/filename");
                     XmlHelper.SetAttribute(fileNameNode, "input", "yes");
                     }
                 }
             }
         else if (Node.Name.ToLower() == "summaryfile")
             {
             XmlNode fileNameNode;
             if ((fileNameNode = XmlHelper.Find(Node, "filename")) != null)
                 Node.RemoveChild(fileNameNode);
             }
         }
      }
   }