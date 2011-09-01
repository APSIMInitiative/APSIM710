using System;
using System.Collections.Generic;
using System.Text;
using System.Xml;
using System.Windows.Forms;
using CSGeneral;
using System.IO;
using ApsimFile;
using System.Diagnostics;

namespace ConToApsim
   {
   public class Converter
      {

      /// <summary>
      /// Go convert the file.
      /// </summary>
      public static XmlNode Go(string ConFileName)
         {
         Form1 F = new Form1();
         F.Show();
         try
            {
            F.SetText("Converting " + ConFileName);
            Application.DoEvents();

            XmlDocument Doc = new XmlDocument();
            XmlNode Root = Doc.AppendChild(Doc.CreateElement("simulations"));
            XmlHelper.SetName(Root, "FromConFile");
            XmlHelper.SetAttribute(Root, "version", "15");

            if (Path.GetExtension(ConFileName) == ".sim")
               ConvertSimToApsim(ConFileName, Root);
            else
               {
               RemoveOldSims(ConFileName);
               ConvertConToSims(ConFileName);


               foreach (string SimFileName in Directory.GetFiles(Path.GetDirectoryName(ConFileName), "*.sim"))
                  ConvertSimToApsim(SimFileName, Root);
               }
            F.Close();
            return Doc.DocumentElement;
            }
         catch (Exception err)
            {
            F.SetText("Error: " + err.Message);
            }
         return null;
         }

      /// <summary>
      /// Remove all existing .sim files from directory.
      /// </summary>
      private static void RemoveOldSims(string ConFileName)
         {
         string[] SimFileNames = Directory.GetFiles(Path.GetDirectoryName(ConFileName), "*.sim");
         if (SimFileNames.Length > 0)
            {
            if (MessageBox.Show("This program needs to delete all old .sim files from " + Path.GetDirectoryName(ConFileName) +
                                ". Delete files?", "Question", MessageBoxButtons.YesNo, MessageBoxIcon.Question) == DialogResult.No)
               throw new Exception("Cannot continue until .sim files are removed.");

            foreach (string SimFileName in SimFileNames)
               File.Delete(SimFileName);
            }
         }

      /// <summary>
      /// Call ConToSim to convert the .con file to a .sim file.
      /// </summary>
      private static void ConvertConToSims(string ConFileName)
         {
         string Exe = Configuration.RemoveMacros(Path.Combine("%apsim%", "Model", "ConToSim.exe"));
         Process ConToSim = Utility.RunProcess(Exe, Path.GetFileName(ConFileName), Path.GetDirectoryName(ConFileName));
         Utility.CheckProcessExitedProperly(ConToSim);
         }

      /// <summary>
      /// Convert the specified .sim file and append the xml to the specified root node.
      /// </summary>
      private static void ConvertSimToApsim(string SimFileName, XmlNode Root)
         {
         XmlDocument SimDoc = new XmlDocument();
         SimDoc.Load(SimFileName);
         XmlNode OldSimNode = SimDoc.DocumentElement;

         // create a new sim node and name it.
         XmlNode NewSimNode = CreateChildNode(Root, "simulation", XmlHelper.Name(OldSimNode));

         // create a new paddock node and name it.
         XmlNode NewPaddockNode = XmlHelper.CreateNode(NewSimNode.OwnerDocument, "area", "paddock");

         foreach (XmlNode Child in XmlHelper.ChildNodes(OldSimNode, ""))
            {
            string ModuleName = Path.GetFileNameWithoutExtension(XmlHelper.Attribute(Child, "executable")).ToLower();

            if (Child.Name.ToLower() == "title")
               XmlHelper.SetName(NewSimNode, Child.InnerText);

            if (ModuleName == "clock")
               {
               XmlNode NewClockNode = CreateChildNode(NewSimNode, ModuleName, "");
			   if (XmlHelper.Value(Child, "InitData/start_date") != "") 
                  TransferParameter(Child, "InitData/start_date", NewClockNode, "start_date");
			   if (XmlHelper.Value(Child, "InitData/end_date") != "") 
                  TransferParameter(Child, "InitData/end_date", NewClockNode, "end_date");

               // add in a summary file.
               CreateChildNode(NewSimNode, "summaryfile", "");
               }
            else if (ModuleName == "canopy")
               {
               XmlNode NewCanopyNode = CreateChildNode(NewPaddockNode, ModuleName, XmlHelper.Name(Child));
               TransferParameter(Child, "InitData/intercrop", NewCanopyNode, "intercrop");
			   }
            else if (ModuleName == "report")
               {
               XmlNode NewReportNode = CreateChildNode(NewPaddockNode, "outputfile", XmlHelper.Name(Child));
               XmlNode NewVariablesNode = CreateChildNode(NewReportNode, "variables", "");
               TransferReportVariables(Child, NewVariablesNode);

               string OutputFrequency = XmlHelper.Value(Child, "InitData/outputfrequency");
               XmlNode EventsNode = CreateChildNode(NewReportNode, "events", "");
               if (OutputFrequency != "")
                  TransferValueIntoNameAttribute(EventsNode, "event", OutputFrequency);
               }
            else if (ModuleName == "met" || XmlHelper.Name(Child).ToLower() == "met")
               {
               XmlNode NewMetNode = CreateChildNode(NewSimNode, "metfile", "met");
               TransferParameter(Child, "InitData/filename", NewMetNode, "filename");
               }
            else if (ModuleName == "operations")
               {
               XmlNode NewNode = CreateChildNode(NewPaddockNode, "operations", XmlHelper.Name(Child));
               TransferOperations(Child, NewNode);
               }
            else if (ModuleName == "micromet")
               {
               XmlNode NewNode = CreateChildNode(NewPaddockNode, ModuleName, XmlHelper.Name(Child));
               string Xml = "<soilalbedo name=\"soilalbedo\">0.23</soilalbedo>" +
                            "<a_interception name=\"a_interception\">0.0</a_interception>" +
                            "<b_interception name=\"b_interception\">0.0</b_interception>" +
                            "<c_interception name=\"c_interception\">0.0</c_interception>" +
                            "<d_interception name=\"d_interception\">0.0</d_interception>";
               NewNode.InnerXml = Xml;
               TransferParameter(Child, "InitData/soil_albedo", NewNode, "soilalbedo");
               }
            else if (ModuleName == "fertiliser")
               {
               CreateChildNode(NewPaddockNode, ModuleName, "");
               }
            else if (ModuleName == "irrigation")
               {
               XmlNode NewNode = CreateChildNode(NewPaddockNode, ModuleName, "");
               string Xml = "<automatic_irrigation type=\"list\" listvalues=\"on,off\" description=\"Automatic irrigation\">off</automatic_irrigation>" +
                            "<asw_depth type=\"text\" description=\"Depth to which ASW is calculated. (mm)\">600</asw_depth>" +
                            "<crit_fr_asw type=\"text\" description=\"Fraction of ASW below which irrigation is applied (0-1.0)\">0.5</crit_fr_asw>" +
                            "<irrigation_efficiency type=\"text\" description=\"Efficiency of the irrigation. (0-1.0)\">1</irrigation_efficiency>" +
                            "<irrigation_allocation type=\"list\" listvalues=\"on,off\" description=\"Allocation limits\">off</irrigation_allocation>" +
                            "<allocation type=\"text\" description=\"Allocation in mm\">0</allocation>" +
                            "<default_no3_conc type=\"text\" description=\"Nitrate concentration (ppm N)\">0.0</default_no3_conc>" +
                            "<default_nh4_conc type=\"text\" description=\"Ammonium concentration (ppm N)\">0.0</default_nh4_conc>" +
                            "<default_cl_conc type=\"text\" description=\"Chloride concentration (ppm Cl)\">0.0</default_cl_conc>";
               NewNode.InnerXml = Xml;
               if (XmlHelper.Value(Child, "initdata/automatic_irrigation") != "")
                  TransferParameter(Child, "InitData/automatic_irrigation", NewNode, "automatic_irrigation");

               if (XmlHelper.Value(Child, "initdata/crit_fr_asw") != "")
                  TransferParameter(Child, "InitData/crit_fr_asw", NewNode, "crit_fr_asw");

               if (XmlHelper.Value(Child, "initdata/asw_depth") != "")
                  TransferParameter(Child, "InitData/asw_depth", NewNode, "asw_depth");

               if (XmlHelper.Value(Child, "initdata/irrigation_efficiency") != "")
                  TransferParameter(Child, "InitData/irrigation_efficiency", NewNode, "irrigation_efficiency");

               if (XmlHelper.Value(Child, "initdata/irrigation_allocation") != "")
                  TransferParameter(Child, "InitData/irrigation_allocation", NewNode, "irrigation_allocation");
               }
            else if (ModuleName == "manager")
               {
               XmlNode NewNode = CreateChildNode(NewPaddockNode, "manager", XmlHelper.Name(Child));
               TransferManagerRules(Child, NewNode);
               }
            else if (ModuleName == "surfaceom")
               {
               XmlNode NewNode = CreateChildNode(NewPaddockNode, "surfaceom", XmlHelper.Name(Child));

               string Xml = "<PoolName description=\"Organic Matter pool name\">unknown</PoolName>" +
                            "<type description=\"Organic Matter type\">wheat</type>" +
                            "<mass description=\"Initial surface residue (kg/ha)\">500</mass>" +
                            "<cnr description=\"C:N ratio of initial residue\">100</cnr>" +
                            "<standing_fraction description=\"Fraction of residue standing\">0.0</standing_fraction>";
               NewNode.InnerXml = Xml;
               TransferParameter(Child, "InitData/type", NewNode, "PoolName");
               TransferParameter(Child, "InitData/type", NewNode, "type");
               TransferParameter(Child, "InitData/mass", NewNode, "mass");
               TransferParameter(Child, "InitData/cnr", NewNode, "cnr");
               TransferParameter(Child, "InitData/type", NewNode, "type");
               }
            else if (ModuleName == "solute")
               {
               XmlNode NewNode = CreateChildNode(NewPaddockNode, "cl", XmlHelper.Name(Child));

               string Xml = "<d0 description=\"Cl diffusivity in water (mm2/d)\">108</d0>" +
                            "<layers description=\"Cl in each layer (kg/ha)\">491 62 1490 1000 1000 1000 1000 1000 1000 1000 1000 1000 1000</layers>";
               NewNode.InnerXml = Xml;
               TransferParameter(Child, "InitData/d0_cl", NewNode, "d0");
               TransferParameter(Child, "InitData/cl", NewNode, "layers");
               }
            else if (ModuleName == "plant" || ModuleName == "growth" || ModuleName == "sorghum")
               {
               CreateChildNode(NewPaddockNode, XmlHelper.Name(Child).ToLower(), "");
               XmlNode ProfileNode = XmlHelper.Find(NewPaddockNode, "soil/profile");
               if (ProfileNode != null)
                  {
                  TransferCropSoilParameter(Child, "InitData/ll", ProfileNode, "ll", XmlHelper.Name(Child));
                  TransferCropSoilParameter(Child, "InitData/kl", ProfileNode, "kl", XmlHelper.Name(Child));
                  TransferCropSoilParameter(Child, "InitData/xf", ProfileNode, "xf", XmlHelper.Name(Child));

                  }
               }

            else if (ModuleName == "soilwat2" || ModuleName == "soilwat")
               {
               XmlNode NewNode = CreateChildNode(NewPaddockNode, "soil", "");
               TransferParameter(Child, "InitData/u", NewNode, "U");
               TransferParameter(Child, "InitData/cn2_bare", NewNode, "Cn2Bare");
               TransferParameter(Child, "InitData/salb", NewNode, "Salb");
               TransferParameter(Child, "InitData/cn_red", NewNode, "CnRed");
               TransferParameter(Child, "InitData/cn_cov", NewNode, "CnCov");
               TransferParameter(Child, "InitData/cn_canopy_fact", NewNode, "CnCanopyFact");
               TransferParameter(Child, "InitData/cona", NewNode, "Cona");
               TransferParameter(Child, "InitData/diffus_const", NewNode, "DiffusConst");
               TransferParameter(Child, "InitData/diffus_slope", NewNode, "DiffusSlope");

               XmlNode ProfileNode = CreateChildNode(NewNode, "profile", "");
               TransferProfileParameter(Child, "InitData/dlayer", ProfileNode, "thickness");
               TransferProfileParameter(Child, "InitData/ll15", ProfileNode, "ll15");
               TransferProfileParameter(Child, "InitData/dul", ProfileNode, "dul");
               TransferProfileParameter(Child, "InitData/sat", ProfileNode, "sat");
               TransferProfileParameter(Child, "InitData/bd", ProfileNode, "bd");
               TransferProfileParameter(Child, "InitData/swcon", ProfileNode, "swcon");
               TransferProfileParameter(Child, "InitData/mwcon", ProfileNode, "mwcon");
               TransferProfileParameter(Child, "InitData/air_dry", ProfileNode, "airdry");
               string[] Thickness = GetLayered(ProfileNode, "thickness");

               XmlNode InitWater = CreateChildNode(NewNode, "InitWater", "");
               string InSoil = RemoveComment(XmlHelper.Value(Child, "InitData/insoil"));
               if (InSoil != "" && Convert.ToDouble(InSoil) <= 1)
                  {
                  XmlHelper.SetValue(InitWater, "percentmethod/percent", InSoil);
                  XmlHelper.SetValue(InitWater, "percentmethod/distributed", "Evenly distributed");
                  }
               else
                  {
                  ProfileNode = CreateChildNode(InitWater, "profile", "");
                  TransferProfileParameter(Child, "InitData/sw", ProfileNode, "sw");
                  SetLayered(ProfileNode, "thickness", Thickness);
                  }
               }
            else if (ModuleName == "soiln2" || ModuleName == "soiln")
               {
               XmlNode SoilNode = XmlHelper.Find(NewPaddockNode, "soil");
               if (SoilNode != null)
                  {
                  TransferParameter(Child, "InitData/soil_cn", SoilNode, "SoilCn");
                  TransferParameter(Child, "InitData/enr_a_coeff", SoilNode, "EnrACoeff");
                  TransferParameter(Child, "InitData/enr_b_coeff", SoilNode, "EnrBCoeff");
                  TransferParameter(Child, "InitData/root_wt", SoilNode, "RootWt");
                  TransferParameter(Child, "InitData/root_cn", SoilNode, "RootCn");

                  string Value = XmlHelper.Value(Child, "InitData/soiltype");
                  if (Value != "")
                     XmlHelper.SetValue(SoilNode, "soiltype", Value);

                  XmlNode ProfileNode = XmlHelper.Find(SoilNode, "profile");
                  string[] Thickness = GetLayered(ProfileNode, "thickness");
                  TransferProfileParameter(Child, "InitData/oc", ProfileNode, "oc");
                  TransferProfileParameter(Child, "InitData/ph", ProfileNode, "ph");
                  TransferProfileParameter(Child, "InitData/fbiom", ProfileNode, "fbiom");
                  TransferProfileParameter(Child, "InitData/finert", ProfileNode, "finert");

                  XmlNode InitNitrogen = CreateChildNode(SoilNode, "InitNitrogen", "");
                  ProfileNode = CreateChildNode(InitNitrogen, "profile", "");
                  TransferProfileParameter(Child, "InitData/no3ppm", ProfileNode, "no3");
                  TransferProfileParameter(Child, "InitData/nh4ppm", ProfileNode, "nh4");
                  SetLayered(ProfileNode, "thickness", Thickness);

                  MoveTavAmpToMetFile(Child, NewSimNode);
                  }
               }
            else if (ModuleName == "tracker")
               {
               XmlNode NewNode = CreateChildNode(NewPaddockNode, ModuleName, XmlHelper.Name(Child));
               TransferReportVariables(Child, NewNode);
               }

            }
         // Now append the paddock. This way clock and report and met will be above the paddock.
         NewSimNode.AppendChild(NewPaddockNode);
         }


      /// <summary>
      /// In some con/par files need to move tav/amp to the met file.
      /// </summary>
      private static void MoveTavAmpToMetFile(XmlNode Child, XmlNode SimNode)
         {
         string Tav = XmlHelper.Value(Child, "initdata/tav");
         string Amp = XmlHelper.Value(Child, "initdata/amp");
         string MetFileName = XmlHelper.Value(SimNode, "met/filename");
         MetFileName = Configuration.RemoveMacros(MetFileName);
         if (Tav != "" && Amp != "" && MetFileName != "")
            {
            string TempFileName = Path.GetTempFileName();

            StreamReader In = new StreamReader(MetFileName);

            // See if the met file already has tav in it. If so, then don't put another in it.
            string Contents = In.ReadToEnd();
            In.Close();
            if (Contents.IndexOf("tav") == -1)
               {
               In = new StreamReader(MetFileName);
               StreamWriter Out = new StreamWriter(TempFileName);
               while (!In.EndOfStream)
                  {
                  string Line = In.ReadLine();
                  Out.WriteLine(Line);
                  if (Line.IndexOf('[') != -1 && Line.IndexOf(']') != -1)
                     {
                     Out.WriteLine("   tav = " + Tav);
                     Out.WriteLine("   amp = " + Amp);
                     }
                  }
               In.Close();
               Out.Close();
               File.Delete(MetFileName);
               File.Move(TempFileName, MetFileName);
               }
            }
         }

      /// <summary>
      /// Transfer crop parameters e.g. ll, kl, xf from oldnode to new node.
      /// </summary>
      private static void TransferCropSoilParameter(XmlNode OldNode, string OldPath, XmlNode ProfileNode, string NewType, string NewName)
         {
         string Value = XmlHelper.Value(OldNode, OldPath);
         string[] Values = Value.Split(" ".ToCharArray(), StringSplitOptions.RemoveEmptyEntries);
         List<XmlNode> Layers = XmlHelper.ChildNodes(ProfileNode, "layer");
         for (int i = 0; i != Values.Length; i++)
            {
            if (i < Layers.Count)
               {
               XmlNode ValueNode = Layers[i].AppendChild(XmlHelper.CreateNode(ProfileNode.OwnerDocument, NewType, NewName));
               ValueNode.InnerText = Values[i];
               }
            }
         }

      /// <summary>
      /// Transfer a soil profile parameter to new profile node.
      /// </summary>
      private static void TransferProfileParameter(XmlNode OldNode, string OldPath,
                                            XmlNode ProfileNode, string NewName)
         {
         string Value = XmlHelper.Value(OldNode, OldPath);
         Value = RemoveComment(Value);

         string[] Values = Value.Split(" ".ToCharArray(), StringSplitOptions.RemoveEmptyEntries);
         if (Values.Length > 0)
            SetLayered(ProfileNode, NewName, Values);
         }

      /// <summary>
      /// Remove comments and units from the specified string.
      /// </summary>
      private static string RemoveComment(string Value)
         {
         int PosComment = Value.IndexOf('!');
         if (PosComment != -1)
            Value = Value.Remove(PosComment);

         StringManip.SplitOffBracketedValue(ref Value, '(', ')');
         return Value;
         }


      /// <summary>
      ///  Sets the values of the specified node as strings.
      /// </summary>
      public static void SetLayered(XmlNode Profile, string PropertyName, string[] Values)
         {
         // make sure we have the right amount of layer nodes.
         XmlHelper.EnsureNumberOfChildren(Profile, "layer", "", Values.Length);

         List<XmlNode> Layers = XmlHelper.ChildNodes(Profile, "layer");
         for (int i = 0; i != Values.Length; i++)
            XmlHelper.SetValue(Layers[i], PropertyName, Values[i]);
         }


      /// <summary>
      ///  Gets the values of the specified node as strings.
      /// </summary>
      public static string[] GetLayered(XmlNode Profile, string PropertyName)
         {
         List<string> Values = new List<string>();
         List<XmlNode> Layers = XmlHelper.ChildNodes(Profile, "layer");
         for (int i = 0; i != Layers.Count; i++)
            Values.Add(XmlHelper.Value(Layers[i], PropertyName));
         string[] Vals = new string[Values.Count];
         Values.CopyTo(Vals);
         return Vals;
         }

      /// <summary>
      /// Transfer all manager rules from the old manager node specified to the new node.
      /// </summary>
      private static void TransferManagerRules(XmlNode OldNode, XmlNode NewNode)
         {
         XmlNode InitData = XmlHelper.Find(OldNode, "initdata");
         if (InitData != null)
            {
            foreach (XmlNode Script in XmlHelper.ChildNodes(InitData, "script"))
               {
               XmlNode NewScriptNode = CreateChildNode(NewNode, "script", "");
               XmlHelper.SetValue(NewScriptNode, "event", XmlHelper.Value(Script, "event"));

               string Text = XmlHelper.Value(Script, "text");
               Text = RemoveModulePrefixes(Text);
               XmlHelper.SetValue(NewScriptNode, "text", Text);
               }
            }
         }

      /// <summary>
      /// Transfer the operations parameters from the specified old component node to the
      /// new specified node.
      /// </summary>
      private static void TransferOperations(XmlNode OldNode, XmlNode NewNode)
         {
         XmlNode InitData = XmlHelper.Find(OldNode, "initdata");
         if (InitData != null)
            {
            foreach (XmlNode Script in XmlHelper.ChildNodes(InitData, "script"))
               {
               string EventName = XmlHelper.Value(Script, "event");
               if (EventName.ToLower() == "parameters")
                  EventName = "start_of_day";

               string[] Lines = XmlHelper.Value(Script, "text").Split("\r\n".ToCharArray(), StringSplitOptions.RemoveEmptyEntries);

               foreach (string L in Lines)
                  {
                  string Line = L;

                  // get rid of everything after comment.
                  Line = RemoveComment(Line);

                  if (Line.Trim() != "")
                     {
                     XmlNode NewOperation = CreateChildNode(NewNode, "operation", "");
                     XmlHelper.SetAttribute(NewOperation, "condition", EventName);

                     string[] LineBits = Line.Split(" ".ToCharArray(), StringSplitOptions.RemoveEmptyEntries);
                     if (LineBits.Length > 2)
                        {
                        // Extract date
                        int DayNumber = Convert.ToInt32(LineBits[0]);
                        int Year = Convert.ToInt32(LineBits[1]);
                        if (DayNumber > 1000)
                           {
                           int Temp = Year;
                           Year = DayNumber;
                           DayNumber = Temp;
                           }
                        DateTime OperationDate = new DateTime(Year, 1, 1);
                        OperationDate = OperationDate.AddDays(DayNumber - 1);

                        // Rebuild the rest of the line.
                        string Action = "";
                        for (int i = 2; i < LineBits.Length; i++)
                           {
                           if (Action != "")
                              Action += " ";
                           Action += LineBits[i];
                           }

                        // put date and action into new condition.
                        XmlHelper.SetValue(NewOperation, "date", OperationDate.ToString("dd/MM/yyyy"));
                        Action = RemoveModulePrefixes(Action);
                        XmlHelper.SetValue(NewOperation, "action", Action);
                        }
                     }
                  }
               }
            }
         }

      /// <summary>
      /// Remove unwanted module prefixes.
      /// </summary>
      private static string RemoveModulePrefixes(string St)
         {
         St = St.Replace("clock.", "");
         St = St.Replace("SoilWat.", "");
         St = St.Replace("soilwat.", "");
         St = St.Replace("SoilN.", "");
         St = St.Replace("soiln.", "");
         St = St.Replace("met.", "");
         St = St.Replace("Met.", "");
         St = St.ToLower().Replace("soilwat ", "'soil water' ");
         St = St.ToLower().Replace("soiln ", "'soil nitrogen' ");
         return St;
         }

      /// <summary>
      /// Transfer report variables from the specified child node to the 
      /// </summary>
      private static void TransferReportVariables(XmlNode Report, XmlNode NewVariablesNode)
         {
         XmlNode InitData = XmlHelper.Find(Report, "initdata");
         if (InitData != null)
            {
            List<string> Values = XmlHelper.Values(InitData, "variable");
            foreach (string Variable in Values)
               {
               string Var = RemoveModulePrefixes(Variable);
               TransferValueIntoNameAttribute(NewVariablesNode, "variable", Var);
               }
            }
         }

      private static void TransferValueIntoNameAttribute(XmlNode ParentNode, string ChildNodeType, string NameValue)
         {
         XmlNode Node = ParentNode.AppendChild(ParentNode.OwnerDocument.CreateElement(ChildNodeType));
         XmlHelper.SetName(Node, NameValue);

         }

      /// <summary>
      /// Create a new child node under the specified parent node that has the 
      /// specified type and name.
      /// </summary>
      private static XmlNode CreateChildNode(XmlNode Parent, string Type, string Name)
         {
         return Parent.AppendChild(XmlHelper.CreateNode(Parent.OwnerDocument, Type, Name));
         }

      /// <summary>
      /// Transfer a single parameter value (as specified by oldnode and oldpath) to a new 
      /// node and path.
      /// </summary>
      private static void TransferParameter(XmlNode OldNode, string OldPath,
                                     XmlNode NewNode, string NewPath)
         {
         string Value = XmlHelper.Value(OldNode, OldPath);
         StringManip.SplitOffBracketedValue(ref Value, '(', ')');
         XmlHelper.SetValue(NewNode, NewPath, Value);
         }

      /// <summary>
      /// Transfer a range of parameters from OldNode:OldPath to a NewNode:NewPath.
      /// </summary>
      private static void TransferParameters(XmlNode OldNode, string OldPath, string OldVariable,
                                      XmlNode NewNode, string NewPath, string NewVariable)
         {
         OldNode = XmlHelper.Find(OldNode, OldPath);

         List<string> Values = XmlHelper.Values(OldNode, OldVariable);
         XmlHelper.SetValues(NewNode, NewVariable, Values);
         }








      }
   }
