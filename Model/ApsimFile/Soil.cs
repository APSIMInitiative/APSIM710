using System;
using System.Collections.Generic;
using System.Collections.Specialized;
using System.IO;
using System.Xml;

using CSGeneral;
using System.Data;


namespace ApsimFile
   {
   /// <summary> 
   /// The Soil class encapsulates all reading and writing of soil information.
   /// To create a soil call one of the static Create methods.
   /// </summary>
   public class Soil
      {
      private XmlNode Data;
      private double[] UserSetTargetThickness;
      private const double ppm = 1000000.0;

      private Soil(XmlNode data)
         {
         Data = data;
         }




      /// <summary>
      /// Name property.
      /// </summary>
      public string Name
         {
         get { return XmlHelper.Name(Data); }
         set { XmlHelper.SetName(Data, value); }
         }

      /// <summary>
      /// Create an empty soil object with the specified name.
      /// </summary>
      public static Soil Create(string Name)
         {
         System.Reflection.Assembly thisExe = System.Reflection.Assembly.GetExecutingAssembly();
         System.IO.Stream file = thisExe.GetManifestResourceStream("ApsimFile.Resources.Blank.soil");
         StreamReader In = new StreamReader(file);
         string XML = In.ReadToEnd();
         In.Close();
         Soil NewSoil = Soil.CreateFromXML(XML);
         NewSoil.Name = Name;
         return NewSoil;
         }


      /// <summary>
      /// Create a soil from XML
      /// </summary>
      public static Soil CreateFromXML(string XML)

         {
         XmlDocument Doc = new XmlDocument();
         Doc.LoadXml(XML);
         return new Soil(Doc.DocumentElement);
         }

      /// <summary>
      /// Create a soil from XML
      /// </summary>
      public static Soil CreateFromXML(XmlNode Node)
         {
         return new Soil(Node);
         }

      public void UseNode(XmlNode Node)
         {
         Data = Node;
         }

      /// <summary>
      /// Return the XML for the soil
      /// </summary>
      public string XML
         {
         get
            {
            return Data.OuterXml;
            }
         }

      /// <summary>
      /// Return the XML for the soil
      /// </summary>
      public string InnerXml
         {
         get
            {
            return Data.InnerXml;
            }
         }

      /// <summary>
      /// Loop through all soil macros (e.g. [soil.ll15]) and replace them 
      /// with a value. Will throw if macro is invalid.
      /// </summary>
      public string ReplaceSoilMacros(string Str)
         {
         string ErrorMessages = "";

         // Replace the [Soil.] macros will values.         
         string ReturnString = Str;

         // Loop through all soil macros.
         int PosMacro = ReturnString.IndexOf("[soil.");
         while (PosMacro != -1)
            {
            int PosEndMacro = ReturnString.IndexOf(']', PosMacro);
            if (PosEndMacro == -1)
               throw new Exception("Invalid soil macro found: " + ReturnString.Substring(PosMacro));

            // Get macro name e.g. soil.thickness
            string MacroName = ReturnString.Substring(PosMacro + 1, PosEndMacro - PosMacro - 1);

            // Split macro name into bits.
            string[] MacroBits = MacroName.Split(".".ToCharArray());

            try
               {
               string MacroValue = "";
               if (MacroBits.Length == 2 && MacroBits[0].ToLower() == "soil")
                  {
                  // See if the macro is a variable (i.e. an array of numbers)
                  if (MacroBits[1] == "Name")
                     MacroValue = Name;

                  else if (SoilMetaData.Instance.IsProperty(MacroBits[1]))
                     MacroValue = Property(MacroBits[1]);

                  else
                     {
                     double[] DoubleValues = Variable(MacroBits[1]);
                     if (MathUtility.ValuesInArray(DoubleValues))
                        {
                        foreach (double Value in DoubleValues)
                           {
                           string StringValue;
                           if (Value == MathUtility.MissingValue)
                              StringValue = "0.000";
                           else
                              StringValue = Value.ToString("f3");
                           MacroValue += new string(' ', 10 - StringValue.Length) + StringValue;
                           }
                        }
                     }
                  ReturnString = ReturnString.Remove(PosMacro, PosEndMacro - PosMacro + 1);
                  ReturnString = ReturnString.Insert(PosMacro, MacroValue);
                  }
               }
            catch (Exception err)
               {
               ReturnString = ReturnString.Remove(PosMacro, PosEndMacro - PosMacro + 1);
               ErrorMessages += err.Message + "\r\n";
               }

            // Find next macro.
            PosMacro = ReturnString.IndexOf("[soil.", PosMacro);
            }
         if (ErrorMessages != "")
            throw new Exception(ErrorMessages);
         return ReturnString;
         }


      // ------------------------- Property methods ------------------------------

      /// <summary>
      /// Return a soil property e.g. site / region / soiltype
      /// </summary>
      public string Property(string PropertyName)
         {
         List<string> PropertyPaths = SoilMetaData.Instance.GetPropertyPaths(PropertyName);
         foreach (string PropertyPath in PropertyPaths)
            {
            string Value = XmlHelper.Value(Data, PropertyPath);
            if (Value != "")
               return Value;
            }
         return "";
         }


      /// <summary>
      /// Set the value of the specified property.
      /// </summary>
      public void SetProperty(string PropertyName, string Value)
         {
         string PropertyPath = SoilMetaData.Instance.GetPropertyPath(PropertyName);
         if (PropertyPath != "")
            XmlHelper.SetValue(Data, PropertyPath, Value.Trim());
         }

      



      // ----------------------- Variable methods -------------------------------

      /// <summary>
      /// Return a layered soil variable to caller. The variables will be 
      /// mapped to the target layer structure. e.g. VariableName = LL15 (%)
      /// If a sample variable is specified, the first one found will be
      /// returned and mapped.
      /// </summary>
      public double[] Variable(string VariableName)
         {
         VariableValue Value = FindVariable(VariableName, TargetThickness);
         return Value.Doubles;
         }

      /// <summary>
      /// Return a layered soil variable to caller as strings. The variables will be 
      /// mapped to the target layer structure. e.g. VariableName = LL15Code (%)
      /// </summary>
      public string[] VariableAsStrings(string VariableName)
         {
         VariableValue Value = FindVariable(VariableName, TargetThickness);
         if (VariableName.Contains("Code"))
            return Value.Codes;
         else
            return Value.Strings;
         }
      
      /// <summary>
      /// Return a layered soil variable to caller. The variables will NOT be 
      /// mapped to the target layer structure. e.g. VariableName = LL15 (%)
      /// </summary>
      public double[] VariableUnMapped(string VariableName)
         {
         string RawVariableName = VariableName;
         string ToUnits = StringManip.SplitOffBracketedValue(ref RawVariableName, '(', ')');
         VariableValue Value = FindVariable(RawVariableName);
         ConvertUnits(Value, ToUnits);
         return Value.Doubles;
         }

      /// <summary>
      /// Return a layered variable, from a specific location, to caller. 
      /// The variables will NOT be mapped to the target layer structure. 
      /// e.g. VariableName = NO3 (ppm)
      /// </summary>
      public double[] VariableUnMapped(string VariableName, string LocationName)
         {
         string RawVariableName = VariableName;
         string ToUnits = StringManip.SplitOffBracketedValue(ref RawVariableName, '(', ')');
         VariableValue Value = FindVariable(RawVariableName, LocationName);
         ConvertUnits(Value, ToUnits);
         return Value.Doubles;
         }

      /// <summary>
      /// Set a variable using an array of doubles.
      /// </summary>
      public void SetVariable(string VariableName, double[] Values)
         {
         VariableValue Value = new VariableValue();
         Value.Name = VariableName;
         Value.Units = StringManip.SplitOffBracketedValue(ref Value.Name, '(', ')');
         Value.Doubles = Values;
         SetVariable(Value);
         }

      /// <summary>
      /// Set a variable using an array of strings.
      /// </summary>
      public void SetVariable(string VariableName, string[] Values)
         {
         VariableValue Value = new VariableValue();
         Value.Name = VariableName;
         Value.Units = StringManip.SplitOffBracketedValue(ref Value.Name, '(', ')');
         Value.Strings = Values;
         SetVariable(Value);
         }

      /// <summary>
      /// Set a variable in a specific location.
      /// </summary>
      public void SetVariable(string VariableName, double[] Values, string LocationName)
         {
         VariableValue Value = new VariableValue();
         Value.Name = VariableName;
         Value.Units = StringManip.SplitOffBracketedValue(ref Value.Name, '(', ')');
         Value.Doubles = Values;
         SetVariable(LocationName, Value);
         }
      /// <summary>
      /// Set a variable in a specific location.
      /// </summary>
      public void SetVariable(string VariableName, string[] Values, string LocationName)
         {
         VariableValue Value = new VariableValue();
         Value.Name = VariableName;
         Value.Units = StringManip.SplitOffBracketedValue(ref Value.Name, '(', ')');
         Value.Strings = Values;
         SetVariable(LocationName, Value);
         }
      /// <summary>
      /// Set the layered values of the specified variable. Use default
      /// locations (Profile nodes) as no profile node name has been 
      /// specified. No mapping is performed.
      /// </summary>
      private void SetVariable(VariableValue Value)
         {
         SetVariable(SoilMetaData.Instance.GetVariablePath(Value.Name), Value);
         }

      /// <summary>
      /// Set the layered values of the specified variable to 
      /// a specified location name. No mapping is performed.
      /// </summary>
      private void SetVariable(string LocationName, VariableValue Value)
         {
         if (Value.Name.Contains(" "))
            SetCropVariable(Value);
         else
            {
            if (Value.Name.Contains("Code"))
               {
               string NameWithoutCode = Value.Name.Replace("Code", "");
               LocationName = SoilMetaData.Instance.GetVariablePath(NameWithoutCode);
               if (LocationName != "")
                  {
                  XmlNode LocationNode = FindProfileNode(LocationName);
                  SoilUtility.SetVariableValue(LocationNode, Value);
                  }
               }
            else if (Value.Name.Contains("Method"))
               {
               string NameWithoutCode = Value.Name.Replace("Method", "");

               LocationName = SoilMetaData.Instance.GetVariablePath(NameWithoutCode);
               XmlNode LocationNode = FindProfileNode(LocationName);
               if (LocationNode != null && Value.Strings.Length > 0)
                  {
                  XmlNode FirstNode = XmlHelper.Find(LocationNode, "Layer/" + NameWithoutCode);
                  if (FirstNode != null)
                     XmlHelper.SetAttribute(FirstNode, "units", Value.Strings[0]);
                  }
               }
            else
               {
               if (LocationName == "")
                  throw new Exception("Cannot set the value of soil variable: " + Value.Name);

               XmlNode LocationNode = FindProfileNode(LocationName);

               SoilUtility.SetVariableValue(LocationNode, Value);

               RemoveEmptyLayers(LocationNode);

               // make sure we have thickness in this profile node. If not then add default ones.
               if (!SoilUtility.VariableExists(LocationNode, "Thickness") &&
                   !SoilUtility.VariableExists(LocationNode, "Depth") &&
                  MathUtility.ValuesInArray(TargetThickness))
                  {
                  SoilUtility.SetLayered(LocationNode, "Thickness", TargetThickness);
                  XmlNode ThicknessNode = XmlHelper.Find(LocationNode, "Layer/Thickness");
                  XmlHelper.SetAttribute(ThicknessNode, "units", "mm");
                  }
               }
            }
         }

      /// <summary>
      /// Remove any layer nodes at the bottom that don't have any data in them.
      /// </summary>
      private void RemoveEmptyLayers(XmlNode LocationNode)
         {
         List<XmlNode> Layers = XmlHelper.ChildNodes(LocationNode, "Layer");
         for (int i = Layers.Count - 1; i >= 0; i--)
            {
            // See if we have any data in this layer.
            bool DataFound = false;
            foreach (XmlNode Child in XmlHelper.ChildNodes(Layers[i], ""))
               DataFound = DataFound || Child.InnerText != "";

            // Remove layer if no data found and then continue to next layer.
            if (!DataFound)
               LocationNode.RemoveChild(Layers[i]);
            else
               break;
            }
         }

      /// <summary>
      /// Return a list of valid variables for the specified NodeName (eg. water).
      /// </summary>
      public List<string> ValidVariablesForProfileNode(string NodeType, string NodeName)
         {
         // We need to create a list of variable names for the specified node.
         List<string> VariableNames = SoilMetaData.Instance.ValidVariablesForProfileNode(NodeType);
         for (int i = 0; i < VariableNames.Count; i++)
            {
            string RawVariableName = VariableNames[i];
            string Units = StringManip.SplitOffBracketedValue(ref RawVariableName, '(', ')');

            if (NodeType == "SoilCrop")
               RawVariableName = NodeName + " " + RawVariableName;
            
            VariableValue Value;
            if (NodeType == "Sample")
               Value = FindVariable(RawVariableName, NodeName);
            else
               Value = FindVariable(RawVariableName);

            if (Value != null && Value.Units != "")
               Units = Value.Units;

            // Add the variable name to our list.
            if (Units == "")
               VariableNames[i] = RawVariableName;
            else
               VariableNames[i] = RawVariableName + " (" + Units + ")";
            }
         return VariableNames;
         }

      // ----------------------- Find methods -------------------------------

      /// <summary>
      /// Find a variable, convert it's units and maps to the TargetThickness. Throws on error.
      /// Never returns null.
      /// </summary>
      private VariableValue FindVariable(string VariableName, double[] TargetThickness)
         {
         VariableValue Value = FindVariable(VariableName);
         if (Value == null)
            throw new Exception("Cannot find soil variable: " + VariableName);

         // Mapping.
         SoilLayerMap.Map(Value, TargetThickness, this);

         return Value;
         }

      /// <summary>
      /// Retrieve and return a variable value to caller. Never returns null.
      /// Will throw on error. No mapping. If units are specified then the
      /// unit conversion will happen.
      /// </summary>
      private VariableValue FindVariable(string VariableName)
         {
         string RawVariableName = VariableName;
         string ToUnits = StringManip.SplitOffBracketedValue(ref RawVariableName, '(', ')');

         bool IsCodeVariable = RawVariableName.Contains("Code");
         if (IsCodeVariable)
            RawVariableName = RawVariableName.Replace("Code", "");

         // See if it is a SW variable
         VariableValue Value = SWFromInitWater(RawVariableName);

         // If not then try for a calculated variable.
         if (Value == null)
            Value = CalculatedVariables(RawVariableName);

         // If not SW then try for a crop variable.
         if (Value == null)
            Value = CropVariable(RawVariableName);

         // If not then must be a normal variable.
         if (Value == null)
            Value = FindVariable(VariableName, "");
         else
            {
            if (IsCodeVariable)
               {
               Value.Strings = Value.Codes;
               Value.Doubles = null;
               }

            if (ToUnits != "")
               ConvertUnits(Value, ToUnits);
            }
         return Value;
         }

      /// <summary>
      /// Retrieve and return a variable value to caller from the specified location. 
      /// Never returns null. Will throw on error. No mapping. If units are specified then the
      /// unit conversion will happen.
      /// </summary>
      private VariableValue FindVariable(string VariableName, string LocationName)
         {
         string RawVariableName = VariableName;
         string ToUnits = StringManip.SplitOffBracketedValue(ref RawVariableName, '(', ')');

         bool IsCodeVariable = RawVariableName.Contains("Code");
         if (IsCodeVariable)
            RawVariableName = RawVariableName.Replace("Code", "");

         if (LocationName == "")
            {
            List<string> LocationNames = SoilMetaData.Instance.GetVariablePaths(RawVariableName);
            if (LocationNames.Count == 1 && LocationNames[0] == "Sample")
               LocationName = FindFirstMatchingLocation(RawVariableName);
            else if (LocationNames.Count > 1)
               {
               // Loop through all location names looking for one that exists.
               foreach (string Name in LocationNames)
                  {
                  if (XmlHelper.Find(Data, Name) != null)
                     {
                     LocationName = Name;
                     break;
                     }
                  }
               }
            else if (LocationNames.Count == 1)
               LocationName = LocationNames[0];

            if (LocationName == "")
               return DefaultValues(RawVariableName);
            }

         XmlNode LocationNode = FindProfileNode(LocationName);

         if (LocationNode == null)
            throw new Exception("Cannot find sample node: " + LocationName);

         VariableValue Value = SoilUtility.GetVariableValueFromProfile(LocationNode, RawVariableName);

         if (IsCodeVariable)
            {
            Value.Strings = Value.Codes;
            Value.Doubles = null;
            }
         if (ToUnits != "")
            ConvertUnits(Value, ToUnits);
         return Value;
         }

      /// <summary>
      /// Return a default value for the specified variable.
      /// </summary>
      private VariableValue DefaultValues(string RawVariableName)
         {
         if (RawVariableName.ToLower() == "no3" || RawVariableName.ToLower() == "nh4")
            {
            VariableValue Value = new VariableValue();
            Value.Name = RawVariableName;
            Value.Units = "ppm";
            Value.ThicknessMM = TargetThickness;
            Value.Doubles = MathUtility.CreateArrayOfValues(0.1, Value.ThicknessMM.Length);
            return Value;
            }
         throw new Exception("No values found for: " + RawVariableName);
         }

      /// <summary>
      /// Find a profile node with the specified name
      /// </summary>
      private XmlNode FindProfileNode(string LocationName)
         {
         XmlNode LocationNode;
         LocationNode = XmlHelper.FindByType(Data, LocationName); // normal node
         if (LocationNode == null)
            LocationNode = XmlHelper.Find(Data, "Water/" + LocationName); // crop node
         if (LocationNode == null)
            LocationNode = XmlHelper.Find(Data, LocationName);            // sample node
         if (LocationNode == null)
            LocationNode = XmlHelper.Find(Data, "Swim/" + LocationName); // crop node
         if (LocationNode == null)
            LocationNode = Data.AppendChild(Data.OwnerDocument.CreateElement(LocationName));  // go create it.
         return LocationNode;
         }

      /// <summary>
      /// Find the first matching location node for the specified variable and return it's name.
      /// </summary>
      private string FindFirstMatchingLocation(string RawVariableName)
         {
         foreach (XmlNode Node in XmlHelper.ChildNodes(Data, ""))
            {
            if (XmlHelper.Find(Node, "Layer/" + RawVariableName) != null)
               return XmlHelper.Name(Node);
            }
         return "";
         }

      // ----------------------- Unit methods -------------------------------

      private void ConvertUnits(VariableValue Value, string ToUnits)
         {
         if (Value.Name == "Depth" && Value.Units != ToUnits)
            {
            Value.Doubles = Value.ThicknessMM;
            Value.Units = "mm";
            Value.Name = "Thickness";
            ConvertUnits(Value, ToUnits);
            Value.Strings = SoilUtility.ToDepthStrings(Value.Doubles);
            Value.Doubles = null;
            Value.Units = ToUnits;
            Value.Name = "Depth";
            }
         if (MathUtility.ValuesInArray(Value.Doubles))
            {
            if (Value.Units == "" || ToUnits == "")
               { }

            else if (Value.Units == "mm/mm" && ToUnits == "mm")
               Value.Doubles = MathUtility.Multiply(Value.Doubles, Value.ThicknessMM);

            else if (Value.Units == "mm" && ToUnits == "mm/mm")
               Value.Doubles = MathUtility.Divide(Value.Doubles, Value.ThicknessMM);

            else if (Value.Units == "mm" && ToUnits == "cm")
               Value.Doubles = MathUtility.Divide_Value(Value.Doubles, 10);

            else if (Value.Units == "cm" && ToUnits == "mm")
               Value.Doubles = MathUtility.Multiply_Value(Value.Doubles, 10);

            else if (Value.Units == "mm/mm" && ToUnits == "grav. mm/mm")
               {
               VariableValue BD = FindVariable("BD (g/cc)", Value.ThicknessMM);
               Value.Doubles = MathUtility.Divide(Value.Doubles, BD.Doubles);
               }

            else if (Value.Units == "grav. mm/mm" && ToUnits == "mm/mm")
               {
               VariableValue BD = FindVariable("BD (g/cc)", Value.ThicknessMM);
               Value.Doubles = MathUtility.Multiply(Value.Doubles, BD.Doubles);
               }

            else if (Value.Units == "Walkley Black %" && ToUnits == "Total %")
               Value.Doubles = MathUtility.Multiply_Value(Value.Doubles, 1.3);

            else if (Value.Units == "Total %" && ToUnits == "Walkley Black %")
               Value.Doubles = MathUtility.Divide_Value(Value.Doubles, 1.3);

            else if (Value.Units == "kg/ha" && ToUnits == "ppm")
               {
               VariableValue BD = FindVariable("BD (g/cc)", Value.ThicknessMM);
               for (int i = 0; i < Value.Doubles.Length; i++)
                  Value.Doubles[i] = Value.Doubles[i] * 100 / (BD.Doubles[i] * Value.ThicknessMM[i]);
               Value.Units = "ppm";
               }
            else if (Value.Units == "ppm" && ToUnits == "kg/ha")
               {
               VariableValue BD = FindVariable("BD (g/cc)", Value.ThicknessMM);
               for (int i = 0; i < Value.Doubles.Length; i++)
                  Value.Doubles[i] = Value.Doubles[i] / 100 * (BD.Doubles[i] * Value.ThicknessMM[i]);
               Value.Units = "ppm";
               }

            else if (Value.Units == "CaCl2" && ToUnits == "1:5 water")
               {
               // pH in water = (pH in CaCl X 1.1045) - 0.1375
               Value.Doubles = MathUtility.Subtract_Value(MathUtility.Multiply_Value(Value.Doubles, 1.1045), 0.1375);
               }

            else if (Value.Units != ToUnits)
               throw new Exception("Cannot convert units from " + Value.Units + " to " + ToUnits);

            Value.Units = ToUnits;
            }
         }

      // ----------------------- Data table methods -------------------------------

      /// <summary>
      /// Write the specified variable names to to the specified data table. Mapped.
      /// </summary>
      public void Write(DataTable Table, List<string> VariableNames)
         {
         int StartRow = Table.Rows.Count;
         foreach (string VariableName in VariableNames)
            {
            if (VariableName == "Name")
               {
               if (!Table.Columns.Contains(VariableName))
                  Table.Columns.Add(VariableName, typeof(string));
               DataTableUtility.AddValue(Table, VariableName, Name, StartRow, TargetThickness.Length);
               }
            else if (SoilMetaData.Instance.IsProperty(VariableName))
               {
               if (!Table.Columns.Contains(VariableName))
                  Table.Columns.Add(VariableName, typeof(string));
               DataTableUtility.AddValue(Table, VariableName, Property(VariableName), StartRow, TargetThickness.Length);
               }
            else
               {
               VariableValue Value = FindVariable(VariableName, TargetThickness);
               if (Value.Strings != null)
                  {
                  // strings
                  if (!Table.Columns.Contains(VariableName))
                     Table.Columns.Add(VariableName, typeof(string));
                  DataTableUtility.AddColumn(Table, VariableName, Value.Strings, StartRow, TargetThickness.Length);
                  }
               else
                  {
                  if (!Table.Columns.Contains(VariableName))
                     Table.Columns.Add(VariableName, typeof(double));
                  DataTableUtility.AddColumn(Table, VariableName, Value.Doubles, StartRow, TargetThickness.Length);
                  }
               }

            }
         }

      /// <summary>
      /// Write the specified variable names to the specified data table. UnMapped.
      /// </summary>
      public void WriteUnMapped(DataTable Table, List<string> VariableNames)
         {
         WriteUnMapped(Table, VariableNames, "");

         }

      /// <summary>
      /// Write the specified variable names to the specified data table. UnMapped.
      /// </summary>
      public void WriteUnMapped(DataTable Table, List<string> VariableNames, string LocationName)
         {
         VariableValue Thickness = new VariableValue();
         Thickness.Units = "mm";

         // Get all variables other than thickness and depth - we'll do them later.
         foreach (string VariableName in VariableNames)
            {
            string RawVariableName = VariableName;
            string ToUnits = StringManip.SplitOffBracketedValue(ref RawVariableName, '(', ')');

            if (RawVariableName == "Thickness")
               Table.Columns.Add(VariableName, typeof(double));
            else if (RawVariableName == "Depth")
               Table.Columns.Add(VariableName, typeof(string));
            else if (RawVariableName == "DepthMidPoints")
               Table.Columns.Add(VariableName, typeof(double));
            else
               {
               VariableValue Value = CalculatedVariables(RawVariableName);
               if (Value == null)
                  {
                  if (LocationName == "")
                     Value = FindVariable(VariableName);
                  else
                     Value = FindVariable(VariableName, LocationName);
                  }
               if (Thickness.Doubles == null)
                  Thickness.Doubles = Value.ThicknessMM;
               
               if (Value.Strings != null)
                  {
                  Table.Columns.Add(VariableName, typeof(string));
                  DataTableUtility.AddColumn(Table, VariableName, Value.Strings);
                  }
               else
                  {
                  Table.Columns.Add(VariableName, typeof(double));
                  DataTableUtility.AddColumn(Table, VariableName, Value.Doubles);
                  }
               }
            }

         if (Thickness.Doubles == null)
            Thickness.Doubles = TargetThickness;
            
         // Now we can do thickness and depth.
         foreach (string VariableName in VariableNames)
            {
            string RawVariableName = VariableName;
            string ToUnits = StringManip.SplitOffBracketedValue(ref RawVariableName, '(', ')');

            if (RawVariableName == "Thickness")
               {
               VariableValue ConvertedThickness = Thickness;
               ConvertUnits(ConvertedThickness, ToUnits);
               DataTableUtility.AddColumn(Table, VariableName, ConvertedThickness.Doubles);
               }
            else if (RawVariableName == "Depth")
               {
               VariableValue ConvertedThickness = Thickness;
               ConvertUnits(ConvertedThickness, ToUnits);
               string[] Depths = SoilUtility.ToDepthStrings(ConvertedThickness.Doubles);
               DataTableUtility.AddColumn(Table, VariableName, Depths);
               }
            else if (RawVariableName == "DepthMidPoints")
               {
               VariableValue ConvertedThickness = Thickness;
               ConvertUnits(ConvertedThickness, ToUnits);
               double[] Depths = SoilUtility.ToMidPoints(ConvertedThickness.Doubles);
               DataTableUtility.AddColumn(Table, VariableName, Depths);
               }
            }

         }
         
      /// <summary>
      /// Read in all soil information from the specified data table 
      /// starting with the specified row in the table. No mapping of
      /// values will be done. Instead the values are simply written
      /// to the appropriate node.
      /// </summary>
      public void Read(DataTable Table, int StartRow)
         {
         // Firstly work out how many rows (starting from StartRow) does the
         // soil go for.
         string[] Names = DataTableUtility.GetColumnAsStrings(Table, "Name");
         if (Names.Length > 0)
            {
            int NumRows = 1;
            for (int i = StartRow+1; i < Names.Length; i++)
               {
               if (Names[i] == Names[StartRow])
                  NumRows++;
               else
                  break;
               }

            // Set the name.
            XmlHelper.SetName(Data, Names[StartRow]);

            // Set everything else.
            string[] ColumnNames = DataTableUtility.GetColumnNames(Table);
            foreach (string ColumnName in ColumnNames)
               {
               if (ColumnName.ToLower() != "name")
                  Read(Table, ColumnName, StartRow, NumRows, "");
               }
            }
         
         }

      /// <summary>
      /// Read in all soil information from the specified data table 
      /// starting with the specified row in the table. No mapping of
      /// values will be done. Instead the values are simply written
      /// to the appropriate node.
      /// </summary>
      private void Read(DataTable Table, string VariableName, int StartRow, int NumRows, string LocationName)
         {
         VariableValue Value = new VariableValue();
         Value.Name = VariableName;
         Value.Units = StringManip.SplitOffBracketedValue(ref Value.Name, '(', ')');
         if (Table.Columns[VariableName].DataType == typeof(string))
            Value.Strings = DataTableUtility.GetColumnAsStrings(Table, VariableName, NumRows, StartRow);
         else
            Value.Doubles = DataTableUtility.GetColumnAsDoubles(Table, VariableName, NumRows, StartRow);

         Value.ThicknessMM = GetThicknessMMFromTable(Table, StartRow, NumRows);
         if (SoilMetaData.Instance.IsProperty(Value.Name))
            {
            if (Value.Strings[0] != "")
               SetProperty(Value.Name, Value.Strings[0]);
            }
         else
            {
            if (LocationName == "")
               LocationName = SoilMetaData.Instance.GetVariablePath(Value.Name);
            if (Value.Name == "ParticleSizeCode")
               {
               Value.Name = "ParticleSizeSandCode";
               SetVariable(LocationName, Value);
               Value.Name = "ParticleSizeSiltCode";
               SetVariable(LocationName, Value);
               Value.Name = "ParticleSizeClayCode";
               SetVariable(LocationName, Value);
               }
            else
               SetVariable(LocationName, Value);
            }
         }

      private double[] GetThicknessMMFromTable(DataTable Table, int StartRow, int NumRows)
         {
         double[] ThicknessMM = null;
         foreach (DataColumn Column in Table.Columns)
            {
            string RawVariableName = Column.ColumnName;
            string Units = StringManip.SplitOffBracketedValue(ref RawVariableName, '(', ')');

            if (RawVariableName == "Thickness")
               ThicknessMM = DataTableUtility.GetColumnAsDoubles(Table, Column.ColumnName, NumRows, StartRow);
            else if (RawVariableName == "Depth")
               {
               string[] DepthStrings = DataTableUtility.GetColumnAsStrings(Table, Column.ColumnName, NumRows, StartRow);
               ThicknessMM = SoilUtility.ToThickness(DepthStrings);
               }

            if (ThicknessMM != null)
               {
               if (Units == "cm")
                  ThicknessMM = MathUtility.Multiply_Value(ThicknessMM, 10);
               return ThicknessMM;
               }
            }
         return null;
         }

      public void Read(DataTable Table, string VariableName, string LocationName)
         {
         Read(Table, VariableName, 0, Table.Rows.Count, LocationName);
         }

      // ----------------------- Crop methods -------------------------------

      /// <summary>
      /// Return a list of crop names (measured + predicted)
      /// </summary>
      public string[] Crops
         {
         get
            {
            List<string> CropNames = new List<string>();
            XmlNode WaterNode = XmlHelper.Find(Data, "Water");
            if (WaterNode != null)
               {
               foreach (XmlNode CropNode in XmlHelper.ChildNodes(WaterNode, "SoilCrop"))
                  CropNames.Add(XmlHelper.Name(CropNode));
               XmlNode PredLLCoeff = Configuration.Instance.GetSettingsNode("PredictedLLCoeff");
               if (PredLLCoeff != null && Property("SoilType") != "")
                  {
                  XmlNode PredSoilTypeNode = XmlHelper.Find(PredLLCoeff, Property("SoilType"));
                  if (PredSoilTypeNode != null)
                     {
                     foreach (XmlNode Node in PredSoilTypeNode.ChildNodes)
                        {
                        if (StringManip.IndexOfCaseInsensitive(CropNames, Node.Name) == -1 &&
                            PredictedCropVariable(Node.Name, "ll") != null)
                           CropNames.Add(Node.Name);
                        }
                     }
                  }
               }
            string[] NamesToReturn = new string[CropNames.Count];
            CropNames.CopyTo(NamesToReturn);
            return NamesToReturn;
            }
         }

      /// <summary>
      /// Return a list of measured crop names
      /// </summary>
      private string[] CropsMeasured
         {
         get
            {
            List<string> CropNames = new List<string>();
            XmlNode CropsNode = XmlHelper.Find(Data, "Crops");
            if (CropsNode != null)
               {
               foreach (XmlNode CropNode in CropsNode.ChildNodes)
                  CropNames.Add(XmlHelper.Name(CropNode));
               }
            string[] NamesToReturn = new string[CropNames.Count];
            CropNames.CopyTo(NamesToReturn);
            return NamesToReturn;
            }
         }
      
      /// <summary>
      /// Return a crop variable to caller. e.g. VariableName = wheat ll
      /// </summary>
      private VariableValue CropVariable(string RawVariableName)
         {
         VariableValue Value = null;
         int PosSpace = RawVariableName.LastIndexOf(" ");
         if (PosSpace != -1)
            {
            string CropName = RawVariableName.Substring(0, PosSpace);
            string CropVariableName = RawVariableName.Substring(PosSpace + 1);

            // Return calculated PAWC relative to crop ll
            if (CropVariableName.ToLower() == "pawc")
               {
               VariableValue LL = FindVariable(CropName + " LL(mm/mm)");
               VariableValue XF = FindVariable(CropName + " XF(0-1)");
               VariableValue DUL = FindVariable("DUL(mm/mm)", LL.ThicknessMM);

               Value = new VariableValue();
               Value.Doubles = PAWC(LL.ThicknessMM, LL.Doubles, DUL.Doubles, XF.Doubles);
               Value.Name = CropName + " PAWC";
               Value.Units = "mm/mm";
               Value.Codes = StringManip.CreateStringArray("Calculated", Value.Doubles.Length);
               Value.ThicknessMM = LL.ThicknessMM;
               }
            // Return calculated PAWC relative to crop ll
            else if (CropVariableName.ToLower() == "paw")
               {
               VariableValue LL = FindVariable(CropName + " LL(mm/mm)");
               VariableValue XF = FindVariable(CropName + " XF(0-1)");
               VariableValue SW = FindVariable("SW (mm/mm)", LL.ThicknessMM);

               Value = new VariableValue();
               Value.Doubles = PAWC(LL.ThicknessMM, LL.Doubles, SW.Doubles, XF.Doubles);
               Value.Name = CropName + " PAW";
               Value.Units = "mm/mm";
               Value.Codes = StringManip.CreateStringArray("Calculated", Value.Doubles.Length);
               Value.ThicknessMM = LL.ThicknessMM;
               }
            else
               {
               // Find the crop node firstly.
               XmlNode CropNode = XmlHelper.Find(Data, "Water/" + CropName);
               if (CropNode != null)
                  {
                  Value = SoilUtility.GetVariableValueFromProfile(CropNode, CropVariableName);
                  Value.Name = RawVariableName;
                  }

               // Try getting a predicted crop variable.
               if (Value == null)
                  Value = PredictedCropVariable(CropName, CropVariableName);
               }
            return Value;
            }
         return null;
         }

      /// <summary>
      /// Return a predicted crop variable name for the specified crop and variable
      /// </summary>
      private VariableValue PredictedCropVariable(string CropName, string CropVariableName)
         {
         VariableValue Value = null;
         string SoilType = Property("SoilType");
         if (SoilType != "" && CropVariableName.ToLower() == "ll")
            {
            // If we get to here then must be a predicted variable.
            XmlNode PredLLNode = XmlHelper.Find(Configuration.Instance.GetSettingsNode("PredictedLLCoeff"), SoilType + "/" + CropName);
            if (PredLLNode != null)
               {
               double[] a = SoilUtility.GetLayered(PredLLNode, "a");
               double[] b = SoilUtility.GetLayered(PredLLNode, "b");
               double[] CoeffDepthCentre = SoilUtility.GetLayered(PredLLNode, "layercentre");

               // Get some soil numbers we're going to need.
               double[] SoilDepthCentre = Variable("DepthMidPoints (mm)");
               double[] SoilDUL = Variable("DUL (mm/mm)");
               double[] SoilLL15 = Variable("LL15 (mm/mm)");

               // only continue if our soil depth  centers are within range of
               // the coefficient depth centers.
               if (SoilDepthCentre[SoilDepthCentre.Length - 1] <= CoeffDepthCentre[CoeffDepthCentre.Length - 1])
                  {
                  double[] PredLL = new double[SoilDepthCentre.Length];
                  for (int i = 0; i != SoilDepthCentre.Length; i++)
                     {
                     bool DidInterpolate = false;
                     double A = MathUtility.LinearInterpReal(SoilDepthCentre[i], CoeffDepthCentre, a, ref DidInterpolate);
                     double B = MathUtility.LinearInterpReal(SoilDepthCentre[i], CoeffDepthCentre, b, ref DidInterpolate);
                     PredLL[i] = SoilDUL[i] * (A + B * SoilDUL[i]) / 100.0;

                     // Bound the predicted LL values.
                     PredLL[i] = Math.Max(PredLL[i], SoilLL15[i]);
                     PredLL[i] = Math.Min(PredLL[i], SoilDUL[i]);
                     }

                  //  make the top 3 layers the same as the the top 3 layers of LL15
                  if (PredLL.Length >= 3)
                     {
                     PredLL[0] = SoilLL15[0];
                     PredLL[1] = SoilLL15[1];
                     PredLL[2] = SoilLL15[2];
                     }
                  // Create a variable value structure to return to caller.
                  Value = new VariableValue();
                  Value.Name = CropName + " " + CropVariableName;
                  Value.Units = "mm/mm";
                  Value.Doubles = PredLL;
                  Value.ThicknessMM = TargetThickness;
                  Value.Codes = StringManip.CreateStringArray("Calculated", Value.Doubles.Length);
                  }
               }
            }
         else if (SoilType != "" && CropVariableName.ToLower() == "kl")
            {
            // If we get to here then must be a predicted variable.
            XmlNode PredKLNode = XmlHelper.Find(Configuration.Instance.GetSettingsNode("PredictedKLCoeff"), CropName);
            if (PredKLNode != null)
               {
               double[] kl = SoilUtility.GetLayered(PredKLNode, "kl");
               double[] CoeffDepthCentre = SoilUtility.GetLayered(PredKLNode, "layercentre");
               double[] SoilDepthCentre = Variable("DepthMidPoints (mm)");
               double[] Values = new double[SoilDepthCentre.Length];
               bool DidInterpolate = true;
               for (int i = 0; i != SoilDepthCentre.Length; i++)
                  Values[i] = MathUtility.LinearInterpReal(SoilDepthCentre[i], CoeffDepthCentre, kl, ref DidInterpolate);

               // Create a variable value structure to return to caller.
               Value = new VariableValue();
               Value.Name = CropName + " " + CropVariableName;
               Value.Units = "/day";
               Value.Doubles = Values;
               Value.ThicknessMM = TargetThickness;
               Value.Codes = StringManip.CreateStringArray("Calculated", Value.Doubles.Length);
               }
            }
         else if (SoilType != "" && CropVariableName.ToLower() == "xf")
            {
            // Set the estimated XF values to the first measured crop.
            Value = new VariableValue();
            Value.Name = CropName + " " + CropVariableName;
            Value.Units = "0-1";
            Value.ThicknessMM = TargetThickness;

            string[] Crops = CropsMeasured;
            if (Crops.Length >= 1)
               Value.Doubles = Variable(Crops[0] + " xf (0-1)");
            else
               {
               double[] xf = new double[TargetThickness.Length];
               for (int i = 0; i < xf.Length; i++)
                  xf[i] = 1.0;
               Value.Doubles = xf;
               }
            Value.Codes = StringManip.CreateStringArray("Calculated", Value.Doubles.Length);
            }
         return Value;
         }

      /// <summary>
      /// Sets a crop variable - no mapping.
      /// </summary>
      internal void SetCropVariable(VariableValue Value)
         {
         if (MathUtility.ValuesInArray(Value.Doubles) || MathUtility.ValuesInArray(Value.Strings))
            {
            int PosSpace = Value.Name.LastIndexOf(" ");
            string CropName = Value.Name.Substring(0, PosSpace);
            string CropVariableName = Value.Name.Substring(PosSpace + 1);

            // Find the crops node.
            XmlNode WaterNode = XmlHelper.Find(Data, "Water");
            if (WaterNode == null)
               WaterNode = Data.AppendChild(Data.OwnerDocument.CreateElement("Water"));

            // Try and find the crop node firstly.
            XmlNode CropNode = XmlHelper.Find(WaterNode, CropName);
            if (CropNode == null)
               {
               CropNode = WaterNode.AppendChild(Data.OwnerDocument.CreateElement("SoilCrop"));
               XmlHelper.SetName(CropNode, CropName);

               // setup default thicknesses.
               SoilUtility.SetLayered(CropNode, "Thickness", TargetThickness);
               XmlNode ThicknessNode = XmlHelper.Find(CropNode, "Layer/Thickness");
               XmlHelper.SetAttribute(ThicknessNode, "units", "mm");
               }

            // Now we can set the values.
            Value.Name = CropVariableName;
            SoilUtility.SetVariableValue(CropNode, Value);

            // Make sure we have depths for each crop layer.
            if (Value.ThicknessMM != null)
               {
               SoilUtility.SetLayered(CropNode, "Thickness", Value.ThicknessMM);
               XmlNode FirstThicknessNode = XmlHelper.Find(CropNode, "Layer/Thickness");
               XmlHelper.SetAttribute(FirstThicknessNode, "units", "mm");
               }
            }
         }


      /// <summary>
      /// Return SW as it comes from a <InitWater> node. Returns null if not found.
      /// </summary>
      private VariableValue SWFromInitWater(string RawVariableName)
         {
         XmlNode InitWaterNode = XmlHelper.FindByType(Data, "InitWater");
         if (RawVariableName.ToLower() == "sw" && InitWaterNode != null)
            {
            double[] ll;
            double[] pawc;
            double[] xf = null;
            string RelativeTo = XmlHelper.Value(InitWaterNode, "RelativeTo");
            if (RelativeTo == "" || RelativeTo == "ll15")
               {
               ll = Variable("LL15 (mm)");
               pawc = Variable("PAWC (mm)");
               }
            else
               {
               ll = Variable(RelativeTo + " ll (mm)");
               xf = Variable(RelativeTo + " xf (0-1)");
               pawc = Variable(RelativeTo + " PAWC (mm)");
               }

            double[] dul = Variable("DUL (mm)");
            double[] sw = new double[ll.Length];
            if (XmlHelper.Value(InitWaterNode, "DepthWetSoilMethod/Depth") == "")
               {
               XmlNode PercentMethodNode = XmlHelper.Find(InitWaterNode, "PercentMethod");
               double Percent = 0;
               if (PercentMethodNode != null)
                  Percent = Convert.ToDouble(XmlHelper.Value(PercentMethodNode, "Percent")) * 100;
               if (XmlHelper.Value(PercentMethodNode, "Distributed").ToLower() == "filled from top")
                  {
                  double AmountWater = MathUtility.Sum(pawc) * (Percent / 100.0);
                  for (int Layer = 0; Layer < ll.Length; Layer++)
                     {
                     if (AmountWater >= 0 && xf != null && xf[Layer] == 0)
                        sw[Layer] = ll[Layer];
                     else if (AmountWater >= pawc[Layer])
                        {
                        sw[Layer] = dul[Layer];
                        AmountWater = AmountWater - pawc[Layer];
                        }
                     else
                        {
                        double Prop = AmountWater / pawc[Layer];
                        sw[Layer] = Prop * (dul[Layer] - ll[Layer]) + ll[Layer];
                        AmountWater = 0;
                        }
                     }
                  }
               else
                  {
                  for (int Layer = 0; Layer < ll.Length; Layer++)
                     sw[Layer] = Percent / 100.0 * (dul[Layer] - ll[Layer]) + ll[Layer];
                  }
               }
            else
               {
               double DepthWetSoil = Convert.ToDouble(XmlHelper.Value(InitWaterNode, "DepthWetSoilMethod/Depth"));

               double[] Thickness = TargetThickness;
               double DepthSoFar = 0;
               for (int Layer = 0; Layer < ll.Length; Layer++)
                  {
                  if (DepthWetSoil > DepthSoFar + Thickness[Layer])
                     sw[Layer] = dul[Layer];
                  else
                     {
                     double Prop = Math.Max(DepthWetSoil - DepthSoFar, 0) / Thickness[Layer];
                     sw[Layer] = Prop * (dul[Layer] - ll[Layer]) + ll[Layer];
                     }
                  DepthSoFar += Thickness[Layer];
                  }
               }
            VariableValue Value = new VariableValue();
            Value.Name = "SW";
            Value.Units = "mm";
            Value.ThicknessMM = TargetThickness;
            Value.Doubles = sw;
            StringManip.CreateStringArray("Calculated", Value.ThicknessMM.Length);
            return Value;
            }
         else
            return null;
         }

      // ----------------------- Calculated methods -------------------------------

      /// <summary>
      /// Return the plant available water CAPACITY using the specified lower limits and XF values.
      /// The XF array can equal null.
      /// </summary>
      private double[] PAWC(double[] Thickness, double[] LL, double[] DUL, double[] XF)
         {
         double[] PAWC = new double[Thickness.Length];

         if (Thickness.Length != DUL.Length || Thickness.Length != LL.Length)
            return null;

         for (int layer = 0; layer != Thickness.Length; layer++)
            if (DUL[layer] == MathUtility.MissingValue ||
                LL[layer] == MathUtility.MissingValue ||
                (XF != null && XF[layer] == 0) ||
                (layer > 0 && PAWC[layer - 1] == 0))
               PAWC[layer] = 0;
            else
               PAWC[layer] = DUL[layer] - LL[layer];
         return PAWC;
         }

      /// <summary>
      /// Try and return the value of various calculated variables. Returns null if not a
      /// calculated variable.
      /// </summary>
      private VariableValue CalculatedVariables(string RawVariableName)
         {
         RawVariableName = RawVariableName.ToLower();

         VariableValue Value = null;
         if (RawVariableName == "depth")
            {
            Value = FindVariable("Thickness");
            Value.Name = "Depth";
            Value.Strings = SoilUtility.ToDepthStrings(Value.Doubles);
            Value.Doubles = null;
            Value.Codes = StringManip.CreateStringArray("Calculated", Value.Strings.Length);
            }

         else if (RawVariableName == "depthmidpoints")
            {
            Value = FindVariable("Thickness");
            Value.Name = "DepthMidPoints";
            Value.Doubles = SoilUtility.ToMidPoints(Value.Doubles);
            Value.Codes = StringManip.CreateStringArray("Calculated", Value.Doubles.Length);
            }

         // PAWC relative to LL15
         else if (RawVariableName == "pawc")
            {
            Value = new VariableValue();
            double[] LL15 = Variable("LL15 (mm/mm)");
            double[] DUL = Variable("DUL (mm/mm)");
            Value.Doubles = PAWC(TargetThickness, LL15, DUL, null);
            Value.Name = "PAWC";
            Value.Units = "mm/mm";
            Value.Codes = StringManip.CreateStringArray("Calculated", Value.Doubles.Length);
            Value.ThicknessMM = TargetThickness;
            }

         // PAW relative to LL15
         else if (RawVariableName == "paw")
            {
            Value = new VariableValue();
            double[] LL15 = Variable("LL15 (mm/mm)");
            double[] SW = Variable("SW (mm/mm)");
            Value.Doubles = PAWC(TargetThickness, LL15, SW, null);
            Value.Name = "PAW";
            Value.Units = "mm/mm";
            Value.Codes = StringManip.CreateStringArray("Calculated", Value.Doubles.Length);
            Value.ThicknessMM = TargetThickness;
            }

         // InertC
         else if (RawVariableName == "inertc")
            {
            // Could be a different layer structure to TargetThickness - can't use Variable method.
            VariableValue OC = FindVariable("OC");
            ConvertUnits(OC, "Total %");
            VariableValue FInert = FindVariable("FInert (0-1)", OC.ThicknessMM);
            VariableValue BD = FindVariable("BD (g/cc)", OC.ThicknessMM);

            double[] InertC = new double[OC.ThicknessMM.Length];

            for (int i = 0; i < OC.Doubles.Length; i++)
               {
               if (FInert.Doubles[i] == MathUtility.MissingValue || 
                   OC.Doubles[i] == MathUtility.MissingValue ||
                   BD.Doubles[i] == MathUtility.MissingValue)
                  InertC[i] = MathUtility.MissingValue;
               else
                  {
                  double soiln2_fac = 100.0 / (BD.Doubles[i] * OC.ThicknessMM[i]);
                  double oc_ppm = OC.Doubles[i] / 100 * ppm;
                  double carbon_tot = oc_ppm / soiln2_fac;
                  InertC[i] = FInert.Doubles[i] * carbon_tot;
                  }
               }
            Value = new VariableValue();
            Value.Doubles = InertC;
            Value.Name = "InertC";
            Value.Units = "kg/ha";
            Value.Codes = StringManip.CreateStringArray("Calculated", Value.Doubles.Length);
            Value.ThicknessMM = OC.ThicknessMM;
            }

         // BiomC
         else if (RawVariableName == "biomc")
            {
            // Could be a different layer structure to TargetThickness - can't use Variable method.
            VariableValue OC = FindVariable("OC");
            ConvertUnits(OC, "Total %");
            VariableValue FBiom = FindVariable("FBiom (0-1)", OC.ThicknessMM);
            VariableValue BD = FindVariable("BD (g/cc)", OC.ThicknessMM);
            VariableValue InertC = FindVariable("InertC (kg/ha)", OC.ThicknessMM);

            double[] BiomC = new double[OC.ThicknessMM.Length];
            for (int i = 0; i < OC.ThicknessMM.Length; i++)
               {
               if (OC.Doubles[i] == MathUtility.MissingValue ||
                   FBiom.Doubles[i] == MathUtility.MissingValue ||
                   BD.Doubles[i] == MathUtility.MissingValue ||
                   InertC.Doubles[i] == MathUtility.MissingValue)
                  BiomC[i] = MathUtility.MissingValue;
               else
                  {
                  double soiln2_fac = 100.0 / (BD.Doubles[i] * OC.ThicknessMM[i]);
                  double oc_ppm = OC.Doubles[i] / 100 * ppm;
                  double carbon_tot = oc_ppm / soiln2_fac;
                  BiomC[i] = ((carbon_tot - InertC.Doubles[i]) * FBiom.Doubles[i]) / (1.0 + FBiom.Doubles[i]);
                  }
               }
            Value = new VariableValue();
            Value.Doubles = BiomC;
            Value.Name = "BiomC";
            Value.Units = "kg/ha";
            Value.Codes = StringManip.CreateStringArray("Calculated", Value.Doubles.Length);
            Value.ThicknessMM = OC.ThicknessMM;
            }
         // HumC
         else if (RawVariableName == "humc")
            {
            // Could be a different layer structure to TargetThickness - can't use Variable method.
            VariableValue OC = FindVariable("OC");
            ConvertUnits(OC, "Total %");
            VariableValue BD = FindVariable("BD (g/cc)", OC.ThicknessMM);
            VariableValue BiomC = FindVariable("BiomC (kg/ha)", OC.ThicknessMM);
            VariableValue InertC = FindVariable("InertC (kg/ha)", OC.ThicknessMM);

            double[] HumC = new double[BiomC.Doubles.Length];

            for (int i = 0; i < OC.ThicknessMM.Length; i++)
               {
               if (BiomC.Doubles[i] == MathUtility.MissingValue)
                  HumC[i] = MathUtility.MissingValue;
               else
                  {
                  double soiln2_fac = 100.0 / (BD.Doubles[i] * OC.ThicknessMM[i]);
                  double oc_ppm = OC.Doubles[i] / 100 * ppm;
                  double carbon_tot = oc_ppm / soiln2_fac;
                  HumC[i] = carbon_tot - BiomC.Doubles[i] - InertC.Doubles[i];
                  }
               }
            Value = new VariableValue();
            Value.Doubles = HumC;
            Value.Name = "HumC";
            Value.Units = "kg/ha";
            Value.Codes = StringManip.CreateStringArray("Calculated", Value.Doubles.Length);
            Value.ThicknessMM = OC.ThicknessMM;
            }

         return Value;
         }

      /// <summary>
      /// This property returns the target layer structure that we want all variables to
      /// be in. 
      /// </summary>
      public double[] TargetThickness
         {
         get
            {
            if (UserSetTargetThickness != null)
               return UserSetTargetThickness;
            else
               return VariableUnMapped("Thickness (mm)");
            }
         set
            {
            UserSetTargetThickness = value;
            }
         }

      /// <summary>
      /// Use the thickness node if it is present as a UserSetTargetThickness
      /// </summary>
      public void UseThicknessIfPresent()
         {
         string UserThickness = XmlHelper.Value(Data, "Thickness/Values");
         if (UserThickness != "")
            {
            string[] Values = UserThickness.Split(" ".ToCharArray(), StringSplitOptions.RemoveEmptyEntries);
            TargetThickness = MathUtility.StringsToDoubles(Values);
            }
         }

      /// <summary>
      /// Return a list of valid units for the specified variable.
      /// </summary>
      public List<string> ValidUnits(string RawVariableName)
         {
         return SoilMetaData.Instance.ValidUnits(RawVariableName);
         }

      /// <summary>
      /// Return a full code name from the abbreviated code passed in.
      /// </summary>
      public string GetFullCodeName(string CodeText, string RawVariableName)
         {
         return SoilMetaData.Instance.GetFullCodeName(CodeText, RawVariableName);
         }
      
      //public bool UseEC
      //   {
      //   get { return (SoilComponentUtility.GetStringValue(Data, "", "UseEC").ToLower() == "yes"); }
      //   set
      //      {
      //      if (value)
      //         SoilComponentUtility.SetValue(Data, "", "UseEC", "yes");
      //      else
      //         SoilComponentUtility.SetValue(Data, "", "UseEC", "no");
      //      }
      //   }
      //public int MaxRootDepth
      //   {
      //   get
      //      {
      //      string StringValue = SoilComponentUtility.GetStringValue(Data, "", "MaxRootDepth");
      //      if (StringValue != "")
      //         return Convert.ToInt32(StringValue);
      //      else
      //         return 0;
      //      }
      //   set
      //      {
      //      SoilComponentUtility.SetValue(Data, "", "MaxRootDepth", value.ToString());
      //      }
      //   }

      //#region Export
      //public void ExportToPar(string FileName, string SectionName, bool AppendToFile)
      //   {
      //   string Template =
      //      "[$SECTIONNAME$.soilwat2.parameters]\r\n" +
      //      "   diffus_const = [soil.DiffusConst]    ! coeffs for unsaturated water flow\r\n" +
      //          "   diffus_slope = [soil.DiffusSlope]\r\n" +
      //          "   cn2_bare     = [soil.Cn2Bare]    ! bare soil runoff curve number\r\n" +
      //          "   cn_red       = [soil.CnRed]    ! potetial reduction in curve number due to residue\r\n" +
      //          "   cn_cov       = [soil.CnCov]   ! cover for maximum reduction in curve number\r\n" +
      //          "   salb         = [soil.Salb]  ! bare soil albedo\r\n";
      //   if (SummerCona != MathUtility.MissingValue)
      //      {
      //      Template += "   SummerCona   = [soil.SummerCona]   ! stage 2 evap coef. for summer\r\n" +
      //                  "   WinterCona   = [soil.WinterCona]   ! stage 2 evap coef. for winter\r\n" +
      //                  "   SummerU      = [soil.SummerU]      ! stage 1 soil evaporation coefficient for summer (mm)\r\n" +
      //                  "   WinterU      = [soil.WinterU]      ! stage 1 soil evaporation coefficient for winter (mm)\r\n" +
      //                  "   SummerDate   = [soil.SummerDate]      ! Start date of summer\r\n" +
      //                  "   WinterDate   = [soil.WinterDate]      ! Start date of winter\r\n";
      //      }
      //   else
      //      Template += "   cona         = [soil.Cona]   ! stage 2 evap coef.\r\n" +
      //                  "   u            = [soil.U]     ! stage 1 soil evaporation coefficient (mm)\r\n";
      //   Template +=
      //       "\r\n" +
      //       "[foreach Soil.profile]\r\n" +
      //       "   dlayer  =[foreach profile.layer as Layer]  [Layer.thickness.3][endfor]   ! layer thickness mm soil\r\n" +
      //       "   air_dry =[foreach profile.layer as Layer]    [Layer.airdry.3][endfor]   ! air dry mm water/mm soil\r\n" +
      //       "   ll15    =[foreach profile.layer as Layer]    [Layer.ll15.3][endfor]   ! lower limit mm water/mm soil\r\n" +
      //       "   dul     =[foreach profile.layer as Layer]    [Layer.dul.3][endfor]   ! drained upper limit mm water/mm soil\r\n" +
      //       "   sat     =[foreach profile.layer as Layer]    [Layer.sat.3][endfor]   ! saturation mm water/mm soil\r\n" +
      //       "   swcon   =[foreach profile.layer as Layer]    [Layer.swcon.3][endfor]   ! drainage coefficient\r\n" +
      //       "   bd      =[foreach profile.layer as Layer]    [Layer.bd.3][endfor]   ! bulk density gm dry soil/cc moist soil\r\n" +
      //       "$SW$\r\n";

      //   if (MWCON.Length > 0 && MWCON[0] != MathUtility.MissingValue)
      //      Template +=
      //      "   mwcon   =[foreach profile.layer as Layer]    [Layer.mwcon.3][endfor]   \r\n\r\n";
      //   if (KS.Length > 0 && KS[0] != MathUtility.MissingValue)
      //      Template +=
      //      "   ks   =[foreach profile.layer as Layer]    [Layer.ks.3][endfor]   \r\n\r\n";

      //   Template +=
      //       "[endfor]\r\n" +//END OF WATER FOR LOOP
      //       "\r\n" +
      //       "[$SECTIONNAME$.soiln2.parameters]\r\n" +//TITLE
      //       "   root_cn      = [soil.rootcn]     ! C:N ratio of initial root residues\r\n" +
      //       "   root_wt      = [soil.rootwt]   ! root residues as biomass (kg/ha)\r\n" +
      //       "   soil_cn      = [soil.soilcn]   ! C:N ratio of soil\r\n" +
      //       "   enr_a_coeff  = [soil.enracoeff]\r\n" +
      //       "   enr_b_coeff  = [soil.enrbcoeff]\r\n" +
      //       "   profile_reduction =  off\r\n" +
      //       "\r\n" +
      //       "[foreach Soil.profile]\r\n" +
      //       "$NITROGEN$\r\n" +
      //       "   oc      =[foreach profile.layer as Layer]\r\n      [Layer.oc.3][endfor]   ! Soil Organic Carbon\r\n" +
      //       "   ph      =$PH$   ! pH of soil\r\n" +
      //       "   fbiom   =[foreach profile.layer as Layer]\r\n      [Layer.fbiom.3][endfor]   ! Organic C Biomass Fraction\r\n" +
      //       "   finert  =[foreach profile.layer as Layer]\r\n      [Layer.finert.3][endfor]   ! Inert Organic C Fraction\r\n" +
      //       "[endfor]\r\n" +//END OF NITROGEN FOR LOOP
      //       "\r\n" +
      //       "[if [soil.rootcp] > 0]\r\n" +
      //       "[$SECTIONNAME$.soilp.parameters]\r\n" +
      //       "   root_cp            =  [soil.rootcp]      () !c:p ratio of roots at initialisation\r\n" +
      //       "   rate_dissol_rock_P =  [soil.RateDissolRock] (/yr)   !rate at which rock P source becomes available\r\n" +
      //       "   rate_loss_avail_P  =  [soil.RateLossAvailP] (/yr)   ! (< 1) Fraction lost per yr specified at 25 oC" +
      //       "\r\n" +
      //       "[foreach Soil.profile]\r\n" +
      //       "   labile_P  = [foreach profile.layer]    [layer.labilep.3][endfor]   (mg/kg)\r\n" +
      //       "   banded_P  = [foreach profile.layer]    [layer.bandedP.3][endfor]   (kg/ha) ! banded p content for each layer\r\n" +
      //       "   rock_P    = [foreach profile.layer]    [layer.rockP.3][endfor]   (kg/ha)   !rock p content for each layer ie no water soluble\r\n" +
      //       "   sorption  =[foreach profile.layer]  [layer.sorption.3][endfor]   ()   !P sorbed at 0.2ppm\r\n" +
      //       "[endfor]\r\n" +
      //       "[endif]\r\n" +
      //       "$CROP$\r\n" +
      //       "[endfile]\r\n\r\n";

      //   string CropStuff = "";
      //   foreach (string CropName in Crops)
      //      {
      //      CropStuff += "[$SECTIONNAME$." + CropName + ".parameters]\r\n";
      //      if (CropIsPredicted(CropName))
      //         Template += "   !These crop numbers are predicted\r\n";

      //      string LLLine = "";
      //      string KLLine = "";
      //      string XFLine = "";
      //      double[] ll = LL(CropName);
      //      double[] kl = KL(CropName);
      //      double[] xf = XF(CropName);

      //      for (int i = 0; i != ll.Length; i++)
      //         {
      //         LLLine += "      " + ll[i].ToString("f3");
      //         KLLine += "      " + kl[i].ToString("f3");
      //         XFLine += "      " + xf[i].ToString("f3");
      //         }
      //      CropStuff += "   ll      =" + LLLine + "\r\n";
      //      if (CropName.ToLower() == "ozcot")
      //         CropStuff += "   Title = XXX\r\n" +
      //                     "   asoil = 3.0\r\n";

      //      CropStuff += "   kl      =" + KLLine + "\r\n";
      //      CropStuff += "   xf      =" + XFLine + "\r\n";
      //      }
      //   Template = Template.Replace("$CROP$", CropStuff);

      //   string PHLine = "";
      //   double[] ph = PH;

      //   int NumLayers = Thickness.Length;
      //   for (int i = 0; i != NumLayers; i++)
      //      {
      //      if (i < ph.Length)
      //         PHLine += "      " + ph[i].ToString("f3");
      //      }

      //   Template = Template.Replace("$PH$", PHLine);
      //   Template = Template.Replace("$SECTIONNAME$", SectionName);

      //   string SWLine = "";
      //   XmlNode InitW = XmlHelper.Find(Data, "InitWater");
      //   if (InitW != null)
      //      {
      //      SWLine = "   sw      =";
      //      InitWater InitWater = new InitWater(InitW, this);
      //      double[] sw = InitWater.SW;
      //      for (int i = 0; i != sw.Length; i++)
      //         SWLine += "    " + sw[i].ToString("f3");
      //      }
      //   Template = Template.Replace("$SW$", SWLine);

      //   string NitrogenLine = "";
      //   XmlNode InitN = XmlHelper.Find(Data, "InitNitrogen");
      //   if (InitN != null)
      //      {
      //      InitNitrogen InitNitrogen = new InitNitrogen(InitN, this);
      //      double[] no3 = InitNitrogen.NO3;
      //      double[] nh4 = InitNitrogen.NH4;

      //      NitrogenLine = "   no3     =";
      //      for (int i = 0; i != no3.Length; i++)
      //         NitrogenLine += "      " + no3[i].ToString("f3");
      //      NitrogenLine += "\r\n";
      //      NitrogenLine += "   nh4     =";
      //      for (int i = 0; i != nh4.Length; i++)
      //         NitrogenLine += "      " + nh4[i].ToString("f3");
      //      NitrogenLine += "\r\n";
      //      }
      //   Template = Template.Replace("$NITROGEN$", NitrogenLine);


      //   string szSoilFileTemplate = "[file " + Path.GetFileName(FileName) + "]\r\n" + Template;
      //   Macro SoilMacro = new Macro();
      //   StringCollection scSoilFiles = SoilMacro.Go(Data, szSoilFileTemplate,
      //                                    Path.GetDirectoryName(FileName),
      //                                    AppendToFile);
      //   }
      //public XmlNode ExportToSim(XmlNode ParentNode)
      //   {
      //   string errors = CheckThatSimulationWillRun();
      //   if (errors != "")
      //      throw new Exception(errors);

      //   // Water variables
      //   XmlNode SoilNode = ParentNode.AppendChild(ParentNode.OwnerDocument.CreateElement("component"));
      //   XmlHelper.SetName(SoilNode, Name + " Water");
      //   XmlHelper.SetAttribute(SoilNode, "executable", "%apsim%\\Model\\SoilWat.dll");
      //   XmlNode InitData = SoilNode.AppendChild(SoilNode.OwnerDocument.CreateElement("initdata"));
      //   XmlHelper.SetValue(InitData, "include", "%apsim%\\Model\\SoilWat.xml");
      //   XmlHelper.SetValue(InitData, "diffus_const", DiffusConst.ToString());
      //   XmlHelper.SetValue(InitData, "diffus_slope", DiffusSlope.ToString());
      //   XmlHelper.SetValue(InitData, "cn2_bare", CN2Bare.ToString());
      //   XmlHelper.SetValue(InitData, "cn_red", CNRed.ToString());
      //   XmlHelper.SetValue(InitData, "cn_cov", CNCov.ToString());
      //   XmlHelper.SetValue(InitData, "salb", Salb.ToString());
      //   if (SummerCona != MathUtility.MissingValue)
      //      {
      //      XmlHelper.SetValue(InitData, "SummerCona", SummerCona.ToString());
      //      XmlHelper.SetValue(InitData, "WinterCona", WinterCona.ToString());
      //      XmlHelper.SetValue(InitData, "SummerU", SummerU.ToString());
      //      XmlHelper.SetValue(InitData, "WinterU", WinterU.ToString());
      //      XmlHelper.SetValue(InitData, "SummerDate", SummerDate.ToString());
      //      XmlHelper.SetValue(InitData, "WinterDate", WinterDate.ToString());
      //      }
      //   else
      //      {
      //      XmlHelper.SetValue(InitData, "cona", Cona.ToString());
      //      XmlHelper.SetValue(InitData, "u", U.ToString());
      //      }
      //   XmlHelper.SetValue(InitData, "dlayer", SoilComponentUtility.LayeredToString(Thickness));
      //   XmlHelper.SetValue(InitData, "sat", SoilComponentUtility.LayeredToString(SAT));
      //   XmlHelper.SetValue(InitData, "dul", SoilComponentUtility.LayeredToString(DUL));
      //   XmlHelper.SetValue(InitData, "ll15", SoilComponentUtility.LayeredToString(LL15));
      //   XmlHelper.SetValue(InitData, "air_dry", SoilComponentUtility.LayeredToString(Airdry));
      //   XmlHelper.SetValue(InitData, "swcon", SoilComponentUtility.LayeredToString(SWCON));
      //   XmlHelper.SetValue(InitData, "bd", SoilComponentUtility.LayeredToString(BD));
      //   if (MWCON.Length > 0 && MWCON[0] != MathUtility.MissingValue)
      //      XmlHelper.SetValue(InitData, "mwcon", SoilComponentUtility.LayeredToString(MWCON));
      //   if (KS.Length > 0 && KS[0] != MathUtility.MissingValue)
      //      XmlHelper.SetValue(InitData, "ks", SoilComponentUtility.LayeredToString(KS));



      //   // Nitrogen variables
      //   XmlNode Nitrogen = ParentNode.AppendChild(ParentNode.OwnerDocument.CreateElement("component"));
      //   XmlHelper.SetName(Nitrogen, Name + " Nitrogen");
      //   XmlHelper.SetAttribute(Nitrogen, "executable", "%apsim%\\Model\\SoilN.dll");
      //   XmlNode NitrogenInitData = Nitrogen.AppendChild(SoilNode.OwnerDocument.CreateElement("initdata"));
      //   XmlHelper.SetValue(NitrogenInitData, "include", "%apsim%\\Model\\SoilN.xml");
      //   if (Property("SoilType") != "")
      //      XmlHelper.SetValue(NitrogenInitData, "soiltype", Property("SoilType"));
      //   XmlHelper.SetValue(NitrogenInitData, "root_cn", RootCN.ToString());
      //   XmlHelper.SetValue(NitrogenInitData, "root_wt", RootWT.ToString());
      //   XmlHelper.SetValue(NitrogenInitData, "soil_cn", SoilCN.ToString());
      //   XmlHelper.SetValue(NitrogenInitData, "enr_a_coeff", EnrACoeff.ToString());
      //   XmlHelper.SetValue(NitrogenInitData, "enr_b_coeff", EnrBCoeff.ToString());
      //   XmlHelper.SetValue(NitrogenInitData, "profile_reduction", "off");
      //   XmlHelper.SetValue(NitrogenInitData, "oc", SoilComponentUtility.LayeredToString(OC));
      //   XmlHelper.SetValue(NitrogenInitData, "ph", SoilComponentUtility.LayeredToString(PH));
      //   XmlHelper.SetValue(NitrogenInitData, "fbiom", SoilComponentUtility.LayeredToString(FBIOM));
      //   XmlHelper.SetValue(NitrogenInitData, "finert", SoilComponentUtility.LayeredToString(FINERT));
      //   if (Rocks.Length > 0 && Rocks[0] != MathUtility.MissingValue)
      //      XmlHelper.SetValue(NitrogenInitData, "rocks", SoilComponentUtility.LayeredToString(Rocks));

      //   // Write in some default NH4 values.
      //   double[] DefaultNH4 = new double[Thickness.Length];
      //   for (int i = 0; i != Thickness.Length; i++)
      //      DefaultNH4[i] = 0.2;
      //   XmlHelper.SetValue(NitrogenInitData, "nh4ppm", SoilComponentUtility.LayeredToString(DefaultNH4));

      //   // Phosphorus variables
      //   if (RootCP != MathUtility.MissingValue)
      //      {
      //      XmlNode Phosphorus = ParentNode.AppendChild(ParentNode.OwnerDocument.CreateElement("component"));
      //      XmlHelper.SetName(Phosphorus, Name + " Phosphorus");
      //      XmlHelper.SetAttribute(Phosphorus, "executable", "%apsim%\\Model\\SoilP.dll");
      //      XmlNode PhosphorusInitData = Phosphorus.AppendChild(SoilNode.OwnerDocument.CreateElement("initdata"));
      //      XmlHelper.SetValue(PhosphorusInitData, "include", "%apsim%\\Model\\SoilP.xml");
      //      XmlHelper.SetValue(PhosphorusInitData, "Root_CP", RootCP.ToString());
      //      XmlHelper.SetValue(PhosphorusInitData, "rate_dissol_rock_P", RateDissolRock.ToString());
      //      XmlHelper.SetValue(PhosphorusInitData, "rate_loss_avail_P", RateLossAvail.ToString());

      //      XmlHelper.SetValue(PhosphorusInitData, "Labile_P", SoilComponentUtility.LayeredToString(LabileP));
      //      XmlHelper.SetValue(PhosphorusInitData, "banded_P", SoilComponentUtility.LayeredToString(BandedP));
      //      XmlHelper.SetValue(PhosphorusInitData, "rock_P", SoilComponentUtility.LayeredToString(RockP));
      //      XmlHelper.SetValue(PhosphorusInitData, "sorption", SoilComponentUtility.LayeredToString(Sorption));
      //      }
      //   return SoilNode;
      //   }
      //public XmlNode ExportCropToSim(XmlNode ParentNode, string CropName)
      //   {
      //   // Go look for our component node which has already been created for us.
      //   foreach (XmlNode Node in ParentNode.ParentNode.ParentNode.ChildNodes)
      //      {
      //      if (XmlHelper.Name(Node).ToLower() == CropName.ToLower())
      //         {
      //         if (CropExists(CropName) || CropIsPredicted(CropName))
      //            {
      //            XmlHelper.SetValue(Node, "initdata/ll", SoilComponentUtility.LayeredToString(LL(CropName)));
      //            XmlHelper.SetValue(Node, "initdata/kl", SoilComponentUtility.LayeredToString(KL(CropName)));
      //            XmlHelper.SetValue(Node, "initdata/xf", SoilComponentUtility.LayeredToString(XF(CropName)));
      //            }
      //         else
      //            throw new Exception("No soil/crop parameterisation for crop: " + CropName);
      //         return Node;
      //         }
      //      }
      //   throw new Exception("Cannot find crop node : " + CropName);
      //   }

      //#endregion


      //#region Error checking
      /// <summary>
      /// Check that the soil is a valid one.
      /// </summary>
      /// <returns></returns>
      public string CheckForErrors(bool IgnoreWaterAndNitrogen)
         {
         string ApsimToSim = Types.Instance.ApsimToSim("soil").InnerText;
         if (IgnoreWaterAndNitrogen)
            {
            ApsimToSim = ApsimToSim.Replace("[soil.SW(mm/mm)]", "");
            ApsimToSim = ApsimToSim.Replace("[soil.NO3(ppm)]", "");
            ApsimToSim = ApsimToSim.Replace("[soil.NH4(ppm)]", "");
            }

         string ErrorMessages = "";
         try
            {
            string NewApsimToSim = ReplaceSoilMacros(ApsimToSim);
            }
         catch (Exception err)
            {
            ErrorMessages = err.Message;
            }

         // Do some more rigorous checks.
         if (ErrorMessages == "")
            ErrorMessages += CheckProfile();
         if (ErrorMessages == "" && !IgnoreWaterAndNitrogen)
            ErrorMessages += CheckSW();

         return ErrorMessages;
         }

      /// <summary>
      /// Checks validity of soil water parameters for a soil profile layer
      /// This is a port of the soilwat2_check_profile routine.
      /// </summary>
      private string CheckProfile()
         {
         string errorMessages = "";
         const double min_sw = 0.0;
         const double specific_bd = 2.65; // (g/cc)

         double[] thickness = Variable("Thickness(mm)");
         double[] airdry = Variable("AirDry(mm/mm)");
         double[] ll15 = Variable("LL15(mm/mm)");
         double[] dul = Variable("DUL(mm/mm)");
         double[] sat = Variable("SAT(mm/mm)");
         double[] bd = Variable("BD(g/cc)");
         double[] oc = Variable("OC(Total %)");
         double[] ph = Variable("PH(1:5 water)");

         // Check crop variables.
         foreach (string Crop in Crops)
            {
            double[] ll = Variable(Crop + " LL(mm/mm)");
            double[] kl = Variable(Crop + " KL(/day)");
            double[] xf = Variable(Crop + " XF(0-1)");
            if (!MathUtility.ValuesInArray(ll) || !MathUtility.ValuesInArray(kl) ||
                !MathUtility.ValuesInArray(xf))
               errorMessages += "Values for LL, KL or XF are missing for crop " + Crop + "\r\n";

            else
               {
               for (int layer = 0; layer != thickness.Length; layer++)
                  {
                  int RealLayerNumber = layer + 1;

                  if (kl[layer] > 1)
                     errorMessages += Crop + " KL value of " + kl[layer].ToString("f3")
                              + " in layer " + RealLayerNumber.ToString() + " is greater than 1"
                              + "\r\n";

                  if (xf[layer] > 1)
                     errorMessages += Crop + " XF value of " + xf[layer].ToString("f3")
                              + " in layer " + RealLayerNumber.ToString() + " is greater than 1"
                              + "\r\n";

                  if (ll[layer] < airdry[layer])
                     errorMessages += Crop + " LL of " + ll[layer].ToString("f3")
                                  + " in layer " + RealLayerNumber.ToString() + " is below air dry value of " + airdry[layer].ToString("f3")
                                + "\r\n";

                  if (ll[layer] > dul[layer])
                     errorMessages += Crop + " LL of " + ll[layer].ToString("f3")
                                  + " in layer " + RealLayerNumber.ToString() + " is above drained upper limit of " + dul[layer].ToString("f3")
                                + "\r\n";
                  }
               }
            }

         // Check other profile variables.
         for (int layer = 0; layer != thickness.Length; layer++)
            {
            double max_sw = MathUtility.Round(1.0 - bd[layer] / specific_bd, 3);
            int RealLayerNumber = layer + 1;

            if (airdry[layer] < min_sw)
               errorMessages += " Air dry lower limit of " + airdry[layer].ToString("f3")
                                  + " in layer " + RealLayerNumber.ToString() + " is below acceptable value of " + min_sw.ToString("f3")
                          + "\r\n";

            if (ll15[layer] < airdry[layer])
               errorMessages += "15 bar lower limit of " + ll15[layer].ToString("f3")
                            + " in layer " + RealLayerNumber.ToString() + " is below air dry value of " + airdry[layer].ToString("f3")
                          + "\r\n";

            if (dul[layer] < ll15[layer])
               errorMessages += "Drained upper limit of " + dul[layer].ToString("f3")
                            + " in layer " + RealLayerNumber.ToString() + " is at or below lower limit of " + ll15[layer].ToString("f3")
                          + "\r\n";

            if (sat[layer] < dul[layer])
               errorMessages += "Saturation of " + sat[layer].ToString("f3")
                            + " in layer " + RealLayerNumber.ToString() + " is at or below drained upper limit of " + dul[layer].ToString("f3")
                          + "\r\n";

            if (sat[layer] > max_sw)
               {
               double max_bd = (1.0 - sat[layer]) * specific_bd;
               errorMessages += "Saturation of " + sat[layer].ToString("f3")
                            + " in layer " + RealLayerNumber.ToString() + " is above acceptable value of  " + max_sw.ToString("f3")
                          + ". You must adjust bulk density to below " + max_bd.ToString("f3")
                          + " OR saturation to below " + max_sw.ToString("f3")
                          + "\r\n";
               }

            if (bd[layer] > 2.65)
               errorMessages += "BD value of " + bd[layer].ToString("f3")
                            + " in layer " + RealLayerNumber.ToString() + " is greater than the theoretical maximum of 2.65"
                          + "\r\n";
            if (oc[layer] < 0.01)
               errorMessages += "OC value of " + oc[layer].ToString("f3")
                             + " in layer " + RealLayerNumber.ToString() + " is less than 0.01"
                             + "\r\n";
            if (ph[layer] < 3.5)
               errorMessages += "PH value of " + ph[layer].ToString("f3")
                             + " in layer " + RealLayerNumber.ToString() + " is less than 3.5"
                             + "\r\n";
            if (ph[layer] > 11)
               errorMessages += "PH value of " + ph[layer].ToString("f3")
                             + " in layer " + RealLayerNumber.ToString() + " is greater than 11"
                             + "\r\n";
            }
         return errorMessages;
         }

      /// <summary>
      /// Check the validity of initial soil water.
      /// </summary>
      /// <returns></returns>
      private string CheckSW()
         {
         string errorMessages = "";

         double[] thickness = Variable("Thickness(mm)");
         double[] airdry = Variable("AirDry(mm/mm)");
         double[] sat = Variable("SAT(mm/mm)");
         double[] sw = Variable("SW(mm/mm)");
         if (sw.Length > 0)
            {
            for (int layer = 0; layer != thickness.Length; layer++)
               {
               int RealLayerNumber = layer + 1;

               if (sw[layer] > sat[layer])
                  errorMessages += "Soil water of " + sw[layer].ToString("f3")
                                + " in layer " + RealLayerNumber.ToString() + " is above saturation of " + sat[layer].ToString("f3")
                                + "\r\n";

               if (sw[layer] < airdry[layer])
                  errorMessages += "Soil water of " + sw[layer].ToString("f3")
                                + " in layer " + RealLayerNumber.ToString() + " is below air-dry value of " + airdry[layer].ToString("f3")
                                + "\r\n";
               }
            }
         return errorMessages;
         }

      //#region Manipulation / fudges
      //public void ApplyMaxWaterCapacity(int maxWaterCapacity)
      //   {
      //   //---------------------------------------------------
      //   // Adjust the DUL curve, given a max water Capacity
      //   // start from bottom most layer that has water in it and reduced to zero or until
      //   // the whole profile equals the given max water capacity
      //   //---------------------------------------------------
      //   double[] localDUL = DUL;
      //   double[] localLL15 = LL15;
      //   double[] localThickness = Thickness;
      //   //find lowest layer that has water
      //   int bottomLayer;
      //   int layer;
      //   for (layer = 0; layer < localDUL.Length; layer++)
      //      {
      //      double test = ((localDUL[layer] * Thickness[layer]) - (localLL15[layer] * Thickness[layer]));
      //      if (((localDUL[layer] * Thickness[layer]) - (localLL15[layer] * Thickness[layer])) == 0) break;
      //      }
      //   bottomLayer = layer - 1;
      //   double currentCapacitySum = MathUtility.Sum(PAWC());
      //   double capacityDifference = maxWaterCapacity - currentCapacitySum;
      //   if (!(capacityDifference == 0))
      //      {
      //      if (capacityDifference > 0)
      //         {
      //         double newThickness = localThickness[bottomLayer] + (((Math.Abs(capacityDifference)) / (localDUL[bottomLayer] - localLL15[bottomLayer])));
      //         localThickness[bottomLayer] = newThickness;
      //         }
      //      else
      //         {
      //         for (int j = bottomLayer; j >= 0; j--)
      //            {
      //            double waterInLayer = ((localThickness[j]) * (localDUL[j] - localLL15[j]));
      //            if ((Math.Abs(capacityDifference) > waterInLayer))
      //               {
      //               capacityDifference = (Math.Abs(capacityDifference) - (localThickness[j]) * (localDUL[j] - localLL15[j]));
      //               localThickness[j] = 0;
      //               }
      //            else
      //               {
      //               double newThickness = ((Math.Abs(capacityDifference)) / (localDUL[j] - localLL15[j]));
      //               localThickness[j] = localThickness[j] - newThickness;
      //               break;
      //               }
      //            }
      //         }
      //      }
      //   Thickness = localThickness;
      //   }
      //public void ApplyMaxSoilDepth(int soilDepth)
      //   {
      //   // -------------------------------------------------
      //   // Adjust the DUL curve, given a max soil depth(cutoff)
      //   // Leave the DUL number for all whole layers above the cutoff layer
      //   // Work out the proportional DUL number for the layer that has the cutoff within it
      //   // Set the LL15 to the DUL for the whole layers below the cutoff
      //   // -------------------------------------------------
      //   double[] localDUL = DUL;
      //   double[] localLL15 = LL15;
      //   double[] CumThickness = SoilComponentUtility.ToCumThickness(Thickness);
      //   for (int i = 0; i != CumThickness.Length; i++)
      //      {
      //      if (CumThickness[i] > soilDepth)
      //         {
      //         double PreviousCumThickness = 0.0;
      //         if (i > 0)
      //            PreviousCumThickness = CumThickness[i - 1];

      //         if (PreviousCumThickness > soilDepth)
      //            {
      //            localLL15[i] = localDUL[i];
      //            }
      //         else
      //            {
      //            double Proportion = (soilDepth - PreviousCumThickness) / Thickness[i];
      //            localDUL[i] = (LL15[i] + ((localDUL[i] - LL15[i]) * Proportion));
      //            }
      //         }
      //      }
      //   DUL = localDUL;
      //   LL15 = localLL15;
      //   }
      //#endregion



      }
   }
