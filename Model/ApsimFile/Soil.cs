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
      public class Variable
         {
         public string Name;
         private string _Units;
         public string Value;
         private double[] _ThicknessMM;
         private double[] _Doubles;
         private string[] _Strings;
         public string[] Codes;
         private XmlNode _SoilNode;

         /// <summary>
         /// A simple constructor to create a property soil variable.
         /// </summary>
         public Variable(string name, string value)
            {
            Name = name;
            Value = value;
            }

         /// <summary>
         /// A constructor to create a soil variable with an array of doubles.
         /// </summary>
         public Variable(string name, string units, double[] Values, double[] Thickness, XmlNode SoilNode)
            {
            Name = name;
            _Units = units;
            _Doubles = Values;
            _ThicknessMM = Thickness;
            _SoilNode = SoilNode;
            if (_Units == null || _Units == "")
               _Units = SoilMetaData.Instance.DefaultUnits(Name);
            Soil.CheckUnits(Name, Units);
            }

         /// <summary>
         /// A constructor to create a soil variable with an array of strings.
         /// </summary>
         public Variable(string name, string units, string[] Values, double[] Thickness, XmlNode SoilNode)
            {
            Name = name;
            _Units = units;
            _Strings = Values;
            _ThicknessMM = Thickness;
            _SoilNode = SoilNode;
            if (_Units == null || _Units == "")
               _Units = SoilMetaData.Instance.DefaultUnits(Name);
            Soil.CheckUnits(Name, Units);
            }

         /// <summary>
         /// Constructor to create a variable from the XML under the specified ProfileNode.
         /// </summary>
         public Variable(XmlNode ProfileNode, string VariableName)
            {
            if (VariableName.Contains(" "))
               {
               // crop variable (e.g. wheat ll) - only keep the variable name after the space
               string[] VariableNameBits = VariableName.Split(" ".ToCharArray(), StringSplitOptions.RemoveEmptyEntries);
               Name = VariableNameBits[1];
               }
            else
               Name = VariableName;
            FindSoilNode(ProfileNode);

            // Check for a property first.
            if (XmlHelper.Find(ProfileNode, VariableName) != null)
               Value = XmlHelper.Value(ProfileNode, VariableName);
            else if (XmlHelper.Find(ProfileNode, "Layer/" + Name) == null)
               _Strings = new string[0];
            else
               {
               List<XmlNode> LayerNodes = XmlHelper.ChildNodes(ProfileNode, "Layer");
               _Strings = new string[LayerNodes.Count];
               _ThicknessMM = new double[LayerNodes.Count];
               Codes = new string[LayerNodes.Count];
               for (int i = 0; i < LayerNodes.Count; i++)
                  {
                  string ThicknessString = XmlHelper.Value(LayerNodes[i], "Thickness");
                  if (ThicknessString != "")
                     _ThicknessMM[i] = Convert.ToDouble(ThicknessString);
                  else
                     _ThicknessMM[i] = 0;
                  _Strings[i] = XmlHelper.Value(LayerNodes[i], Name);
                  XmlNode ValueNode = XmlHelper.Find(LayerNodes[i], Name);
                  if (ValueNode != null)
                     Codes[i] = XmlHelper.Attribute(ValueNode, "code");
                  else
                     Codes[i] = "";
                  if (i == 0 && ValueNode != null)
                     _Units = XmlHelper.Attribute(ValueNode, "units");
                  }
               Soil.CheckUnits(Name, Units);
               }
            }

         /// <summary>
         /// Constructor to create a variable from the specified Table beginning at StartRow.
         /// </summary>
         public Variable(DataTable Table, string ColumnName, int StartRow, double[] Thickness)
            {
            // Firstly work out how many rows (starting from StartRow) does the
            // soil go for.
            int NumRows = 1;
            string[] Names = DataTableUtility.GetColumnAsStrings(Table, "Name");
            if (Names.Length > 0)
               {
               for (int i = StartRow + 1; i < Names.Length; i++)
                  {
                  if (Names[i] == Names[StartRow])
                     NumRows++;
                  else
                     break;
                  }
               }
            else
               NumRows = Table.Rows.Count;

            Name = ColumnName;
            _Units = StringManip.SplitOffBracketedValue(ref Name, '(', ')');
            _ThicknessMM = Thickness;

            // get the values.
            if (Table.Columns[ColumnName].DataType == typeof(string))
               _Strings = DataTableUtility.GetColumnAsStrings(Table, ColumnName, NumRows, StartRow);
            else
               _Doubles = DataTableUtility.GetColumnAsDoubles(Table, ColumnName, NumRows, StartRow);

            // get the codes.
            if (Table.Columns.Contains(Name + "Code"))
               Codes = DataTableUtility.GetColumnAsStrings(Table, Name + "Code", NumRows, StartRow);
            Soil.CheckUnits(Name, Units);
            }

         /// <summary>
         /// Locate the parent soil node. Will throw if not found.
         /// </summary>
         private void FindSoilNode(XmlNode ProfileNode)
            {
            if (ProfileNode == null)
               throw new Exception("Cannot create soil variable " + Name + ". ProfileNode is null");

            _SoilNode = ProfileNode;
            while (_SoilNode != null && _SoilNode.Name.ToLower() != "soil")
               _SoilNode = _SoilNode.ParentNode;
            if (_SoilNode == null)
               throw new Exception("Cannot find parent soil node for soil variable " + Name);
            }

         /// <summary>
         /// Allow access to a variables units. Conversion of values is done when units are changed.
         /// </summary>
         public string Units
            {
            get
               {
               return _Units;
               }
            set
               {
               string ToUnits = value;
               if (Units != ToUnits && Value == null && MathUtility.ValuesAreNumerical(Strings))
                  {
                  if (Doubles == null)
                     throw new Exception("Cannot change the units on variable " + Name + ". No values were found.");

                  if (MathUtility.ValuesInArray(Doubles))
                     {
                     double[] NewValues = null;
                     if (Units == "mm/mm" && ToUnits == "mm")
                        NewValues = MathUtility.Multiply(Doubles, ThicknessMM);

                     else if (Units == "mm" && ToUnits == "mm/mm")
                        NewValues = MathUtility.Divide(Doubles, ThicknessMM);

                     else if (Units == "mm" && ToUnits == "cm")
                        NewValues = MathUtility.Divide_Value(Doubles, 10);

                     else if (Units == "cm" && ToUnits == "mm")
                        NewValues = MathUtility.Multiply_Value(Doubles, 10);

                     else if (Units == "mm/mm" && ToUnits == "grav. mm/mm")
                        {
                        Soil.Variable BD = Soil.Get(_SoilNode, "BD");
                        BD.Units = "g/cc";
                        BD.ThicknessMM = ThicknessMM;
                        NewValues = MathUtility.Divide(Doubles, BD.Doubles);
                        }

                     else if (Units == "grav. mm/mm" && ToUnits == "mm/mm")
                        {
                        Soil.Variable BD = Soil.Get(_SoilNode, "BD");
                        BD.Units = "g/cc";
                        BD.ThicknessMM = ThicknessMM;
                        NewValues = MathUtility.Multiply(Doubles, BD.Doubles);
                        }

                     else if (Units == "Walkley Black %" && ToUnits == "Total %")
                        NewValues = MathUtility.Multiply_Value(Doubles, 1.3);

                     else if (Units == "Total %" && ToUnits == "Walkley Black %")
                        NewValues = MathUtility.Divide_Value(Doubles, 1.3);

                     else if (Units == "kg/ha" && ToUnits == "ppm")
                        {
                        Soil.Variable BD = Soil.Get(_SoilNode, "BD");
                        BD.Units = "g/cc";
                        BD.ThicknessMM = ThicknessMM;
                        NewValues = new double[Doubles.Length];
                        for (int i = 0; i < Doubles.Length; i++)
                           {
                           if (Doubles[i] != MathUtility.MissingValue)
                              NewValues[i] = Doubles[i] * 100 / (BD.Doubles[i] * ThicknessMM[i]);
                           }
                        }
                     else if ( (Units == "ppm" || Units == "mg/kg") && ToUnits == "kg/ha")
                        {
                        Soil.Variable BD = Soil.Get(_SoilNode, "BD");
                        BD.Units = "g/cc";
                        BD.ThicknessMM = ThicknessMM;
                        NewValues = new double[Doubles.Length];
                        for (int i = 0; i < Doubles.Length; i++)
                           {
                           if (Doubles[i] != MathUtility.MissingValue)
                              NewValues[i] = Doubles[i] / 100 * (BD.Doubles[i] * ThicknessMM[i]);
                           }
                        }

                     else if (Units == "CaCl2" && ToUnits == "1:5 water")
                        {
                        // pH in water = (pH in CaCl X 1.1045) - 0.1375
                        NewValues = MathUtility.Subtract_Value(MathUtility.Multiply_Value(Doubles, 1.1045), 0.1375);
                        }

                     else if (Units == "1:5 water" && ToUnits == "CaCl2")
                        {
                        // pH in CaCl = (pH in water + 0.1375) / 1.1045
                        NewValues = MathUtility.Divide_Value(MathUtility.Add_Value(Doubles, 0.1375), 1.1045);
                        }

                     else
                        throw new Exception("Cannot convert units from " + Units + " to " + ToUnits + " for variable " + Name);

                     _Doubles = NewValues;
                     _Strings = null;
                     }
                  }
               _Units = ToUnits;
               }
            }

         /// <summary>
         /// Return the values of this variable as doubles.
         /// </summary>
         public double[] Doubles
            {
            get
               {
               if (_Doubles != null)
                  return _Doubles;
               else if (_Strings != null)
                  {
                  double[] Values = new double[_Strings.Length];
                  for (int i = 0; i < _Strings.Length; i++)
                     {
                     if (_Strings[i] == "")
                        Values[i] = MathUtility.MissingValue;
                     else
                        Values[i] = Convert.ToDouble(_Strings[i]);
                     }
                  return Values;
                  }
               else
                  throw new Exception("Cannot return doubles for variable " + Name + ". No values found.");
               }
            }

         /// <summary>
         /// Return the values of this variable as strings.
         /// </summary>
         public string[] Strings
            {
            get
               {
               if (_Strings != null)
                  return _Strings;
               else if (_Doubles != null)
                  {
                  string[] Values = new string[_Doubles.Length];
                  for (int i = 0; i < _Doubles.Length; i++)
                     {
                     if (_Doubles[i] == MathUtility.MissingValue)
                        Values[i] = "";
                     else
                        Values[i] = _Doubles[i].ToString();
                     }
                  return Values;
                  }
               else
                  throw new Exception("Cannot return string values for variable " + Name + ". No values found.");
               }
            }

         public double[] ThicknessMM
            {
            get
               {
               return _ThicknessMM;
               }
            set
               {
               double[] ToThickness = value;
               if (!MathUtility.AreEqual(ToThickness, ThicknessMM))
                  {
                  _Doubles = MapToTarget(Name, Units, Doubles, _ThicknessMM, ToThickness, _SoilNode);
                  _Strings = null;

                  Constrain(_Doubles, ToThickness);
                  _ThicknessMM = ToThickness;
                  if (Codes == null)
                     Codes = new string[_ThicknessMM.Length];
                  if (Codes.Length != _ThicknessMM.Length)
                     Array.Resize(ref Codes, _ThicknessMM.Length);

                  for (int i = 0; i < Codes.Length; i++)
                     {
                     if (Codes[i] != "")
                        Codes[i] += " and Mapped";
                     else
                        Codes[i] = "Mapped";
                     }
                  }
               }
            }

         /// <summary>
         /// Constrain the values passin according to bounds specified in the 
         /// metadata for this variable
         /// </summary>
         private double[] Constrain(double[] Values, double[] Thickness)
            {
            string VariableName = Name;
            if (Name.Contains(" "))
               VariableName = VariableName.Substring(VariableName.IndexOf(' ') + 1);
            double[] LowerBounds, UpperBounds;
            string LowerBoundSt = SoilMetaData.Instance.MetaData(Name, "LowerBound");
            if (LowerBoundSt != "")
               {
               LowerBounds = GetBounds(Values, LowerBoundSt, Thickness);
               if (LowerBounds != null)
                  for (int i = 0; i < Values.Length; i++)
                     Values[i] = Math.Max(LowerBounds[i], Values[i]);
               }
            string UpperBoundSt = SoilMetaData.Instance.MetaData(Name, "UpperBound");
            if (UpperBoundSt != "")
               {
               UpperBounds = GetBounds(Values, UpperBoundSt, Thickness);
               if (UpperBounds != null)
                  for (int i = 0; i < Values.Length; i++)
                     Values[i] = Math.Min(UpperBounds[i], Values[i]);
               }
            return Values;
            }

         /// <summary>
         /// Retrieve an array of bounds for this variable using the metadata string passed in.
         /// </summary>
         private double[] GetBounds(double[] Values, string BoundSt, double[] Thickness)
            {
            double[] Bounds = null;
            if (MathUtility.IsNumerical(BoundSt))
               {
               Bounds = new double[Values.Length];
               for (int i = 0; i < Values.Length; i++)
                  Bounds[i] = double.Parse(BoundSt);
               }
            else
               {
               Soil.Variable Var = Soil.GetOptional(_SoilNode, BoundSt);
               Var.ThicknessMM = Thickness;
               if (Var != null)
                  Bounds = Var.Doubles;
               }
            return Bounds;
            }

         /// <summary>
         /// Write this variable to the specified XML Node.
         /// </summary>
         public void WriteTo(XmlNode ProfileNode)
            {
            string ChildName = Name;
            // remove the crop part of the variable name if it exists.
            if (ChildName.Contains(" "))
               {
               string CropName = ChildName.Substring(0, ChildName.LastIndexOf(' '));
               ChildName = ChildName.Substring(ChildName.LastIndexOf(' ') + 1);
               if (ProfileNode.Name == "Water")
                  {
                  XmlNode CropNode = XmlHelper.Find(ProfileNode, CropName);
                  if (CropNode == null)
                     CropNode = ProfileNode.AppendChild(ProfileNode.OwnerDocument.CreateElement("SoilCrop"));
                  ProfileNode = CropNode;
                  }
               }
            if (Value != null)
               XmlHelper.SetValue(ProfileNode, ChildName, Value);
            else
               {
               _ThicknessMM = MathUtility.RemoveMissingValuesFromBottom(_ThicknessMM);

               // The number of thickness numbers determines how many layers we have.
               XmlHelper.EnsureNumberOfChildren(ProfileNode, "Layer", "", ThicknessMM.Length);

               List<XmlNode> Layers = XmlHelper.ChildNodes(ProfileNode, "Layer");
               string[] Values = Strings;
               for (int i = 0; i != ThicknessMM.Length; i++)
                  {
                  // Give this layer a thickness.
                  XmlHelper.SetValue(Layers[i], "Thickness", ThicknessMM[i].ToString());
                  XmlNode ThicknessNode = XmlHelper.Find(Layers[i], "Thickness");
                  if (i == 0)
                     XmlHelper.SetAttribute(ThicknessNode, "units", "mm");

                  // Give this layer a value node.
                  if (i >= Values.Length)
                     XmlHelper.SetValue(Layers[i], ChildName, "");
                  else
                     XmlHelper.SetValue(Layers[i], ChildName, Values[i]);
                  XmlNode ValueNode = XmlHelper.Find(Layers[i], ChildName);

                  if (ValueNode != null)
                     {
                     // Assign a code to our value.
                     if (Codes != null && i < Codes.Length)
                        XmlHelper.SetAttribute(ValueNode, "code", Codes[i]);
                     else
                        XmlHelper.DeleteAttribute(ValueNode, "code"); // remove old code

                     // Put a unit on the first value node.
                     if (i == 0)
                        XmlHelper.SetAttribute(ValueNode, "units", Units);
                     }
                  }
               }
            }


         /// <summary>
         /// Write this variable to the specified data table, adding a new column if necessary.
         /// </summary>
         public void WriteTo(DataTable Table, int StartRow)
            {
            // Work out what the table column name should be.
            string TableColumnName = Name;
            if (Units != null && Units != "")
               TableColumnName += " (" + Units + ")";

            if (Value != null)
               DataTableUtility.AddValue(Table, TableColumnName, Value, StartRow, ThicknessMM.Length);
            else if (MathUtility.ValuesAreNumerical(Strings))
               DataTableUtility.AddColumn(Table, TableColumnName, Doubles, StartRow, ThicknessMM.Length);
            else
               DataTableUtility.AddColumn(Table, TableColumnName, Strings, StartRow, ThicknessMM.Length);
            }


         /// <summary>
         /// Map the values/thickness passed in, into the target layer 
         /// thicknesses. Uses the variable name to determine the method.
         /// </summary>
         private static double[] MapToTarget(string VariableName, string Units,
                                             double[] Values, double[] FromThickness,
                                             double[] ToThickness, XmlNode SoilNode)
            {
            if (!MathUtility.ValuesInArray(Values))
               return MathUtility.CreateArrayOfValues(MathUtility.MissingValue, ToThickness.Length);
            else if (VariableName == "Thickness")
               return ToThickness;
            else if (Units == "kg/ha")
               {
               double[] DefaultValues = new double[ToThickness.Length];
               for (int i = 0; i < ToThickness.Length; i++)
                  DefaultValues[i] = 0;
               CreateVariableForMapping(ref Values, ref FromThickness, DefaultValues, ToThickness);
               return MassRedistributeInternal(Values, FromThickness, ToThickness, SoilNode);
               }
            else
               {
               double[] DefaultValues = new double[ToThickness.Length];
               for (int i = 0; i < ToThickness.Length; i++)
                  DefaultValues[i] = 0;
               CreateVariableForMapping(ref Values, ref FromThickness, DefaultValues, ToThickness);
               Values = MathUtility.Multiply(Values, FromThickness);
               Values = SpatialRedistributeInternal(Values, FromThickness, ToThickness);
               return MathUtility.Divide(Values, ToThickness);
               }
            }

         /// <summary>
         /// Spatial mass redistribution algorithm.
         /// </summary>
         private static double[] SpatialRedistributeInternal(double[] FromMass, double[] FromThickness, double[] ToThickness)
            {
            if (FromMass.Length != FromThickness.Length)
               {
               throw new Exception("Cannot redistribute soil sample layer structure to soil layer structure. " +
                                   "The number of values in the sample doesn't match the number of layers in the sample.");
               }

            // Remapping is achieved by first constructing a map of
            // cumulative mass vs depth
            // The new values of mass per layer can be linearly
            // interpolated back from this shape taking into account
            // the rescaling of the profile.

            double[] CumDepth = new double[FromMass.Length + 1];
            double[] CumMass = new double[FromMass.Length + 1];
            CumDepth[0] = 0.0;
            CumMass[0] = 0.0;
            for (int Layer = 0; Layer < FromThickness.Length; Layer++)
               {
               CumDepth[Layer + 1] = CumDepth[Layer] + FromThickness[Layer];
               CumMass[Layer + 1] = CumMass[Layer] + FromMass[Layer];
               }

            //look up new mass from interpolation pairs
            double[] ToMass = new double[ToThickness.Length];
            for (int Layer = 1; Layer <= ToThickness.Length; Layer++)
               {
               double LayerBottom = MathUtility.Sum(ToThickness, 0, Layer, 0.0);
               double LayerTop = LayerBottom - ToThickness[Layer - 1];
               bool DidInterpolate;
               double CumMassTop = MathUtility.LinearInterpReal(LayerTop, CumDepth,
                   CumMass, out DidInterpolate);
               double CumMassBottom = MathUtility.LinearInterpReal(LayerBottom, CumDepth,
                   CumMass, out DidInterpolate);
               ToMass[Layer - 1] = CumMassBottom - CumMassTop;
               }
            return ToMass;
            }

         /// <summary>
         /// Mass Redistribution algorithm
         /// </summary>
         private static double[] MassRedistributeInternal(double[] FromValues, double[] FromThickness,
                                                          double[] ToThickness, XmlNode SoilNode)
            {
            // Firstly we need to convert the values passed in, into a mass using
            // bulk density.

            Soil.Variable BD = Soil.Get(SoilNode, "BD");
            BD.Units = "g/cc";
            BD.ThicknessMM = FromThickness;

            double[] FromMass = new double[FromValues.Length];
            for (int Layer = 0; Layer < FromValues.Length; Layer++)
               FromMass[Layer] = FromValues[Layer] * BD.Doubles[Layer] * FromThickness[Layer] / 100;

            // spatially interpolate mass.
            double[] ToMass = SpatialRedistributeInternal(FromMass, FromThickness, ToThickness);

            // Now map the BD to our target layer structure.
            BD.ThicknessMM = ToThickness;

            //now convert mass back to original values.
            double[] ToValues = new double[ToMass.Length];
            for (int Layer = 0; Layer < ToMass.Length; Layer++)
               ToValues[Layer] = ToMass[Layer] * 100.0 / BD.Doubles[Layer] / ToThickness[Layer];

            return ToValues;
            }

         /// <summary>
         /// Remaps the thicknesses and values to more closely match the specified 
         /// soil thickness and values. This algorithm removes all missing values
         /// and their associated depths.   
         /// </summary>
         private static void CreateVariableForMapping(ref double[] SampleValues, ref double[] SampleThickness,
                                                      double[] SoilValues, double[] SoilThickness)
            {
            //-------------------------------------------------------------------------
            //  e.g. IF             SoilThickness  Values   SampleThickness	SampleValues
            //                           0-100		2         0-100				10
            //                         100-250	   3	     100-600				11
            //                         250-500		4		
            //                         500-750		5
            //                         750-900		6
            //						         900-1200		7
            //                        1200-1500		8
            //                        1500-1800		9
            //
            // will produce:		SampleThickness	Values
            //						     0-100				  10
            //						   100-600				  11
            //						   600-750				   5
            //						   750-900				   6
            //						   900-1200				   7
            //						  1200-1500				   8
            //						  1500-1800				   9
            //
            //-------------------------------------------------------------------------
            double[] ReturnThickness = new double[SampleThickness.Length + SoilThickness.Length + 1];
            double[] ReturnValues = new double[SampleThickness.Length + SoilThickness.Length + 1];

            // Copy values and thicknesses to return arrays until a missing value is found.
            double CumSampleDepth = 0.0;
            int SampleLayer = 0;
            for (SampleLayer = 0; ((SampleLayer != SampleThickness.Length) && (double)SampleValues[SampleLayer] != MathUtility.MissingValue); SampleLayer++)
               {
               ReturnThickness[SampleLayer] = SampleThickness[SampleLayer];
               ReturnValues[SampleLayer] = SampleValues[SampleLayer];
               CumSampleDepth += (double)SampleThickness[SampleLayer];
               }

            //Work out if we need to create a dummy layer so that the sample depths line up 
            //with the soil depths
            double CumSoilDepth = 0.0;
            for (int SoilLayer = 0; SoilLayer < SoilThickness.Length; SoilLayer++)
               {
               CumSoilDepth += SoilThickness[SoilLayer];
               if (CumSoilDepth > CumSampleDepth)
                  {
                  ReturnThickness[SampleLayer] = CumSoilDepth - CumSampleDepth;
                  ReturnValues[SampleLayer] = SoilValues[SoilLayer];
                  SampleLayer++;
                  CumSampleDepth = CumSoilDepth;
                  }
               }

            // Copy Values from our return arrays back to the parameters passed in.
            SampleThickness = new double[SampleLayer];
            SampleValues = new double[SampleLayer];
            for (int i = 0; i != SampleLayer; i++)
               {
               SampleThickness[i] = ReturnThickness[i];
               SampleValues[i] = ReturnValues[i];
               }
            }
         }

      //////////////////////////////////////////////////////////////////////////////////////////////
      //////////////////////////////////////////////////////////////////////////////////////////////
      //////////////////////////////////////////////////////////////////////////////////////////////
      

      private const double ppm = 1000000.0;

      private Soil() { } // Don't allow anyone to create a soil object.

      /// <summary>
      /// Create an empty soil object with the specified name.
      /// </summary>
      public static XmlNode Create(string Name)
         {
         System.Reflection.Assembly thisExe = System.Reflection.Assembly.GetExecutingAssembly();
         System.IO.Stream file = thisExe.GetManifestResourceStream("ApsimFile.Resources.Blank.soil");
         StreamReader In = new StreamReader(file);
         string XML = In.ReadToEnd();
         In.Close();
         XmlNode NewSoil = Soil.CreateFromXML(XML);
         XmlHelper.SetName(NewSoil, Name);
         return NewSoil;
         }


      /// <summary>
      /// Create a soil from XML
      /// </summary>
      public static XmlNode CreateFromXML(string XML)

         {
         XmlDocument Doc = new XmlDocument();
         Doc.LoadXml(XML);
         return Doc.DocumentElement;
         }

      /// <summary>
      /// Loop through all soil macros (e.g. [soil.ll15]) and replace them 
      /// with a value. Will throw if macro is invalid.
      /// </summary>
      public static string ReplaceSoilMacros(XmlNode SoilNode, string Str)
         {
         Macro m = new Macro();
         Str = m.ParseForEach(SoilNode, Str);

         // Calculate the layer structure we need to have all our variables in.
         double[] Thickness;
         string UserThickness = XmlHelper.Value(SoilNode, "Thickness/Values");
         if (UserThickness != "")
            {
            string[] Values = UserThickness.Split(" ".ToCharArray(), StringSplitOptions.RemoveEmptyEntries);
            Thickness = MathUtility.StringsToDoubles(Values);
            }
         else
            {
            Soil.Variable LL15 = Soil.Get(SoilNode, "LL15");
            Thickness = LL15.ThicknessMM;
            }

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
                     MacroValue = XmlHelper.Name(SoilNode);

                  else
                     {
                     string VariableName = MacroBits[1];
                     string Units = StringManip.SplitOffBracketedValue(ref VariableName, '(', ')');

                     Soil.Variable Var = Get(SoilNode, VariableName);
                     if (Var.Value != null)
                        MacroValue = Var.Value;  // property
                     else
                        {
                        if (Units != "")
                           Var.Units = Units;

                        Var.ThicknessMM = Thickness;
                        double[] DoubleValues = Var.Doubles;
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

      /// <summary>
      /// Look for specified variable under the specified soil node. Will throw if
      /// not found or if variable was found more than once.
      /// </summary>
      public static Soil.Variable Get(XmlNode SoilNode, string VariableName)
         {
         Soil.Variable Var = GetCalculated(SoilNode, VariableName);
         if (Var == null)
            {
            XmlNode ParentNode = FindVariableParent(SoilNode, VariableName, false);
            return new Soil.Variable(ParentNode, VariableName);
            }
         else
            return Var;
         }

      /// <summary>
      /// Optionally look for specified variable under the specified soil node. Will return null if
      /// not found.
      /// </summary>
      public static Soil.Variable GetOptional(XmlNode SoilNode, string VariableName)
         {
         Soil.Variable Var = null;
         try
            {
            Var = GetCalculated(SoilNode, VariableName);
            }
         catch (Exception)
            { }
         if (Var == null)
            {
            XmlNode ParentNode = FindVariableParentOptional(SoilNode, VariableName, false);
            if (ParentNode == null)
               return null;
            else
               return new Soil.Variable(ParentNode, VariableName);
            }
         else
            return Var;
         }

      /// <summary>
      /// Optionally look for specified variable under the specified soil node. Will return null if
      /// not found.
      /// </summary>
      public static Soil.Variable GetOptionalFromProfileNode(XmlNode SoilNode, XmlNode ProfileNode, string VariableName)
         {
         Soil.Variable Var = new Soil.Variable(ProfileNode, VariableName);
         if (Var.Strings.Length == 0)
            return GetCalculated(SoilNode, VariableName);
         else
            return Var;
         }

      
      /// <summary>
      ///  Return a parent node for where the specified variable belongs. Will throw 
      ///  if not found or if there are more than one found.
      /// </summary>
      private static XmlNode FindVariableParentOptional(XmlNode SoilNode, string VariableName, bool CreateCropVariablesIfNecessary)
         {
         List<XmlNode> AllNodes = new List<XmlNode>();

         // If the variable name has a space in it then it is a crop.
         if (VariableName.Contains(" "))
            {
            string[] VariableNameBits = VariableName.Split(" ".ToCharArray(), StringSplitOptions.RemoveEmptyEntries);
            if (VariableNameBits.Length != 2)
               throw new Exception("Unknown soil variable " + VariableName);

            XmlNode WaterNode = XmlHelper.Find(SoilNode, "Water");
            if (WaterNode != null)
               {
               XmlHelper.FindAllRecursively(WaterNode, VariableNameBits[0], ref AllNodes);
               if (AllNodes.Count == 1)
                  {
                  XmlNode CropNode = AllNodes[0];
                  AllNodes.Clear();
                  XmlHelper.FindAllRecursively(CropNode, VariableNameBits[1], ref AllNodes);
                  }
               if (AllNodes.Count == 0)
                  {
                  if (CreateCropVariablesIfNecessary)
                     {
                     XmlNode CropNode = WaterNode.AppendChild(WaterNode.OwnerDocument.CreateElement("SoilCrop"));
                     XmlHelper.SetName(CropNode, VariableNameBits[0]);
                     XmlHelper.SetAttribute(XmlHelper.EnsureNodeExists(CropNode, "Layer/Thickness"), "units", "mm");
                     XmlHelper.SetAttribute(XmlHelper.EnsureNodeExists(CropNode, "Layer/ll"), "units", "mm/mm");
                     XmlHelper.SetAttribute(XmlHelper.EnsureNodeExists(CropNode, "Layer/kl"), "units", "/day");
                     XmlHelper.SetAttribute(XmlHelper.EnsureNodeExists(CropNode, "Layer/xf"), "units", "0-1");

                     return CropNode;
                     }
                  }
               }
            }
         else
            XmlHelper.FindAllRecursively(SoilNode, VariableName, ref AllNodes);

         // Now go through all nodes and look for unique parent nodes.
         List<XmlNode> ParentNodes = new List<XmlNode>();
         foreach (XmlNode Node in AllNodes)
            {
            if (Node.ParentNode != null)
               {
               if (Node.ParentNode.Name == "Layer")
                  {
                  // Its a profile node - add to our list if it isn't alreay added
                  if (ParentNodes.IndexOf(Node.ParentNode.ParentNode) == -1)
                     ParentNodes.Add(Node.ParentNode.ParentNode);
                  }
               else
                  ParentNodes.Add(Node.ParentNode); // not a profile node.
               }
            }
         if (ParentNodes.Count > 1)
            {
            // go through all parent nodes and remove the ones that have blank values
            // until we have just one left.
            for (int i = ParentNodes.Count - 1; i >= 0;  i--)
               {
               if (XmlHelper.Value(ParentNodes[i], "Layer/" + VariableName) == "")
                  {
                  ParentNodes.RemoveAt(i);
                  i--;
                  if (ParentNodes.Count == 1)
                     break;
                  }
               }
            }
         if (ParentNodes.Count > 1)
            {
            // Go through all nodes and try and try and find a sample node. If found then use it and get
            // rid of the others.
            for (int i = 0; i < ParentNodes.Count; i++)
               {
               if (ParentNodes[i].Name == "Sample")
                  {
                  XmlNode SampleNode = ParentNodes[i];
                  ParentNodes.Clear();
                  ParentNodes.Add(SampleNode);
                  }
               }
            }

         if (ParentNodes.Count > 1)
            throw new Exception("Found variable " + VariableName + " " + ParentNodes.Count.ToString() +
                                " times in soil " + XmlHelper.Name(SoilNode) + ". Only expecting 1 occurrence.");
         else if (ParentNodes.Count == 0)
            return null;
         else
            return ParentNodes[0];
         }

      private static XmlNode FindVariableParent(XmlNode SoilNode, string VariableName, bool CreateCropVariablesIfNecessary)
         {
         XmlNode Parent = FindVariableParentOptional(SoilNode, VariableName, CreateCropVariablesIfNecessary);
         if (Parent == null)
            throw new Exception("Invalid soil variable " + VariableName + " in soil " + XmlHelper.Name(SoilNode));
         else
            return Parent;
         }

      /// <summary>
      /// Look for specified variable under the specified soil node. Will throw if
      /// not found or if variable was found more than once.
      /// </summary>
      public static void Set(XmlNode SoilNode, Soil.Variable Value)
         {
         XmlNode ParentNode = FindVariableParent(SoilNode, Value.Name, true);
         if (Value.Name.Contains(" "))
            {
            // Crop variable - only interested in second part of name i.e. after crop name.
            Value.Name = Value.Name.Substring(Value.Name.IndexOf(' ') + 1);
            }
         Value.WriteTo(ParentNode);
         }


      /// <summary>
      /// Fill the specified table with data from the specified soil.
      /// </summary>
      public static void WriteToTable(XmlNode SoilNode, DataTable Table, List<string> VariableNames)
         {
         // Find a thickness.
         Soil.Variable Thickness = null;
         for (int i = 0; i < VariableNames.Count; i++)
            {
            if (!VariableNames[i].Contains("Thickness") &&
                !VariableNames[i].Contains("Depth"))
               {
               string Name = VariableNames[i];
               /*string Units = */ StringManip.SplitOffBracketedValue(ref Name, '(', ')');

               Soil.Variable Var = Soil.Get(SoilNode, Name);
               if (Var.ThicknessMM != null && Var.Value == null)
                  {
                  Thickness = new Soil.Variable("Thickness", "mm", Var.ThicknessMM, Var.ThicknessMM, SoilNode);
                  break;
                  }
               }
            }
         if (Thickness == null)
            throw new Exception("Cannot find a thickness column in table.");

         // Loop through all variables and write each to the table.
         foreach (string VariableName in VariableNames)
            {
            string Name = VariableName;
            string Units = StringManip.SplitOffBracketedValue(ref Name, '(', ')');

            Soil.Variable Var;
            if (Name == "Thickness")
               {
               Thickness.Units = Units;
               Var = Thickness;
               }
            else if (Name == "Depth")
               {
               Thickness.Units = Units;
               Var = new Soil.Variable("Depth", Units,
                                      SoilUtility.ToDepthStrings(Thickness.Doubles),
                                      Thickness.ThicknessMM, SoilNode);
               }
            else if (Name == "DepthMidPoints")
               {
               Thickness.Units = Units;
               Var = new Soil.Variable("DepthMidPoints", Units,
                                      SoilUtility.ToMidPoints(Thickness.Doubles),
                                      Thickness.ThicknessMM, SoilNode);
               }
            else
               {
               Var = Soil.Get(SoilNode, Name);
               Var.Units = Units;
               if (Var.ThicknessMM != null)
                  Var.ThicknessMM = Thickness.ThicknessMM; // mapping may occur!

               // Crop variables will have a Var.Name of LL, KL etc without the name of the crop.
               // The "Name" local variable above will have the name of the crop so set the name of the 
               // variable back to the local Name variable.
               Var.Name = Name;
               }

            if (Var.Value != null) 
               DataTableUtility.AddValue(Table, Name, 
                                         Var.Value, 0, Thickness.Doubles.Length);  // property.
            else
               Var.WriteTo(Table, 0);   // layered variable.
            }
         }
      /// <summary>
      /// Fill the specified table with data from the specified profile node.
      /// </summary>
      public static void WriteToTableFromProfileNode(XmlNode ProfileNode, DataTable Table, List<string> VariableNames)
         {
         // Find a thickness.
         Soil.Variable Thickness = null;
         for (int i = 0; i < VariableNames.Count; i++)
            {
            if (!VariableNames[i].Contains("Thickness") &&
                !VariableNames[i].Contains("Depth"))
               {
               string Name = VariableNames[i];
               //string Units = StringManip.SplitOffBracketedValue(ref Name, '(', ')');

               Soil.Variable Var = new Soil.Variable(ProfileNode, Name); 
               if (Var.ThicknessMM != null && Var.Value == null)
                  {
                  Thickness = new Soil.Variable("Thickness", "mm", Var.ThicknessMM, Var.ThicknessMM, null);
                  break;
                  }
               }
            }
         if (Thickness == null)
            throw new Exception("Cannot find a thickness column in table.");

         // Loop through all variables and write each to the table.
         foreach (string VariableName in VariableNames)
            {
            string Name = VariableName;
            string Units = StringManip.SplitOffBracketedValue(ref Name, '(', ')');

            Soil.Variable Var;
            if (Name == "Thickness")
               {
               Thickness.Units = Units;
               Var = Thickness;
               }
            else if (Name == "Depth")
               {
               Thickness.Units = Units;
               Var = new Soil.Variable("Depth", Units,
                                      SoilUtility.ToDepthStrings(Thickness.Doubles),
                                      Thickness.ThicknessMM, null);
               }
            else if (Name == "DepthMidPoints")
               {
               Thickness.Units = Units;
               Var = new Soil.Variable("DepthMidPoints", Units,
                                      SoilUtility.ToMidPoints(Thickness.Doubles),
                                      Thickness.ThicknessMM, null);
               }
            else
               {
               Var = new Soil.Variable(ProfileNode, Name); 
               Var.Units = Units;
               if (Var.ThicknessMM != null)
                  Var.ThicknessMM = Thickness.ThicknessMM; // mapping may occur!

               // Crop variables will have a Var.Name of LL, KL etc without the name of the crop.
               // The "Name" local variable above will have the name of the crop so set the name of the 
               // variable back to the local Name variable.
               Var.Name = Name;
               }

            if (Var.Value != null)
               DataTableUtility.AddValue(Table, Name,
                                         Var.Value, 0, Thickness.Doubles.Length);  // property.
            else
               Var.WriteTo(Table, 0);   // layered variable.
            }
         }


      /// <summary>
      /// Read in all columns from the specified table
      /// </summary>
      public static void ReadFromTable(XmlNode SoilNode, DataTable Table)
         {
         foreach (DataColumn Column in Table.Columns)
            {
            if (!Column.ColumnName.Contains("Thickness") &&
                !Column.ColumnName.Contains("Depth") &&
                !Column.ColumnName.Contains("DepthMidPoints"))
               ReadFromTable(SoilNode, null, Table, Column.ColumnName);
            }
         }
      /// <summary>
      /// Read in a column from the specified table
      /// </summary>
      public static void ReadFromTable(XmlNode SoilNode, XmlNode ProfileNode, DataTable Table, string ColumnName)
         {
         // Find a thickness.
         Soil.Variable Thickness = null;
         foreach (DataColumn Column in Table.Columns)
            {
            string ColName = Column.ColumnName;
            string ColUnits = StringManip.SplitOffBracketedValue(ref ColName, '(', ')');
            if (ColName == "Depth")
               {
               string[] DepthStrings = DataTableUtility.GetColumnAsStrings(Table, Column.ColumnName);
               double[] Values = SoilUtility.ToThickness(DepthStrings);
               Thickness = new Soil.Variable("Thickness", ColUnits, Values, Values, SoilNode);
               break;
               }
            else if (ColName == "Thickness")
               {
               double[] Values = DataTableUtility.GetColumnAsDoubles(Table, Column.ColumnName);
               Thickness = new Soil.Variable("Thickness", ColUnits, Values, Values, SoilNode);
               break;
               }
            }
         if (Thickness == null)
            throw new Exception("Cannot find a thickness column in table.");
         Thickness.Units = "mm";

         string VariableName = ColumnName;
         string Units = StringManip.SplitOffBracketedValue(ref VariableName, '(', ')');

         // Go look to see if there is an existing variable. If so, then the destination
         // units should be the same as the existing variable.
         Soil.Variable OldVar;
         if (ProfileNode == null)
            OldVar = Soil.GetOptional(SoilNode, VariableName);
         else
            OldVar = Soil.GetOptionalFromProfileNode(SoilNode, ProfileNode, VariableName);
         if (OldVar != null && Units == "")
            Units = OldVar.Units;

         string UnitsForNewVariable = Units;
         if (OldVar != null)
            UnitsForNewVariable = OldVar.Units;

         Soil.Variable Var;
         string[] StringValues = DataTableUtility.GetColumnAsStrings(Table, ColumnName);
         if (MathUtility.ValuesAreNumerical(StringValues))
            {
            double[] Values = MathUtility.StringsToDoubles(StringValues);
            Var = new Soil.Variable(VariableName, UnitsForNewVariable, Values, Thickness.Doubles, SoilNode);
            }
         else
            Var = new Soil.Variable(VariableName, UnitsForNewVariable, StringValues, Thickness.Doubles, SoilNode);

         // Now change the unit to the new ones. This will change the values if necessary.
         Var.Units = Units;

         if (OldVar == null)
            Var.Codes = StringManip.CreateStringArray("", Var.Strings.Length);
         else
            SetCodesInVar(Var, OldVar);

         if (ProfileNode == null)
            ProfileNode = FindVariableParent(SoilNode, VariableName, true);
         Var.WriteTo(ProfileNode);


         }

      /// <summary>
      /// Add a codes column into the specified Var. It does this by comparing the specified
      /// var with the one already in the soil with the same name.
      /// </summary>
      private static void SetCodesInVar(Soil.Variable Var, Soil.Variable OldVar)
         {
         OldVar.Units = Var.Units;
         
         string[] OldValues = OldVar.Strings;
         string[] NewValues = Var.Strings;
         Var.Codes = new string[NewValues.Length];
         for (int i = 0; i < NewValues.Length; i++)
            {
            bool ValueIdentical = false;
            if (i < OldValues.Length)
               {
               // Do a string comparison first of all.
               ValueIdentical = OldValues[i] == NewValues[i];

               // Now try and do a numerical comparison.
               double OldValue;
               if (Double.TryParse(OldValues[i], out OldValue))
                  {
                  double NewValue;
                  if (Double.TryParse(NewValues[i], out NewValue))
                     ValueIdentical = MathUtility.FloatsAreEqual(OldValue, NewValue);                     
                  }
               }
            if (ValueIdentical && OldVar.Codes != null && i < OldVar.Codes.Length)
               Var.Codes[i] = OldVar.Codes[i];
            else
               Var.Codes[i] = "";
            }
         }

      /// <summary>
      /// Try and return a calculated variable. Returns null if not found.
      /// </summary>
      private static Soil.Variable GetCalculated(XmlNode SoilNode, string VariableName)
         {
         if (VariableName == "Thickness" || VariableName == "Depth" || VariableName == "DepthMidPoints")
            {
            Soil.Variable Var = Soil.Get(SoilNode, "LL15");
            if (VariableName == "Thickness")
               return new Variable(VariableName, "mm", Var.ThicknessMM, Var.ThicknessMM, SoilNode);
            if (VariableName == "Depth")
               return new Variable(VariableName, "mm", SoilUtility.ToDepthStrings(Var.ThicknessMM), Var.ThicknessMM, SoilNode);
            if (VariableName == "DepthMidPoints")
               return new Variable(VariableName, "mm", SoilUtility.ToMidPoints(Var.ThicknessMM), Var.ThicknessMM, SoilNode);
            }

         Soil.Variable Value = SWFromInitWater(SoilNode, VariableName);

         // If not then try for a calculated variable.
         if (Value == null)
            Value = CalculatedVariables(SoilNode, VariableName);

         // If not SW then try for a crop variable.
         if (Value == null)
            Value = CropVariable(SoilNode, VariableName);
         return Value;
         }


      /// <summary>
      /// Return a list of valid variables for the specified NodeName (eg. water).
      /// </summary>
      public static List<string> ValidVariablesForProfileNode(XmlNode ProfileNode)
         {
         List<XmlNode> LayerNodes = XmlHelper.ChildNodes(ProfileNode, "Layer");
         if (LayerNodes.Count > 0)
            {
            List<string> Names = new List<string>();
            foreach (XmlNode Child in LayerNodes[0].ChildNodes)
               {
               string VariableName = Child.Name;
               string Units = XmlHelper.Attribute(Child, "units");
               if (Units != "")
                  VariableName += " (" + Units + ")";
               Names.Add(VariableName);
               }
            return Names;
            }
         return null;
         }

      /// <summary>
      /// Return valid units for the specified variable.
      /// </summary>
      public static List<string> ValidUnits(string VariableName)
         {
         return SoilMetaData.Instance.ValidUnits(VariableName);
         }

      /// <summary>
      /// Check to make sure units are valid for the specified variable.
      /// </summary>
      /// <param name="Name"></param>
      /// <param name="Units"></param>
      public static void CheckUnits(string Name, string Units)
         {
         List<string> AllowedUnits = Soil.ValidUnits(Name);
         if (AllowedUnits.Count > 0 && StringManip.IndexOfCaseInsensitive(AllowedUnits, Units) == -1)
            {
            if (Units == "")
               throw new Exception("No units found for variable " + Name);
            else
               throw new Exception("Invalid units found for variable " + Name + ". Units = " + Units);
            }
         }

      /// <summary>
      /// Return a full code name for the given short codetext.
      /// </summary>
      public static string GetFullCodeName(string CodeText, string VariableName)
         {
         return SoilMetaData.Instance.GetFullCodeName(CodeText, VariableName);
         }

      // ----------------------- Crop methods -------------------------------

      /// <summary>
      /// Return a list of crop names (measured + predicted)
      /// </summary>
      public static string[] Crops(XmlNode SoilNode)
         {
         List<string> CropNames = new List<string>();
         XmlNode WaterNode = XmlHelper.Find(SoilNode, "Water");
         if (WaterNode != null)
            {
            foreach (XmlNode CropNode in XmlHelper.ChildNodes(WaterNode, "SoilCrop"))
               CropNames.Add(XmlHelper.Name(CropNode));
            XmlNode PredLLCoeff = Configuration.Instance.GetSettingsNode("Soil/PredictedLLCoeff");

            Soil.Variable SoilType = Soil.Get(SoilNode, "SoilType");
            if (PredLLCoeff != null && SoilType.Value != null && SoilType.Value != "")
               {
               XmlNode PredSoilTypeNode = XmlHelper.Find(PredLLCoeff, SoilType.Value);
               if (PredSoilTypeNode != null)
                  {
                  foreach (XmlNode Node in PredSoilTypeNode.ChildNodes)
                     {
                     if (StringManip.IndexOfCaseInsensitive(CropNames, Node.Name) == -1 &&
                         PredictedCropVariable(SoilNode, Node.Name, "ll") != null)
                        CropNames.Add(Node.Name);
                     }
                  }
               }
            }
         string[] NamesToReturn = new string[CropNames.Count];
         CropNames.CopyTo(NamesToReturn);
         return NamesToReturn;
         }

      /// <summary>
      /// Return a list of measured crop names
      /// </summary>
      public static string[] CropsMeasured(XmlNode SoilNode)
         {
         List<string> CropNames = new List<string>();
         XmlNode WaterNode = XmlHelper.Find(SoilNode, "Water");
         if (WaterNode != null)
            {
            foreach (XmlNode CropNode in XmlHelper.ChildNodes(WaterNode, "SoilCrop"))
               CropNames.Add(XmlHelper.Name(CropNode));
            }

         string[] NamesToReturn = new string[CropNames.Count];
         CropNames.CopyTo(NamesToReturn);
         return NamesToReturn;
         }
      
      /// <summary>
      /// Return a crop variable to caller. e.g. VariableName = wheat ll
      /// </summary>
      private static Soil.Variable CropVariable(XmlNode SoilNode, string RawVariableName)
         {
         Soil.Variable Value = null;
         int PosSpace = RawVariableName.LastIndexOf(" ");
         if (PosSpace != -1)
            {
            string CropName = RawVariableName.Substring(0, PosSpace);
            string CropVariableName = RawVariableName.Substring(PosSpace + 1);

            // Return calculated PAWC relative to crop ll
            if (CropVariableName.ToLower() == "pawc")
               {
               Soil.Variable LL = Soil.Get(SoilNode, CropName + " LL");
               LL.Units = "mm/mm";
               
               Soil.Variable XF = Soil.Get(SoilNode, CropName + " XF");
               XF.Units = "0-1";
               XF.ThicknessMM = LL.ThicknessMM;

               Soil.Variable DUL = Soil.Get(SoilNode, "DUL");
               DUL.Units = "mm/mm";
               DUL.ThicknessMM = LL.ThicknessMM;

               Value = new Soil.Variable(CropName = " PAWC", "mm/mm", 
                                        PAWC(LL.ThicknessMM, LL.Doubles, DUL.Doubles, XF.Doubles),
                                        LL.ThicknessMM, SoilNode);
               Value.Codes = StringManip.CreateStringArray("Calculated", Value.Doubles.Length);
               Value.ThicknessMM = LL.ThicknessMM;
               }
            // Return calculated PAWC relative to crop ll
            else if (CropVariableName.ToLower() == "paw")
               {
               Soil.Variable SW = Soil.Get(SoilNode, "SW");
               SW.Units = "mm/mm";
               
               Soil.Variable LL = Soil.Get(SoilNode, CropName + " LL");
               LL.Units = "mm/mm";
               LL.ThicknessMM = SW.ThicknessMM;

               Soil.Variable XF = Soil.Get(SoilNode, CropName + " XF");
               XF.Units = "0-1";
               XF.ThicknessMM = SW.ThicknessMM;

               Value = new Soil.Variable(CropName + " PAW", "mm/mm", 
                                        PAWC(LL.ThicknessMM, LL.Doubles, SW.Doubles, XF.Doubles),
                                        LL.ThicknessMM, SoilNode);
               Value.Codes = StringManip.CreateStringArray("Calculated", Value.Doubles.Length);
               }
            else
               {
               // Find the crop node firstly.
               XmlNode CropNode = XmlHelper.Find(SoilNode, "Water/" + CropName);
               if (CropNode != null)
                  Value = new Soil.Variable(CropNode, CropVariableName);

               // Try getting a predicted crop variable.
               if (Value == null)
                  Value = PredictedCropVariable(SoilNode, CropName, CropVariableName);
               }
            return Value;
            }
         return null;
         }

      /// <summary>
      /// Return a predicted crop variable name for the specified crop and variable
      /// </summary>
      private static Soil.Variable PredictedCropVariable(XmlNode SoilNode, string CropName, string CropVariableName)
         {
         Soil.Variable Value = null;
         Soil.Variable SoilType = Soil.Get(SoilNode, "SoilType");
         if (SoilType.Value != "" && CropVariableName.ToLower() == "ll")
            {
            // If we get to here then must be a predicted variable.
            XmlNode PredLLNode = XmlHelper.Find(Configuration.Instance.GetSettingsNode("Soil"),
                                                "PredictedLLCoeff/" + SoilType.Value + "/" + CropName);
            if (PredLLNode != null)
               {
               Soil.Variable a = new Soil.Variable(PredLLNode, "a");
               Soil.Variable b = new Soil.Variable(PredLLNode, "b");
               double[] CoeffDepthCentre = SoilUtility.ToMidPoints(a.ThicknessMM);

               // Get some soil numbers we're going to need.
               
               Soil.Variable DUL = Soil.Get(SoilNode, "DUL");
               DUL.Units = "mm/mm";
               DUL.ThicknessMM = a.ThicknessMM;

               Soil.Variable LL15 = Soil.Get(SoilNode, "LL15");
               LL15.Units = "mm/mm";
               LL15.ThicknessMM = a.ThicknessMM;

               double[] DepthCentre = SoilUtility.ToMidPoints(a.ThicknessMM);

               // only continue if our soil depth  centers are within range of
               // the coefficient depth centers.
               if (DepthCentre[DepthCentre.Length - 1] <= CoeffDepthCentre[CoeffDepthCentre.Length - 1])
                  {
                  double[] LL = new double[DepthCentre.Length];
                  for (int i = 0; i != DepthCentre.Length; i++)
                     {
                     bool DidInterpolate;
                     double A = MathUtility.LinearInterpReal(DepthCentre[i], CoeffDepthCentre, a.Doubles, out DidInterpolate);
                     double B = MathUtility.LinearInterpReal(DepthCentre[i], CoeffDepthCentre, b.Doubles, out DidInterpolate);
                     LL[i] = DUL.Doubles[i] * (A + B * DUL.Doubles[i]) / 100.0;

                     // Bound the predicted LL values.
                     LL[i] = Math.Max(LL[i], LL15.Doubles[i]);
                     LL[i] = Math.Min(LL[i], DUL.Doubles[i]);
                     }

                  //  make the top 3 layers the same as the the top 3 layers of LL15
                  if (LL.Length >= 3)
                     {
                     LL[0] = LL15.Doubles[0];
                     LL[1] = LL15.Doubles[1];
                     LL[2] = LL15.Doubles[2];
                     }
                  // Create a variable value structure to return to caller.
                  Value = new Soil.Variable(CropName + " " + CropVariableName, "mm/mm", LL, a.ThicknessMM, SoilNode);
                  Value.ThicknessMM = Soil.Get(SoilNode, "DUL").ThicknessMM;
                  Value.Codes = StringManip.CreateStringArray("Calculated", Value.Doubles.Length);
                  }
               }
            }
         else if (SoilType.Value != "" && CropVariableName.ToLower() == "kl")
            {
            // If we get to here then must be a predicted variable.
            XmlNode PredKLNode = XmlHelper.Find(Configuration.Instance.GetSettingsNode("Soil"), 
                                                "PredictedKLCoeff/" + CropName);
            if (PredKLNode != null)
               {
               Soil.Variable LL = Soil.Get(SoilNode, CropName + " " + "LL");

               Soil.Variable KL = Soil.Get(PredKLNode, "KL");
               KL.ThicknessMM = LL.ThicknessMM;

               // Create a variable value structure to return to caller.
               KL.Codes = StringManip.CreateStringArray("Calculated", KL.Doubles.Length);

               Value = KL;
               }
            }
         else if (SoilType.Value != "" && CropVariableName.ToLower() == "xf")
            {
            Soil.Variable LL15 = Soil.Get(SoilNode, "LL15");

            // Set the estimated XF values to the first measured crop.

            string[] Crops = CropsMeasured(SoilNode);
            if (Crops.Length >= 1)
               {
               Value = Soil.Get(SoilNode, Crops[0] + " xf");
               Value.ThicknessMM = LL15.ThicknessMM;
               Value.Name = CropName + " " + CropVariableName;
               }
            else
               {
               double[] xf = new double[LL15.ThicknessMM.Length];
               for (int i = 0; i < xf.Length; i++)
                  xf[i] = 1.0;
               Value = new Soil.Variable(CropName + " " + CropVariableName, "0-1", xf, LL15.ThicknessMM, SoilNode);
               }
            Value.Codes = StringManip.CreateStringArray("Calculated", Value.Doubles.Length);
            }
         return Value;
         }

      /// <summary>
      /// Return SW as it comes from a <InitWater> node. Returns null if not found.
      /// </summary>
      private static Soil.Variable SWFromInitWater(XmlNode SoilNode, string RawVariableName)
         {
         XmlNode InitWaterNode = XmlHelper.FindByType(SoilNode, "InitWater");
         if (RawVariableName.ToLower() == "sw" && InitWaterNode != null)
            {
            Soil.Variable dul = Soil.Get(SoilNode, "DUL");
            dul.Units = "mm";
            Soil.Variable ll;
            double[] xf;
            string RelativeTo = XmlHelper.Value(InitWaterNode, "RelativeTo");
            if (RelativeTo == "" || RelativeTo == "ll15")
               {
               ll = Soil.Get(SoilNode, "LL15");
               xf = MathUtility.CreateArrayOfValues(1.0, ll.ThicknessMM.Length);
               }
            else
               {
               ll = Soil.Get(SoilNode, RelativeTo + " ll");
               xf = Soil.Get(SoilNode, RelativeTo + " xf").Doubles;
               }
            ll.Units = "mm";

            double[] pawc = Soil.PAWC(ll.ThicknessMM, ll.Doubles, dul.Doubles, xf);

            double[] sw = new double[ll.Doubles.Length];
            if (XmlHelper.Value(InitWaterNode, "DepthWetSoilMethod/Depth") == "")
               {
               XmlNode PercentMethodNode = XmlHelper.Find(InitWaterNode, "PercentMethod");
               double Percent = 0;
               if (PercentMethodNode != null)
                  Percent = Convert.ToDouble(XmlHelper.Value(PercentMethodNode, "Percent")) * 100;
               if (XmlHelper.Value(PercentMethodNode, "Distributed").ToLower() == "filled from top")
                  {
                  double AmountWater = MathUtility.Sum(pawc) * (Percent / 100.0);
                  for (int Layer = 0; Layer < ll.Doubles.Length; Layer++)
                     {
                     if (AmountWater >= 0 && xf != null && xf[Layer] == 0)
                        sw[Layer] = ll.Doubles[Layer];
                     else if (AmountWater >= pawc[Layer])
                        {
                        sw[Layer] = dul.Doubles[Layer];
                        AmountWater = AmountWater - pawc[Layer];
                        }
                     else
                        {
                        double Prop = AmountWater / pawc[Layer];
                        sw[Layer] = Prop * (dul.Doubles[Layer] - ll.Doubles[Layer]) + ll.Doubles[Layer];
                        AmountWater = 0;
                        }
                     }
                  }
               else
                  {
                  for (int Layer = 0; Layer < ll.Doubles.Length; Layer++)
                     sw[Layer] = Percent / 100.0 * (dul.Doubles[Layer] - ll.Doubles[Layer]) + ll.Doubles[Layer];
                  }
               }
            else
               {
               double DepthWetSoil = Convert.ToDouble(XmlHelper.Value(InitWaterNode, "DepthWetSoilMethod/Depth"));

               double[] Thickness = ll.ThicknessMM;
               double DepthSoFar = 0;
               for (int Layer = 0; Layer < Thickness.Length; Layer++)
                  {
                  if (DepthWetSoil > DepthSoFar + Thickness[Layer])
                     sw[Layer] = dul.Doubles[Layer];
                  else
                     {
                     double Prop = Math.Max(DepthWetSoil - DepthSoFar, 0) / Thickness[Layer];
                     sw[Layer] = Prop * (dul.Doubles[Layer] - ll.Doubles[Layer]) + ll.Doubles[Layer];
                     }
                  DepthSoFar += Thickness[Layer];
                  }
               }
            Soil.Variable Value = new Soil.Variable("SW", "mm", sw, ll.ThicknessMM, SoilNode);
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
      private static double[] PAWC(double[] Thickness, double[] LL, double[] DUL, double[] XF)
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
      private static Soil.Variable CalculatedVariables(XmlNode SoilNode, string RawVariableName)
         {
         RawVariableName = RawVariableName.ToLower();

         Soil.Variable Value = null;

         // PAWC relative to LL15
         if (RawVariableName == "pawc")
            {
            Soil.Variable LL15 = Soil.Get(SoilNode, "LL15");
            LL15.Units = "mm/mm";

            Soil.Variable DUL = Soil.Get(SoilNode, "DUL");
            DUL.Units = "mm/mm";

            Value = new Soil.Variable("PAWC", "mm/mm", PAWC(LL15.ThicknessMM, LL15.Doubles, DUL.Doubles, null), LL15.ThicknessMM, SoilNode);
            }

         // PAW relative to LL15
         else if (RawVariableName == "paw")
            {
            Soil.Variable LL15 = Soil.Get(SoilNode, "LL15");
            LL15.Units = "mm/mm";

            Soil.Variable SW = Soil.Get(SoilNode, "SW");
            SW.Units = "mm/mm";

            Value = new Soil.Variable("PAW", "mm/mm", PAWC(LL15.ThicknessMM, LL15.Doubles, SW.Doubles, null), LL15.ThicknessMM, SoilNode);
            }

         // InertC
         else if (RawVariableName == "inertc")
            {
            // Could be a different layer structure to TargetThickness - can't use Variable method.
            Soil.Variable OC = Soil.Get(SoilNode, "OC");
            OC.Units = "Total %";

            Soil.Variable FInert = Soil.Get(SoilNode, "FInert");
            FInert.Units = "0-1";
            FInert.ThicknessMM = OC.ThicknessMM;

            Soil.Variable BD = Soil.Get(SoilNode, "BD");
            BD.Units = "g/cc";
            BD.ThicknessMM = OC.ThicknessMM;

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
            Value = new Soil.Variable("InertC", "kg/ha", InertC, OC.ThicknessMM, SoilNode);
            }

         // BiomC
         else if (RawVariableName == "biomc")
            {
            // Could be a different layer structure to TargetThickness - can't use Variable method.
            Soil.Variable OC = Soil.Get(SoilNode, "OC");
            OC.Units = "Total %";

            Soil.Variable FBiom = Soil.Get(SoilNode, "FBiom");
            FBiom.Units = "0-1";
            FBiom.ThicknessMM = OC.ThicknessMM;

            Soil.Variable BD = Soil.Get(SoilNode, "BD");
            BD.Units = "g/cc";
            BD.ThicknessMM = OC.ThicknessMM;

            Soil.Variable InertC = Soil.Get(SoilNode, "InertC");
            InertC.Units = "kg/ha";
            InertC.ThicknessMM = OC.ThicknessMM;

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
            Value = new Soil.Variable("BiomC", "kg/ha", BiomC, OC.ThicknessMM, SoilNode);
            }
         // HumC
         else if (RawVariableName == "humc")
            {
            Soil.Variable OC = Soil.Get(SoilNode, "OC");
            OC.Units = "Total %";

            Soil.Variable BD = Soil.Get(SoilNode, "BD");
            BD.Units = "g/cc";
            BD.ThicknessMM = OC.ThicknessMM;

            Soil.Variable InertC = Soil.Get(SoilNode, "InertC");
            InertC.Units = "kg/ha";
            InertC.ThicknessMM = OC.ThicknessMM;

            Soil.Variable BiomC = Soil.Get(SoilNode, "BiomC");
            BiomC.Units = "kg/ha";
            BiomC.ThicknessMM = OC.ThicknessMM;

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
            Value = new Soil.Variable("HumC", "kg/ha", HumC, OC.ThicknessMM, SoilNode);
            }
         if (Value != null)
            Value.Codes = StringManip.CreateStringArray("Calculated", Value.Doubles.Length);

         return Value;
         }

      /// <summary>
      /// Check that the soil is a valid one.
      /// </summary>
      public static string CheckForErrors(XmlNode SoilNode, bool IgnoreWaterAndNitrogen)
         {
         if (Types.Instance.TypeNames.Length == 0)
            PlugIns.LoadAll();

         XmlNode ApsimToSimNode = Types.Instance.ApsimToSim("soil");
         if (ApsimToSimNode == null)
            throw new Exception("Cannot find an <ApsimToSim> for soil type");
         string ApsimToSim = ApsimToSimNode.InnerText;

         if (IgnoreWaterAndNitrogen)
            {
            ApsimToSim = ApsimToSim.Replace("[soil.SW(mm/mm)]", "");
            ApsimToSim = ApsimToSim.Replace("[soil.NO3(ppm)]", "");
            ApsimToSim = ApsimToSim.Replace("[soil.NH4(ppm)]", "");
            }

         string ErrorMessages = "";
         try
            {
            ReplaceSoilMacros(SoilNode, ApsimToSim);
            }
         catch (Exception err)
            {
            ErrorMessages = err.Message;
            }

         // Do some more rigorous checks.
         if (ErrorMessages == "")
            ErrorMessages += CheckProfile(SoilNode);
         if (ErrorMessages == "" && !IgnoreWaterAndNitrogen)
            ErrorMessages += CheckSW(SoilNode);

         return ErrorMessages;
         }


      /// <summary>
      /// Checks validity of soil water parameters for a soil profile layer
      /// This is a port of the soilwat2_check_profile routine.
      /// </summary>
      private static string CheckProfile(XmlNode SoilNode)
         {
         string errorMessages = "";
         const double min_sw = 0.0;
         const double specific_bd = 2.65; // (g/cc)

         Soil.Variable airdry = Soil.Get(SoilNode, "AirDry");
         airdry.Units = "mm/mm";

         Soil.Variable ll15 = Soil.Get(SoilNode, "ll15");
         ll15.Units = "mm/mm";

         Soil.Variable dul = Soil.Get(SoilNode, "dul");
         dul.Units = "mm/mm";

         Soil.Variable sat = Soil.Get(SoilNode, "sat");
         sat.Units = "mm/mm";

         Soil.Variable bd = Soil.Get(SoilNode, "bd");
         bd.Units = "g/cc";

         Soil.Variable oc = Soil.Get(SoilNode, "oc");
         oc.Units = "Total %";

         Soil.Variable ph = Soil.Get(SoilNode, "ph");
         ph.Units = "1:5 water";

         // Check crop variables.
         foreach (string Crop in CropsMeasured(SoilNode))
            {
            Soil.Variable ll = Soil.Get(SoilNode, Crop + " LL");
            ll.Units = "mm/mm";

            Soil.Variable kl = Soil.Get(SoilNode, Crop + " KL");
            kl.Units = "/day";

            Soil.Variable xf = Soil.Get(SoilNode, Crop + " XF");
            xf.Units = "0-1";

            if (!MathUtility.ValuesInArray(ll.Doubles) || !MathUtility.ValuesInArray(kl.Doubles) ||
                !MathUtility.ValuesInArray(xf.Doubles))
               errorMessages += "Values for LL, KL or XF are missing for crop " + Crop + "\r\n";

            else
               {
               for (int layer = 0; layer != ll15.ThicknessMM.Length; layer++)
                  {
                  int RealLayerNumber = layer + 1;

                  if (kl.Doubles[layer] == MathUtility.MissingValue)
                     errorMessages += Crop + " KL value missing"
                              + " in layer " + RealLayerNumber.ToString() + "\r\n";

                  else if (MathUtility.GreaterThan(kl.Doubles[layer], 1, 3))
                     errorMessages += Crop + " KL value of " + kl.Doubles[layer].ToString("f3")
                              + " in layer " + RealLayerNumber.ToString() + " is greater than 1"
                              + "\r\n";

                  if (xf.Doubles[layer] == MathUtility.MissingValue)
                     errorMessages += Crop + " XF value missing"
                              + " in layer " + RealLayerNumber.ToString() + "\r\n";

                  else if (MathUtility.GreaterThan(xf.Doubles[layer], 1, 3))
                     errorMessages += Crop + " XF value of " + xf.Doubles[layer].ToString("f3")
                              + " in layer " + RealLayerNumber.ToString() + " is greater than 1"
                              + "\r\n";

                  if (ll.Doubles[layer] == MathUtility.MissingValue)
                     errorMessages += Crop + " LL value missing"
                              +" in layer " + RealLayerNumber.ToString() + "\r\n";

                  else if (MathUtility.LessThan(ll.Doubles[layer], airdry.Doubles[layer], 3))
                     errorMessages += Crop + " LL of " + ll.Doubles[layer].ToString("f3")
                                  + " in layer " + RealLayerNumber.ToString() + " is below air dry value of " + airdry.Doubles[layer].ToString("f3")
                                + "\r\n";

                  else if (MathUtility.GreaterThan(ll.Doubles[layer], dul.Doubles[layer], 3))
                     errorMessages += Crop + " LL of " + ll.Doubles[layer].ToString("f3")
                                  + " in layer " + RealLayerNumber.ToString() + " is above drained upper limit of " + dul.Doubles[layer].ToString("f3")
                                + "\r\n";
                  }
               }
            }

         // Check other profile variables.
         for (int layer = 0; layer != ll15.ThicknessMM.Length; layer++)
            {
            double max_sw = MathUtility.Round(1.0 - bd.Doubles[layer] / specific_bd, 3);
            int RealLayerNumber = layer + 1;

            if (airdry.Doubles[layer] == MathUtility.MissingValue)
               errorMessages += " Air dry value missing"
                        +" in layer " + RealLayerNumber.ToString() + "\r\n";

            else if (MathUtility.LessThan(airdry.Doubles[layer], min_sw, 3))
               errorMessages += " Air dry lower limit of " + airdry.Doubles[layer].ToString("f3")
                                  + " in layer " + RealLayerNumber.ToString() + " is below acceptable value of " + min_sw.ToString("f3")
                          + "\r\n";

            if (ll15.Doubles[layer] == MathUtility.MissingValue)
               errorMessages += "15 bar lower limit value missing"
                        +" in layer " + RealLayerNumber.ToString() + "\r\n";

            else if (MathUtility.LessThan(ll15.Doubles[layer], airdry.Doubles[layer], 3))
               errorMessages += "15 bar lower limit of " + ll15.Doubles[layer].ToString("f3")
                            + " in layer " + RealLayerNumber.ToString() + " is below air dry value of " + airdry.Doubles[layer].ToString("f3")
                          + "\r\n";

            if (dul.Doubles[layer] == MathUtility.MissingValue)
               errorMessages += "Drained upper limit value missing"
                        +" in layer " + RealLayerNumber.ToString() + "\r\n";

            else if (MathUtility.LessThan(dul.Doubles[layer], ll15.Doubles[layer], 3))
               errorMessages += "Drained upper limit of " + dul.Doubles[layer].ToString("f3")
                            + " in layer " + RealLayerNumber.ToString() + " is at or below lower limit of " + ll15.Doubles[layer].ToString("f3")
                          + "\r\n";

            if (sat.Doubles[layer] == MathUtility.MissingValue)
               errorMessages += "Saturation value missing"
                        +" in layer " + RealLayerNumber.ToString() + "\r\n";
            
            else if (MathUtility.LessThan(sat.Doubles[layer], dul.Doubles[layer], 3))
               errorMessages += "Saturation of " + sat.Doubles[layer].ToString("f3")
                            + " in layer " + RealLayerNumber.ToString() + " is at or below drained upper limit of " + dul.Doubles[layer].ToString("f3")
                          + "\r\n";

            else if (MathUtility.GreaterThan(sat.Doubles[layer], max_sw, 3))
               {
               double max_bd = (1.0 - sat.Doubles[layer]) * specific_bd;
               errorMessages += "Saturation of " + sat.Doubles[layer].ToString("f3")
                            + " in layer " + RealLayerNumber.ToString() + " is above acceptable value of  " + max_sw.ToString("f3")
                          + ". You must adjust bulk density to below " + max_bd.ToString("f3")
                          + " OR saturation to below " + max_sw.ToString("f3")
                          + "\r\n";
               }

            if (bd.Doubles[layer] == MathUtility.MissingValue)
               errorMessages += "BD value missing"
                        +" in layer " + RealLayerNumber.ToString() + "\r\n";
            
            else if (MathUtility.GreaterThan(bd.Doubles[layer], 2.65, 3))
               errorMessages += "BD value of " + bd.Doubles[layer].ToString("f3")
                            + " in layer " + RealLayerNumber.ToString() + " is greater than the theoretical maximum of 2.65"
                          + "\r\n";

            if (oc.Doubles[layer] == MathUtility.MissingValue)
               errorMessages += "OC value missing"
                        +" in layer " + RealLayerNumber.ToString() + "\r\n";

            else if (MathUtility.LessThan(oc.Doubles[layer], 0.01, 3))
               errorMessages += "OC value of " + oc.Doubles[layer].ToString("f3")
                             + " in layer " + RealLayerNumber.ToString() + " is less than 0.01"
                             + "\r\n";

            if (ph.Doubles[layer] == MathUtility.MissingValue)
               errorMessages += "PH value missing"
                        +" in layer " + RealLayerNumber.ToString() + "\r\n";

            else if (MathUtility.LessThan(ph.Doubles[layer], 3.5, 3))
               errorMessages += "PH value of " + ph.Doubles[layer].ToString("f3")
                             + " in layer " + RealLayerNumber.ToString() + " is less than 3.5"
                             + "\r\n";
            else if (MathUtility.GreaterThan(ph.Doubles[layer], 11, 3))
               errorMessages += "PH value of " + ph.Doubles[layer].ToString("f3")
                             + " in layer " + RealLayerNumber.ToString() + " is greater than 11"
                             + "\r\n";
            }
         return errorMessages;
         }

      /// <summary>
      /// Check the validity of initial soil water.
      /// </summary>
      /// <returns></returns>
      private static string CheckSW(XmlNode SoilNode)
         {
         string errorMessages = "";

         Soil.Variable sw = Soil.Get(SoilNode, "sw");
         sw.Units = "mm/mm";

         Soil.Variable airdry = Soil.Get(SoilNode, "AirDry");
         airdry.Units = "mm/mm";
         airdry.ThicknessMM = sw.ThicknessMM;

         Soil.Variable sat = Soil.Get(SoilNode, "sat");
         sat.Units = "mm/mm";
         sat.ThicknessMM = sw.ThicknessMM;

         if (sw.Doubles.Length > 0)
            {
            for (int layer = 0; layer != airdry.ThicknessMM.Length; layer++)
               {
               int RealLayerNumber = layer + 1;

               if (sw.Doubles[layer] == MathUtility.MissingValue)
                  errorMessages += "Soil water value missing"
                           +" in layer " + RealLayerNumber.ToString() + "\r\n";

               else if (MathUtility.GreaterThan(sw.Doubles[layer], sat.Doubles[layer], 3))
                  errorMessages += "Soil water of " + sw.Doubles[layer].ToString("f3")
                                + " in layer " + RealLayerNumber.ToString() + " is above saturation of " + sat.Doubles[layer].ToString("f3")
                                + "\r\n";

               else if (MathUtility.LessThan(sw.Doubles[layer], airdry.Doubles[layer], 3))
                  errorMessages += "Soil water of " + sw.Doubles[layer].ToString("f3")
                                + " in layer " + RealLayerNumber.ToString() + " is below air-dry value of " + airdry.Doubles[layer].ToString("f3")
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
