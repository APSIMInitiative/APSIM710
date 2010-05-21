
using System;
using System.Collections;
using System.Collections.Generic;
using System.Collections.Specialized;
using System.Xml;

using Microsoft.VisualBasic;

using CSGeneral;
using System.IO;

namespace ApsimFile
   {
   internal class VariableValue
      {
      public string Name;
      public string Units;
      public double[] ThicknessMM;
      public double[] Doubles;
      public string[] Strings;
      public string[] Codes;
      }

   internal class SoilMetaData
      {
      private static SoilMetaData Singleton = null;
      private XmlNode MetaDataNode;

      /// <summary>
      /// Singleton instance of the MetaData node.
      /// </summary>
      public static SoilMetaData Instance
         {
         get
            {
            if (Singleton == null)
               Singleton = new SoilMetaData();
            return Singleton;
            }
         }

      /// <summary>
      /// Return the specified metadata for the specified variable. 
      /// </summary>
      /// <param name="MetaDataType">e.g. "Location", "Units", "Description"</param>
      internal string MetaData(string VariableName, string MetaDataType)
         {
         ReadMetaData();
         XmlNode Node = XmlHelper.FindRecursively(MetaDataNode, VariableName);
         if (Node != null)
            return XmlHelper.Value(Node, MetaDataType);
         return "";
         }

      /// <summary>
      /// Return true if the specified variable is a property.
      /// </summary>
      internal bool IsProperty(string VariableName)
         {
         ReadMetaData();
         StringManip.SplitOffBracketedValue(ref VariableName, '(', ')');
         XmlNode Property = XmlHelper.FindRecursively(MetaDataNode, VariableName);
         if (Property != null)
            {
            string Path = XmlHelper.FullPath(Property);
            return Path.Contains("Properties");
            }
         return false;
         }

      /// <summary>
      /// Ensure the specified variable is a valid one. Will throw if not valid.
      /// </summary>
      internal void EnsureValidVariable(string VariableName)
         {
         ReadMetaData();
         StringManip.SplitOffBracketedValue(ref VariableName, '(', ')');
         if (XmlHelper.FindRecursively(MetaDataNode, VariableName) == null)
            throw new Exception("Invalid soil variable name: " + VariableName);
         }

      /// <summary>
      /// Return a list of valid variables for the specified NodeName (eg. water).
      /// </summary>
      internal List<string> ValidVariablesForProfileNode(string NodeName)
         {
         ReadMetaData();
         // Go find the metadata node.
         XmlNode MetaDataN = XmlHelper.Find(MetaDataNode, NodeName + "/Variables");
         if (MetaDataN == null)
            throw new Exception("Invalid soil node name: " + NodeName);

         // We need to create a list of variable names for the specified node.
         List<string> VariableNames = new List<string>();
         foreach (XmlNode VariableNode in MetaDataN.ChildNodes)
            {
            string Units = XmlHelper.Value(VariableNode, "Units");
            VariableNames.Add(VariableNode.Name + " (" + Units + ")");
            }
         return VariableNames;
         }

      /// <summary>
      /// If necessary, this method will read all metadata from the internal resource. 
      /// Can be called multiple times.
      /// </summary>
      private void ReadMetaData()
         {
         if (MetaDataNode == null)
            {
            System.Reflection.Assembly thisExe = System.Reflection.Assembly.GetExecutingAssembly();
            System.IO.Stream file = thisExe.GetManifestResourceStream("ApsimFile.Resources.SoilMetaData.xml");
            StreamReader In = new StreamReader(file);
            string XML = In.ReadToEnd();
            In.Close();
            XmlDocument Doc = new XmlDocument();
            Doc.LoadXml(XML);
            MetaDataNode = Doc.DocumentElement;
            if (MetaDataNode == null)
               throw new Exception("Cannot find soil metadata resource");
            }
         }

      /// <summary>
      /// Use metadata to get the path for a property. Returns it's path if found or blank otherwise.
      /// </summary>
      internal string GetPropertyPath(string PropertyName)
         {
         ReadMetaData();
         XmlNode Property = XmlHelper.FindRecursively(MetaDataNode, PropertyName);
         if (Property != null)
            {
            string Path = XmlHelper.FullPath(Property);
            if (Path.Contains("Properties"))
               {
               Path = Path.Replace("/Soil/", "");
               Path = Path.Replace("Properties/", "");
               return Path;
               }
            }
         throw new Exception("Invalid property name: " + PropertyName);
         }
      /// <summary>
      /// Use metadata to get the path for a property. Returns it's path if found or blank otherwise.
      /// </summary>
      internal List<string> GetPropertyPaths(string PropertyName)
         {
         ReadMetaData();

         List<string> Paths = new List<string>();
         List<XmlNode> Nodes = new List<XmlNode>();
         XmlHelper.FindAllRecursively(MetaDataNode, PropertyName, ref Nodes);
         foreach (XmlNode Property in Nodes)
            {
            string Path = XmlHelper.FullPath(Property);
            if (Path.Contains("Properties"))
               {
               Path = Path.Replace("/Soil/", "");
               Path = Path.Replace("Properties/", "");
               Paths.Add(Path);
               }
            }
         return Paths;
         }
      /// <summary>
      /// Use metadata to get the path for a variable profile node. 
      /// Returns it's path if found or blank otherwise.
      /// </summary>
      internal string GetVariablePath(string VariableName)
         {
         ReadMetaData();
         XmlNode Variable = XmlHelper.FindRecursively(MetaDataNode, VariableName);
         if (Variable != null)
            {
            string Path = XmlHelper.FullPath(Variable.ParentNode.ParentNode);
            return Path.Replace("/Soil/", "");
            }
         return "";
         }

      /// <summary>
      /// Return a list of variable paths for the specified variable name.
      /// </summary>
      internal List<string> GetVariablePaths(string RawVariableName)
         {
         ReadMetaData();
         List<string> ReturnPaths = new List<string>();
         List<XmlNode> Nodes = new List<XmlNode>();
         XmlHelper.FindAllRecursively(MetaDataNode, RawVariableName, ref Nodes);
         foreach (XmlNode Node in Nodes)
            {
            string Path = XmlHelper.FullPath(Node.ParentNode.ParentNode);
            ReturnPaths.Add(Path.Replace("/Soil/", ""));
            }
         return ReturnPaths;
         }

      /// <summary>
      /// Return a list of valid units for the specified variable.
      /// </summary>
      internal List<string> ValidUnits(string RawVariableName)
         {
         ReadMetaData();
         XmlNode Variable = XmlHelper.FindRecursively(MetaDataNode, RawVariableName);
         if (Variable == null)
            return new List<string>();
         else
            return XmlHelper.Values(Variable, "units");
         }

      }

   public class SoilUtility
      {
      /// <summary>
      /// Low level routine to return a specific layered variable from
      /// a specified child profile node - as doubles.
      /// </summary>
      static public double[] GetLayered(XmlNode ChildNode, string VariableName)
         {
         StringManip.SplitOffBracketedValue(ref VariableName, '(', ')');
         return MathUtility.StringsToDoubles(GetLayeredAsStrings(ChildNode, VariableName));
         }

      /// <summary>
      /// Low level routine to return a specific layered variable from
      /// a specified child profile node - as strings.
      /// </summary>
      static public string[] GetLayeredAsStrings(XmlNode Child, string VariableName)
         {
         List<XmlNode> LayerNodes = XmlHelper.ChildNodes(Child, "layer");
         string[] values = new string[LayerNodes.Count];
         for (int i = 0; i < LayerNodes.Count; i++)
            values[i] = XmlHelper.Value(LayerNodes[i], VariableName);

         return values;
         }

      /// <summary>
      /// Return the current units for the specified variable in the specified profile node.
      /// </summary>
      static public string GetUnitsForVariable(XmlNode ProfileNode, string VariableName)
         {
         XmlNode Node = XmlHelper.Find(ProfileNode, "Layer/" + VariableName);
         if (Node != null)
            return XmlHelper.Attribute(Node, "units");
         return "";
         }

      /// <summary>
      /// Low level routine to set the layered double values for the specified
      /// variable, under the specified profile node.
      /// </summary>
      static public void SetLayered(XmlNode ChildNode, string VariableName, double[] Values)
         {
         string[] StringValues = new string[Values.Length];
         int Index = 0;
         foreach (double Value in Values)
            {
            if (Value == MathUtility.MissingValue)
               StringValues[Index] = "";
            else
               StringValues[Index] = Values[Index].ToString();
            Index++;
            }
         SetLayeredAsStrings(ChildNode, VariableName, StringValues);
         }

      /// <summary>
      /// Low level routine to set the layered string values for the specified
      /// variable, under the specified profile node.
      /// </summary>
      static public void SetLayeredAsStrings(XmlNode ChildNode, string VariableName, string[] Values)
         {
         if (Values.Length > 0)
            {
            XmlHelper.EnsureNumberOfChildren(ChildNode, "Layer", "", Values.Length);

            List<XmlNode> Layers = XmlHelper.ChildNodes(ChildNode, "Layer");
            for (int i = 0; i != Values.Length; i++)
               XmlHelper.SetValue(Layers[i], VariableName, Values[i]);
            }
         }


      /// <summary>
      /// Retrieve and return a variable value to caller. Will throw on error. Never returns
      /// null. No mapping or unit conversion.
      /// </summary>
      internal static VariableValue GetVariableValueFromProfile(XmlNode ProfileNode, string RawVariableName)
         {
         VariableValue Value = new VariableValue();
         Value.Name = RawVariableName;
         StringManip.SplitOffBracketedValue(ref Value.Name, '(', ')');

         if (Value.Name.ToLower() == "texture")
            {
            Value.Strings = SoilUtility.GetLayeredAsStrings(ProfileNode, Value.Name);
            Value.Codes = StringManip.CreateStringArray("", Value.Strings.Length);
            }
         else if (Value.Name.ToLower() == "thickness")
            {
            if (VariableExists(ProfileNode, "Depth"))
               {
               Value.Doubles = ToThickness(SoilUtility.GetLayeredAsStrings(ProfileNode, "Depth"));
               Value.Units = SoilUtility.GetUnitsForVariable(ProfileNode, "Depth");
               Value.Codes = GetLayeredCode(ProfileNode, "Depth");
               }
            else
               {
               Value.Doubles = SoilUtility.GetLayered(ProfileNode, "Thickness");
               Value.Units = SoilUtility.GetUnitsForVariable(ProfileNode, "Thickness");
               Value.Codes = GetLayeredCode(ProfileNode, "Thickness");
               }
            Value.ThicknessMM = Value.Doubles;
            if (Value.Units == "cm")
               Value.ThicknessMM = MathUtility.Multiply_Value(Value.ThicknessMM, 10);
            }
         else if (Value.Name.ToLower() == "depth")
            {
            if (VariableExists(ProfileNode, "Depth"))
               {
               Value.Strings = SoilUtility.GetLayeredAsStrings(ProfileNode, "Depth");
               Value.Units = SoilUtility.GetUnitsForVariable(ProfileNode, "Depth");
               Value.Codes = GetLayeredCode(ProfileNode, "Depth");
               Value.ThicknessMM = SoilUtility.ToThickness(Value.Strings);
               if (Value.Units == "cm")
                  Value.ThicknessMM = MathUtility.Multiply_Value(Value.ThicknessMM, 10);

               }
            else
               {
               Value.ThicknessMM = SoilUtility.GetLayered(ProfileNode, "Thickness");
               Value.Strings = ToDepthStrings(Value.ThicknessMM);
               Value.Units = SoilUtility.GetUnitsForVariable(ProfileNode, "Thickness");
               Value.Codes = GetLayeredCode(ProfileNode, "Thickness");
               if (Value.Units == "cm")
                  Value.ThicknessMM = MathUtility.Multiply_Value(Value.ThicknessMM, 10);
               }
            }

         else if (Value.Name.ToLower() == "depthmidpoints")
            {
            VariableValue Thickness = GetVariableValueFromProfile(ProfileNode, "Thickness");
            Value.Doubles = ToMidPoints(Thickness.Doubles);
            Value.Units = Thickness.Units;
            Value.Codes = Thickness.Codes;
            }
         else
            {
            Value.Doubles = SoilUtility.GetLayered(ProfileNode, Value.Name);
            Value.Units = SoilUtility.GetUnitsForVariable(ProfileNode, Value.Name);
            Value.Codes = GetLayeredCode(ProfileNode, Value.Name);
            VariableValue ThicknessValue = GetVariableValueFromProfile(ProfileNode, "Thickness");
            Value.ThicknessMM = ThicknessValue.Doubles;
            if (ThicknessValue.Units == "cm")
               Value.ThicknessMM = MathUtility.Multiply_Value(Value.ThicknessMM, 10);
            }

         return Value;
         }

      /// <summary>
      /// Set a variable value.
      /// </summary>
      internal static void SetVariableValue(XmlNode ProfileNode, VariableValue Value)
         {
         if (Value.Name == "Thickness")
            DeleteVariable(ProfileNode, "Depth");
         else if (Value.Name == "Depth")
            DeleteVariable(ProfileNode, "Thickness");

         if (Value.Strings != null && Value.Name.Contains("Code"))
            {
            string NameWithNoCode = Value.Name.Replace("Code", "");
            SoilUtility.SetLayeredCode(ProfileNode, NameWithNoCode, Value.Strings);
            }
         else if (Value.Strings != null)
            SoilUtility.SetLayeredAsStrings(ProfileNode, Value.Name, Value.Strings);
         else
            SoilUtility.SetLayered(ProfileNode, Value.Name, Value.Doubles);

         if (Value.Units == "" && !Value.Name.Contains("Code") && !Value.Name.Contains("Method"))
            {
            List<string> Units = SoilMetaData.Instance.ValidUnits(Value.Name);
            if (Units.Count > 0)
               Value.Units = Units[0];
            }

         // Store the units.
         XmlNode Node = XmlHelper.Find(ProfileNode, "Layer/" + Value.Name);
         if (Node != null && Value.Units != "")
            XmlHelper.SetAttribute(Node, "units", Value.Units);
         }


      private static void DeleteVariable(XmlNode ProfileNode, string VariableName)
         {
         foreach (XmlNode Layer in XmlHelper.ChildNodes(ProfileNode, "Layer"))
            {
            XmlNode VariableNode = XmlHelper.Find(Layer, VariableName);
            if (VariableNode != null)
               Layer.RemoveChild(VariableNode);
            }
         }


      /// <summary>
      /// Return true if the specified soil variable exists.
      /// </summary>
      internal static bool VariableExists(XmlNode ProfileNode, string VariableName)
         {
         return XmlHelper.Find(ProfileNode, "Layer/" + VariableName) != null;
         }

      /// <summary>
      ///  Return layered "code" values for the specified variable under the specified node.
      /// </summary>
      internal static string[] GetLayeredCode(XmlNode Node, string VariableName)
         {
         if (Node == null)
            return null;

         List<XmlNode> Layers = XmlHelper.ChildNodes(Node, "Layer");
         string[] Values = new string[Layers.Count];

         for (int i = 0; i < Layers.Count; i++)
            {
            XmlNode Child = XmlHelper.Find(Layers[i], VariableName);
            if (Child != null)
               Values[i] = XmlHelper.Attribute(Child, "code");
            else
               Values[i] = "";
            }
         return Values;
         }

      /// <summary>
      /// Set the codes for the specified variablename
      /// </summary>
      /// <param name="ProfileNode"></param>
      /// <param name="NameWithNoCode"></param>
      /// <param name="p"></param>
      private static void SetLayeredCode(XmlNode ProfileNode, string VariableName, string[] Codes)
         {
         if (ProfileNode != null)
            {
            List<XmlNode> Layers = XmlHelper.ChildNodes(ProfileNode, "Layer");
            if (Codes.Length == Layers.Count)
               {
               for (int i = 0; i < Layers.Count; i++)
                  {
                  XmlNode Child = XmlHelper.Find(Layers[i], VariableName);
                  if (Child != null)
                     XmlHelper.SetAttribute(Child, "code", Codes[i]);
                  }
               }
            }
         }

      static public string[] ToDepthStrings(double[] Thickness)
         {
         // ------------------------------------------------------------------
         // Convert an array of thickness to depth strings
         //    e.g. "0-10", "10-30"
         // ------------------------------------------------------------------
         string[] Strings = new string[Thickness.Length];
         double DepthSoFar = 0;
         for (int i = 0; i != Thickness.Length; i++)
            {
            if (Thickness[i] == MathUtility.MissingValue)
               Strings[i] = "";
            else
               {
               double ThisThickness = Thickness[i];
               double TopOfLayer = DepthSoFar;
               double BottomOfLayer = DepthSoFar + ThisThickness;
               Strings[i] = TopOfLayer.ToString() + "-" + BottomOfLayer.ToString();
               DepthSoFar = BottomOfLayer;
               }
            }
         return Strings;
         }
      static public double[] ToThickness(string[] DepthStrings)
         {
         double[] Thickness = new double[DepthStrings.Length];
         for (int i = 0; i != DepthStrings.Length; i++)
            {
            if (DepthStrings[i] == "")
               Thickness[i] = MathUtility.MissingValue;
            else
               {
               int PosDash = DepthStrings[i].IndexOf('-');
               if (PosDash == -1)
                  throw new Exception("Invalid layer string: " + DepthStrings[i] +
                            ". String must be of the form: 10-30");

               int TopOfLayer = Convert.ToInt32(DepthStrings[i].Substring(0, PosDash));
               int BottomOfLayer = Convert.ToInt32(DepthStrings[i].Substring(PosDash + 1));
               Thickness[i] = (BottomOfLayer - TopOfLayer);
               }
            }
         return Thickness;
         }
      static public double[] ToCumThickness(double[] Thickness)
         {
         // ------------------------------------------------
         // Return cumulative thickness for each layer - mm
         // ------------------------------------------------
         double[] CumThickness = new double[Thickness.Length];
         if (Thickness.Length > 0)
            {
            CumThickness[0] = Thickness[0];
            for (int Layer = 1; Layer != Thickness.Length; Layer++)
               CumThickness[Layer] = Thickness[Layer] + CumThickness[Layer - 1];
            }
         return CumThickness;
         }
      static public double[] ToMidPoints(double[] Thickness)
         {
         //-------------------------------------------------------------------------
         // Return cumulative thickness midpoints for each layer - mm
         //-------------------------------------------------------------------------
         double[] CumThickness = ToCumThickness(Thickness);
         double[] MidPoints = new double[CumThickness.Length];
         for (int Layer = 0; Layer != CumThickness.Length; Layer++)
            {
            if (Layer == 0)
               MidPoints[Layer] = CumThickness[Layer] / 2.0;
            else
               MidPoints[Layer] = (CumThickness[Layer] + CumThickness[Layer - 1]) / 2.0;
            }
         return MidPoints;
         }
      static public double[] ToYForPlotting(double[] Thickness)
         {
         // ------------------------------------------------------------
         // Return an array of thickness (Y) values used in depth plots.
         // ------------------------------------------------------------
         double[] ReturnValues = new double[(Thickness.Length - 1) * 3 + 2];

         double CumThickness = 0;
         int Index = 0;
         for (int Layer = 0; Layer != Thickness.Length; Layer++)
            {
            ReturnValues[Index] = CumThickness;
            CumThickness += Thickness[Layer];
            ReturnValues[Index + 1] = CumThickness;
            Index += 2;
            if (Layer != Thickness.Length - 1)
               {
               ReturnValues[Index] = CumThickness;
               Index++;
               }
            }
         return ReturnValues;
         }
      static public double[] ToXForPlotting(double[] Values)
         {
         if (Values.Length <= 0)
            return new double[0];
         double[] ReturnValues = new double[(Values.Length - 1) * 3 + 2];

         int Index = 0;
         for (int Layer = 0; Layer != Values.Length; Layer++)
            {
            ReturnValues[Index] = Values[Layer];
            ReturnValues[Index + 1] = Values[Layer];
            Index += 2;
            if (Layer != Values.Length - 1)
               {
               ReturnValues[Index] = Values[Layer + 1];
               Index++;
               }
            }
         return ReturnValues;
         }
      }


   public class SoilLayerMap
      {
      /// <summary>
      /// Map the specified variable value to the target thickness.
      /// </summary>
      internal static void Map(VariableValue Value, double[] ToThickness, Soil S)
         {
         if (Value.Doubles == null)
            return;
         if (Value.Name == "Thickness")
            Value.Doubles = ToThickness;
         else
            Value.Doubles = MapToTarget(Value.Name + "(" + Value.Units + ")", Value.Doubles, Value.ThicknessMM, ToThickness, S);

         Value.ThicknessMM = ToThickness;
         Value.Codes = new string[ToThickness.Length];
         for (int i = 0; i < Value.Codes.Length; i++)
            Value.Codes[i] = "Mapped";
         }

      /// <summary>
      /// Map the values/thickness passed in, into the target layer 
      /// thicknesses. Uses the variable name to determine the method.
      /// </summary>
      private static double[] MapToTarget(string VariableName, double[] Values, double[] Thickness,
                                          double[] ToThickness, Soil S)
         {
         // If the variable has a space in it then it means that there is a crop name. 
         // Assume the second word is the variable name.
         string[] Words = VariableName.Split(" ".ToCharArray(), StringSplitOptions.RemoveEmptyEntries);
         if (Words.Length == 2)
            VariableName = Words[1];

         string Units = StringManip.SplitOffBracketedValue(ref VariableName, '(', ')');
         if (MathUtility.AreEqual(Thickness, ToThickness))
            return Values;

         string DefaultValueString = SoilMetaData.Instance.MetaData(VariableName, "DefaultValue");
         if (SoilMetaData.Instance.MetaData(VariableName, "MapMethod") == "Spatial" || Units == "ppm")
            {
            if (Units == "mm")
               Values = MathUtility.Divide(Values, Thickness);
            if (DefaultValueString == "")
               Values = MapToTargetUsingSpatial(Values, Thickness, ToThickness);
            else if (DefaultValueString == "LL15")
               Values = MapToTargetUsingSpatial(Values, Thickness, S.Variable("LL15(mm/mm)"), ToThickness);
            else
               Values = MapToTargetUsingSpatial(Values, Thickness, Convert.ToDouble(DefaultValueString), ToThickness);
            if (Units == "mm")
               Values = MathUtility.Multiply(Values, ToThickness);
            return Values;
            }
         else if (SoilMetaData.Instance.MetaData(VariableName, "MapMethod") == "Mass")
            {
            return MapToTargetUsingMass(Values, Thickness, Convert.ToDouble(DefaultValueString), ToThickness, S.Variable("BD(g/cc)"));
            }
         else
            {
            // default using spatial ???
            return MapToTargetUsingSpatial(Values, Thickness, ToThickness);

            //throw new Exception("Cannot map soil variable - invalid soil variable name: " + VariableName);
            }
         }


      /// <summary>
      /// Return interpolated values that match the "target" thicknesses
      /// using a simple spatial interpolation. When a ToThickness layer
      /// is below the last FromThickness layer, the last FromValue will be
      /// used.
      private static double[] MapToTargetUsingSpatial(double[] FromValues, double[] FromThickness,
                                                      double[] ToThickness)
         {
         if (FromValues.Length > 0)
            {
            double DefaultValue = FromValues[FromValues.Length - 1];
            return MapToTargetUsingSpatial(FromValues, FromThickness, DefaultValue, ToThickness);
            }
         return FromValues;
         }


      /// <summary>
      /// Return interpolated values that match the "target" thicknesses
      /// using a simple spatial interpolation. When a ToThickness layer
      /// is below the last FromThickness layer, the DefaultValue will be
      /// used. 
      /// </summary>
      private static double[] MapToTargetUsingSpatial(double[] FromValues, double[] FromThickness, double DefaultValue,
                                                      double[] ToThickness)
         {
         double[] DefaultValues = new double[ToThickness.Length];
         for (int i = 0; i < ToThickness.Length; i++)
            DefaultValues[i] = DefaultValue;
         return MapToTargetUsingSpatial(FromValues, FromThickness, DefaultValues, ToThickness);
         }

      /// <summary>
      /// Return interpolated values that match the "target" thicknesses
      /// using a simple spatial interpolation. When a ToThickness layer
      /// is below the last FromThickness layer, the DefaultValue will be
      /// used. 
      /// </summary>
      private static double[] MapToTargetUsingSpatial(double[] FromValues, double[] FromThickness, double[] DefaultValues,
                                                      double[] ToThickness)
         {
         CreateVariableForMapping(ref FromValues, ref FromThickness, DefaultValues, ToThickness);
         FromValues = MathUtility.Multiply(FromValues, FromThickness);
         FromValues = SpatialRedistributeInternal(FromValues, FromThickness, ToThickness);
         return MathUtility.Divide(FromValues, ToThickness);
         }


      /// <summary>
      /// Return interpolated values that match the "to" thicknesses passed in
      /// using a mass method i.e. by first multiplying Values by BD to convert
      /// them to a mass. When a ToThickness layer is below the last 
      /// FromThickness layer, the specified DefaultValue will be used.
      /// </summary>
      private static double[] MapToTargetUsingMass(double[] FromValues, double[] FromThickness, double DefaultValue,
                                                   double[] ToThickness, double[] BD)
         {
         double[] DefaultValues = new double[ToThickness.Length];
         for (int i = 0; i < ToThickness.Length; i++)
            DefaultValues[i] = DefaultValue;
         return MapUsingMassInternal(FromValues, FromThickness, DefaultValues, ToThickness, BD);
         }

      /// <summary>
      /// Return interpolated values that match the "to" thicknesses passed in
      /// using a simple spatial interpolation. When a ToThickness layer
      /// is below the last FromThickness layer, the last FromValue will be
      /// used. This is the default behaviour of LinearInterpReal.
      /// </summary>
      private static double[] MapUsingSpatialInternal(double[] FromValues, double[] FromThickness, double[] ToThickness)
         {
         if (MathUtility.AreEqual(FromThickness, ToThickness))
            return FromValues;

         double[] FromMass = MathUtility.Multiply(FromValues, FromThickness);
         double[] ToMass = SpatialRedistributeInternal(FromMass, FromThickness, ToThickness);
         return MathUtility.Divide(ToMass, ToThickness);
         }


      /// <summary>
      /// Return interpolated values that match the "to" thicknesses passed in
      /// using a mass method i.e. by first multiplying Values by BD to convert
      /// them to a mass. When a ToThickness layer is below the last 
      /// FromThickness layer, the specified DefaultValues will be used.
      /// </summary>
      private static double[] MapUsingMassInternal(double[] FromValues, double[] FromThickness,
                                                   double[] DefaultValues, double[] ToThickness, double[] ToBD)
         {
         if (DefaultValues.Length > 0)
            {
            CreateVariableForMapping(ref FromValues, ref FromThickness, DefaultValues, ToThickness);
            return MassRedistributeInternal(FromValues, FromThickness, ToThickness, ToBD);
            }
         else
            return new double[0];
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
            bool DidInterpolate = false;
            double CumMassTop = MathUtility.LinearInterpReal(LayerTop, CumDepth,
                CumMass, ref DidInterpolate);
            double CumMassBottom = MathUtility.LinearInterpReal(LayerBottom, CumDepth,
                CumMass, ref DidInterpolate);
            ToMass[Layer - 1] = CumMassBottom - CumMassTop;
            }
         return ToMass;
         }


      /// <summary>
      /// Mass Redistribution algorithm
      /// </summary>
      private static double[] MassRedistributeInternal(double[] FromValues, double[] FromThickness,
                                                       double[] ToThickness, double[] ToBd)
         {
         // Firstly we need to convert the values passed in, into a mass using
         // bulk density.

         double[] FromBd = MapUsingSpatialInternal(ToBd, ToThickness, FromThickness);
         double[] FromMass = new double[FromValues.Length];
         for (int Layer = 0; Layer < FromValues.Length; Layer++)
            FromMass[Layer] = FromValues[Layer] * FromBd[Layer] * FromThickness[Layer] / 100;

         // spatially interpolate mass.
         double[] ToMass = SpatialRedistributeInternal(FromMass, FromThickness, ToThickness);

         //now convert mass back to original values.
         double[] ToValues = new double[ToMass.Length];
         for (int Layer = 0; Layer < ToMass.Length; Layer++)
            ToValues[Layer] = ToMass[Layer] * 100.0 / ToBd[Layer] / ToThickness[Layer];

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
   }
