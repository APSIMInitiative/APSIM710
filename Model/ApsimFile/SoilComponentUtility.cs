
using System;
using System.Collections;
using System.Collections.Generic;
using System.Collections.Specialized;
using System.Xml;

using Microsoft.VisualBasic;

using CSGeneral;

namespace ApsimFile
    {
	public class SoilComponentUtility
		{
        // -----------------------------------------
        // Utility class that other soil classes use
        // -----------------------------------------


        public static string GetStringValue(XmlNode Data, string PropertyType, string PropertyName)
			{
            // ----------------------------------------------------
            // Return a string value to caller for specified child.
            // ----------------------------------------------------
            string Key;
			if (PropertyType != "")
				Key = PropertyType + "/" + PropertyName;
			else
				Key = PropertyName;
			return XmlHelper.Value(Data, Key);
			}
		public static double GetDoubleValue(XmlNode Data, string PropertyType, string PropertyName)
			{
            // ----------------------------------------------------
            // Return a double value to caller for specified child.
            // ----------------------------------------------------
            string Value = GetStringValue(Data, PropertyType, PropertyName);
            if (Value == "")
				return MathUtility.MissingValue;			
			else
				return Convert.ToDouble(Value);
			}
		public static void SetValue(XmlNode Data, string PropertyType, string PropertyName, double Value)
			{
            // ----------------------------------------------------
            // Set a string value for specified property.
            // ----------------------------------------------------
            string StringValue;
			if (Value == MathUtility.MissingValue)
				StringValue = "";
			else
				StringValue = Value.ToString();
			SetValue(Data, PropertyType, PropertyName, StringValue);
			}
        public static void SetValue(XmlNode Data, string PropertyType, string PropertyName, string Value)
			{
            // ----------------------------------------------------
            // Return a double value to caller for specified child.
            // ----------------------------------------------------
            string Key;
			if (PropertyType != "")
				Key = PropertyType + "/" + PropertyName;
			else
				Key = PropertyName;

			if (Value != "")
				XmlHelper.SetValue(Data, Key, Value);
			else
				{
				if (PropertyType == "")
					{
					if (XmlHelper.Find(Data, PropertyName) != null)
						Data.RemoveChild(XmlHelper.Find(Data, PropertyName));
					}
				else
					{
					XmlNode Child = XmlHelper.Find(Data, PropertyType + "/" + PropertyName);
					if (Child != null)
                        {
						Child.ParentNode.RemoveChild(Child);
                        }
					}
				}
			}
        public static string[] getLayeredAsStrings(XmlNode Data, string ParentNodeName, string propertyType, string propertyName)
            {
            // ---------------------------------------
            // Return a layered variable as strings
            // ---------------------------------------
            XmlNode Profile;
            if (ParentNodeName == "")
                Profile = Data;
            else
                Profile = XmlHelper.Find(Data, ParentNodeName);
            if (Profile != null)
                {
                List<XmlNode> LayerNodes = XmlHelper.ChildNodes(Profile, "layer");
                string[] values = new string[LayerNodes.Count];
                int index = 0;
                foreach (XmlNode layer in LayerNodes)
                    {
                    XmlNode MatchingValueNode = FindNodeByTypeAndName(layer, propertyType, propertyName);
                    if (MatchingValueNode != null && MatchingValueNode.InnerXml != null)
                        values[index] = MatchingValueNode.InnerXml;
                    else
                        values[index] = "";
                    index++;
                    }
                return values;
                }
            else
                return new string[0];
            }
        private static XmlNode FindNodeByTypeAndName(XmlNode ParentNode, string Type, string Name)
            {
            // ----------------------------------------------------------
            // Find a child name under Parent that matches the specified
            // type and name. 
            // ----------------------------------------------------------
            foreach (XmlNode Child in XmlHelper.ChildNodes(ParentNode, ""))
                {
                bool ThisChildMatches = false;
                if (Type != "" && Name != "")
                    ThisChildMatches = (XmlHelper.Type(Child).ToLower() == Type.ToLower() &&
                                        XmlHelper.Name(Child).ToLower() == Name.ToLower());
                else if (Type == "" && Name != "")
                    ThisChildMatches = (XmlHelper.Name(Child).ToLower() == Name.ToLower());
                else if (Type != "" && Name == "")
                    ThisChildMatches = (XmlHelper.Type(Child).ToLower() == Type.ToLower());

                if (ThisChildMatches)
                    return Child;
                }
            return null;
            }
        public static double[] getLayered(XmlNode Data, string ParentNodeName, string propertyType, string propertyName)
            {
            // ---------------------------------------
            // Return a layered variable as doubles.
            // ---------------------------------------
            string[] StringValues = getLayeredAsStrings(Data, ParentNodeName, propertyType, propertyName);
			double[] values = new double[StringValues.Length];
            int Index = 0;
			foreach (string StringValue in StringValues)
				{
				if (StringValue == "")
					values[Index] = MathUtility.MissingValue;
				else
					{
					values[Index] = Convert.ToDouble(StringValue);
					if (values[Index] < -100000 || values[Index] > 100000)
						values[Index] = MathUtility.MissingValue;
					}
                Index++;
				}
			return values;
			}
        public static void setLayeredAsStrings(XmlNode Data, string ParentNodeName, string PropertyType, string PropertyName, string[] Values)
            {
            //--------------------------------------------------------------------------
            // Sets the values of the specified node as strings.
            //--------------------------------------------------------------------------	
            XmlNode Profile;
            if (ParentNodeName == "")
                Profile = Data;
            else
                Profile = XmlHelper.Find(Data, ParentNodeName);
            if (Profile == null)
                Profile = Data.AppendChild(Data.OwnerDocument.CreateElement("profile"));


            // if values is zero length then the caller wants to delete a property
            // from all layers.
            if (Values.Length == 0)
                {
                int NumLayers = XmlHelper.ChildNodes(Profile, "layer").Count;
                Values = new string[NumLayers];
                for (int i = 0; i != NumLayers; i++)
                    Values[i] = "";
                }

            // make sure we have the right amount of layer nodes.
            XmlHelper.EnsureNumberOfChildren(Profile, "layer", "", Values.Length);

            List<XmlNode> Layers = XmlHelper.ChildNodes(Profile, "layer");
            for (int i = 0; i != Values.Length; i++)
				{
                XmlNode MatchingValueNode = FindNodeByTypeAndName(Layers[i], PropertyType, PropertyName);
                if (MatchingValueNode == null)
                    {
                    if (Values[i] != "")
                        MatchingValueNode = Layers[i].AppendChild(XmlHelper.CreateNode(Layers[i].OwnerDocument, PropertyType, PropertyName));
                    }
                else if (Values[i] == "")
                    Layers[i].RemoveChild(MatchingValueNode);
                if (MatchingValueNode != null)
                    MatchingValueNode.InnerText = Values[i];
				}
			}
        public static void setLayered(XmlNode Data, string ParentNodeName, string PropertyType, string PropertyName, double[] Values)
            {
            //--------------------------------------------------------------------------
            // Sets the values of the specified node as doubles
            //--------------------------------------------------------------------------	
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
            setLayeredAsStrings(Data, ParentNodeName, PropertyType, PropertyName, StringValues);
            }
        public static void DeleteLayered(XmlNode Data, string ParentNodeName, string PropertyType, string PropertyName)
            {
            //--------------------------------------------------------------------------
            // Deletes the values of the specified property
            //--------------------------------------------------------------------------	
            string[] StringValues = new string[0];
            setLayeredAsStrings(Data, ParentNodeName, PropertyType, PropertyName, StringValues);
            }
        public static string[] ToDepthStrings(double[] Thickness)
            {
            // ------------------------------------------------------------------
            // Convert an array of thickness(mm) to depth strings(cm)
            //    e.g. "0-10", "10-30"
            // ------------------------------------------------------------------
            string[] Strings = new string[Thickness.Length];
            int DepthSoFar = 0;
            for (int i = 0; i != Thickness.Length; i++)
                {
                int ThisThickness = Convert.ToInt32(Thickness[i]) / 10;   // to cm
                int TopOfLayer = DepthSoFar;
                int BottomOfLayer = DepthSoFar + ThisThickness;
                Strings[i] = TopOfLayer.ToString() + "-" + BottomOfLayer.ToString();
                DepthSoFar = BottomOfLayer;
                }
            return Strings;
            }
        public static double[] ToThickness(string[] DepthStrings)
		    {
            double[] Thickness = new double[DepthStrings.Length];
            for (int i = 0; i != DepthStrings.Length; i++)
				{
                int PosDash = DepthStrings[i].IndexOf('-');
				if (PosDash == -1)
                    throw new Exception("Invalid layer string: " + DepthStrings[i] +
										". String must be of the form: 10-30");

                int TopOfLayer = Convert.ToInt32(DepthStrings[i].Substring(0, PosDash));
                int BottomOfLayer = Convert.ToInt32(DepthStrings[i].Substring(PosDash + 1));
				Thickness[i] = (BottomOfLayer - TopOfLayer) * 10;
				}
			return Thickness;
			}
		public static double[] ToCumThickness(double[] Thickness)
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
		public static double[] ToMidPoints(double[] Thickness)
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
				ReturnValues[Index+1] = CumThickness;
				Index += 2;
				if (Layer != Thickness.Length-1)
					{
					ReturnValues[Index] = CumThickness;
					Index++;
					}
				}
			return ReturnValues;
			}
		// ------------------------------------------------------------
		// Return an array of (X) values used in depth plots.
		// ------------------------------------------------------------
		static public double[] ToXForPlotting(double[] Values)
			{
			if (Values.Length <= 0)
				return new double[0];
			double[] ReturnValues = new double[(Values.Length-1)*3+2];

			int Index = 0;
			for (int Layer = 0; Layer != Values.Length; Layer++)
				{
				ReturnValues[Index] = Values[Layer];
				ReturnValues[Index+1] = Values[Layer];
				Index += 2;
				if (Layer != Values.Length-1)
					{
					ReturnValues[Index] = Values[Layer+1];
					Index++;
					}
				}
			return ReturnValues;
			}

        public static string LayeredToString(double[] Values)
            {
            // ------------------------------------------------------------------
            // Convert an array of values to a single space separated string
            //    e.g. "100.000    300.000   300.000"
            // ------------------------------------------------------------------
            string ReturnString = "";

            foreach (double Value in Values)
                {
                string StringValue = MathUtility.Round(Value, 3).ToString("f3");
                ReturnString += new string(' ', 10 - StringValue.Length) + StringValue;
                }
            return ReturnString;
            }
        #region Soil mapping methods
        public static double[] MapSoilToSampleUsingSpatial(double[] FromValues, double[] FromThickness, double[] ToThickness)
            {
            // ----------------------------------------------------------------
            // Interpolate some BD values that match this sample's thicknesses.
            // ----------------------------------------------------------------
            double[] FromMass = MathUtility.Multiply(FromValues, FromThickness);
            double[] ToMass = SpatialRedistribute(FromMass, FromThickness, ToThickness);
            return MathUtility.Divide(ToMass, ToThickness);
            }
        public static double[] MapSampleToSoilUsingMass(double[] FromValues, double[] FromThickness,
                                                         double[] DefaultValues, double[] ToThickness, double[] ToBD)
            {
            // ------------------------------------------------------------------------
            // Map the specified values to the linked soil by first converting to
            // a mass value i.e. by first multiplying Values by BD. 
            // The result will be a set of values that correspond to the linked soil layers.
            // ------------------------------------------------------------------------
            if (DefaultValues.Length > 0)
                {
                CreateVariableForMapping(ref FromValues, ref FromThickness, DefaultValues, ToThickness);
                return MassRedistribute(FromValues, FromThickness, ToThickness, ToBD);
                }
            else
                return new double[0];
            }
        public static double[] MapSampleToSoilUsingSpatial(double[] FromValues, double[] FromThickness,
                                                            double[] DefaultValues, double[] ToThickness)
            {
            // ------------------------------------------------------------------------
            // Map the specified values to the linked soil using a simple spatial
            // interpolation. The result will be a set of values that correspond 
            // to the linked soil layers.
            // ------------------------------------------------------------------------
            if (DefaultValues.Length > 0)
                {
                CreateVariableForMapping(ref FromValues, ref FromThickness,
                                        DefaultValues, ToThickness);
                FromValues = MathUtility.Multiply(FromValues, FromThickness);
                FromValues = SpatialRedistribute(FromValues, FromThickness, ToThickness);
                return MathUtility.Divide(FromValues, ToThickness);
                }
            else
                return new double[0];
            }
        private static double[] SpatialRedistribute(double[] FromMass, double[] FromThickness,
                                                    double[] ToThickness)
            {
            //-------------------------------------------------------------------------
            //Spatial mass redistribution algorithm.
            //-------------------------------------------------------------------------
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
        private static double[] MassRedistribute(double[] FromValues, double[] FromThickness,
                                                 double[] ToThickness, double[] ToBd)
            {
            //-------------------------------------------------------------------------
            //Mass Redistribution algorithm
            //-------------------------------------------------------------------------
            // Firstly we need to convert the values passed in, into a mass using
            // bulk density.

            double[] FromBd = MapSoilToSampleUsingSpatial(ToBd, ToThickness, FromThickness);
            double[] FromMass = new double[FromValues.Length];
            for (int Layer = 0; Layer < FromValues.Length; Layer++)
                FromMass[Layer] = FromValues[Layer] * FromBd[Layer] * FromThickness[Layer] / 100;

            // spatially interpolate mass.
            double[] ToMass = SpatialRedistribute(FromMass, FromThickness, ToThickness);

            //now convert mass back to original values.
            double[] ToValues = new double[ToMass.Length];
            for (int Layer = 0; Layer < ToMass.Length; Layer++)
                ToValues[Layer] = ToMass[Layer] * 100.0 / ToBd[Layer] / ToThickness[Layer];

            return ToValues;
            }
        private static void CreateVariableForMapping(ref double[] SampleValues, ref double[] SampleThickness,
                                                     double[] SoilValues, double[] SoilThickness)
            {
            //-------------------------------------------------------------------------
            // Remaps the thicknesses and values to more closely match the specified 
            // soil thickness and values. This algorithm removes all missing values
            // and their associated depths.
            // e.g. IF             SoilThickness  Values   SampleThickness	SampleValues
            //                           0-100		2         0-100				10
            //                         100-250	    3		100-600				11
            //                         250-500		4		
            //                         500-750		5
            //                         750-900		6
            //						   900-1200		7
            //                        1200-1500		8
            //                        1500-1800		9
            //
            // will produce:		SampleThickness			Values
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
        #endregion



		}
	}
