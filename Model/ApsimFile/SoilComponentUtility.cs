
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

      /// <summary>
      /// Return a full code name from the abbreviated code passed in.
      /// </summary>
      internal string GetFullCodeName(string CodeText, string RawVariableName)
         {
         ReadMetaData();
         // If this is a crop variable then remove the crop bit.
         int PosSpace = RawVariableName.IndexOf(' ');
         if (PosSpace != -1)
            RawVariableName = RawVariableName.Remove(0, PosSpace+1);
         XmlNode CodeNode = XmlHelper.FindRecursively(MetaDataNode, RawVariableName);
         if (CodeNode != null)
            {
            foreach (XmlNode Code in XmlHelper.ChildNodes(CodeNode, "Code"))
               {
               if (Code.InnerText == CodeText)
                  return XmlHelper.Attribute(Code, "description");
               }
            return CodeText;
            }
         else
            return CodeText;
         }
      }

   public class SoilUtility
      {
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


   }
