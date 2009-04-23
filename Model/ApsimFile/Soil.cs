
using System;
using System.Collections.Generic;
using System.Collections.Specialized;
using System.IO;
using System.Xml;

using CSGeneral;


namespace ApsimFile
   {
   // ---------------------------------
   // A class for encapsulating a soil
   // ---------------------------------
   public class Soil
      {
      public XmlNode Data;
      public Soil(XmlNode data)
         {
         Data = data;
         }

      #region General soil descriptive properties

      public string Name
         {
         get { return XmlHelper.Name(Data); }
         set { XmlHelper.SetName(Data, value); }
         }
      public string State
         {
             get { return SoilComponentUtility.GetStringValue(Data, "", "state"); }
             set { SoilComponentUtility.SetValue(Data, "", "state", value); }
         }
      public string Region
         {
             get { return SoilComponentUtility.GetStringValue(Data, "", "region"); }
             set { SoilComponentUtility.SetValue(Data, "", "region", value); }
         }
      public string NearestTown
         {
             get { return SoilComponentUtility.GetStringValue(Data, "", "nearesttown"); }
             set { SoilComponentUtility.SetValue(Data, "", "nearesttown", value); }
         }
      public string Site
         {
             get { return SoilComponentUtility.GetStringValue(Data, "", "site"); }
             set { SoilComponentUtility.SetValue(Data, "", "site", value); }
         }
      public string Classification
         {
         get { return SoilComponentUtility.GetStringValue(Data, "", "SoilType"); }
         set { SoilComponentUtility.SetValue(Data, "", "SoilType", value); }
         }
      public string ApsoilNumber
         {
         get { return SoilComponentUtility.GetStringValue(Data, "", "ApsoilNumber"); }
         set { SoilComponentUtility.SetValue(Data, "", "ApsoilNumber", value); }
         }
      public string DataSource
         {
         get { return SoilComponentUtility.GetStringValue(Data, "", "datasource"); }
         set { SoilComponentUtility.SetValue(Data, "", "DataSource", value); }
         }
      public string Comment
         {
         get { return SoilComponentUtility.GetStringValue(Data, "", "comment"); }
         set { SoilComponentUtility.SetValue(Data, "", "comment", value); }
         }
      public string NaturalVegetation
         {
         get { return SoilComponentUtility.GetStringValue(Data, "", "naturalvegetation"); }
         set { SoilComponentUtility.SetValue(Data, "", "naturalvegetation", value); }
         }
      public bool UseEC
         {
         get { return (SoilComponentUtility.GetStringValue(Data, "", "UseEC").ToLower() == "yes"); }
         set
            {
            if (value)
               SoilComponentUtility.SetValue(Data, "", "UseEC", "yes");
            else
               SoilComponentUtility.SetValue(Data, "", "UseEC", "no");
            }
         }
      public int MaxRootDepth
         {
         get
            {
            string StringValue = SoilComponentUtility.GetStringValue(Data, "", "MaxRootDepth");
            if (StringValue != "")
               return Convert.ToInt32(StringValue);
            else
               return 0;
            }
         set
            {
            SoilComponentUtility.SetValue(Data, "", "MaxRootDepth", value.ToString());
            }
         }
      public string AttachmentFileName
         {
         get { return SoilComponentUtility.GetStringValue(Data, "attachment", "filename"); }
         set
            {
            if (File.Exists(value))
               {
               SoilComponentUtility.SetValue(Data, "attachment", "filename", Path.GetFileName(value));
               FileStream fs = File.OpenRead(value);
               int NumBytes = Convert.ToInt32(fs.Length);
               byte[] buffer = new byte[NumBytes + 1];
               fs.Read(buffer, 0, NumBytes);
               string Contents = Convert.ToBase64String(buffer);
               SoilComponentUtility.SetValue(Data, "attachment", "bytes", Contents);
               fs.Close();
               fs = null;
               }
            else
               SoilComponentUtility.SetValue(Data, "", "attachment", "");
            }
         }
      public string Attachment
         {
         get { return SoilComponentUtility.GetStringValue(Data, "attachment", "bytes"); }
         }
      public string CreateAttachment()
         {
         if (AttachmentFileName != "")
            {
            string TempFileName = Path.GetTempPath() + AttachmentFileName;
            FileStream fs = new FileStream(TempFileName, FileMode.Create);
            byte[] buffer = Convert.FromBase64String(SoilComponentUtility.GetStringValue(Data, "attachment", "bytes"));
            fs.Write(buffer, 0, buffer.Length);
            fs.Close();
            fs = null;
            return TempFileName;
            }
         else
            return "";
         }


      public double Latitude
         {
         get
            {
            string StringValue = SoilComponentUtility.GetStringValue(Data, "", "Latitude");
            if (StringValue != "")
               return Convert.ToDouble(StringValue);
            else
               return 0;
            }
         set
            {
            SoilComponentUtility.SetValue(Data, "", "Latitude", value.ToString());
            }
         }
      public double Longitude
         {
         get
            {
            string StringValue = SoilComponentUtility.GetStringValue(Data, "", "Longitude");
            if (StringValue != "")
               return Convert.ToDouble(StringValue);
            else
               return 0;
            }
         set
            {
            SoilComponentUtility.SetValue(Data, "", "Longitude", value.ToString());
            }
         }
      public string LocationAccuracy
         {
         get
            {
            return SoilComponentUtility.GetStringValue(Data, "", "LocationAccuracy");
            }
         set
            {
            SoilComponentUtility.SetValue(Data, "", "LocationAccuracy", value);
            }
         }
      #endregion

      public double[] Values(string DataType)
         {
         // ---------------------------------------------------------
         // Generic method to return the values of a profile variable
         // as specified by the DataType passed in.
         // ---------------------------------------------------------
         return SoilComponentUtility.getLayered(Data, "profile", DataType, "");
         }
      #region Profile
      public double[] Thickness
         {
         get { return SoilComponentUtility.getLayered(Data, "profile", "thickness", ""); }
         set { SoilComponentUtility.setLayered(Data, "profile", "thickness", "", value); }
         }
      public double[] LL15
         {
         get { return SoilComponentUtility.getLayered(Data, "profile", "ll15", ""); }
         set { SoilComponentUtility.setLayered(Data, "profile", "ll15", "", value); }
         }
      public double[] Airdry
         {
         get { return SoilComponentUtility.getLayered(Data, "profile", "airdry", ""); }
         set { SoilComponentUtility.setLayered(Data, "profile", "airdry", "", value); }
         }
      public double[] DUL
         {
         get { return SoilComponentUtility.getLayered(Data, "profile", "dul", ""); }
         set { SoilComponentUtility.setLayered(Data, "profile", "dul", "", value); }
         }
      public double[] SAT
         {
         get { return SoilComponentUtility.getLayered(Data, "profile", "sat", ""); }
         set { SoilComponentUtility.setLayered(Data, "profile", "sat", "", value); }
         }
      public double[] BD
         {
         get { return SoilComponentUtility.getLayered(Data, "profile", "bd", ""); }
         set { SoilComponentUtility.setLayered(Data, "profile", "bd", "", value); }
         }
      public double[] Rocks
         {
         get { return SoilComponentUtility.getLayered(Data, "profile", "Rocks", ""); }
         set { SoilComponentUtility.setLayered(Data, "profile", "Rocks", "", value); }
         }
      public double[] SWCON
         {
         get { return SoilComponentUtility.getLayered(Data, "profile", "swcon", ""); }
         set { SoilComponentUtility.setLayered(Data, "profile", "swcon", "", value); }
         }
      public double[] MWCON
         {
         get { return SoilComponentUtility.getLayered(Data, "profile", "mwcon", ""); }
         set { SoilComponentUtility.setLayered(Data, "profile", "mwcon", "", value); }
         }
      public double[] FBIOM
         {
         get { return SoilComponentUtility.getLayered(Data, "profile", "fbiom", ""); }
         set { SoilComponentUtility.setLayered(Data, "profile", "fbiom", "", value); }
         }
      public double[] FINERT
         {
         get { return SoilComponentUtility.getLayered(Data, "profile", "finert", ""); }
         set { SoilComponentUtility.setLayered(Data, "profile", "finert", "", value); }
         }
      public double[] KS
         {
         get { return SoilComponentUtility.getLayered(Data, "profile", "ks", ""); }
         set { SoilComponentUtility.setLayered(Data, "profile", "ks", "", value); }
         }
      public double[] OC
         {
         get { return SoilComponentUtility.getLayered(Data, "profile", "oc", ""); }
         set { SoilComponentUtility.setLayered(Data, "profile", "oc", "", value); }
         }
      public bool PHStoredAsWater() { return (SoilComponentUtility.GetStringValue(Data, "profile", "phunits") != "CaCl"); }
      public double[] PH          // pH in water = (pH in CaCl X 1.1045) - 0.1375
         {
         get
            {
            if (PHStoredAsWater())
               return SoilComponentUtility.getLayered(Data, "profile", "ph", "");
            else
               return MathUtility.Subtract_Value(MathUtility.Multiply_Value(SoilComponentUtility.getLayered(Data, "profile", "ph", ""), 1.1045), 0.1375);
            }
         set
            {
            SoilComponentUtility.SetValue(Data, "profile", "phunits", "");
            SoilComponentUtility.setLayered(Data, "profile", "ph", "", value);
            }
         }
      public double[] PHCaCl
         {
         get
            {
            if (PHStoredAsWater())
               return MathUtility.Add_Value(MathUtility.Divide_Value(SoilComponentUtility.getLayered(Data, "profile", "ph", ""), 1.1045), 0.1375);
            else
               return SoilComponentUtility.getLayered(Data, "profile", "ph", "");
            }
         set
            {
            SoilComponentUtility.setLayered(Data, "profile", "ph", "", value);
            SoilComponentUtility.SetValue(Data, "profile", "phunits", "CaCl");
            }
         }
      public double[] EC
         {
         get { return SoilComponentUtility.getLayered(Data, "profile", "ec", ""); }
         set { SoilComponentUtility.setLayered(Data, "profile", "ec", "", value); }
         }
      public double[] CL
         {
         get { return SoilComponentUtility.getLayered(Data, "profile", "cl", ""); }
         set { SoilComponentUtility.setLayered(Data, "profile", "cl", "", value); }
         }
      public double[] Boron
         {
         get { return SoilComponentUtility.getLayered(Data, "profile", "boron", ""); }
         set { SoilComponentUtility.setLayered(Data, "profile", "boron", "", value); }
         }
      public double[] CEC
         {
         get { return SoilComponentUtility.getLayered(Data, "profile", "cec", ""); }
         set { SoilComponentUtility.setLayered(Data, "profile", "cec", "", value); }
         }
      public double[] Ca
         {
         get { return SoilComponentUtility.getLayered(Data, "profile", "ca", ""); }
         set { SoilComponentUtility.setLayered(Data, "profile", "ca", "", value); }
         }
      public double[] Mg
         {
         get { return SoilComponentUtility.getLayered(Data, "profile", "Mg", ""); }
         set { SoilComponentUtility.setLayered(Data, "profile", "Mg", "", value); }
         }
      public double[] Na
         {
         get { return SoilComponentUtility.getLayered(Data, "profile", "Na", ""); }
         set { SoilComponentUtility.setLayered(Data, "profile", "Na", "", value); }
         }
      public double[] Mn
         {
         get { return SoilComponentUtility.getLayered(Data, "profile", "Mn", ""); }
         set { SoilComponentUtility.setLayered(Data, "profile", "Mn", "", value); }
         }
      public double[] Al
         {
         get { return SoilComponentUtility.getLayered(Data, "profile", "Al", ""); }
         set { SoilComponentUtility.setLayered(Data, "profile", "Al", "", value); }
         }
      public double[] K
         {
         get { return SoilComponentUtility.getLayered(Data, "profile", "K", ""); }
         set { SoilComponentUtility.setLayered(Data, "profile", "K", "", value); }
         }
      public double[] ESP
         {
         get { return SoilComponentUtility.getLayered(Data, "profile", "esp", ""); }
         set { SoilComponentUtility.setLayered(Data, "profile", "esp", "", value); }
         }
      public double[] ParticleSizeSand
         {
         get { return SoilComponentUtility.getLayered(Data, "profile", "ParticleSizeSand", ""); }
         set { SoilComponentUtility.setLayered(Data, "profile", "ParticleSizeSand", "", value); }
         }
      public double[] ParticleSizeSilt
         {
         get { return SoilComponentUtility.getLayered(Data, "profile", "ParticleSizeSilt", ""); }
         set { SoilComponentUtility.setLayered(Data, "profile", "ParticleSizeSilt", "", value); }
         }
      public double[] ParticleSizeClay
         {
         get { return SoilComponentUtility.getLayered(Data, "profile", "ParticleSizeClay", ""); }
         set { SoilComponentUtility.setLayered(Data, "profile", "ParticleSizeClay", "", value); }
         }
      public string[] Texture
         {
         get { return SoilComponentUtility.getLayeredAsStrings(Data, "profile", "texture", ""); }
         set { SoilComponentUtility.setLayeredAsStrings(Data, "profile", "texture", "", value); }
         }
      public double[] LabileP
         {
         get { return SoilComponentUtility.getLayered(Data, "profile", "LabileP", ""); }
         set { SoilComponentUtility.setLayered(Data, "profile", "LabileP", "", value); }
         }
      public double[] BandedP
         {
         get { return SoilComponentUtility.getLayered(Data, "profile", "BandedP", ""); }
         set { SoilComponentUtility.setLayered(Data, "profile", "BandedP", "", value); }
         }
      public double[] RockP
         {
         get { return SoilComponentUtility.getLayered(Data, "profile", "RockP", ""); }
         set { SoilComponentUtility.setLayered(Data, "profile", "RockP", "", value); }
         }
      public double[] Sorption
         {
         get { return SoilComponentUtility.getLayered(Data, "profile", "Sorption", ""); }
         set { SoilComponentUtility.setLayered(Data, "profile", "Sorption", "", value); }
         }

      #endregion

      #region Crop
      public string[] Crops
         {
         get
            {
            String[] ReturnListCollection = CropsMeasured;
            StringCollection PredCrops = PredictedCrops;
            string[] ReturnList = new string[ReturnListCollection.Length + PredCrops.Count];
            ReturnListCollection.CopyTo(ReturnList, 0);
            PredCrops.CopyTo(ReturnList, ReturnListCollection.Length);
            return ReturnList;
            }
         }
      public string[] CropsMeasured
         {
         get
            {
            XmlNode Profile = XmlHelper.Find(Data, "profile");
            if (Profile != null)
               {
               XmlNode Layer = XmlHelper.FindByType(Profile, "layer");
               if (Layer != null)
                  {
                  String[] ReturnListCollection = XmlHelper.ChildNames(Layer, "ll");
                  string[] ReturnList = new string[ReturnListCollection.Length];
                  ReturnListCollection.CopyTo(ReturnList, 0);
                  return ReturnList;
                  }
               }
            return new string[0];
            }
         }
      public bool CropExists(string CropName)
         {
         foreach (string MeasuredCrop in CropsMeasured)
            {
            if (MeasuredCrop.ToLower() == CropName.ToLower())
               return true;
            }
         return false;
         }
      public bool CropIsPredicted(string CropName)
         {
         return StringManip.IndexOfCaseInsensitive(Crops, CropName) != -1 && !CropExists(CropName);
         }
      public void AddCrop(string CropName)
         {
         double[] ll = LL15;
         double[] kl = new double[ll.Length];
         double[] xf = new double[ll.Length];
         for (int i = 0; i != ll.Length; i++)
            {
            kl[i] = 0.06;
            xf[i] = 1.0;
            }
         SetCrop(CropName, ll, kl, xf);
         }
      public void DeleteCrop(string CropName)
         {
         SoilComponentUtility.DeleteLayered(Data, "profile", "ll", CropName);
         SoilComponentUtility.DeleteLayered(Data, "profile", "kl", CropName);
         SoilComponentUtility.DeleteLayered(Data, "profile", "xf", CropName);
         }
      public double[] LL(string CropName)
         {
         if (CropExists(CropName))
            return SoilComponentUtility.getLayered(Data, "profile", "ll", CropName);
         else
            return PredictedLL(CropName);
         }
      public double[] KL(string CropName)
         {
         if (CropExists(CropName))
            return SoilComponentUtility.getLayered(Data, "profile", "kl", CropName);
         else
            return PredictedKL(CropName);
         }
      public double[] XF(string CropName)
         {
         if (CropExists(CropName))
            return SoilComponentUtility.getLayered(Data, "profile", "xf", CropName);
         else
            return PredictedXF(CropName);
         }
      public void SetCrop(string CropName, double[] ll, double[] kl, double[] xf)
         {
         SoilComponentUtility.setLayered(Data, "profile", "ll", CropName, ll);
         SoilComponentUtility.setLayered(Data, "profile", "kl", CropName, kl);
         SoilComponentUtility.setLayered(Data, "profile", "xf", CropName, xf);
         }

      struct SavedCrop
         {
         public string Name;
         public double[] ll;
         public double[] kl;
         public double[] xf;

         };
      public void SetCropOrder(string[] CropNames)
         {
         List<SavedCrop> Crops = new List<SavedCrop>();
         for (int i = 0; i != CropNames.Length; i++)
            {
            SavedCrop Crop;
            Crop.Name = CropNames[i];
            Crop.ll = LL(CropNames[i]);
            Crop.kl = KL(CropNames[i]);
            Crop.xf = XF(CropNames[i]);
            Crops.Add(Crop);
            DeleteCrop(CropNames[i]);
            }
         foreach (SavedCrop Crop in Crops)
            SetCrop(Crop.Name, Crop.ll, Crop.kl, Crop.xf);
         }
      #endregion

      #region Predicted Crop
      private StringCollection PredictedCrops
         {
         get
            {
            StringCollection PredCrops = new StringCollection();
            if (SoilComponentUtility.ToMidPoints(Thickness).Length > 0)
               {
               // get a list of all possible predicted crops.
               string SoilNameNoSpaces = Classification.Replace(" ", "");
               double[] SoilDepthCentre = SoilComponentUtility.ToMidPoints(Thickness);

               XmlNode PredLLCoeff = Configuration.Instance.GetSettingsNode("PredictedLLCoeff");

               foreach (XmlNode PredSoil in XmlHelper.ChildNodes(PredLLCoeff, ""))
                  if (PredSoil.Name.ToLower() == SoilNameNoSpaces.ToLower())
                     foreach (string CropName in XmlHelper.ChildNames(PredSoil, ""))
                        if (!CropExists(CropName))
                           {
                           double[] a = null;
                           double[] b = null;
                           double[] CoeffDepthCentre = null;
                           PredictedCoeffs(CropName, ref a, ref b, ref CoeffDepthCentre);
                           if (SoilDepthCentre[SoilDepthCentre.Length - 1] <= CoeffDepthCentre[CoeffDepthCentre.Length - 1])
                              PredCrops.Add(CropName);
                           }
               }
            return PredCrops;
            }
         }

      private void PredictedCoeffs(string CropName, ref double[] a, ref double[] b, ref double[] CoeffDepthCentre)
         {
         // Get all coefficients and convert to double arrays.
         StringCollection AStrings = new StringCollection();
         StringCollection BStrings = new StringCollection();
         StringCollection LayerCentreStrings = new StringCollection();
         string SoilNameNoSpaces = Classification.Replace(" ", "");

         XmlNode PredLLCoeff = Configuration.Instance.GetSettingsNode("PredictedLLCoeff");

         XmlNode CropCoeffNode = XmlHelper.Find(PredLLCoeff, SoilNameNoSpaces + XmlHelper.Delimiter + CropName);
         foreach (XmlNode Layer in XmlHelper.ChildNodes(CropCoeffNode, "layer"))
            {
            AStrings.Add(XmlHelper.Value(Layer, "a"));
            BStrings.Add(XmlHelper.Value(Layer, "b"));
            LayerCentreStrings.Add(XmlHelper.Value(Layer, "LayerCentre"));
            }

         if (AStrings.Count == 0 || AStrings.Count != BStrings.Count || AStrings.Count != LayerCentreStrings.Count)
            throw new Exception("Invalid predicted LL coeffs found for soil: " + Classification + " and crop: " + CropName);
         a = new double[AStrings.Count];
         b = new double[BStrings.Count];
         CoeffDepthCentre = new double[LayerCentreStrings.Count];
         for (int i = 0; i != AStrings.Count; i++)
            {
            a[i] = Convert.ToDouble(AStrings[i]);
            b[i] = Convert.ToDouble(BStrings[i]);
            CoeffDepthCentre[i] = Convert.ToDouble(LayerCentreStrings[i]);
            }
         }

      private double[] PredictedLL(string CropName)
         {
         if (CSGeneral.StringManip.IndexOfCaseInsensitive(PredictedCrops, CropName) != -1)
            {
            double[] a = null;
            double[] b = null;
            double[] CoeffDepthCentre = null;
            PredictedCoeffs(CropName, ref a, ref b, ref CoeffDepthCentre);

            // Get some soil numbers we're going to need.
            double[] SoilDepthCentre = SoilComponentUtility.ToMidPoints(Thickness);
            double[] SoilDUL = MathUtility.Multiply_Value(this.DUL, 100);

            // Find the lowest measured crop LL in 3rd layer.
            double LowestLL3rdLayer = FindLowestCropLL3rdLayer();

            // only continue if our soil depth depth centers are within range of
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
                  PredLL[i] = Math.Max(PredLL[i], this.LL15[i]);
                  PredLL[i] = Math.Min(PredLL[i], this.DUL[i]);
                  }

               // make the top 3 layers the same as the the top 3 layers of LL15
               if (PredLL.Length >= 3)
                  {
                  PredLL[0] = LL15[0];
                  PredLL[1] = LL15[1];
                  PredLL[2] = LL15[2];
                  }

               return PredLL;
               }
            }
         return new double[0];
         }

      private double FindLowestCropLL3rdLayer()
         {
         double LowestLL3rdLayer = MathUtility.MissingValue;
         foreach (string CropName in CropsMeasured)
            {
            double[] ll = LL(CropName);
            if (ll.Length >= 3)
               LowestLL3rdLayer = Math.Min(LowestLL3rdLayer, ll[2]);
            }
         return LowestLL3rdLayer;
         }

      private double[] PredictedKL(string CropName)
         {
         XmlNode PredKLCoeff = Configuration.Instance.GetSettingsNode("PredictedKLCoeff");
         XmlNode CropKlValues = XmlHelper.Find(PredKLCoeff, CropName);
         if (CropKlValues != null)
            {
            StringCollection LayerCentreStrings = new StringCollection();
            StringCollection KLStrings = new StringCollection();
            foreach (XmlNode Layer in XmlHelper.ChildNodes(CropKlValues, "layer"))
               {
               KLStrings.Add(XmlHelper.Value(Layer, "kl"));
               LayerCentreStrings.Add(XmlHelper.Value(Layer, "LayerCentre"));
               }
            double[] kl = new double[KLStrings.Count];
            double[] CoeffDepthCentre = new double[LayerCentreStrings.Count];
            for (int i = 0; i != KLStrings.Count; i++)
               {
               kl[i] = Convert.ToDouble(KLStrings[i]);
               CoeffDepthCentre[i] = Convert.ToDouble(LayerCentreStrings[i]);
               }

            double[] SoilDepthCentre = SoilComponentUtility.ToMidPoints(Thickness);
            double[] Values = new double[SoilDepthCentre.Length];
            bool DidInterpolate = true;
            for (int i = 0; i != SoilDepthCentre.Length; i++)
               Values[i] = MathUtility.LinearInterpReal(SoilDepthCentre[i], CoeffDepthCentre, kl, ref DidInterpolate);
            return Values;
            }
         return new double[0];
         }

      private double[] PredictedXF(string CropName)
         {
         if (CropsMeasured.Length > 0)
            return XF(CropsMeasured[0]);
         else
            {
            int NumLayers = Thickness.Length;
            double[] Values = new double[NumLayers];
            for (int i = 0; i != NumLayers; i++)
               Values[i] = 1.0;
            return Values;
            }
         }
      #endregion

      #region APSIM
      public double U
         {
         get { return SoilComponentUtility.GetDoubleValue(Data, "", "U"); }
         }
      public double Cona
         {
         get { return SoilComponentUtility.GetDoubleValue(Data, "", "Cona"); }
         }
      public void SetUCona(double U, double Cona)
         {
         SoilComponentUtility.SetValue(Data, "", "Cona", Cona);
         SoilComponentUtility.SetValue(Data, "", "U", U);
         SoilComponentUtility.SetValue(Data, "", "SummerCona", "");
         SoilComponentUtility.SetValue(Data, "", "WinterCona", "");
         SoilComponentUtility.SetValue(Data, "", "SummerU", "");
         SoilComponentUtility.SetValue(Data, "", "WinterU", "");
         SoilComponentUtility.SetValue(Data, "", "SummerDate", "");
         SoilComponentUtility.SetValue(Data, "", "WinterDate", "");
         }

      public void SetSummerWinterUCona(double SummerU, double WinterU,
                                       double SummerCona, double WinterCona,
                                       string SummerDate, string WinterDate)
         {
         SoilComponentUtility.SetValue(Data, "", "Cona", "");
         SoilComponentUtility.SetValue(Data, "", "U", "");
         SoilComponentUtility.SetValue(Data, "", "SummerCona", SummerCona);
         SoilComponentUtility.SetValue(Data, "", "WinterCona", WinterCona);
         SoilComponentUtility.SetValue(Data, "", "SummerU", SummerU);
         SoilComponentUtility.SetValue(Data, "", "WinterU", WinterU);
         SoilComponentUtility.SetValue(Data, "", "SummerDate", SummerDate);
         SoilComponentUtility.SetValue(Data, "", "WinterDate", WinterDate);
         }
      public double WinterU
         {
         get { return SoilComponentUtility.GetDoubleValue(Data, "", "WinterU"); }
         }
      public double SummerU
         {
         get { return SoilComponentUtility.GetDoubleValue(Data, "", "SummerU"); }
         }
      public double WinterCona
         {
         get { return SoilComponentUtility.GetDoubleValue(Data, "", "WinterCona"); }
         }
      public double SummerCona
         {
         get { return SoilComponentUtility.GetDoubleValue(Data, "", "SummerCona"); }
         }
      public string WinterDate
         {
         get { return SoilComponentUtility.GetStringValue(Data, "", "WinterDate"); }
         }
      public string SummerDate
         {
         get { return SoilComponentUtility.GetStringValue(Data, "", "SummerDate"); }
         }
      public double Salb
         {
         get { return SoilComponentUtility.GetDoubleValue(Data, "", "Salb"); }
         set { SoilComponentUtility.SetValue(Data, "", "Salb", value); }
         }
      public double DiffusConst
         {
         get { return SoilComponentUtility.GetDoubleValue(Data, "", "DiffusConst"); }
         set { SoilComponentUtility.SetValue(Data, "", "DiffusConst", value); }
         }
      public double DiffusSlope
         {
         get { return SoilComponentUtility.GetDoubleValue(Data, "", "DiffusSlope"); }
         set { SoilComponentUtility.SetValue(Data, "", "DiffusSlope", value); }
         }
      public double CN2Bare
         {
         get { return SoilComponentUtility.GetDoubleValue(Data, "", "CN2Bare"); }
         set { SoilComponentUtility.SetValue(Data, "", "CN2Bare", value); }
         }
      public double CNRed
         {
         get { return SoilComponentUtility.GetDoubleValue(Data, "", "CNRed"); }
         set { SoilComponentUtility.SetValue(Data, "", "CNRed", value); }
         }
      public double CNCov
         {
         get { return SoilComponentUtility.GetDoubleValue(Data, "", "CNCov"); }
         set { SoilComponentUtility.SetValue(Data, "", "CNCov", value); }
         }
      public double RootCN
         {
         get { return SoilComponentUtility.GetDoubleValue(Data, "", "RootCN"); }
         set { SoilComponentUtility.SetValue(Data, "", "RootCN", value); }
         }
      public double RootWT
         {
         get { return SoilComponentUtility.GetDoubleValue(Data, "", "RootWT"); }
         set { SoilComponentUtility.SetValue(Data, "", "RootWT", value); }
         }
      public double SoilCN
         {
         get { return SoilComponentUtility.GetDoubleValue(Data, "", "SoilCN"); }
         set { SoilComponentUtility.SetValue(Data, "", "SoilCN", value); }
         }
      public double EnrACoeff
         {
         get { return SoilComponentUtility.GetDoubleValue(Data, "", "EnrACoeff"); }
         set { SoilComponentUtility.SetValue(Data, "", "EnrACoeff", value); }
         }
      public double EnrBCoeff
         {
         get { return SoilComponentUtility.GetDoubleValue(Data, "", "EnrBCoeff"); }
         set { SoilComponentUtility.SetValue(Data, "", "EnrBCoeff", value); }
         }
      public double RateLossAvail
         {
         get { return SoilComponentUtility.GetDoubleValue(Data, "", "RateLossAvail"); }
         set { SoilComponentUtility.SetValue(Data, "", "RateLossAvail", value); }
         }
      public double RootCP
         {
         get { return SoilComponentUtility.GetDoubleValue(Data, "", "RootCP"); }
         set { SoilComponentUtility.SetValue(Data, "", "RootCP", value); }
         }
      public double RateDissolRock
         {
         get { return SoilComponentUtility.GetDoubleValue(Data, "", "RateDissolRock"); }
         set { SoilComponentUtility.SetValue(Data, "", "RateDissolRock", value); }
         }
      #endregion

      #region PAWC calculations
      public double[] PAWC(string CropName)
         {
         return PAWC(this.LL(CropName), this.XF(CropName));
         }
      public double[] PAWC()
         {
         double[] xf = new double[LL15.Length];
         for (int i = 0; i != xf.Length; i++)
            xf[i] = 1;

         return PAWC(this.LL15, xf);
         }
      private double[] PAWC(double[] LL, double[] XF)
         {
         double[] Thickness = this.Thickness;
         double[] DUL = this.DUL;
         double[] PAWC = new double[Thickness.Length];

         if (Thickness.Length != DUL.Length || Thickness.Length != LL.Length)
            return new double[0];

         for (int layer = 0; layer != Thickness.Length; layer++)
            if (DUL[layer] == MathUtility.MissingValue ||
               LL[layer] == MathUtility.MissingValue ||
                    XF[layer] == 0 ||
                    (layer > 0 && PAWC[layer - 1] == 0))
               PAWC[layer] = 0;
            else
               PAWC[layer] = (DUL[layer] - LL[layer]) * Thickness[layer];
         return PAWC;
         }
      #endregion

      #region Export
      public void ExportToPar(string FileName, string SectionName, bool AppendToFile)
         {
         string Template =
            "[$SECTIONNAME$.soilwat2.parameters]\r\n" +
            "   diffus_const = [soil.DiffusConst]    ! coeffs for unsaturated water flow\r\n" +
                "   diffus_slope = [soil.DiffusSlope]\r\n" +
                "   cn2_bare     = [soil.Cn2Bare]    ! bare soil runoff curve number\r\n" +
                "   cn_red       = [soil.CnRed]    ! potetial reduction in curve number due to residue\r\n" +
                "   cn_cov       = [soil.CnCov]   ! cover for maximum reduction in curve number\r\n" +
                "   salb         = [soil.Salb]  ! bare soil albedo\r\n";
         if (SummerCona != MathUtility.MissingValue)
            {
            Template += "   SummerCona   = [soil.SummerCona]   ! stage 2 evap coef. for summer\r\n" +
                        "   WinterCona   = [soil.WinterCona]   ! stage 2 evap coef. for winter\r\n" +
                        "   SummerU      = [soil.SummerU]      ! stage 1 soil evaporation coefficient for summer (mm)\r\n" +
                        "   WinterU      = [soil.WinterU]      ! stage 1 soil evaporation coefficient for winter (mm)\r\n" +
                        "   SummerDate   = [soil.SummerDate]      ! Start date of summer\r\n" +
                        "   WinterDate   = [soil.WinterDate]      ! Start date of winter\r\n";
            }
         else
            Template += "   cona         = [soil.Cona]   ! stage 2 evap coef.\r\n" +
                        "   u            = [soil.U]     ! stage 1 soil evaporation coefficient (mm)\r\n";
         Template +=
             "\r\n" +
             "[foreach Soil.profile]\r\n" +
             "   dlayer  =[foreach profile.layer as Layer]  [Layer.thickness.3][endfor]   ! layer thickness mm soil\r\n" +
             "   air_dry =[foreach profile.layer as Layer]    [Layer.airdry.3][endfor]   ! air dry mm water/mm soil\r\n" +
             "   ll15    =[foreach profile.layer as Layer]    [Layer.ll15.3][endfor]   ! lower limit mm water/mm soil\r\n" +
             "   dul     =[foreach profile.layer as Layer]    [Layer.dul.3][endfor]   ! drained upper limit mm water/mm soil\r\n" +
             "   sat     =[foreach profile.layer as Layer]    [Layer.sat.3][endfor]   ! saturation mm water/mm soil\r\n" +
             "   swcon   =[foreach profile.layer as Layer]    [Layer.swcon.3][endfor]   ! drainage coefficient\r\n" +
             "   bd      =[foreach profile.layer as Layer]    [Layer.bd.3][endfor]   ! bulk density gm dry soil/cc moist soil\r\n" +
             "$SW$\r\n";

         if (MWCON.Length > 0 && MWCON[0] != MathUtility.MissingValue)
            Template +=
            "   mwcon   =[foreach profile.layer as Layer]    [Layer.mwcon.3][endfor]   \r\n\r\n";
         if (KS.Length > 0 && KS[0] != MathUtility.MissingValue)
            Template +=
            "   ks   =[foreach profile.layer as Layer]    [Layer.ks.3][endfor]   \r\n\r\n";

         Template +=
             "[endfor]\r\n" +//END OF WATER FOR LOOP
             "\r\n" +
             "[$SECTIONNAME$.soiln2.parameters]\r\n" +//TITLE
             "   root_cn      = [soil.rootcn]     ! C:N ratio of initial root residues\r\n" +
             "   root_wt      = [soil.rootwt]   ! root residues as biomass (kg/ha)\r\n" +
             "   soil_cn      = [soil.soilcn]   ! C:N ratio of soil\r\n" +
             "   enr_a_coeff  = [soil.enracoeff]\r\n" +
             "   enr_b_coeff  = [soil.enrbcoeff]\r\n" +
             "   profile_reduction =  off\r\n" +
             "\r\n" +
             "[foreach Soil.profile]\r\n" +
             "$NITROGEN$\r\n" +
             "   oc      =[foreach profile.layer as Layer]\r\n      [Layer.oc.3][endfor]   ! Soil Organic Carbon\r\n" +
             "   ph      =$PH$   ! pH of soil\r\n" +
             "   fbiom   =[foreach profile.layer as Layer]\r\n      [Layer.fbiom.3][endfor]   ! Organic C Biomass Fraction\r\n" +
             "   finert  =[foreach profile.layer as Layer]\r\n      [Layer.finert.3][endfor]   ! Inert Organic C Fraction\r\n" +
             "[endfor]\r\n" +//END OF NITROGEN FOR LOOP
             "\r\n" +
             "[if [soil.rootcp] > 0]\r\n" +
             "[$SECTIONNAME$.soilp.parameters]\r\n" +
             "   root_cp            =  [soil.rootcp]      () !c:p ratio of roots at initialisation\r\n" +
             "   rate_dissol_rock_P =  [soil.RateDissolRock] (/yr)   !rate at which rock P source becomes available\r\n" +
             "   rate_loss_avail_P  =  [soil.RateLossAvailP] (/yr)   ! (< 1) Fraction lost per yr specified at 25 oC" +
             "\r\n" +
             "[foreach Soil.profile]\r\n" +
             "   labile_P  = [foreach profile.layer]    [layer.labilep.3][endfor]   (mg/kg)\r\n" +
             "   banded_P  = [foreach profile.layer]    [layer.bandedP.3][endfor]   (kg/ha) ! banded p content for each layer\r\n" +
             "   rock_P    = [foreach profile.layer]    [layer.rockP.3][endfor]   (kg/ha)   !rock p content for each layer ie no water soluble\r\n" +
             "   sorption  =[foreach profile.layer]  [layer.sorption.3][endfor]   ()   !P sorbed at 0.2ppm\r\n" +
             "[endfor]\r\n" +
             "[endif]\r\n" +
             "$CROP$\r\n" +
             "[endfile]\r\n\r\n";

         string CropStuff = "";
         foreach (string CropName in Crops)
            {
            CropStuff += "[$SECTIONNAME$." + CropName + ".parameters]\r\n";
            if (CropIsPredicted(CropName))
               Template += "   !These crop numbers are predicted\r\n";

            string LLLine = "";
            string KLLine = "";
            string XFLine = "";
            double[] ll = LL(CropName);
            double[] kl = KL(CropName);
            double[] xf = XF(CropName);

            for (int i = 0; i != ll.Length; i++)
               {
               LLLine += "      " + ll[i].ToString("f3");
               KLLine += "      " + kl[i].ToString("f3");
               XFLine += "      " + xf[i].ToString("f3");
               }
            CropStuff += "   ll      =" + LLLine + "\r\n";
            if (CropName.ToLower() == "ozcot")
               CropStuff += "   Title = XXX\r\n" +
                           "   asoil = 3.0\r\n";

            CropStuff += "   kl      =" + KLLine + "\r\n";
            CropStuff += "   xf      =" + XFLine + "\r\n";
            }
         Template = Template.Replace("$CROP$", CropStuff);

         string PHLine = "";
         double[] ph = PH;

         int NumLayers = Thickness.Length;
         for (int i = 0; i != NumLayers; i++)
            {
            if (i < ph.Length)
               PHLine += "      " + ph[i].ToString("f3");
            }

         Template = Template.Replace("$PH$", PHLine);
         Template = Template.Replace("$SECTIONNAME$", SectionName);

         string SWLine = "";
         XmlNode InitW = XmlHelper.Find(Data, "InitWater");
         if (InitW != null)
            {
            SWLine = "   sw      =";
            InitWater InitWater = new InitWater(InitW, this);
            double[] sw = InitWater.SW;
            for (int i = 0; i != sw.Length; i++)
               SWLine += "    " + sw[i].ToString("f3");
            }
         Template = Template.Replace("$SW$", SWLine);

         string NitrogenLine = "";
         XmlNode InitN = XmlHelper.Find(Data, "InitNitrogen");
         if (InitN != null)
            {
            InitNitrogen InitNitrogen = new InitNitrogen(InitN, this);
            double[] no3 = InitNitrogen.NO3;
            double[] nh4 = InitNitrogen.NH4;

            NitrogenLine = "   no3     =";
            for (int i = 0; i != no3.Length; i++)
               NitrogenLine += "      " + no3[i].ToString("f3");
            NitrogenLine += "\r\n";
            NitrogenLine += "   nh4     =";
            for (int i = 0; i != nh4.Length; i++)
               NitrogenLine += "      " + nh4[i].ToString("f3");
            NitrogenLine += "\r\n";
            }
         Template = Template.Replace("$NITROGEN$", NitrogenLine);


         string szSoilFileTemplate = "[file " + Path.GetFileName(FileName) + "]\r\n" + Template;
         Macro SoilMacro = new Macro();
         StringCollection scSoilFiles = SoilMacro.Go(Data, szSoilFileTemplate,
                                          Path.GetDirectoryName(FileName),
                                          AppendToFile);
         }
      public XmlNode ExportToSim(XmlNode ParentNode)
         {
         string errors = CheckThatSimulationWillRun();
         if (errors != "")
            throw new Exception(errors);

         // Water variables
         XmlNode SoilNode = ParentNode.AppendChild(ParentNode.OwnerDocument.CreateElement("component"));
         XmlHelper.SetName(SoilNode, Name + " Water");
         XmlHelper.SetAttribute(SoilNode, "executable", "%apsim%\\Model\\SoilWat.dll");
         XmlNode InitData = SoilNode.AppendChild(SoilNode.OwnerDocument.CreateElement("initdata"));
         XmlHelper.SetValue(InitData, "include", "%apsim%\\Model\\SoilWat.xml");
         XmlHelper.SetValue(InitData, "diffus_const", DiffusConst.ToString());
         XmlHelper.SetValue(InitData, "diffus_slope", DiffusSlope.ToString());
         XmlHelper.SetValue(InitData, "cn2_bare", CN2Bare.ToString());
         XmlHelper.SetValue(InitData, "cn_red", CNRed.ToString());
         XmlHelper.SetValue(InitData, "cn_cov", CNCov.ToString());
         XmlHelper.SetValue(InitData, "salb", Salb.ToString());
         if (SummerCona != MathUtility.MissingValue)
            {
            XmlHelper.SetValue(InitData, "SummerCona", SummerCona.ToString());
            XmlHelper.SetValue(InitData, "WinterCona", WinterCona.ToString());
            XmlHelper.SetValue(InitData, "SummerU", SummerU.ToString());
            XmlHelper.SetValue(InitData, "WinterU", WinterU.ToString());
            XmlHelper.SetValue(InitData, "SummerDate", SummerDate.ToString());
            XmlHelper.SetValue(InitData, "WinterDate", WinterDate.ToString());
            }
         else
            {
            XmlHelper.SetValue(InitData, "cona", Cona.ToString());
            XmlHelper.SetValue(InitData, "u", U.ToString());
            }
         XmlHelper.SetValue(InitData, "dlayer", SoilComponentUtility.LayeredToString(Thickness));
         XmlHelper.SetValue(InitData, "sat", SoilComponentUtility.LayeredToString(SAT));
         XmlHelper.SetValue(InitData, "dul", SoilComponentUtility.LayeredToString(DUL));
         XmlHelper.SetValue(InitData, "ll15", SoilComponentUtility.LayeredToString(LL15));
         XmlHelper.SetValue(InitData, "air_dry", SoilComponentUtility.LayeredToString(Airdry));
         XmlHelper.SetValue(InitData, "swcon", SoilComponentUtility.LayeredToString(SWCON));
         XmlHelper.SetValue(InitData, "bd", SoilComponentUtility.LayeredToString(BD));
         if (MWCON.Length > 0 && MWCON[0] != MathUtility.MissingValue)
            XmlHelper.SetValue(InitData, "mwcon", SoilComponentUtility.LayeredToString(MWCON));
         if (KS.Length > 0 && KS[0] != MathUtility.MissingValue)
            XmlHelper.SetValue(InitData, "ks", SoilComponentUtility.LayeredToString(KS));



         // Nitrogen variables
         XmlNode Nitrogen = ParentNode.AppendChild(ParentNode.OwnerDocument.CreateElement("component"));
         XmlHelper.SetName(Nitrogen, Name + " Nitrogen");
         XmlHelper.SetAttribute(Nitrogen, "executable", "%apsim%\\Model\\SoilN.dll");
         XmlNode NitrogenInitData = Nitrogen.AppendChild(SoilNode.OwnerDocument.CreateElement("initdata"));
         XmlHelper.SetValue(NitrogenInitData, "include", "%apsim%\\Model\\SoilN.xml");
         if (Classification != "")
            XmlHelper.SetValue(NitrogenInitData, "soiltype", Classification);
         XmlHelper.SetValue(NitrogenInitData, "root_cn", RootCN.ToString());
         XmlHelper.SetValue(NitrogenInitData, "root_wt", RootWT.ToString());
         XmlHelper.SetValue(NitrogenInitData, "soil_cn", SoilCN.ToString());
         XmlHelper.SetValue(NitrogenInitData, "enr_a_coeff", EnrACoeff.ToString());
         XmlHelper.SetValue(NitrogenInitData, "enr_b_coeff", EnrBCoeff.ToString());
         XmlHelper.SetValue(NitrogenInitData, "profile_reduction", "off");
         XmlHelper.SetValue(NitrogenInitData, "oc", SoilComponentUtility.LayeredToString(OC));
         XmlHelper.SetValue(NitrogenInitData, "ph", SoilComponentUtility.LayeredToString(PH));
         XmlHelper.SetValue(NitrogenInitData, "fbiom", SoilComponentUtility.LayeredToString(FBIOM));
         XmlHelper.SetValue(NitrogenInitData, "finert", SoilComponentUtility.LayeredToString(FINERT));
         if (Rocks.Length > 0 && Rocks[0] != MathUtility.MissingValue)
            XmlHelper.SetValue(NitrogenInitData, "rocks", SoilComponentUtility.LayeredToString(Rocks));

         // Write in some default NH4 values.
         double[] DefaultNH4 = new double[Thickness.Length];
         for (int i = 0; i != Thickness.Length; i++)
            DefaultNH4[i] = 0.2;
         XmlHelper.SetValue(NitrogenInitData, "nh4ppm", SoilComponentUtility.LayeredToString(DefaultNH4));

         // Phosphorus variables
         if (RootCP != MathUtility.MissingValue)
            {
            XmlNode Phosphorus = ParentNode.AppendChild(ParentNode.OwnerDocument.CreateElement("component"));
            XmlHelper.SetName(Phosphorus, Name + " Phosphorus");
            XmlHelper.SetAttribute(Phosphorus, "executable", "%apsim%\\Model\\SoilP.dll");
            XmlNode PhosphorusInitData = Phosphorus.AppendChild(SoilNode.OwnerDocument.CreateElement("initdata"));
            XmlHelper.SetValue(PhosphorusInitData, "include", "%apsim%\\Model\\SoilP.xml");
            XmlHelper.SetValue(PhosphorusInitData, "Root_CP", RootCP.ToString());
            XmlHelper.SetValue(PhosphorusInitData, "rate_dissol_rock_P", RateDissolRock.ToString());
            XmlHelper.SetValue(PhosphorusInitData, "rate_loss_avail_P", RateLossAvail.ToString());

            XmlHelper.SetValue(PhosphorusInitData, "Labile_P", SoilComponentUtility.LayeredToString(LabileP));
            XmlHelper.SetValue(PhosphorusInitData, "banded_P", SoilComponentUtility.LayeredToString(BandedP));
            XmlHelper.SetValue(PhosphorusInitData, "rock_P", SoilComponentUtility.LayeredToString(RockP));
            XmlHelper.SetValue(PhosphorusInitData, "sorption", SoilComponentUtility.LayeredToString(Sorption));
            }
         return SoilNode;
         }
      public XmlNode ExportCropToSim(XmlNode ParentNode, string CropName)
         {
         // Go look for our component node which has already been created for us.
         foreach (XmlNode Node in ParentNode.ParentNode.ParentNode.ChildNodes)
            {
            if (XmlHelper.Name(Node).ToLower() == CropName.ToLower())
               {
               if (CropExists(CropName) || CropIsPredicted(CropName))
                  {
                  XmlHelper.SetValue(Node, "initdata/ll", SoilComponentUtility.LayeredToString(LL(CropName)));
                  XmlHelper.SetValue(Node, "initdata/kl", SoilComponentUtility.LayeredToString(KL(CropName)));
                  XmlHelper.SetValue(Node, "initdata/xf", SoilComponentUtility.LayeredToString(XF(CropName)));
                  }
               else
                  throw new Exception("No soil/crop parameterisation for crop: " + CropName);
               return Node;
               }
            }
         throw new Exception("Cannot find crop node : " + CropName);
         }

      #endregion


      #region Error checking
      public string CheckForErrors()
         {
         string ErrorMessages = "";
         ErrorMessages += CheckForMissing(this.Thickness, "THICKNESS");
         ErrorMessages += CheckForMissing(this.Airdry, "AIRDRY");
         ErrorMessages += CheckForMissing(this.DUL, "DUL");
         ErrorMessages += CheckForMissing(this.SAT, "SAT");
         ErrorMessages += CheckForMissing(this.SWCON, "SWCON");
         ErrorMessages += CheckForMissing(this.BD, "BD");
         ErrorMessages += CheckForMissing(this.OC, "OC");
         ErrorMessages += CheckForMissing(this.PH, "PH");
         ErrorMessages += CheckForMissing(this.FBIOM, "FBIOM");
         ErrorMessages += CheckForMissing(this.FINERT, "FINERT");
         ErrorMessages += CheckForMissing(this.Salb, "SALB");
         ErrorMessages += CheckForMissing(this.CN2Bare, "CN2BARE");
         ErrorMessages += CheckForMissing(this.CNRed, "CNRED");
         ErrorMessages += CheckForMissing(this.CNCov, "CNCOV");
         ErrorMessages += CheckForMissing(this.DiffusConst, "DIFFUSCONST");
         ErrorMessages += CheckForMissing(this.DiffusSlope, "DIFFUSSLOPE");
         ErrorMessages += CheckForMissing(this.RootCN, "ROOTCN");
         ErrorMessages += CheckForMissing(this.RootWT, "ROOTWT");
         ErrorMessages += CheckForMissing(this.SoilCN, "SOILCN");
         ErrorMessages += CheckForMissing(this.EnrACoeff, "ENRACOEFF");
         ErrorMessages += CheckForMissing(this.EnrBCoeff, "ENRBCOEFF");

         foreach (string Crop in this.Crops)
            {
            ErrorMessages += CheckForMissing(this.LL(Crop), "LL-" + Crop);
            if (Crop.ToLower() != "ozcot")
               {
               ErrorMessages += CheckForMissing(this.KL(Crop), "KL-" + Crop);
               ErrorMessages += CheckForMissing(this.XF(Crop), "XF-" + Crop);
               }
            }

         // Do some more rigorous checks.
         if (ErrorMessages == "")
            ErrorMessages += CheckProfile();

         return ErrorMessages;
         }
      public string CheckThatSimulationWillRun()
         {
         //-------------------------------------------------------------------------
         // This checks the soil for errors and returns an error message if a
         // problem was found. A blank string returned indicates no problems.
         //-------------------------------------------------------------------------
         string ErrorMessages = CheckForErrors();

         //ErrorMessages += CheckForMissing(this.InitialSW, "SW");
         //ErrorMessages += CheckForMissing(this.InitialNO3, "NO3");
         //ErrorMessages += CheckForMissing(this.InitialNH4, "NH4");
         ErrorMessages += CheckSW();
         return ErrorMessages;
         }
      private string CheckForMissing(double[] Values, string PropertyName)
         {
         // -----------------------------------------------------
         // Checks the specified array for missing values.
         // Returns an error message if a problem was found.
         // -----------------------------------------------------
         bool AllMissing = true;
         for (int i = 0; i != Values.Length; i++)
            AllMissing = (AllMissing && Values[i] == MathUtility.MissingValue);
         if (AllMissing)
            return "There are no values for " + PropertyName + "\r\n";

         for (int i = 0; i != Values.Length; i++)
            {
            int RealLayerNumber = i + 1;
            if (Values[i] == MathUtility.MissingValue)
               return "A missing value was found in layer " + RealLayerNumber.ToString() + " for " + PropertyName + "\r\n";
            }
         return "";
         }
      private string CheckForMissing(double Value, string PropertyName)
         {
         // -----------------------------------------------------
         // Checks the specified value for missing number.
         // Returns an error message if a problem was found.
         // -----------------------------------------------------
         if (Value == MathUtility.MissingValue)
            return "Missing a value for " + PropertyName + "\r\n";
         return "";
         }
      private string CheckProfile()
         {
         // ------------------------------------------------------------------
         // Checks validity of soil water parameters for a soil profile layer
         // This is a port of the soilwat2_check_profile routine.
         // ------------------------------------------------------------------
         string errorMessages = "";
         const double min_sw = 0.0;
         const double specific_bd = 2.65; // (g/cc)

         double[] thickness = this.Thickness;
         double[] airdry = this.Airdry;
         double[] ll15 = this.LL15;
         double[] dul = this.DUL;
         double[] sat = this.SAT;
         double[] bd = this.BD;
         //double[] sw = this.InitialSW;

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

            foreach (string Crop in Crops)
               {
               double[] ll = LL(Crop);
               double[] kl = KL(Crop);
               double[] xf = XF(Crop);

               if (kl[layer] > 1)
                  errorMessages += "KL value of " + kl[layer].ToString("f3")
                           + " in layer " + RealLayerNumber.ToString() + " is greater than 1"
                           + "\r\n";

               if (xf[layer] > 1)
                  errorMessages += "XF value of " + xf[layer].ToString("f3")
                           + " in layer " + RealLayerNumber.ToString() + " is greater than 1"
                           + "\r\n";

               if (ll[layer] < airdry[layer])
                  errorMessages += "Crop lower limit of " + ll[layer].ToString("f3")
                             + " for crop " + Crop
                               + " in layer " + RealLayerNumber.ToString() + " is below air dry value of " + airdry[layer].ToString("f3")
                             + "\r\n";

               if (ll[layer] > DUL[layer])
                  errorMessages += "Crop lower limit of " + ll[layer].ToString("f3")
                             + " for crop " + Crop
                               + " in layer " + RealLayerNumber.ToString() + " is above drained upper limit of " + dul[layer].ToString("f3")
                             + "\r\n";

               }

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
            if (OC[layer] < 0.01)
               errorMessages += "OC value of " + OC[layer].ToString("f3")
                             + " in layer " + RealLayerNumber.ToString() + " is less than 0.01"
                             + "\r\n";
            if (PH[layer] < 3.5)
               errorMessages += "PH value of " + PH[layer].ToString("f3")
                             + " in layer " + RealLayerNumber.ToString() + " is less than 3.5"
                             + "\r\n";
            if (PH[layer] > 11)
               errorMessages += "PH value of " + PH[layer].ToString("f3")
                             + " in layer " + RealLayerNumber.ToString() + " is greater than 11"
                             + "\r\n";
            }
         return errorMessages;
         }
      private string CheckSW()
         {
         // ------------------------------------------------------------------
         // Checks validity of initial soil water 
         // ------------------------------------------------------------------
         string errorMessages = "";

         double[] thickness = this.Thickness;
         double[] airdry = this.Airdry;
         double[] sat = this.SAT;
         //double[] sw = this.InitialSW;
         //if (sw.Length > 0)
         //    {
         //    for (int layer = 0; layer != thickness.Length; layer++)
         //        {
         //        int RealLayerNumber = layer + 1;

         //        if (sw[layer] > sat[layer])
         //            errorMessages += "Soil water of " + sw[layer].ToString("f3")
         //                          + " in layer " + RealLayerNumber.ToString() + " is above saturation of " + sat[layer].ToString("f3")
         //                          + "\r\n";

         //        if (sw[layer] < airdry[layer])
         //            errorMessages += "Soil water of " + sw[layer].ToString("f3")
         //                          + " in layer " + RealLayerNumber.ToString() + " is below air-dry value of " + airdry[layer].ToString("f3")
         //                          + "\r\n";
         //        }
         //    }
         return errorMessages;
         }

      #endregion

      #region Manipulation / fudges
      public void ApplyMaxWaterCapacity(int maxWaterCapacity)
         {
         //---------------------------------------------------
         // Adjust the DUL curve, given a max water Capacity
         // start from bottom most layer that has water in it and reduced to zero or until
         // the whole profile equals the given max water capacity
         //---------------------------------------------------
         double[] localDUL = DUL;
         double[] localLL15 = LL15;
         double[] localThickness = Thickness;
         //find lowest layer that has water
         int bottomLayer;
         int layer;
         for (layer = 0; layer < localDUL.Length; layer++)
            {
            double test = ((localDUL[layer] * Thickness[layer]) - (localLL15[layer] * Thickness[layer]));
            if (((localDUL[layer] * Thickness[layer]) - (localLL15[layer] * Thickness[layer])) == 0) break;
            }
         bottomLayer = layer - 1;
         double currentCapacitySum = MathUtility.Sum(PAWC());
         double capacityDifference = maxWaterCapacity - currentCapacitySum;
         if (!(capacityDifference == 0))
            {
            if (capacityDifference > 0)
               {
               double newThickness = localThickness[bottomLayer] + (((Math.Abs(capacityDifference)) / (localDUL[bottomLayer] - localLL15[bottomLayer])));
               localThickness[bottomLayer] = newThickness;
               }
            else
               {
               for (int j = bottomLayer; j >= 0; j--)
                  {
                  double waterInLayer = ((localThickness[j]) * (localDUL[j] - localLL15[j]));
                  if ((Math.Abs(capacityDifference) > waterInLayer))
                     {
                     capacityDifference = (Math.Abs(capacityDifference) - (localThickness[j]) * (localDUL[j] - localLL15[j]));
                     localThickness[j] = 0;
                     }
                  else
                     {
                     double newThickness = ((Math.Abs(capacityDifference)) / (localDUL[j] - localLL15[j]));
                     localThickness[j] = localThickness[j] - newThickness;
                     break;
                     }
                  }
               }
            }
         Thickness = localThickness;
         }
      public void ApplyMaxSoilDepth(int soilDepth)
         {
         // -------------------------------------------------
         // Adjust the DUL curve, given a max soil depth(cutoff)
         // Leave the DUL number for all whole layers above the cutoff layer
         // Work out the proportional DUL number for the layer that has the cutoff within it
         // Set the LL15 to the DUL for the whole layers below the cutoff
         // -------------------------------------------------
         double[] localDUL = DUL;
         double[] localLL15 = LL15;
         double[] CumThickness = SoilComponentUtility.ToCumThickness(Thickness);
         for (int i = 0; i != CumThickness.Length; i++)
            {
            if (CumThickness[i] > soilDepth)
               {
               double PreviousCumThickness = 0.0;
               if (i > 0)
                  PreviousCumThickness = CumThickness[i - 1];

               if (PreviousCumThickness > soilDepth)
                  {
                  localLL15[i] = localDUL[i];
                  }
               else
                  {
                  double Proportion = (soilDepth - PreviousCumThickness) / Thickness[i];
                  localDUL[i] = (LL15[i] + ((localDUL[i] - LL15[i]) * Proportion));
                  }
               }
            }
         DUL = localDUL;
         LL15 = localLL15;
         }
      #endregion

      #region Note methods
      public void DeleteNote(string GridName, int Col, int Row)
         {
         string NoteNameToDelete = "";
         foreach (XmlNode Note in XmlHelper.ChildNodes(Data, "note"))
            {
            if (XmlHelper.Value(Note, "grid") == GridName &&
               Convert.ToInt32(XmlHelper.Value(Note, "col")) == Col &&
               Convert.ToInt32(XmlHelper.Value(Note, "row")) == Row)
               NoteNameToDelete = Note.Name;
            }
         if (NoteNameToDelete != "")
            Data.RemoveChild(XmlHelper.Find(Data, NoteNameToDelete));
         }
      public struct Note
         {
         public string GridName;
         public int Col, Row;
         public string Text;
         };
      public void AddNote(string GridName, int Col, int Row, string Text)
         {
         XmlNode Note = XmlHelper.CreateNode(Data.OwnerDocument, "note", "");
         XmlHelper.SetValue(Note, "grid", GridName);
         XmlHelper.SetValue(Note, "col", Col.ToString());
         XmlHelper.SetValue(Note, "row", Row.ToString());
         XmlHelper.SetValue(Note, "text", Text);
         Data.AppendChild(Note);
         }
      public Note[] GetNotes()
         {
         Note[] ReturnList = new Note[XmlHelper.ChildNodes(Data, "note").Count];
         int i = 0;
         foreach (XmlNode NoteNode in XmlHelper.ChildNodes(Data, "note"))
            {
            ReturnList[i].GridName = XmlHelper.Value(NoteNode, "grid");
            ReturnList[i].Col = Convert.ToInt32(XmlHelper.Value(NoteNode, "col"));
            ReturnList[i].Row = Convert.ToInt32(XmlHelper.Value(NoteNode, "row"));
            ReturnList[i].Text = XmlHelper.Value(NoteNode, "text");

            i++;
            }
         return ReturnList;
         }
      #endregion

      #region Add/Delete layer
      public void AddLayerToBottom()
         {
         // ----------------------------------
         // Add another layer to the profile.
         // ----------------------------------
         XmlNode Profile = XmlHelper.Find(Data, "profile");
         if (Profile != null)
            {
            List<XmlNode> Layers = XmlHelper.ChildNodes(Profile, "layer");
            Profile.AppendChild(Layers[Thickness.Length - 1]);
            }
         }
      public void DeleteLayerFromBottom()
         {
         // ----------------------------------
         // Add another layer to the profile.
         // ----------------------------------
         XmlNode Profile = XmlHelper.Find(Data, "profile");
         if (Profile != null)
            {
            List<XmlNode> Layers = XmlHelper.ChildNodes(Profile, "layer");

            Profile.RemoveChild(Layers[Thickness.Length - 1]);
            }
         }
      #endregion
      }
   }
