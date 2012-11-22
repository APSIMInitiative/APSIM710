using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Xml.Serialization;
using CSGeneral;
using System.Data;
using System.IO;
using Model;

namespace ApsimFile
{
    /// <summary>
    /// The soil class encapsulates a soil characterisation and 0 or more soil samples.
    /// the methods in this class that return double[] always return using the 
    /// "Standard layer structure" i.e. the layer structure as defined by the Water child object.
    /// method. Mapping will occur to achieve this if necessary.
    /// To obtain the "raw", unmapped, values use the child classes e.g. SoilWater, Analysis and Sample.
    /// </summary>
    public class Soil
    {
        [UIIgnore]
        [XmlAttribute("name")]
        public string Name { get; set; }

        [Description("Australian Soil Classification Order")]
        public string ASCOrder { get; set; }
        [Description("Australian Soil Classification Sub-Order")]
        public string ASCSubOrder { get; set; }
        [Description("Soil description")]
        public string SoilType { get; set; }
        [Description("Local name")]
        public string LocalName { get; set; }
        public string Site { get; set; }
        [Description("Nearest town")]
        public string NearestTown { get; set; }
        public string Region { get; set; }
        public string State { get; set; }
        public string Country { get; set; }
        [Description("Natural vegetation")]
        public string NaturalVegetation { get; set; }
        [Description("APSoil number")]
        public string ApsoilNumber { get; set; }
        [Description("Latitude (WGS84)")]
        public double Latitude { get; set; }
        [Description("Longitude (WGS84)")]
        public double Longitude { get; set; }
        [Description("Location accuracy")]
        public string LocationAccuracy { get; set; }

        [UILargeText]
        [Description("Data source")]
        public string DataSource { get; set; }
        [UILargeText]
        public string Comments { get; set; }

        public Water Water { get; set; }
        public SoilWater SoilWater { get; set; }
        public SoilOrganicMatter SoilOrganicMatter { get; set; }
        public Analysis Analysis { get; set; }
        public InitialWater InitialWater { get; set; }
        public Phosphorus Phosphorus { get; set; }
        public Swim Swim { get; set; }
        public LayerStructure LayerStructure { get; set; }

        [XmlElement("Sample")]
        public List<Sample> Samples { get; set; }

        
        /// <summary>
        /// Constructor
        /// </summary>
        public Soil()
        {
            Name = "Soil";                
            Water = new Water();
            SoilOrganicMatter = new SoilOrganicMatter();
            Analysis = new Analysis();
            Samples = new List<Sample>();
        }

        /// <summary>
        /// Create a soil object from the XML passed in.
        /// </summary>
        public static Soil Create(string Xml)
        {
            XmlSerializer x = new XmlSerializer(typeof(Soil));
            StringReader F = new StringReader(Xml);
            return x.Deserialize(F) as Soil;
        }

        /// <summary>
        /// Write soil to XML
        /// </summary>
        public string ToXml()
        {
            XmlSerializerNamespaces ns = new XmlSerializerNamespaces();
            ns.Add("", "");
            XmlSerializer x = new XmlSerializer(typeof(Soil));

            StringWriter Out = new StringWriter();
            x.Serialize(Out, this, ns);
            return Out.ToString();
        }

        #region Water

        /// <summary>
        /// Return the thicknesses to run APSIM.
        /// </summary>
        public double[] Thickness 
        {
            get
            {
                if (LayerStructure != null)
                    return LayerStructure.Thickness;
                else
                    return Water.Thickness;
            }
        }

        /// <summary>
        /// Bulk density at standard thickness. Units: mm/mm
        /// </summary>
        internal double[] BD { get { return Map(Water.BD, Water.Thickness, Thickness, MapType.Concentration, Water.BD.Last()); } }

        /// <summary>
        /// Soil water at standard thickness. Units: mm/mm
        /// </summary>
        public double[] SW
        {
            get
            {
                double[] Thicknesses = Water.Thickness;
                double[] Values = null;
                if (InitialWater != null)
                    Values = InitialWater.SW(Water.Thickness, Water.LL15, Water.DUL, null);
                else
                {
                    foreach (Sample Sample in Samples)
                    {
                        if (MathUtility.ValuesInArray(Sample.SW))
                        {
                            Values = Sample.SWVolumetric(this);
                            Thicknesses = Sample.Thickness;
                        }
                    }

                }

                if (Thicknesses == Thickness)
                    return Values;


                // Get the last item in the sw array and its indx.
                double LastSW = LastValue(Values);
                double LastThickness = LastValue(Thicknesses);
                int LastIndex = Values.Length - 1;

                Array.Resize(ref Thicknesses, Thicknesses.Length + 3); // Increase array size by 3.
                Array.Resize(ref Values, Values.Length + 3);

                Thicknesses[LastIndex + 1] = LastThickness;
                Thicknesses[LastIndex + 2] = LastThickness;
                Thicknesses[LastIndex + 3] = 3000;

                Values[LastIndex + 1] = LastSW * 0.8;
                Values[LastIndex + 2] = LastSW * 0.4;
                Values[LastIndex + 3] = 0.0; // This will be constrained below to crop LL below.

                // Get the first crop ll or ll15.
                double[] LowerBound;
                if (Water.Crops.Count > 0)
                    LowerBound = LLMapped(Water.Crops[0].Name, Thicknesses);
                else
                    LowerBound = LL15Mapped(Thicknesses);
                if (LowerBound == null)
                    throw new Exception("Cannot find crop lower limit or LL15 in soil");

                // Make sure all SW values below LastIndex don't go below CLL.
                for (int i = LastIndex + 1; i < Thicknesses.Length; i++)
                    Values[i] = Math.Max(Values[i], LowerBound[i]);

                return Map(Values, Thicknesses, Thickness, MapType.Concentration);
            }
        }

        /// <summary>
        /// Return AirDry at standard thickness. Units: mm/mm
        /// </summary>
        public double[] AirDry { get { return Map(Water.AirDry, Water.Thickness, Thickness, MapType.Concentration); } }

        /// <summary>
        /// Return lower limit at standard thickness. Units: mm/mm
        /// </summary>
        public double[] LL15 { get { return Map(Water.LL15, Water.Thickness, Thickness, MapType.Concentration); } }

        /// <summary>
        /// Return drained upper limit at standard thickness. Units: mm/mm
        /// </summary>
        public double[] DUL { get { return Map(Water.DUL, Water.Thickness, Thickness, MapType.Concentration); } }

        /// <summary>
        /// Return saturation at standard thickness. Units: mm/mm
        /// </summary>
        public double[] SAT { get { return Map(Water.SAT, Water.Thickness, Thickness, MapType.Concentration); } }

        /// <summary>
        /// KS at standard thickness. Units: mm/mm
        /// </summary>
        internal double[] KS { get { return Map(Water.KS, Water.Thickness, Thickness, MapType.Concentration); } }

        /// <summary>
        /// SWCON at standard thickness. Units: 0-1
        /// </summary>
        internal double[] SWCON 
        { 
            get 
            {
                if (SoilWater == null) return null;
                return Map(SoilWater.SWCON, SoilWater.Thickness, Thickness, MapType.Concentration, 0); 
            } 
        }

        /// <summary>
        /// MWCON at standard thickness. Units: 0-1
        /// </summary>
        internal double[] MWCON 
        { 
            get 
            {
                if (SoilWater == null) return null;
                return Map(SoilWater.MWCON, SoilWater.Thickness, Thickness, MapType.Concentration); 
            } 
        }


        /// <summary>
        /// Return the plant available water CAPACITY. Units: mm/mm
        /// </summary>
        public double[] PAWC
        {
            get
            {
                return CalcPAWC(Thickness,
                                LL15,
                                DUL,
                                null);
            }
        }

        /// <summary>
        /// Return the plant available water CAPACITY. Units: mm
        /// </summary>
        public double[] PAWCmm
        {
            get
            {
                return MathUtility.Multiply(PAWC, Water.Thickness);
            }
        }

        public double[] PAW
        {
            get
            {
                return CalcPAWC(Water.Thickness,
                                Water.LL15,
                                SW,
                                null);
            }
        }
        #endregion

        #region Crops

        /// <summary>
        /// A list of crop names.
        /// </summary>
        [XmlIgnore]
        public string[] CropNames { get { return Water.CropNames; } set { Water.CropNames = value; } }

        /// <summary>
        /// Return a specific crop to caller. Will throw if crop doesn't exist.
        /// </summary>
        public SoilCrop Crop(string CropName) 
        { 
            SoilCrop MeasuredCrop = Water.Crop(CropName); 
            if (MeasuredCrop != null)
                return MeasuredCrop;
            SoilCrop Predicted = PredictedCrop(CropName);
            if (Predicted != null)
                return Predicted;
            throw new Exception("Cannot find crop: " + CropName);
        }

        /// <summary>
        /// Crop lower limit mapped. Units: mm/mm
        /// </summary>
        public double[] LL(string CropName)
        {
            return LLMapped(CropName, Thickness);
        }

        /// <summary>
        /// KL mapped. Units: /day
        /// </summary>
        public double[] KL(string CropName)
        {
            SoilCrop SoilCrop = Crop(CropName);
            return Map(SoilCrop.KL, SoilCrop.Thickness, Thickness, MapType.Concentration, 0.0/*SoilCrop.KL.Last()*/);
        }

        /// <summary>
        /// XF mapped. Units: 0-1
        /// </summary>
        public double[] XF(string CropName)
        {
            SoilCrop SoilCrop = Crop(CropName);
            return Map(SoilCrop.XF, SoilCrop.Thickness, Thickness, MapType.Concentration, 0.0/*SoilCrop.XF.Last()*/);
        }

        /// <summary>
        /// Return the plant available water CAPACITY. Units: mm/mm
        /// </summary>
        public double[] PAWCCrop(string CropName)
        {
            return CalcPAWC(Water.Thickness,
                            LLMapped(CropName, Thickness),
                            Water.DUL,
                            XF(CropName));
        }

        /// <summary>
        /// Plant available water for the specified crop. Will throw if crop not found. Units: mm/mm
        /// </summary>
        public double[] PAWCrop(string CropName)
        {
            return CalcPAWC(Water.Thickness,
                            LLMapped(CropName, Thickness),
                            SW,
                            XF(CropName));
        }
        public double[] PAWmm(string CropName)
        {
            return MathUtility.Multiply(PAWCrop(CropName), Thickness);
        }
        #endregion

        #region Predicted Crops
        static string[] BlackVertosolCropList = new string[] { "Wheat", "Sorghum", "Cotton" };
        static string[] GreyVertosolCropList = new string[] { "Wheat", "Sorghum", "Cotton" };
        static double[] PredictedThickness  = new double[] { 150, 150, 300, 300, 300, 300, 300 };
        static double[] PredictedXF = new double[] { 1.00, 1.00, 1.00, 1.00, 1.00, 1.00, 1.00 };
        static double[] WheatKL = new double[] { 0.06, 0.06, 0.06, 0.04, 0.04, 0.02, 0.01 };
        static double[] SorghumKL = new double[] { 0.07, 0.07, 0.07, 0.05, 0.05, 0.04, 0.03 };
        static double[] BarleyKL = new double[] { 0.07, 0.07, 0.07, 0.05, 0.05, 0.03, 0.02 };
        static double[] ChickpeaKL = new double[] { 0.06, 0.06, 0.06, 0.06, 0.06, 0.06, 0.06 };
        static double[] MungbeanKL = new double[] { 0.06, 0.06, 0.06, 0.04, 0.04, 0.00, 0.00 };
        static double[] CottonKL = new double[] { 0.10, 0.10, 0.10, 0.10, 0.09, 0.07, 0.05 };
        static double[] CanolaKL = new double[] { 0.06, 0.06, 0.06, 0.04, 0.04, 0.02, 0.01 };
        static double[] PigeonPeaKL = new double[] { 0.06, 0.06, 0.06, 0.05, 0.04, 0.02, 0.01 };
        static double[] MaizeKL = new double[] { 0.06, 0.06, 0.06, 0.04, 0.04, 0.02, 0.01 };
        static double[] CowpeaKL = new double[] { 0.06, 0.06, 0.06, 0.04, 0.04, 0.02, 0.01 };
        static double[] SunflowerKL = new double[] { 0.01, 0.01, 0.08, 0.06, 0.04, 0.02, 0.01 };
        static double[] FababeanKL = new double[] { 0.08, 0.08, 0.08, 0.08, 0.06, 0.04, 0.03 };
        static double[] LucerneKL = new double[] { 0.01, 0.01, 0.01, 0.01, 0.09, 0.09, 0.09 };
        static double[] PerennialKL = new double[] { 0.01, 0.01, 0.01, 0.01, 0.09, 0.07, 0.05 };

        class BlackVertosol
        {
            internal static double[] CottonA = new double[] { 0.832, 0.868, 0.951, 0.988, 1.043, 1.095, 1.151 };
            internal static double[] SorghumA = new double[] { 0.699, 0.802, 0.853, 0.907, 0.954, 1.003, 1.035 };
            internal static double[] WheatA = new double[] { 0.124, 0.049, 0.024, 0.029, 0.146, 0.246, 0.406 };

            internal static double CottonB = -0.0070;
            internal static double SorghumB = -0.0038;
            internal static double WheatB = 0.0116;

        }
        class GreyVertosol
        {
            internal static double[] CottonA = new double[] { 0.853, 0.851, 0.883, 0.953, 1.022, 1.125, 1.186 };
            internal static double[] SorghumA = new double[] { 0.818, 0.864, 0.882, 0.938, 1.103, 1.096, 1.172 };
            internal static double[] WheatA = new double[] { 0.660, 0.655, 0.701, 0.745, 0.845, 0.933, 1.084 };
            internal static double[] BarleyA = new double[] { 0.847, 0.866, 0.835, 0.872, 0.981, 1.036, 1.152 };
            internal static double[] ChickpeaA = new double[] { 0.435, 0.452, 0.481, 0.595, 0.668, 0.737, 0.875 };
            internal static double[] FababeanA = new double[] { 0.467, 0.451, 0.396, 0.336, 0.190, 0.134, 0.084 };
            internal static double[] MungbeanA = new double[] { 0.779, 0.770, 0.834, 0.990, 1.008, 1.144, 1.150 };
            internal static double CottonB = -0.0082;
            internal static double SorghumB = -0.007;
            internal static double WheatB = -0.0032;
            internal static double BarleyB = -0.0051;
            internal static double ChickpeaB = 0.0029;
            internal static double FababeanB = 0.02455;
            internal static double MungbeanB = -0.0034;
        }

        /// <summary>
        /// Return a list of predicted crop names or an empty string[] if none found.
        /// </summary>
        public string[] PredictedCropNames
        {
            get
            {
                if (SoilType != null)
                {
                    if (SoilType.Equals("Black Vertosol", StringComparison.CurrentCultureIgnoreCase))
                        return BlackVertosolCropList;
                    else if (SoilType.Equals("Grey Vertosol", StringComparison.CurrentCultureIgnoreCase))
                        return GreyVertosolCropList;
                }
                return new string[0];
            }
        }

        /// <summary>
        /// Return a predicted SoilCrop for the specified crop name or null if not found.
        /// </summary>
        private SoilCrop PredictedCrop(string CropName)
        {
            double[] A = null;
            double B = double.NaN;
            double[] KL = null;

            if (SoilType == null)
                return null;

            if (SoilType.Equals("Black Vertosol", StringComparison.CurrentCultureIgnoreCase))
            {
                if (CropName.Equals("Cotton", StringComparison.CurrentCultureIgnoreCase))
                {
                    A = BlackVertosol.CottonA;
                    B = BlackVertosol.CottonB;
                    KL = CottonKL;
                }
                else if (CropName.Equals("Sorghum", StringComparison.CurrentCultureIgnoreCase))
                {
                    A = BlackVertosol.SorghumA;
                    B = BlackVertosol.SorghumB;
                    KL = SorghumKL;
                }
                else if (CropName.Equals("Wheat", StringComparison.CurrentCultureIgnoreCase))
                {
                    A = BlackVertosol.WheatA;
                    B = BlackVertosol.WheatB;
                    KL = WheatKL;
                }
            }
            else if (SoilType.Equals("Grey Vertosol", StringComparison.CurrentCultureIgnoreCase))
            {
                if (CropName.Equals("Cotton", StringComparison.CurrentCultureIgnoreCase))
                {
                    A = GreyVertosol.CottonA;
                    B = GreyVertosol.CottonB;
                    KL = CottonKL;
                }
                else if (CropName.Equals("Sorghum", StringComparison.CurrentCultureIgnoreCase))
                {
                    A = GreyVertosol.SorghumA;
                    B = GreyVertosol.SorghumB;
                    KL = SorghumKL;
                }
                else if (CropName.Equals("Wheat", StringComparison.CurrentCultureIgnoreCase))
                {
                    A = GreyVertosol.WheatA;
                    B = GreyVertosol.WheatB;
                    KL = WheatKL;
                }
                else if (CropName.Equals("Barley", StringComparison.CurrentCultureIgnoreCase))
                {
                    A = GreyVertosol.BarleyA;
                    B = GreyVertosol.BarleyB;
                    KL = BarleyKL;
                }
                else if (CropName.Equals("Chickpea", StringComparison.CurrentCultureIgnoreCase))
                {
                    A = GreyVertosol.ChickpeaA;
                    B = GreyVertosol.ChickpeaB;
                    KL = ChickpeaKL;
                }
                else if (CropName.Equals("Fababean", StringComparison.CurrentCultureIgnoreCase))
                {
                    A = GreyVertosol.FababeanA;
                    B = GreyVertosol.FababeanB;
                    KL = FababeanKL;
                }
                else if (CropName.Equals("Mungbean", StringComparison.CurrentCultureIgnoreCase))
                {
                    A = GreyVertosol.MungbeanA;
                    B = GreyVertosol.MungbeanB;
                    KL = MungbeanKL;
                }
            }


            if (A == null)
                return null;

            double[] LL = PredictedLL(A, B);
            LL = Map(LL, PredictedThickness, Water.Thickness, MapType.Concentration, LL.Last());
            KL = Map(KL, PredictedThickness, Water.Thickness, MapType.Concentration, KL.Last());
            double[] XF = Map(PredictedXF, PredictedThickness, Water.Thickness, MapType.Concentration, PredictedXF.Last());
            string[] Metadata = StringManip.CreateStringArray("Estimated", Water.Thickness.Length);

            return new SoilCrop()
            {
                Thickness = Water.Thickness,
                LL = LL,
                LLMetadata = Metadata,
                KL = KL,
                KLMetadata = Metadata,
                XF = XF,
                XFMetadata = Metadata
            };
        }

        /// <summary>
        /// Calculate and return a predicted LL from the specified A and B values.
        /// </summary>
        private double[] PredictedLL(double[] A, double B)
        {
            double[] DepthCentre = ToMidPoints(PredictedThickness);
            double[] LL15 = LL15Mapped(PredictedThickness);
            double[] DUL = DULMapped(PredictedThickness);
            double[] LL = new double[PredictedThickness.Length];
            for (int i = 0; i != PredictedThickness.Length; i++)
            {
                double DULPercent = DUL[i] * 100.0;
                LL[i] = DULPercent * (A[i] + B * DULPercent);
                LL[i] /= 100.0;

                // Bound the predicted LL values.
                LL[i] = Math.Max(LL[i], LL15[i]);
                LL[i] = Math.Min(LL[i], DUL[i]);
            }

            //  make the top 3 layers the same as the the top 3 layers of LL15
            if (LL.Length >= 3)
            {
                LL[0] = LL15[0];
                LL[1] = LL15[1];
                LL[2] = LL15[2];
            }
            return LL;
        }

        #endregion

        #region Soil organic matter
        /// <summary>
        /// Organic carbon. Units: %
        /// </summary>
        public double[] OC
        {
            get
            {
                double[] SoilOC = SoilOrganicMatter.OCTotal;
                double[] SoilOCThickness = SoilOrganicMatter.Thickness;

                // Try and find a sample with OC in it.
                foreach (Sample Sample in Samples)
                    if (Sample.OverlaySampleOnTo(Sample.OCTotal, Sample.Thickness, ref SoilOC, ref SoilOCThickness))
                        break;
                if (SoilOC == null)
                    return null;
                return Map(SoilOC, SoilOCThickness, Thickness,
                           MapType.Concentration, SoilOC.Last());
            }
        }

        /// <summary>
        /// FBiom. Units: 0-1
        /// </summary>
        public double[] FBiom
        {
            get
            {
                if (SoilOrganicMatter.FBiom == null) return null;
                return Map(SoilOrganicMatter.FBiom, SoilOrganicMatter.Thickness, Thickness,
                           MapType.Concentration, 0.0);
            }
        }

        /// <summary>
        /// FInert. Units: 0-1
        /// </summary>
        public double[] FInert
        {
            get
            {
                if (SoilOrganicMatter.FInert == null) return null;
                return Map(SoilOrganicMatter.FInert, SoilOrganicMatter.Thickness, Thickness,
                           MapType.Concentration, 0.0);
            }
        }
        #endregion

        #region Analysis
        /// <summary>
        /// PH. Units: 1:5 water
        /// </summary>
        public double[] PH
        {
            get
            {
                if (Analysis.PH == null) return null;
                return Map(Analysis.PHWater, 
                           Analysis.Thickness, Thickness,
                           MapType.Concentration, Analysis.PH.Last());
            }
        }

        /// <summary>
        /// Rocks. Units: %
        /// </summary>
        public double[] Rocks { get { return Map(Analysis.Rocks, Analysis.Thickness, Thickness, MapType.Concentration); } }

        /// <summary>
        /// Particle size sand
        /// </summary>
        public double[] ParticleSizeSand
        {
            get
            {
                if (Analysis.ParticleSizeSand == null) return null;
                return Map(Analysis.ParticleSizeSand, Analysis.Thickness, Thickness,
                           MapType.Concentration, 0.0);
            }
        }

        /// <summary>
        /// Particle size silt
        /// </summary>
        public double[] ParticleSizeSilt
        {
            get
            {
                if (Analysis.ParticleSizeSilt == null) return null;
                return Map(Analysis.ParticleSizeSilt, Analysis.Thickness, Thickness,
                           MapType.Concentration, 0.0);
            }
        }

        /// <summary>
        /// Particle size silt
        /// </summary>
        public double[] ParticleSizeClay
        {
            get
            {
                if (Analysis.ParticleSizeClay == null) return null;
                return Map(Analysis.ParticleSizeClay, Analysis.Thickness, Thickness,
                           MapType.Concentration, 0.0);
            }
        }
        #endregion

        #region Sample

        /// <summary>
        /// Find the specified sample. Will throw if not found.
        /// </summary>
        public Sample FindSample(string SampleName)
        {
            foreach (Sample Sample in Samples)
                if (Sample.Name.Equals(SampleName, StringComparison.CurrentCultureIgnoreCase))
                    return Sample;
            throw new Exception("Cannot find soil sample named: " + SampleName);
        }

        /// <summary>
        /// Nitrate (ppm).
        /// </summary>
        public double[] NO3
        {
            get
            {
                foreach (Sample Sample in Samples)
                {
                    if (MathUtility.ValuesInArray(Sample.NO3ppm(this)))
                    {
                        double[] Values = Sample.NO3ppm(this);
                        double[] Thicknesses = Sample.Thickness;                
                        return Map(Values, Thicknesses, Thickness, MapType.Concentration, 1.0 /*Sample.NO3ppm(this).Last()*/);
                    }
                }
                return null;
            }
        }

        /// <summary>
        /// Nitrate (ppm).
        /// </summary>
        public double[] NH4
        {
            get
            {
                foreach (Sample Sample in Samples)
                {
                    if (MathUtility.ValuesInArray(Sample.NH4))
                    {
                        double[] Values = Sample.NH4ppm(this);
                        double[] Thicknesses = Sample.Thickness;                
                        return Map(Values, Thicknesses, Thickness, MapType.Concentration, 0.2);
                    }
                }
                return null;
            }
        }

        /// <summary>
        /// Cloride from either a sample or from Analysis. Units: mg/kg
        /// </summary>
        public double[] Cl
        {
            get
            {
                double[] Values = Analysis.CL;
                double[] Thicknesses = Analysis.Thickness;

                // Try and find a sample with CL in it.
                foreach (Sample Sample in Samples)
                    if (Sample.OverlaySampleOnTo(Sample.CL, Sample.Thickness, ref Values, ref Thicknesses))
                        break;
                if (Values != null)
                    return Map(Values, Thicknesses, Thickness,
                               MapType.Concentration, 0.0 /*LastValue(Values)*/);
                return Values;
            }
        }

        /// <summary>
        /// ESP from either a sample or from Analysis. Units: %
        /// </summary>
        public double[] ESP
        {
            get
            {
                double[] Values = Analysis.ESP;
                double[] Thicknesses = Analysis.Thickness;

                // Try and find a sample with ESP in it.
                foreach (Sample Sample in Samples)
                    if (Sample.OverlaySampleOnTo(Sample.ESP, Sample.Thickness, ref Values, ref Thicknesses))
                        break;
                if (Values != null)
                    return Map(Values, Thicknesses, Thickness,
                               MapType.Concentration, LastValue(Values));
                return Values;
            }
        }

        /// <summary>
        /// EC from either a sample or from Analysis. Units: 1:5 dS/m
        /// </summary>
        public double[] EC
        {
            get
            {
                double[] Values = Analysis.EC;
                double[] Thicknesses = Analysis.Thickness;

                // Try and find a sample with EC in it.
                foreach (Sample Sample in Samples)
                    if (Sample.OverlaySampleOnTo(Sample.EC, Sample.Thickness, ref Values, ref Thicknesses))
                        break;
                if (Values != null)
                    return Map(Values, Thicknesses, Thickness,
                               MapType.Concentration, LastValue(Values));
                return null;
            }
        }

        #endregion

        #region Phosphorus
        /// <summary>
        /// LabileP at standard thickness
        /// </summary>
        public double[] LabileP
        {
            get
            {
                if (Phosphorus == null)
                    return null;
                return Map(Phosphorus.LabileP, Phosphorus.Thickness, Thickness, MapType.Concentration);
            }
        }
        /// <summary>
        /// Sorption at standard thickness
        /// </summary>
        public double[] Sorption
        {
            get
            {
                if (Phosphorus == null)
                    return null;
                return Map(Phosphorus.Sorption, Phosphorus.Thickness, Thickness, MapType.Concentration);
            }
        }

        /// <summary>
        /// BandedP at standard thickness
        /// </summary>
        public double[] BandedP
        {
            get
            {
                if (Phosphorus == null)
                    return null;
                return Map(Phosphorus.BandedP, Phosphorus.Thickness, Thickness, MapType.Concentration);
            }
        }

        /// <summary>
        /// RockP at standard thickness
        /// </summary>
        public double[] RockP
        {
            get
            {
                if (Phosphorus == null)
                    return null;
                return Map(Phosphorus.RockP, Phosphorus.Thickness, Thickness, MapType.Concentration);
            }
        }


        #endregion

        #region SWIM
        /// <summary>
        /// Swim.SwimSoluteParameters.NO3Exco at standard thickness
        /// </summary>
        public double[] NO3Exco
        {
            get
            {
                if (Swim == null || Swim.SwimSoluteParameters == null)
                    return null;
                return Map(Swim.SwimSoluteParameters.NO3Exco, Swim.SwimSoluteParameters.Thickness, Thickness, MapType.Concentration);
            }
        }

        /// <summary>
        /// Swim.SwimSoluteParameters.NO3FIP at standard thickness
        /// </summary>
        public double[] NO3FIP
        {
            get
            {
                if (Swim == null || Swim.SwimSoluteParameters == null)
                    return null;
                return Map(Swim.SwimSoluteParameters.NO3FIP, Swim.SwimSoluteParameters.Thickness, Thickness, MapType.Concentration);
            }
        }

        /// <summary>
        /// Swim.SwimSoluteParameters.NH4Exco at standard thickness
        /// </summary>
        public double[] NH4Exco
        {
            get
            {
                if (Swim == null || Swim.SwimSoluteParameters == null)
                    return null;
                return Map(Swim.SwimSoluteParameters.NH4Exco, Swim.SwimSoluteParameters.Thickness, Thickness, MapType.Concentration);
            }
        }

        /// <summary>
        /// Swim.SwimSoluteParameters.NH4FIP at standard thickness
        /// </summary>
        public double[] NH4FIP
        {
            get
            {
                if (Swim == null || Swim.SwimSoluteParameters == null)
                    return null;
                return Map(Swim.SwimSoluteParameters.NH4FIP, Swim.SwimSoluteParameters.Thickness, Thickness, MapType.Concentration);
            }
        }

        /// <summary>
        /// Swim.SwimSoluteParameters.UreaExco at standard thickness
        /// </summary>
        public double[] UreaExco
        {
            get
            {
                if (Swim == null || Swim.SwimSoluteParameters == null)
                    return null;
                return Map(Swim.SwimSoluteParameters.UreaExco, Swim.SwimSoluteParameters.Thickness, Thickness, MapType.Concentration);
            }
        }

        /// <summary>
        /// Swim.SwimSoluteParameters.UreaFIP at standard thickness
        /// </summary>
        public double[] UreaFIP
        {
            get
            {
                if (Swim == null || Swim.SwimSoluteParameters == null)
                    return null;
                return Map(Swim.SwimSoluteParameters.UreaFIP, Swim.SwimSoluteParameters.Thickness, Thickness, MapType.Concentration);
            }
        }

        /// <summary>
        /// Swim.SwimSoluteParameters.ClExco at standard thickness
        /// </summary>
        public double[] ClExco
        {
            get
            {
                if (Swim == null || Swim.SwimSoluteParameters == null)
                    return null;
                return Map(Swim.SwimSoluteParameters.ClExco, Swim.SwimSoluteParameters.Thickness, Thickness, MapType.Concentration);
            }
        }

        /// <summary>
        /// Swim.SwimSoluteParameters.ClFIP at standard thickness
        /// </summary>
        public double[] ClFIP
        {
            get
            {
                if (Swim == null || Swim.SwimSoluteParameters == null)
                    return null;
                return Map(Swim.SwimSoluteParameters.ClFIP, Swim.SwimSoluteParameters.Thickness, Thickness, MapType.Concentration);
            }
        }
        #endregion
        
        #region Mapping
        /// <summary>
        /// Bulk density - mapped to the specified layer structure. Units: mm/mm
        /// </summary>
        internal double[] BDMapped(double[] ToThickness)
        {
            return Map(Water.BD, Water.Thickness, ToThickness, MapType.Concentration, 0.0 /*Water.BD.Last()*/);
        }

        /// <summary>
        /// AirDry - mapped to the specified layer structure. Units: mm/mm
        /// </summary>
        internal double[] AirDryMapped(double[] ToThickness)
        {
            return Map(Water.AirDry, Water.Thickness, ToThickness, MapType.Concentration, Water.AirDry.Last());
        }

        /// <summary>
        /// Lower limit 15 bar - mapped to the specified layer structure. Units: mm/mm
        /// </summary>
        internal double[] LL15Mapped(double[] ToThickness)
        {
            return Map(Water.LL15, Water.Thickness, ToThickness, MapType.Concentration, Water.LL15.Last());
        }

        /// <summary>
        /// Drained upper limit - mapped to the specified layer structure. Units: mm/mm
        /// </summary>
        internal double[] DULMapped(double[] ToThickness)
        {
            return Map(Water.DUL, Water.Thickness, ToThickness, MapType.Concentration, Water.DUL.Last());
        }

        /// <summary>
        /// Crop lower limit mapped. Units: mm/mm
        /// </summary>
        internal double[] LLMapped(string CropName, double[] ToThickness)
        {
            SoilCrop SoilCrop = Crop(CropName);
            double[] Values = Map(SoilCrop.LL, SoilCrop.Thickness, ToThickness, MapType.Concentration, LastValue(SoilCrop.LL));
            if (Values == null) return null;
            double[] AirDry = AirDryMapped(ToThickness);
            double[] DUL = DULMapped(ToThickness);
            for (int i = 0; i < Values.Length; i++)
            {
                Values[i] = Math.Max(Values[i], AirDry[i]);
                Values[i] = Math.Min(Values[i], DUL[i]);
            }
            return Values;
        }

        private enum MapType { Mass, Concentration, UseBD }
        /// <summary>
        /// Map soil variables from one layer structure to another.
        /// </summary>
        private double[] Map(double[] FromValues, double[] FromThickness,
                             double[] ToThickness, MapType MapType,
                             double DefaultValueForBelowProfile = double.NaN)
        {
            if (FromValues == null)
                return null;

            // remove missing layers.
            for (int i = 0; i < FromValues.Length; i++)
            {
                if (double.IsNaN(FromValues[i]) || double.IsNaN(FromThickness[i]))
                {
                    FromValues[i] = double.NaN;
                    FromThickness[i] = double.NaN;
                }
            }
            FromValues = MathUtility.RemoveMissingValuesFromBottom(FromValues);
            FromThickness = MathUtility.RemoveMissingValuesFromBottom(FromThickness);

            if (MathUtility.AreEqual(FromThickness, ToThickness))
                return FromValues;

            if (FromValues.Length != FromThickness.Length)
            {
                throw new Exception("Cannot redistribute soil sample layer structure to soil layer structure. " +
                                    "The number of values in the sample doesn't match the number of layers in the sample.");
            }

            // Add the default value if it was specified.
            if (!double.IsNaN(DefaultValueForBelowProfile))
            {
                Array.Resize(ref FromThickness, FromThickness.Length + 1);
                Array.Resize(ref FromValues, FromValues.Length + 1);
                FromThickness[FromThickness.Length - 1] = 3000;  // to push to profile deep.
                FromValues[FromValues.Length - 1] = DefaultValueForBelowProfile;
            }

            // If necessary convert FromValues to a mass.
            if (MapType == Soil.MapType.Concentration)
                FromValues = MathUtility.Multiply(FromValues, FromThickness);
            else if (MapType == Soil.MapType.UseBD)
            {
                double[] BD = Water.BD;
                for (int Layer = 0; Layer < FromValues.Length; Layer++)
                    FromValues[Layer] = FromValues[Layer] * BD[Layer] * FromThickness[Layer] / 100;
            }

            // Remapping is achieved by first constructing a map of
            // cumulative mass vs depth
            // The new values of mass per layer can be linearly
            // interpolated back from this shape taking into account
            // the rescaling of the profile.

            double[] CumDepth = new double[FromValues.Length + 1];
            double[] CumMass = new double[FromValues.Length + 1];
            CumDepth[0] = 0.0;
            CumMass[0] = 0.0;
            for (int Layer = 0; Layer < FromThickness.Length; Layer++)
            {
                CumDepth[Layer + 1] = CumDepth[Layer] + FromThickness[Layer];
                CumMass[Layer + 1] = CumMass[Layer] + FromValues[Layer];
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

            // If necessary convert FromValues back into their former units.
            if (MapType == Soil.MapType.Concentration)
                ToMass = MathUtility.Divide(ToMass, ToThickness);
            else if (MapType == Soil.MapType.UseBD)
            {
                double[] BD = BDMapped(ToThickness);
                for (int Layer = 0; Layer < FromValues.Length; Layer++)
                    ToMass[Layer] = ToMass[Layer] * 100.0 / BD[Layer] / ToThickness[Layer];
            }

            for (int i = 0; i < ToMass.Length; i++)
                if (double.IsNaN(ToMass[i]))
                    ToMass[i] = 0.0;
            return ToMass;
        }


        #endregion

        #region Utility
        /// <summary>
        /// Convert an array of thickness (mm) to depth strings (cm)
        //    e.g. "0-10", "10-30"
        /// </summary>
        static public string[] ToDepthStrings(double[] Thickness)
        {
            if (Thickness == null)
                return null;
            string[] Strings = new string[Thickness.Length];
            double DepthSoFar = 0;
            for (int i = 0; i != Thickness.Length; i++)
            {
                if (Thickness[i] == MathUtility.MissingValue)
                    Strings[i] = "";
                else
                {
                    double ThisThickness = Thickness[i] / 10; // to cm
                    double TopOfLayer = DepthSoFar;
                    double BottomOfLayer = DepthSoFar + ThisThickness;
                    Strings[i] = TopOfLayer.ToString() + "-" + BottomOfLayer.ToString();
                    DepthSoFar = BottomOfLayer;
                }
            }
            return Strings;
        }
        /// <summary>
        /// Convert an array of depth strings (cm) to thickness (mm) e.g.
        ///     "0-10", "10-30" 
        /// To 
        ///     100, 200
        /// </summary>
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

                    double TopOfLayer = Convert.ToDouble(DepthStrings[i].Substring(0, PosDash));
                    double BottomOfLayer = Convert.ToDouble(DepthStrings[i].Substring(PosDash + 1));
                    Thickness[i] = (BottomOfLayer - TopOfLayer) * 10;
                }
            }
            return Thickness;
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
        static public string[] CodeToMetaData(string[] Codes)
        {
            string[] Metadata = new string[Codes.Length];
            for (int i = 0; i < Codes.Length; i++)
                if (Codes[i] == "FM")
                    Metadata[i] = "Field measured and checked for sensibility";
                else if (Codes[i] == "C_grav")
                    Metadata[i] = "Calculated from gravimetric moisture when profile wet but drained";
                else if (Codes[i] == "E")
                    Metadata[i] = "Estimated based on local knowledge";
                else if (Codes[i] == "U")
                    Metadata[i] = "Unknown source or quality of data";
                else if (Codes[i] == "LM")
                    Metadata[i] = "Laboratory measured";
                else if (Codes[i] == "V")
                    Metadata[i] = "Volumetric measurement";
                else if (Codes[i] == "M")
                    Metadata[i] = "Mass measured";
                else if (Codes[i] == "C_bd")
                    Metadata[i] = "Calculated from measured, estimated or calculated BD";
                else if (Codes[i] == "C_pt")
                    Metadata[i] = "Developed using a pedo-transfer function";
                else
                    Metadata[i] = Codes[i];
            return Metadata;
        }

        /// <summary>
        /// Plant available water for the specified crop. Will throw if crop not found. Units: mm/mm
        /// </summary>
        static private double[] CalcPAWC(double[] Thickness, double[] LL, double[] DUL, double[] XF)
        {
            double[] PAWC = new double[Thickness.Length];

            if (Thickness.Length != DUL.Length || Thickness.Length != LL.Length)
                return null;

            for (int layer = 0; layer != Thickness.Length; layer++)
                if (DUL[layer] == MathUtility.MissingValue ||
                    LL[layer] == MathUtility.MissingValue)
                    PAWC[layer] = 0;
                else
                    PAWC[layer] = Math.Max(DUL[layer] - LL[layer], 0.0);

            bool ZeroXFFound = false;
            for (int layer = 0; layer != Thickness.Length; layer++)
                if (ZeroXFFound || XF != null && XF[layer] == 0)
                {
                    ZeroXFFound = true;
                    PAWC[layer] = 0;
                }
            return PAWC;
        }

        /// <summary>
        /// Return the last value that isn't a missing value.
        /// </summary>
        private double LastValue(double[] Values)
        {
            if (Values == null) return double.NaN;
            for (int i = Values.Length - 1; i >= 0; i--)
                if (!double.IsNaN(Values[i]))
                    return Values[i];
            return double.NaN;
        }

        #endregion

        #region Checking
        /// <summary>
        /// Checks validity of soil water parameters
        /// This is a port of the soilwat2_check_profile routine.
        /// </summary>
        public string Check(bool IgnoreStartingWaterN)
        {
            const double min_sw = 0.0;
            const double specific_bd = 2.65; // (g/cc)
            string Msg = "";

            foreach (string Crop in CropNames)
            {
                double[] LL = this.LLMapped(Crop, Water.Thickness);
                double[] KL = this.KL(Crop);
                double[] XF = this.XF(Crop);

                if (!MathUtility.ValuesInArray(LL) || 
                    !MathUtility.ValuesInArray(KL) ||
                    !MathUtility.ValuesInArray(XF))
                    Msg += "Values for LL, KL or XF are missing for crop " + Crop + "\r\n";

                else
                {
                    for (int layer = 0; layer != Water.Thickness.Length; layer++)
                    {
                        int RealLayerNumber = layer + 1;

                        if (KL[layer] == MathUtility.MissingValue)
                            Msg += Crop + " KL value missing"
                                     + " in layer " + RealLayerNumber.ToString() + "\r\n";

                        else if (MathUtility.GreaterThan(KL[layer], 1, 3))
                            Msg += Crop + " KL value of " + KL[layer].ToString("f3")
                                     + " in layer " + RealLayerNumber.ToString() + " is greater than 1"
                                     + "\r\n";

                        if (XF[layer] == MathUtility.MissingValue)
                            Msg += Crop + " XF value missing"
                                     + " in layer " + RealLayerNumber.ToString() + "\r\n";

                        else if (MathUtility.GreaterThan(XF[layer], 1, 3))
                            Msg += Crop + " XF value of " + XF[layer].ToString("f3")
                                     + " in layer " + RealLayerNumber.ToString() + " is greater than 1"
                                     + "\r\n";

                        if (LL[layer] == MathUtility.MissingValue)
                            Msg += Crop + " LL value missing"
                                     + " in layer " + RealLayerNumber.ToString() + "\r\n";

                        else if (MathUtility.LessThan(LL[layer], Water.AirDry[layer], 3))
                            Msg += Crop + " LL of " + LL[layer].ToString("f3")
                                         + " in layer " + RealLayerNumber.ToString() + " is below air dry value of " + Water.AirDry[layer].ToString("f3")
                                       + "\r\n";

                        else if (MathUtility.GreaterThan(LL[layer], Water.DUL[layer], 3))
                            Msg += Crop + " LL of " + LL[layer].ToString("f3")
                                         + " in layer " + RealLayerNumber.ToString() + " is above drained upper limit of " + Water.DUL[layer].ToString("f3")
                                       + "\r\n";
                    }
                }
            }

            // Check other profile variables.
            for (int layer = 0; layer != Water.Thickness.Length; layer++)
            {
                double max_sw = MathUtility.Round(1.0 - Water.BD[layer] / specific_bd, 3);
                int RealLayerNumber = layer + 1;

                if (Water.AirDry[layer] == MathUtility.MissingValue)
                    Msg += " Air dry value missing"
                             + " in layer " + RealLayerNumber.ToString() + "\r\n";

                else if (MathUtility.LessThan(Water.AirDry[layer], min_sw, 3))
                    Msg += " Air dry lower limit of " + Water.AirDry[layer].ToString("f3")
                                       + " in layer " + RealLayerNumber.ToString() + " is below acceptable value of " + min_sw.ToString("f3")
                               + "\r\n";

                if (Water.LL15[layer] == MathUtility.MissingValue)
                    Msg += "15 bar lower limit value missing"
                             + " in layer " + RealLayerNumber.ToString() + "\r\n";

                else if (MathUtility.LessThan(Water.LL15[layer], Water.AirDry[layer], 3))
                    Msg += "15 bar lower limit of " + Water.LL15[layer].ToString("f3")
                                 + " in layer " + RealLayerNumber.ToString() + " is below air dry value of " + Water.AirDry[layer].ToString("f3")
                               + "\r\n";

                if (Water.DUL[layer] == MathUtility.MissingValue)
                    Msg += "Drained upper limit value missing"
                             + " in layer " + RealLayerNumber.ToString() + "\r\n";

                else if (MathUtility.LessThan(Water.DUL[layer], Water.LL15[layer], 3))
                    Msg += "Drained upper limit of " + Water.DUL[layer].ToString("f3")
                                 + " in layer " + RealLayerNumber.ToString() + " is at or below lower limit of " + Water.LL15[layer].ToString("f3")
                               + "\r\n";

                if (Water.SAT[layer] == MathUtility.MissingValue)
                    Msg += "Saturation value missing"
                             + " in layer " + RealLayerNumber.ToString() + "\r\n";

                else if (MathUtility.LessThan(Water.SAT[layer], Water.DUL[layer], 3))
                    Msg += "Saturation of " + Water.SAT[layer].ToString("f3")
                                 + " in layer " + RealLayerNumber.ToString() + " is at or below drained upper limit of " + Water.DUL[layer].ToString("f3")
                               + "\r\n";

                else if (MathUtility.GreaterThan(Water.SAT[layer], max_sw, 3))
                {
                    double max_bd = (1.0 - Water.SAT[layer]) * specific_bd;
                    Msg += "Saturation of " + Water.SAT[layer].ToString("f3")
                                 + " in layer " + RealLayerNumber.ToString() + " is above acceptable value of  " + max_sw.ToString("f3")
                               + ". You must adjust bulk density to below " + max_bd.ToString("f3")
                               + " OR saturation to below " + max_sw.ToString("f3")
                               + "\r\n";
                }

                if (Water.BD[layer] == MathUtility.MissingValue)
                    Msg += "BD value missing"
                             + " in layer " + RealLayerNumber.ToString() + "\r\n";

                else if (MathUtility.GreaterThan(Water.BD[layer], 2.65, 3))
                    Msg += "BD value of " + Water.BD[layer].ToString("f3")
                                 + " in layer " + RealLayerNumber.ToString() + " is greater than the theoretical maximum of 2.65"
                               + "\r\n";
            }

            if (OC.Length == 0)
                throw new Exception("Cannot find OC values in soil");

            for (int layer = 0; layer != Water.Thickness.Length; layer++)
            {
                int RealLayerNumber = layer + 1;
                if (OC[layer] == MathUtility.MissingValue)
                    Msg += "OC value missing"
                             + " in layer " + RealLayerNumber.ToString() + "\r\n";

                else if (MathUtility.LessThan(OC[layer], 0.01, 3))
                    Msg += "OC value of " + OC[layer].ToString("f3")
                                  + " in layer " + RealLayerNumber.ToString() + " is less than 0.01"
                                  + "\r\n";

                if (PH[layer] == MathUtility.MissingValue)
                    Msg += "PH value missing"
                             + " in layer " + RealLayerNumber.ToString() + "\r\n";

                else if (MathUtility.LessThan(PH[layer], 3.5, 3))
                    Msg += "PH value of " + PH[layer].ToString("f3")
                                  + " in layer " + RealLayerNumber.ToString() + " is less than 3.5"
                                  + "\r\n";
                else if (MathUtility.GreaterThan(PH[layer], 11, 3))
                    Msg += "PH value of " + PH[layer].ToString("f3")
                                  + " in layer " + RealLayerNumber.ToString() + " is greater than 11"
                                  + "\r\n";
            }

            if (!IgnoreStartingWaterN)
            {
                if (!MathUtility.ValuesInArray(SW))
                    Msg += "No starting soil water values found.\r\n";
                else
                    for (int layer = 0; layer != Water.Thickness.Length; layer++)
                    {
                        int RealLayerNumber = layer + 1;

                        if (SW[layer] == MathUtility.MissingValue)
                            Msg += "Soil water value missing"
                                        + " in layer " + RealLayerNumber.ToString() + "\r\n";

                        else if (MathUtility.GreaterThan(SW[layer], Water.SAT[layer], 3))
                            Msg += "Soil water of " + SW[layer].ToString("f3")
                                            + " in layer " + RealLayerNumber.ToString() + " is above saturation of " + Water.SAT[layer].ToString("f3")
                                            + "\r\n";

                        else if (MathUtility.LessThan(SW[layer], Water.AirDry[layer], 3))
                            Msg += "Soil water of " + SW[layer].ToString("f3")
                                            + " in layer " + RealLayerNumber.ToString() + " is below air-dry value of " + Water.AirDry[layer].ToString("f3")
                                            + "\r\n";
                    }

                if (!MathUtility.ValuesInArray(NO3))
                    Msg += "No starting NO3 values found.\r\n";
                if (!MathUtility.ValuesInArray(NH4))
                    Msg += "No starting NH4 values found.\r\n";


            }


            return Msg;
        }

        #endregion

    }
}