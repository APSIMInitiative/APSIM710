
using System;
using System.Xml;

using CSGeneral;

namespace ApsimFile
	{
	//------------------------------------
	// Encapsulates an initial water type
	// -----------------------------------
	public class InitWater
		{
        private XmlNode Data;
		private Soil ParentSoil;

		public InitWater(XmlNode data, Soil ParentSoil)
			{
            // -----------
            // Constructor
            // -----------

            this.ParentSoil = ParentSoil;
            Data = data;
			}

		public enum MethodType {Percent, DepthWetSoil, Layered};
		public MethodType Method
			{
			get {
                if (XmlHelper.ChildNames(Data, "profile").Length > 0)
                    return MethodType.Layered;
                else if (XmlHelper.Find(Data, "DepthWetSoilMethod") != null)
                    return MethodType.DepthWetSoil;
			    else
				    return MethodType.Percent;
				}
			}

		public int Percent
			{
			get {
                string PercentString = XmlHelper.Value(Data, "PercentMethod/Percent");
                if (PercentString == "")
                    return 0;
                else
                    return Convert.ToInt32(Convert.ToDouble(PercentString) * 100);
                }
			}
		public bool FilledFromTop
			{
            get { return (XmlHelper.Value(Data, "PercentMethod/Distributed").ToLower() == "filled from top"); }
			}
		public int DepthWetSoil
			{
            get { return Convert.ToInt32(XmlHelper.Value(Data, "DepthWetSoilMethod/Depth")); }
			}
        public string RelativeTo
            {
            get {
            string value = XmlHelper.Value(Data, "RelativeTo");
                if (value == "")
                    value = "ll15";
                return value;
                }
            set {
                XmlHelper.SetValue(Data, "RelativeTo", value);
                }
            }

        public double[] Thickness
            {
            get {
                if (Method == MethodType.Layered)
                    return SoilComponentUtility.getLayered(Data, "profile", "thickness", "");
                else
                    return ParentSoil.Thickness;
                }
                set { SoilComponentUtility.setLayered(Data, "profile", "thickness", "", value); }
            }
		public double[] SW
			{
			get {
                double[] ll;
                double[] pawc;
                if (RelativeTo == "ll15")
                    {
                    ll = ParentSoil.LL15;
                    pawc = ParentSoil.PAWC();
                    }
                else
                    {
                    ll = ParentSoil.LL(RelativeTo);
                    pawc = ParentSoil.PAWC(RelativeTo);
                    }
                
				double[] dul = ParentSoil.DUL;
				double[] sw = new double[ll.Length];
				switch (Method)
					{
					case MethodType.Percent: 
						{
						if (FilledFromTop)
							{
							double AmountWater = MathUtility.Sum(pawc) * (Percent / 100.0);
							for (int Layer = 0; Layer < ll.Length; Layer++)
								{
								if (AmountWater >= pawc[Layer])
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
						break;
						}
					case MethodType.DepthWetSoil:
						{
						double[] Thickness = ParentSoil.Thickness;
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
						break;
						}
					case MethodType.Layered:
                        sw = SoilComponentUtility.getLayered(Data, "profile", "sw", "");
						break;
					}
				return sw;
				}
			}
		public double[] SWMM
			{
			get {return MathUtility.Multiply(SW, Thickness);}
			set {MathUtility.Divide(SW, Thickness);}
			}
        public double[] SWMapedToSoil
            {
            get
                {
                if (Method == MethodType.Layered)
                    {
                    double[] DefaultValues = ParentSoil.LL15;
                    return SoilComponentUtility.MapSampleToSoilUsingSpatial(SW, Thickness, DefaultValues, ParentSoil.Thickness);
                    }
                else
                    return SW;
                }
            }

		public void SetUsingPercent(int Percent, bool FilledFromTop)
			{
            // ----------------------------------
            // Set water via the percent method.
            // ----------------------------------
            foreach (XmlNode Child in XmlHelper.ChildNodes(Data, ""))
                {
                if (XmlHelper.Type(Child).ToLower() == "depthwetsoilmethod" ||
                    XmlHelper.Type(Child).ToLower() == "profile")
                    Child.ParentNode.RemoveChild(Child);
                }
			double Prop = Percent / 100.0;
			XmlHelper.SetValue(Data, "PercentMethod/Percent", Prop.ToString("f2"));
			string Distributed = "Filled from top";
			if (!FilledFromTop)
				Distributed = "Evenly distributed";
			XmlHelper.SetValue(Data, "PercentMethod/Distributed", Distributed);
			}
		public void SetUsingDepthWetSoil(int Depth)
			{
            // ----------------------------------
            // Set water via the depth wet soil method.
            // ----------------------------------
            foreach (XmlNode Child in XmlHelper.ChildNodes(Data, ""))
                {
                if (XmlHelper.Type(Child).ToLower() == "percentmethod" ||
                    XmlHelper.Type(Child).ToLower() == "profile")
                    Child.ParentNode.RemoveChild(Child);
                }
            XmlHelper.SetValue(Data, "DepthWetSoilMethod/Depth", Depth.ToString());
			}
		public void SetUsingLayered(double[] sw)
			{
            // ----------------------------------
            // Set water via the layered method.
            // ----------------------------------
            if (sw.Length > 0)
                {
                foreach (XmlNode Child in XmlHelper.ChildNodes(Data, ""))
                    {
                    if (XmlHelper.Type(Child).ToLower() == "percentmethod" ||
                        XmlHelper.Type(Child).ToLower() == "depthwetsoilmethod")
                        Child.ParentNode.RemoveChild(Child);
                    }
                    SoilComponentUtility.setLayered(Data, "profile", "sw", "", sw);
                }
			}
        public double[] PAW(string cropName)
            {
            double[] Thickness = this.Thickness;
            double[] LL = ParentSoil.LL(cropName);
            double[] SW = this.SW;
            double[] PAW = new double[Thickness.Length];

            // calculate depth increments.
            if (SW.Length == Thickness.Length &&
                LL.Length == Thickness.Length &&
                Thickness.Length > 0)
                {
                for (int layer = 0; layer != SW.Length; layer++)
                    PAW[layer] = (SW[layer] - LL[layer]) * Thickness[layer];
                }
            return PAW;
            }

        public XmlNode ExportToSim(XmlNode ParentNode)
            {
                XmlHelper.SetValue(ParentNode, "sw", SoilComponentUtility.LayeredToString(SWMapedToSoil));
            return ParentNode;
            }
        }
	}
