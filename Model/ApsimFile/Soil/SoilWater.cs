using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using ModelAttributes;

namespace ApsimFile
{
    public class SoilWater
    {
        [Description("Summer Cona")]
        public double SummerCona { get; set; }
        [Description("Summer U")]
        public double SummerU { get; set; }
        [Description("Summer Date")]
        public string SummerDate { get; set; }
        [Description("Winter Cona")]
        public double WinterCona { get; set; }
        [Description("Winter U")]
        public double WinterU { get; set; }
        [Description("Winter Date")]
        public string WinterDate { get; set; }
        [Description("Diffusivity Constant")]
        public double DiffusConst { get; set; }
        [Description("Diffusivity Slope")]
        public double DiffusSlope { get; set; }
        [Description("Soil albedo")]
        public double Salb { get; set; }
        [Description("Bare soil runoff curve number")]
        public double CN2Bare { get; set; }
        [Description("Max. reduction in curve number due to cover")]
        public double CNRed { get; set; }
        [Description("Cover for max curve number reduction")]
        public double CNCov { get; set; }
        public double Slope { get; set; }
        [Description("Discharge width")]
        public double DischargeWidth { get; set; }
        [Description("Catchment area")]
        public double CatchmentArea { get; set; }
        [Description("Maximum pond")]
        public double MaxPond { get; set; }

        public double[] Thickness { get; set; }
        [Units("0-1")]
        public double[] SWCON { get; set; }
        [Units("0-1")]
        public double[] MWCON { get; set; }
        [Units("mm/day")]
        public double[] KLAT { get; set; }

        public SoilWater()
        {
            SummerCona = double.NaN;
            SummerU = double.NaN;
            WinterCona = double.NaN;
            WinterU = double.NaN;
            DiffusConst = double.NaN;
            DiffusSlope = double.NaN;
            Salb = double.NaN;
            CN2Bare = double.NaN;
            CNRed = double.NaN;
            CNCov = double.NaN;
            Slope = double.NaN;
            DischargeWidth = double.NaN;
            CatchmentArea= double.NaN;
            MaxPond = double.NaN;
        }

    }

}
