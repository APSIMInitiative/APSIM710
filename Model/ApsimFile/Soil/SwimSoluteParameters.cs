using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using Model;

namespace ApsimFile
{
    public class SwimSoluteParameters
    {
        [Description("Dispersivity - dis ((cm^2/h)/(cm/h)^p)")]
        public double Dis { get; set; }
        [Description("Dispersivity Power - disp")]
        public double Disp { get; set; }
        [Description("Tortuosity Constant - a")]
        public double A { get; set; }
        [Description("Tortuoisty Offset - dthc")]
        public double DTHC { get; set; }
        [Description("Tortuoisty Power - dthp")]
        public double DTHP { get; set; }
        [Description("Water Table Cl Concentration (ppm)")]
        public double WaterTableCl { get; set; }
        [Description("Water Table NO3 Concentration (ppm)")]
        public double WaterTableNO3 { get; set; }
        [Description("Water Table NH4 Concentration (ppm)")]
        public double WaterTableNH4 { get; set; }
        [Description("Water Table Urea Concentration (ppm)")]
        public double WaterTableUrea { get; set; }
        public double[] Thickness { get; set; }
        public double[] NO3Exco { get; set; }
        public double[] NO3FIP { get; set; }
        public double[] NH4Exco { get; set; }
        public double[] NH4FIP { get; set; }
        public double[] UreaExco { get; set; }
        public double[] UreaFIP { get; set; }
        public double[] ClExco { get; set; }
        public double[] ClFIP { get; set; }

    }

}