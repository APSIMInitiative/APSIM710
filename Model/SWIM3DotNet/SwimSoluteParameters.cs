using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

    public class SwimSoluteParameters
    {
        [Param(IsOptional = true, MinVal = 0.0, MaxVal = 20.0)]
        [Description("Dispersivity - dis ((cm^2/h)/(cm/h)^p)")]
        public double Dis { get; set; }

        [Param(IsOptional = true, MinVal = 0.0, MaxVal = 5.0)]
        [Description("Dispersivity Power - disp")]
        public double Disp { get; set; }

        [Param(IsOptional = true, MinVal = 0.0, MaxVal = 1.0)]
        [Description("Tortuosity Constant - a")]
        public double A { get; set; }

        [Param(IsOptional = true, MinVal = 0.0, MaxVal = 1.0)]
        [Description("Tortuoisty Offset - dthc")]
        [Units("cm^3/cm^3")]
        public double DTHC { get; set; }

        [Param(IsOptional=true, MinVal = 0.0, MaxVal = 10.0)]
        [Description("Tortuoisty Power - dthp")]
        public double DTHP { get; set; }

        [Param(IsOptional=true)]
        [Description("Water Table Cl Concentration (ppm)")]
        public double WaterTableCl { get; set; }

        [Param(IsOptional=true)]
        [Description("Water Table NO3 Concentration (ppm)")]
        public double WaterTableNO3 { get; set; }

        [Param(IsOptional=true)]
        [Description("Water Table NH4 Concentration (ppm)")]
        public double WaterTableNH4 { get; set; }

        [Param(IsOptional=true)]
        [Description("Water Table Urea Concentration (ppm)")]
        public double WaterTableUrea { get; set; }

        [Param(IsOptional=true)]
        [Description("Water Table Tracer (ppm)")]
        public double WaterTableTracer { get; set; }

        [Param(IsOptional=true)]
        [Description("Water Table Mineralisation Inhibitor (ppm)")]
        public double WaterTableMineralisationInhibitor { get; set; }

        [Param(IsOptional=true)]
        [Description("Water Table Urease Inhibitor (ppm)")]
        public double WaterTableUreaseInhibitor { get; set; }

        [Param(IsOptional=true)]
        [Description("Water Table Nitrification Inhibitor (ppm)")]
        public double WaterTableNitrificationInhibitor { get; set; }

        [Param(IsOptional=true)]
        [Description("Water Table Denitrification Inhibitor (ppm)")]
        public double WaterTableDenitrificationInhibitor { get; set; }

        [Param(IsOptional=true)]
        public double[] Thickness { get; set; }

        [Param(IsOptional=true)]
        public double[] NO3Exco { get; set; }

        [Param(IsOptional=true)]
        public double[] NO3FIP { get; set; }

        [Param(IsOptional=true)]
        public double[] NH4Exco { get; set; }

        [Param(IsOptional=true)]
        public double[] NH4FIP { get; set; }

        [Param(IsOptional=true)]
        public double[] UreaExco { get; set; }

        [Param(IsOptional=true)]
        public double[] UreaFIP { get; set; }

        [Param(IsOptional=true)]
        public double[] ClExco { get; set; }

        [Param(IsOptional=true)]
        public double[] ClFIP { get; set; }

        [Param(IsOptional=true)]
        public double[] TracerExco { get; set; }

        [Param(IsOptional=true)]
        public double[] TracerFIP { get; set; }

        [Param(IsOptional=true)]
        public double[] MineralisationInhibitorExco { get; set; }

        [Param(IsOptional=true)]
        public double[] MineralisationInhibitorFIP { get; set; }

        [Param(IsOptional=true)]
        public double[] UreaseInhibitorExco { get; set; }

        [Param(IsOptional=true)]
        public double[] UreaseInhibitorFIP { get; set; }

        [Param(IsOptional=true)]
        public double[] NitrificationInhibitorExco { get; set; }

        [Param(IsOptional=true)]
        public double[] NitrificationInhibitorFIP { get; set; }

        [Param(IsOptional=true)]
        public double[] DenitrificationInhibitorExco { get; set; }

        [Param(IsOptional=true)]
        public double[] DenitrificationInhibitorFIP { get; set; }
    }

