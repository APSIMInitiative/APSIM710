﻿using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using ModelAttributes;

namespace ApsimFile
{
    public class SoilTemperature
    {
        [Description("Boundary layer conductance")]
        [Units("(W/m2/K)")]
        public double BoundaryLayerConductance { get; set; }

        public double[] Thickness { get; set; }

        [Description("Initial soil temperature")]
        [Units("oC")]
        public double[] InitialSoilTemperature { get; set; }
    }

}


      