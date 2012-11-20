using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using Model;

namespace ApsimFile
{
    public class SwimWaterTable
    {
        [Description("Depth of Water Table (mm)")]
        public double WaterTableDepth { get; set; }
    }

}