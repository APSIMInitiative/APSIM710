using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using CSGeneral;
using System.Xml.Serialization;
using ModelAttributes;

namespace ApsimFile
{
    public class Phosphorus
    {
        [XmlAttribute("name")]
        public string Name { get; set; }

        [Description("Root C:P ratio")]
        public double RootCP { get; set; }
        [Description("Rate disolved rock")]
        public double RateDissolRock { get; set; }
        [Description("Rate loss available")]
        public double RateLossAvail { get; set; }
        [Description("Sorption coefficient")]
        public double SorptionCoeff { get; set; }

        public double[] Thickness { get; set; }
        [Units("mg/kg")]
        public double[] LabileP { get; set; }
        [Units("kg/ha")]
        public double[] BandedP { get; set; }
        [Units("kg/ha")]
        public double[] RockP { get; set; }
        [Units("-")]
        public double[] Sorption { get; set; }
    }


}
