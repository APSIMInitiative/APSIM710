using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Xml.Serialization;

namespace ApsimFile
{
    public class SoilCrop
    {
        [XmlAttribute("name")]
        public string Name { get; set; }
        public double[] Thickness { get; set; }
        public double[] LL { get; set; }
        public double[] KL { get; set; }
        public double[] XF { get; set; }

        public string[] LLMetadata { get; set; }
        public string[] KLMetadata { get; set; }
        public string[] XFMetadata { get; set; }

    }
}