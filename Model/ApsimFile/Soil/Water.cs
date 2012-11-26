﻿using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Xml.Serialization;
using CSGeneral;
using System.Xml;
using ModelAttributes;

namespace ApsimFile
{
    public class Water
    {
        public double[] Thickness { get; set; }

        [Units("g/cc")]
        public double[] BD { get; set; }
        [Units("mm/mm")]
        public double[] AirDry { get; set; }
        [Units("mm/mm")]
        public double[] LL15 { get; set; }
        [Units("mm/mm")]
        public double[] DUL { get; set; }
        [Units("mm/mm")]
        public double[] SAT { get; set; }
        [Units("mm/day")]
        public double[] KS { get; set; }

        public string[] BDMetadata { get; set; }
        public string[] AirDryMetadata { get; set; }
        public string[] LL15Metadata { get; set; }
        public string[] DULMetadata { get; set; }
        public string[] SATMetadata { get; set; }
        public string[] KSMetadata { get; set; }

        [XmlElement("SoilCrop")]
        public List<SoilCrop> Crops { get; set; }


        /// <summary>
        /// Constructor
        /// </summary>
        public Water()
        {
            Crops = new List<SoilCrop>();
        }

        /// <summary>
        /// Return the specified crop to caller. Will return null if not found.
        /// </summary>
        public SoilCrop Crop(string CropName)
        {
            SoilCrop C = FindCrop(CropName);
            if (C == null)
                return null;
            else
                return C;
        }

        /// <summary>
        /// Get or set the names of crops. Note: When setting, the crops will be reorded to match
        /// the setting list of names. Also new crops will be added / deleted as required.
        /// </summary>
        [XmlIgnore]
        public string[] CropNames
        {
            get
            {
                string[] CropNames = new string[Crops.Count];
                for (int i = 0; i < Crops.Count; i++)
                    CropNames[i] = Crops[i].Name;
                return CropNames;
            }
            set
            {
                // Add in extra crops if necessary.
                for (int i = 0; i < value.Length; i++)
                {
                    SoilCrop Crop = FindCrop(value[i]);
                    if (Crop == null)
                        Crops.Add(new SoilCrop() { Name = value[i] });

                }

                // Remove unwanted crops if necessary.
                for (int i = 0; i < Crops.Count; i++)
                {
                    int Pos = StringManip.IndexOfCaseInsensitive(value, Crops[i].Name);
                    if (Pos != -1)
                        Crops[i].Name = value[Pos];  // ensure case transfer.
                    else
                        Crops.Remove(Crops[i]);      // remove unwanted crop.
                }

                // Now reorder.
                for (int i = 0; i < value.Length; i++)
                {
                    int ExistingCropIndex = FindCropIndex(value[i]);
                    if (i != ExistingCropIndex)
                    {
                        SoilCrop C = Crops[ExistingCropIndex];  // grab the crop
                        Crops.RemoveAt(ExistingCropIndex);      // remove it from the list.
                        Crops.Insert(i, C);                     // add it at the correct index.
                    }
                }

            }
        }

        /// <summary>
        /// Return the specified crop to caller. Will return null if not found.
        /// </summary>
        private SoilCrop FindCrop(string CropName)
        {
            foreach (SoilCrop Crop in Crops)
                if (Crop.Name.Equals(CropName, StringComparison.CurrentCultureIgnoreCase))
                    return Crop;
            return null;
        }

        /// <summary>
        /// Return the specified crop to caller. Will return null if not found.
        /// </summary>
        private int FindCropIndex(string CropName)
        {
            for (int i = 0; i < Crops.Count; i++)
                if (Crops[i].Name.Equals(CropName, StringComparison.CurrentCultureIgnoreCase))
                    return i;
            return -1;
        }

    }
}
