using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Linq;
using System.Text;
using System.Windows.Forms;
using CSGeneral;
using ApsimFile;

namespace CSUserInterface
{
    public partial class ManageCropsForm : Form
    {
        Soil Soil;
        public ManageCropsForm(Soil S)
        {
            InitializeComponent();
            Soil = S;
        }

        private void ManageCropsFormLoad(object sender, EventArgs e)
        {
            GridUtility.AddColumn(Grid, "Crop name", Soil.CropNames);
            Grid.RowCount = 50;

        }

        public string[] CropNames { get { return GridUtility.GetColumnAsStrings(Grid, 0); } }
    }
}
