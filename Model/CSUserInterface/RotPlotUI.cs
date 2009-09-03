using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Text;
using System.Windows.Forms;

namespace CSUserInterface
{
    public partial class RotPlotUI : Controllers.BaseView
    {
        public RotPlotUI()
        {
            InitializeComponent();
        }

        public override void OnRefresh()
        {
            base.OnRefresh();
            txtFile.Text = "working";
        }

    }
}
