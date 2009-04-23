
using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Text;
using System.Windows.Forms;

using Controllers;
using CSGeneral;

namespace GraphUserInterface
    {
    public partial class FilterUI : BaseView
        {
        private ChartPageUI ParentUI;

        public FilterUI()
            {
            InitializeComponent();
            }

        protected override void OnLoad()
            {
            ParentUI = (ChartPageUI)Parent;
            }
        public override void OnRefresh()
            {
            // -----------------------------------------------
            // Called when it's time to refresh
            // -----------------------------------------------
            base.OnRefresh();
            GroupBox.Text = Name;

            FilterBox.TextChanged -= OnFilterChanged;
            List<string> FilterStrings = XmlHelper.Values(Data, "FilterString");
            string[] Filters = new string[FilterStrings.Count];
            FilterStrings.CopyTo(Filters);
            FilterBox.Lines = Filters;
            FilterBox.TextChanged += OnFilterChanged;
            }

        private void OnFilterChanged(object sender, EventArgs e)
            {
            List<string> FilterStrings = new List<string>();
            FilterStrings.AddRange(FilterBox.Lines);
            XmlHelper.SetValues(Data, "FilterString", FilterStrings);
            PublishViewChanged();
            }


        }
    }

