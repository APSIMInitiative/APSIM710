
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
    public partial class RegressionUI : BaseView
        {
        private ChartPageUI ParentUI;

        public RegressionUI()
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
            
            XDropDown.TextChanged -= OnXChanged;
            YDropDown.TextChanged -= OnYChanged;

            XDropDown.Items.Clear();
            XDropDown.Items.AddRange(ParentUI.Processor.GetFieldNamesForDataSet(XmlHelper.Value(Data, "source")));
            YDropDown.Items.Clear();
            YDropDown.Items.AddRange(ParentUI.Processor.GetFieldNamesForDataSet(XmlHelper.Value(Data, "source")));
            XDropDown.Text = XmlHelper.Value(Data, "XFieldName");
            YDropDown.Text = XmlHelper.Value(Data, "YFieldName");

            XDropDown.TextChanged += OnXChanged;
            YDropDown.TextChanged += OnYChanged;
            }

        private void OnXChanged(object sender, EventArgs e)
            {
            XmlHelper.SetValue(Data, "XFieldName", XDropDown.Text);
            PublishViewChanged();
            }

        private void OnYChanged(object sender, EventArgs e)
            {
            XmlHelper.SetValue(Data, "YFieldName", YDropDown.Text);
            PublishViewChanged();
            }


        }
    }

