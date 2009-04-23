
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
    public partial class REMSUI : BaseView
        {
        private ChartPageUI ParentUI;

        public REMSUI()
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

            FileNameEdit.TextChanged -= OnFileNameChanged;
            ExperimentDropDown.TextChanged -= OnExperimentChanged;
            TreatmentDropDown.TextChanged -= OnTreatmentChanged;
            DataSourceDropDown.TextChanged -= OnDataSourceChanged;

            FileNameEdit.Text = XmlHelper.Value(Data, "FileName");
            ExperimentDropDown.Items.Clear();
            ExperimentDropDown.Items.AddRange(ParentUI.Processor.GetExperimentNames(FileNameEdit.Text));
            ExperimentDropDown.Text = XmlHelper.Value(Data, "Experiment");

            TreatmentDropDown.Items.Clear();
            TreatmentDropDown.Items.AddRange(ParentUI.Processor.GetTreatmentNames(FileNameEdit.Text, ExperimentDropDown.Text));
            TreatmentDropDown.Text = XmlHelper.Value(Data, "Treatment");

            DataSourceDropDown.Text = XmlHelper.Value(Data, "DataSource");

            FileNameEdit.TextChanged += OnFileNameChanged;
            ExperimentDropDown.TextChanged += OnExperimentChanged;
            TreatmentDropDown.TextChanged += OnTreatmentChanged;
            DataSourceDropDown.TextChanged += OnDataSourceChanged;
            }

        private void OnFileNameChanged(object sender, EventArgs e)
            {
            XmlHelper.SetValue(Data, "FileName", FileNameEdit.Text);
            PublishViewChanged();
            }
        private void OnExperimentChanged(object sender, EventArgs e)
            {
            XmlHelper.SetValue(Data, "Experiment", ExperimentDropDown.Text);
            PublishViewChanged();
            }
        private void OnTreatmentChanged(object sender, EventArgs e)
            {
            XmlHelper.SetValue(Data, "Treatment", TreatmentDropDown.Text);
            PublishViewChanged();
            }
        private void OnDataSourceChanged(object sender, EventArgs e)
            {
            XmlHelper.SetValue(Data, "DataSource", DataSourceDropDown.Text);
            PublishViewChanged();
            }

        private void OnBrowseClick(object sender, EventArgs e)
            {
            if (OpenFileDialog.ShowDialog() == DialogResult.OK)
                FileNameEdit.Text = OpenFileDialog.FileName;
            }

        }
    }

