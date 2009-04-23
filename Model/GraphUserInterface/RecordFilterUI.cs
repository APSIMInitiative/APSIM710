
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
    public partial class RecordFilterUI : BaseView
        {
        private ChartPageUI ParentUI;

        public RecordFilterUI()
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

            FirstRecordCheck.CheckedChanged -= OnFirstRecordChanged;
            LastRecordCheck.CheckedChanged -= OnLastRecordChanged;
            RecordNumberEdit.TextChanged -= OnRecordNumberChanged;

            FirstRecordCheck.Checked = (XmlHelper.Value(Data, "FirstRecord").ToLower() == "yes");
            LastRecordCheck.Checked = (XmlHelper.Value(Data, "LastRecord").ToLower() == "yes");
            RecordNumberEdit.Text = XmlHelper.Value(Data, "RecordNumber");

            FirstRecordCheck.CheckedChanged += OnFirstRecordChanged;
            LastRecordCheck.CheckedChanged += OnLastRecordChanged;
            RecordNumberEdit.TextChanged += OnRecordNumberChanged;
            }


        private void OnFirstRecordChanged(object sender, EventArgs e)
            {
            string YesNo;
            if (FirstRecordCheck.Checked)
                YesNo = "Yes";
            else
                YesNo = "No";
            XmlHelper.SetValue(Data, "FirstRecord", YesNo);
            PublishViewChanged();
            }
        private void OnLastRecordChanged(object sender, EventArgs e)
            {
            string YesNo;
            if (LastRecordCheck.Checked)
                YesNo = "Yes";
            else
                YesNo = "No";
            XmlHelper.SetValue(Data, "LastRecord", YesNo);
            PublishViewChanged();
            }
        private void OnRecordNumberChanged(object sender, EventArgs e)
            {
            XmlHelper.SetValue(Data, "RecordNumber", RecordNumberEdit.Text);
            PublishViewChanged();
            }



        }
    }

