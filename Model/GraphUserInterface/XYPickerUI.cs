
using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Text;
using System.Windows.Forms;
using System.Xml;

using FarPoint.Win.Spread.CellType;

using Controllers;
using CSGeneral;


namespace GraphUserInterface
    {
    public partial class XYPickerUI : BaseView
        {
        private bool InRefresh = false;
        private ChartPageUI ParentUI;

        public XYPickerUI()
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
            InRefresh = true;
            string DataSource = XmlHelper.Value(Data, "Source");
            if (DataSource != "")
                {
                string[] FieldNames = ParentUI.Processor.GetFieldNamesForDataSet(DataSource);

                // setup the x drop down
                XDropDown.Items.Clear();
                XDropDown.Items.AddRange(FieldNames);
                XDropDown.Text = XmlHelper.Value(Data, "X");


                // setup the Y lists.
                SetupYCombo(Y1, Y1Right, FieldNames, 0);
                SetupYCombo(Y2, Y2Right, FieldNames, 1);
                SetupYCombo(Y3, Y3Right, FieldNames, 2);
                SetupYCombo(Y4, Y4Right, FieldNames, 3);
                SetupYCombo(Y5, Y5Right, FieldNames, 4);

                // setup the radio buttons
                TypeCombo.Text = XmlHelper.Value(Data, "SeriesType");
                PointCombo.Text = XmlHelper.Value(Data, "PointType");
                }
            PointCombo.Enabled = (TypeCombo.Text != "Bar");
            InRefresh = false;
            }

        private void SetupYCombo(ComboBox YCombo, CheckBox YRight, 
                                 string[] FieldNames, int Index)
            {
            List<string> YFieldNames = XmlHelper.Values(Data, "Y");
            List<string> Y2FieldNames = XmlHelper.Values(Data, "YRight");

            YCombo.Items.Clear();
            YCombo.Items.AddRange(FieldNames);
            if (YFieldNames.Count > Index)
                {
                YCombo.Text = YFieldNames[Index];
                YRight.Checked = false;
                }
            else if (Y2FieldNames.Count > Index)
                {
                YCombo.Text = Y2FieldNames[Index];
                YRight.Checked = true;
                }

            }

        private void OnXChanged(object sender, EventArgs e)
            {
            if (!InRefresh)
                {
                XmlHelper.SetValue(Data, "X", XDropDown.Text);
                PublishViewChanged();
                }
            }
        private void OnTypeChanged(object sender, EventArgs e)
            {
            if (!InRefresh)
                {
                XmlHelper.SetValue(Data, "SeriesType", TypeCombo.Text);
                PointCombo.Enabled = (TypeCombo.Text != "Bar");
                PublishViewChanged();
                }
            }
        private void OnPointChanged(object sender, EventArgs e)
            {
            if (!InRefresh)
                {
                XmlHelper.SetValue(Data, "PointType", PointCombo.Text);
                PublishViewChanged();
                }
            }


        private void OnYChanged(object sender, EventArgs e)
            {
            if (!InRefresh)
                {
                List<string> Values = new List<string>();
                if (Y1.Text != "" && !Y1Right.Checked)
                    Values.Add(Y1.Text);
                if (Y2.Text != "" && !Y2Right.Checked)
                    Values.Add(Y2.Text);
                if (Y3.Text != "" && !Y3Right.Checked)
                    Values.Add(Y3.Text);
                if (Y4.Text != "" && !Y4Right.Checked)
                    Values.Add(Y4.Text);
                if (Y5.Text != "" && !Y5Right.Checked)
                    Values.Add(Y5.Text);
                XmlHelper.SetValues(Data, "Y", Values);

                Values.Clear();
                if (Y1.Text != "" && Y1Right.Checked)
                    Values.Add(Y1.Text);
                if (Y2.Text != "" && Y2Right.Checked)
                    Values.Add(Y2.Text);
                if (Y3.Text != "" && Y3Right.Checked)
                    Values.Add(Y3.Text);
                if (Y4.Text != "" && Y4Right.Checked)
                    Values.Add(Y4.Text);
                if (Y5.Text != "" && Y5Right.Checked)
                    Values.Add(Y5.Text);
                XmlHelper.SetValues(Data, "YRight", Values);
                PublishViewChanged();
                }
            }

        }
    }

