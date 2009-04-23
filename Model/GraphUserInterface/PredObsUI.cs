
using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Text;
using System.Windows.Forms;
using System.Xml;

using Controllers;
using CSGeneral;


namespace GraphUserInterface
    {
    public partial class PredObsUI : BaseView
        {
        private bool InRefresh = false;
        private ChartPageUI ParentUI;

        public PredObsUI()
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

            InRefresh = true;
            GroupBox.Text = Name;

            string DataSource = XmlHelper.Value(Data, "Source");
            string[] FieldNames = ParentUI.Processor.GetFieldNamesForDataSet(DataSource);

            // setup the Field list.
            FieldList.Items.Clear();
            FieldList.Items.AddRange(FieldNames);
            foreach (string FieldName in XmlHelper.Values(Data, "FieldName"))
                {
                int PosItem = FieldList.Items.IndexOf(FieldName);
                if (PosItem != -1)
                    FieldList.SetItemChecked(PosItem, true);
                else
                    Data.RemoveChild(XmlHelper.ChildByTypeAndValue(Data, "FieldName", FieldName));
                }
            InRefresh = false; 
            }

        private void OnFieldListChanged(object sender, ItemCheckEventArgs e)
            {
            if (!InRefresh)
                {
                string FieldName = FieldList.Items[e.Index].ToString();
                if (e.NewValue == CheckState.Checked)
                    {
                    XmlNode NewField = XmlHelper.CreateNode(Data.OwnerDocument, "FieldName", "");
                    NewField.InnerText = FieldName;
                    Data.AppendChild(NewField);
                    }
                else
                    Data.RemoveChild(XmlHelper.ChildByTypeAndValue(Data, "FieldName", FieldName));
                PublishViewChanged();
                }
            }

        }
    }

