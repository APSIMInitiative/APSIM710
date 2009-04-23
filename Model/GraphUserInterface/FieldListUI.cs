
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
    public partial class FieldListUI : BaseView
        {
        private ChartPageUI ParentUI;

        public FieldListUI()
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
            FieldList.ItemCheck -= OnItemCheck;
            FieldList.Items.Clear();
            FieldList.Items.AddRange(ParentUI.Processor.GetFieldNamesForDataSet(XmlHelper.Value(Data, "source")));
            if (FieldList.Items.Count > 0)
                {
                foreach (string FieldName in XmlHelper.Values(Data, "FieldName"))
                    {
                    int FieldIndex = FieldList.Items.IndexOf(FieldName);
                    if (FieldIndex == -1)
                        Data.RemoveChild(XmlHelper.ChildByTypeAndValue(Data, "FieldName", FieldName));
                    else
                        FieldList.SetItemChecked(FieldIndex, true);
                    }
                }

            FieldList.ItemCheck += OnItemCheck;
            }
        private void OnItemCheck(object sender, ItemCheckEventArgs e)
            {
            if (e.NewValue == CheckState.Checked)
                {
                XmlNode NewField = XmlHelper.CreateNode(Data.OwnerDocument, "FieldName", "");
                NewField.InnerText = FieldList.Items[e.Index].ToString();
                Data.AppendChild(NewField);
                }
            else
                Data.RemoveChild(XmlHelper.ChildByTypeAndValue(Data, "FieldName", FieldList.Items[e.Index].ToString()));
            PublishViewChanged();
            }

        }
    }

