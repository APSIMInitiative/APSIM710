
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
   public partial class StatsUI : BaseView
      {
      private ChartPageUI ParentUI;

      public StatsUI()
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

         IncludeZeros.Checked = XmlHelper.Value(Data, "IncludeZeros") == "" || XmlHelper.Value(Data, "IncludeZeros") == "Yes";
         FieldList.ItemCheck -= OnItemCheck;
         StatsList.ItemCheck -= OnStatItemCheck;
         RollingMean.ValueChanged -= OnRollingMeanChanged;

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
         foreach (string Stat in XmlHelper.Values(Data, "Stat"))
            {
            int StatIndex = StatsList.Items.IndexOf(Stat);
            if (StatIndex == -1)
               Data.RemoveChild(XmlHelper.ChildByTypeAndValue(Data, "Stat", Stat));
            else
               StatsList.SetItemChecked(StatIndex, true);
            }
         if (XmlHelper.Value(Data, "RollingMean") != "")
            RollingMean.Value = Convert.ToInt32(XmlHelper.Value(Data, "RollingMean"));
         StatsList.Enabled = (RollingMean.Value == 0);

         FieldList.ItemCheck += OnItemCheck;
         StatsList.ItemCheck += OnStatItemCheck;
         RollingMean.ValueChanged += OnRollingMeanChanged;
         }


      private void OnItemCheck(object sender, ItemCheckEventArgs e)
         {
         if (e.NewValue == CheckState.Checked)
            {
            XmlNode NewField = XmlHelper.CreateNode(Data.OwnerDocument, "FieldName", "");
            XmlHelper.SetValue(NewField, "", FieldList.Items[e.Index].ToString());
            Data.AppendChild(NewField);
            }
         else
            Data.RemoveChild(XmlHelper.ChildByTypeAndValue(Data, "FieldName", FieldList.Items[e.Index].ToString()));
         PublishViewChanged();
         }

      private void OnStatItemCheck(object sender, ItemCheckEventArgs e)
         {
         if (e.NewValue == CheckState.Checked)
            {
            XmlNode NewStat = XmlHelper.CreateNode(Data.OwnerDocument, "Stat", "");
            XmlHelper.SetValue(NewStat, "", StatsList.Items[e.Index].ToString());
            Data.AppendChild(NewStat);
            }
         else
            Data.RemoveChild(XmlHelper.ChildByTypeAndValue(Data, "Stat", StatsList.Items[e.Index].ToString()));
         PublishViewChanged();
         }

      private void OnRollingMeanChanged(object sender, EventArgs e)
         {
         if (RollingMean.Value > 0)
            {
            // remove all stat nodes.
            foreach (XmlNode StatNode in XmlHelper.ChildNodes(Data, "stat"))
               Data.RemoveChild(StatNode);
            XmlHelper.SetValue(Data, "RollingMean", RollingMean.Value.ToString());
            StatsList.Enabled = true;
            PublishViewChanged();
            }
         else
            {
            XmlNode RollingMeanNode = XmlHelper.Find(Data, "RollingMean");
            if (RollingMeanNode != null)
               Data.RemoveChild(RollingMeanNode);
            }
         StatsList.Enabled = (RollingMean.Value == 0);
         }

      private void OnIncludeZeros(object sender, EventArgs e)
         {
         if (IncludeZeros.Checked)
            XmlHelper.SetValue(Data, "IncludeZeros", "Yes");
         else
            XmlHelper.SetValue(Data, "IncludeZeros", "No");
         PublishViewChanged();
         }

      }
   }

