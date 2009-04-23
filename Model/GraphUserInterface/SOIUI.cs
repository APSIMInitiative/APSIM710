
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
   public partial class SOIUI : BaseView
      {
      public SOIUI()
         {
         InitializeComponent();
         }

      public override void OnRefresh()
         {
         // -----------------------------------------------
         // Called when it's time to refresh
         // -----------------------------------------------
         base.OnRefresh();
         GroupBox.Text = Name;

         FileNameEdit.TextChanged -= OnFileNameChanged;
         MonthDropDown.TextChanged -= OnMonthChanged;
         PhaseList.ItemCheck -= OnItemCheck;

         FileNameEdit.Text = XmlHelper.Value(Data, "FileName");
         MonthDropDown.Text = XmlHelper.Value(Data, "Month");

         List<string> CheckedPhases = XmlHelper.Values(Data, "Phase");
         for (int i = 0; i != PhaseList.Items.Count; i++)
            {
            int Index = CheckedPhases.IndexOf(PhaseList.Items[i].ToString());
            PhaseList.SetItemChecked(i, Index != -1);
            }

         FileNameEdit.TextChanged += OnFileNameChanged;
         MonthDropDown.TextChanged += OnMonthChanged;
         PhaseList.ItemCheck += OnItemCheck;
         }

      private void OnFileNameChanged(object sender, EventArgs e)
         {
         XmlHelper.SetValue(Data, "FileName", FileNameEdit.Text);
         PublishViewChanged();
         }

      private void OnMonthChanged(object sender, EventArgs e)
         {
         XmlHelper.SetValue(Data, "Month", MonthDropDown.Text);
         PublishViewChanged();
         }

      private void OnItemCheck(object sender, ItemCheckEventArgs e)
         {
         List<string> CheckedPhases = new List<string>();
         for (int i = 0; i != PhaseList.Items.Count; i++)
            {
            bool IsChecked = false;
            if (e.Index == i)
               IsChecked = (e.NewValue == CheckState.Checked);
            else
               IsChecked = PhaseList.GetItemChecked(i);
            if (IsChecked)
               CheckedPhases.Add(PhaseList.Items[i].ToString());
            }
         XmlHelper.SetValues(Data, "Phase", CheckedPhases);
         PublishViewChanged();
         }

      }
   }

