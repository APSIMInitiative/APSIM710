using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Text;
using System.Windows.Forms;
using CSGeneral;
using System.Xml;

namespace Graph
   {
   public partial class PlotUI : Graph.DataUserInterface
      {
      List<string> EmptyList = new List<string>();
      Color HightlightColour = Color.Pink;
      bool AllXOnTopAxis = false;
      public PlotUI()
         {
         InitializeComponent();
         }

      public override void OnRefresh()
         {
         // -----------------------------------------------
         // Called when it's time to refresh the canvas and
         // everything on it.
         // -----------------------------------------------
         base.OnRefresh();

         DataGrid.BringToFront();

         // populate x and y edit boxes.
         X.Items.Clear();
         Y.Items.Clear();

         LoadListBox(X, "X", "");
         LoadListBox(X, "XTop", "Top axis: ");
         LoadListBox(Y, "Y", "");
         LoadListBox(Y, "YRight", "Right axis: ");

         // setup the type drop downs.
         TypeCombo.Text = XmlHelper.Value(Data, "SeriesType");
         PointCombo.Text = XmlHelper.Value(Data, "PointType");

         // see if all X variables are to be on top axis .e.g. Depth plots.
         AllXOnTopAxis = DataGrid.Columns.Count > 2 && 
                         DataGrid.Columns[0].Name == "Date" && 
                         DataGrid.Columns[1].Name == "Depth";
         
         base.OnColumnClickEvent -= OnColumnClick;
         base.OnColumnClickEvent += OnColumnClick;
         if (Y.Items.Count == 0 && X.Items.Count != 0)
            TransitionToY();
         else
            TransitionToX();

         string ColourString = XmlHelper.Value(Data, "colour");
         SpecificColourCheckBox.Checked = (ColourString != "");
         OnSpecificColourChange(null, null);
         }

      private void LoadListBox(ListBox List, string DataName, string Prefix)
         {
         List<string> ListValues = XmlHelper.Values(Data, DataName);
         string[] Values = new string[ListValues.Count];
         ListValues.CopyTo(Values, 0);
         if (Prefix != "")
            for (int i = 0; i != Values.Length; i++)
               Values[i] = Prefix + Values[i];
         List.Items.AddRange(Values);
         }

      private void OnColumnClick(string ColumnName)
         {
         if (X.BackColor == HightlightColour)
            SetX(ColumnName);
         else
            SetY(ColumnName);
         }

      private void TransitionToX()
         {
         base.HelpText = "Select X variables by clicking on the column(s) at the bottom.";
         X.BackColor = HightlightColour;
         Y.BackColor = SystemColors.Window;
         }

      private void TransitionToY()
         {
         base.HelpText = "Select Y variables by clicking on the column(s) at the bottom.";
         X.BackColor = SystemColors.Window;
         Y.BackColor = HightlightColour;
         }

      private void SetX(string ColumnName)
         {
         if (AllXOnTopAxis)
            ColumnName = "Top axis: " + ColumnName;
         X.Items.Add(ColumnName);
         SaveXValues();
         }
      private void SetY(string ColumnName)
         {
         Y.Items.Add(ColumnName);
         SaveYValues();
         }

      private void SaveXValues()
         {
         List<string> XValues = new List<string>();
         List<string> X2Values = new List<string>();
         for (int i = 0; i != X.Items.Count; i++)
            {
            string Value = X.Items[i].ToString();
            if (Value.Contains("Top axis: "))
               X2Values.Add(Value.Replace("Top axis: ", ""));
            else
               XValues.Add(Value);
            }
         XmlHelper.SetValues(Data, "X", XValues);
         XmlHelper.SetValues(Data, "XTop", X2Values);
         }
      private void SaveYValues()
         {
         List<string> YValues = new List<string>();
         List<string> Y2Values = new List<string>();
         for (int i = 0; i != Y.Items.Count; i++)
            {
            string Value = Y.Items[i].ToString();
            if (Value.Contains("Right axis: "))
               Y2Values.Add(Value.Replace("Right axis: ", ""));
            else
               YValues.Add(Value);
            }
         XmlHelper.SetValues(Data, "Y", YValues);
         XmlHelper.SetValues(Data, "YRight", Y2Values);
         }


      private void OnXClick(object sender, EventArgs e)
         {
         TransitionToX();
         }
      private void OnYClick(object sender, EventArgs e)
         {
         TransitionToY();
         }
      private void OnTypeChanged(object sender, EventArgs e)
         {
         XmlHelper.SetValue(Data, "SeriesType", TypeCombo.Text);
         PointCombo.Enabled = (TypeCombo.Text != "Bar");
         }
      private void OnPointChanged(object sender, EventArgs e)
         {
         XmlHelper.SetValue(Data, "PointType", PointCombo.Text);
         }

      private void OnTopAxisMenuClick(object sender, EventArgs e)
         {
         if (X.SelectedIndex != -1)
            {
            string Item = X.Items[X.SelectedIndex].ToString();
            if (Item.Contains("Top axis: "))
               Item = Item.Replace("Top axis: ", "");
            else
               Item = "Top axis: " + Item;
            X.Items[X.SelectedIndex] = Item;
            SaveXValues();
            }
         }

      private void OnDeleteXMenuClick(object sender, EventArgs e)
         {
         if (X.SelectedIndex != -1)
            {
            X.Items.RemoveAt(X.SelectedIndex);
            SaveXValues();
            }
         }

      private void OnDeleteAllXMenuClick(object sender, EventArgs e)
         {
         X.Items.Clear();
         SaveXValues();
         }

      private void OnRightAxisClick(object sender, EventArgs e)
         {
         if (Y.SelectedIndex != -1)
            {
            string Item = Y.Items[Y.SelectedIndex].ToString();
            if (Item.Contains("Right axis: "))
               Item = Item.Replace("Right axis: ", "");
            else
               Item = "Right axis: " + Item;
            Y.Items[Y.SelectedIndex] = Item;
            SaveYValues();
            }
         }
      private void OnCumulativeClick(object sender, EventArgs e)
         {
         if (Y.SelectedIndex != -1)
            {
            string CurrentItem = Y.Items[Y.SelectedIndex].ToString();
            if (CurrentItem.Contains("Cumulative "))
               Y.Items[Y.SelectedIndex] = CurrentItem.Replace("Cumulative ", "");
            else
               Y.Items[Y.SelectedIndex] = "Cumulative " + CurrentItem;
            SaveYValues();
            }
         }


      private void OnDeleteYMenuClick(object sender, EventArgs e)
         {
         if (Y.SelectedIndex != -1)
            {
            Y.Items.RemoveAt(Y.SelectedIndex);
            SaveYValues();
            }
         }

      private void OnDeleteAllYMenuClick(object sender, EventArgs e)
         {
         Y.Items.Clear();
         SaveYValues();
         }

      private void OnColourClick(object sender, EventArgs e)
         {
         XmlHelper.SetValue(Data, "colour", ColourButton.Color.ToArgb().ToString());
         }

      private void OnSpecificColourChange(object sender, EventArgs e)
         {
         ColourButton.Visible = SpecificColourCheckBox.Checked;
         if (SpecificColourCheckBox.Checked)
            XmlHelper.SetValue(Data, "colour", ColourButton.Color.ToArgb().ToString());
         else
            XmlHelper.SetValue(Data, "colour", "");
         }

      private void OnXKeyDown(object sender, KeyEventArgs e)
         {
         if (e.KeyCode == Keys.Delete)
            OnDeleteXMenuClick(null, null);
         }

      private void OnYKeyDown(object sender, KeyEventArgs e)
         {
         if (e.KeyCode == Keys.Delete)
            OnDeleteYMenuClick(null, null);
         }

      private void OnCumulativeXClick(object sender, EventArgs e)
         {
         string CurrentItem = X.Items[X.SelectedIndex].ToString();
         if (CurrentItem.Contains("Cumulative "))
            X.Items[X.SelectedIndex] = CurrentItem.Replace("Cumulative ", "");
         else
            X.Items[X.SelectedIndex] = "Cumulative " + CurrentItem;
         SaveXValues();
         }




      }
   }

