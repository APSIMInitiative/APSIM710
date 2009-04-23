using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Text;
using System.Windows.Forms;
using ApsimFile;
using System.Diagnostics;
using System.IO;

namespace ApsimRun
   {
   public partial class SimulationRunnerReportForm : Form
      {
      private List<Detail> Simulations;
      public SimulationRunnerReportForm()
         {
         InitializeComponent();
         }

      public void Populate(List<Detail> Simulations)
         {
         this.Simulations = Simulations;

         ListViewGroup Group = null;
         foreach (Detail Simulation in Simulations)
            {
            if (Group == null || Group.Header != Simulation.FileName)
               {
               Group = new ListViewGroup(Simulation.FileName);
               SimulationList.Groups.Add(Group);
               }
            ListViewItem Item = new ListViewItem(Group);
            Item.Text = Simulation.Name;
            Item.UseItemStyleForSubItems = false;
            if (Simulation.IsCompleted)
               {
               Item.ImageIndex = 0;
               if (Simulation.HasErrors)
                  {
                  System.Windows.Forms.ListViewItem.ListViewSubItem Sub = Item.SubItems.Add("ERRORS");
                  Sub.ForeColor = System.Drawing.Color.Red;
                  Sub.Font = SystemFonts.CaptionFont;
                  Item.ImageIndex = 1;
                  }
               else
                  {
                  System.Windows.Forms.ListViewItem.ListViewSubItem Sub = Item.SubItems.Add("No errors");
                  Sub.ForeColor = System.Drawing.Color.Gray;
                  }
               if (Simulation.HasWarnings)
                  {
                  System.Windows.Forms.ListViewItem.ListViewSubItem Sub = Item.SubItems.Add("WARNINGS");
                  Sub.ForeColor = System.Drawing.Color.HotPink; // Such a lovely colour!
                  Sub.Font = SystemFonts.CaptionFont;
                  Item.ImageIndex = 2;
                  }
               else
                  {
                  System.Windows.Forms.ListViewItem.ListViewSubItem Sub = Item.SubItems.Add("No warnings");
                  Sub.ForeColor = System.Drawing.Color.Gray;
                  }
               }
            SimulationList.Items.Add(Item);
            }
         }

      private void OnDoubleClick(object sender, MouseEventArgs e)
         {
         ListViewItem ClickedItem = SimulationList.GetItemAt(e.X, e.Y);
         if (ClickedItem != null)
            {
            string SummaryFileName = Simulations[ClickedItem.Index].SummaryFileName;
            try
               {
               if (File.Exists(SummaryFileName))
                  Process.Start(SummaryFileName);
               else
                  MessageBox.Show("Cannot find summary file: " + SummaryFileName);
               }
            catch
               {
               Process.Start("notepad", SummaryFileName);
               }
            }
         }

      private void OnShown(object sender, EventArgs e)
         {
         if (SimulationList.Items.Count > 0)
            {
            Invalidate(true); // this is to get around a painting problem in Windows.
            }
         }



      }
   }