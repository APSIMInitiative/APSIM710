using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Text;
using System.Windows.Forms;
using Steema.TeeChart;
using Steema.TeeChart.Styles;
using CSGeneral;

namespace Graph
   {
   public partial class AddSeriesForm : Form
      {
      private TChart Chart;
      private Series Series;
      private DataTable Data;
      private GraphUI2 ParentGraph;
      private Steema.TeeChart.Editors.DatabaseEditor DataSourceEditor;
      private Steema.TeeChart.GalleryPanel GalleryPanel;

      public AddSeriesForm(TChart c, DataTable D, GraphUI2 parentGraph)
         {
         // Constructor
         InitializeComponent();
         Chart = c;
         Data = D;
         ParentGraph = parentGraph;
         Series = c.Series.Add(new Line());
         Series.DataSource = Data;
         }
      private void OnLoad(object sender, EventArgs e)
         {
         // Form has been loaded - populate ourselves.
         CreateGalleryPanel(StandardTab, "Standard");
         CreateGalleryPanel(ExtendedTab, "Extended");
         CreateGalleryPanel(FinancialTab, "Financial");
         CreateGalleryPanel(OtherTab, "Other");
         CreateGalleryPanel(tabPage3, "3D");
         CreateGalleryPanel(StatsTab, "Stats");
         CreateDatabaseEditor();
         }
      private void CreateGalleryPanel(TabPage Tab, string PageName)
         {
         // Create a TeeChart gallery on the specified tab.
         GalleryPanel = new Steema.TeeChart.GalleryPanel();
         GalleryPanel.Width = Width;
         GalleryPanel.Height = Height / 2;
         GalleryPanel.CreateGalleryPage(PageName);
         GalleryPanel.Visible = true;
         GalleryPanel.Parent = Tab;
         GalleryPanel.Dock = DockStyle.Fill;
         GalleryPanel.View3D = false;

         GalleryPanel.OnChangeChart += new System.EventHandler(this.OnGallerySelectionChanged);
         }
      private void CreateDatabaseEditor()
         {
         // Create a new TeeChart DataSource form.
         DataSourceEditor = new Steema.TeeChart.Editors.DatabaseEditor(Series);
         DataSourceEditor.TopLevel = false;
         DataSourceEditor.Parent = DataSourcePanel;
         DataSourceEditor.Dock = DockStyle.Fill;
         DataSourceEditor.ControlBox = false;
         DataSourceEditor.BringToFront();
         DataSourceEditor.FormBorderStyle = FormBorderStyle.None;
         DataSourceEditor.Visible = true;
         }
      private void ApplyDataSource()
         {
         // We're about to close the TeeChart DataSource form so we need
         // to click apply for the user in case they forgot.
         if (DataSourceEditor != null)
            {
            // Set the series title to something appropriate.
            string SeriesTitle = "";
            foreach (Control C in DataSourceEditor.Controls)
               {
               if (C is ComboBox)
                  {
                  ComboBox Combo = (ComboBox)C;
                  if (Combo.Text != "")
                     {
                     SeriesTitle = Combo.Text;
                     }
                  }
               }

            //ParentGraph.AddSeries(Series, SeriesTitle, 
            //                      BottomRadio.Checked, LeftRadio.Checked, 
            //                      CumulativeCheckBox.Checked);
            }
         }

      public static Control FindControl(Control Parent, string ControlName)
         {
         // Utility function to recursively go find a control.
         foreach (Control C in Parent.Controls)
            {
            if (C.Text == ControlName)
               return C;
            Control FoundControl = FindControl(C, ControlName);
            if (FoundControl != null)
               return FoundControl;
            }
         return null;
         }



      #region Event Handlers
      private void OnGallerySelectionChanged(object sender, System.EventArgs e)
         {
         // User has clicked on a gallery picture. Go Change the series to match
         // the selection.
         GalleryPanel Gallery = (GalleryPanel)sender;
         Steema.TeeChart.Styles.Series.ChangeType(ref Series, Gallery.SelectedChart[0].GetType());
         CreateDatabaseEditor();
         }
      private void OnAddAnother(object sender, EventArgs e)
         {
         ApplyDataSource();
         Series = Chart.Series.Add(new Line());
         Series.DataSource = Data;
         CreateDatabaseEditor();
         }
      private void OnCancel(object sender, EventArgs e)
         {
         Chart.Series.Remove(Series);
         Close();
         }
      private void OnClosing(object sender, FormClosingEventArgs e)
         {
         if (DialogResult == DialogResult.OK)
            {
            ApplyDataSource();

            // Remove all series that have a blank title.
            for (int i = Chart.Series.Count - 1; i >= 0; i--)
               {
               if (Chart.Series[i].Title == "")
                  Chart.Series.RemoveAt(i);
               }
            }
         }
      #endregion


      }
   }
