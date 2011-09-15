using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Text;
using System.Windows.Forms;
using System.Xml;
using CSGeneral;
using Steema.TeeChart;
using Steema.TeeChart.Styles;
using System.Reflection;
using System.Runtime.InteropServices;

namespace Graph
   {
   public partial class PlotsForm : Form
      {
      [DllImport("user32.dll")]
      public static extern int PostMessage(
            IntPtr hWnd,      // handle to destination window
            uint Msg,       // message
            long wParam,  // first message parameter
            long lParam   // second message parameter
            );
      public const int WM_LBUTTONDOWN = 0x0201;
      public const int WM_LBUTTONUP = 0x0202;
         
      private GraphUI2 Chart;
      private bool UserChange;
      private ListBox CurrentListBox = null;
      private string SavedSeriesTitle = "";

      /// <summary>
      /// constructor
      /// </summary>
      public PlotsForm(GraphUI2 C)
         {
         InitializeComponent();

         UserChange = false;
         Chart = C;
         foreach (DataTable Table in C.DataSources)
            DataList.Items.Add(Table.TableName);
         CreateGalleryOnPage("Standard", Standard);
         CreateGalleryOnPage("Extended", Extended);
         CreateGalleryOnPage("Financial", Financial);
         CreateGalleryOnPage("Other", Other);

         // Select the first data item.
         if (DataList.Items.Count > 0)
            {
            DataList.SelectedItem = DataList.Items[0];
            FillDataGrid();
            }

         // Make the X list box the current listbox.
         CurrentListBox = X;
         CurrentListBox.BackColor = SystemColors.Highlight;
         CurrentListBox.ForeColor = SystemColors.HighlightText;
         UserChange = true;

         OnGalleryChartChange(null, null);
         }


      /// <summary>
      /// Populate all controls from the specified series.
      /// </summary>
      public void SetSeries(Series S)
         {
         if (S != null)
            {
            SavedSeriesTitle = S.Title;

            XmlDocument Doc = new XmlDocument();
            Doc.LoadXml(S.ValuesLists[0].DataMember);
            XmlNode PlotNode = Doc.DocumentElement;

            string PageName = XmlHelper.Value(PlotNode, "GalleryPageName");
            int SeriesIndex = 0;
            int SubIndex = 0;
            if (PageName != "")
               {
               SeriesIndex = Convert.ToInt32(XmlHelper.Value(PlotNode, "GallerySeriesIndex"));
               SubIndex = Convert.ToInt32(XmlHelper.Value(PlotNode, "GallerySubIndex"));
               }
            else
               PageName = "Standard";

            // Select the appropriate tab page.
            tabControl1.SelectTab(PageName);
            GalleryPanel Gallery = (GalleryPanel)tabControl1.SelectedTab.Controls[0];

            // Tell the gallery to select a chart.
            Gallery.Charts[SeriesIndex].Select();

            X.Items.Clear();
            X.Items.Add(XmlHelper.Value(PlotNode, "X"));

            string Y = XmlHelper.Value(PlotNode, "Y");
            if (Y != "")
               Y1.Items.Add(Y);

            Y = XmlHelper.Value(PlotNode, "Y2");
            if (Y != "")
               Y2.Items.Add(Y);

            Y = XmlHelper.Value(PlotNode, "Y3");
            if (Y != "")
               Y3.Items.Add(Y);

            Y = XmlHelper.Value(PlotNode, "Y4");
            if (Y != "")
               Y4.Items.Add(Y);
            DataList.Text = XmlHelper.Value(PlotNode, "DataSource");
            }
         }

      /// <summary>
      /// Return one or more series - fully configured, ready to go.
      /// </summary>
      public List<Series> GetSeries()
         {
         List<Series> ReturnSeries = new List<Series>();

         GalleryPanel Gallery = (GalleryPanel)tabControl1.SelectedTab.Controls[0];

         int i = 0;
         for (i = 0; i != Gallery.Charts.Count; i++)
            {
            if (Gallery.Charts[i] == Gallery.SelectedChart)
               break;
            }

         if (X.Items.Count == 1 && Y1.Items.Count > 0)
            {
            // XYYYY
            foreach (string Y in Y1.Items)
               {
               Series S = Gallery.SelectedChart.Series[0];
               if (Y != Y1.Items[0].ToString())
                  S = (Series) Gallery.SelectedChart.Series[0].Clone();
               S.Title = Y;
               
               XmlDocument Doc = new XmlDocument();
               XmlNode PlotNode = Doc.AppendChild(Doc.CreateElement("XY"));
               XmlHelper.SetValue(PlotNode, "GalleryPageName", tabControl1.SelectedTab.Text);
               XmlHelper.SetValue(PlotNode, "GallerySeriesIndex", i.ToString());
               XmlHelper.SetValue(PlotNode, "GallerySubIndex", Gallery.SubIndex.ToString());
               XmlHelper.SetValue(PlotNode, "DataSource", DataList.Text);
               XmlHelper.SetValue(PlotNode, "X", X.Items[0].ToString());
               XmlHelper.SetValue(PlotNode, "Y", Y);
               if (Y2.Items.Count > 0)
                  XmlHelper.SetValue(PlotNode, "Y2", Y2.Items[0].ToString());
               if (Y3.Items.Count > 0)
                  XmlHelper.SetValue(PlotNode, "Y3", Y3.Items[0].ToString());
               if (Y4.Items.Count > 0)
                  XmlHelper.SetValue(PlotNode, "Y4", Y4.Items[0].ToString());
               S.ValuesLists[0].DataMember = PlotNode.OuterXml;

               ReturnSeries.Add(S);
               }
            }
         else if (X.Items.Count > 1 && Y1.Items.Count == 1)
            {
            // XXXXY
            foreach (string XName in X.Items)
               {
               Series S = Gallery.SelectedChart.Series[0];
               if (XName != X.Items[0].ToString())
                  S = (Series) Gallery.SelectedChart.Series[0].Clone();

               S.Title = XName;
               XmlDocument Doc = new XmlDocument();
               XmlNode PlotNode = Doc.AppendChild(Doc.CreateElement("XY"));
               XmlHelper.SetValue(PlotNode, "GalleryPageName", tabControl1.SelectedTab.Text);
               XmlHelper.SetValue(PlotNode, "GallerySeriesIndex", i.ToString());
               XmlHelper.SetValue(PlotNode, "GallerySubIndex", Gallery.SubIndex.ToString());
               XmlHelper.SetValue(PlotNode, "DataSource", DataList.Text);
               XmlHelper.SetValue(PlotNode, "X", XName);
               XmlHelper.SetValue(PlotNode, "Y", Y1.Items[0].ToString());
               if (Y2.Items.Count > 0)
                  XmlHelper.SetValue(PlotNode, "Y2", Y2.Items[0].ToString());
               if (Y3.Items.Count > 0)
                  XmlHelper.SetValue(PlotNode, "Y3", Y3.Items[0].ToString());
               if (Y4.Items.Count > 0)
                  XmlHelper.SetValue(PlotNode, "Y4", Y4.Items[0].ToString());
               S.ValuesLists[0].DataMember = PlotNode.OuterXml;

               ReturnSeries.Add(S);
               }
            }
         if (ReturnSeries.Count == 1 && SavedSeriesTitle != "")
            ReturnSeries[0].Title = SavedSeriesTitle;

         return ReturnSeries;
         }

      /// <summary>
      /// Create a gallery page on the specified tab
      /// </summary>
      private void CreateGalleryOnPage(string PageName, TabPage Tab)
         {
         GalleryPanel Gallery = new GalleryPanel();
         Gallery.CreateGalleryPage(PageName);
         Gallery.Parent = Tab;
         Gallery.NumCols = 6;
         Gallery.NumRows = 2;
         Gallery.Dock = DockStyle.Fill;
         Gallery.View3D = false;
         Tab.AutoScroll = true;
         Gallery.OnChangeChart += new EventHandler(OnGalleryChartChange);
         }

      /// <summary>
      /// Show or hide a particular label / listbox depending on the series passed in.
      /// </summary>
      private void ShowHideListBox(string ControlName, Steema.TeeChart.Styles.Series S, int i)
         {
         Label L = (Label) FindControl(DetailsPanel, ControlName + "Label"); 
         ListBox List = (ListBox) FindControl(DetailsPanel, ControlName);
         if (i >= S.ValuesLists.Count)
            {
            L.Visible = false;
            List.Visible = false;
            }
         else
            {
            L.Visible = true;
            List.Visible = true;
            L.Text = S.ValuesLists[i].Name;
            }
         }

      /// <summary>
      /// Static function to find a particular control on a parent control.
      /// </summary>
      private static Control FindControl(Control Parent, string ControlName)
         {
         // Utility function to recursively go find a control.
         foreach (Control C in Parent.Controls)
            {
            if (C.Name == ControlName)
               return C;
            Control FoundControl = FindControl(C, ControlName);
            if (FoundControl != null)
               return FoundControl;
            }
         return null;
         }

      /// <summary>
      /// User has clicked on an XY list. This will focus it so we need to update the colours. 
      /// </summary>
      private void OnXYListClick(object sender, EventArgs e)
         {
         ListBox List = (ListBox)sender;
         CurrentListBox.BackColor = SystemColors.Window;
         CurrentListBox.ForeColor = SystemColors.WindowText;
         CurrentListBox = List;
         CurrentListBox.BackColor = SystemColors.Highlight;
         CurrentListBox.ForeColor = SystemColors.HighlightText;
         
         }




      /// <summary>
      /// User has changed the selected series in the gallery - update the details box.
      /// </summary>
      void OnGalleryChartChange(object sender, EventArgs e)
         {
         if (UserChange)
            {
            GalleryPanel Gallery = (GalleryPanel)tabControl1.SelectedTab.Controls[0];
            Steema.TeeChart.Styles.Series S = Gallery.SelectedChart[0];

            ShowHideListBox("X", S, 0);
            ShowHideListBox("Y1", S, 1);
            ShowHideListBox("Y2", S, 2);
            ShowHideListBox("Y3", S, 3);
            ShowHideListBox("Y4", S, 4);

            if (S.ValuesLists.Count > 5)
               MessageBox.Show("Too many y variables for this type of plot");
            }
         }


      /// <summary>
      /// The currently selected datasource has changed. Populate the datagrid.
      /// </summary>
      private void OnDataListChanged(object sender, EventArgs e)
         {
         if (UserChange)
            FillDataGrid();
         }

      /// <summary>
      /// Fill the datagrid with data.
      /// </summary>
      private void FillDataGrid()
         {
         if (DataList.Text != "")
            {
            DataGrid.DataSource = Chart.GetDataSourceWithName(DataList.Text);

            DataGrid.BringToFront();
            foreach (DataGridViewColumn Col in DataGrid.Columns)
               Col.SortMode = DataGridViewColumnSortMode.NotSortable;
            }
         }


      /// <summary>
      /// User has clicked on the datagrid. If they have clicked on a header, then
      /// add that variable to the appropriate XY list.
      /// </summary>
      private void OnDataGridCellMouseClick(object sender, DataGridViewCellMouseEventArgs e)
         {
         if (e.ColumnIndex >= 0 && e.RowIndex == -1 && e.Button == MouseButtons.Left && DataGrid.Columns.Count > 0)
            {
            CurrentListBox.Items.Add(DataGrid.Columns[e.ColumnIndex].HeaderText);
            if (CurrentListBox == X)
               OnXYListClick(Y1, null);
            }
         }

      /// <summary>
      /// Retrieve the items from the specified list box.
      /// </summary>
      private List<string> GetValuesFromList(ListBox List)
         {
         List<string> Values = new List<string>();
         foreach (string Item in List.Items)
            Values.Add(Item);
         return Values;
         }

      /// <summary>
      /// Set the items in the specified list box.
      /// </summary>
      private void SetValuesFromList(ListBox List, List<string> Items)
         {
         List.Items.Clear();
         foreach (string Item in Items)
            List.Items.Add(Item);
         }

      private void OnOkClick(object sender, EventArgs e)
         {
         Close();
         }

      private void OnCancelClick(object sender, EventArgs e)
         {
         Close();
         }

      private void OnDeleteVariable(object sender, EventArgs e)
         {
         if (CurrentListBox.SelectedIndex != -1)
            CurrentListBox.Items.RemoveAt(CurrentListBox.SelectedIndex);
         }

      }
   }
