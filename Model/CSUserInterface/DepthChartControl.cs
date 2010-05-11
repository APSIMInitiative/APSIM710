using System;
using System.Collections.Generic;
using System.Collections.Specialized;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Windows.Forms;

using ApsimFile;
using CSGeneral;
using System.Xml;
using Controllers;


namespace CSUserInterface
   {
   public class DepthChartControl : System.Windows.Forms.UserControl, SoilProfileView
      {
      private IContainer components;
      #region Visual Studio Stuff


      public DepthChartControl()
         {
         // This call is required by the Windows.Forms Form Designer.
         InitializeComponent();
         }
      protected override void Dispose(bool disposing)
         {
         if (disposing)
            {
            if (components != null)
               {
               components.Dispose();
               }
            }
         base.Dispose(disposing);
         }

      #region Component Designer generated code
      /// <summary> 
      /// Required method for Designer support - do not modify 
      /// the contents of this method with the code editor.
      /// </summary>
      private void InitializeComponent()
         {
         this.components = new System.ComponentModel.Container();
         System.ComponentModel.ComponentResourceManager resources = new System.ComponentModel.ComponentResourceManager(typeof(DepthChartControl));
         this.dataTable1 = new System.Data.DataTable();
         this.editor1 = new Steema.TeeChart.Editor(this.components);
         this.dataSet1 = new System.Data.DataSet();
         ((System.ComponentModel.ISupportInitialize)(this.dataTable1)).BeginInit();
         ((System.ComponentModel.ISupportInitialize)(this.dataSet1)).BeginInit();
         this.SuspendLayout();
         // 
         // dataTable1
         // 
         this.dataTable1.TableName = "Table1";
         // 
         // editor1
         // 
         this.editor1.HighLightTabs = false;
         this.editor1.Location = new System.Drawing.Point(0, 0);
         this.editor1.Name = "editor1";
         this.editor1.Options = null;
         this.editor1.TabIndex = 0;
         // 
         // dataSet1
         // 
         this.dataSet1.DataSetName = "NewDataSet";
         // 
         // DepthChartControl
         // 
         this.Name = "DepthChartControl";
         this.Size = new System.Drawing.Size(648, 600);
         ((System.ComponentModel.ISupportInitialize)(this.dataTable1)).EndInit();
         ((System.ComponentModel.ISupportInitialize)(this.dataSet1)).EndInit();
         this.ResumeLayout(false);

         }
      #endregion
      #endregion

      private SoilProfileEditor Editor;
      private double[] CumThicknessMidPoints;

      public DataTable Data;
      public DataSet DeanSet;

      BaseController Controller;
      private DataTable dataTable1;
      private Steema.TeeChart.Editor editor1;
      private DataSet dataSet1; 

      public delegate void OnWaterChangeDelegate(int LayerNumber, double NewValue);
      public event OnWaterChangeDelegate OnWaterChange;

      public void OnLoad(XmlNode Data, BaseController C)
         {
         // --------------------------------------------------------
         // Chart control have just been loaded.
         // --------------------------------------------------------
         Editor = new SoilProfileEditor(Data, this);
         Controller = C;
         }
      public void OnRefresh()
         {
         // --------------------------------------------------------
         // Our parent view want's to refresh us.
         // --------------------------------------------------------
         //Controller.MainForm.Controls.Add(DeanSet);
         Data = new DataTable("dd", "dd");
         DeanSet = new DataSet("deanset");
         Data.TableName = "deano";
         Data.Columns.Add("colDesc", typeof(System.String));
         Data.Columns.Add("colX", typeof(System.Double));
         Data.Columns.Add("colY", typeof(System.Double));
         Data.Columns.Add("colZ", typeof(System.Double));

         DeanSet.DataSetName = "asdf";
         DeanSet.Tables.Add(Data);
         DeanSet.Locale = new System.Globalization.CultureInfo("es-ES");
         PopulateTable();

         Editor.RefreshView();

         Chart.Series[0].DataSource = DeanSet;
         Chart.Series[0].XValues.DataMember = "colX";
         Chart.Series[0].YValues.DataMember = "colY";
         Chart.Series[0].CheckDataSource();
         }
      private void PopulateTable()
         {
         Random r = new Random(255);
         int numVals = 20;

         DataRow NewRow;
         for (int x = 0; x < numVals; x++)
            for (int z = 0; z < numVals; z++)
               {
               double tmpvar = r.Next(10);
               NewRow = Data.NewRow();
               NewRow["colDesc"] = "Text" + x.ToString() + z.ToString();
               NewRow["colX"] = x;
               NewRow["colY"] = 0.5 * Math.Pow(Math.Cos(x / (numVals * 0.2)), 2) +
                                       Math.Pow(Math.Cos(z / (numVals * 0.2)), 2) -
                                       Math.Cos(z / (numVals * 0.5));
               NewRow["colZ"] = z;
               Data.Rows.Add(NewRow);
               }
         }

      #region SoilProfileView Members

      public void SetColumn(int Col, string Name, string Units, string[] Values)
         {
         if (Name == "Depth")
            CumThicknessMidPoints = SoilComponentUtility.ToMidPoints(SoilComponentUtility.ToThickness(Values));
         else if (Values.Length == CumThicknessMidPoints.Length)
            {
            Steema.TeeChart.Styles.Line NewSeries = new Steema.TeeChart.Styles.Line();
            Chart.Series.Add(NewSeries);

            NewSeries.Title = Name;
            NewSeries.HorizAxis = Steema.TeeChart.Styles.HorizontalAxis.Top;
            NewSeries.YValues.Order = Steema.TeeChart.Styles.ValueListOrder.None;
            NewSeries.XValues.Order = Steema.TeeChart.Styles.ValueListOrder.None;
            for (int i = 0; i < Values.Length; i++)
               {
               if (Values[i] != "")
                  NewSeries.Add(Convert.ToDouble(Values[i]), CumThicknessMidPoints[i]);

               }
            }
         }

      #endregion


      // -----------------------------------------------
      // User has clicked on legend - save ticked crops.
      // -----------------------------------------------
      private void WaterChart_ClickLegend(object sender, MouseEventArgs e)
         {
         //AirDryLine.Active = true;
         //SatLine.Active = true;
         //DulLine.Active = true;
         //LL15Line.Active = true;
         //InitialWaterLine.Active = true;
         //List<string> TickedCrops = new List<string>();
         //for (int s = 5; s != Chart.Series.Count; s++)
         //   {
         //   if (Chart.Series[s].Active)
         //      TickedCrops.Add(MySoil.Crops[s - 5]);
         //   }
         //Configuration.Instance.SetSettings("CropsOnGraph", TickedCrops);
         }

      private void OnDragInitWater(Steema.TeeChart.Tools.DragPoint sender, int index)
         {
         // -----------------------------------------------
         // User is dragging a initwater point - send out
         // an event so that our parent form can subscribe
         // to the event and update their table.
         // -----------------------------------------------
         //Steema.TeeChart.Tools.DragPoint dp = (Steema.TeeChart.Tools.DragPoint)sender;
         //double NewValue = InitialWaterLine.XValues[index];
         //if (OnWaterChange != null)
         //   OnWaterChange.Invoke(index, NewValue);
         }

      private void Chart_MouseDoubleClick(object sender, MouseEventArgs e)
         {
         //Chart.ShowEditor();
         editor1.ShowModal();
         //Steema.TeeChart.Editors.SeriesEditor.ShowEditor(Chart.Series[0], Steema.TeeChart.Editors.ChartEditorTabs.SeriesDataSource);
         }





      }
   }
