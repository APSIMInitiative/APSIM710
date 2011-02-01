using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Text;
using System.Windows.Forms;
using System.Xml;
using CSGeneral;
using System.Drawing.Printing;
using System.IO;

namespace Graph
   {
   public partial class GraphReportUI : Controllers.BaseView
      {
      private XmlNode AllData;

      public GraphReportUI()
         {
         InitializeComponent();
         }

      public override void OnRefresh()
         {
         base.OnRefresh();

         XmlDocument Doc = new XmlDocument();
         Doc.LoadXml(Controller.Selection.FullXML());
         AllData = Doc.DocumentElement;

         Panel.Controls.Clear();
         foreach (XmlNode GraphNode in XmlHelper.ChildNodes(AllData, ""))
            {
            string GraphNodePath = NodePath + "/" + XmlHelper.Name(GraphNode);
            ApsimFile.Component Comp = Controller.ApsimData.Find(GraphNodePath);

            if (GraphNode.Name == "Graph")
               {
               GraphUI Graph = new GraphUI();
               Graph.Parent = this;
               Graph.OnLoad(Controller, GraphNodePath, Comp.Contents);
               }
            if (GraphNode.Name == "Graph2")
               {
               GraphUI2 Graph = new GraphUI2();
               Graph.Parent = Panel;
               Graph.OnLoad(Controller, GraphNodePath, Comp.Contents);
               }
            if (GraphNode.Name == "RegressionGraph")
               {
               RegressionGraphUI Graph = new RegressionGraphUI();
               Graph.Parent = Panel;
               Graph.OnLoad(Controller, GraphNodePath, Comp.Contents);
               }
            }

         PositionAndRefreshGraphs();

         }

      public override void OnSave()
         {
         base.OnSave();
         foreach (Control C in Panel.Controls)
            {
            if (C is Controllers.BaseView)
               {
               Controllers.BaseView View = (Controllers.BaseView)C;
               View.OnSave();
               ApsimFile.Component Comp = Controller.ApsimData.Find(View.NodePath);
               if (Comp != null)
                  Comp.Contents = View.GetData();
               }
            }
         }

      public void RefreshSeries()
         {
         foreach (Control C in Panel.Controls)
            {
            if (C is Controllers.BaseView)
               {
               Controllers.BaseView View = (Controllers.BaseView)C;
               View.OnRefresh();
               }
            }
         }

      private void PositionAndRefreshGraphs()
         {
         this.Resize -= OnResize;
         int NumGraphs = Panel.Controls.Count;
         if (NumGraphs > 0)
            {
            
            int NumRows;
            if (NumGraphs == 1)
               NumRows = 1;
            else
               NumRows = (int)Math.Sqrt(NumGraphs) + 1;
            int NumCols = (int)Math.Ceiling((double)NumGraphs / NumRows);
            int Width = Panel.Size.Width / NumCols;
            int Height = Panel.Size.Height / NumRows - 1;
            int GraphNumber = 0;
            int Col = 0;
            int Row = 0;
            foreach (Control C in Panel.Controls)
               {
               if (C is Controllers.BaseView)
                  {
                  C.Location = new Point(Col * Width, Row * Height);
                  C.Width = Width;
                  C.Height = Height;
                  GraphNumber++;
                  Col++;
                  if (Col >= NumCols)
                     {
                     Col = 0;
                     Row++;
                     }
                  Controllers.BaseView Graph = (Controllers.BaseView)C;
                  Graph.OnRefresh();
                  }

               }
            }
         this.Resize += OnResize;
         }

      private void OnResize(object sender, EventArgs e)
         {
         PositionAndRefreshGraphs();
         }

      public override void PrintPage(Rectangle MarginBounds, Graphics g)
         {
         foreach (Control C in Panel.Controls)
            {
            double XProportion = C.Location.X * 1.0 / Panel.Size.Width;
            double YProportion = C.Location.Y * 1.0 / Panel.Size.Height;
            double WidthProportion = C.Size.Width * 1.0 / Panel.Size.Width;
            double HeightProportion = C.Size.Height * 1.0 / Panel.Size.Height;
            int W = (int)(MarginBounds.Width * WidthProportion);
            int H = (int)(MarginBounds.Height * HeightProportion);
            Rectangle DrawRectangle = new Rectangle(0, 0, W, H);

            Bitmap b = new Bitmap(W, H);

            if (C is GraphUI)
               {
               GraphUI Graph = (GraphUI)C;
               Graph.Chart.DrawToBitmap(b, DrawRectangle);
               }
            if (C is GraphUI2)
               {
               GraphUI2 Graph = (GraphUI2)C;
               Graph.Chart.DrawToBitmap(b, DrawRectangle);
               }

            Point P = new Point((int)(MarginBounds.Width * XProportion),
                                (int)(MarginBounds.Height * YProportion));
            g.DrawImage(b, P);
            }
         }


      }
   }

