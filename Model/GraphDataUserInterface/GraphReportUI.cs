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

namespace GraphDataUserInterface
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

         Controls.Clear();
         foreach (XmlNode GraphNode in XmlHelper.ChildNodes(AllData, "Graph"))
            {
            GraphUI Graph = new GraphUI();
            Graph.OnLoad(Controller, NodePath + "/" + XmlHelper.Name(GraphNode), GraphNode.OuterXml);
            Graph.OnRefresh();
            Graph.Parent = this;
            }

         PositionGraphs();

         }

      private void PositionGraphs()
         {
         this.Resize -= OnResize;
         int NumGraphs = XmlHelper.ChildNodes(AllData, "Graph").Count;
         if (NumGraphs > 0)
            {

            int NumRows = (int)Math.Sqrt(NumGraphs) + 1;
            int NumCols = (int)Math.Ceiling((double)NumGraphs / NumRows);
            int Width = Size.Width / NumCols;
            int Height = Size.Height / NumRows - 1;
            int GraphNumber = 0;
            int Col = 0;
            int Row = 0;
            foreach (Control C in Controls)
               {
               if (C is GraphUI)
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
                  }

               }
            }
         this.Resize += OnResize;
         }

      private void OnResize(object sender, EventArgs e)
         {
         PositionGraphs();
         }

      public override void PrintPage(Rectangle MarginBounds, Graphics g)
         {
         foreach (Control C in Controls)
            {
            if (C is GraphUI)
               {
               GraphUI Graph = (GraphUI)C;

               double XProportion = C.Location.X * 1.0 / Size.Width;
               double YProportion = C.Location.Y * 1.0 / Size.Height;
               double WidthProportion = C.Size.Width * 1.0 / Size.Width;
               double HeightProportion = C.Size.Height * 1.0 / Size.Height;
               int W = (int)(MarginBounds.Width * WidthProportion);
               int H = (int)(MarginBounds.Height * HeightProportion);
               Rectangle DrawRectangle = new Rectangle(0, 0, W, H);

               Bitmap b = new Bitmap(W, H);
               Graph.Chart.DrawToBitmap(b, DrawRectangle);


               Point P = new Point((int)(MarginBounds.Width * XProportion),
                                   (int)(MarginBounds.Height * YProportion));
               g.DrawImage(b, P);
               }
            }
         }


      }
   }

