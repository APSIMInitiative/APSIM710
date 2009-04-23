

using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Drawing;
using System.Data;
using System.Text;
using System.Windows.Forms;


namespace UIBits
    {
        public partial class Line : System.Windows.Forms.UserControl
            {
            private Color lineColour = System.Drawing.Color.Black;
            private int lineThickness = 1;
            private int lineTransparent = 50;


            public Line()
                {
                InitializeComponent();
                }

            public Color LineColour
                {
                get { return lineColour; }
                set { lineColour = value; Invalidate(); }
                }

            public int LineThickness
                {
                get { return lineThickness; }
                set { lineThickness = value; Invalidate(); }
                }


            public int LineTransparent
                {
                get { return lineTransparent; }
                set { lineTransparent = value; Invalidate(); }
                }


            protected override void OnPaint(PaintEventArgs pe)
                {
                System.Drawing.Pen myPen = new System.Drawing.Pen(LineColour, LineThickness);
                System.Drawing.Graphics formGraphics;
                formGraphics = this.CreateGraphics();
                formGraphics.DrawLine(myPen, 0, 0, this.Location.X + this.Size.Width, 0);
                myPen.Dispose();
                formGraphics.Dispose();
                }
            }
        }
   