
using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Drawing;
using System.Data;
using System.Text;
using System.Windows.Forms;


namespace UIBits
    {
    public partial class ColorBox : UserControl
        {
        private Color boxColor;
       

        public ColorBox()
            {
            boxColor = SystemColors.ActiveCaption;
            }

        public Color BoxColor
            {
            get {return boxColor;}
            set {boxColor=value;}
            }

        protected override void OnPaint(System.Windows.Forms.PaintEventArgs e)
            {
            Rectangle rect = new Rectangle(0, 0, this.Width, this.Height);
            // Fill with gradient 
            e.Graphics.FillRectangle(new SolidBrush(boxColor), rect);
            }
        }
    }
