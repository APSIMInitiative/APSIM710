

using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Drawing;
using System.Data;
using System.Text;
using System.Windows.Forms;


namespace UIBits
    {
    public partial class TextHeader : System.Windows.Forms.UserControl
        {
        private Color left;
        private Color right;
        private int textAlignmentX;
        private String text;

        public TextHeader()
            {
            left = SystemColors.ActiveCaption;
            right = SystemColors.Control;
            text = "TextHeader";
            }
           
        public Color LeftColor
            {
            get {return left;}
            set {left = value;        }
            }
           
        public Color RightColor
            {
                get { return right; }
                set { right = value;}
            }

        public int TextAlignmentX
            {
            get { return textAlignmentX; }
            set { textAlignmentX = value; }
            }

        public String HeaderText
            {
            get { return text; }
            set { text = value; }
            }

          protected override void OnPaint(System.Windows.Forms.PaintEventArgs e)
          {
            System.Drawing.Drawing2D.LinearGradientBrush GBrush = new System.Drawing.Drawing2D.LinearGradientBrush(
                new Point(0, 0),
                new Point(this.Width, 0), left, right);
            Rectangle rect = new Rectangle(0,0,this.Width,this.Height);
             // Fill with gradient 
            e.Graphics.FillRectangle(GBrush, rect);
            SolidBrush drawBrush = new SolidBrush(this.ForeColor);
             // output string
            e.Graphics.DrawString(text, this.Font, drawBrush, textAlignmentX, this.Height / 2 - Font.Height / 2);
          }
        }
    }
