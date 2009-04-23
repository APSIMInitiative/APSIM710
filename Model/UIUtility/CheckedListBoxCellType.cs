
using System;
using System.Windows.Forms;
using System.Drawing;


//This is used in VBUserInterface project in GenericUI

namespace UIUtility
{

    public class CheckedListBoxCellType : FarPoint.Win.Spread.CellType.GeneralCellType
    {


        CheckedListBox list = new CheckedListBox();

        public CheckedListBoxCellType()
        {
            list.DrawMode = DrawMode.OwnerDrawFixed;
            list.ScrollAlwaysVisible = true;
            list.ItemHeight = 20;
            list.DrawItem += this.listDrawitem;
        }
        private void listDrawitem(object sender, DrawItemEventArgs e)
        {
            DrawListBoxItem((ListBox)sender, e);
        }
        public override System.Windows.Forms.Control GetEditorControl(FarPoint.Win.Spread.Appearance appearance, float zoomFactor)
        {
            return list;
        }
        public override void PaintCell(System.Drawing.Graphics g, System.Drawing.Rectangle r, FarPoint.Win.Spread.Appearance appearance, object value, bool isSelected, bool isLocked, float zoomFactor)
        {
            SolidBrush brushBackground = new SolidBrush(appearance.BackColor);
            g.FillRectangle(brushBackground, r);
            SolidBrush brushText = new SolidBrush(appearance.ForeColor);
            if ((value != null))
            {
                g.DrawString(value.ToString(), appearance.Font, brushText, r.X, r.Y);
            }
        }
        public void DrawListBoxItem(ListBox list, DrawItemEventArgs e)
        {
            Brush brshBackgroundBrush = default(Brush);
            Brush brshTextBrush = default(Brush);
            Font fntTextFont = default(Font);
            string strText = null;
            StringFormat objStringFormat = default(StringFormat);
            e.DrawBackground();
            strText = list.Items[e.Index].ToString();
            if ((e.State & DrawItemState.Selected) == DrawItemState.Selected)
            {
                brshBackgroundBrush = new SolidBrush(System.Drawing.SystemColors.Highlight);
                brshTextBrush = new SolidBrush(Color.White);
            }
            else
            {
                brshBackgroundBrush = new SolidBrush(Color.White);
                brshTextBrush = new SolidBrush(Color.Black);
            }
            fntTextFont = list.Font;
            e.Graphics.FillRectangle(brshBackgroundBrush, e.Bounds);
            objStringFormat = new StringFormat();
            objStringFormat.Alignment = StringAlignment.Near;
            objStringFormat.LineAlignment = StringAlignment.Center;
            e.Graphics.DrawString(strText, fntTextFont, brshTextBrush, new RectangleF(e.Bounds.X, e.Bounds.Y, e.Graphics.MeasureString(strText, fntTextFont).Width, e.Bounds.Height), objStringFormat);
            e.DrawFocusRectangle();
        }

        public string[] Items
        {
            get
            {
                string[] ReturnItems = new string[list.Items.Count];
                for (int i = 0; i <= list.Items.Count - 1; i++)
                {
                    ReturnItems[i] = (string)list.Items[i];
                }
                return ReturnItems;
            }
            set
            {
                string[] Values = value;    //passed in

                list.Items.Clear();
                foreach (string item in Values)
                {
                    list.Items.Add(item);
                }
            }
        }
        public override void SetEditorValue(object value)
        {
            if ((value != null))
            {
                char[] newline = new char[2];    //C# equivalent of vbCrLf
                newline[0] = '\r';
                newline[1] = '\n';
                string[] SelectedStrings = value.ToString().Split(newline);
                for (int i = 0; i <= list.Items.Count - 1; i++)
                {

                    bool IsSelected = Array.IndexOf(SelectedStrings, list.Items[i]) != -1;
                    list.SetItemChecked(i, IsSelected);
                }
            }
        }
        public override object GetEditorValue()
        {
            string St = "";
            foreach (object CheckedItem in list.CheckedItems)
            {
                if (!string.IsNullOrEmpty(St))
                {
                    char[] newline = new char[2];    //C# equivalent of vbCrLf
                    newline[0] = '\r';
                    newline[1] = '\n';
                    St += newline;
                }
                St += (string)CheckedItem;
            }
            return St;
        }
    }
}