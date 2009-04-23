
using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Text;
using System.Windows.Forms;

using Controllers;
using CSGeneral;


namespace CSUserInterface
   {
   public partial class MemoUI : BaseView
      {
      public MemoUI()
         {
         InitializeComponent();
         }



      protected override void OnLoad()
         {
         base.HelpText = "Add simulation notes in the box below.";
         }


      public override void OnSave()
         {
         //Encode rtf syntax to base64 and save out to xml file. Encoding to base64 overcomes 
         //issues with xml keyword syntax. 
         XmlHelper.SetValue(Data, "", TextBox.Text);
         }


      public override void OnRefresh()
         {
         //update the memo field with rich text syntax, after decoding the string from base64. 
         TextBox.Text = XmlHelper.Value(Data, "");
         }

      public override void PrintPage(Rectangle MarginBounds, Graphics g)
         {
         Rectangle R = new Rectangle(0, 0, MarginBounds.Width - MarginBounds.X, MarginBounds.Height - MarginBounds.Y);
         Bitmap b = new Bitmap(R.Width, R.Height);
         TextBox.Dock = DockStyle.None;
         TextBox.Width = R.Width;
         TextBox.Height = R.Height;
         TextBox.DrawToBitmap(b, R);
         TextBox.Dock = DockStyle.Fill;
         g.DrawImage(b, MarginBounds.Location);
         }


      }
   }
