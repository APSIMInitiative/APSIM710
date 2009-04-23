using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Text;
using System.Windows.Forms;
using System.Drawing.Printing;

namespace UIBits
   {
   public partial class PrintPreviewDialog : Form
      {
      private PrintDocument Doc;
      private int NumPages;
      public PrintPreviewDialog()
         {
         InitializeComponent();
         }

      public PrintDocument Document
         {
         get { return Doc; }
         set {Doc = value;}
         }
      private void OnLoad(object sender, EventArgs e)
         {
         PreviewControl.Document = Doc;
         PreviewControl.Document.PrintPage += new PrintPageEventHandler(OnPrintPage);
         PreviewControl.Document.EndPrint += new PrintEventHandler(OnEndPrint);
         NumPages = 0;
         }

      void OnPrintPage(object sender, PrintPageEventArgs e)
         {
         NumPages++;
         }

      void OnEndPrint(object sender, PrintEventArgs e)
         {
         PageNumberUpDown.Maximum = NumPages;
         }

      private void OnPrint(object sender, EventArgs e)
         {
         PrintDialog.Document = Doc;
         PrintDialog.PrinterSettings = Document.PrinterSettings;
         if (PrintDialog.ShowDialog() == DialogResult.OK)
            {
            NumPages = 0;
            Doc.Print();
            }
         }

      private void OnPageSetup(object sender, EventArgs e)
         {
         PageSetupDialog.Document = Document;
         PageSetupDialog.PrinterSettings = Document.PrinterSettings;
         PageSetupDialog.ShowDialog();
         Document.PrinterSettings = PageSetupDialog.PrinterSettings;
         NumPages = 0;
         PreviewControl.InvalidatePreview();
         }

      private void OnPageChanged(object sender, EventArgs e)
         {
         if (PageNumberUpDown.Value > 0 && PageNumberUpDown.Value - 1 != PreviewControl.StartPage)
            PreviewControl.StartPage = (int) PageNumberUpDown.Value-1;
         }
      }
   }