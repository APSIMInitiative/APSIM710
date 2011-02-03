using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Text;
using System.Windows.Forms;

namespace UIBits
   {
   public partial class DuplicateForm : Form
      {
      public DuplicateForm()
         {
         InitializeComponent();
         }

      public int NumDuplicates { get { return Convert.ToInt32(NumberControl.Value); } }
      public bool DoLinkDuplicates {get {return LinkedCheckBox.Checked;}}

      private void OkButtonClick(object sender, EventArgs e)
         {
         Close();
         }

      private void CancelButtonClick(object sender, EventArgs e)
         {
         Close();
         }
      }
   }
