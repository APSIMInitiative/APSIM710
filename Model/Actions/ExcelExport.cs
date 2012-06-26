using Microsoft.VisualBasic;
using System;
using System.Collections;
using System.Collections.Generic;
using System.Data;
using System.Diagnostics;
namespace Actions
{
    public partial class ExcelExport
    {

        public string fileList;
        private void Button1_Click(System.Object sender, System.EventArgs e)
        {
            for (int i = 0; i <= fileListBox.Items.Count - 1; i++)
            {
                fileListBox.SetItemChecked(i, true);
            }
        }

        private void bDeselectAll_Click(System.Object sender, System.EventArgs e)
        {
            for (int i = 0; i <= fileListBox.Items.Count - 1; i++)
            {
                fileListBox.SetItemChecked(i, false);
            }
        }

        private void bOK_Click(System.Object sender, System.EventArgs e)
        {
            string[] allFiles = new string[fileListBox.Items.Count + 1];
            fileList = "";
            fileListBox.Items.CopyTo(allFiles, 0);

            for (int i = 0; i <= fileListBox.Items.Count - 1; i++)
            {
                if (fileListBox.GetItemChecked(i))
                {
                    fileList += allFiles[i] + ",";
                }
            }
            if (fileList.Length != 0)
            {
                fileList = fileList.Substring(0, fileList.Length - 1);
            }
            this.Close();
        }

        private void bCancel_Click(System.Object sender, System.EventArgs e)
        {
            this.Close();
        }
        public ExcelExport()
        {
            InitializeComponent();
        }

    }
}
