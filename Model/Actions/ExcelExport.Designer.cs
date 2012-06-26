using Microsoft.VisualBasic;
using System;
using System.Collections;
using System.Collections.Generic;
using System.Data;
using System.Diagnostics;
namespace Actions
{
	[Microsoft.VisualBasic.CompilerServices.DesignerGenerated()]
	partial class ExcelExport : System.Windows.Forms.Form
	{
        /// <summary>
        /// Required designer variable.
        /// </summary>
        private System.ComponentModel.IContainer components = null;

        /// <summary>
        /// Clean up any resources being used.
        /// </summary>
        /// <param name="disposing">true if managed resources should be disposed; otherwise, false.</param>
        protected override void Dispose(bool disposing)
        {
            if (disposing && (components != null))
            {
                components.Dispose();
            }
            base.Dispose(disposing);
        }

        #region Windows Form Designer generated code

        /// <summary>
        /// Required method for Designer support - do not modify
        /// the contents of this method with the code editor.
        /// </summary>
        private void InitializeComponent()
		{
            this.fileListBox = new System.Windows.Forms.CheckedListBox();
            this.bSelectAll = new System.Windows.Forms.Button();
            this.bOK = new System.Windows.Forms.Button();
            this.bCancel = new System.Windows.Forms.Button();
            this.bDeselectAll = new System.Windows.Forms.Button();
            this.SuspendLayout();
            // 
            // fileListBox
            // 
            this.fileListBox.Anchor = ((System.Windows.Forms.AnchorStyles)((((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Bottom) 
            | System.Windows.Forms.AnchorStyles.Left) 
            | System.Windows.Forms.AnchorStyles.Right)));
            this.fileListBox.CheckOnClick = true;
            this.fileListBox.FormattingEnabled = true;
            this.fileListBox.HorizontalScrollbar = true;
            this.fileListBox.Location = new System.Drawing.Point(3, 6);
            this.fileListBox.Margin = new System.Windows.Forms.Padding(2);
            this.fileListBox.Name = "fileListBox";
            this.fileListBox.Size = new System.Drawing.Size(326, 274);
            this.fileListBox.TabIndex = 0;
            // 
            // bSelectAll
            // 
            this.bSelectAll.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Bottom | System.Windows.Forms.AnchorStyles.Left)));
            this.bSelectAll.Location = new System.Drawing.Point(3, 296);
            this.bSelectAll.Margin = new System.Windows.Forms.Padding(2);
            this.bSelectAll.Name = "bSelectAll";
            this.bSelectAll.Size = new System.Drawing.Size(75, 24);
            this.bSelectAll.TabIndex = 1;
            this.bSelectAll.Text = "Select All";
            this.bSelectAll.UseVisualStyleBackColor = true;
            this.bSelectAll.Click += new System.EventHandler(this.Button1_Click);
            // 
            // bOK
            // 
            this.bOK.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Bottom | System.Windows.Forms.AnchorStyles.Right)));
            this.bOK.Location = new System.Drawing.Point(195, 296);
            this.bOK.Margin = new System.Windows.Forms.Padding(2);
            this.bOK.Name = "bOK";
            this.bOK.Size = new System.Drawing.Size(65, 24);
            this.bOK.TabIndex = 3;
            this.bOK.Text = "OK";
            this.bOK.UseVisualStyleBackColor = true;
            this.bOK.Click += new System.EventHandler(this.bOK_Click);
            // 
            // bCancel
            // 
            this.bCancel.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Bottom | System.Windows.Forms.AnchorStyles.Right)));
            this.bCancel.DialogResult = System.Windows.Forms.DialogResult.Cancel;
            this.bCancel.Location = new System.Drawing.Point(264, 296);
            this.bCancel.Margin = new System.Windows.Forms.Padding(2);
            this.bCancel.Name = "bCancel";
            this.bCancel.Size = new System.Drawing.Size(65, 24);
            this.bCancel.TabIndex = 4;
            this.bCancel.Text = "Cancel";
            this.bCancel.UseVisualStyleBackColor = true;
            this.bCancel.Click += new System.EventHandler(this.bCancel_Click);
            // 
            // bDeselectAll
            // 
            this.bDeselectAll.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Bottom | System.Windows.Forms.AnchorStyles.Left)));
            this.bDeselectAll.Location = new System.Drawing.Point(82, 296);
            this.bDeselectAll.Margin = new System.Windows.Forms.Padding(2);
            this.bDeselectAll.Name = "bDeselectAll";
            this.bDeselectAll.Size = new System.Drawing.Size(75, 24);
            this.bDeselectAll.TabIndex = 5;
            this.bDeselectAll.Text = "Deselect All";
            this.bDeselectAll.UseVisualStyleBackColor = true;
            this.bDeselectAll.Click += new System.EventHandler(this.bDeselectAll_Click);
            // 
            // ExcelExport
            // 
            this.AcceptButton = this.bOK;
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.CancelButton = this.bCancel;
            this.ClientSize = new System.Drawing.Size(333, 329);
            this.Controls.Add(this.bDeselectAll);
            this.Controls.Add(this.bCancel);
            this.Controls.Add(this.bOK);
            this.Controls.Add(this.bSelectAll);
            this.Controls.Add(this.fileListBox);
            this.Margin = new System.Windows.Forms.Padding(2);
            this.Name = "ExcelExport";
            this.ShowIcon = false;
            this.StartPosition = System.Windows.Forms.FormStartPosition.CenterParent;
            this.Text = "ExcelExport";
            this.TopMost = true;
            this.ResumeLayout(false);

		}

        #endregion

        internal System.Windows.Forms.CheckedListBox fileListBox;
		internal System.Windows.Forms.Button bSelectAll;
        internal System.Windows.Forms.Button bOK;
		internal System.Windows.Forms.Button bCancel;
		internal System.Windows.Forms.Button bDeselectAll;
	}
}
