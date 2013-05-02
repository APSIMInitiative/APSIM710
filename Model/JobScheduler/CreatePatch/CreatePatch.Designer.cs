
partial class MainForm
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
            System.ComponentModel.ComponentResourceManager resources = new System.ComponentModel.ComponentResourceManager(typeof(MainForm));
            this.ListView = new System.Windows.Forms.ListView();
            this.columnHeader1 = ((System.Windows.Forms.ColumnHeader)(new System.Windows.Forms.ColumnHeader()));
            this.columnPath = ((System.Windows.Forms.ColumnHeader)(new System.Windows.Forms.ColumnHeader()));
            this.columnType = ((System.Windows.Forms.ColumnHeader)(new System.Windows.Forms.ColumnHeader()));
            this.columnStatus = ((System.Windows.Forms.ColumnHeader)(new System.Windows.Forms.ColumnHeader()));
            this.columnRevision = ((System.Windows.Forms.ColumnHeader)(new System.Windows.Forms.ColumnHeader()));
            this.columnModified = ((System.Windows.Forms.ColumnHeader)(new System.Windows.Forms.ColumnHeader()));
            this.SelectCheckBox = new System.Windows.Forms.CheckBox();
            this.OKButton = new System.Windows.Forms.Button();
            this.CanclButton = new System.Windows.Forms.Button();
            this.SaveFileDialog = new System.Windows.Forms.SaveFileDialog();
            this.labelChecked = new System.Windows.Forms.Label();
            this.DirectoryLabel = new System.Windows.Forms.Label();
            this.SuspendLayout();
            // 
            // ListView
            // 
            this.ListView.Anchor = ((System.Windows.Forms.AnchorStyles)((((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Bottom) 
            | System.Windows.Forms.AnchorStyles.Left) 
            | System.Windows.Forms.AnchorStyles.Right)));
            this.ListView.CheckBoxes = true;
            this.ListView.Columns.AddRange(new System.Windows.Forms.ColumnHeader[] {
            this.columnHeader1,
            this.columnPath,
            this.columnType,
            this.columnStatus,
            this.columnRevision,
            this.columnModified});
            this.ListView.Location = new System.Drawing.Point(13, 27);
            this.ListView.Name = "ListView";
            this.ListView.ShowGroups = false;
            this.ListView.Size = new System.Drawing.Size(874, 476);
            this.ListView.TabIndex = 0;
            this.ListView.UseCompatibleStateImageBehavior = false;
            this.ListView.View = System.Windows.Forms.View.Details;
            this.ListView.ColumnClick += new System.Windows.Forms.ColumnClickEventHandler(this.ListView_ColumnClick);
            this.ListView.ItemChecked += new System.Windows.Forms.ItemCheckedEventHandler(this.ListView_ItemChecked);
            // 
            // columnHeader1
            // 
            this.columnHeader1.Text = "Name";
            this.columnHeader1.Width = 364;
            // 
            // columnPath
            // 
            this.columnPath.Text = "Path";
            this.columnPath.Width = 166;
            // 
            // columnType
            // 
            this.columnType.Text = "Type";
            this.columnType.Width = 61;
            // 
            // columnStatus
            // 
            this.columnStatus.Text = "SVN Status";
            this.columnStatus.Width = 84;
            // 
            // columnRevision
            // 
            this.columnRevision.Text = "Revision";
            // 
            // columnModified
            // 
            this.columnModified.Text = "Modified";
            this.columnModified.Width = 107;
            // 
            // SelectCheckBox
            // 
            this.SelectCheckBox.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Bottom | System.Windows.Forms.AnchorStyles.Left)));
            this.SelectCheckBox.AutoSize = true;
            this.SelectCheckBox.Location = new System.Drawing.Point(13, 522);
            this.SelectCheckBox.Name = "SelectCheckBox";
            this.SelectCheckBox.Size = new System.Drawing.Size(120, 17);
            this.SelectCheckBox.TabIndex = 1;
            this.SelectCheckBox.Text = "Select / deselect all";
            this.SelectCheckBox.UseVisualStyleBackColor = true;
            this.SelectCheckBox.CheckedChanged += new System.EventHandler(this.OnSelectCheckBoxChanged);
            // 
            // OKButton
            // 
            this.OKButton.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Bottom | System.Windows.Forms.AnchorStyles.Right)));
            this.OKButton.DialogResult = System.Windows.Forms.DialogResult.OK;
            this.OKButton.Location = new System.Drawing.Point(722, 522);
            this.OKButton.Name = "OKButton";
            this.OKButton.Size = new System.Drawing.Size(75, 23);
            this.OKButton.TabIndex = 2;
            this.OKButton.Text = "OK";
            this.OKButton.UseVisualStyleBackColor = true;
            this.OKButton.Click += new System.EventHandler(this.OnOKButtonClick);
            // 
            // CanclButton
            // 
            this.CanclButton.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Bottom | System.Windows.Forms.AnchorStyles.Right)));
            this.CanclButton.DialogResult = System.Windows.Forms.DialogResult.Cancel;
            this.CanclButton.Location = new System.Drawing.Point(812, 522);
            this.CanclButton.Name = "CanclButton";
            this.CanclButton.Size = new System.Drawing.Size(75, 23);
            this.CanclButton.TabIndex = 3;
            this.CanclButton.Text = "Cancel";
            this.CanclButton.UseVisualStyleBackColor = true;
            this.CanclButton.Click += new System.EventHandler(this.OnCancelButtonClick);
            // 
            // SaveFileDialog
            // 
            this.SaveFileDialog.DefaultExt = "zip";
            this.SaveFileDialog.Filter = "Zip files|*.zip";
            this.SaveFileDialog.RestoreDirectory = true;
            this.SaveFileDialog.Title = "Enter name of patch zip file.";
            // 
            // labelChecked
            // 
            this.labelChecked.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Bottom | System.Windows.Forms.AnchorStyles.Left)));
            this.labelChecked.AutoSize = true;
            this.labelChecked.Location = new System.Drawing.Point(175, 523);
            this.labelChecked.Name = "labelChecked";
            this.labelChecked.Size = new System.Drawing.Size(83, 13);
            this.labelChecked.TabIndex = 4;
            this.labelChecked.Text = "0 items selected";
            // 
            // DirectoryLabel
            // 
            this.DirectoryLabel.AutoSize = true;
            this.DirectoryLabel.Location = new System.Drawing.Point(13, 8);
            this.DirectoryLabel.Name = "DirectoryLabel";
            this.DirectoryLabel.Size = new System.Drawing.Size(0, 13);
            this.DirectoryLabel.TabIndex = 5;
            // 
            // MainForm
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.ClientSize = new System.Drawing.Size(899, 554);
            this.Controls.Add(this.DirectoryLabel);
            this.Controls.Add(this.labelChecked);
            this.Controls.Add(this.CanclButton);
            this.Controls.Add(this.OKButton);
            this.Controls.Add(this.SelectCheckBox);
            this.Controls.Add(this.ListView);
            this.Icon = ((System.Drawing.Icon)(resources.GetObject("$this.Icon")));
            this.Name = "MainForm";
            this.Text = "Create Patch";
            this.Shown += new System.EventHandler(this.OnMainFormShown);
            this.ResumeLayout(false);
            this.PerformLayout();

    }

    #endregion

    private System.Windows.Forms.ListView ListView;
    private System.Windows.Forms.CheckBox SelectCheckBox;
    private System.Windows.Forms.Button OKButton;
    private System.Windows.Forms.Button CanclButton;
    private System.Windows.Forms.ColumnHeader columnHeader1;
    private System.Windows.Forms.SaveFileDialog SaveFileDialog;
    private System.Windows.Forms.ColumnHeader columnType;
    private System.Windows.Forms.ColumnHeader columnPath;
    private System.Windows.Forms.ColumnHeader columnStatus;
    private System.Windows.Forms.Label labelChecked;
    private System.Windows.Forms.ColumnHeader columnRevision;
    private System.Windows.Forms.Label DirectoryLabel;
    private System.Windows.Forms.ColumnHeader columnModified;
}


