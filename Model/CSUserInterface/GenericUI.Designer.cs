namespace CSUserInterface
{
    partial class GenericUI
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

        #region Component Designer generated code

        /// <summary> 
        /// Required method for Designer support - do not modify 
        /// the contents of this method with the code editor.
        /// </summary>
        private void InitializeComponent()
        {
            this.PictureBox = new System.Windows.Forms.PictureBox();
            this.Grid = new UIBits.EnhancedGrid();
            this.TextBox = new System.Windows.Forms.TextBox();
            this.Splitter = new System.Windows.Forms.Splitter();
            ((System.ComponentModel.ISupportInitialize)(this.PictureBox)).BeginInit();
            ((System.ComponentModel.ISupportInitialize)(this.Grid)).BeginInit();
            this.SuspendLayout();
            // 
            // MyHelpLabel
            // 
            this.MyHelpLabel.Size = new System.Drawing.Size(1022, 16);
            // 
            // PictureBox
            // 
            this.PictureBox.BackgroundImageLayout = System.Windows.Forms.ImageLayout.None;
            this.PictureBox.Dock = System.Windows.Forms.DockStyle.Left;
            this.PictureBox.Location = new System.Drawing.Point(0, 16);
            this.PictureBox.Name = "PictureBox";
            this.PictureBox.Size = new System.Drawing.Size(125, 701);
            this.PictureBox.SizeMode = System.Windows.Forms.PictureBoxSizeMode.AutoSize;
            this.PictureBox.TabIndex = 3;
            this.PictureBox.TabStop = false;
            // 
            // Grid
            // 
            this.Grid.AllowUserToAddRows = false;
            this.Grid.AllowUserToResizeRows = false;
            this.Grid.BackgroundColor = System.Drawing.SystemColors.Window;
            this.Grid.BorderStyle = System.Windows.Forms.BorderStyle.None;
            this.Grid.ColumnHeadersHeightSizeMode = System.Windows.Forms.DataGridViewColumnHeadersHeightSizeMode.AutoSize;
            this.Grid.DataSourceTable = null;
            this.Grid.Dock = System.Windows.Forms.DockStyle.Fill;
            this.Grid.Location = new System.Drawing.Point(125, 141);
            this.Grid.Name = "Grid";
            this.Grid.RowHeadersVisible = false;
            this.Grid.Size = new System.Drawing.Size(897, 576);
            this.Grid.TabIndex = 4;
            this.Grid.TableColumnChangedEvent += new UIBits.EnhancedGrid.TableColumnChangedDelegate(this.OnTableColumnChanged);
            this.Grid.CellContentClick += new System.Windows.Forms.DataGridViewCellEventHandler(this.OnCellContentClick);
            // 
            // TextBox
            // 
            this.TextBox.Dock = System.Windows.Forms.DockStyle.Top;
            this.TextBox.Font = new System.Drawing.Font("Microsoft Sans Serif", 9.75F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.TextBox.Location = new System.Drawing.Point(125, 16);
            this.TextBox.Multiline = true;
            this.TextBox.Name = "TextBox";
            this.TextBox.Size = new System.Drawing.Size(897, 125);
            this.TextBox.TabIndex = 5;
            this.TextBox.TextChanged += new System.EventHandler(this.OnMemoTextChanged);
            // 
            // Splitter
            // 
            this.Splitter.Dock = System.Windows.Forms.DockStyle.Top;
            this.Splitter.Location = new System.Drawing.Point(125, 141);
            this.Splitter.Name = "Splitter";
            this.Splitter.Size = new System.Drawing.Size(897, 3);
            this.Splitter.TabIndex = 6;
            this.Splitter.TabStop = false;
            // 
            // GenericUI
            // 
            this.Controls.Add(this.Splitter);
            this.Controls.Add(this.Grid);
            this.Controls.Add(this.TextBox);
            this.Controls.Add(this.PictureBox);
            this.Name = "GenericUI";
            this.Size = new System.Drawing.Size(1022, 717);
            this.Controls.SetChildIndex(this.MyHelpLabel, 0);
            this.Controls.SetChildIndex(this.PictureBox, 0);
            this.Controls.SetChildIndex(this.TextBox, 0);
            this.Controls.SetChildIndex(this.Grid, 0);
            this.Controls.SetChildIndex(this.Splitter, 0);
            ((System.ComponentModel.ISupportInitialize)(this.PictureBox)).EndInit();
            ((System.ComponentModel.ISupportInitialize)(this.Grid)).EndInit();
            this.ResumeLayout(false);
            this.PerformLayout();

        }

        #endregion

        private System.Windows.Forms.PictureBox PictureBox;
        private UIBits.EnhancedGrid Grid;
        private System.Windows.Forms.TextBox TextBox;
        private System.Windows.Forms.Splitter Splitter;
    }
}

