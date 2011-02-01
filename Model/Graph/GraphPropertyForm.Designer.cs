namespace Graph
   {
   partial class GraphPropertyForm
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
         this.components = new System.ComponentModel.Container();
         System.Windows.Forms.TreeNode treeNode1 = new System.Windows.Forms.TreeNode("Series");
         System.Windows.Forms.TreeNode treeNode2 = new System.Windows.Forms.TreeNode("Left");
         System.Windows.Forms.TreeNode treeNode3 = new System.Windows.Forms.TreeNode("Top");
         System.Windows.Forms.TreeNode treeNode4 = new System.Windows.Forms.TreeNode("Right");
         System.Windows.Forms.TreeNode treeNode5 = new System.Windows.Forms.TreeNode("Bottom");
         System.Windows.Forms.TreeNode treeNode6 = new System.Windows.Forms.TreeNode("Axes", new System.Windows.Forms.TreeNode[] {
            treeNode2,
            treeNode3,
            treeNode4,
            treeNode5});
         System.Windows.Forms.TreeNode treeNode7 = new System.Windows.Forms.TreeNode("Titles");
         System.Windows.Forms.TreeNode treeNode8 = new System.Windows.Forms.TreeNode("Legend");
         System.Windows.Forms.TreeNode treeNode9 = new System.Windows.Forms.TreeNode("Panel");
         System.Windows.Forms.TreeNode treeNode10 = new System.Windows.Forms.TreeNode("Left wall");
         System.Windows.Forms.TreeNode treeNode11 = new System.Windows.Forms.TreeNode("Right wall");
         System.Windows.Forms.TreeNode treeNode12 = new System.Windows.Forms.TreeNode("Back wall");
         System.Windows.Forms.TreeNode treeNode13 = new System.Windows.Forms.TreeNode("Bottom wall");
         System.Windows.Forms.TreeNode treeNode14 = new System.Windows.Forms.TreeNode("Walls", new System.Windows.Forms.TreeNode[] {
            treeNode10,
            treeNode11,
            treeNode12,
            treeNode13});
         System.Windows.Forms.TreeNode treeNode15 = new System.Windows.Forms.TreeNode("Paging");
         System.Windows.Forms.TreeNode treeNode16 = new System.Windows.Forms.TreeNode("3D");
         System.Windows.Forms.TreeNode treeNode17 = new System.Windows.Forms.TreeNode("Tools");
         System.Windows.Forms.TreeNode treeNode18 = new System.Windows.Forms.TreeNode("Themes");
         System.Windows.Forms.TreeNode treeNode19 = new System.Windows.Forms.TreeNode("Chart", new System.Windows.Forms.TreeNode[] {
            treeNode1,
            treeNode6,
            treeNode7,
            treeNode8,
            treeNode9,
            treeNode14,
            treeNode15,
            treeNode16,
            treeNode17,
            treeNode18});
         this.AddSeriesMenu = new System.Windows.Forms.ContextMenuStrip(this.components);
         this.toolStripMenuItem2 = new System.Windows.Forms.ToolStripMenuItem();
         this.PropertyPanel = new System.Windows.Forms.Panel();
         this.splitter1 = new System.Windows.Forms.Splitter();
         this.Tree = new System.Windows.Forms.TreeView();
         this.SeriesContextMenu = new System.Windows.Forms.ContextMenuStrip(this.components);
         this.deleteToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
         this.toolStripMenuItem1 = new System.Windows.Forms.ToolStripMenuItem();
         this.EnabledMenu = new System.Windows.Forms.ToolStripMenuItem();
         this.toolStripSeparator1 = new System.Windows.Forms.ToolStripSeparator();
         this.toolStripMenuItem3 = new System.Windows.Forms.ToolStripMenuItem();
         this.toolStripMenuItem4 = new System.Windows.Forms.ToolStripMenuItem();
         this.toolStripSeparator2 = new System.Windows.Forms.ToolStripSeparator();
         this.dataToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
         this.AddSeriesMenu.SuspendLayout();
         this.SeriesContextMenu.SuspendLayout();
         this.SuspendLayout();
         // 
         // AddSeriesMenu
         // 
         this.AddSeriesMenu.Items.AddRange(new System.Windows.Forms.ToolStripItem[] {
            this.toolStripMenuItem2});
         this.AddSeriesMenu.Name = "SeriesContextMenu";
         this.AddSeriesMenu.Size = new System.Drawing.Size(105, 26);
         // 
         // toolStripMenuItem2
         // 
         this.toolStripMenuItem2.Name = "toolStripMenuItem2";
         this.toolStripMenuItem2.Size = new System.Drawing.Size(104, 22);
         this.toolStripMenuItem2.Text = "Add";
         this.toolStripMenuItem2.Click += new System.EventHandler(this.OnAddSeriesMenu);
         // 
         // PropertyPanel
         // 
         this.PropertyPanel.Dock = System.Windows.Forms.DockStyle.Fill;
         this.PropertyPanel.Location = new System.Drawing.Point(150, 0);
         this.PropertyPanel.Name = "PropertyPanel";
         this.PropertyPanel.Size = new System.Drawing.Size(379, 389);
         this.PropertyPanel.TabIndex = 11;
         // 
         // splitter1
         // 
         this.splitter1.Location = new System.Drawing.Point(147, 0);
         this.splitter1.Name = "splitter1";
         this.splitter1.Size = new System.Drawing.Size(3, 389);
         this.splitter1.TabIndex = 12;
         this.splitter1.TabStop = false;
         // 
         // Tree
         // 
         this.Tree.Dock = System.Windows.Forms.DockStyle.Left;
         this.Tree.LabelEdit = true;
         this.Tree.Location = new System.Drawing.Point(0, 0);
         this.Tree.Name = "Tree";
         treeNode1.ContextMenuStrip = this.AddSeriesMenu;
         treeNode1.Name = "Node3";
         treeNode1.Text = "Series";
         treeNode2.Name = "Node16";
         treeNode2.Text = "Left";
         treeNode3.Name = "Node17";
         treeNode3.Text = "Top";
         treeNode4.Name = "Node18";
         treeNode4.Text = "Right";
         treeNode5.Name = "Node19";
         treeNode5.Text = "Bottom";
         treeNode6.Name = "Node4";
         treeNode6.Text = "Axes";
         treeNode7.Name = "Node5";
         treeNode7.Text = "Titles";
         treeNode8.Name = "Node6";
         treeNode8.Text = "Legend";
         treeNode9.Name = "Node7";
         treeNode9.Text = "Panel";
         treeNode10.Name = "Node20";
         treeNode10.Text = "Left wall";
         treeNode11.Name = "Node21";
         treeNode11.Text = "Right wall";
         treeNode12.Name = "Node22";
         treeNode12.Text = "Back wall";
         treeNode13.Name = "Node23";
         treeNode13.Text = "Bottom wall";
         treeNode14.Name = "Node8";
         treeNode14.Text = "Walls";
         treeNode15.Name = "Node9";
         treeNode15.Text = "Paging";
         treeNode16.Name = "Node10";
         treeNode16.Text = "3D";
         treeNode17.Name = "Node11";
         treeNode17.Text = "Tools";
         treeNode18.Name = "Node12";
         treeNode18.Text = "Themes";
         treeNode19.Name = "Node2";
         treeNode19.Text = "Chart";
         this.Tree.Nodes.AddRange(new System.Windows.Forms.TreeNode[] {
            treeNode19});
         this.Tree.ShowNodeToolTips = true;
         this.Tree.Size = new System.Drawing.Size(147, 389);
         this.Tree.TabIndex = 13;
         this.Tree.AfterLabelEdit += new System.Windows.Forms.NodeLabelEditEventHandler(this.Tree_AfterLabelEdit);
         this.Tree.AfterSelect += new System.Windows.Forms.TreeViewEventHandler(this.OnTreeNodeSelect);
         this.Tree.BeforeLabelEdit += new System.Windows.Forms.NodeLabelEditEventHandler(this.Tree_BeforeLabelEdit);
         this.Tree.KeyDown += new System.Windows.Forms.KeyEventHandler(this.OnKeyDown);
         // 
         // SeriesContextMenu
         // 
         this.SeriesContextMenu.Items.AddRange(new System.Windows.Forms.ToolStripItem[] {
            this.dataToolStripMenuItem,
            this.toolStripSeparator2,
            this.deleteToolStripMenuItem,
            this.toolStripMenuItem1,
            this.EnabledMenu,
            this.toolStripSeparator1,
            this.toolStripMenuItem3,
            this.toolStripMenuItem4});
         this.SeriesContextMenu.Name = "SeriesContextMenu";
         this.SeriesContextMenu.Size = new System.Drawing.Size(200, 170);
         // 
         // deleteToolStripMenuItem
         // 
         this.deleteToolStripMenuItem.Name = "deleteToolStripMenuItem";
         this.deleteToolStripMenuItem.ShortcutKeys = System.Windows.Forms.Keys.Delete;
         this.deleteToolStripMenuItem.ShowShortcutKeys = false;
         this.deleteToolStripMenuItem.Size = new System.Drawing.Size(199, 22);
         this.deleteToolStripMenuItem.Text = "Delete";
         this.deleteToolStripMenuItem.Click += new System.EventHandler(this.OnDeleteSeriesMenu);
         // 
         // toolStripMenuItem1
         // 
         this.toolStripMenuItem1.Name = "toolStripMenuItem1";
         this.toolStripMenuItem1.ShortcutKeys = System.Windows.Forms.Keys.F2;
         this.toolStripMenuItem1.Size = new System.Drawing.Size(199, 22);
         this.toolStripMenuItem1.Text = "Rename";
         this.toolStripMenuItem1.Click += new System.EventHandler(this.OnRenameSeriesMenu);
         // 
         // EnabledMenu
         // 
         this.EnabledMenu.Checked = true;
         this.EnabledMenu.CheckOnClick = true;
         this.EnabledMenu.CheckState = System.Windows.Forms.CheckState.Checked;
         this.EnabledMenu.Name = "EnabledMenu";
         this.EnabledMenu.Size = new System.Drawing.Size(199, 22);
         this.EnabledMenu.Text = "Enabled";
         this.EnabledMenu.Click += new System.EventHandler(this.OnEnabledMenu);
         // 
         // toolStripSeparator1
         // 
         this.toolStripSeparator1.Name = "toolStripSeparator1";
         this.toolStripSeparator1.Size = new System.Drawing.Size(196, 6);
         // 
         // toolStripMenuItem3
         // 
         this.toolStripMenuItem3.Name = "toolStripMenuItem3";
         this.toolStripMenuItem3.ShortcutKeys = ((System.Windows.Forms.Keys)((System.Windows.Forms.Keys.Control | System.Windows.Forms.Keys.Down)));
         this.toolStripMenuItem3.Size = new System.Drawing.Size(199, 22);
         this.toolStripMenuItem3.Text = "Move down";
         this.toolStripMenuItem3.Click += new System.EventHandler(this.OnMoveDown);
         // 
         // toolStripMenuItem4
         // 
         this.toolStripMenuItem4.Name = "toolStripMenuItem4";
         this.toolStripMenuItem4.ShortcutKeys = ((System.Windows.Forms.Keys)((System.Windows.Forms.Keys.Control | System.Windows.Forms.Keys.Up)));
         this.toolStripMenuItem4.Size = new System.Drawing.Size(199, 22);
         this.toolStripMenuItem4.Text = "Move up";
         this.toolStripMenuItem4.Click += new System.EventHandler(this.OnMoveUp);
         // 
         // toolStripSeparator2
         // 
         this.toolStripSeparator2.Name = "toolStripSeparator2";
         this.toolStripSeparator2.Size = new System.Drawing.Size(196, 6);
         // 
         // dataToolStripMenuItem
         // 
         this.dataToolStripMenuItem.Name = "dataToolStripMenuItem";
         this.dataToolStripMenuItem.Size = new System.Drawing.Size(199, 22);
         this.dataToolStripMenuItem.Text = "Graph type and &Data";
         this.dataToolStripMenuItem.Click += new System.EventHandler(this.OnXYDataSeriesMenu);
         // 
         // GraphPropertyForm
         // 
         this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
         this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
         this.ClientSize = new System.Drawing.Size(529, 389);
         this.Controls.Add(this.PropertyPanel);
         this.Controls.Add(this.splitter1);
         this.Controls.Add(this.Tree);
         this.Name = "GraphPropertyForm";
         this.Text = "GraphPropertyForm";
         this.Load += new System.EventHandler(this.OnLoad);
         this.AddSeriesMenu.ResumeLayout(false);
         this.SeriesContextMenu.ResumeLayout(false);
         this.ResumeLayout(false);

         }

      #endregion

      private System.Windows.Forms.Panel PropertyPanel;
      private System.Windows.Forms.Splitter splitter1;
      private System.Windows.Forms.TreeView Tree;
      private System.Windows.Forms.ContextMenuStrip SeriesContextMenu;
      private System.Windows.Forms.ToolStripMenuItem deleteToolStripMenuItem;
      private System.Windows.Forms.ToolStripMenuItem toolStripMenuItem1;
      private System.Windows.Forms.ToolStripMenuItem dataToolStripMenuItem;
      private System.Windows.Forms.ContextMenuStrip AddSeriesMenu;
      private System.Windows.Forms.ToolStripMenuItem toolStripMenuItem2;
      private System.Windows.Forms.ToolStripMenuItem EnabledMenu;
      private System.Windows.Forms.ToolStripSeparator toolStripSeparator1;
      private System.Windows.Forms.ToolStripMenuItem toolStripMenuItem3;
      private System.Windows.Forms.ToolStripMenuItem toolStripMenuItem4;
      private System.Windows.Forms.ToolStripSeparator toolStripSeparator2;
      }
   }