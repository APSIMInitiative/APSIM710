using System; 
using System.Collections.Specialized; 
using System.Drawing; 
using System.IO;
using System.Windows.Forms;
using System.Xml; 

using ApsimFile; 
using CSGeneral;


namespace UIBits
   {

   public class NewDocumentForm : System.Windows.Forms.Form
      {

      protected XmlNode SelectedData;

      internal System.Windows.Forms.ImageList ImageList;
      internal System.Windows.Forms.TreeView TreeView;
      internal System.Windows.Forms.Label Label1;
      internal System.Windows.Forms.PictureBox PictureBox1;
      internal System.Windows.Forms.Button OKButton;
      internal System.Windows.Forms.Button CancelButton1;


      #region " Windows Form Designer generated code "

      public NewDocumentForm()
         : base()
         {

         //This call is required by the Windows Form Designer. 
         InitializeComponent();

         //Add any initialization after the InitializeComponent() call 

         }

      //Required by the Windows Form Designer 
      private System.ComponentModel.IContainer components;

      protected override void Dispose(bool disposing)
         {
         if (disposing)
            {
            if ((components != null))
               {
               components.Dispose();
               }
            }
         base.Dispose(disposing);
         }



      private void InitializeComponent()
         {
         this.components = new System.ComponentModel.Container();
         System.ComponentModel.ComponentResourceManager resources = new System.ComponentModel.ComponentResourceManager(typeof(NewDocumentForm));
         this.Label1 = new System.Windows.Forms.Label();
         this.PictureBox1 = new System.Windows.Forms.PictureBox();
         this.OKButton = new System.Windows.Forms.Button();
         this.CancelButton1 = new System.Windows.Forms.Button();
         this.TreeView = new System.Windows.Forms.TreeView();
         this.ImageList = new System.Windows.Forms.ImageList(this.components);
         ((System.ComponentModel.ISupportInitialize)(this.PictureBox1)).BeginInit();
         this.SuspendLayout();
         // 
         // Label1
         // 
         this.Label1.Location = new System.Drawing.Point(128, 8);
         this.Label1.Name = "Label1";
         this.Label1.Size = new System.Drawing.Size(248, 23);
         this.Label1.TabIndex = 0;
         this.Label1.Text = "Choose a simulation type from the list below";
         // 
         // PictureBox1
         // 
         this.PictureBox1.Dock = System.Windows.Forms.DockStyle.Left;
         this.PictureBox1.Image = ((System.Drawing.Image)(resources.GetObject("PictureBox1.Image")));
         this.PictureBox1.Location = new System.Drawing.Point(0, 0);
         this.PictureBox1.Name = "PictureBox1";
         this.PictureBox1.Size = new System.Drawing.Size(120, 398);
         this.PictureBox1.TabIndex = 1;
         this.PictureBox1.TabStop = false;
         // 
         // OKButton
         // 
         this.OKButton.DialogResult = System.Windows.Forms.DialogResult.OK;
         this.OKButton.Location = new System.Drawing.Point(432, 368);
         this.OKButton.Name = "OKButton";
         this.OKButton.Size = new System.Drawing.Size(75, 23);
         this.OKButton.TabIndex = 3;
         this.OKButton.Text = "OK";
         // 
         // CancelButton1
         // 
         this.CancelButton1.DialogResult = System.Windows.Forms.DialogResult.Cancel;
         this.CancelButton1.Location = new System.Drawing.Point(520, 368);
         this.CancelButton1.Name = "CancelButton1";
         this.CancelButton1.Size = new System.Drawing.Size(75, 23);
         this.CancelButton1.TabIndex = 4;
         this.CancelButton1.Text = "Cancel";
         // 
         // TreeView
         // 
         this.TreeView.ImageIndex = 0;
         this.TreeView.ImageList = this.ImageList;
         this.TreeView.Location = new System.Drawing.Point(131, 24);
         this.TreeView.Name = "TreeView";
         this.TreeView.SelectedImageIndex = 0;
         this.TreeView.Size = new System.Drawing.Size(464, 338);
         this.TreeView.TabIndex = 5;
         this.TreeView.MouseDoubleClick += new System.Windows.Forms.MouseEventHandler(this.OnDoubleClick);
         // 
         // ImageList
         // 
         this.ImageList.ImageStream = ((System.Windows.Forms.ImageListStreamer)(resources.GetObject("ImageList.ImageStream")));
         this.ImageList.TransparentColor = System.Drawing.Color.Transparent;
         this.ImageList.Images.SetKeyName(0, "folder_closed16.png");
         this.ImageList.Images.SetKeyName(1, "document16.png");
         // 
         // NewDocumentForm
         // 
         this.AutoScaleBaseSize = new System.Drawing.Size(5, 13);
         this.CancelButton = this.CancelButton1;
         this.ClientSize = new System.Drawing.Size(610, 398);
         this.Controls.Add(this.TreeView);
         this.Controls.Add(this.OKButton);
         this.Controls.Add(this.CancelButton1);
         this.Controls.Add(this.PictureBox1);
         this.Controls.Add(this.Label1);
         this.FormBorderStyle = System.Windows.Forms.FormBorderStyle.FixedToolWindow;
         this.MaximizeBox = false;
         this.MinimizeBox = false;
         this.Name = "NewDocumentForm";
         this.StartPosition = System.Windows.Forms.FormStartPosition.CenterScreen;
         this.Text = "APSIM";
         this.Load += new System.EventHandler(this.NewDocumentForm_Load);
         ((System.ComponentModel.ISupportInitialize)(this.PictureBox1)).EndInit();
         this.ResumeLayout(false);

         }

      #endregion


      private void NewDocumentForm_Load(object sender, System.EventArgs e)
         {
         //----------------------------------------------------- 
         // Document has just been displayed - set everything up 
         // ---------------------------------------------------- 
         foreach (string Folder in Configuration.Instance.Settings("NewSimulationFolder"))
            {
            PopulateTreeNode(null, Folder);
            }
         TreeView.Nodes[0].Expand();

         }

      private void PopulateTreeNode(TreeNode Node, string Folder)
         {
         string NodeName = Folder.Substring(Folder.LastIndexOf("\\") + 1);
         if ((Node == null))
            {
            Node = TreeView.Nodes.Add(NodeName);
            }
         else
            {
            Node = Node.Nodes.Add(NodeName);
            }
         Node.ImageIndex = 0;
         Node.SelectedImageIndex = 0;
         foreach (string SubFolder in Directory.GetDirectories(Folder))
            {
            PopulateTreeNode(Node, SubFolder);
            // recursion 
            }

         foreach (string SimulationFile in Directory.GetFiles(Folder, "*.apsim"))
            {
            TreeNode Child = Node.Nodes.Add(Path.GetFileNameWithoutExtension(SimulationFile));
            Child.ImageIndex = 1;
            Child.SelectedImageIndex = 1;
            Child.Tag = SimulationFile;
            }
         }

      public string Selection
         {
         // ----------------------------------- 
         // Return selection to caller. 
         // ----------------------------------- 
         get
            {
            StreamReader SelectedStream = new StreamReader(TreeView.SelectedNode.Tag.ToString());
            return SelectedStream.ReadToEnd();
            }
         }


      private void OnDoubleClick(object sender, MouseEventArgs e)
         {
         OKButton.PerformClick();
         }
      }
   }