namespace Graph
   {
   partial class AnimatedReportUI
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
         System.ComponentModel.ComponentResourceManager resources = new System.ComponentModel.ComponentResourceManager(typeof(AnimatedReportUI));
         this.panel1 = new System.Windows.Forms.Panel();
         this.TrackBar = new System.Windows.Forms.TrackBar();
         this.DateLabel = new System.Windows.Forms.Label();
         this.PauseButton = new System.Windows.Forms.RadioButton();
         this.FastForwardButton = new System.Windows.Forms.Button();
         this.RewindButton = new System.Windows.Forms.Button();
         this.PlayButton = new System.Windows.Forms.RadioButton();
         this.panel1.SuspendLayout();
         ((System.ComponentModel.ISupportInitialize)(this.TrackBar)).BeginInit();
         this.SuspendLayout();
         // 
         // Panel
         // 
         this.Panel.Location = new System.Drawing.Point(0, 46);
         this.Panel.Size = new System.Drawing.Size(655, 495);
         // 
         // panel1
         // 
         this.panel1.Controls.Add(this.TrackBar);
         this.panel1.Controls.Add(this.DateLabel);
         this.panel1.Controls.Add(this.PauseButton);
         this.panel1.Controls.Add(this.FastForwardButton);
         this.panel1.Controls.Add(this.RewindButton);
         this.panel1.Controls.Add(this.PlayButton);
         this.panel1.Dock = System.Windows.Forms.DockStyle.Top;
         this.panel1.Location = new System.Drawing.Point(0, 16);
         this.panel1.Name = "panel1";
         this.panel1.Size = new System.Drawing.Size(655, 30);
         this.panel1.TabIndex = 11;
         // 
         // TrackBar
         // 
         this.TrackBar.AutoSize = false;
         this.TrackBar.Location = new System.Drawing.Point(155, 3);
         this.TrackBar.Maximum = 100;
         this.TrackBar.Name = "TrackBar";
         this.TrackBar.Size = new System.Drawing.Size(183, 22);
         this.TrackBar.TabIndex = 16;
         this.TrackBar.TickStyle = System.Windows.Forms.TickStyle.None;
         this.TrackBar.Value = 1;
         // 
         // DateLabel
         // 
         this.DateLabel.AutoSize = true;
         this.DateLabel.Location = new System.Drawing.Point(344, 8);
         this.DateLabel.Name = "DateLabel";
         this.DateLabel.Size = new System.Drawing.Size(30, 13);
         this.DateLabel.TabIndex = 15;
         this.DateLabel.Text = "Date";
         // 
         // PauseButton
         // 
         this.PauseButton.Appearance = System.Windows.Forms.Appearance.Button;
         this.PauseButton.Image = ((System.Drawing.Image)(resources.GetObject("PauseButton.Image")));
         this.PauseButton.Location = new System.Drawing.Point(79, 3);
         this.PauseButton.Name = "PauseButton";
         this.PauseButton.Size = new System.Drawing.Size(32, 23);
         this.PauseButton.TabIndex = 14;
         this.PauseButton.TabStop = true;
         this.PauseButton.UseVisualStyleBackColor = true;
         this.PauseButton.Click += new System.EventHandler(this.OnPauseClick);
         // 
         // FastForwardButton
         // 
         this.FastForwardButton.Image = ((System.Drawing.Image)(resources.GetObject("FastForwardButton.Image")));
         this.FastForwardButton.Location = new System.Drawing.Point(117, 3);
         this.FastForwardButton.Name = "FastForwardButton";
         this.FastForwardButton.Size = new System.Drawing.Size(32, 23);
         this.FastForwardButton.TabIndex = 12;
         this.FastForwardButton.UseVisualStyleBackColor = true;
         this.FastForwardButton.Click += new System.EventHandler(this.OnForwardClick);
         // 
         // RewindButton
         // 
         this.RewindButton.Image = ((System.Drawing.Image)(resources.GetObject("RewindButton.Image")));
         this.RewindButton.Location = new System.Drawing.Point(4, 3);
         this.RewindButton.Name = "RewindButton";
         this.RewindButton.Size = new System.Drawing.Size(32, 23);
         this.RewindButton.TabIndex = 11;
         this.RewindButton.UseVisualStyleBackColor = true;
         this.RewindButton.Click += new System.EventHandler(this.OnRewindClick);
         // 
         // PlayButton
         // 
         this.PlayButton.Appearance = System.Windows.Forms.Appearance.Button;
         this.PlayButton.Image = ((System.Drawing.Image)(resources.GetObject("PlayButton.Image")));
         this.PlayButton.Location = new System.Drawing.Point(42, 3);
         this.PlayButton.Name = "PlayButton";
         this.PlayButton.Size = new System.Drawing.Size(32, 23);
         this.PlayButton.TabIndex = 13;
         this.PlayButton.TabStop = true;
         this.PlayButton.UseVisualStyleBackColor = true;
         this.PlayButton.Click += new System.EventHandler(this.OnPlayClick);
         // 
         // AnimatedReportUI
         // 
         this.Controls.Add(this.panel1);
         this.Name = "AnimatedReportUI";
         this.Controls.SetChildIndex(this.MyHelpLabel, 0);
         this.Controls.SetChildIndex(this.panel1, 0);
         this.Controls.SetChildIndex(this.Panel, 0);
         this.panel1.ResumeLayout(false);
         this.panel1.PerformLayout();
         ((System.ComponentModel.ISupportInitialize)(this.TrackBar)).EndInit();
         this.ResumeLayout(false);

         }

      #endregion

      private System.Windows.Forms.Panel panel1;
      private System.Windows.Forms.Label DateLabel;
      private System.Windows.Forms.RadioButton PauseButton;
      private System.Windows.Forms.Button FastForwardButton;
      private System.Windows.Forms.Button RewindButton;
      private System.Windows.Forms.RadioButton PlayButton;
      private System.Windows.Forms.TrackBar TrackBar;

      }
   }
