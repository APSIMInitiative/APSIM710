using Microsoft.VisualBasic;
using System;
using System.Collections;
using System.Collections.Generic;
using System.Data;
using System.Drawing;
using System.Diagnostics;
using System.Windows.Forms;

using System.Collections.Specialized;

using ApsimFile;
using Controllers;
using CSGeneral;
namespace CSUserInterface
{


	public partial class EmptyUI : BaseView
	{

		public EmptyUI() : base()
		{

			//This call is required by the Windows Form Designer.
			InitializeComponent();

			//Add any initialization after the InitializeComponent() call

		}

        public override void OnRefresh()
		{
			MainLabel.Text = XmlHelper.Type(Data);
			this.HelpText = "This module does not have any editable properties.";

			string imagefile = Types.Instance.MetaData(Data.Name, "image");
			if (System.IO.File.Exists(imagefile)) {
				PictureBox.Image = Image.FromFile(imagefile);
			} else {
				PictureBox.Image = null;
			}

			// Populate the group box will links for each document file found.
			int PosX = 30;
			int PosY = 30;
			int LineSpacing = 20;
			GroupBox.Controls.Clear();
			List<string> DocNames = new List<string>();
			List<string> Urls = new List<string>();
			Types.Instance.Documentation(XmlHelper.Type(Data), out DocNames, out Urls);
			for (int i = 0; i <= DocNames.Count - 1; i++) {
				LinkLabel NewLink = new LinkLabel();
				NewLink.Text = DocNames[i];
				NewLink.Tag = Urls[i];
				NewLink.LinkClicked += OnLinkClicked;
				NewLink.Parent = GroupBox;
				NewLink.AutoSize = true;
				NewLink.Top = PosY;
				NewLink.Left = PosX;
				PosY = PosY + LineSpacing;
			}
			GroupBox.Visible = DocNames.Count > 0;
		}

		private void OnLinkClicked(System.Object sender, System.Windows.Forms.LinkLabelLinkClickedEventArgs e)
		{
			try {
                string url = sender is LinkLabel ? (string)(sender as LinkLabel).Tag : "";
				Process.Start(url);
			} catch (Exception ex) {
				MessageBox.Show(ex.Message);
			}
		}

	}
}
