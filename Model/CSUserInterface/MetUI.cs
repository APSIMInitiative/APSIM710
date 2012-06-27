using Microsoft.VisualBasic;
using System;
using System.Collections;
using System.Collections.Generic;
using System.Data;
using System.Drawing;
using System.Diagnostics;
using System.Windows.Forms;

using System.IO;

using CSGeneral;
using ApsimFile;
using Controllers;
namespace CSUserInterface
{


	public partial class MetUI : BaseView
	{

		public MetUI() : base()
		{
			//This call is required by the Windows Form Designer.
			InitializeComponent();

			//Add any initialization after the InitializeComponent() call

		}

		protected override void OnLoad()
		{
			MetGraphControl1.OnLoad(Controller, NodePath, Data.OuterXml);
			//Controller.Selection.Contents)
		}
		public override void OnRefresh()
		{
			string FileName = XmlHelper.Value(Data, "filename");

			MetGraphControl1.OnRefresh();
			HelpText = FileName;
			OpenFileDialog.InitialDirectory = Path.GetDirectoryName(FileName);
			MetGraphControl1.SetFileName(FileName);
		}

		public override void OnSave()
		{
			MetGraphControl1.OnSave();
			System.Xml.XmlDocument Doc = new System.Xml.XmlDocument();
			Doc.LoadXml(MetGraphControl1.GetData());
			Data.InnerXml = Doc.DocumentElement.InnerXml;
		}

		private void btnBrowse_Paint(object sender, System.Windows.Forms.PaintEventArgs e)
		{
			ControlPaint.DrawBorder3D(e.Graphics, e.ClipRectangle, Border3DStyle.Etched);
		}

		private void btnBrowse_Click(object sender, System.EventArgs e)
		{
			try {
				if (OpenFileDialog.ShowDialog() == DialogResult.OK) {
					MetGraphControl1.SetFileName(OpenFileDialog.FileName);
					this.HelpText = MetGraphControl1.GetFileName();
				}
			} catch (Exception ex) {
				MessageBox.Show(ex.Message, "Error", MessageBoxButtons.OK, MessageBoxIcon.Error);
			}

		}

	}
}
