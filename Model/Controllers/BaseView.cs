using Microsoft.VisualBasic;
using System;
using System.Collections;
using System.Collections.Generic;
using System.Data;
using System.Drawing;
using System.Diagnostics;
using System.Windows.Forms;
using System.IO;
using System.Collections.Specialized;
namespace Controllers
{

	// ----------------------------------
	// Base class for all user interfaces
	// It has data and knowledge of a
	// user interface.
	// ----------------------------------   
	public partial class BaseView : System.Windows.Forms.UserControl
	{

		protected BaseController _Controller;
		protected string MyNodePath;

		protected System.Xml.XmlNode Data;
		public delegate void NotifyEventHandler(BaseView NodeChanged);
		public event NotifyEventHandler ViewChanged;


		public BaseView() : base()
		{
			InitializeComponent();
			HelpText = "";
		}

        public void OnLoad(BaseController Controller, string NodePath, string Contents)
		{
			_Controller = Controller;
			MyNodePath = NodePath;

			System.Xml.XmlDocument Doc = new System.Xml.XmlDocument();
			Doc.LoadXml(Contents);
			Data = Doc.DocumentElement;
			OnLoad();
		}
		public string GetData()
		{
			if ((Data == null)) {
				return "";
			} else {
				return Data.OuterXml;
			}
		}
		public string NodePath {
			get { return MyNodePath; }
		}

		public BaseController Controller {
			get { return _Controller; }
		}


		protected virtual void OnLoad()
		{
			// ---------------------------------------------
			// An overridable method that is called whenever
			// the object has just been loaded.
			// ---------------------------------------------
		}

		public virtual void OnRefresh()
		{
			// ---------------------------------------------
			// An overridable method that is called whenever
			// the object needs to be refreshed.
			// ---------------------------------------------
		}

		public virtual void OnSave()
		{
			// ---------------------------------------------
			// An overridable method that is called whenever
			// data should be saved back to the APSIMData 
			// instance.
			// ---------------------------------------------
		}

        public virtual bool OnDropData(StringCollection SourcePaths, string FullXML)
		{
			// ---------------------------------------------
			// An overridable method that is called whenever
			// a Component tree node in the Factorial Tree
			// has data dropped on it. 
			// default behaviour is to return false and let
			// the FactorTree look after the logic
			// The Factor View needs the event to add Targets 
			// automatically when the first component is dropped
			// ---------------------------------------------
			return false;
		}

		protected void PublishViewChanged()
		{
			// ---------------------------------------------
			// This is used by the graph system.
			// ---------------------------------------------
			if (ViewChanged != null) {
				ViewChanged(this);
			}
		}

		public virtual void OnClose()
		{
			// ---------------------------------------------
			// An overridable method that is called whenever
			// the object is being destroyed.
			// ---------------------------------------------
		}

		public string HelpText {
			// ---------------------------------------------
			// Provide access to the help label of this ui
			// ---------------------------------------------
			get { return MyHelpLabel.Text; }
			set {
				MyHelpLabel.Text = value;
				MyHelpLabel.Visible = !string.IsNullOrEmpty(value);
				System.Drawing.Size s = new System.Drawing.Size(MyHelpLabel.Size.Width, 100);
				MyHelpLabel.Height = MyHelpLabel.GetPreferredSize(s).Height;
			}
		}


		public virtual void PrintPage(Rectangle MarginBounds, Graphics Graphs)
		{
		}


		public virtual void Export(string FileName)
		{
		}

        public virtual void ExportData(string exportFolder)
        {
        }

    }
}
