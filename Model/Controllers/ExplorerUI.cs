using Microsoft.VisualBasic;
using System;
using System.Collections;
using System.Collections.Generic;
using System.Data;
using System.Drawing;
using System.Diagnostics;
using System.Windows.Forms;


using System.Collections.Specialized;
using System.IO;
using System.Xml;

using CSGeneral;
namespace Controllers
{

	public partial class ExplorerUI : UserControl
	{
		private ArrayList UIs = new ArrayList();
		private StringCollection UITypes = new StringCollection();
		private int CurrentUIIndex = -1;
		//this is very important. The base controller controls all the actions and events in ApsimUI. There is only one base controller variable for the entire ApsimUI and it gets passed around because it is needed to deal with clicks etc.
		private BaseController Controller;


		public ExplorerUI() : base()
		{
			InitializeComponent();
		}

        public void OnLoad(BaseController Controller)
		{
			this.Controller = Controller;
			//set the controller to "the" base controller for ApsimUI
			DataTree.OnLoad(Controller);
			FactorTree.OnLoad(Controller);
			Controller.SelectionChangedEvent += OnSelectionChanged;
			//ExplorerUI will handle a "Selection Changed" event (see OnSelectionChanged method for how)
			Controller.ApsimData.BeforeSave += OnBeforeSave;
			//ExplorerUI will handle a "Before Save" event (see OnBeforeSave method for how)

			Controller.FactorialSelectionChangedEvent += OnFactorialSelectionChanged;
			//ExplorerUI will handle a "Factorial Selection Changed" event (see OnFactorialSelectionChanged method for how)
			//initialise panels to avoid redrawing problem
			UIPanel.Dock = DockStyle.Fill;
			pnlHost.Dock = DockStyle.Fill;
			pnlHost.Visible = false;

		}


		public void ExpandAll()
		{
			DataTree.ExpandAll();
		}
		public void CollapseAll()
		{
			DataTree.CollapseAll();
		}
		private void ShowUI(string SelectedPath, ApsimFile.Component SelectedComponent)
		{
			// -------------------------------------------------
			// Create and show a specific UI depending on the
			// currently selected data
			// -------------------------------------------------
			//Dim SelectedData As ApsimFile.Component = Controller.ApsimData.Find(Controller.SelectedPath)
			if ((SelectedComponent != null)) {
				if (CurrentUIIndex == -1 || UITypes[CurrentUIIndex] != SelectedComponent.Type) {
					CloseUI();
					CurrentUIIndex = UITypes.IndexOf(SelectedComponent.Type);
					if (CurrentUIIndex == -1) {
						BaseView View = Controller.CreateUI(SelectedComponent.Type);
						if ((View != null)) {
							UIs.Add(View);
							UITypes.Add(SelectedComponent.Type);
							CurrentUIIndex = UIs.Count - 1;
						}
					}
				} else {
					SaveCurrentView();
				}
				if (CurrentUIIndex != -1) {
					try {
						BaseView View = (BaseView)UIs[CurrentUIIndex];
						View.OnLoad(Controller, SelectedPath, SelectedComponent.Contents);
						View.Parent = UIPanel;
						View.Dock = DockStyle.Fill;
						View.Show();
						View.OnRefresh();
					} catch (Exception ex) {
						MessageBox.Show(ex.Message, "Error", MessageBoxButtons.OK, MessageBoxIcon.Error);
					}
				}
			}
		}
		public void CloseUI()
		{
			// -------------------------------------------------
			// Close the current UI
			// -------------------------------------------------
			if (CurrentUIIndex != -1) {
				BaseView View = (BaseView)UIs[CurrentUIIndex];
				SaveCurrentView();
				View.OnClose();
				UIPanel.Controls.Remove(View);
				CurrentUIIndex = -1;
			}
		}
		public void SaveCurrentView()
		{
			// -----------------------------------------------------
			// Tell current view to save.
			// -----------------------------------------------------
			if (CurrentUIIndex != -1) {
				BaseView View = (BaseView)UIs[CurrentUIIndex];
				//if path has a delimiter at the beginning it is normal view
				//else it is a factorial view
				ApsimFile.Component Comp = null;
				//
				int pos = View.NodePath.IndexOf(ApsimFile.Component.Delimiter);
				if ((pos == 0)) {
					Comp = Controller.ApsimData.Find(View.NodePath);
				} else {
					Comp = Controller.FindFactorialComponent(View.NodePath);
				}
				if ((Comp != null)) {
					View.OnSave();
					Comp.Contents = View.GetData();
				}
			}
		}
		public void RefreshCurrentView()
		{
			if (CurrentUIIndex != -1) {
				BaseView View = (BaseView)UIs[CurrentUIIndex];
				View.OnLoad(Controller, Controller.SelectedPath, Controller.Selection.Contents);
				View.OnRefresh();
			}
		}
		public BaseView CurrentView {
			get {
				if (CurrentUIIndex != -1) {
					return (BaseView)UIs[CurrentUIIndex];
				} else {
					return null;
				}
			}
		}
		private void OnBeforeSave()
		{
			// -----------------------------------------------------
			// User is about to do a save.
			// -----------------------------------------------------
			if (Controller.SelectedPaths.Count == 1) {
				SaveCurrentView();
			}
		}
		private void OnSelectionChanged(StringCollection OldSelections, StringCollection NewSelections)
		{
			// -----------------------------------------------------
			// User has selected a node - update user interface
			// -----------------------------------------------------
			Visible = true;
			//make the ExplorerUI visible
			Cursor SavedCursor = System.Windows.Forms.Cursor.Current;
			//store the current cursor object (usually an arrow)
			System.Windows.Forms.Cursor.Current = Cursors.WaitCursor;
			//set the cursor object to the default cursor object used for waiting (usually an hourglass) 

			//Parent panel is either the main form, or the Component tab within Factorial
			//if Component tab not active then don't display the UI
			if (!Controller.FactorialMode) {
				UIPanel.Parent = pnlDisplayArea;

				//If there is only 1 node that is selected
				if (Controller.SelectedPaths.Count == 1) {
					ShowUI(Controller.SelectedPath, Controller.Selection);
					//show the corresponding UI for that node in the panel to the right of the tree
				} else {
					CloseUI();
					//show no UI in the panel to the right of the tree
				}
			}

			System.Windows.Forms.Cursor.Current = SavedCursor;
			//restore the cursor object to what it was before the wait cursor.
		}
		private void OnFactorialSelectionChanged(string OldSelection, string NewSelection)
		{
			// -----------------------------------------------------
			// User has selected a node - update user interface
			// -----------------------------------------------------
			Visible = true;
			//make the ExplorerUI visible
			Cursor SavedCursor = System.Windows.Forms.Cursor.Current;
			//store the current cursor object (usually an arrow)
			System.Windows.Forms.Cursor.Current = Cursors.WaitCursor;
			//set the cursor object to the default cursor object used for waiting (usually an hourglass) 

			UIPanel.Parent = Panel1;

			//If there is only 1 node that is selected
			if (!string.IsNullOrEmpty(NewSelection)) {
				ShowUI(Controller.SelectedFactorialPath, Controller.FactorialSelection);
				//show the corresponding UI for that node in the panel to the right of the tree
			} else {
				CloseUI();
				//show no UI in the panel to the right of the tree
			}

			System.Windows.Forms.Cursor.Current = SavedCursor;
			//restore the cursor object to what it was before the wait cursor.
		}
		public void RefreshDisplayMode()
		{
			OnBeforeSave();

			Visible = true;
			//make the ExplorerUI visible
			Cursor SavedCursor = System.Windows.Forms.Cursor.Current;
			//store the current cursor object (usually an arrow)
			System.Windows.Forms.Cursor.Current = Cursors.WaitCursor;
			//set the cursor object to the default cursor object used for waiting (usually an hourglass) 

			if (Controller.FactorialMode) {
				pnlHost.Visible = true;
				UIPanel.Parent = Panel1;
				if (string.IsNullOrEmpty(Controller.SelectedFactorialPath)) {
					Controller.SelectedFactorialPath = Controller.ApsimData.FactorComponent.FullPath;
				} else {
					OnFactorialSelectionChanged(Controller.SelectedFactorialPath, Controller.SelectedFactorialPath);
				}
			} else {
				UIPanel.Parent = pnlDisplayArea;
				pnlHost.Visible = false;
				OnSelectionChanged(Controller.SelectedPaths, Controller.SelectedPaths);
			}
			System.Windows.Forms.Cursor.Current = SavedCursor;
			//restore the cursor object to what it was before the wait cursor.
		}

		private void FactorTabControl_Selecting(System.Object sender, System.Windows.Forms.TabControlCancelEventArgs e)
		{
		}
	}
}
