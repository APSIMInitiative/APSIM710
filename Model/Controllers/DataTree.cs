using Microsoft.VisualBasic;
using System;
using System.Collections;
using System.Collections.Generic;
using System.Data;
using System.Drawing;
using System.Diagnostics;
using System.Windows.Forms;
using System.Xml;
using System.Collections.Specialized;
using CSGeneral;
using ApsimFile;
namespace Controllers
{


	public class DataTree : TreeView
	{
		//DataTree inherits from TreeView NOT from BaseView, but it still uses BaseController as its go between to the Model.
		//ApsimUI only has 2 Views, BaseView and TreeView. TreeView is the parent of the DataTree, and BaseView is the parent of every other UI in ApsimUI.

		// ---------------------------------------------------
		// Tree control for visualising an ApsimFile
		// ---------------------------------------------------

		//create a new Context Menu for the page [right click anywhere on the page]
		private System.Windows.Forms.ContextMenuStrip PopupMenu = new System.Windows.Forms.ContextMenuStrip();
		private System.Windows.Forms.ContextMenuStrip PopupMenuRightDrag = new System.Windows.Forms.ContextMenuStrip();

        private BaseController Controller;
		private bool FirstTimeRename = false;
		private bool EnableNodeSelection = true;

		private bool DisplayNodeTypeInTree = false;

        private void InitializeComponent()
        {
            this.SuspendLayout();
            //
            //DataTree
            //
            this.ForeColor = System.Drawing.Color.Goldenrod;
            this.HideSelection = false;
            this.ResumeLayout(false);

        }

        public void OnLoad(BaseController Controller)
		{
			// ---------------------------------------------------
			// Set ourselves up.
			// ---------------------------------------------------

			Controller.ApsimData.ComponentChangedEvent += OnRefresh;
			Controller.ApsimData.FileNameChanged += OnFileNameChanged;
			Controller.SelectionChangedEvent += OnSelectionChanged;

			PathSeparator = "/";
			ImageList = Controller.ImageList("SmallIcon");

			this.ShowNodeToolTips = true;
			this.Controller = Controller;
			this.ContextMenuStrip = PopupMenu;
			//tell the datatree that its context menu is the popup menu

			//PopupMenu.TopLevel = False                              'tell the popup menu that it belongs to a parent control.
			//PopupMenu.Parent = Me                                   'tell the popup menu that it belongs to the datatree.

			Controller.ProvideToolStrip(PopupMenu, "ContextMenu");
			//initialise Context Menu for the page using the controller [popup when you right click anywhere on the page]

			//initialise Context Menu for the right mouse button drag [popup when you drop the node you were dragging with the right mouse]
			PopupMenuRightDrag.Items.Add("Copy Here");
			PopupMenuRightDrag.Items.Add("Move Here");
			PopupMenuRightDrag.Items.Add("Create Link Here");
			PopupMenuRightDrag.Items.Add(new ToolStripSeparator());
			PopupMenuRightDrag.Items.Add("Cancel");
            PopupMenuRightDrag.ItemClicked += PopupMenuRightDrag_ItemClicked;

			DisplayNodeTypeInTree = Configuration.Instance.Setting("DisplayNodeTypeInTree") == "Yes";
		}

        private TreeNode GetNodeFromPath(string ChildPath)
		{
			// --------------------------------------------------
			// Returns a tree node given a fullly delimited path.
			// --------------------------------------------------
			string name = null;
			string Path = ChildPath.Substring(1);
			TreeNode CurrentNode = null;
			while (!(string.IsNullOrEmpty(Path))) {
				int PosDelimiter = Path.IndexOf(PathSeparator);
				if (PosDelimiter != -1) {
					name = Path.Substring(0, PosDelimiter);
					Path = Path.Substring(PosDelimiter + 1);
				} else {
					name = Path;
					Path = "";
				}

				TreeNode ChildNode = null;
				if (CurrentNode == null) {
					if (Nodes.Count == 0) {
						return null;
					} else {
						ChildNode = Nodes[0];
					}
				} else {
					foreach (TreeNode ChildNode_loopVariable in CurrentNode.Nodes) {
						ChildNode = ChildNode_loopVariable;
						if (ChildNode.Text.ToLower() == name.ToLower()) {
							break; // TODO: might not be correct. Was : Exit For
						}
					}
				}
				CurrentNode = ChildNode;
				if ((CurrentNode != null)) {
					if (CurrentNode.Text.ToLower() != name.ToLower()) {
						CurrentNode = null;
					}
				}
				if ((CurrentNode == null)) {
					break; // TODO: might not be correct. Was : Exit Do
				}
			}

			return CurrentNode;
		}

		private string GetPathFromNode(TreeNode Node)
		{
			return PathSeparator + Node.FullPath;
			//just put an extra "/" in front of the node path, so "root/child" becomes "/root/child" (this is needed because our "Selected Path" root starts with a /, whereas the inbuilt node full path property does not start with a / at the root)
		}

		private void OnFileNameChanged(string FileName)
		{
			// ---------------------------------------------------------
			// The file name has changed so expand all folder nodes.
			// ---------------------------------------------------------
			if (Nodes.Count == 1) {
				TreeNode RootNode = Nodes[0];
				CollapseAll();
				RootNode.Expand();
				if (RootNode.Nodes.Count == 1) {
					if (RootNode.Nodes[0].Tag.ToString().ToLower() == "simulation") {
						// Count the number of paddocks.
						int NumPaddocks = 0;
						foreach (TreeNode Child in RootNode.Nodes[0].Nodes) {
							if ((string)Child.Tag == "area") {
								NumPaddocks = NumPaddocks + 1;
							}
						}
						if (NumPaddocks == 1) {
							foreach (TreeNode Child in RootNode.Nodes[0].Nodes) {
								Child.Expand();
							}
						}
					}
					RootNode.Nodes[0].Expand();
				}
				Controller.SelectedPath = "";
				Controller.SelectedPath = Controller.ApsimData.RootComponent.FullPath;
			}
		}
		private void ExpandAllFolders(TreeNode Node)
		{
			if (Node.Tag.ToString().ToLower() == "folder") {
				bool ThereAreSubFolders = false;
				foreach (TreeNode Child in Node.Nodes) {
					if (Child.Tag.ToString().ToLower() == "folder") {
						ThereAreSubFolders = true;
						break; // TODO: might not be correct. Was : Exit For
					}
				}
				if (ThereAreSubFolders) {
					Node.Expand();
					foreach (TreeNode Child in Node.Nodes) {
						ExpandAllFolders(Child);
					}
				}
			}
		}




		#region "Refresh methods"

		private void OnRefresh(ApsimFile.Component Comp)
		{
			// ----------------------------------------------
			// Do a refresh from the specified Comp down
			// ----------------------------------------------
			System.Windows.Forms.Cursor.Current = Cursors.WaitCursor;
			//set the cursor object (usually an arrow) to the wait cursor (usually an hourglass)
			BeginUpdate();
			//inbuilt tree function, it disables redrawing of the tree


			try {
				//If (the tree has no nodes) OR (Comp [the component parameter this sub was passed] is Null)
				if ((Nodes.Count == 0) || (Comp == null)) {
					Nodes.Clear();
					//get rid of all the nodes in the tree 
					TreeNode RootNode = Nodes.Add(Controller.ApsimData.RootComponent.Name);
					//create the root node from the root component and add it to the tree.
					RefreshNodeAndChildren(RootNode, Controller.ApsimData.RootComponent);
					//refresh the tree from the root node down.

				//Get the node you want to refresh 

				} else {
					TreeNode NodeToRefresh = GetNodeFromPath(Comp.FullPath);
					//get the corresponding node for the component this sub was passed
					//if you have switched from one toolbox to another toolbox, then even though the components exist to do the refresh, the corresponding nodes do not yet exist because this OnRefresh is supposed to provide them. So GetNodeFromPath will return Nothing.
					if ((NodeToRefresh == null)) {
						RefreshNodeAndChildren(Nodes[0], Controller.ApsimData.RootComponent);
						//refresh the tree from this node down.
					} else {
						RefreshNodeAndChildren(NodeToRefresh, Comp);
						//refresh the tree from this node down.
					}

				}
			} catch (Exception) {
				EndUpdate();
				//inbuilt tree function, reinables redrawing of the tree
				System.Windows.Forms.Cursor.Current = Cursors.Default;
				//set the cursor object back to the default windows cursor (usually an arrow)
				throw;
			}


			//If multiple nodes are selected set the tree's selected node to the first one.

			//NB. Windows inbuilt Tree control does not allow you to select more then one node.
			//   So we had to create our own code to manually highlight the nodes that are selected. (manually changed the background/foreground properties of each node in the tree) (see ColourNode sub in the 'Selection methods' Region) 
			//   This is also why we needed to create the SelectedPaths property in the Base Control, instead of just using the tree controls inbuilt SelectedNode property.

			if ((Controller.SelectedPaths.Count > 0)) {
				EnableNodeSelection = false;
				//don't let the user click any other nodes while this code executes
				SelectedNode = GetNodeFromPath(Controller.SelectedPaths[0]);
				//set tree's selected node property to the first item in the SelectedPaths. The tree control complains if you don't have something set as the SelectedNode, but we don't use it.
				EnableNodeSelection = true;
				//let the user click on other nodes again

			}


			EndUpdate();
			//inbuilt tree function, reinables redrawing of the tree
			System.Windows.Forms.Cursor.Current = Cursors.Default;
			//set the cursor object back to the default windows cursor (usually an arrow)
		}
		private void RefreshNodeAndChildren(TreeNode Node, ApsimFile.Component Comp)
		{
			// --------------------------------------------------
			// Recursively refresh the specified treenode and its
			// child nodes in the tree.
			// --------------------------------------------------

			// Refresh the specified node first.
			Node.Text = Comp.Name;
			Node.ImageIndex = Controller.ImageIndex(Comp.Type, "SmallIcon");
			Node.SelectedImageIndex = Node.ImageIndex;
			Node.Tag = Comp.Type;
			if (DisplayNodeTypeInTree) {
				Node.ToolTipText = Comp.Type;
			} else {
				Node.ToolTipText = Comp.Description;
			}
			if ((Comp.ShortCutTo != null)) {
				Node.ToolTipText = "Linked to " + Comp.ShortCutTo.FullPath;
				if (!Comp.Enabled) {
					Node.ToolTipText = "Disabled: " + Node.ToolTipText;
				}
			}
			if (!Comp.Enabled) {
				Node.ToolTipText = "Disabled" + Node.ToolTipText;
			}
			ColourNode(Node);
			// Go refresh all children.
			int ChildIndex = 0;
			foreach (ApsimFile.Component Child in Comp.ChildNodes) {
				if (Child.Type != "factorial") {
					TreeNode ChildTreeNode = null;
					if (ChildIndex < Node.Nodes.Count) {
						ChildTreeNode = Node.Nodes[ChildIndex];
					} else {
						ChildTreeNode = Node.Nodes.Add(Child.Name);
					}
					RefreshNodeAndChildren(ChildTreeNode, Child);
					ChildIndex = ChildIndex + 1;
				}
			}
			while (Node.Nodes.Count > ChildIndex) {
				Node.Nodes.Remove(Node.Nodes[Node.Nodes.Count - 1]);
			}
		}
		#endregion




		#region "Selection methods"

		private TreeNode PreviousNode;
		private void OnSelectionChanged(StringCollection OldSelections, StringCollection NewSelections)
		{
			// -----------------------------------------------------------------
			// Selection has changed - update tree.
			// -----------------------------------------------------------------

			//NB. Windows inbuilt Tree control does not allow you to select more then one node.
			//   So we had to create our own code to manually highlight the nodes that are selected. (manually changed the background/foreground properties of each node in the tree) (see the ColourNode sub below) 
			//   This is also why we needed to create the SelectedPaths property in the Base Control, instead of just using the tree controls inbuilt SelectedNode property.


			EnableNodeSelection = false;
			//don't let the user click any other nodes while this code executes

			//Change the colour of all the old selected nodes to the "unselected" colours
			foreach (string NodePath in OldSelections) {
				TreeNode Node = GetNodeFromPath(NodePath);
				//get the node that the old selected path points to
				if ((Node != null)) {
					ColourNode(Node);
					//change the colour of the unselected node to the unselected colours.
				}
			}

			//Change the colour of all the new selected nodes to the "selected" colours

			foreach (string NodePath in NewSelections) {
				TreeNode Node = GetNodeFromPath(NodePath);
				//get the node that the new selected path points to.
				if ((Node != null)) {
					SelectedNode = Node;
					//set the Tree's selected node to the node specified in the new selected path (just used to trigger the AfterSelect event, which is handled by OnTreeSelectionChanged() subroutine below this subroutine) (nb. we REDO this for EVERY node in NewSelections. We have to do this one node at a time because the Tree does not allow you to select more then one node) 
					ColourNode(Node);
					//change the colour of the new selected node to the selected colours.
					Node.EnsureVisible();
					//use inbuilt tree node function that expands the tree to make sure the node specified is visible in the tree.  
				}
			}

			EnableNodeSelection = true;
			//let the user click on other nodes again
		}
		private void OnTreeSelectionChanged(object Sender, TreeViewEventArgs e)
		{
			if (EnableNodeSelection) {
				Controller.SelectedPath = GetPathFromNode(e.Node);
			}
		}

		#endregion

		private void ColourNode(TreeNode Node)
		{

     		Font LinkFont = new System.Drawing.Font(this.Font.FontFamily, this.Font.Size, FontStyle.Underline);
	    	Font UnLinkFont = new System.Drawing.Font(this.Font.FontFamily, this.Font.Size, FontStyle.Regular);

			//NB. Windows inbuilt Tree control does not allow you to select more then one node.
			//   So we had to create our own code to manually highlight the nodes that are selected. (manually changed the background/foreground properties of each node in the tree) (see the code below) 
			//   This is also why we needed to create the SelectedPaths property in the Base Control, instead of just using the tree controls inbuilt SelectedNode property.

			//If the node is linked to another node  
			//nb. ToolTipText is the text that appears when you hover the mouse over the node. IndexOf just returns the index of the first occurance of one string in another.
			if (Node.ToolTipText.IndexOf("Linked to") == 0) {
				Node.ForeColor = Color.Blue;
				//colour to blue
				Node.NodeFont = LinkFont;
				//font to underlined (see LinkFont variable declared just above this ColourNode function)
				Node.BackColor = BackColor;
				//back colour to default back colour for the tree

			//If the node is disabled 

			} else if (Node.ToolTipText.IndexOf("Disabled") == 0) {
				Node.ForeColor = SystemColors.InactiveCaptionText;
				//colour to the default colour for a windows system disabled element 
				Node.BackColor = SystemColors.InactiveCaption;
				//back colour to the default back colour for a disabled item in windows 

			//If it's just a normal node
			} else {
				Node.ForeColor = Color.Black;
				//colour to black
				Node.BackColor = BackColor;
				//back colour to default back colour for the tree
				Node.NodeFont = UnLinkFont;
				//font to regular (see UnLinkFont variable declared just above this ColourNode function)


			}

			//If the node is a selected node
			//this IndexOf is for a string collection NOT a string. So it it checks every string in the collection for an exact match with the search string. If it finds one it returns that strings index in the collection.
			if (Controller.SelectedPaths.IndexOf(GetPathFromNode(Node)) != -1) {
				Node.ForeColor = SystemColors.HighlightText;
				//colour to the default colour for a selected item in windows
				Node.BackColor = SystemColors.Highlight;
				//back colour to the default back colour for a selected item in windows

			}
		}




		#region "Rename methods"        'This is a Rename done by doing 2 seperate clicks (See "Left Click Only" code in TreeView_MouseDown). NOT by a right mouse click then selecting rename, this is handled by Rename() sub in BaseAction.vb



		//event handlers for a Node.BeginEdit()

		private void OnBeforeEdit(object sender, System.Windows.Forms.NodeLabelEditEventArgs e)
		{
			// ---------------------------------------------------
			// User is about to start editing a tree node.
			// We must disable the popup menu because if the user
			// hits DELETE while editing the node, the ACTION
			// will trigger, deleting the whole node rather than
			// the bit of text on the node caption.
			// ---------------------------------------------------
			//PopupMenu.Enabled = False
			this.ContextMenuStrip.Enabled = false;

		}
		private void OnAfterEdit(object sender, System.Windows.Forms.NodeLabelEditEventArgs e)
		{
			// ---------------------------------------------------
			// User has just finished editing the label of a node.
			// ---------------------------------------------------

			if (!FirstTimeRename) {

				if ((e.Label != null)) {
					//Check user typed something in. So you are not trying to rename it to a blank.
					if ((e.Label.Length > 0)) {


						if (!(CSGeneral.Utility.CheckForInvalidChars(e.Label))) {
							// Firstly empty the current selections.
							Controller.SelectedPath = "";

							// Change the data. 
							ApsimFile.Component Comp = Controller.ApsimData.Find(GetPathFromNode(e.Node));
							string oldName = Comp.Name;
							Comp.Name = e.Label;

							// The component may baulk at this change and suggest something else. If so, give up.
							if (Comp.Name != e.Label) {
								Comp.Name = oldName;
								MessageBox.Show("You can not have two components with the same name (" + e.Label + ")");
								e.CancelEdit = true;
							} else {
								// Now tell the base controller about the new selections.
								Controller.SelectedPath = Comp.FullPath;
							}

						} else {
							MessageBox.Show("You can not use characters such as < > / \\ ' \" ` : ? | * & = ! . or space in the name");
							e.CancelEdit = true;
							//cancel the edit event.

						}

					} else {
						e.CancelEdit = true;
						//cancel the edit event.
					}
				}
				LabelEdit = false;
			}
			FirstTimeRename = false;
			//PopupMenu.Enabled = True
			this.ContextMenuStrip.Enabled = true;
		}

		#endregion




		#region "Drag / Drop methods"       'These events handle a drag with both the left mouse button and the right mouse button

		//Global variables for Drag/Drop methods
			//used to store the paths for all the components that have been selected in the drag
		public StringCollection PathsBeingDragged;
			//is the drag event a drag using the right mouse button
		private bool isRightBtnDrag = false;


		private void TreeView_ItemDrag(object sender, System.Windows.Forms.ItemDragEventArgs e)
		{
			// -----------------------------------------------------------------
			// User has initiated a drag on a node - the full xml of the node
			// is stored as the data associated with the drag event args.
			// -----------------------------------------------------------------

			//If what is being dragged is not already in the controller as a selected item.
			if (Controller.SelectedPaths.IndexOf(GetPathFromNode((TreeNode)e.Item)) == -1) {
				SelectedNode = (TreeNode)e.Item;
				//add it to the base controller (by setting the selected node of the tree to the dragged item. This then fires the selection changed event for the tree which I think is handled by the base controller. This will add the dragged items to the base controller)
			}

			//Work out the xml of what you are dragging.
			string FullXML = "";
			//used to store the xml of ALL the components that have been selected in the drag  'reset it to nothing, ready for recreation.
			//get the full xml of all the selected nodes that are getting dragged
			foreach (string SelectedPath in Controller.SelectedPaths) {
				ApsimFile.Component Comp = Controller.ApsimData.Find(SelectedPath);
				//get the component for this particular selected node (using it's path)
				FullXML = FullXML + Comp.FullXML();
				//get the xml for the component and add it to the xml of previous selected nodes
			}
			PathsBeingDragged = Controller.SelectedPaths;
			//store the paths of ALL the nodes that are being dragged in a global variable, so it can be used by other drag events.

			//Raise the other DragDropEvents
			DoDragDrop(FullXML, DragDropEffects.Copy | DragDropEffects.Move | DragDropEffects.Link);
			//parameters: (Store xml of what you are dragging in "data" Drag Event Argument), (allowable types of left mouse drags [Drag Drop Effects are of type FlagsAttribute, which allow bitwise operators AND and OR]). 

		}

		private void TreeView_DragOver(object sender, System.Windows.Forms.DragEventArgs e)
		{
			// --------------------------------------------------
			// User has dragged a node over us - allow drop?
			// --------------------------------------------------

			//Make sure you are actually dragging something
			//check the "data" Drag Event Argument
			if (e.Data.GetDataPresent(typeof(System.String))) {

				//If the mouse is currently dragging over a node and not over blank area
				Point pt = PointToClient(new Point(e.X, e.Y));
				//get the drop location
				TreeNode DestinationNode = GetNodeAt(pt);
				//find the node closest to the drop location

				if ((DestinationNode != null)) {
					//Work out the type of left drag this is (copy, move, create link/shortcut), and store it in the "Effect" Drag Event Argument
					string FullXML = (string)e.Data.GetData(DataFormats.Text);
					ApsimFile.Component DropComp = Controller.ApsimData.Find(GetPathFromNode(DestinationNode));
					//get the corresponding component for the destination node.
					//if allowed to drop this node onto this destination node
					if (DropComp.AllowAdd(FullXML)) {
						if ((PathsBeingDragged != null) && PathsBeingDragged.Count > 0 && (Control.ModifierKeys & Keys.Shift) == Keys.Shift) {
							e.Effect = DragDropEffects.Move;
							//these DragDropEffects are just the changes to the mouse icon when you hover over a node whilst dragging
						} else if ((PathsBeingDragged != null) && (PathsBeingDragged.Count > 0) && (Control.ModifierKeys & Keys.Alt) == Keys.Alt) {
							e.Effect = DragDropEffects.Link;
						} else {
							e.Effect = DragDropEffects.Copy;
						}
					//if NOT allowed to drop this node onto this destination node
					} else {
						e.Effect = DragDropEffects.None;
						//display circle with line through it symbol
					}

					//If this is a right mouse drag, set the global variable flag
					//you have to do this test in "DragOver" event because the DragEventArgs disappear once the "DragDrop" event occurs [even thought it has a DragEventArgs parameter]
					if ((e.KeyState == 2)) {
						isRightBtnDrag = true;
						//set the flag to true
					}

				}
			}
		}


		private void TreeView_DragDrop(object sender, System.Windows.Forms.DragEventArgs e)
		{
			// --------------------------------------------------
			// User has released mouse button during a drag.
			// Accept the dragged node.
			// --------------------------------------------------

			//Get the Destination Node
			Point pt = ((TreeView)sender).PointToClient(new Point(e.X, e.Y));
			//get the drop location
			TreeNode DestinationNode = ((TreeView)sender).GetNodeAt(pt);
			//find the node closest to the drop location
			Controller.SelectedPath = GetPathFromNode(DestinationNode);
			//set the selected path for the controller to the path for the destination node

			//Get the xml of what was dragged.
			string FullXML = (string)e.Data.GetData(DataFormats.Text);
			//it was put into the "data" Drag Event Argument in the DoDragDrop call (in TreeViewDragItem event handler)  


			//Drag using the right mouse button

			if (isRightBtnDrag) {
				//Display the popup menu to let the user select the drag action

				PopupMenuRightDrag.Tag = FullXML;
				//use this as a dodgy way of passing the xml of what was dragged to the "Right Drag Context Menu" ItemClicked event.  
				PopupMenuRightDrag.Show(this, pt);
				//display the context menu at the place user dropped the node(s) at
				isRightBtnDrag = false;
				//reset the flag back to false 


			} else {
				//Drag using the left mouse button

				//depending on the type of left mouse drag, do the corresponding drag action.
				switch (e.Effect) {
					//use the "Effect" Drag Event Argument to get the type of left mouse drag
					case DragDropEffects.Copy:
						//these DragDropEffects are just the changes to the mouse icon when you hover over a node whilst dragging
						DragCopy(FullXML);
						break;
					case DragDropEffects.Move:
						DragMove(FullXML);
						break;
					case DragDropEffects.Link:
						DragLink();
						break;
					case DragDropEffects.None:
						//for when the user drags to somewhere that does not allow a drag to.
						DragCancel();
						break;
					default:
						//should not needed this, but just put it in to prevent errors.
						DragCancel();
						break;
				}

			}
		}

		// "Right Drag Context Menu" Click Event.


		private void PopupMenuRightDrag_ItemClicked(object sender, ToolStripItemClickedEventArgs e)
		{
			//Get what was dragged. 
			string FullXML = Convert.ToString(PopupMenuRightDrag.Tag);
			//it was put into the Tag property of the right mouse click popup menu in the "DragDrop" event (just before the right click popup menu is displayed) 


			//Get what option in popup menu was clicked, and do the drag action
			switch (e.ClickedItem.Text) {
				//what did the user select from the Context Menu of a right drag.
				case "Copy Here":
					DragCopy(FullXML);
					break;
				case "Move Here":
					DragMove(FullXML);
					break;
				case "Create Link Here":
					DragLink();
					break;
				case "Cancel":
					DragCancel();
					break;
				default:
					//should not needed this, but just put it in to prevent errors.
					DragCancel();
					break;
			}


		}

		//Drag Actions

		//copy
		private void DragCopy(string FullXML)
		{
			Controller.Selection.Add(FullXML);
			//add the dragged nodes to the destination node (destination node is stored as the current selection in the controller) 
		}
		//move
		private void DragMove(string FullXML)
		{
			Controller.Selection.Add(FullXML);
			//add the dragged nodes to the destination node (destination node is stored as the current selection in the controller) 
			//when you drag from datatree in simulation to datatree in a toolbox the PathsBeingDragged disappear and it causes an error. I have done this to avoid the error but moving does not really make sense in this situation anyway. Instead just copying makes more sense.
			if ((PathsBeingDragged != null)) {
				//delete all the dragged nodes from their original path.
				foreach (string DraggedPath in PathsBeingDragged) {
					ApsimFile.Component Comp = Controller.ApsimData.Find(DraggedPath);
					if ((Comp != null)) {
						Comp.Parent.Delete(Comp);
					}
				}
			}
		}
		//create link
		private void DragLink()
		{
			//when you drag from datatree in simulation to datatree in a toolbox the PathsBeingDragged disappear and it causes an error. I have done this to avoid the error but creating a link does not really make sense in this situation anywhere. Instead just doing nothing makes more sense.
			if ((PathsBeingDragged != null)) {
				//add all the dragged node as a link to the destination node
				foreach (string DraggedPath in PathsBeingDragged) {
					ApsimFile.Component Comp = Controller.ApsimData.Find(DraggedPath);
					if ((Comp != null)) {
						Controller.Selection.AddShortCut(Comp);
						//add one of the dragged nodes as a link to the destination node (destination node is stored as the current selection in the controller)
					}
				}
			}
		}
		//cancel
		private void DragCancel()
		{
			//when you drag from datatree in simulation to datatree in a toolbox the PathsBeingDragged disappear and it causes an error. I have done this to avoid the error.
			if ((PathsBeingDragged != null)) {
				PathsBeingDragged.Clear();
				//clear the variable storing the dragged nodes 
			}
		}

		#endregion




		#region "Mouse events"

		private void TreeView_MouseDown(object sender, System.Windows.Forms.MouseEventArgs e)
		{
			// ---------------------------------------------------------------
			// If the user right clicks on a node and that node isn't already
			// selected, then go select it.
			// ---------------------------------------------------------------


			//Initialise variables

			TreeNode ClickedNode = GetNodeAt(e.Location);
			//get the node in the datatree that was clicked on.
			//if the button was pressed on a non node area of the datatree
			if ((ClickedNode == null)) {
				return;
				//do nothing.
			}

			//allow for an image 20 pixels to the left of the text of a node
			if ((e.X < ClickedNode.Bounds.Left - 20)) {
				return;
			}

			//Don't let the user click any other nodes while this code executes

			EnableNodeSelection = false;

			bool RightClick = (e.Button == System.Windows.Forms.MouseButtons.Right);
			//is it a right mouse button click
			bool Control = (ModifierKeys == Keys.Control);
			//is control button on keyboard pressed 
			bool Shift = (ModifierKeys == Keys.Shift);
			//is shift button on keyboard pressed

			StringCollection SelectedPaths = Controller.SelectedPaths;
			//get the selected paths from the controller. (stores more then one nodes path because the user can hold down the control key and select more then one node)




			//Click with a Control button OR Right Click         'TODO: this may execute for CTRL + Centre Mouse Click too, you may want to specifically exclude this later.


			if (Control) {
				//check to see if the clicked node has been clicked on already 
				//this IndexOf is for a string "collection" NOT a string. So it it checks every string "in the collection" for an exact match with the search string. If it finds one it returns that strings index in the collection
				if (SelectedPaths.IndexOf(GetPathFromNode(ClickedNode)) == -1) {
					SelectedPaths.Add(GetPathFromNode(ClickedNode));
					//if not then add it's path to the list of selected paths
					PreviousNode = ClickedNode;
					//store clicked node as the previously clicked on node (used for Shift button)
				}



			//Click with a Shift button


			} else if (Shift) {
				//if (user has previously clicked on a node with the shift key down) AND (clicked node has the same parent as the previous node [they are siblings]) 

				if ((PreviousNode != null) && PreviousNode.Parent.Equals(ClickedNode.Parent)) {
					int FirstIndex = PreviousNode.Index;
					//set to the index of the previously clicked on node
					int LastIndex = ClickedNode.Index;
					//set to the index of the clicked node
					//if they clicked lower sibling before higher sibling, then rearrange the order they were clicked.
					if (FirstIndex > LastIndex) {

						int TempIndex = LastIndex;
						LastIndex = FirstIndex;
						FirstIndex = TempIndex;
					}

					SelectedPaths.Clear();
					//get rid of old selected paths
					//add the paths for all the node's between the first index and the last index, to the list of selected paths 
					for (int i = FirstIndex; i <= LastIndex; i++) {
						SelectedPaths.Add(GetPathFromNode(PreviousNode.Parent.Nodes[i]));
					}

					PreviousNode = ClickedNode;
					//store clicked node as the previously clicked on node (incase Shift button is used on the next click as well)

				}


			//(Do right and left click test after the multiple select tests of Control and Shift, so that Control and Shift can work for both Left or Right mouse clicks)

			//Right Click Only           


			} else if (RightClick) {
				SelectedPaths.Clear();
				//get rid of existing selected paths
				SelectedPaths.Add(GetPathFromNode(ClickedNode));
				//add new path to selected paths
				PreviousNode = ClickedNode;
				//store clicked node as the previously clicked on node (used for Shift button)


			//Left Click Only

			//if user clicked on a node and not a blank area
			} else if ((ClickedNode != null)) {

				//if the user clicked again on the same thing that was already selected -> then do a Rename.

				//click on same thing that was already selected.

				if (!Controller.ApsimData.IsReadOnly && SelectedPaths.Count == 1 && SelectedPaths[0] == GetPathFromNode(ClickedNode) && ClickedNode.Level > 0) {
					//if not readonly, 
					//and user has clicked once before, 
					//and what they clicked this time is the same as what they clicked last time, 
					//and they have clicked on a node that is lower down then the root node [can't rename the root node] 

					LabelEdit = true;
					//set the tree's label edit property  to true, allowing all the nodes on the tree to have their labels edited. (needs to be set to true for Node.BeginEdit() to work)  
					FirstTimeRename = true;
					ClickedNode.BeginEdit();
					//call the inbuilt tree node function that allows the user to edit the nodes label. (see OnBeforeEdit and OnAfterEdit sub for what happens before and after the user edits the label)
					EnableNodeSelection = true;
					return;

				//if they clicked on something different to what was already selected -> change the selection


				} else {
					SelectedPaths.Clear();
					//get rid of existing selected paths
					SelectedPaths.Add(GetPathFromNode(ClickedNode));
					//add new path to selected paths
					PreviousNode = ClickedNode;
					//store clicked node as the previously clicked on node (used for Shift button)

				}
			}


			//Finish off

			Controller.SelectedPaths = SelectedPaths;
			//update the selected paths "in the controller"


			//Let the user click on other nodes again

			EnableNodeSelection = true;

		}

		#endregion

		public DataTree()
		{
			MouseDown += TreeView_MouseDown;
			DragDrop += TreeView_DragDrop;
			DragOver += TreeView_DragOver;
			ItemDrag += TreeView_ItemDrag;
			AfterLabelEdit += OnAfterEdit;
			BeforeLabelEdit += OnBeforeEdit;
			AfterSelect += OnTreeSelectionChanged;
		}

	}
}
