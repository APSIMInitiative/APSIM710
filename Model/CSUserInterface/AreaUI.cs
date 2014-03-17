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
//using System.Convert;

using System.Xml;

using ApsimFile;
using CSGeneral;
using Controllers;
using UIUtility;
namespace CSUserInterface
{


	public partial class AreaUI : BaseView
	{

		public AreaUI() : base()
		{

			//This call is required by the Windows Form Designer.
			InitializeComponent();

			//Add any initialization after the InitializeComponent() call
		}

		// ----------------------------------
		// Refresh the listview
		// ----------------------------------
		public override void OnRefresh()
		{
            panelArea.Visible = Data.Name.ToLower() == "area";

			ListView.Clear();
			ListView.LargeImageList = Controller.ImageList("LargeIcon");

			// Add an item for all children of this system.
			ApsimFile.Component Comp = Controller.ApsimData.Find(NodePath);
			if ((Comp != null)) {
				foreach (ApsimFile.Component Child in Comp.ChildNodes) {
					if (Child.Type != "factorial") {
						//create new item
						ListViewItem item = new ListViewItem(Child.Name, 0);
						item.ImageIndex = Controller.ImageIndex(Child.Type, "LargeIcon");
						ListView.Items.Add(item);
					}
				}


				// Put up a background bitmap on listview.
				XmlNode BitmapNode = XmlHelper.Find(Data, "bitmap");
				if ((BitmapNode != null)) {
					string TempFileName = Path.Combine(Path.GetTempPath(), "apsimui.jpg");
					Bitmap b = CSGeneral.BitmapUtility.DecodeStringToBitmap(BitmapNode.Value);
					b.Save(TempFileName);
					UIUtility.ListViewAPI.SetListViewImage(ListView, TempFileName, UIUtility.ImagePosition.TopLeft);
				}
			}
            textBoxArea.Text = "1.0";  // Default to a 1 ha area if none is provided
            if (panelArea.Visible && Data.HasChildNodes)
            {
                foreach (XmlNode Child in Data.ChildNodes)
                {
                    if (Child.Name.ToLower() == "paddock_area")
                    {
                        textBoxArea.Text = Child.InnerText;
                        break;
                    }
                }
            }
		}

        /// <summary>
        /// Save all our changes back to Data
        /// </summary>
        public override void OnSave()
        {
            if (textBoxArea.Modified)
            {
                XmlNode newArea = Data.OwnerDocument.CreateElement("paddock_area");
                newArea.InnerText = textBoxArea.Text;
                double area;
                if (!Double.TryParse(textBoxArea.Text, out area))
                    MessageBox.Show("Cannot convert string: " + textBoxArea.Text + " to an area", "Error", MessageBoxButtons.OK, MessageBoxIcon.Error);
                else
                {
                    XmlNode areaNode = null;
                    foreach (XmlNode Child in Data.ChildNodes)
                    {
                        if (Child.Name.ToLower() == "paddock_area")
                        {
                            areaNode = Child;
                            break;
                        }
                    }
                    if (areaNode == null)
                        Data.AppendChild(newArea);
                    else
                        Data.ReplaceChild(newArea, areaNode);
                }
            }
        }

		// ---------------------------------------------------------
		// User has double clicked an item - show user interface
		// for that item.
		// ---------------------------------------------------------
		private void ListView_DoubleClick(object sender, System.EventArgs e)
		{
			Controller.SelectedPath = NodePath + "/" + ListView.SelectedItems[0].Text;
		}


		// ------------------------------------------------
		// User has selected an item on the 
		// context menu.
		// ------------------------------------------------
		private void MenuItem1_Click(System.Object sender, System.EventArgs e)
		{
			if (OpenFileDialog.ShowDialog() == DialogResult.OK) {
				string FileName = OpenFileDialog.FileName;
				UIUtility.ListViewAPI.SetListViewImage(ListView, FileName, UIUtility.ImagePosition.TopLeft);

				XmlNode BitmapNode = XmlHelper.Find(Data, "bitmap");
				if ((BitmapNode == null)) {
					BitmapNode = XmlHelper.CreateNode(Data.OwnerDocument, "bitmap", "bitmap");
				}

				Bitmap b = new Bitmap(FileName);
				BitmapNode.Value = CSGeneral.BitmapUtility.EncodeBitmapToString(b);
				Data.AppendChild(BitmapNode);

			}
		}


		// --------------------------------------------------------
		// User is trying to initiate a drag - allow drag operation
		// --------------------------------------------------------
		private void ListView_ItemDrag(object sender, System.Windows.Forms.ItemDragEventArgs e)
		{
			ApsimFile.Component Comp = Controller.ApsimData.Find(NodePath);
			ApsimFile.Component SelectedComp = Comp.Find(ListView.SelectedItems[0].Text);
			string DataString = SelectedComp.FullXML();
			ListView.DoDragDrop(DataString, DragDropEffects.All);
		}


		// --------------------------------------------------
		// User is dragging an item
		// --------------------------------------------------
		private void ListView_DragEnter(object sender, System.Windows.Forms.DragEventArgs e)
		{
			if ((e.KeyState & 5) == 5) {
				e.Effect = DragDropEffects.Move;
			} else {
				e.Effect = DragDropEffects.Copy;
			}
		}


		// -------------------------------------------------
		// User has dropped selected items.
		// -------------------------------------------------
		private void ListView_DragDrop(object sender, System.Windows.Forms.DragEventArgs e)
		{
			//Convert the mouse coordinates to client coordinates.
			Point p = ListView.PointToClient(new Point(e.X, e.Y));

			if (e.Effect == DragDropEffects.Copy) {
				string NewDataString = (string)e.Data.GetData(DataFormats.Text);
				XmlDocument Doc = new XmlDocument();
				Doc.LoadXml(NewDataString);
				Data.AppendChild(Doc.DocumentElement);
				OnRefresh();
			} else {
				foreach (ListViewItem item in ListView.SelectedItems) {
					UIUtility.ListViewAPI.SetItemPosition(ListView, ListView.SelectedItems[0].Index, p.X, p.Y);
					XmlNode child = XmlHelper.Find(Data, item.Text);
					XmlHelper.SetAttribute(child, "x", p.X.ToString());
					XmlHelper.SetAttribute(child, "y", p.Y.ToString());
				}
			}
		}

		private void ListView_DragOver(object sender, System.Windows.Forms.DragEventArgs e)
		{
			string FullXML = (string)e.Data.GetData(DataFormats.Text);
			if (Controller.Selection.AllowAdd(FullXML)) {
				if ((e.KeyState & 5) == 5) {
					e.Effect = DragDropEffects.Move;
				} else {
					e.Effect = DragDropEffects.Copy;
				}
			} else {
				e.Effect = DragDropEffects.None;
			}

		}

		private void ListView_KeyDown(object sender, System.Windows.Forms.KeyEventArgs e)
		{
		}

	}
}
