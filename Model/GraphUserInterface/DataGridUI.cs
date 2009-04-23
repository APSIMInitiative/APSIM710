
using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Text;
using System.Windows.Forms;

using Controllers;
using CSGeneral;

namespace GraphUserInterface
    {
    public partial class DataGridUI : BaseView
        {
        private uint DataWindow = 0;
        private ChartPageUI ParentUI;

        public DataGridUI()
            {
            InitializeComponent();
            }

        protected override void OnLoad()
            {
            ParentUI = (ChartPageUI)Parent;
            if (DataWindow == 0)
                DataWindow = ParentUI.Processor.CreateDataWindow(Handle);
            }

        public override void OnRefresh()
            {
            // -----------------------------------------------
            // Called when it's time to refresh
            // -----------------------------------------------
            base.OnRefresh();

            ParentUI.Processor.RefreshDataWindow(DataWindow, XmlHelper.Value(Data, "Source"));
            OnResize(null, null);
            }

        public override void OnClose()
            {
            if (DataWindow != 0)
                ParentUI.Processor.DeleteDataWindow(DataWindow);
            }

        private void OnResize(object sender, EventArgs e)
            {
            // ----------------------------------------------------------
            // User has resized window - resize our data window
            // ----------------------------------------------------------
            if (DataWindow != 0)
                {
                //GroupBox.Visible = false;
                ParentUI.Processor.SizeDataWindow(DataWindow, 10, 20, Width - 20, Height - 30);
                }
            }

        protected override bool IsInputKey(Keys keyData)
            {
            // ----------------------------------------------------------
            // Because we're hosting a non .NET grid we need to pass
            // arrow keys down into the control. Doesn't seem to happen
            // automatically for some reason.
            // ----------------------------------------------------------
            return true;
            //if (keyData == Keys.Down || keyData == Keys.Up ||
            //    keyData == Keys.Left || keyData == Keys.Right)
            //    return true;
            //else
            //    return base.IsInputKey(keyData);
            }

        }
    }

