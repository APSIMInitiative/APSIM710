
using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.IO;
using System.Text;
using System.Windows.Forms;
using System.Xml;

using Controllers;
using CSGeneral;
using UIUtility;


namespace GraphUserInterface
    {
    public partial class TabbedPageUI : BaseView
        {
        private DataProcessor DataProcessor;

        public TabbedPageUI()
            {
            InitializeComponent();
            }
        ~TabbedPageUI()
            {
            if (DataProcessor != null)
                {
                DataProcessor.Shutdown();
                DataProcessor = null;
                }
            }
        public DataProcessor Processor
            {
            get { return DataProcessor; }
            }

        protected override void OnLoad()
            {
            DataProcessor = new DataProcessor();

            TabControl.TabPages.Clear();
            foreach (XmlNode Page in XmlHelper.ChildNodes(Data, "page"))
                {
                TabPage NewTabPage = new TabPage(XmlHelper.Name(Page));
                ChartPageUI NewCanvas = new ChartPageUI();
                NewCanvas.Parent = NewTabPage;
                NewCanvas.Dock = DockStyle.Fill;
                TabControl.TabPages.Add(NewTabPage);
                NewCanvas.Processor = DataProcessor;
                NewCanvas.OnLoad(Controller, XmlHelper.FullPath(Page), Page.OuterXml);
                NewCanvas.ViewChanged += OnViewChanged;
                }
            }
        public override void OnRefresh()
            {
            // -----------------------------------------------
            // Called when it's time to refresh the canvas and
            // everything on it.
            // -----------------------------------------------
            base.OnRefresh();
            foreach (TabPage Page in TabControl.TabPages)
                {
                foreach (ChartPageUI ChartPage in Page.Controls)
                    ChartPage.OnRefresh();
                }
            }
        public override void OnSave()
            {
            // -----------------------------------------------
            // Called when it's time to save everything back
            // to XML
            // -----------------------------------------------
            base.OnSave();
            string Contents = "";
            foreach (TabPage Page in TabControl.TabPages)
                {
                ChartPageUI Canvas = (ChartPageUI)Page.Controls[0];
                Canvas.OnSave();
                Contents += Canvas.GetData();
                }
            Data.InnerXml = Contents;
            }
        public override void OnClose()
            {
            foreach (TabPage Page in TabControl.TabPages)
                {
                ChartPageUI Canvas = (ChartPageUI)Page.Controls[0];
                Canvas.OnClose();
                }
            }
        private void OnViewChanged(BaseView ChangedNode)
            {
            OnSave();
            foreach (TabPage Page in TabControl.TabPages)
                {
                foreach (ChartPageUI ChartPage in Page.Controls)
                    if (ChartPage != ChangedNode)
                        ChartPage.OnRefresh();
                }

            }



        }
    }

