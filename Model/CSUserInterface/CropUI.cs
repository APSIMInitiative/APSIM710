using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Text;
using System.Windows.Forms;
using System.Xml;
using QWhale;
using CSGeneral;


namespace CSUserInterface
{
    public partial class CropUI : Controllers.BaseView
    {
        //private bool IsShortcut = false;                        //if selectednode points to another node
        private ApsimFile.Component Simulation = null;          //Simulation node that the selected node belongs to - if Root or Toolbox is true this will be null
        //private bool IsRoot = false;                            //not inside a simulation (includes links)

        public CropUI()
        {
            InitializeComponent();
        }
        protected override void OnLoad()
        { }
        public override void OnRefresh()
        {
            Simulation = findCurrentSimulation();
            string stmp = MyNodePath;
            
            //What
            UpdateStatesList();
            UpdateCropsList();
            
            //Planting Criteria
            txtStart.Text = ReadUIVariable("date1");
            txtEnd.Text = ReadUIVariable("date2");
            txtRain.Text = ReadUIVariable("raincrit");
            txtDaysRain.Text = ReadUIVariable("rainnumdays");
            txtMinWater.Text = ReadUIVariable("esw_amount");

            string sResult = ReadUIVariable("must_sow");
            chkSow.Checked = sResult == "yes";
            sResult = ReadUIVariable("wait_machinery");
            chkMachinery.Checked = sResult == "yes";

            //Establishment
            UpdateCultivarsList();
            txtDensity.Text = ReadUIVariable("plants");
            txtDepth.Text = ReadUIVariable("sowing_depth");
            txtRows.Text = ReadUIVariable("row_spacing");
            txtSowingcost.Text = ReadUIVariable("sowing_costs");
            //  machinery
            UpdateMachineryList(cboTractor, lblTractorWarning, "tractor", "sow_tractor");
            UpdateMachineryList(cboImplement, lblImplementWarning, "implement", "sow_implement");
            //UpdateTractorsList();
            //UpdateImplementsList();

            //  fertiliser
            UpdateFertiliserList();
            txtFertiliserCost.Text = ReadUIVariable("fert_cost");
            txtNitrogen.Text = ReadUIVariable("target_n");

            //Harvesting
            txtHarvestCost.Text = ReadUIVariable("harvest_costs");
            txtCropPrice.Text = ReadUIVariable("price");
            txtMoisture.Text = ReadUIVariable("moisture");
            UpdateMachineryList(cboHarvestTractor, lblHarvestTractorWarning, "tractor", "harv_tractor");
            UpdateMachineryList(cboHarvestImplement, lblHarvestImplementWarning, "implement", "harv_implement");

            LoadCustom();

            XmlNode ScriptNode = Data.SelectSingleNode("//script/text");
            string sScript = "";
            if(ScriptNode != null)
                sScript = XmlHelper.Value(ScriptNode, "");
            syntaxEdit1.Text = sScript;
        }
        private void LoadCustom()
        {
            XmlNode UINode = XmlHelper.Find(Data, "CustomUI");
            if(UINode != null)
            {
                GenericUI.OnLoad(Controller, NodePath, UINode.OuterXml);
                GenericUI.OnRefresh();
            }
        }
        public override void OnSave()
        {
            Data.RemoveAll();

            UpdateUIVariable("state", cboState.Text);
            UpdateUIVariable("crop", cboCrops.Text);
            UpdateUIVariable("date1", txtStart.Text);
            UpdateUIVariable("date2", txtEnd.Text);
            UpdateUIVariable("raincrit", txtRain.Text);
            UpdateUIVariable("rainnumdays", txtDaysRain.Text);
            UpdateUIVariable("esw_amount", txtMinWater.Text);

            string sText = "no";
            if (chkSow.Checked)
                sText = "yes";
            UpdateUIVariable("must_sow", sText);
            sText = "no";
            if (chkMachinery.Checked)
                sText = "yes";
            UpdateUIVariable("wait_machinery", sText);

            //Establishment
            UpdateUIVariable("cultivar", cboCultivar.Text);
            UpdateUIVariable("plants", txtDensity.Text);
            UpdateUIVariable("sowing_depth", txtDepth.Text);
            UpdateUIVariable("row_spacing", txtRows.Text);
            UpdateUIVariable("sowing_costs", txtSowingcost.Text);

            //Machinery
            UpdateUIVariable("sow_tractor", cboTractor.Text);
            UpdateUIVariable("sow_implement", cboImplement.Text);

            //Fertiliser
            UpdateUIVariable("fert_type", cboFertiliser.Text);
            UpdateUIVariable("fert_cost", txtFertiliserCost.Text);
            UpdateUIVariable("target_n", txtNitrogen.Text);

            //Harvest
            UpdateUIVariable("harvest_costs", txtHarvestCost.Text);
            UpdateUIVariable("price", txtCropPrice.Text);
            UpdateUIVariable("moisture", txtMoisture.Text);
            UpdateUIVariable("harv_tractor", cboHarvestTractor.Text);
            UpdateUIVariable("harv_implement", cboHarvestImplement.Text);

            //Custom
            GenericUI.OnSave();
            //need to add the saved data into this component's xml 
            string savedData = GenericUI.GetData();
            ReplaceNode("CustomUI", savedData);

            XmlNode ScriptNode = Data.AppendChild(Data.OwnerDocument.CreateElement("script"));
            XmlHelper.SetName(ScriptNode, "Init");
            XmlHelper.SetValue(ScriptNode, "text", syntaxEdit1.Text);
            XmlHelper.SetValue(ScriptNode, "event", "init");

        }
        private void ReplaceNode(string sName, string savedData)
        {
            if (savedData != "")
            {
                //this overwrites the fields within the CustomUI node
                //these values need to becopied into the ui node to allow the ui to access it fully
                //ie: ui node values are used to find & replace
                //in order to separate the custom nodes from the rest, they must be stored in another node
                XmlDocument tmpDoc = new XmlDocument();
                tmpDoc.LoadXml(savedData);

                XmlNode NodeToUpdate = XmlHelper.Find(Data, sName);
                if (NodeToUpdate == null)
                    NodeToUpdate = Data.AppendChild(Data.OwnerDocument.CreateElement(sName));

                NodeToUpdate.InnerXml = tmpDoc.DocumentElement.InnerXml;

                //append nodes to ui node
                XmlNode uiNode = Data.SelectSingleNode("//ui");
                if (uiNode == null)
                    uiNode = Data.AppendChild(Data.OwnerDocument.CreateElement("ui"));
                uiNode.InnerXml = uiNode.InnerXml + tmpDoc.DocumentElement.InnerXml;
            }
        }
        public string ReadUIVariable(string sVar)
        {
            XmlNode varNode = Data.SelectSingleNode("//ui/" + sVar);
            if (varNode != null)
            {
                return varNode.InnerText;
            }
            return "";
        }
        public void UpdateUIVariable(string sVar, string sVal)
        {
            XmlNode uiNode = Data.SelectSingleNode("//ui");
            if (uiNode == null)
                uiNode = Data.AppendChild(Data.OwnerDocument.CreateElement("ui"));

            XmlNode varNode = uiNode.SelectSingleNode("//" + sVar);
            if (varNode == null)
                varNode = uiNode.AppendChild(Data.OwnerDocument.CreateElement(sVar));
            varNode.InnerText = sVal;
        }
        public void UpdateStatesList()
        {
            string currentState = cboState.Text;
            cboState.Items.Clear();
            List<string> states = findStates();
            foreach (string state in states)
            {
                cboState.Items.Add(state);
            }
            //read ui variable value
            string vState = ReadUIVariable("state");
            cboState.Text = vState;
            if (vState != "")
            {
                int idx = cboState.Items.IndexOf(vState);
                lblStateWarning.Visible = idx == -1;
            }
            else
            {
                lblStateWarning.Visible = false;
            }
        }
        public void UpdateCropsList()
        {
            string currentCrop = cboCrops.Text;

            cboCrops.Items.Clear();
            List<string> crops = findCrops();
            foreach (string crop in crops)
            {
                cboCrops.Items.Add(crop);
            }
            string vCrop = ReadUIVariable("crop");
            cboCrops.Text = vCrop;
            if (vCrop != "")
            {
                int idx = cboCrops.Items.IndexOf(vCrop);
                lblCropWarning.Visible = idx == -1;
            }
            else
            {
                lblCropWarning.Visible = false;
            }
        }
        public void UpdateCultivarsList()
        {
            cboCultivar.Items.Clear();
            if (cboCrops.Text != "")
            {
                List<string> cultivars = findCultivars(cboCrops.Text);
                foreach (string cultivar in cultivars)
                {
                    cboCultivar.Items.Add(cultivar);
                }
                string vCultivar = ReadUIVariable("cultivar");
                cboCultivar.Text = vCultivar;
                if (vCultivar != "")
                {
                    int idx = cboCultivar.Items.IndexOf(vCultivar);
                    lblCultivarWarning.Visible = idx == -1;
                }
                else
                {
                    lblCultivarWarning.Visible = false;
                }
            }
        }
        public void UpdateMachineryList(ComboBox combo, Label lblWarning, string type, string variable)
        {
            combo.Items.Clear();
            combo.Items.Add("none");
            List<string> machinery = findMachinery(type);
            foreach (string machine in machinery)
            {
                combo.Items.Add(machine);
            }
            //read ui variable value
            string vMachine = ReadUIVariable(variable);
            combo.Text = vMachine;
            if (vMachine != "")
            {
                int idx = combo.Items.IndexOf(vMachine);
                lblWarning.Visible = idx == -1;
            }
            else
            {
                lblWarning.Visible = false;
            }
        }
        public void UpdateTractorsList()
        {
            cboTractor.Items.Clear();
            cboTractor.Items.Add("none");
            List<string> tractors = findMachinery("tractor");
            foreach (string tractor in tractors)
            {
                cboTractor.Items.Add(tractor);
            }
            //read ui variable value
            string vTractor = ReadUIVariable("sow_tractor");
            cboTractor.Text = vTractor;
            if (vTractor != "")
            {
                int idx = cboTractor.Items.IndexOf(vTractor);
                lblTractorWarning.Visible = idx == -1;
            }
            else
            {
                lblTractorWarning.Visible = false;
            }
        }
        public void UpdateImplementsList()
        {
            cboImplement.Items.Clear();
            cboImplement.Items.Add("none");
            List<string> implements = findMachinery("implement");
            foreach (string implement in implements)
            {
                cboImplement.Items.Add(implement);
            }
            //read ui variable value
            string vImplement = ReadUIVariable("sow_implement");
            cboImplement.Text = vImplement;
            if (vImplement != "")
            {
                int idx = cboImplement.Items.IndexOf(vImplement);
                lblImplementWarning.Visible = idx == -1;
            }
            else
            {
                lblImplementWarning.Visible = false;
            }
        }
        public void UpdateFertiliserList()
        {
            cboFertiliser.Items.Clear();
            cboFertiliser.Items.Add("none");
            List<string> fertilisers = findFertilisers();
            foreach (string fertiliser in fertilisers)
            {
                cboFertiliser.Items.Add(fertiliser);
            }
            //read ui variable value
            string vFert = ReadUIVariable("fert_type");
            cboFertiliser.Text = vFert;
            if (vFert != "")
            {
                int idx = cboFertiliser.Items.IndexOf(vFert);
                lblFertiliserWarning.Visible = idx == -1;
            }
            else
            {
                lblFertiliserWarning.Visible = false;
            }
        }
        private List<string> findStates()
        {
            //Find a list of states that the management UI knows about
            //states will come from a rotation management node (nodes called "node")
            List<string> knownStates = new List<string>();

            string sXML = "";
            if (Simulation != null)
            {
                sXML = Simulation.FullXMLNoShortCuts();
            }
            else
            {
                //rootnode/folder or toolbox
                sXML = Controller.ApsimData.RootComponent.Contents;
            }

            XmlDocument tmpDoc = new XmlDocument();
            tmpDoc.LoadXml(sXML);
            XmlNodeList nodes = tmpDoc.SelectNodes("//node/name");

            foreach(XmlNode tmpNode in nodes)
            {
                string stateName = tmpNode.InnerText;
                if(stateName != null && knownStates.IndexOf(stateName) < 0) //not found
                {
                    knownStates.Add(stateName);
                }
            }
            return knownStates;
        }
        private List<string> findCrops()
        {
            //Find a list of states that the management UI knows about
            //states will come from a rotation management node (nodes called "node")
            //it is possible for this to be held above simulations - therefore using global stats
            //otherwise it should only return those states within the current simulation
            List<string> knownCrops = new List<string>();
            ApsimFile.Component component = Simulation;
            if (component == null)
            {
                component = Controller.ApsimData.RootComponent;
            }
            AddCrops(component, knownCrops);
            return knownCrops;
        }
        private void AddCrops(ApsimFile.Component component, List<string> knownCrops)
        {
            if (Types.Instance.MetaData(component.Type, "IsCrop").ToLower() == "yes")
            {
                //component.NAme is the name that appears in the tree - can be changed
                //component.Type is the name used toidentify the comonent's type - ie: xml file
                string cropName = component.Type;
                if (cropName != null && knownCrops.IndexOf(cropName) < 0) //not found
                {
                    knownCrops.Add(cropName);
                }
            }
            foreach (ApsimFile.Component child in component.ChildNodes)
            {
                AddCrops(child, knownCrops);
            }
        }
        private ApsimFile.Component findCurrentSimulation()
        {
            ApsimFile.Component thisComp = Controller.ApsimData.Find(NodePath);
            //loop back up the component datastructure until you get to the parent simulation. 
            while ((thisComp != null))
            {
                if (thisComp.Type.ToLower() == "simulation")
                {
                    return thisComp;
                }
                thisComp = thisComp.Parent;
            }
            return null;
        }
        private List<string> findCultivars(string crop)
        {
            List<string> lstCultivars = new List<string>();
            if (crop != "")
            {
                string[] cultivars = Types.Instance.Cultivars(crop);
                for (int i = 0; i < cultivars.Length; ++i)
                {
                    lstCultivars.Add(cultivars[i]);
                }
            }
            return lstCultivars;
        }
        private List<string> findMachinery(string id)
        {
            List<string> lstImplements = new List<string>();
            //this node could be a shortcut 
                //shortcut to a root node (not in a simulation)
                //shortcut to another simulation
                //in both cases the user should be warned of bad selections
            
            //otherwise show all available tractors

            //Find a list of tractors that the management UI knows about
            //Tractors are identified by the node named "Tractor"
            //if this node is a shortcut, valid tractors are those only within this simulation

            string sXML = "";
            if (Simulation != null)
            {
                sXML = Simulation.FullXMLNoShortCuts();
            }
            else 
            { 
                //rootnode/folder or toolbox
                sXML = Controller.ApsimData.RootComponent.Contents;
            }
            AddUniqueMachineryNames(lstImplements, sXML, id);
            return lstImplements;
        }
        private void AddUniqueMachineryNames(List<string> lstMachinery, string sXML, string id)
        {
            XmlDocument tmpDoc = new XmlDocument();
            tmpDoc.LoadXml(sXML);

            XmlNodeList nodes = tmpDoc.SelectNodes("//" + id);
            foreach (XmlNode tmpNode in nodes)
            {
                //name is in the parent node - name attribute
                XmlNode parentNode = tmpNode.ParentNode;
                if (parentNode != null)
                {
                    XmlAttribute nameAtt = parentNode.Attributes["name"];
                    if (nameAtt != null)
                    {
                        string machineName = nameAtt.Value;
                        if (machineName != null && lstMachinery.IndexOf(machineName) < 0) //not found
                        {
                            lstMachinery.Add(machineName);
                        }
                    }
                }
            }
        }
        private List<string> findFertilisers()
        {
            List<string> lstFertilisers = new List<string>();
            string sPath = System.Environment.GetEnvironmentVariable("Apsim");
            if (sPath != "" && sPath != null)
            {
                XmlDocument tmpDoc = new XmlDocument();
                tmpDoc.Load(sPath + "//Model///Fertiliser.xml");

                XmlNode node = tmpDoc.SelectSingleNode("//Model");
                if (node != null)
                {
                    foreach (XmlNode childNode in node.ChildNodes)
                    {
                        lstFertilisers.Add(childNode.Name);
                    }
                }
            }
            return lstFertilisers;
        }

        private void cboTractor_TextChanged(object sender, EventArgs e)
        {
            if (cboTractor.Text != "")
            {
                int idx = cboTractor.Items.IndexOf(cboTractor.Text);
                lblTractorWarning.Visible = idx == -1;
            }
            else
            {
                lblTractorWarning.Visible = false;
            }
        }
        private void cboImplement_TextChanged(object sender, EventArgs e)
        {
            if (cboImplement.Text != "")
            {
                int idx = cboImplement.Items.IndexOf(cboImplement.Text);
                lblImplementWarning.Visible = idx == -1;
            }
            else
            {
                lblImplementWarning.Visible = false;
            }
        }
        private void cboFertiliser_TextChanged(object sender, EventArgs e)
        {
            if (cboFertiliser.Text != "")
            {
                int idx = cboFertiliser.Items.IndexOf(cboFertiliser.Text);
                lblFertiliserWarning.Visible = idx == -1;
            }
            else
            {
                lblFertiliserWarning.Visible = false;
            }
        }
        private void cboCultivar_TextChanged(object sender, EventArgs e)
        {
            if (cboCultivar.Text != "")
            {
                int idx = cboCultivar.Items.IndexOf(cboCultivar.Text);
                lblCultivarWarning.Visible = idx == -1;
            }
            else
            {
                lblCultivarWarning.Visible = false;
            }
        }
        private void cboState_TextChanged(object sender, EventArgs e)
        {
            if (cboState.Text != "")
            {
                int idx = cboState.Items.IndexOf(cboState.Text);
                lblStateWarning.Visible = idx == -1;
            }
            else
            {
                lblStateWarning.Visible = false;
            }
        }
        private void cboCrops_TextChanged(object sender, EventArgs e)
        {
            if (cboCrops.Text != "")
            {
                int idx = cboCrops.Items.IndexOf(cboCrops.Text);
                lblCropWarning.Visible = idx == -1;
            }
            else
            {
                lblCropWarning.Visible = false;
            }
        }

        public List<ApsimFile.Component> getShortcutNodes(string sXML)
        {
            XmlDocument tmpDoc = new XmlDocument();
            tmpDoc.LoadXml(sXML);

            List<ApsimFile.Component> lstComponents = new List<ApsimFile.Component>();
            XmlNodeList nodes = tmpDoc.SelectNodes("//*[@shortcut]");
            foreach (XmlNode tmpNode in nodes)
            {
                //get xml for destination shortcut
                //string stores the name attribute for each node in the path
                ApsimFile.Component tmpComponent = null;
                string sShortcut = XmlHelper.Attribute(tmpNode, "shortcut");
                while (sShortcut != "")
                {
                    tmpComponent = Controller.ApsimData.Find(sShortcut);
                    if (tmpComponent != null)
                    {
                        //code not correct as Contents refers to innerXML so misses shorcuts to shortcuts
                        sShortcut = XmlHelper.Attribute(tmpComponent.ContentsAsXML, "shortcut");
                    }
                }
                if (tmpComponent != null)
                {
                    lstComponents.Add(tmpComponent);
                }
            }
            return lstComponents;
        }

        private void scriptUI1_Load(object sender, EventArgs e)
        {

        }

        private void tabPage3_Click(object sender, EventArgs e)
        {

        }

        private void GenericUI_Load(object sender, EventArgs e)
        {

        }
    }
}
