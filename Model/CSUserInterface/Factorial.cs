using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Text;
using System.Windows.Forms;
using System.Xml;
using CSGeneral;


namespace CSUserInterface
{
    public partial class Factorial : Controllers.BaseView
    {
        private Color[] GroupColours = new Color[5];
        private List<string> targets = new List<string>();
        private List<string> targetTypes = new List<string>();
        Color getGroupColour(int iGroup)
        {
            if (iGroup < GroupColours.Length)
                return GroupColours[iGroup];
            return Color.Red;
        }
        
        public Factorial()
        {
            InitializeComponent();
            GroupColours[0] = Color.LightSteelBlue;
            GroupColours[1] = Color.LightBlue;
            GroupColours[2] = Color.LightCyan;
            GroupColours[3] = Color.Tan;
            GroupColours[4] = Color.Wheat;
        }
        public override void OnRefresh()
        {
            //ProcessConfig is used to remove references that no longer exist
            //UpdateConfig will add new variables
            //DisplayConfig will then display the udated settings
            //targets.Clear();
            //targetTypes.Clear();
            //ProcessSubGroup(Controller.Selection, Data);
            //DisplayConfig();

            //DisplayXMLValues();
        }
        public override void OnSave()
        {
/*
  //update variable settings
           int iRow = 1;
            SaveVariables(Data, ref iRow);

            while(iRow < fpSpread.ActiveSheet.RowCount)
            {
                SaveChildGroups(Data, ref iRow, -1);
                ++iRow;
                //row should be pointing to the last row of the current subgroup (blank row)
            }
 */ 
        }

        private void DisplayXMLValues()
        {
            treeView1.Nodes.Clear();
            //find vars node
            XmlNode varsNode = Data.SelectSingleNode("vars");
            if (varsNode != null)
            {
                foreach (XmlNode node in varsNode.ChildNodes)
                {
                    AddTreeNode(null, node);
                }
            }
        }
        private void AddTreeNode(TreeNode parentNode, XmlNode xmlnode)
        {
            string sText = XmlHelper.Name(xmlnode);
            TreeNode treeNode = null;

            if (parentNode == null)
            {
                treeNode = treeView1.Nodes.Add(sText);
            }
            else
            {
                treeNode = parentNode.Nodes.Add(sText);
                parentNode.Expand();
            }

            bool bEnabled = XmlHelper.Attribute(xmlnode, "enabled") == "yes";
            treeNode.Checked = bEnabled;
            bool bInherited = XmlHelper.Attribute(xmlnode, "inherited") == "yes";
            if(bInherited)
                treeNode.ForeColor = Color.Gray;

            foreach (XmlNode node in xmlnode.ChildNodes)
            {
                AddTreeNode(treeNode, node);
            }
        }

        private void ProcessSubGroup(ApsimFile.Component tmpComponent, XmlNode subNode)
        {
            //get variables node
            foreach (ApsimFile.Component childComponent in tmpComponent.ChildNodes)
            {
                //loop through other nodes looking for components that are not folders or variables
                //add them to the variabes node (check if they exist first)
                //after adding variables - test each variable for validity (including targets)
                //remove invalid static variables, but provide a warning for user variables
                string sType = childComponent.Type;
                if (sType == "folder")
                {
                    //look for existing subgroup - if not found then create
                    //note, we need to create a duplicate xml hierarchy of the subgrouped components
                    //as their xml data may not be kept in the component nodes
                    XmlNode subGroupNode = subNode.SelectSingleNode("subgroup[@name='" + childComponent.Name + "']");
                    if (subGroupNode == null)
                    {
                        subGroupNode = subNode.AppendChild(subNode.OwnerDocument.CreateElement("subgroup"));
                        XmlHelper.SetName(subGroupNode, childComponent.Name);
                    }
                    ProcessSubGroup(childComponent, subGroupNode);
                }
                else
                {
                    //shortcuts will be the targets
                    if (childComponent.ShortCutTo == null)
                        AddVariableToVariablesNode(subNode, childComponent);
                    else
                    {
                        targets.Add(childComponent.Name);
                        targetTypes.Add(childComponent.Type);
                    }
                }
            }
            //ValidateSubGroupVariables(variablesNode, subGroupNode);
        }
        private void AddVariableToVariablesNode(XmlNode variablesNode, ApsimFile.Component tmpComponent)
        {
            if (tmpComponent == null || variablesNode == null) return;
            //type of component will decide how this is processed
            //soil nodes can have subnodes (initWater, InitNitrogen)
            //met file has no subnodes
            //may have to add exceptions to this later

            string sID = tmpComponent.Name;
            XmlNode variableNode = variablesNode.SelectSingleNode("variable[@name='" + sID + "']");
            //if it doesn't exist then create it - shouldn't have to change anything if it does exist
            if (variableNode == null)
            {
                variableNode = variablesNode.AppendChild(variablesNode.OwnerDocument.CreateElement("variable"));
                XmlHelper.SetName(variableNode, sID);
                XmlHelper.SetAttribute(variableNode, "component", tmpComponent.Type);
                XmlHelper.SetAttribute(variableNode, "active", "true");
                //Target should be the first target item that is the same type
                for(int i = 0; i < targetTypes.Count; ++i)
                {
                    if(targetTypes[i] == tmpComponent.Type)
                    {
                        XmlHelper.SetAttribute(variableNode, "target", targets[i]);
                        break;
                    }
                }
            }
            //if tmpComponent is a soil, then it may havesubnodes that should be added as well
            //attempting to do this generically to begin with so we don;t liit it to soils
            foreach(ApsimFile.Component childComponent in tmpComponent.ChildNodes)
            {
                AddVariableToVariablesNode(variableNode, childComponent);
            }
        }
        private void ValidateSubGroupVariables(XmlNode variablesNode, XmlNode subGroupNode)
        {
            //check each variable in the variablesnode
            //if it is a component variable and the component doesn't exist, then remove the variable
            //leave non component variables for the moment
            foreach (XmlNode tmpNode in variablesNode.ChildNodes)
            {
                string sComponent = XmlHelper.Value(tmpNode, "component");
                if (sComponent != "")
                {
                    string sTarget = XmlHelper.Value(tmpNode, "target");

                }
            }
        }
        private void UpdateVariable(XmlNode node, int iRow)
        {
            int col = fpSpread.ActiveSheet.ColumnCount - 4;
            if(!fpSpread.ActiveSheet.Cells[iRow, col].Locked)
                XmlHelper.SetAttribute(node, "active", fpSpread.ActiveSheet.Cells[iRow, col].Text);
            if (!fpSpread.ActiveSheet.Cells[iRow, col + 1].Locked)
                XmlHelper.SetAttribute(node, "target", fpSpread.ActiveSheet.Cells[iRow, col + 1].Text);
            if (!fpSpread.ActiveSheet.Cells[iRow, col + 2].Locked)
                XmlHelper.SetAttribute(node, "variable", fpSpread.ActiveSheet.Cells[iRow, col + 2].Text);
            if (!fpSpread.ActiveSheet.Cells[iRow, col + 3].Locked)
                XmlHelper.SetAttribute(node, "param", fpSpread.ActiveSheet.Cells[iRow, col + 3].Text);
        }
        private void AddNewVariable(int iRow, int iCol)
        {
            //find the current subgroup and add a new variable to it.
            XmlNode subNode = FindParentGroupNode(iRow, -1);
            if (subNode != null)
            {
                XmlNode variableNode = subNode.AppendChild(subNode.OwnerDocument.CreateElement("variable"));
                UpdateVariable(variableNode, iRow);
            }
        }
        private void SaveChildGroups(XmlNode parentGroup, ref int iRow, int iCurrentCol)
        {
            bool bChildrenRemain = true;
            while (bChildrenRemain)
            {
                FindNextGroupRow(ref iRow);
                if (iRow < fpSpread.ActiveSheet.RowCount && iRow > -1)
                {
                    int iCol = FindGroupNameCol(iRow);
                    if (iCol > iCurrentCol)
                    {
                        //child of the current group
                        string sName = fpSpread.ActiveSheet.Cells[iRow, iCol].Text;
                        XmlNode tmpNode = XmlHelper.Find(parentGroup, sName);
                        ++iRow;
                        SaveVariables(tmpNode, ref iRow);
                        SaveChildGroups(tmpNode, ref iRow, iCol);
                    }
                    else
                        bChildrenRemain = false;
                }
                else
                    bChildrenRemain = false;
            }
        }
        private void FindNextGroupRow(ref int iRow)
        {
            int iActiveCol = fpSpread.ActiveSheet.ColumnCount - 4;
            int iTargetCol = fpSpread.ActiveSheet.ColumnCount - 3;
            if (iActiveCol > -1)
            {
                while (iRow < fpSpread.ActiveSheet.RowCount)
                {
                    if (fpSpread.ActiveSheet.Cells[iRow, iActiveCol].CellType == null
                    && fpSpread.ActiveSheet.Cells[iRow, iTargetCol].CellType == null)
                    {
                        return;
                    }
                    ++iRow;
                }
            }
        }
        private int FindGroupNameCol(int iRow)
        {
            //assumes that this is definately a group column
            //will start from active column and work back in case we put something in the first col later
            int iActiveCol = fpSpread.ActiveSheet.ColumnCount - 4;
            for (int iCol = iActiveCol; iCol >= 0; --iCol)
            {
                if (fpSpread.ActiveSheet.Cells[iRow, iCol].Text != "")
                    return iCol;
            }
            return -1;//this should be in error if used correctly
        }
        private void SaveVariables(XmlNode parentNode, ref int iRow)
        {
            if (parentNode == null)
                return;
            int iActiveCol = fpSpread.ActiveSheet.ColumnCount - 4;
            int iTargetCol = fpSpread.ActiveSheet.ColumnCount - 3;
            int iVarCol = fpSpread.ActiveSheet.ColumnCount - 2;
            if (iActiveCol > -1)
            {
                //read rows until reach a null active cell
                XmlNode varNode = FindNextVariableNode(parentNode.FirstChild);
                int iLast = fpSpread.ActiveSheet.GetLastNonEmptyRow(FarPoint.Win.Spread.NonEmptyItemFlag.Data) + 1;
                while (iRow < iLast)
                {
                    if (fpSpread.ActiveSheet.Cells[iRow, iActiveCol].CellType == null)
                    {
                        return;
                    }
                    //if variable cell is locked then this is a component
                    //componenets may have subnodes eg: soil where multiple rows are represented for a single component
                    //if row is locked, then check if it is equal to the current node - if not increment - if this is not equal then it is out of sybch
                    if (fpSpread.ActiveSheet.Cells[iRow, iVarCol].Locked)
                    {
                        if (varNode != null)
                        {
                            string sName = XmlHelper.Name(varNode);
                            string sRowName = fpSpread.ActiveSheet.Cells[iRow, iVarCol].Text;
                            if (sName != sRowName)
                            {
                                //if varnode is a subgroup - keep getting next node until it is not a subgroup
                                varNode = FindNextVariableNode(varNode.NextSibling);
                            }
                            if (varNode == null)
                            {
                                return;
                            }
                            sName = XmlHelper.Name(varNode);
                            if (sName != sRowName)
                            {
                                //out of sync
                                return;
                            }
                            UpdateVariable(varNode, iRow);
                        }
                    }
                    else
                    {
                        //if xmlnode is a component node then it needs to be incremented - check again to ensure synchronised
                        string sVal = XmlHelper.Attribute(varNode, "component");
                        if (sVal != "")
                        {
                            varNode = FindNextVariableNode(varNode.NextSibling);
                            if (varNode == null)
                            {
                                return;
                            }
                        }
                        UpdateVariable(varNode, iRow);
                        varNode = FindNextVariableNode(varNode.NextSibling);
                    }
                    ++iRow;
                }
            }
        }
        private XmlNode FindNextVariableNode(XmlNode varNode)
        {
            XmlNode nextNode = varNode;
            while (nextNode != null)
            {
                if (nextNode.Name == "variable")
                {
                    return nextNode;
                }
                nextNode = nextNode.NextSibling;
            }
            return null;
        }

        private XmlNode FindParentGroupNode(int iRow, int iCurrentCol)
        {
            //if iCol < 0 then the subgroup has not been found yet

            //a subgroup does not have a checkbox in the active column, nor a combobox in the target col
            //once the first subgroup has been identified then it's parent name will be 1 col less - excpt for Common which is an exception
            int iActiveCol = fpSpread.ActiveSheet.ColumnCount - 4;
            int iTargetCol = fpSpread.ActiveSheet.ColumnCount - 3;
            for (int i = iRow; i >= 0; --i)
            {
                if (fpSpread.ActiveSheet.Cells[i, iActiveCol].CellType == null
                && fpSpread.ActiveSheet.Cells[i, iTargetCol].CellType == null)
                {
                    //find col that has the group name
                    int iCol = FindGroupNameCol(i);
                    if(iCol > -1)
                    {
                        XmlNode parentNode = null;
                        if (fpSpread.ActiveSheet.Cells[i, iCol].Text == "Common")
                        {
                            return Data;
                        }
                        else
                        {
                            //if this is the first call then we'renot sure which level this group is at
                            //after the first call then the parentnode will be in the column before this one
                            if (iCol < iCurrentCol || iCurrentCol == -1)
                            {
                                parentNode = FindParentGroupNode(i - 1, iCol);
                            }
                        }
                        if (parentNode != null)
                        {
                            string subGroupName = fpSpread.ActiveSheet.Cells[i, iCol].Text;
                            XmlNode tmpNode = XmlHelper.Find(parentNode, subGroupName);
                            return tmpNode;
                        }
                    }
                }
            }
            return null;
        }
        private void DisplayConfig()
        {
            //need to calc the subgroup depth to know where to put the columns
            fpSpread.SuspendLayout();

            fpSpread.ActiveSheet.RowCount = 0;
            fpSpread.ActiveSheet.RowCount = 1000;

            int iTotalDepth = 0;
            iTotalDepth = CalcSubGroupDepth(iTotalDepth, Data);
            UpdateColumnFormatting(iTotalDepth);

            //first row is common, after that add remaining subgroup nodes
            //leave a blank row in each subgroup to addvariables
            int iRow = 0;
            DisplayGroupHeader(iRow, 0, "Common");

            XmlNodeList nodes = Data.SelectNodes("variable");
            foreach (XmlNode childNode in nodes)
            {
                DisplayVariable(childNode, ref iRow);
            }
            ++iRow;
            DisplayBlankVariable(iRow);

            nodes = Data.SelectNodes("subgroup");
            foreach (XmlNode node in nodes)
            {
                int iDepth = 0;
                DisplaySubGroup(node, ref iRow, iDepth);
            }
            fpSpread.ActiveSheet.RowCount = iRow+1;
            fpSpread.ResumeLayout();
        }
        private void DisplayGroupHeader(int iRow, int iCol, string sHeader)
        {
            fpSpread.ActiveSheet.SetValue(iRow, iCol, sHeader);
            fpSpread.ActiveSheet.Cells[iRow, iCol].ColumnSpan = fpSpread.ActiveSheet.ColumnCount - iCol;
            fpSpread.ActiveSheet.Cells[iRow, iCol].BackColor = getGroupColour(iCol);
        }
        private void DisplaySubGroup(XmlNode node, ref int iRow, int iDepth)
        {
            //iRow passd in should be the last row used
            ++iRow;
            DisplayGroupHeader(iRow, iDepth, XmlHelper.Name(node));

            //display variables
            XmlNodeList nodes = node.SelectNodes("variable");
            foreach (XmlNode childNode in nodes)
            {
                DisplayVariable(childNode, ref iRow);
            }
            ++iRow; //leave 1 row blank
            DisplayBlankVariable(iRow);
            nodes = node.SelectNodes("subgroup");
            foreach (XmlNode childNode in nodes)
            {
                DisplaySubGroup(childNode, ref iRow, iDepth + 1);
            }
        }
        private void DisplayVariable(XmlNode node, ref int iRow)
        {
            ++iRow;
            int iCol = fpSpread.ActiveSheet.ColumnCount - 4;

            string sTarget = XmlHelper.Attribute(node, "target");
            DisplayCheckBox(iRow, iCol, XmlHelper.Attribute(node, "active"));
            DisplayTargetsCombo(iRow, iCol + 1, sTarget);

            //components are readonly (met, soil etc...)
            bool bMultipleTargets = false;
            string sComp = XmlHelper.Attribute(node, "component");
            if (sComp == "")
            {
                string sVariable = XmlHelper.Attribute(node, "variable");
                DisplayVariablesCombo(iRow, iCol + 2, sTarget);
                fpSpread.ActiveSheet.SetValue(iRow, iCol + 2, sVariable);
                fpSpread.ActiveSheet.SetValue(iRow, iCol + 3, XmlHelper.Attribute(node, "values"));
            }
            else
            {
                fpSpread.ActiveSheet.SetValue(iRow, iCol + 2, XmlHelper.Name(node));
                bMultipleTargets = CheckMultipleTargets(sComp);
                fpSpread.ActiveSheet.Cells[iRow, iCol + 1].Locked = !bMultipleTargets;
                fpSpread.ActiveSheet.Cells[iRow, iCol + 2].Locked = true;
                fpSpread.ActiveSheet.Cells[iRow, iCol + 3].Locked = true;
            }
            //if variable has subnodes, then add the name as a parameter, and rewrite the variable name
            //first thing the DisplayVariable does is add a row - as we want to overwrite the first row, need to adjust therow cound
            int iCurrentRow = iRow-1;
            XmlNodeList nodes = node.SelectNodes("variable");
            foreach (XmlNode childNode in nodes)
            {
                DisplayVariable(childNode, ref iCurrentRow);
                //child variable nodes are written, then overwritten with currentvariables name
                fpSpread.ActiveSheet.Cells[iCurrentRow, iCol + 1].Locked = !bMultipleTargets;
                fpSpread.ActiveSheet.SetValue(iCurrentRow, iCol + 1, sTarget);
                fpSpread.ActiveSheet.SetValue(iCurrentRow, iCol + 2, XmlHelper.Name(node));
                fpSpread.ActiveSheet.SetValue(iCurrentRow, iCol + 3, XmlHelper.Name(childNode));
            }
            if (iCurrentRow > iRow)
                iRow = iCurrentRow;
        }
        private bool CheckMultipleTargets(string sType)
        {
            int iTargetCount = 0;
            for (int i = 0; i < targetTypes.Count; ++i)
            {
                if (targetTypes[i] == sType)
                {
                    ++iTargetCount;
                    if (iTargetCount > 1)
                        return true;
                }
            }
            return false;
        }
        private void DisplayBlankVariable(int iRow)
        {
            FarPoint.Win.Spread.CellType.ComboBoxCellType comboBox = new FarPoint.Win.Spread.CellType.ComboBoxCellType();
            comboBox.Items = targets.ToArray();
            int iCol = fpSpread.ActiveSheet.ColumnCount - 4;
            fpSpread.ActiveSheet.Cells[iRow, iCol + 1].CellType = comboBox;
        }
        private void DisplayCheckBox(int iRow, int iCol, string sActive)
        {
            FarPoint.Win.Spread.CellType.CheckBoxCellType checkBox = new FarPoint.Win.Spread.CellType.CheckBoxCellType();
            fpSpread.ActiveSheet.Cells[iRow, iCol].Text = sActive;
            fpSpread.ActiveSheet.Cells[iRow, iCol].CellType = checkBox;
            fpSpread.ActiveSheet.Cells[iRow, iCol].HorizontalAlignment = FarPoint.Win.Spread.CellHorizontalAlignment.Center;
            fpSpread.ActiveSheet.Cells[iRow, iCol].VerticalAlignment = FarPoint.Win.Spread.CellVerticalAlignment.Center;
        }
        private void DisplayVariablesCombo(int iRow, int iCol, string sTarget)
        {
            List<string> variables = getVariableList(sTarget);
            DisplayCombo(iRow, iCol, variables.ToArray(), "");
        }
        private void DisplayTargetsCombo(int iRow, int iCol, string sTarget)
        {
            DisplayCombo(iRow, iCol, targets.ToArray(), sTarget);
        }
        private void DisplayCombo(int iRow, int iCol, string[] sTargets, string sTarget)
        {
            FarPoint.Win.Spread.CellType.ComboBoxCellType comboBox = new FarPoint.Win.Spread.CellType.ComboBoxCellType();
            comboBox.Items = sTargets;
            comboBox.Editable = false;
            fpSpread.ActiveSheet.Cells[iRow, iCol].CellType = comboBox;
            fpSpread.ActiveSheet.SetValue(iRow, iCol, sTarget);
        }
        private List<string> getVariableList(string sTarget)
        {
            List<string> variables = new List<string>();
            foreach (ApsimFile.Component childComponent in Controller.Selection.ChildNodes)
            {
                if (childComponent.ShortCutTo != null)
                {
                    if (childComponent.Name == sTarget)
                    {
                        if (childComponent.Type == "manager")
                        {
                            return getManagerVariables(childComponent);
                        }
                    }
                }
            }
            return variables;
        }
        private List<string> getManagerVariables(ApsimFile.Component manager)
        {
            List<string> variables = new List<string>();
            XmlNode managerNode = manager.ContentsAsXML;
            if (managerNode != null)
            {
                XmlNode uiNode = managerNode.SelectSingleNode("ui");
                if (uiNode != null)
                {
                    foreach (XmlNode node in uiNode.ChildNodes)
                    {
                        variables.Add(node.Name);
                    }
                }
            }
            return variables;
        }
        private int CalcSubGroupDepth(int depth, XmlNode parentNode)
        {
            int maxDepth = depth;
            XmlNodeList nodes = parentNode.SelectNodes("subgroup");
            foreach (XmlNode node in nodes)
            {
                int tmpDepth = CalcSubGroupDepth(depth + 1, node);
                maxDepth = System.Math.Max(tmpDepth, maxDepth);
            }
            return maxDepth;
        }
        private void UpdateColumnFormatting(int iTotalDepth)
        {
            fpSpread.ActiveSheet.RowHeader.Visible = false;
            fpSpread.ActiveSheet.ColumnCount = iTotalDepth + 3;
            //subgroup heading columns are ??
            int i = 0;
            for (; i < iTotalDepth; ++i)
            {
                fpSpread.ActiveSheet.Columns[i].Width = 20;
                fpSpread.ActiveSheet.Columns[i].Label = " ";
            }
            fpSpread.ActiveSheet.Columns[i-1].Width = 40;
            fpSpread.ActiveSheet.Columns[i-1].Label = "Active";
            //fpSpread.ActiveSheet.Columns[i - 1].HorizontalAlignment = FarPoint.Win.Spread.CellHorizontalAlignment.Center;
            //fpSpread.ActiveSheet.Columns[i - 1].VerticalAlignment = FarPoint.Win.Spread.CellVerticalAlignment.Center;

            fpSpread.ActiveSheet.Columns[i].Width = 150;
            fpSpread.ActiveSheet.Columns[i].Label = "Target";
            fpSpread.ActiveSheet.Columns[i + 1].Width = 90;
            fpSpread.ActiveSheet.Columns[i + 1].Label = "Variable";
            fpSpread.ActiveSheet.Columns[i + 2].Width = 150;
            fpSpread.ActiveSheet.Columns[i + 2].Label = "Parameters";

        }

        private void fpSpread_EditChange(object sender, FarPoint.Win.Spread.EditorNotifyEventArgs e)
        {
            //Target Col = fpSpread.ActiveSheet.ColumnCount - 3;
            if (e.Column == fpSpread.ActiveSheet.ColumnCount - 3)
            {
                //update variables cell
                string sTarget = fpSpread.ActiveSheet.Cells[e.Row, e.Column].Text;
                DisplayVariablesCombo(e.Row, e.Column + 1, sTarget);
                //add active checkbox
                DisplayCheckBox(e.Row, e.Column - 1, "true");
                //display new line
                fpSpread.ActiveSheet.Rows.Add(e.Row+1, 1);
                DisplayBlankVariable(e.Row + 1);
                AddNewVariable(e.Row, e.Column);
            }
        }

        private void treeView1_AfterSelect(object sender, TreeViewEventArgs e)
        {

        }
    }
}
