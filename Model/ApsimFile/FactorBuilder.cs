using System;
using System.Collections.Generic;
using System.IO;
using System.Text;
using System.Xml;
using System.Linq;
using CSGeneral;

namespace ApsimFile
{
    public class FactorItem
    {
        public FactorItem NextItem = null;
        public List<string> Targets = null;

        public Component FactorComponent = null;
        public FactorBuilder Builder = null;

        public FactorItem(FactorBuilder b)
        {
            Builder = b;
        }

        public string FolderLevel { get; set; }

        public virtual string getDesc()
        {
            if (FactorComponent != null)
                return FactorComponent.Name;
            return "unknown";
        }
        //getCount returns the number of parameters for this factor only
        public virtual int getCount()
        {
            if (FactorComponent != null)
            {
                if (FactorComponent.Type == "folder")
                    return 1;
                return FactorComponent.ChildNodes.Count;
            }
            return 0;
        }

        //CalcCount returns the total number of parameters for this chain of factors
        public virtual int CalcCount()
        {
            if (NextItem != null)
            {
                if (FactorComponent.Type == "folder")
                    return NextItem.CalcCount();
                return NextItem.CalcCount() * FactorComponent.ChildNodes.Count;
            }
            if (FactorComponent.Type == "folder")
                return 1;
            return FactorComponent.ChildNodes.Count;
        }

        // calculate a list of complete factorial "names" - one string for each instance

		public virtual void CalcFactorialList(List<string> factorials, string prefix, SortedDictionary<string, string> scratch)
        {
            if (FactorComponent.Type == "folder")
            {
                SortedDictionary<string, string> s = scratch;
				s[pfx(prefix) + FactorComponent.Name] = FolderLevel;
                if (NextItem != null)
                {
					NextItem.CalcFactorialList(factorials, (Builder.useFQKeys ? pfx(prefix) + FactorComponent.Name + "." + FolderLevel : ""), s);
                }
                else
                {
                    factorials.Add(ToKVString(s));
                }
            }
            else
            {
                foreach (Component child in FactorComponent.ChildNodes)
                {
                    SortedDictionary<string, string> s = scratch;
					s[pfx(prefix) + FactorComponent.Name] = child.Name;
                    if (NextItem != null)
                    {
						NextItem.CalcFactorialList(factorials, prefix, s);
                    }
                    else
                    {
                        factorials.Add(ToKVString(s));
                    }
                }
            }
        }
        protected string pfx (string s) {
			return(Builder.useFQKeys ? ((s != "") ? s + "." : s) : "");
        }
		public virtual void Process(Component Simulation, string prefix, SortedDictionary<string, string> factorsToMatch)
        {
            if (FactorComponent.Type != "folder")
            {
                foreach (Component child in FactorComponent.ChildNodes)
                {
					if (factorsToMatch.Contains(new KeyValuePair<string, string>(pfx(prefix) + FactorComponent.Name, child.Name)))
                    {
                        //replace each target that is within the provided simulation with the child's xml
                        foreach (string target in Targets)
                        {
                            //need to remove the path of this simulation from the target to get the relative path
                            string relativePath = target.Substring(target.IndexOf(Simulation.FullPath) + Simulation.FullPath.Length + 1);
                            Component targetComp = Simulation.Find(relativePath);
                            if (targetComp != null)
                            {
                                //replace target nodes with factor nodes - add child factor nodes if they don't exist 
                                //don't remove any children from the target
                                ReplaceComponent(targetComp, child);
                            }
                            else
                            {
								throw new Exception("target " + target + ", path " + relativePath + " was not found in factor " + pfx(prefix) + FactorComponent.Name);
                            }
                        }
                    }
                }
                if (NextItem != null)
                {
					NextItem.Process(Simulation, prefix, factorsToMatch);
                }
            }
            else
            {
                if (factorsToMatch.Contains(new KeyValuePair<string, string>(FactorComponent.Name, FolderLevel)))
                {
                    if (NextItem != null)
                    {
                        //call next factor in the list
						NextItem.Process(Simulation, (Builder.useFQKeys ? pfx(prefix) + FactorComponent.Name + "." + FolderLevel : ""), factorsToMatch);
                    }
                }
            }
        }

        public void ReplaceComponent(Component targetComp, Component Source)
        {
            //replace the contents (innerxml).This doesn't affect child xml nodes that belong to child components
            targetComp.Contents = Source.Contents;
            //targetComp.Name = Source.Name;
            targetComp.ChildNodes.Clear();

            foreach (Component comp in Source.ChildNodes)
            {
                //add a component
                Component targetchild = targetComp.Add(comp.Contents);
                ReplaceComponent(targetchild, comp);
            }
        }

        public static string ToKVString(IDictionary<string, string> dict)
        {
            return string.Join(";", dict.Select(x => x.Key + "=" + x.Value));
        }


        public static bool FactorsMatch<TKey, TValue>(IDictionary<TKey, TValue> first, IDictionary<TKey, TValue> second)
        {
            if (first == second)
                return true;
            if ((first == null) || (second == null))
                return false;
            if (first.Count != second.Count)
                return false;

            var comparer = EqualityComparer<TValue>.Default;

            foreach (KeyValuePair<TKey, TValue> kvp in first)
            {
                TValue secondValue;
                if (!second.TryGetValue(kvp.Key, out secondValue))
                    return false;
                if (!comparer.Equals(kvp.Value, secondValue))
                    return false;
            }
            return true;
        }

    }

    public class ManagerFactorItem : FactorItem
    {
        public ManagerFactorItem(FactorBuilder b)
            : base(b)
        {
        }

        public XmlNode Variable = null;
        public List<string> Parameters = null;

        public override string getDesc()
        {
            if (Variable != null)
                return FactorComponent.Name + "." + XmlHelper.Name(Variable);
            return FactorComponent.Name;
        }
        //getCount returns the number of parameters for this factor only
        public override int getCount()
        {
            if (Parameters != null)
                return Parameters.Count;
            return 0;
        }
        //CalcCount returns the number of parameters for the chain of factors
        public override int CalcCount()
        {
            if (NextItem != null)
                return NextItem.CalcCount() * Parameters.Count;
            return Parameters.Count;
        }

		public override void CalcFactorialList(List<string> factorials, string prefix, SortedDictionary<string, string> scratch)
        {
            foreach (string par in Parameters)
            {
                SortedDictionary<string, string> s = scratch;
				s[(Builder.useFQKeys ? pfx(prefix) + FactorComponent.Name + "." : "") + Variable.Name] = par;

                if (NextItem != null)
                {
					NextItem.CalcFactorialList(factorials, prefix, s);
                }
                else
                {
                    factorials.Add(ToKVString(s));
                }
            }
        }

        public override void Process(Component Simulation, string prefix, SortedDictionary<string, string> factorsToMatch)
        {

            foreach (string par in Parameters)
            {
				if (factorsToMatch.Contains(new KeyValuePair<string, string>((Builder.useFQKeys ? pfx(prefix) + FactorComponent.Name + "." : "") + Variable.Name, par)))
                {
                    //replace each target that is within the provided simulation with the child's xml
                    foreach (string target in Targets)
                    {
                        string sRelativeTarget = target.Substring(target.IndexOf(Simulation.Name) + Simulation.Name.Length + 1);
                        Component targetComp = Simulation.Find(sRelativeTarget);
                        if (targetComp != null)
                        {
                            //find name of var in ui or CustomUI nodes
                            XmlNode variablesNode = targetComp.ContentsAsXML;
                            XmlNode varNode = variablesNode.SelectSingleNode("//ui/" + Variable.Name + " | " + "//CustomUI/" + Variable.Name);
                            if (varNode != null)
                            {
                                varNode.InnerText = par;
                            }
                            else
                            {
                                string sSelect = "//property[@name='" + Variable.Name + "']";
                                varNode = variablesNode.SelectSingleNode(sSelect);
                                if (varNode == null)
                                {
                                    sSelect = "//" + Variable.Name;
                                    varNode = variablesNode.SelectSingleNode(sSelect);
                                }
                                if (varNode != null)
                                {
                                    varNode.InnerText = par;
                                }//TODO if else then record as an error
                            }
                            targetComp.Contents = variablesNode.OuterXml;
                        }
                        else
                        {
							throw new Exception("target " + target + ", rel path" + sRelativeTarget + " was not found in factor " + (Builder.useFQKeys ? pfx(prefix) + FactorComponent.Name + "." : "") + Variable.Name);
                        }
                    }
				}
			}
                if (NextItem != null)
                {
                    //call next factor in the list
                    NextItem.Process(Simulation, prefix, factorsToMatch);
                }
        }
    }

    public class FactorBuilder
    {
        public bool TitleIsSingleLine { get; set; }

        public bool SaveExtraInfoInFilename { get; set; }

		public bool useFQKeys { get; set; } // keys are fully qualified (a.b.c... vs c)

		public FactorBuilder() 
        {
			useFQKeys = false;
			TitleIsSingleLine = true;
            SaveExtraInfoInFilename = false;
        }
        public FactorBuilder(Component factorial) 
        {
			useFQKeys = false;
			TitleIsSingleLine = true;
            SaveExtraInfoInFilename = false;
            XmlNode varNode = factorial.ContentsAsXML.SelectSingleNode("//settings");
			if (XmlHelper.Attribute(varNode, "fn") == "1")
                SaveExtraInfoInFilename = true;
			if (XmlHelper.Attribute(varNode, "fqKeys") == "1")
				useFQKeys = true;
        }
        public List<FactorItem> BuildFactorItems(Component factorial, string SimulationPath)
        {
            //read options 
            XmlNode varNode = factorial.ContentsAsXML.SelectSingleNode("//settings");
            SaveExtraInfoInFilename = false;
			if (XmlHelper.Attribute(varNode, "fn") == "1")
                SaveExtraInfoInFilename = true;

			TitleIsSingleLine = true;
			if (XmlHelper.Attribute(varNode, "tl") == "1")
                TitleIsSingleLine = false;

			useFQKeys = false;
			if (XmlHelper.Attribute(varNode, "fqKeys") == "1")
				useFQKeys = true;

            List<FactorItem> factorItems = new List<FactorItem>();
            List<Component> leaves = new List<Component>();
            FindLeaves(factorial, leaves);
            if (leaves.Count() > 0)
            {
                //leaves require a different way of calculating the factors
                //they work from the final leaf and then work back up the tree using parents
                //the difficult path is work out inheritance, so need to create the list, and then process it?
                foreach (var leaf in leaves)
                {
                    //build a list of factors for each leaf - work up through it's parents
                    bool folderlevel = true;
                    List<FactorItem> leafFactorItems = new List<FactorItem>();
                    ProcessAdvancedFactorItems(leaf, leafFactorItems, folderlevel, SimulationPath);
                    SortLeafFactors(leafFactorItems);
                    if (leafFactorItems.Count > 0)
                        factorItems.Add(leafFactorItems[0]);
                }
            }
            else
            {
                FactorItem lastItem = null;
                ProcessFactorNodes(factorial, ref lastItem, ref factorItems, SimulationPath);
            }
            return factorItems;
        }

        private void SortLeafFactors(List<FactorItem> leafFactorItems)
        {
            //move from the back, push factors that point to folders to the front.
            //make sure you stop before overlapping the start again
            //then reset the next items on each factor
            int idx = leafFactorItems.Count - 1;
            for (int i = leafFactorItems.Count - 1; i >= 0; --i)
            {
                if (leafFactorItems[idx].FactorComponent.Type == "folder")
                {
                    FactorItem tmp = leafFactorItems[idx];
                    leafFactorItems.RemoveAt(idx);
                    leafFactorItems.Insert(0, tmp);
                }
                else
                {
                    --idx;
                }
            }
            int iNext = 0;
            for (; iNext < leafFactorItems.Count - 1; ++iNext)
            {
                leafFactorItems[iNext].NextItem = leafFactorItems[iNext + 1];
            }
            if (iNext < leafFactorItems.Count)
                leafFactorItems[iNext].NextItem = null;

        }

        public void LoadVariableTypes(XmlNode metNode, List<string> variableTypes)
        {
            if (metNode != null && variableTypes != null)
            {
                foreach (XmlNode varNode in XmlHelper.ChildNodes(metNode, "facvar"))
                {
                    variableTypes.Add(varNode.InnerText.ToLower());
                }
            }
        }

        public void ProcessFactorNodes(Component parentComponent, ref FactorItem lastItem, ref List<FactorItem> items, string SimulationPath)
        {
            foreach (Component comp in parentComponent.ChildNodes)
            {
                if (comp.Type == "factor")
                {
                    List<string> targets = new List<string>();
                    XmlNode varNode = comp.ContentsAsXML.SelectSingleNode("//targets");
                    if (varNode != null)
                    {
                        foreach (XmlNode target in varNode.ChildNodes)
                        {
                            string spath = SimulationPath + "/";
                            if (target.InnerText.Contains(spath))
                                targets.Add(target.InnerText);
                        }
                    }
                    if (targets.Count > 0)
                    {
                        List<string> variableTypes = new List<string>();
                        LoadVariableTypes(Types.Instance.MetaDataNode("Factor", "FactorVariables"), variableTypes);
                        if (comp.ChildNodes.Count == 1 && variableTypes.Contains(comp.ChildNodes[0].Type.ToLower()))
                        {//(comp.ChildNodes[0].Type == "manager" || comp.ChildNodes[0].Type == "rule" || comp.ChildNodes[0].Type == "cropui"))
                            //process variables within manager code
                            XmlNodeList varNodes = comp.ContentsAsXML.SelectNodes("//vars/*");
                            foreach (XmlNode node in varNodes)
                            {
                                ManagerFactorItem item = new ManagerFactorItem(this);
                                item.Targets = targets;
                                item.FactorComponent = comp;
                                item.Variable = node;
                                string[] pars = node.InnerText.Split(new char[] { ',' });
                                pars = pars.Select(x => x.Trim(new char[] { '\"', ' ' })).ToArray();
                                var unique_pars = new HashSet<string>(pars);
                                item.Parameters = unique_pars.OrderBy(a => a).ToList<string>();
                                if (lastItem == null)
                                    items.Add(item);
                                else
                                    lastItem.NextItem = item;
                                lastItem = item;
                            }
                        }
                        else
                        {
                            FactorItem item = new FactorItem(this);
                            item.Targets = targets;
                            item.FactorComponent = comp;
                            if (lastItem == null)
                                items.Add(item);
                            else
                                lastItem.NextItem = item;
                            lastItem = item;
                        }
                    }
                }
                else
                {
                    ProcessFactorNodes(comp, ref lastItem, ref items, SimulationPath);
                }
            }
        }

        private void ProcessAdvancedFactorItems(Component leaf, List<FactorItem> factorItems, bool folderlevel, string simulationPath)
        {
            List<string> variableTypes = new List<string>();
            LoadVariableTypes(Types.Instance.MetaDataNode("Factor", "FactorVariables"), variableTypes);

            //add any childnodes that are factors
            var result = (from c in leaf.ChildNodes
                          where c.Type == "factor" && c.ContentsAsXML.SelectSingleNode("targets") != null
                          select c);

            foreach (var factor in result)
            {
                List<string> targets = new List<string>();
                string spath = simulationPath + "/";
                foreach (XmlNode node in factor.ContentsAsXML.SelectSingleNode("//targets").ChildNodes)
                {
                    if (node.InnerText.Contains(spath))
                        targets.Add(node.InnerText);
                }
                if (targets.Count > 0)
                {
                    //manager factor or normal
                    if (factor.ChildNodes.Count == 1 && variableTypes.Contains(factor.ChildNodes[0].Type.ToLower()))
                    {

                        XmlNodeList varNodes = factor.ContentsAsXML.SelectNodes("//vars/*");
                        foreach (XmlNode node in varNodes)
                        {
                            ManagerFactorItem item = new ManagerFactorItem(this);
                            item.Targets = targets;
                            item.FactorComponent = factor;
                            item.Variable = node;
                            string[] pars = node.InnerText.Split(new char[] { ',' });
                            var unique_pars = new HashSet<string>(pars.Select(x => x.Trim(new char[] { '\"', ' ' })));
                            item.Parameters = unique_pars.OrderBy(a => a).ToList<string>();
                            if ((from f in factorItems
                                 where f.getDesc() == item.getDesc()
                                 select f).Count() == 0)
                            {
                                if (factorItems.Count > 0)
                                    item.NextItem = factorItems[0];
                                factorItems.Insert(0, item);
                            }
                        }
                    }
                    else
                    {
                        FactorItem item = new FactorItem(this);
                        item.Targets = targets;
                        item.FactorComponent = factor;
                        if ((from f in factorItems
                             where f.getDesc() == item.getDesc()
                             select f).Count() == 0)
                        {
                            if (factorItems.Count > 0)
                                item.NextItem = factorItems[0];
                            factorItems.Insert(0, item);
                        }
                    }
                }
            }
            if (leaf.Parent != null && leaf.Parent.Parent != null)
            {//PArent.Parent is to check against the root node
                if (leaf.Parent.Type == "folder" && folderlevel)
                {
                    //parent is a factorfolder
                    FactorItem item = new FactorItem(this);
                    //item.Targets = targets;
                    item.FactorComponent = leaf.Parent;
                    item.FolderLevel = leaf.Name;
                    if ((from f in factorItems
                         where f.getDesc() == item.getDesc()
                         select f).Count() == 0)
                    {
                        if (factorItems.Count > 0)
                            item.NextItem = factorItems[0];
                        factorItems.Insert(0, item);
                    }

                }
                ProcessAdvancedFactorItems(leaf.Parent, factorItems, !folderlevel, simulationPath);
            }
        }

        private void FindLeaves(Component comp, List<Component> leaves)
        {
            //List<Component> leaves = new List<Component>();
            var result = (from c in comp.ChildNodes
                          where c.Type == "folder"
                          select c);
            if (result.Count() > 0)
            {
                foreach (var folder in result)
                {
                    FindLeaves(folder, leaves);
                }
            }
            else
            {
                if (comp.Type == "folder")
                {
                    leaves.Add(comp);
                }
            }
        }
    }

    public class Factor
    {
        private static string ProcessSingleSimulation(ApsimFile copiedFile, string SimulationPath, SortedDictionary<string, string> factorInstance, int uniqueId, string destFolder = "")
        {
            Component Simulation = copiedFile.Find(SimulationPath);
            string rootName = Simulation.Name;
            try
            {
                FactorBuilder builder = new FactorBuilder();
                List<FactorItem> items = builder.BuildFactorItems(copiedFile.FactorComponent, SimulationPath);
                int totalCount = 0;

                foreach (FactorItem item in items)
                {
                    totalCount += item.CalcCount();
                    item.Process(Simulation, "", factorInstance);
                }

                Simulation.Name = simulationName(builder.SaveExtraInfoInFilename, rootName, factorInstance, uniqueId, totalCount);
                return (CreateJobFromSimulation(Simulation, factorInstance, builder.TitleIsSingleLine, destFolder));
            }
            catch (Exception ex)
            {
                throw new Exception("Error encountered creating Factorials\n" + ex.Message);
            }
        }

        private static ApsimFile CreateCopy(ApsimFile apsimfile)
        {
            XmlDocument doc = new XmlDocument();
            doc.LoadXml(apsimfile.RootComponent.FullXML());
            XmlHelper.SetAttribute(doc.DocumentElement, "version", APSIMChangeTool.CurrentVersion.ToString());

            ApsimFile tmpFile = new ApsimFile();
            tmpFile.New(doc.OuterXml);
            return tmpFile;
        }
        public static List<string> CalcFactorialList(ApsimFile _F, string SimulationPath)
        {
            FactorBuilder builder = new FactorBuilder();
            List<string> allFactorials = new List<string>();
            foreach (FactorItem item in builder.BuildFactorItems(_F.FactorComponent, SimulationPath))
            {
                var scratch = new SortedDictionary<string, string>();
                item.CalcFactorialList(allFactorials, "", scratch);
            }
            return (allFactorials);
        }

        public static List<string> CreateSimFiles(ApsimFile _F, string[] SimsToRun, string destFolder = "")
        {

            List<string> SimFiles = new List<string>();
            foreach (string FullSimulationPath in SimsToRun)
            {
                string[] simPathParts = FullSimulationPath.Split(new string[] { "@factorial=" }, StringSplitOptions.None);
                string simXmlPath = simPathParts[0];
                string simPathFactorInstance = "";
                if (simPathParts.Count() > 1) simPathFactorInstance = simPathParts[1].Trim(new char[] { '\'', ' ' });

                FactorBuilder builder = new FactorBuilder();
                List<string> allFactorials = new List<string>();
                foreach (FactorItem item in builder.BuildFactorItems(_F.FactorComponent, simXmlPath))
                {
                    var scratch = new SortedDictionary<string, string>();
                    item.CalcFactorialList(allFactorials, "", scratch);
                }

                if (simPathFactorInstance != "")
                {
                    var myFactors = new SortedDictionary<string, string>(simPathFactorInstance.Split(';').Select(s => s.Split('=')).ToDictionary(a => a[0].Trim(), a => a[1].Trim()));
                    var sequenceNumber = 1 + allFactorials.FindIndex(x => x == simPathFactorInstance);
                    if (sequenceNumber < 1) throw new Exception("factor level '" + simPathFactorInstance + "' not found");
                    //work on a copy of the file - names will change during the processing of the factorial nodes
                    SimFiles.Add(Factor.ProcessSingleSimulation(CreateCopy(_F), simXmlPath, myFactors, sequenceNumber, destFolder));
                }
                else
                {
                    foreach (string str in allFactorials)
                    {
                        var myFactors = new SortedDictionary<string, string>(str.Split(';').Select(s => s.Split('=')).ToDictionary(a => a[0].Trim(), a => a[1].Trim()));
                        var sequenceNumber = 1 + allFactorials.FindIndex(x => x == str);
                        SimFiles.Add(Factor.ProcessSingleSimulation(CreateCopy(_F), simXmlPath, myFactors, sequenceNumber, destFolder));
                    }
                }
            }
            return SimFiles;
        }
        public static List<string> CreateSimulationNames(ApsimFile _F, string[] SimsToRun, ProgressNotifier Notifier)
        {

            List<string> SimFiles = new List<string>();
            foreach (string FullSimulationPath in SimsToRun)
            {
                Notifier(0, "Opening " + FullSimulationPath);
                FactorBuilder builder = new FactorBuilder();
                List<string> allFactorials = new List<string>();
                int totalCount = 0;
                foreach (FactorItem item in builder.BuildFactorItems(_F.FactorComponent, FullSimulationPath))
                {
                    totalCount += item.CalcCount();
                    var scratch = new SortedDictionary<string, string>();
                    item.CalcFactorialList(allFactorials, "", scratch);
                }

                string FullSimulationPathName = _F.Find(FullSimulationPath).Name;
                int sequenceNumber = 0;
                foreach (string str in allFactorials)
                {
                    SimFiles.Add(FullSimulationPath + "@factorial=" + str);
                    Notifier((int)(100 * ((double)sequenceNumber / totalCount)), "Found factorial " + sequenceNumber.ToString());
                    sequenceNumber++;
                }
            }
            return SimFiles;
        }
        public static List<string> CreateSimulationJobs(ApsimFile _F, string[] SimsToRun, ref List<Job> ApsimJobs, ProgressNotifier Notifier)
        {
            List<string> SimFiles = new List<string>();
            foreach (string FullSimulationPath in SimsToRun)
            {
                Notifier(0, "Opening " + FullSimulationPath);
                FactorBuilder builder = new FactorBuilder();
                List<string> allFactorials = new List<string>();
                int totalCount = 0;
                foreach (FactorItem item in builder.BuildFactorItems(_F.FactorComponent, FullSimulationPath))
                {
                    totalCount += item.CalcCount();
                    var scratch = new SortedDictionary<string, string>();
                    item.CalcFactorialList(allFactorials, "",  scratch);
                }


                string FullSimulationPathName = _F.Find(FullSimulationPath).Name;
                int sequenceNumber = 0;
                foreach (string str in allFactorials)
                {
                    SimFiles.Add(FullSimulationPath + "@factorial=" + str);
                    Notifier((int)(100 * ((double)sequenceNumber / totalCount)), "Found factorial " + sequenceNumber.ToString());
                    sequenceNumber++;
                }
            }
            return SimFiles;
        }

        public static string simulationName(bool saveExtraInfoInFilename, string rootName, SortedDictionary<string, string> factorsList, int counter, int totalCount)
        {
            string result = rootName;
            if (saveExtraInfoInFilename)
            {
                result += ";" + FactorItem.ToKVString(factorsList);
            }
            else
            {
                //write a spacer to list sims in order eg: 01 or 001 or 0001 depending on count
                string sPad = "";
                double tot = Math.Floor(Math.Log10(totalCount) + 1);
                double file = Math.Floor(Math.Log10(counter) + 1);
                for (int i = 0; i < (int)(tot - file); ++i)
                    sPad += "0";

                result += "_" + sPad + counter;
            }
            return (result);
        }

        private static string CreateJobFromSimulation(Component Simulation, SortedDictionary<string, string> factorsList, bool titleIsSingleLine, string destFolder)
        {
            //add title line into simulation
            //find all components that are outputfiles... or use xml?
            List<Component> outputfiles = new List<Component>();
            AddOutputFilesToList(Simulation, outputfiles);
            foreach (Component comp in outputfiles)
            {
                // Ensure the output name is correct for this simulation
                XmlNode compNode = comp.ContentsAsXML;
                XmlNode fileNode = compNode.SelectSingleNode("//filename");
                if (fileNode != null)
                    fileNode.InnerText = ComponentUtility.CalcFileName(comp);
                else
                    throw new Exception("Cant find an outputfile filename node!");

                if (titleIsSingleLine)
                {
                    //Title node is now read only, so need to add this as a constant 
                    //XmlNode titleNode = compNode.SelectSingleNode("//title");
                    //if (titleNode != null)
                    //    titleNode.InnerText = factorsList;
                    //else
                    //    throw new Exception("Cant find an outputfile title node!");
                    Component constantsComponent = null;
                    foreach (Component c in comp.ChildNodes)
                        if (c.Type == "variables")
                            constantsComponent = c;
                    if (constantsComponent == null)
                        throw new Exception("No variables in outputfile!");

                    XmlNode componentNode = constantsComponent.ContentsAsXML;
                    XmlNode constantsNode = componentNode.SelectSingleNode("//constants");
                    if (constantsNode == null)
                    {
                        constantsNode = componentNode.OwnerDocument.CreateElement("constants");
                        componentNode.AppendChild(constantsNode);
                    }
                    XmlNode factorNode = constantsNode.SelectSingleNode("//constant[@name='factors']");
                    if (factorNode == null)
                    {
                        factorNode = constantsNode.OwnerDocument.CreateElement("constant");
                        constantsNode.AppendChild(factorNode);
                    }
                    XmlHelper.SetAttribute(factorNode, "name", "factors");
                    factorNode.InnerText = FactorItem.ToKVString(factorsList);
                    constantsComponent.Contents = componentNode.OuterXml;
                }
                else
                {
                    //Find the variables node (should be a child of the output file - it will find and use the first one 
                    Component constantsComponent = null;
                    foreach (Component c in comp.ChildNodes)
                        if (c.Type == "variables")
                            constantsComponent = c;
                    if (constantsComponent == null)
                        throw new Exception("No variables in outputfile!");

                    XmlNode componentNode = constantsComponent.ContentsAsXML;
                    XmlNode constantsNode = componentNode.SelectSingleNode("//constants");
                    if (constantsNode == null)
                    {
                        constantsNode = componentNode.OwnerDocument.CreateElement("constants");
                        componentNode.AppendChild(constantsNode);
                    }
                    else
                    {
                        //clean out existing nodes - will remove existing constants
                        constantsNode.RemoveAll();
                    }

                    foreach (string factor in factorsList.Keys)
                    {
                        XmlNode factorNode = constantsNode.OwnerDocument.CreateElement("constant");
                        constantsNode.AppendChild(factorNode);
                        XmlHelper.SetAttribute(factorNode, "name", factor);
                        factorNode.InnerText = factorsList[factor];
                    }
                    constantsComponent.Contents = componentNode.OuterXml;
                }
                comp.Contents = compNode.OuterXml;
            }
            string currDirectory = Directory.GetCurrentDirectory();
            if (destFolder != "" && Directory.Exists(destFolder))
                Directory.SetCurrentDirectory(destFolder);

            string SimFileName = ApsimToSim.WriteSimFile(Simulation, Configuration.getArchitecture());

            Directory.SetCurrentDirectory(currDirectory);

            return (SimFileName);
        }

        public static void AddOutputFilesToList(Component node, List<Component> outputfiles)
        {
            if (node.Type == "outputfile")
                outputfiles.Add(node);
            foreach (Component comp in node.ChildNodes)
            {
                AddOutputFilesToList(comp, outputfiles);
            }
        }

    }
}
