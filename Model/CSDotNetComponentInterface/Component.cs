using System;
using System.Collections.Generic;
using System.Collections.Specialized;
using System.Runtime.InteropServices;
using CMPServices;
using System.Xml;
using System.Reflection;
using CSGeneral;

namespace ModelFramework
{
    /// <summary>
    /// --------------------------------------------------------------------
    /// Returns the singleton instance of a reflection class that is
    /// capable of returning metadata about the structure of the simulation.
    /// --------------------------------------------------------------------
    /// </summary>
    public class Component : TypedItem
    {
        ApsimComponent HostComponent;
        protected String FTypeName;
        protected String ParentCompName;            //Name of the parent component in the simulation
        protected String FQN;                       //Name of the actual component
        internal Instance In;
        protected Dictionary<uint, TComp> ChildComponents = null;
        protected string NamePrefix = "";

        private String TypeName
        {
            get { return FTypeName; }
        }
        // --------------------------------------------------------------------
        /// <summary>
        /// 
        /// </summary>
        /// <param name="TypeNameToMatch"></param>
        /// <returns></returns>
        // --------------------------------------------------------------------
        internal override bool IsOfType(String TypeNameToMatch)
        {
            string matchString = TypeNameToMatch.ToLower();
            // If the search name is compound (e.g., plant2.wheat),
            // match the full string
            if (TypeNameToMatch.IndexOf('.') > 0)
            {
                return TypeName.ToLower() == matchString;
            }
            else
            // Search name is not compound
            {
                int dotPos = TypeName.IndexOf('.');
                // If our typename is compound, allow a match of either half
                // That is, if our typename is "plant2.wheat", then both
                // IsOfType("plant2") and IsOfType("wheat") return true
                if (dotPos > 0)
                {
                    string left = TypeName.Substring(0, dotPos);
                    string right = TypeName.Substring(dotPos + 1);
                    return matchString == left.ToLower() || matchString == right.ToLower();
                }
                // If our typename is not compound, it's just a simple match
                else
                {
                    return TypeName.ToLower() == matchString;
                }
            }
        }
        // --------------------------------------------------------------------
        /// <summary>
        /// Constructor
        /// </summary>
        /// <param name="In">Instance of a root/leaf/shoot/phenology</param>
        // --------------------------------------------------------------------
        internal Component(Instance _In)
        {
            In = _In;

            //get the name of the owner component of the Instance (e.g. Plant2)
            NamePrefix = FQN + ".";
            FQN = In.ParentComponent().GetName();
            ParentCompName = "";
            if (FQN.LastIndexOf('.') > -1)
                ParentCompName = FQN.Substring(0, FQN.LastIndexOf('.')); //e.g. Paddock
            //get the name of the host component for the calling object
            HostComponent = In.ParentComponent();   //e.g. Plant2
            FTypeName = HostComponent.CompClass;     //type of Plant2
        }
        // --------------------------------------------------------------------
        /// <summary>
        /// Constructor
        /// </summary>
        /// <param name="_FullName">Name of the actual component</param>
        /// <param name="component">The apsim component that hosts this object</param>
        // --------------------------------------------------------------------
        public Component(String _FullName, object component)
        {
            FQN = _FullName;
            NamePrefix = FQN + ".";
            ParentCompName = "";
            if (FQN.LastIndexOf('.') > -1)
                ParentCompName = FQN.Substring(0, FQN.LastIndexOf('.'));
            HostComponent = component as ApsimComponent;
            //get the type for this component
            List<TComp> comps = new List<TComp>();
            if (_FullName != ".MasterPM")
                HostComponent.Host.queryCompInfo(FQN, TypeSpec.KIND_COMPONENT, ref comps);
            if (comps.Count > 0)
                FTypeName = comps[0].CompClass;
            else
                FTypeName = this.GetType().Name;
        }

        // --------------------------------------------------------------------
        /// <summary>
        /// Return a list of all child paddock components to caller.
        /// </summary>
        // --------------------------------------------------------------------
        public virtual List<Component> Children
        {
            get
            {
                List<Component> Children = new List<Component>();
                Instance Inst = In;
                if (Inst == null)
                    Inst = HostComponent.ModelInstance;
                if (Inst == null)
                {
                    queryChildComponents();
                    foreach (KeyValuePair<uint, TComp> pair in ChildComponents)
                    {
                        Paddock C = new Paddock(pair.Value.name, HostComponent);
                        Children.Add(C);
                    }
                }
                else
                {
                    foreach (Instance Child in Inst.Children)
                        Children.Add(new Component(Child));
                }
                return Children;
            }
        }

        // --------------------------------------------------------------------
        /// <summary>
        /// Return a list of all child paddock components to caller.
        /// </summary>
        // --------------------------------------------------------------------
        public virtual List<object> ChildrenAsObjects
        {
            get
            {
                List<object> Children = new List<object>();
                Instance Inst = In;
                if (Inst == null)
                    Inst = HostComponent.ModelInstance;
                if (Inst != null)
                {
                    foreach (Instance Child in Inst.Children)
                        Children.Add(Child.Model);
                }
                return Children;
            }
        }

        // --------------------------------------------------------------------
        /// <summary>
        /// Returns a reference to a variable.
        /// </summary>
        /// <param name="VariableName"></param>
        /// <returns></returns>
        // --------------------------------------------------------------------
        public virtual Variable Variable(String VariableName)
        {
            return new Variable(HostComponent, FQN + '.' + VariableName);
        }
        // --------------------------------------------------------------------
        /// <summary>
        /// Publish a notification event (i.e. one that doesn't have any data 
        /// associated with it) to this component only.
        /// <param name="EventName"></param>
        /// </summary>
        // --------------------------------------------------------------------
        public virtual void Publish(String EventName)
        {
            HostComponent.Publish(NamePrefix + EventName, new NullType());
        }
        // --------------------------------------------------------------------
        /// <summary>
        /// Publish an event that has associated data to this component only.
        /// </summary>
        /// <param name="EventName"></param>
        /// <param name="Data"></param>
        // --------------------------------------------------------------------
        public virtual void Publish(String EventName, ApsimType Data)
        {
            HostComponent.Publish(NamePrefix + EventName, Data);
        }
        // --------------------------------------------------------------------
        /// <summary>
        /// Subscribe to a notification event ie. one without any data associated with it.
        /// </summary>
        /// <param name="EventName"></param>
        /// <param name="F"></param>
        // --------------------------------------------------------------------
        public virtual void Subscribe(String EventName, RuntimeEventHandler.NullFunction F)
        {
            RuntimeEventHandler Event = new RuntimeEventHandler(NamePrefix + EventName, F);
            HostComponent.Subscribe(Event);
        }
        //
        /// <summary>
        /// Manually register a property.
        /// This is NOT meant to be the normal way to do things. It is here largely for
        /// backwards compatibility.
        /// </summary>
        /// <param name="sName">name of the property</param>
        /// <param name="sDDML">DDML describing the property type</param>
        /// <param name="read">can the property be read</param>
        /// <param name="write">can the property be written</param>
        /// <param name="init">is it an initialisation value</param>
        /// <param name="sDescr">short description</param>
        /// <param name="sFullDescr">full description</param>
        /// <param name="pDelegate">delegate pointing to a function handling requests to read or write the property value</param>
        /// <returns></returns>
        public virtual int RegisterProperty(String sName, String sDDML, Boolean read, Boolean write, Boolean init, String sDescr, String sFullDescr, propertyDelegate pDelegate)
        {
            return HostComponent.RegisterProperty(sName, sDDML, read, write, init, sDescr, sFullDescr, pDelegate);
        }

        // --------------------------------------------------------------------
        /// <summary>
        /// </summary>
        /// <returns>Unqualified name of the component.</returns>
        // --------------------------------------------------------------------
        public String Name
        {
            get
            {
                return TRegistrar.unQualifiedName(FullName);
            }
        }
        // --------------------------------------------------------------------
        /// <summary>
        /// Fully qualified name of component eg. .MasterPM.paddock1.wheat
        /// </summary>
        // --------------------------------------------------------------------
        public String FullName
        {
            get
            {
                string CompleteName = FQN;
                if (In != null)
                {
                    if (In.Name.Contains("."))
                        CompleteName = StringManip.ParentName(FQN);
                    CompleteName += "." + In.Name;
                }
                return RemoveMasterPM(CompleteName);
            }
        }

        //=========================================================================
        /// <summary>
        /// Returns a component that is a child of the paddock
        /// <param name="TypeToFind">The type to find. [Type.]ProxyClass</param>
        /// </summary>
        //=========================================================================
        public object LinkByType(String TypeToFind)
        {
            return LinkField.FindApsimObject(TypeToFind, null, FQN, HostComponent);
        }
        //=========================================================================
        /// <summary>
        /// Return a child component of the paddock by unqualified name.
        /// </summary>
        /// <param name="NameToFind">Unqualified name</param>
        /// <returns></returns>
        //=========================================================================
        public object LinkByName(String NameToFind)
        {
            object E = FindInternalEntity(NameToFind);
            if (E != null)
            {
                if (E is Entity)
                {
                    object Value = (E as Entity).Get();
                    return Value;
                }
                else if (E is Instance)
                {
                    object Value = (E as Instance).Model;
                    return Value;
                }

                return null;
            }
            else
                return LinkField.FindApsimObject(null, 
                                                 AddMasterPM(NameToFind), 
                                                 StringManip.ParentName(HostComponent.InstanceName),
                                                 HostComponent);
        }

        /// <summary>
        /// Add a new model to the simulation. The ModelDescription describes the parameterisation of
        /// the model. The ModelAssembly contains the model.
        /// </summary>
        public void AddModel(XmlNode ModelDescription, Assembly ModelAssembly)
        {
            HostComponent.BuildObjects(ModelDescription, ModelAssembly);
        }

        /// <summary>
        /// Send a warning message.
        /// </summary>
        public void Warning(string Message)
        {
            HostComponent.Warning(Message);
        }


        #region Get methods
        Dictionary<string, Entity> VariableCache = new Dictionary<string, Entity>();

        /// <summary>
        /// Attempts to find and return the value of a variable that matches the specified name path. 
        /// The method will return true if found or false otherwise. The value of the variable will be 
        /// returned through the out parameter.
        /// </summary>
        public bool Get(string NamePath, out object Data)
        {
            //// Look in cache first.
            if (VariableCache.ContainsKey(NamePath))
            {
                Data = VariableCache[NamePath].Get();
                return true;
            }

            Data = FindInternalEntity(NamePath);
            if (Data is Entity)
            {
                VariableCache.Add(NamePath, Data as Entity);
                Data = (Data as Entity).Get();
                return true;
            }
            else if (Data is Instance)
                Data = (Data as Instance).Model;
            if (Data == null)
            {
                 WrapBuiltInVariable<double> Value = new WrapBuiltInVariable<double>();
                 if (GetInternal<double>(NamePath, Value))
                     Data = Value.Value;
            }
            return Data != null;
        }
        /// <summary>
        /// Attempts to find and return the value of a variable that matches the specified name path. 
        /// The method will return true if found or false otherwise. The value of the variable will be 
        /// returned through the out parameter.
        /// </summary>
        public bool Get(string NamePath, out int Data)
        {
            WrapBuiltInVariable<int> Value = new WrapBuiltInVariable<int>();
            if (GetInternal<int>(NamePath, Value))
            {
                Data = Value.Value;
                return true;
            }
            else
            {
                Data = Int32.MaxValue;
                return false;
            }
        }
        /// <summary>
        /// Attempts to find and return the value of a variable that matches the specified name path. 
        /// The method will return true if found or false otherwise. The value of the variable will be 
        /// returned through the out parameter.
        /// </summary>
        public bool Get(string NamePath, out float Data)
        {
            WrapBuiltInVariable<float> Value = new WrapBuiltInVariable<float>();
            if (GetInternal<float>(NamePath, Value))
            {
                Data = Value.Value;
                return true;
            }
            else
            {
                Data = Single.NaN;
                return false;
            }
        }
        /// <summary>
        /// Attempts to find and return the value of a variable that matches the specified name path. 
        /// The method will return true if found or false otherwise. The value of the variable will be 
        /// returned through the out parameter.
        /// </summary>
        public bool Get(string NamePath, out double Data)
        {
            WrapBuiltInVariable<double> Value = new WrapBuiltInVariable<double>();
            if (GetInternal<double>(NamePath, Value))
            {
                Data = Value.Value;
                return true;
            }
            else
            {
                Data = Double.NaN;
                return false;
            }
        }
        /// <summary>
        /// Attempts to find and return the value of a variable that matches the specified name path. 
        /// The method will return true if found or false otherwise. The value of the variable will be 
        /// returned through the out parameter.
        /// </summary>
        public bool Get(string NamePath, out string Data)
        {
            WrapBuiltInVariable<string> Value = new WrapBuiltInVariable<string>();
            if (GetInternal<string>(NamePath, Value))
            {
                Data = Value.Value;
                return true;
            }
            else
            {
                Data = "";
                return false;
            }
        }
        /// <summary>
        /// Attempts to find and return the value of a variable that matches the specified name path. 
        /// The method will return true if found or false otherwise. The value of the variable will be 
        /// returned through the out parameter.
        /// </summary>
        public bool Get(string NamePath, out int[] Data)
        {
            WrapBuiltInVariable<int[]> Value = new WrapBuiltInVariable<int[]>();
            if (GetInternal<int[]>(NamePath, Value))
            {
                Data = Value.Value;
                return true;
            }
            else
            {
                Data = null;
                return false;
            }
        }
        /// <summary>
        /// Attempts to find and return the value of a variable that matches the specified name path. 
        /// The method will return true if found or false otherwise. The value of the variable will be 
        /// returned through the out parameter.
        /// </summary>
        public bool Get(string NamePath, out float[] Data)
        {
            WrapBuiltInVariable<float[]> Value = new WrapBuiltInVariable<float[]>();
            if (GetInternal<float[]>(NamePath, Value))
            {
                Data = Value.Value;
                return true;
            }
            else
            {
                Data = null;
                return false;
            }
        }
        /// <summary>
        /// Attempts to find and return the value of a variable that matches the specified name path. 
        /// The method will return true if found or false otherwise. The value of the variable will be 
        /// returned through the out parameter.
        /// </summary>
        public bool Get(string NamePath, out double[] Data)
        {
            WrapBuiltInVariable<double[]> Value = new WrapBuiltInVariable<double[]>();
            if (GetInternal<double[]>(NamePath, Value))
            {
                Data = Value.Value;
                return true;
            }
            else
            {
                Data = null;
                return false;
            }
        }
        /// <summary>
        /// Attempts to find and return the value of a variable that matches the specified name path. 
        /// The method will return true if found or false otherwise. The value of the variable will be 
        /// returned through the out parameter.
        /// </summary>
        public bool Get(string NamePath, out string[] Data)
        {
            WrapBuiltInVariable<string[]> Value = new WrapBuiltInVariable<string[]>();
            if (GetInternal<string[]>(NamePath, Value))
            {
                Data = Value.Value;
                return true;
            }
            else
            {
                Data = null;
                return false;
            }
        }
        #endregion

        #region Set methods
        /// <summary>
        /// Attempts to set the value of a variable that matches the specified name path. 
        /// The method will return true if the set was successful or false otherwise.
        /// </summary>
        public bool Set(string NamePath, int Data)
        {
            return SetInternal<int>(NamePath, Data);
        }
        /// <summary>
        /// Attempts to set the value of a variable that matches the specified name path. 
        /// The method will return true if the set was successful or false otherwise.
        /// </summary>
        public bool Set(string NamePath, float Data)
        {
            return SetInternal<float>(NamePath, Data);
        }
        /// <summary>
        /// Attempts to set the value of a variable that matches the specified name path. 
        /// The method will return true if the set was successful or false otherwise.
        /// </summary>
        public bool Set(string NamePath, double Data)
        {
            return SetInternal<double>(NamePath, Data);
        }
        /// <summary>
        /// Attempts to set the value of a variable that matches the specified name path. 
        /// The method will return true if the set was successful or false otherwise.
        /// </summary>
        public bool Set(string NamePath, string Data)
        {
            return SetInternal<string>(NamePath, Data);
        }
        /// <summary>
        /// Attempts to set the value of a variable that matches the specified name path. 
        /// The method will return true if the set was successful or false otherwise.
        /// </summary>
        public bool Set(string NamePath, int[] Data)
        {
            return SetInternal<int[]>(NamePath, Data);
        }
        /// <summary>
        /// Attempts to set the value of a variable that matches the specified name path. 
        /// The method will return true if the set was successful or false otherwise.
        /// </summary>
        public bool Set(string NamePath, float[] Data)
        {
            return SetInternal<float[]>(NamePath, Data);
        }
        /// <summary>
        /// Attempts to set the value of a variable that matches the specified name path. 
        /// The method will return true if the set was successful or false otherwise.
        /// </summary>
        public bool Set(string NamePath, double[] Data)
        {
            return SetInternal<double[]>(NamePath, Data);
        }
        /// <summary>
        /// Attempts to set the value of a variable that matches the specified name path. 
        /// The method will return true if the set was successful or false otherwise.
        /// </summary>
        public bool Set(string NamePath, string[] Data)
        {
            return SetInternal<string[]>(NamePath, Data);
        }
        #endregion

        #region Private methods
        // --------------------------------------------------------------------
        /// <summary>
        /// Go looking for child components of this component.
        /// </summary>
        // --------------------------------------------------------------------
        protected void queryChildComponents()
        {
            if (ChildComponents == null)
            {
                ChildComponents = new Dictionary<uint, TComp>();

                String sSearchName = FQN + ".*";    //search comp.*

                List<TComp> comps = new List<TComp>();
                HostComponent.Host.queryCompInfo(sSearchName, TypeSpec.KIND_COMPONENT, ref comps);
                ChildComponents.Clear();
                for (int i = 0; i < comps.Count; i++)
                {
                    ChildComponents.Add(comps[i].compID, comps[i]);
                }

            }
        }

        /// <summary>
        /// Locate a variable that matches the specified path and return its value. Returns null
        /// if not found. e.g. NamePath:
        ///     "Plant"   - no path specified so look in scope
        ///     "Phenology.CurrentPhase" - relative path specified so look for matching child
        ///     ".met.maxt" - absolute path specified so look for exact variable.
        /// </summary>    
        private bool GetInternal<T>(string NamePath, WrapBuiltInVariable<T> Data)
        {
            NamePath = AddMasterPM(NamePath);
            if (NamePath.Length > 0 && NamePath[0] == '.')
            {
                // absolute path.
                {
                    bool ok = HostComponent.Get(NamePath, Data, true);
                    return Data.Value != null;
                }
            }
            else if (NamePath.Contains("."))
            {
                // relative path.
                // assume internal entity.
                object E = FindInternalEntity(NamePath);
                if (E != null && E is Entity)
                {
                    Data.setValue((E as Entity).Get());
                    return true;
                }
                else
                {
                    // NamePath might be Leaf.MinT (which doesn't exist anywhere in the simulation
                    // If we pass this to HostComponent.Get, it will throw - we don't want that.
                    bool isAusFarm = FullName[0] != '.';
                    if (isAusFarm || IsComponentASibling(StringManip.ParentName(NamePath)))
                        return HostComponent.Get(NamePath, Data, true);
                    else
                        return false;
                }
            }
            else
            {
                // no path
                // First look for an internal entity.
                object E = FindInternalEntity(NamePath);
                if (E != null && E is Entity)
                {
                    Data.setValue((E as Entity).Get());
                    return true;
                }
                else
                {
                    // external variable
                    // try a full address first. If that doesn't work then try relative address.
                    if (!HostComponent.Get(NamePrefix + NamePath, Data, true))
                        return HostComponent.Get(NamePath, Data, true);
                    else
                        return true;
                }
            }
        }

        /// <summary>
        /// Set the value of a variable.
        /// </summary>
        private bool SetInternal<T>(string NamePath, T Value)
        {
            NamePath = AddMasterPM(NamePath);
            WrapBuiltInVariable<T> Data = new WrapBuiltInVariable<T>();
            Data.Value = Value;
            if (NamePath.Length > 0 && NamePath[0] == '.')
            {
                // absolute path.
                HostComponent.Set(NamePath, Data);
                return true;
            }
            else if (NamePath.Contains("."))
            {
                // relative path.
                object E = FindInternalEntity(NamePath);
                if (E != null && E is Entity)
                    return (E as Entity).Set(Data);
                else
                    return false;
            }
            else
            {
                // no path
                // First look for an internal entity.
                object E = FindInternalEntity(NamePath);
                if (E != null && E is Entity)
                {
                    Data.setValue((E as Entity).Get());
                    return true;
                }
                else
                {
                    // not an internal entity so look for an external one.
                    HostComponent.Set(NamePrefix + NamePath, Data);
                    return true;
                }

            }

        }


        private class Entity
        {
            public MemberInfo MI;
            public object Obj;

            public object Get()
            {
                if (MI is FieldInfo)
                {
                    FieldInfo FI = MI as FieldInfo;
                    return FI.GetValue(Obj);
                }
                else
                {
                    PropertyInfo PI = MI as PropertyInfo;
                    return PI.GetValue(Obj, null);
                }



            }
            public bool Set(object Value)
            {
                if (MI is FieldInfo)
                {
                    FieldInfo FI = MI as FieldInfo;
                    FI.SetValue(Obj, Value);
                }
                else
                {
                    PropertyInfo PI = MI as PropertyInfo;
                    PI.SetValue(Obj, Value, null);
                }
                return true;
            }
        }

        /// <summary>
        /// Return the value (using Reflection) of the specified property on the specified object.
        /// Returns null if not found.
        /// </summary>
        private object FindInternalEntity(string NamePath)
        {
            NamePath = AddMasterPM(NamePath);
            Instance RelativeTo = In;
            if (In == null)
                RelativeTo = HostComponent.ModelInstance;

            if (RelativeTo != null)
            {
                object Value = null;
                do
                {
                    // NamePath will sometimes be absolute - convert to relative for the following method to work.
                    NamePath = NamePath.Replace(RelativeTo.ParentComponent().InstanceName + ".", "");
                    Value = FindChildEntity(NamePath, RelativeTo);
                    RelativeTo = RelativeTo.Parent;
                }
                while (Value == null && RelativeTo != null);
                return Value;
            }
            else
                return null;
        }

        /// <summary>
        /// Return the value (using Reflection) of the specified property on the specified object.
        /// Returns null if not found.
        /// </summary>
        private static object FindChildEntity(string NamePath, Instance RelativeTo)
        {
            string[] Bits = NamePath.Split(".".ToCharArray(), StringSplitOptions.RemoveEmptyEntries);
            for (int i = 0; i < Bits.Length; i++)
            {
                Instance MatchingChild = null;
                for (int j = 0; j < RelativeTo.Children.Count; j++)
                {
                    if (RelativeTo.Children[j] is Instance)
                    {
                        Instance Child = (Instance)RelativeTo.Children[j];
                        if (Child.Name.ToLower() == Bits[i].ToLower())
                        {
                            MatchingChild = Child;
                            break;
                        }
                    }
                    else
                        throw new Exception("Invalid child found: " + RelativeTo.Children[j].Name);
                }

                if (MatchingChild == null)
                {
                    // If we get this far then we didn't find the child. If it's the last bit of the name path
                    // then perhaps it is a field or property - go have a look.
                    if (i == Bits.Length - 1)
                    {
                        FieldInfo FI = RelativeTo.Model.GetType().GetField(Bits[i], BindingFlags.NonPublic | BindingFlags.Instance | BindingFlags.Public);
                        if (FI == null)
                        {
                            PropertyInfo PI = RelativeTo.Model.GetType().GetProperty(Bits[i], BindingFlags.NonPublic | BindingFlags.Instance | BindingFlags.Public);
                            if (PI != null)
                                return new Entity { MI = PI, Obj = RelativeTo.Model };
                        }
                        else
                            return new Entity { MI = FI, Obj = RelativeTo.Model };
                    }
                    return null;
                }
                else
                    RelativeTo = MatchingChild;
            }

            // If we get this far then we've found a match.
            return RelativeTo;
        }

        private bool IsComponentASibling(string ComponentName)
        {
            foreach (KeyValuePair<uint, TComp> Sibling in HostComponent.SiblingComponents)
            {
                string SiblingShortName = Sibling.Value.name.Substring(Sibling.Value.name.LastIndexOf('.') + 1);
                if (SiblingShortName.ToLower() == ComponentName.ToLower())
                    return true;
            }
            return false;
        }

        /// <summary>
        /// Add in .MasterPM to the front of the specified St
        /// </summary>
        protected static string AddMasterPM(string St)
        {
            if (St == ".")
                return ".MasterPM";
            else if (St.Length > 0 && St[0] == '.' && !St.Contains(".MasterPM"))
                return ".MasterPM" + St;
            else
                return St;
        }

        /// <summary>
        /// Remove .MasterPM from the front of the specified St
        /// </summary>
        protected static string RemoveMasterPM(string St)
        {
            if (St.Contains(".MasterPM"))
                return St.Remove(0, 9);
            else
                return St;
        }

        #endregion

    }

}