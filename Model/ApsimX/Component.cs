using System;
using System.Collections.Generic;
using System.Collections.Specialized;
using System.Runtime.InteropServices;
using System.Xml;
using System.Reflection;

namespace ModelFramework
{
	public class RuntimeEventHandler
		{
		// --------------------------------------------------------------------
		// A class for representing an event handler that has been subscribed
		// to at runtime. It is a wrapper around a method in an object.
		// --------------------------------------------------------------------
		public delegate void NullFunction();

		private NullFunction F;
		}
    /// <summary>
    /// --------------------------------------------------------------------
    /// Returns the singleton instance of a reflection class that is
    /// capable of returning metadata about the structure of the simulation.
    /// --------------------------------------------------------------------
    /// </summary>
    public class Component
    {
       // protected ApsimComponent HostComponent;
        protected String FTypeName;
        protected String ParentCompName;            //Name of the parent component in the simulation
        protected String FQN;                       //Name of the actual component

        public String TypeName
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
        //public override bool IsOfType(String TypeNameToMatch)
        //{
        //    return TypeName.ToLower() == TypeNameToMatch.ToLower();
        //}
        // --------------------------------------------------------------------
        /// <summary>
        /// Constructor
        /// </summary>
        /// <param name="In">Instance of a root/leaf/shoot/phenology</param>
        // --------------------------------------------------------------------
        //public Component(Instance In)
        //{
        //    //get the name of the owner component of the Instance (e.g. Plant2)
        //    FQN = In.ParentComponent().GetName();
        //    ParentCompName = "";
        //    if (FQN.LastIndexOf('.') > -1)
        //        ParentCompName = FQN.Substring(0, FQN.LastIndexOf('.')); //e.g. Paddock
        //    //get the name of the host component for the calling object
        //    HostComponent = In.ParentComponent();   //e.g. Plant2
        //    FTypeName = HostComponent.CompClass;     //type of Plant2
        //}
        // --------------------------------------------------------------------
        /// <summary>
        /// Constructor
        /// </summary>
        /// <param name="_FullName">Name of the actual component</param>
        /// <param name="component">The apsim component that hosts this object</param>
        // --------------------------------------------------------------------
        public Component(String _FullName/*, ApsimComponent component*/)
        {
            FQN = _FullName;
            //ParentCompName = "";
            //if (FQN.LastIndexOf('.') > -1)
            //    ParentCompName = FQN.Substring(0, FQN.LastIndexOf('.'));
            //HostComponent = component;
            ////get the type for this component
            //List<TComp> comps = new List<TComp>();
            //if (_FullName != ".MasterPM") 
            //    component.Host.queryCompInfo(FQN, TypeSpec.KIND_COMPONENT, ref comps);
            //if (comps.Count > 0)
            //    FTypeName = comps[0].CompClass;
            //else
            //    FTypeName = this.GetType().Name;
        }
        // --------------------------------------------------------------------
        /// <summary>
        /// Return a list of all sibling components to caller.
        /// </summary>
        // --------------------------------------------------------------------
        //public TypedList<Component> ComponentList
        //{
        //    get
        //    {
        //        TypedList<Component> Children = new TypedList<Component>();
        //        foreach (KeyValuePair<uint, TComp> pair in HostComponent.SiblingComponents)
        //        {
        //            Component C = new Component(pair.Value.name, HostComponent);
        //            Children.Add(C);
        //        }
        //        return Children;
        //    }
        //}
        // --------------------------------------------------------------------
        /// <summary>
        /// Returns a reference to a variable.
        /// </summary>
        /// <param name="VariableName"></param>
        /// <returns></returns>
        // --------------------------------------------------------------------
        public virtual Variable Variable(String VariableName)
        {
            throw new NotImplementedException(); 
            
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
        throw new NotImplementedException();
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
            throw new NotImplementedException();
        }		
		
        // --------------------------------------------------------------------
        /// <summary>
        /// </summary>
        /// <returns>Unqualified name of the component.</returns>
        // --------------------------------------------------------------------
        public string Name
        {
			get {
            throw new NotImplementedException();
			}
        }
        // --------------------------------------------------------------------
        /// <summary>
        /// Static helper function for getting a component name (eg. wheat) from a fully
        /// qualified component name (eg. .MasterPM.paddock1.wheat)
        /// </summary>
        /// <param name="fqn"></param>
        /// <returns>Unqualified name</returns>
        // --------------------------------------------------------------------
        public static string ComponentName(String fqn)
        {
            throw new NotImplementedException();
        }
		
        //=========================================================================
        /// <summary>
        /// Returns a component that is a child of the paddock
        /// <param name="TypeToFind">The type to find. [Type.]ProxyClass</param>
        /// </summary>
        //=========================================================================
        public object LinkByType(String TypeToFind)
        {
            throw new NotImplementedException();
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
            throw new NotImplementedException();
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
                return FQN;
            }
        }
		
        public virtual List<object> ChildrenAsObjects
				{
				get {
				throw new NotImplementedException();
					}
				}

        /// <summary>
        /// 
        /// </summary>
        /// <param name="fqn"></param>
        /// <returns></returns>
        public void BuildObjects(XmlNode ScriptNode, Assembly A)
        {
            throw new NotImplementedException(); 
        }
 
 
         /// <summary>
        /// Attempts to find and return the value of a variable that matches the specified name path. 
        /// The method will return true if found or false otherwise. The value of the variable will be 
        /// returned through the out parameter.
        /// </summary>
        public bool Get(string NamePath, out int Data)
        {
            throw new NotImplementedException();
        }
        /// <summary>
        /// Attempts to find and return the value of a variable that matches the specified name path. 
        /// The method will return true if found or false otherwise. The value of the variable will be 
        /// returned through the out parameter.
        /// </summary>
        public bool Get(string NamePath, out float Data)
        {
            throw new NotImplementedException();
        }
        /// <summary>
        /// Attempts to find and return the value of a variable that matches the specified name path. 
        /// The method will return true if found or false otherwise. The value of the variable will be 
        /// returned through the out parameter.
        /// </summary>
        public bool Get(string NamePath, out double Data)
        {
            throw new NotImplementedException();
        }
        /// <summary>
        /// Attempts to find and return the value of a variable that matches the specified name path. 
        /// The method will return true if found or false otherwise. The value of the variable will be 
        /// returned through the out parameter.
        /// </summary>
        public bool Get(string NamePath, out string Data)
        {
            throw new NotImplementedException();
        }
        /// <summary>
        /// Attempts to find and return the value of a variable that matches the specified name path. 
        /// The method will return true if found or false otherwise. The value of the variable will be 
        /// returned through the out parameter.
        /// </summary>
        public bool Get(string NamePath, out int[] Data)
        {
            throw new NotImplementedException();
        }
        /// <summary>
        /// Attempts to find and return the value of a variable that matches the specified name path. 
        /// The method will return true if found or false otherwise. The value of the variable will be 
        /// returned through the out parameter.
        /// </summary>
        public bool Get(string NamePath, out float[] Data)
        {
            throw new NotImplementedException();
        }
        /// <summary>
        /// Attempts to find and return the value of a variable that matches the specified name path. 
        /// The method will return true if found or false otherwise. The value of the variable will be 
        /// returned through the out parameter.
        /// </summary>
        public bool Get(string NamePath, out double[] Data)
        {
            throw new NotImplementedException();
        }
        /// <summary>
        /// Attempts to find and return the value of a variable that matches the specified name path. 
        /// The method will return true if found or false otherwise. The value of the variable will be 
        /// returned through the out parameter.
        /// </summary>
        public bool Get(string NamePath, out string[] Data)
        {
            throw new NotImplementedException();
        }
         /// <summary>
        /// Attempts to set the value of a variable that matches the specified name path. 
        /// The method will return true if the set was successful or false otherwise.
        /// </summary>
        public bool Set(string NamePath, int Data)
        {
            throw new NotImplementedException();
        }
        /// <summary>
        /// Attempts to set the value of a variable that matches the specified name path. 
        /// The method will return true if the set was successful or false otherwise.
        /// </summary>
        public bool Set(string NamePath, float Data)
        {
            throw new NotImplementedException();
        }
        /// <summary>
        /// Attempts to set the value of a variable that matches the specified name path. 
        /// The method will return true if the set was successful or false otherwise.
        /// </summary>
        public bool Set(string NamePath, double Data)
        {
            throw new NotImplementedException();
        }
        /// <summary>
        /// Attempts to set the value of a variable that matches the specified name path. 
        /// The method will return true if the set was successful or false otherwise.
        /// </summary>
        public bool Set(string NamePath, string Data)
        {
            throw new NotImplementedException();
        }
        /// <summary>
        /// Attempts to set the value of a variable that matches the specified name path. 
        /// The method will return true if the set was successful or false otherwise.
        /// </summary>
        public bool Set(string NamePath, int[] Data)
        {
            throw new NotImplementedException();
        }
        /// <summary>
        /// Attempts to set the value of a variable that matches the specified name path. 
        /// The method will return true if the set was successful or false otherwise.
        /// </summary>
        public bool Set(string NamePath, float[] Data)
        {
            throw new NotImplementedException();
        }
        /// <summary>
        /// Attempts to set the value of a variable that matches the specified name path. 
        /// The method will return true if the set was successful or false otherwise.
        /// </summary>
        public bool Set(string NamePath, double[] Data)
        {
            throw new NotImplementedException();
        }
        /// <summary>
        /// Attempts to set the value of a variable that matches the specified name path. 
        /// The method will return true if the set was successful or false otherwise.
        /// </summary>
        public bool Set(string NamePath, string[] Data)
        {
            throw new NotImplementedException();
        }

 
    }

}