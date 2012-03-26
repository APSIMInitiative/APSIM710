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
		}

    public class Component
    {
        internal ModelInstance Instance;
        protected String FTypeName;
        protected String ParentCompName;            //Name of the parent component in the simulation
        protected String FQN;                       //Name of the actual component
        protected string _Name;

        public String TypeName
        {
            get { return FTypeName; }
        }

        /// <summary>
        /// Constructor
        /// </summary>
        internal Component(ModelInstance Inst)
        {
            Instance = Inst;
            _Name = Inst.Name;
            FQN = Inst.FullName;
        }

        /// <summary>
        /// Return a list of children to caller.
        /// <summary>
        public List<Component> Children
        {
            get
            {
                List<Component> Childs = new List<Component>();
                foreach (ModelInstance Inst in Instance.Children)
                {
                    Childs.Add(new Component(Inst));
                }
                return Childs;
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
            Instance.AddSubscriber(EventName, F.Method, F.Target);
            Instance.ConnectEvents();
        }		
		
        // --------------------------------------------------------------------
        /// <summary>
        /// </summary>
        /// <returns>Unqualified name of the component.</returns>
        // --------------------------------------------------------------------
        public string Name
        {
			get {
                return _Name;
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
		
        /// <summary>
        /// Returns a component that is a child of the paddock
        /// </summary>
        public object LinkByType(String TypeToFind)
        {
            return Instance.FindModelByType(TypeToFind);
        }
        /// <summary>
        /// Return a child component of the paddock by unqualified name.
        /// </summary>
        public object LinkByName(string NameToFind)
        {
            return Instance.FindModelByName(NameToFind);
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
            get
            {
                List<object> Childs = new List<object>();
                foreach (ModelInstance Inst in Instance.Children)
                    Childs.Add(Inst.TheModel);
                return Childs;
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
        public bool Get(string NamePath, out object Data)
        {
            return Instance.Get(NamePath, out Data);
        }
         /// <summary>
        /// Attempts to find and return the value of a variable that matches the specified name path. 
        /// The method will return true if found or false otherwise. The value of the variable will be 
        /// returned through the out parameter.
        /// </summary>
        public bool Get(string NamePath, out int Data)
        {
            return Instance.Get(NamePath, out Data);
        }
        /// <summary>
        /// Attempts to find and return the value of a variable that matches the specified name path. 
        /// The method will return true if found or false otherwise. The value of the variable will be 
        /// returned through the out parameter.
        /// </summary>
        public bool Get(string NamePath, out float Data)
        {
            return Instance.Get(NamePath, out Data);
        }
        /// <summary>
        /// Attempts to find and return the value of a variable that matches the specified name path. 
        /// The method will return true if found or false otherwise. The value of the variable will be 
        /// returned through the out parameter.
        /// </summary>
        public bool Get(string NamePath, out double Data)
        {
            return Instance.Get(NamePath, out Data);
        }
        /// <summary>
        /// Attempts to find and return the value of a variable that matches the specified name path. 
        /// The method will return true if found or false otherwise. The value of the variable will be 
        /// returned through the out parameter.
        /// </summary>
        public bool Get(string NamePath, out string Data)
        {
            return Instance.Get(NamePath, out Data);
        }
        /// <summary>
        /// Attempts to find and return the value of a variable that matches the specified name path. 
        /// The method will return true if found or false otherwise. The value of the variable will be 
        /// returned through the out parameter.
        /// </summary>
        public bool Get(string NamePath, out int[] Data)
        {
            return Instance.Get(NamePath, out Data);
        }
        /// <summary>
        /// Attempts to find and return the value of a variable that matches the specified name path. 
        /// The method will return true if found or false otherwise. The value of the variable will be 
        /// returned through the out parameter.
        /// </summary>
        public bool Get(string NamePath, out float[] Data)
        {
            return Instance.Get(NamePath, out Data);
        }
        /// <summary>
        /// Attempts to find and return the value of a variable that matches the specified name path. 
        /// The method will return true if found or false otherwise. The value of the variable will be 
        /// returned through the out parameter.
        /// </summary>
        public bool Get(string NamePath, out double[] Data)
        {
            return Instance.Get(NamePath, out Data);
        }
        /// <summary>
        /// Attempts to find and return the value of a variable that matches the specified name path. 
        /// The method will return true if found or false otherwise. The value of the variable will be 
        /// returned through the out parameter.
        /// </summary>
        public bool Get(string NamePath, out string[] Data)
        {
            return Instance.Get(NamePath, out Data);
        }
         /// <summary>
        /// Attempts to set the value of a variable that matches the specified name path. 
        /// The method will return true if the set was successful or false otherwise.
        /// </summary>
        public bool Set(string NamePath, int Data)
        {
            return Instance.Set(NamePath, Data);
        }
        /// <summary>
        /// Attempts to set the value of a variable that matches the specified name path. 
        /// The method will return true if the set was successful or false otherwise.
        /// </summary>
        public bool Set(string NamePath, float Data)
        {
            return Instance.Set(NamePath, Data);
        }
        /// <summary>
        /// Attempts to set the value of a variable that matches the specified name path. 
        /// The method will return true if the set was successful or false otherwise.
        /// </summary>
        public bool Set(string NamePath, double Data)
        {
            return Instance.Set(NamePath, Data);
        }
        /// <summary>
        /// Attempts to set the value of a variable that matches the specified name path. 
        /// The method will return true if the set was successful or false otherwise.
        /// </summary>
        public bool Set(string NamePath, string Data)
        {
            return Instance.Set(NamePath, Data);
        }
        /// <summary>
        /// Attempts to set the value of a variable that matches the specified name path. 
        /// The method will return true if the set was successful or false otherwise.
        /// </summary>
        public bool Set(string NamePath, int[] Data)
        {
            return Instance.Set(NamePath, Data);
        }
        /// <summary>
        /// Attempts to set the value of a variable that matches the specified name path. 
        /// The method will return true if the set was successful or false otherwise.
        /// </summary>
        public bool Set(string NamePath, float[] Data)
        {
            return Instance.Set(NamePath, Data);
        }
        /// <summary>
        /// Attempts to set the value of a variable that matches the specified name path. 
        /// The method will return true if the set was successful or false otherwise.
        /// </summary>
        public bool Set(string NamePath, double[] Data)
        {
            return Instance.Set(NamePath, Data);
        }
        /// <summary>
        /// Attempts to set the value of a variable that matches the specified name path. 
        /// The method will return true if the set was successful or false otherwise.
        /// </summary>
        public bool Set(string NamePath, string[] Data)
        {
            return Instance.Set(NamePath, Data);
        }

        /// <summary>
        /// Add a new model to the simulation. The ModelDescription describes the parameterisation of
        /// the model. The ModelAssembly contains the model.
        /// </summary>
        public void AddModel(XmlNode ModelDescription, Assembly ModelAssembly)
        {
            Instance.AddModel(ModelDescription, ModelAssembly);
        }
 

    }
}