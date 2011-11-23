using System;
using System.Collections.Generic;
using System.Collections.Specialized;
using System.Runtime.InteropServices;
using CMPServices;
using System.Xml;
using System.Reflection;

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
        protected ApsimComponent HostComponent;
        protected String FTypeName;
        protected String ParentCompName;            //Name of the parent component in the simulation
        protected String FQN;                       //Name of the actual component

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
        public Component(Instance In)
        {
            //get the name of the owner component of the Instance (e.g. Plant2)
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
        public Component(String _FullName, ApsimComponent component)
        {
            FQN = _FullName;
            ParentCompName = "";
            if (FQN.LastIndexOf('.') > -1)
                ParentCompName = FQN.Substring(0, FQN.LastIndexOf('.'));
            HostComponent = component;
            //get the type for this component
            List<TComp> comps = new List<TComp>();
            if (_FullName != ".MasterPM") 
                component.Host.queryCompInfo(FQN, TypeSpec.KIND_COMPONENT, ref comps);
            if (comps.Count > 0)
                FTypeName = comps[0].CompClass;
            else
                FTypeName = this.GetType().Name;
        }

        // --------------------------------------------------------------------
        /// <summary>
        /// Returns a reference to a variable.
        /// </summary>
        /// <param name="VariableName"></param>
        /// <returns></returns>
        // --------------------------------------------------------------------
        protected virtual Variable Variable(String VariableName)
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
        protected virtual void Publish(String EventName)
        {
            HostComponent.Publish(FQN + "." + EventName, null);
        }
        // --------------------------------------------------------------------
        /// <summary>
        /// Publish an event that has associated data to this component only.
        /// </summary>
        /// <param name="EventName"></param>
        /// <param name="Data"></param>
        // --------------------------------------------------------------------
        protected virtual void Publish(String EventName, ApsimType Data)
        {
            HostComponent.Publish(FQN + "." + EventName, Data);
        }
        // --------------------------------------------------------------------
        /// <summary>
        /// Subscribe to a notification event ie. one without any data associated with it.
        /// </summary>
        /// <param name="EventName"></param>
        /// <param name="F"></param>
        // --------------------------------------------------------------------
        protected virtual void Subscribe(String EventName, RuntimeEventHandler.NullFunction F)
        {
            RuntimeEventHandler Event = new RuntimeEventHandler(EventName, F);
            HostComponent.Subscribe(Event);
        }
        // --------------------------------------------------------------------
        /// <summary>
        /// </summary>
        /// <returns>Unqualified name of the component.</returns>
        // --------------------------------------------------------------------
        public String Name()
        {
            return TRegistrar.unQualifiedName(FQN);
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
 
    }

}