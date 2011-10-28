using System;
using System.Text;
using System.Collections.Generic;
using System.Collections.Specialized;
using System.Runtime.InteropServices;
using ModelFramework;

namespace ModelFramework
{

    public class Paddock : Component
    {
        //public Dictionary<uint, TComp> ChildComponents;
        // --------------------------------------------------------------------
        /// <summary>
        /// Encapsulates an APSIM paddock in a simulation.
        /// </summary>
        /// <param name="Nam">Name of the paddock or system</param>
        /// <param name="component">The hosting component</param>
        // --------------------------------------------------------------------
        public Paddock(String Nam /*, ApsimComponent component*/)
            : base(Nam)
        {
            //ChildComponents = new Dictionary<uint, TComp>();
            queryChildComponents(Nam);
        }
        // --------------------------------------------------------------------
        /// <summary>
        /// 
        /// </summary>
        /// <param name="In">Instance of a root/leaf/shoot/phenology</param>
        // --------------------------------------------------------------------
        //public Paddock(Instance In)
        //    : base(In)
        //{
        //    if (Types.Instance.TypeNames.Length == 0)
        //        PlugIns.LoadAll();
        //}
        public new String Name
        {
            get
            {
                return base.Name(); //name of the component (Paddock)
            }
        }
        // --------------------------------------------------------------------
        /// <summary>
        /// 
        /// </summary>
        /// <param name="FQN"></param>
        /// <returns></returns>
        // --------------------------------------------------------------------
        protected int queryChildComponents(String FQN)
        {
            throw new NotImplementedException();
        }

        // --------------------------------------------------------------------
        /// <summary>
        /// Return a list of all child paddock components to caller.
        /// </summary>
        // --------------------------------------------------------------------
        public List<Paddock> SubPaddocks
        {
            get
            {
                List<Paddock> Children = new List<Paddock>();
                //foreach (KeyValuePair<uint, TComp> pair in ChildComponents)
                //{
                //    if (pair.Value.CompClass.ToLower() == "paddock" || pair.Value.CompClass.ToLower() == "protocolmanager")
                //    {
                //        Paddock C = new Paddock(pair.Value.name, HostComponent);
                //        Children.Add(C);
                //    }
                //}
                return Children;
            }
        }
        //=========================================================================
        /// <summary>
        /// Returns a component that is a child of the paddock
        /// </summary>
        //=========================================================================
        public Component ComponentByType(String TypeToFind)
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
        public Component ComponentByName(String NameToFind)
        {
            throw new NotImplementedException();
        }
        /// <summary>
        /// Returns a reference to a variable.
        /// <param name="VariableName"></param>
        /// </summary>
        public override Variable Variable(String VariableName)
        {
            throw new NotImplementedException();
        }
        //=========================================================================
        /// <summary>
        /// Publish a notification event i.e. one that doesn't have any data 
        /// associated with it. This event is broadcast to all components within scope.
        /// </summary>
        //=========================================================================
        public override void Publish(String EventName)
        {
            throw new NotImplementedException();
        }
        //=========================================================================
        /// <summary>
        /// Publish an event that has associated data. This event is broadcast to all components within scope.
        /// </summary>
        /// <param name="EventName"></param>
        /// <param name="Data"></param>
        //=========================================================================
        //public override void Publish(String EventName/*, ApsimType Data*/)
        //{
        //    throw new NotImplementedException();
        //}
        //// --------------------------------------------------------------------
        /// <summary>
        /// Return a list of all child crops to caller.
        /// </summary>
        // --------------------------------------------------------------------
        public List<Component> Crops
        {
            get
            {
                throw new NotImplementedException();
            }
        }
    }

    /// <summary>
    /// 
    /// </summary>
    //public class MyPaddock
    //{
    //    private static Paddock _Singleton = null;
    //    public Paddock this[String ComponentName]
    //    {
    //        get
    //        {
    //            if (_Singleton == null)
    //                _Singleton = new Paddock(ComponentName, null);
    //            return _Singleton;
    //        }
    //    }
    //}
}