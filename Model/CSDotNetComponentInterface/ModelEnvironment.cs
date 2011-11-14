using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using CMPServices;
using System.Reflection;
using ModelFramework;
using System.Xml;
using CSGeneral;

public class ModelEnvironment
{

    private Instance In;

    /// <summary>
    /// Constructor - created by LinkRef
    /// </summary>
    internal ModelEnvironment(Instance Obj)
    {
        In = Obj;
    }

    /// <summary>
    /// Property for returning the name of this instance.
    /// </summary>
    public string Name
    {
        get
        {
            int PosLastPeriod = In.InstanceName.LastIndexOf('.');
            if (PosLastPeriod == -1)
                return In.InstanceName;
            else
                return In.InstanceName.Substring(PosLastPeriod + 1);

        }
    }

    /// <summary>
    /// Property for returning the name of this instance.
    /// </summary>
    public string FullName
    {
        get
        {
            string SystemName = In.ParentComponent().Name;
            SystemName = SystemName.Remove(SystemName.LastIndexOf('.'));  // remove instance name
            return SystemName + "." + In.InstanceName;
        }
    }

    /// <summary>
    /// Returns a list of fully qualified child model names for the specified system path. 
    /// The returned list may be zero length but will never be null.
    /// </summary>
    /// <returns></returns>
    public string[] ChildNames()
    {
        return ChildNamesForInstance(In);
    }

    /// <summary>
    /// Returns a list of fully qualified child model names for the specified system path. 
    /// The returned list may be zero length but will never be null.
    /// </summary>
    /// <returns></returns>
    public string[] ChildNames(string SystemPath)
    {
        Instance Obj = (Instance) LinkField.FindInstanceObject(In, SystemPath, "Instance");
        if (Obj != null)
            return ChildNamesForInstance(Obj);
        else
        {
            List<string> ReturnNames = new List<string>();

            string SearchName = SystemPath + ".*";    //search comp.*

            List<TComp> comps = new List<TComp>();
            In.ParentComponent().Host.queryCompInfo(SearchName, TypeSpec.KIND_COMPONENT, ref comps);
            for (int i = 0; i < comps.Count; i++)
            {
                ReturnNames.Add(comps[i].name);
            }
            return ReturnNames.ToArray();
        }
    }

    /// <summary>
    /// Returns a list of fully qualified child model names for the specified system path. 
    /// The returned list may be zero length but will never be null.
    /// </summary>
    /// <returns></returns>
    private static string[] ChildNamesForInstance(object Obj)
    {
        string[] Names;
        if (Obj is Instance)
        {
            Instance I = (Instance)Obj;
            if (I.Children.Count > 0)
            {
                Names = new string[I.Children.Count];
                for (int i = 0; i < I.Children.Count; i++)
                {
                    if (I.Children[i] is Instance)
                    {
                        Instance Child = (Instance)I.Children[i];

                        // To get the fqn right we need to remove the left most bit of the child.InstanceName
                        // because it duplicates the right most bit of I.ParentComponent().Name.
                        string ChildName = Child.Name;
                        if (ChildName.Contains('.'))
                            ChildName.Remove(0, ChildName.IndexOf('.'));

                        Names[i] = I.ParentComponent().Name + "." + ChildName;
                    }
                    else
                        throw new Exception("Invalid child found: " + I.Children[i].Name);
                }
            }
            else
            {
                string sSearchName = I.ParentComponent().Name + ".*";    //search comp.*

                List<TComp> comps = new List<TComp>();
                I.ParentComponent().Host.queryCompInfo(sSearchName, TypeSpec.KIND_COMPONENT, ref comps);
                Names = new string[comps.Count];
                for (int i = 0; i < comps.Count; i++)
                    Names[i] = comps[i].name;
            }
            return Names;
        }
        else
            return new string[0];
    }

    public delegate void NullFunction ();
    public void Subscribe(string EventPath, NullFunction F)
    {
        RuntimeEventHandler.NullFunction Fn = new RuntimeEventHandler.NullFunction(F);
        RuntimeEventHandler Event = new RuntimeEventHandler(EventPath, Fn);
        In.ParentComponent().Subscribe(Event);
    }


    public void Publish(string EventPath, ApsimType Data = null)
    {
        if (Data == null)
            Data = new NullType();
        In.ParentComponent().Publish(EventPath, Data);
    }

    /// <summary>
    /// Attempts to find a model in the system that matches the specified name path and
    /// return a link to it.
    /// The NamePath may be a fully qualified name OR a name with no path (in which
    /// case scoping rules are used to perform the match).
    /// Returns a link to the matched model or null if not found. 
    /// </summary>
    public T Link<T>(string NamePath)
    {
        object E = FindInternalEntity(NamePath, In);
        if (E != null)
        {
            if (E is Entity)
            {
                object Value = (E as Entity).Get();
                if (Value is T)
                    return (T)Value;
            }
            else if (E is Instance)
            {
                object Value = (E as Instance).Model;
                if (Value is T)
                    return (T)Value;
            }

            return default(T);
        }
        else
            return (T)LinkField.FindApsimObject(typeof(T).Name, NamePath, StringManip.ParentName(In.ParentComponent().InstanceName),
                                             In.ParentComponent());
    }

    /// <summary>
    /// Attempts to find a model in the system that matches the specified type and
    /// return a link to it.
    /// Returns a link to the matched model or null if not found.
    /// </summary>
    public T Link<T>()
    {
        return (T)LinkField.FindApsimObject(typeof(T).Name, null, StringManip.ParentName(In.ParentComponent().InstanceName),
                                            In.ParentComponent());
    }
    
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
    /// Locate a variable that matches the specified path and return its value. Returns null
    /// if not found. e.g. NamePath:
    ///     "Plant"   - no path specified so look in scope
    ///     "Phenology.CurrentPhase" - relative path specified so look for matching child
    ///     ".met.maxt" - absolute path specified so look for exact variable.
    /// </summary>    
    
    private bool GetInternal<T>(string NamePath, WrapBuiltInVariable<T> Data)
    {
        if (NamePath.Length > 0 && NamePath[0] == '.')
        {
            // absolute path.
            {
                bool ok = In.ParentComponent().Get(NamePath, Data, true);
                return Data.Value != null;
            }
        }
        else if (NamePath.Contains('.'))
        {
            // relative path.
            // assume internal entity.
            object E = FindInternalEntity(NamePath, In);
            if (E != null && E is Entity)
            {
                Data.setValue( (E as Entity).Get());
                return true;
            }
            else
                return false;
        }
        else
        {
            // no path
            // First look for an internal entity.
            object E = FindInternalEntity(NamePath, In);
            if (E != null && E is Entity)
            {
                Data.setValue((E as Entity).Get());
                return true;
            }
            else
            {
                // not an internal entity so look for an external one.
                return In.ParentComponent().Get(NamePath, Data, true);
            }
        }

        return false;
    }

    /// <summary>
    /// Set the value of a variable.
    /// </summary>
    public bool Set(string NamePath, double Data)
    {
        return SetInternal<double>(NamePath, Data);
    }


    /// <summary>
    /// Set the value of a variable.
    /// </summary>
    private bool SetInternal<T>(string NamePath, T Value)
    {
        WrapBuiltInVariable<T> Data = new WrapBuiltInVariable<T>();
        Data.Value = Value;
        if (NamePath.Length > 0 && NamePath[0] == '.')
        {
            // absolute path.
            In.ParentComponent().Set(NamePath, Data);
            return true;
        }
        else if (NamePath.Contains('.'))
        {
            // relative path.
            object E = FindInternalEntity(NamePath, In);
            if (E != null && E is Entity)
                return (E as Entity).Set(Data);
            else
                return false;
        }
        else
        {
            // no path
            // First look for an internal entity.
            object E = FindInternalEntity(NamePath, In);
            if (E != null && E is Entity)
            {
                Data.setValue( (E as Entity).Get());
                return true;
            }
            else
            {
                // not an internal entity so look for an external one.
                In.ParentComponent().Set(NamePath, Data);
                return true;
            }

        }

    }

    class Entity
    {
        public MemberInfo MI;
        public object Obj;

        public object Get()
        {
            if (MI is FieldInfo)
            {
                FieldInfo FI = MI as FieldInfo;
                return  FI.GetValue(Obj);
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
    private static object FindInternalEntity(string NamePath, Instance RelativeTo)
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

    /// <summary>
    /// Add a new model to the simulation. The ModelDescription describes the parameterisation of
    /// the model. The ModelAssembly contains the model.
    /// </summary>
    public void AddModel(XmlNode ModelDescription, Assembly ModelAssembly)
    {
        In.ParentComponent().BuildObjects(ModelDescription, ModelAssembly);
    }

}
