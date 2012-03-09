using System;
using System.Collections.Generic;
using System.Text;
using CSGeneral;
using System.Xml;
using System.Reflection;
using ModelFramework;

//---------------------------------------------------------------
/// <summary>
/// 
/// </summary>
//---------------------------------------------------------------
class Instance : NamedItem
{
    private String MyFQN()
    {
        String FQN = "";
        if (Parent != null)
            FQN = Parent.MyFQN();
        if (FQN != "")
            FQN += ".";
        FQN += Name;
        return FQN;
    }
    private ApsimComponent Component;

    public Instance Parent;
    public NamedList<NamedItem> Children;

    // --------------------------------------------------------------------
    /// <summary>
    /// 
    /// </summary>
    // --------------------------------------------------------------------
    public object _ModelObj;
    public object Model
    {
        get
        {
            if (_ModelObj == null)
                return this;
            else
                return _ModelObj;
        }
    }


    // --------------------------------------------------------------------
    /// <summary>
    /// 
    /// </summary>
    // --------------------------------------------------------------------
    public Instance()
    {
        Children = new NamedList<NamedItem>();
        Parent = null;
        _ModelObj = null;
    }
    // --------------------------------------------------------------------
    /// <summary>
    /// 
    /// </summary>
    // --------------------------------------------------------------------
    public Instance(object model)
    {
        Children = new NamedList<NamedItem>();
        Parent = null;
        _ModelObj = model;
    }
    // --------------------------------------------------------------------
    /// <summary>
    /// 
    /// </summary>
    /// <param name="Child"></param>
    // --------------------------------------------------------------------
    public virtual void Add(Instance Child)
    {
        Children.Add(Child);
    }
    // --------------------------------------------------------------------
    /// <summary>
    /// 
    /// </summary>
    /// <returns></returns>
    // --------------------------------------------------------------------
    public ApsimComponent ParentComponent()
    {
        return Component;
    }
    // --------------------------------------------------------------------
    /// <summary>
    /// 
    /// </summary>
    /// <param name="Name"></param>
    /// <returns></returns>
    // --------------------------------------------------------------------
    public int IndexOf(String Name)
    {
        int PosPeriod = Name.IndexOf('.');
        if (PosPeriod == -1)
        {
            for (int i = 0; i != Children.Count; i++)
            {
                if (Children[i].Name.ToLower() == Name.ToLower())
                    return i;
            }
            return -1;
        }
        else
        {
            String ChildName = Name.Substring(0, PosPeriod);
            String Remainder = Name.Substring(PosPeriod + 1);
            return ((Instance)Children[ChildName]).IndexOf(Remainder);
        }
    }
    // --------------------------------------------------------------------
    /// <summary>
    /// 
    /// </summary>
    /// <param name="Name"></param>
    /// <returns></returns>
    // --------------------------------------------------------------------
    public object Find(String Name)
    {
        int PosPeriod = Name.IndexOf('.');
        if (PosPeriod == -1)
            return Children[Name];
        else
        {
            String ChildName = Name.Substring(0, PosPeriod);
            String Remainder = Name.Substring(PosPeriod + 1);
            return ((Instance)Children[ChildName]).Find(Remainder);
        }
    }
    // --------------------------------------------------------------------
    /// <summary>
    /// 
    /// </summary>
    /// <param name="Nam"></param>
    /// <param name="Paren"></param>
    /// <param name="HostComponent"></param>
    // --------------------------------------------------------------------
    public void Initialise(String Nam, Instance Paren, ApsimComponent ParentComponent)
    {
        Name = Nam;
        Parent = Paren;
        Component = ParentComponent;
        Children.ParentName = MyFQN();
        Initialising();
    }
    // --------------------------------------------------------------------
    /// <summary>
    /// 
    /// </summary>
    // --------------------------------------------------------------------
    public virtual void Initialising()
    {
    }
    // --------------------------------------------------------------------
    /// <summary>
    /// 
    /// </summary>
    // --------------------------------------------------------------------
    public virtual void Initialised()
    {
    }
    // --------------------------------------------------------------------
    /// <summary>
    /// 
    /// </summary>
    // --------------------------------------------------------------------
    internal Instance Root
    {
        get
        {
            if (Parent == null)
                return this;
            else
                return Parent.Root;
        }
    }
    // --------------------------------------------------------------------
    /// <summary>
    /// 
    /// </summary>
    // --------------------------------------------------------------------
    public String InstanceName
    {
        get
        {
            return MyFQN();
        }
    }
    //property PaddockType^ Paddock 
    //   { 
    //   // --------------------------------------------------------------------
    //   // Returns the singleton instance of a reflection class that is
    //   // capable of returning metadata about the structure of the current
    //   // paddock that this model is running in.
    //   // --------------------------------------------------------------------

    //   PaddockType^ get()
    //      { 
    //      if (_Paddock == nullptr)
    //         {
    //         String^ ParentName = Root->ComponentName->Substring(0, Root->ComponentName->LastIndexOf('.'));
    //         _Paddock = gcnew PaddockType(ParentName, Component);
    //         }
    //      return _Paddock;
    //      } 
    //   }  

}


