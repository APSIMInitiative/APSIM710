using System;
using System.Collections.Generic;
using System.Text;
using CSGeneral;
using System.Xml;
using System.Reflection;
using ModelFramework;
     
//---------------------------------------------------------------
/// <summary>
/// This class allows the developer to index into the phase collection
/// using an int key. 
/// </summary>
//---------------------------------------------------------------
public class LayeredList<T> : List<T>
{

}   
//---------------------------------------------------------------
/// <summary>
/// 
/// </summary>
//---------------------------------------------------------------
public class Instance : NamedItem
{
    private String MyFQN()
    {
        String FQN = "";
        if (Parent != null)
            FQN = Parent.MyFQN();
        FQN += Name;
        return FQN;
    }
    private ApsimComponent Component;

    protected virtual bool Override(Type aType, String targetName)
    {
        // Now look for parameter overrides and apply them as needed
        for (int i = 0; i != Children.Count; i++)
        {
            if (Children[i].GetType().Equals(aType) &&
                Children[i].Name == targetName)
            {
                ((DerivedInstance)(Children[i])).ApplyOverrides();
                return true;
            }
        }
        return false;
    }
    public Instance Parent;
    public NamedList<NamedItem> Children;
    // --------------------------------------------------------------------
    /// <summary>
    /// 
    /// </summary>
    // --------------------------------------------------------------------
    public Instance()
    {
        Children = new NamedList<NamedItem>();
        Parent = null;
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
    public Instance Find(String Name)
    {
        int PosPeriod = Name.IndexOf('.');
        if (PosPeriod == -1)
            return (Instance)Children[Name];
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

//---------------------------------------------------------------
/// <summary>
/// 
/// </summary>
//---------------------------------------------------------------
public class DerivedInstance : Instance
{
    protected XmlNode xml;
    //---------------------------------------------------------------
    /// <summary>
    /// 
    /// </summary>
    /// <param name="Node"></param>
    /// <param name="Paren"></param>
    /// <param name="ParentComponent"></param>
    //---------------------------------------------------------------
    public void Initialise(XmlNode Node, Instance Paren, ApsimComponent ParentComponent)
    {
        xml = Node;
        base.Initialise(XmlHelper.Name(Node), Paren, ParentComponent);
    }
    //---------------------------------------------------------------
    /// <summary>
    /// 
    /// </summary>
    //---------------------------------------------------------------
   public void ApplyOverrides()
   {
      foreach (XmlNode Instruction in xml.ChildNodes)
      {
         if (Instruction.Name == "Override")
         {
			 Instance target;
			 String ReferencedNodeName = XmlHelper.Attribute(Instruction, "name");
			 if (ReferencedNodeName == "")
				 target = Root;
			 else
                target = Root.Find(ReferencedNodeName.Replace(".", "/"));
             foreach (XmlNode Child in Instruction.ChildNodes)
             {
               if (Child.Name == "Memo")
               {
               // Ignore memo fields.
               }
               else if (!Child.HasChildNodes && Child.InnerText == "")
                  throw new Exception("Cannot have a blank value for property: " + Child.Name);
               else if (Child.HasChildNodes)
               {
			      bool found = false;
				  // First look for a suitable field
				  FieldInfo field = target.GetType().GetField(Child.Name, BindingFlags.Instance | BindingFlags.Public | BindingFlags.NonPublic);
				  if (field != null)
				  {
                     Object[] Attributes = field.GetCustomAttributes(false);
                     foreach (Object Attr in Attributes)
                     {
                       if ((Param)(Attr) != null)
					   {
				          found = true;
                          ReflectedField FieldToSet = new ReflectedField(field, target);
	                      FieldToSet.Set(Child);
						  break;
					   }
					 }
				  } 
				  if (!found)
				  // Couldn't find a field; maybe it's a property
				  {
                     PropertyInfo property = target.GetType().GetProperty(Child.Name, BindingFlags.Instance | BindingFlags.Public | BindingFlags.NonPublic);
					 if (property != null)
					 {
                        Object[] Attributes = property.GetCustomAttributes(false);
                        foreach (Object Attr in Attributes)
                        {
                           if ((Param)(Attr) != null)
					       {
						      found = true;
                              ReflectedProperty PropertyToSet = new ReflectedProperty(property, target);
  					          PropertyToSet.Set(Child);
						 	  break;
						   }
						}
					 }
				  }
				  if (!found)
                     throw new Exception("Could not find an overrideable parameter: " + Child.Name);
               }
            }
         }
      }
   }
}



