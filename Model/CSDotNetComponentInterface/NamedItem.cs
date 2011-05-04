using System;
using System.Collections.Generic;


public class NamedItem
{
    private String _Name;
    public virtual String Name
    {
        get
        {
            return _Name;
        }
        set
        {
            _Name = value;
        }
    }
}
// --------------------------------------------------------------------------
/// <summary>
/// This class allows the developer to index into the phase collection
/// using a string key.
/// </summary>
/// <typeparam name="T"></typeparam>
// --------------------------------------------------------------------------
public class NamedList<T> : List<T> where T : NamedItem
{
    public String ParentName;
    public T this[String Name]
    {
        get
        {
            foreach (T Obj in this)
            {
                if (Obj.Name.ToLower() == Name.ToLower())
                    return Obj;
            }
            throw new Exception("Cannot find object: " + ParentName + Name);
        }
    }
    public bool Contains(String NameToFInd)
    {
        foreach (T Obj in this)
        {
            if (Obj.Name.ToLower() == NameToFInd.ToLower())
                return true;
        }
        return false;
    }
    public int IndexOf(String NameToFInd)
    {
        for (int i = 0; i < this.Count; i++)
        {
            if (this[i].Name.ToLower() == NameToFInd.ToLower())
                return i;
        }
        return -1;
    }
}
// --------------------------------------------------------------------------
/// <summary>
/// 
/// </summary>
// --------------------------------------------------------------------------
public abstract class TypedItem
{
    public abstract bool IsOfType(String TypeNameToMatch);
}
// --------------------------------------------------------------------------
/// <summary>
/// This class allows the developer to index into a component collection
/// using a type.
/// </summary>
/// <typeparam name="T"></typeparam>
// --------------------------------------------------------------------------
public class TypedList<T> : List<T> where T : TypedItem
{
    public T this[String TypeName]
    {
        get
        {
            foreach (T Obj in this)
            {
                if (Obj.IsOfType(TypeName))
                    return Obj;
            }
            throw new Exception("Cannot find component of type: " + TypeName);
        }
    }
}
/// --------------------------------------------------------------------------
/// <summary>
/// This class allows the developer to index into a component collection
/// using a type.
/// </summary>
/// <typeparam name="T"></typeparam>
/// --------------------------------------------------------------------------
public class TypedMultiList<T> : List<T> where T : TypedItem
{
    public List<T> this[String TypeName]
    {
        get
        {
            List<T> ReturnList = new List<T>();
            foreach (T Obj in this)
            {
                if (Obj.IsOfType(TypeName))
                    ReturnList.Add(Obj);
            }
            return ReturnList;
        }
    }
}


