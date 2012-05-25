using System;
using System.Collections.Generic;
using System.Text;
using System.Reflection;
using CSGeneral;
using System.Xml;

/// <summary>
/// The class encapsulates a FieldInfo or PropertyInfo. In other words it can represent a class field or property.
/// </summary>
public class ClassVariable
{
    private ClassVariable Source;
    private TypeConverter.Converter Converter;
    private MemberInfo Member;
    private object TheModel;
    private string _Name;
    private bool _IsOptional;
    private bool Immutable;
    private bool _HaveSetValue = false;

    /// <summary>
    /// A constructor
    /// </summary>
    internal ClassVariable(MemberInfo member, object theModel)
    {
        Member = member;
        TheModel = theModel;
        if (ParamAttribute != null)
        {
            if (ParamAttribute.Name != null)
                _Name = ParamAttribute.Name;
            _IsOptional = ParamAttribute.IsOptional;
        }
        else if (InputAttribute != null)
            _IsOptional = InputAttribute.IsOptional;
        else if (OutputAttribute != null)
            Immutable = OutputAttribute.Immutable;
        else
            _IsOptional = false;

        if (_Name == null)
            _Name = Member.Name;
    }

    /// <summary>
    /// Connect this class variable to "source" variable so that this variable can be given a value when needed.
    /// </summary>
    internal void ConnectTo(ClassVariable source)
    {
        Source = source;
        Converter = TypeConverter.CreateConverter(Source.Type, Type);
    }

    /// <summary>
    /// Return the name of the class member.
    /// </summary>
    public string Name { get { return _Name; } }

    /// <summary>
    /// Provides access to the value of this class variable. 
    /// </summary>
    public object Value
    {
        get
        {
            if (Member is FieldInfo)
                return (Member as FieldInfo).GetValue(TheModel);
            else
                return (Member as PropertyInfo).GetValue(TheModel, null);
        }
        set
        {
            if (Member is FieldInfo)
                (Member as FieldInfo).SetValue(TheModel, value);
            else
                (Member as PropertyInfo).SetValue(TheModel, value, null);
            _HaveSetValue = true;
        }
    }

    /// <summary>
    /// Return a description
    /// </summary>
    public string Description
    {
        get
        {
            object[] Attributes = Member.GetCustomAttributes(typeof(Description), false);
            if (Attributes.Length == 0)
                return null;
            else
                return (Attributes[0] as Description).ToString();
        }
    }

    /// <summary>
    /// Return the name of the class member.
    /// </summary>
    internal MemberInfo MemberInfo { get { return Member; } }

    /// <summary> 
    /// Update the value of this variable using the source variable.
    /// </summary>
    internal void UpdateValue()
    {
        if (Source == null)
        {
            if (!IsOptional)
                throw new Exception("Cannot find a value for [Input] variable: " + Name + " for model class: " + TheModel.GetType().Name);
        }
        else if (!_HaveSetValue || !Source.Immutable)
        {
            if (Converter == null)
                Value = Source.Value;
            else
                Value = Converter.Invoke(Name, Source.Value);
        }
    }


    internal bool HaveSetValue { get { return _HaveSetValue; } }

    /// <summary>
    /// Return the type of the class member.
    /// </summary>
    internal Type Type
    {
        get
        {
            if (Member is FieldInfo)
                return (Member as FieldInfo).FieldType;
            else
                return (Member as PropertyInfo).PropertyType;
        }
    }

    /// <summary>
    /// Return true if this class variable is an Optional one. [Param] and [Input] variables can be optional.
    /// </summary>
    private bool IsOptional { get { return _IsOptional; } }

    /// <summary>
    /// Return the [Param] attribute for this class variable. Will return null if no
    /// [Param] was specified.
    /// </summary>
    private Param ParamAttribute
    {
        get
        {
            object[] Attributes = Member.GetCustomAttributes(typeof(Param), false);
            if (Attributes.Length == 0)
                return null;
            else
                return Attributes[0] as Param;
        }
    }

    /// <summary>
    /// Return the [Input] attribute for this class variable. Will return null if no
    /// [Input] was specified.
    /// </summary>
    private Input InputAttribute
    {
        get
        {
            object[] Attributes = Member.GetCustomAttributes(typeof(Input), false);
            if (Attributes.Length == 0)
                return null;
            else
                return Attributes[0] as Input;

        }
    }

    /// <summary>
    /// Return the [Output] attribute for this class variable. Will return null if no
    /// [Output] was specified.
    /// </summary>
    private Output OutputAttribute
    {
        get
        {
            object[] Attributes = Member.GetCustomAttributes(typeof(Output), false);
            if (Attributes.Length == 0)
                return null;
            else
                return Attributes[0] as Output;

        }
    }

    internal bool IsInput  { get { return InputAttribute != null; } }
    internal bool IsParam  { get { return ParamAttribute != null; } }
    internal bool IsOutput { get { return OutputAttribute != null; } }
    internal bool IsState
    {
        get
        {
            return !IsInput && !IsParam && Member is FieldInfo &&
                   Member.GetCustomAttributes(typeof(NonSerializedAttribute), false).Length == 0;
        }
    }
}
