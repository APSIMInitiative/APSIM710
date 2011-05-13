using System;
using System.Collections.Generic;
using System.Collections.Specialized;
using System.Text;

public class GenericType : TypeInterpreter
{
    private List<TypeInterpreter> Fields;
    private String TypeDDML;

    public GenericType()
    {
        Fields = new List<TypeInterpreter>();
    }

    public void Add(TypeInterpreter Field)
    {
        Fields.Add(Field);
    }

    public override String DDML()
    {
        return TypeDDML;
    }

    public void SetDDML(String value)
    {
        TypeDDML = value;
    }

    public override uint memorySize()
    {
        uint Size = 0;
        foreach (TypeInterpreter Field in Fields)
            Size += Field.memorySize();
        return Size;
    }
    //this function needs examination and revision - NH
    public override void pack(out byte[] MessageData)
    {
        MessageData = new byte[memorySize()];
        foreach (TypeInterpreter Field in Fields)
            Field.pack(out MessageData);
    }
    //this function needs examination and revision - NH
    public override void unpack(byte[] MessageData)
    {
        foreach (TypeInterpreter Field in Fields)
            Field.unpack(MessageData);
    }
}



