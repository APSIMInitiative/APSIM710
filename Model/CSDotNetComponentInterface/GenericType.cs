using System;
using System.Collections.Generic;
using System.Collections.Specialized;
using System.Text;

using CMPServices;

/// <summary>
/// Represents an event type used in DotNetProxies that is the interface
/// between a byte[] message and a list of C# Fields.
/// </summary>
public class GenericType : TypeInterpreter
{
    private List<TypeInterpreter> Fields;
    private String TypeDDML = "<type/>";

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
        //create a new ddmlvalue based on updated ddml (should try a better approach - NH)
        DDMLValue = new TDDMLValue(value, "");
        TypeDDML = value;
    }
    /// <summary>
    /// Get the size of the instance.
    /// </summary>
    /// <returns>Total size required to represent this instance.</returns>
    public override uint memorySize()
    {
        uint Size = 0;

        for (int i = 0; i < Fields.Count; i++)
            Size += Fields[i].memorySize();
        return Size;
    }
    /// <summary>
    /// Pack the field values for this Generic type
    /// into the MessageData.
    /// </summary>
    /// <param name="MessageData">The packed data from this Generic type</param>
    public override void pack(out byte[] MessageData)
    {
        for (int i = 0; i < Fields.Count; i++)
        {
            byte[] msgData;
            Fields[i].pack(out msgData);
            DDMLValue.item((uint)i + 1).setData(msgData, msgData.Length);
        }
        MessageData = new byte[memorySize()];
        DDMLValue.getData(ref MessageData);
    }
    /// <summary>
    /// Upack the message data into this Generic type.
    /// </summary>
    /// <param name="MessageData">Incoming message data</param>
    public override void unpack(byte[] MessageData)
    {
        //##It would be nice to improve this code a little - NH
        //populate DDMLValue from MessageData
        DDMLValue.setData(MessageData, MessageData.Length, 0);
        for (uint i = 1; i <= DDMLValue.count(); i++) //for each member
        {
            byte[] b = new byte[DDMLValue.item(i).sizeBytes()];
            DDMLValue.getData(ref b);
            Fields[(int)i-1].unpack(b);       //unpack each field
        }
    }
}



