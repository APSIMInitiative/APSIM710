using System;
using CSGeneral;
using CMPServices;

public class RuntimeEventHandler : EvntHandler
{
    // --------------------------------------------------------------------
    // A class for representing an event handler that has been subscribed
    // to at runtime. It is a wrapper around a method in an object.
    // --------------------------------------------------------------------
    public delegate void NullFunction();

    private NullFunction F;
    public RuntimeEventHandler(String EventName, NullFunction F)
        : base(EventName)
    {
        this.F = F;
    }
    public override Type Typ
    {
        get
        {
            return null;
        }
    }
    public override void Invoke(Object Parameter)
    {
        F.Invoke();
    }
    public override void unpack(TTypedValue src)
    {
        Invoke(null);
    }
    public override void unpack(byte[] messageData)
    {
        Invoke(null);
    }
    public override uint memorySize()
    {
        return 0;
    }
    public override String DDML()
    {
        return "<type/>";
    }

}