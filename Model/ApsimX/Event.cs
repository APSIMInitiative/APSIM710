using System;
using System.Collections.Generic;
using System.Text;
using System.Reflection;


class EventPublisher
{
    private EventInfo Info;
    private object TheModel;
    public EventPublisher(EventInfo I, object Model)
    {
        Info = I;
        TheModel = Model;
    }
    public string Name
    {
        get
        {
            return Info.Name;
        }
    }

    public void ConnectTo(EventSubscriber Subscriber)
    {
        Delegate D = Delegate.CreateDelegate(Info.EventHandlerType, Subscriber.TheModel, Subscriber.Info);
        Info.AddEventHandler(TheModel, D);
    }

}

class EventSubscriber
{
    internal MethodInfo Info;
    internal object TheModel;
    private string _Name;
    public EventSubscriber(string Name, MethodInfo I, object Model)
    {
        if (Name != null)
            _Name = Name;
        Info = I;
        TheModel = Model;
    }
    public string Name
    {
        get
        {
            if (_Name != null)
                return _Name;
            string FullName = Info.Name;
            if (FullName.Substring(0, 2) == "On")
                FullName = FullName.Remove(0, 2);
            return FullName;
        }
    }
    public void Invoke()
    {
        if (Info != null)
            Info.Invoke(TheModel, null);
    }

}