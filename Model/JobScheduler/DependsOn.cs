using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Xml.Serialization;

[Serializable]
public class DependsOn
{
    [XmlAttribute("IgnoreErrors")]
    public bool IgnoreErrors { get; set; }

    [XmlText]
    public string Name { get; set; }

    public DependsOn()
    {
        IgnoreErrors = false;
    }

    public DependsOn(string name)
    {
        IgnoreErrors = false;
        Name = name;
    }
}
