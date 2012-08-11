using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System;
using System.IO;
class file
{
    string fileNameInUse;
    string sectionName;
    int sectionPlace = -1;
    public bool critical = false;
    List<string> itemsName = new List<string>();
    List<string> itemsValue = new List<string>();
    public bool FindSection(string aSectionName, int sectionNumber)
    {
	    sectionName=aSectionName;

	    sectionPlace=-1;
	    if(sectionNumber!=-1)
	    {
		    aSectionName=aSectionName+'('+sectionNumber.ToString()+')';
	    }
	    aSectionName='['+aSectionName+']';

	

	    for( int i=0;i<itemsName.Capacity;i++)
	    {

		    if(aSectionName.CompareTo(itemsName[i])==0)
		    {
		
			    sectionPlace=i;
			    return true;

		    }
	    }


	    if(critical==true)
		    {
		    string output="could not find "+sectionName+" "+sectionNumber.ToString();
            throw new System.Exception(output);
		    }



	    return false;
    }
    public bool FindItem(string ItemName, ref double output)
    {
        string outputs = null;
        bool findIt = FindItem(ItemName, ref outputs);
        if (findIt == true)
        {
            output = Convert.ToDouble(outputs);

            return true;
        }
        return false;
    }
    public bool FindItem(string ItemName, ref bool output)
        {
        string outputs = null;
        bool findIt = FindItem(ItemName, ref outputs);
        if(findIt==true)
	    {
            output=Convert.ToBoolean(outputs);

		            return true;
	    }
	return false;

    }
    public bool FindItem(string ItemName, ref int output)
    {
        string outputs = null;
        bool findIt = FindItem(ItemName, ref outputs);
        if(findIt==true)
	    {
            output=Convert.ToInt32(outputs);

		            return true;
	    }
	return false;

    }
    bool FindItem(string ItemName, ref string output)
    {
	    for(int i=sectionPlace+1;i<itemsName.Capacity;i++)
	    {


          

		    if(ItemName.CompareTo(itemsName[i])==0)
		    {
			    output=itemsValue[i];

			    return true;
		    }

			    if(itemsName[i][0]=='['&&itemsName[i][itemsName[i].Length-1]==']')
			    {
					    break;
			    }

	    }
        if (critical == true)
        {
            throw new System.Exception(ItemName + " not found");
        }
        return false;
    }
    public void readFile(string fileName)
    {

	    fileNameInUse=fileName;
        itemsName.Clear();
        itemsValue.Clear();
	
        if(File.Exists(fileName))
        {
            StreamReader sr = new StreamReader(fileName);
            while(!sr.EndOfStream)
            {
                string lineRead=sr.ReadLine();
                string[] words = lineRead.Split(' ');
                Console.WriteLine(words[0]);
                itemsName.Add(words[0]);
                if (words[0][0].CompareTo('[') != 0)
                    itemsValue.Add(words[1]);
                else
                    itemsValue.Add("0");

            }
            sr.Close();
        }
        else
            throw new System.Exception(fileName + " not found");

    }
    
}

