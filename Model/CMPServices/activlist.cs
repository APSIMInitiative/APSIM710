using System;
using System.Collections.Generic;
using System.Text;

namespace CMPServices
{
    internal class TActiveTracker
    {
        public const int ACTIVE = 0;   //the active flag of a component
        public const int INACTIVE = 1;
        protected struct TActiveToggle {
            public uint reqMsgID;   //msg ID of the request message
            public uint compID;     //component to set active/inactive
            public uint value;      //set to...  1=active 0=inactive
            public bool resumeOnReturn; //should send a resumeSimulation() after the value is set. Does the job!
        } 
        protected List<TActiveToggle> setActiveList;

        public TActiveTracker()
        {
            setActiveList = new List<TActiveToggle>();
        }
        public void setActive(uint reqMsgID, uint compID, uint value, bool resumeOnReturn)
        {
            TActiveToggle setComp;

            setComp = new TActiveToggle();

            setComp.reqMsgID = reqMsgID;
            setComp.compID = compID;
            setComp.value = value;
            setComp.resumeOnReturn = resumeOnReturn;    //default is false
            setActiveList.Add(setComp);
        }
        public int getLastSetActive(uint reqMsgID, uint compID)
        {   //To Do:
            return 0;
        }
        public int removeSetActive(uint reqMsgID, uint compID)
        {   //To Do:
            return 0;
        }
        public bool shouldSendResume(uint reqMsgID, uint compID)
        {   //To Do:
            return true;
        }
    }
}
