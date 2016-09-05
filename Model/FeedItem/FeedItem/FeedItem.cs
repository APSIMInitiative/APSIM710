using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;


    public class FeedItem
    {
        [Param]
        public double C_content=0;
        [Param]
        public double K_content = 0;
        [Param]
        public double NH4_content = 0;
       [Param]
        public double NO3_content = 0;
        [Param]
        public double orgN_content = 0;
        [Param]
        public double P_content = 0;
       [Param]
        public double P_digest = 0;
       [Param]
        public double pigFeedUnitsPerItemUnit = 0;
        [Param]
        public double proteinN_digestibility = 0;
        // ! Organic matter Digestibility
        public double OMD;
        [Param]
        public double amount = 0;
        int code;
        private double dryMatter;

        public double GetproteinN_digestibility()
        {
            return proteinN_digestibility;
        }
        public double GetC_content()  {
            return C_content;
        }
        public int GetCode() {
            return code;
        }
        public void setAmount(double aAmount)
        {
            amount = aAmount; ;
        }
        public double getAmount()
        {
            return amount;
        }
        public static FeedItem operator +(FeedItem c1, FeedItem c2)
        {
            FeedItem returnItem=new FeedItem();
            returnItem.setAmount(+ c2.getAmount());
            returnItem.C_content = c1.C_content + c2.C_content;
            returnItem.K_content = c1.K_content + c2.K_content;
   
            returnItem.code =  c1.code + c2.code;
            returnItem.proteinN_digestibility = c1.getAmount() / returnItem.getAmount() * c1.proteinN_digestibility + c2.getAmount() / returnItem.getAmount() * c2.proteinN_digestibility;
            returnItem.dryMatter = c1.getAmount() / returnItem.getAmount() * c1.dryMatter + c2.getAmount() / returnItem.getAmount() * c2.dryMatter;
            returnItem.OMD = c1.getAmount() / returnItem.getAmount() * c1.OMD + c2.getAmount() / returnItem.getAmount() * c2.OMD;
            returnItem.proteinN_digestibility = c1.getAmount() / returnItem.getAmount() * c1.proteinN_digestibility + c2.getAmount() / returnItem.getAmount() * c2.proteinN_digestibility;
            returnItem.P_digest = c1.getAmount() / returnItem.getAmount() * c1.P_digest + c2.getAmount() / returnItem.getAmount() * c2.P_digest;
            returnItem.P_content = c1.getAmount() / returnItem.getAmount() * c1.P_content + c2.getAmount() / returnItem.getAmount() * c2.P_content;
            returnItem.orgN_content = c1.getAmount() / returnItem.getAmount() * c1.orgN_content + c2.getAmount() / returnItem.getAmount() * c2.orgN_content;
            returnItem.NO3_content = c1.getAmount() / returnItem.getAmount() * c1.NO3_content + c2.getAmount() / returnItem.getAmount() * c2.NO3_content;
            returnItem.NH4_content = c1.getAmount() / returnItem.getAmount() * c1.NH4_content + c2.getAmount() / returnItem.getAmount() * c2.NH4_content;
            returnItem.K_content = c1.getAmount() / returnItem.getAmount() * c1.K_content + c2.getAmount() / returnItem.getAmount() * c2.K_content;
            returnItem.pigFeedUnitsPerItemUnit = c1.getAmount() / returnItem.getAmount() * c1.pigFeedUnitsPerItemUnit + c2.getAmount() / returnItem.getAmount() * c2.pigFeedUnitsPerItemUnit;
            return returnItem;
        }
        public double GetdryMatter() {
            return dryMatter;
        }
    }

