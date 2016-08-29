using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;


    public class manure
    {
        public double amount;
        public double orgN_content;
        public double NH4_content ;
        public double NO3_content;
        public double C_content ;
        public double P_content ;
        public double K_content ;
        public double pH;
        public double GetAmount()
        {
            return amount;
        }
        public double GetC_content()
        {
            return C_content;
        }
        public static manure operator +(manure c1, manure c2)
        {
            manure returnItem = new manure();
            returnItem.amount = c1.amount + c2.amount;
            return returnItem;
        }
    }

