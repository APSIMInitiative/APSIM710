

// This is not used in any project at the moment. I could delete it but maybe will will use in the future.

namespace UIUtility
{

    public class APSIMUIDateTimeCellType : FarPoint.Win.Spread.CellType.DateTimeCellType
    {

        public void DefaultFormat()
        {
            // Date/Time format settings 
            this.DateTimeFormat = FarPoint.Win.Spread.CellType.DateTimeFormat.UserDefined;
            this.DateSeparator = "-";
            this.UserDefinedFormat = "dd" + this.DateSeparator + "MMM";
            this.Format("dd" + this.DateSeparator + "MMM");

            // Day/Month Short and Long Formats 
            this.DayNames = new string[] { "Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday" };
            this.MonthNames = new string[] { "January", "February", "March", "April", "May", "June", "July", "August", "September", "October", 
        "November", "December", "" };
            this.ShortDayNames = new string[] { "Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat" };
            this.ShortMonthNames = new string[] { "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", 
        "Nov", "Dec", "" };

            // Set Date Picker 
            this.DropDownButton = true;

        }
    }

}