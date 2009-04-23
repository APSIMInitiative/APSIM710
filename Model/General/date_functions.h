//---------------------------------------------------------------------------
#ifndef date_functionsH
#define date_functionsH

#include <boost/date_time/gregorian/gregorian.hpp>
#include <stdexcept>
#include <general/string_functions.h>
#include <General/platform.h>

  //! Convert ymd to a standard string formatting policies
  template<class ymd_type, class format_type>
  class dmy_formatter
  {
  public:
    //! Convert ymd to a standard string formatting policies
    /*! This is standard code for handling date formatting with
     *  year-month-day based date information.  This function
     *  uses the format_type to control whether the string will
     *  contain separator characters, and if so what the character
     *  will be.  In addtion, it can format the month as either
     *  an integer or a string as controled by the formatting
     *  policy
     */
    static std::string ymd_to_string(ymd_type ymd)
    {
      typedef typename ymd_type::month_type month_type;
      std::ostringstream ss;
      ss  << std::setw(2) << std::setfill('0')
          << ymd.day;
      if (format_type::has_date_sep_chars()) {
        ss << format_type::month_sep_char();
      }
      //this name is a bit ugly, oh well....
      boost::date_time::month_formatter<month_type,format_type>::format_month(ymd.month, ss);
      if (format_type::has_date_sep_chars()) {
        ss << format_type::day_sep_char();
      ss << ymd.year;
      }
      return ss.str();
    }
  };

  //! Convert a date to string using format policies
  template<class date_type, class format_type>
  class date_formatter_dmy
  {
  public:
    //! Convert to a date to standard string using format policies
    static std::string date_to_string(date_type d)
    {
      typedef typename date_type::ymd_type ymd_type;
      if (d.is_not_a_date()) {
        return format_type::not_a_date();
      }
      if (d.is_neg_infinity()) {
        return format_type::neg_infinity();
      }
      if (d.is_pos_infinity()) {
        return format_type::pos_infinity();
      }
      ymd_type ymd = d.year_month_day();
      return dmy_formatter<ymd_type, format_type>::ymd_to_string(ymd);
    }
  };

//! Class to provide common iso formatting spec
class european_format {
public:
  //! Describe month format -- its an integer in iso format
  static boost::date_time::month_format_spec month_format()
  {
    return boost::date_time::month_as_integer;
  }

  //! String used printed is date is invalid
  static const char* not_a_date()
  {       //20010102
    return "NotADate";
  }
  //! String used to for positive infinity value
  static const char* pos_infinity()
  {
    return "+infin  ";
  }
  //! String used to for positive infinity value
  static const char* neg_infinity()
  {
    return "-infin  ";
  }

  //! ISO char for a year -- used in durations
  static char year_sep_char()
  {
    return 'Y';
  }
  //! ISO char for a month
  static char month_sep_char()
  {
    return '/';
  }
  //! ISO char for a day
  static char day_sep_char()
  {
    return '/';
  }
  //! char for minute
  static char hour_sep_char()
  {
    return ':';
  }
  //! char for minute
  static char minute_sep_char()
  {
    return ':';
  }
  //! char for second
  static char second_sep_char()
  {
    return ':';
  }
  //! ISO char for a period
  static char period_start_char()
  {
    return 'P';
  }
  //! Used in time in mixed strings to set start of time
  static char time_start_char()
  {
    return 'T';
  }

  //! Used in mixed strings to identify start of a week number
  static char week_start_char()
  {
    return 'W';
  }

  //! Separators for periods
  static char period_sep_char()
  {
    return '/';
  }
  //! Separator for hh:mm:ss
  static char time_sep_char()
  {
    return ':';
  }
  //! Preferred Separator for hh:mm:ss,decimal_fraction
  static char fractional_time_sep_char()
  {
    return ',';
  }

  static bool is_component_sep(char sep)
  {
    switch(sep) {
    case 'H':
    case 'M':
    case 'S':
    case 'W':
    case 'T':
    case 'Y':
    case 'D':return true;
    default:
      return false;
    }
  }

  static bool is_fractional_time_sep(char sep)
  {
    switch(sep) {
    case ',':
    case '.': return true;
    default: return false;
    }
  }
  static bool is_timezone_sep(char sep)
  {
    switch(sep) {
    case '+':
    case '-': return true;
    default: return false;
    }
  }
  static char element_sep_char()
  {
    return '-';
  }

  static bool has_date_sep_chars()
  {
    return true;
  }
};

  //! To DD-mm-yyyy string where mmm 3 char month name. Example:  15-06-2002
  /*!\ingroup date_format
   */
  inline std::string to_dmy(const boost::gregorian::date& d) {
    return date_formatter_dmy<boost::gregorian::date,european_format>::date_to_string(d);
  }

#ifndef _NO_VCL
boost::gregorian::date fromVCL(const TDateTime& d)
   {
   return boost::gregorian::date(1899, 12, 30) + boost::gregorian::date_duration((int)d);
   }
TDateTime toVCL(const boost::gregorian::date& d)
   {
   return TDateTime(d.year(), d.month(), d.day());
   }
#endif
void addMonths(boost::gregorian::date& d, int numMonths)
   {
   int month = d.month();
   int year = d.year();
   month += numMonths;
   while (month > 12)
      {
      month -= 12;
      year++;
      }
   while (month < 1)
      {
      month += 12;
      year--;
      }
   d = boost::gregorian::date(year, month, d.day());
   }
std::string getShortMonthString(unsigned month)
   {
   static const char* months[13] =
      {"Jan", "Feb", "Mar", "Apr", "May", "Jun",
       "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"};
   return months[month-1];
   }
std::string getLongMonthString(unsigned month)
   {
   static const char* months[13] =
      {"January", "February", "March", "April", "May", "June",
       "July", "August", "September", "October", "November", "December"};
   return months[month-1];
   }
int longMonthToInt(const std::string& longMonthName)
   {
   for (int month = 1; month <= 12; month++)
      {
      if (Str_i_Eq(getLongMonthString(month), longMonthName))
         return month;
      }
   throw std::runtime_error("Invalid month nane: " + longMonthName);
   }

unsigned day_of_year(boost::gregorian::date d)
   {
   return boost::gregorian::date_duration(d - boost::gregorian::date(d.year(), 1, 1)).days() + 1;
   }
//! Generic function to parse a delimited date DMY format (eg: 2/12/2002)
boost::gregorian::date fromDmyString(const std::string& s)
   {
   unsigned day, month, year;
   int pos = 0;
   boost::tokenizer<boost::char_delimiters_separator<char> > tok(s);
   for(boost::tokenizer<>::iterator beg=tok.begin(); beg!=tok.end();++beg)
    {
    int i = boost::lexical_cast<int>(*beg);
    switch(pos)
       {
       case 0: day = i; break;
       case 1: month = i; break;
       case 2: year = i; break;
       };
    pos++;
    }
   return boost::gregorian::date(year, month, day);
   }
#endif
