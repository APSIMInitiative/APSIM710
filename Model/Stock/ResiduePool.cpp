
// Modification log
// 6 Aug 97 J. Hargreaves  Implementation

#define YES 1
#define NO 0
#define TEST_ResiduePool YES					// build unit test?
#include <math.h>

#include <iostream>
using namespace std;

#include "ResiduePool.H"

inline bool floatsAreEqual(float A, float B, float C) {return(fabs(A-B)<C);}

// ----------------------------- ResiduePartType ---------------------------------

// default constructor
// 	initialise data members.
ResiduePartType::ResiduePartType(void) 			 // member initialisation
{
   carbohydrate = 0.0;
   cellulose = 0.0;
   lignin = 0.0;
}

ResiduePartType::ResiduePartType(float carbohydrateInit, float celluloseInit, float ligninInit)
{

	carbohydrate = carbohydrateInit;
	cellulose = celluloseInit;
	lignin = ligninInit;
}

// destructor
ResiduePartType::~ResiduePartType(void)
{
}

std::ostream &operator<<(std::ostream &output, const ResiduePartType &part)
{
	output << "   Carbohydrate:     " << part.carbohydrate << endl;
	output << "   Cellulose:        " << part.cellulose << endl << endl;
	output << "   Lignin:           " << part.lignin << endl << endl;
      return output;
}

// copy constructor
//	copy data members of object
ResiduePartType::ResiduePartType(const ResiduePartType &ResiduePartType)
{
 	carbohydrate = ResiduePartType.carbohydrate;
	cellulose = ResiduePartType.cellulose;
	lignin = ResiduePartType.lignin;
}


// Assigment operator
//	assign data members of object
const ResiduePartType &ResiduePartType::operator=(const float value)
{
		// copy data members over
            carbohydrate = value;
            cellulose = value;
            lignin = value;
	return *this;
}

//	assign data members of object
const ResiduePartType &ResiduePartType::operator=(const ResiduePartType &other)
{
	if (&other != this)			// don't self-assign
	{
		// copy data members over
            carbohydrate = other.carbohydrate;
            cellulose = other.cellulose;
            lignin = other.lignin;
      }
	return *this;
}

//===========================================================================
ResiduePartType ResiduePartType::operator+ (const ResiduePartType &y) const
//===========================================================================
   {
   ResiduePartType result;

   //Implementation
   result.carbohydrate = carbohydrate + y.carbohydrate;
   result.cellulose = cellulose + y.cellulose;
   result.lignin = lignin + y.lignin;

   return result;
   }

//===========================================================================
ResiduePartType ResiduePartType::operator- (const ResiduePartType &y) const
//===========================================================================
   {
   ResiduePartType result;

   //Implementation
   result.carbohydrate = carbohydrate - y.carbohydrate;
   result.cellulose = cellulose - y.cellulose;
   result.lignin = lignin - y.lignin;

   return result;
   }

//===========================================================================
ResiduePartType ResiduePartType::operator* (const ResiduePartType &y) const
//===========================================================================
   {
   ResiduePartType result;

   //Implementation
   result.carbohydrate = carbohydrate * y.carbohydrate;
   result.cellulose = cellulose * y.cellulose;
   result.lignin = lignin * y.lignin;

   return result;
   }

//===========================================================================
ResiduePartType ResiduePartType::operator* (const float y) const
//===========================================================================
   {
   ResiduePartType result;

   //Implementation
   result.carbohydrate = carbohydrate - y;
   result.cellulose = cellulose - y;
   result.lignin = lignin - y;

   return result;
   }

//===========================================================================
ResiduePartType ResiduePartType::operator/ (const ResiduePartType &divisor)  const
//===========================================================================


   {
   ResiduePartType quotient;

   //Implementation
   quotient.carbohydrate = divide(carbohydrate, divisor.carbohydrate, 0.0);
   quotient.cellulose = divide(cellulose, divisor.cellulose, 0.0);
   quotient.lignin = divide(lignin, divisor.lignin, 0.0);

   return quotient;
   }

//===========================================================================
ResiduePartType ResiduePartType::operator/ (const float divisor)  const
//===========================================================================


   {
   ResiduePartType quotient;

   //Implementation
   quotient.carbohydrate = divide(carbohydrate, divisor, 0.0);
   quotient.cellulose = divide(cellulose, divisor, 0.0);
   quotient.lignin = divide(lignin, divisor, 0.0);

   return quotient;
   }

//===========================================================================
float ResiduePartType::divide (float dividend, float divisor, float default_value) const
//===========================================================================

/*Definition
 *   Returns (dividend / divisor) if the division can be done
 *   without overflow or underflow.  If divisor is zero or
 *   overflow would have occurred, a specified default is returned.
 *   If underflow would have occurred, zero is returned.
 *Assumptions
 *   largest/smallest real number is 1.0e+/-30
 *Parameters
 *   dividend:     dividend
 *   divisor:      divisor
 *   defaultValue: default value to return if overflow
 *Calls
 *   reals_are_equal
 */

   {
   //Constant Values
   const float LARGEST = 1.0e30f;    //largest acceptable no. for quotient
   const float SMALLEST = 1.0e-30f;  //smallest acceptable no. for quotient
   const float nought = 0.0f;
   const float one = 1.0f;
   const float granularity = 1.0e-6f;

   //Local Varialbes
   float quotient;

   //Implementation
   if(floatsAreEqual(dividend, nought, granularity))      //multiplying by 0
      {
      quotient = nought;
      }
   else if(floatsAreEqual(divisor, nought, granularity))  //dividing by 0
      {
      quotient = default_value;
      }
   else if(fabs(divisor) < one)            //possible overflow
      {
      if(fabs(dividend) > fabs(LARGEST * divisor)) //overflow
         {
         quotient = default_value;
         }
      else
         {
         quotient = dividend / divisor;          //ok
         }
      }
   else if(fabs(divisor) > one)             //possible underflow
      {
      if(fabs(dividend) < fabs(SMALLEST * divisor))    //underflow
         {
         quotient = nought;
         }
      else
         {
         quotient = dividend / divisor;                //ok
         }
      }
   else
      {
      quotient = dividend / divisor;                   //ok
      }
   return quotient;
   }

//===========================================================================
ResiduePartType ResiduePartType::divide (const ResiduePartType &dividend, const ResiduePartType &divisor, double default_value)
//===========================================================================

/*Definition
 *   Returns (dividend / divisor) if the division can be done
 *   without overflow or underflow.  If divisor is zero or
 *   overflow would have occurred, a specified default is returned.
 *   If underflow would have occurred, zero is returned.
 *Assumptions
 *   largest/smallest real number is 1.0e+/-30
 *Parameters
 *   dividend:     dividend
 *   divisor:      divisor
 *   defaultValue: default value to return if overflow
 *Calls
 *   reals_are_equal
 */

   {
   ResiduePartType quotient;

   //Implementation
   quotient.carbohydrate = divide(dividend.carbohydrate, divisor.carbohydrate, (float)default_value);
   quotient.cellulose = divide(dividend.cellulose, divisor.cellulose, (float)default_value);
   quotient.lignin = divide(dividend.lignin, divisor.lignin, (float)default_value);

   return quotient;
   }

//===========================================================================
ResiduePartType ResiduePartType::multiply (const ResiduePartType &y)
//===========================================================================
   {
   ResiduePartType result;

   //Implementation
   result.carbohydrate = carbohydrate * y.carbohydrate;
   result.cellulose = cellulose * y.cellulose;
   result.lignin = lignin * y.lignin;

   return result;
   }

void ResiduePartType::setValue(float carbohydrateValue, float celluloseValue, float ligninValue)
{

	carbohydrate = carbohydrateValue;
	cellulose = celluloseValue;
	lignin = ligninValue;
}



// Query
float ResiduePartType::total(void) const
{

	return carbohydrate + cellulose + lignin;
}

void ResiduePartType::display(ostream &os) const
{
	os << "ResiduePartType: " << endl;
	os << "carbohydrate:    " << carbohydrate << endl;
	os << "cellulose:       " << cellulose << endl;
	os << "lignin:          " << lignin << endl;
}

// ----------------------------- ResiduePool ---------------------------------


ResiduePool::ResiduePool(void)			 // member initialisation
{
   standing = 0.0;
   lying = 0.0;
}

ResiduePool::ResiduePool(ResiduePartType standingInit, ResiduePartType lyingInit)
{

	standing = standingInit;
	lying = lyingInit;
}

// destructor
ResiduePool::~ResiduePool(void)
{
}

void ResiduePool::setValue(ResiduePartType standingInit, ResiduePartType lyingInit)
{

	standing = standingInit;
	lying = lyingInit;
}

ostream &operator<<(ostream &output, const ResiduePool &part)
{
	output << "   standing:     " << part.standing << endl;
	output << "   lying:     " << part.lying << endl << endl;
      return output;
}

// copy constructor
//	copy data members of object
ResiduePool::ResiduePool(const ResiduePool &ResiduePool)
{
 	standing = ResiduePool.standing;
	lying=ResiduePool.lying;
}


// Assigment operator
//	assign data members of object
const ResiduePool &ResiduePool::operator=(const ResiduePool &other)
{
	if (&other != this)			// don't self-assign
	{
		// copy data members over
            standing = other.standing;
            lying = other.lying;
      }
	return *this;
}

const ResiduePool &ResiduePool::operator=(const float value)
{
            standing = value;
            lying = value;

	return *this;
}

//===========================================================================
ResiduePool ResiduePool::operator+ (const ResiduePool &y) const
//===========================================================================
   {
   ResiduePool result;

   //Implementation
   result.standing = standing + y.standing;
   result.lying = lying + y.lying;

   return result;
   }

//===========================================================================
ResiduePool ResiduePool::operator- (const ResiduePool &y) const
//===========================================================================
   {
   ResiduePool result;

   //Implementation
   result.standing = standing - y.standing;
   result.lying = lying - y.lying;

   return result;
   }

//===========================================================================
ResiduePool ResiduePool::operator* (const ResiduePool &y) const
//===========================================================================
   {
   ResiduePool result;

   //Implementation
   result.standing = standing * y.standing;
   result.lying = lying * y.lying;

   return result;
   }

//===========================================================================
ResiduePool ResiduePool::operator* (const float y) const
//===========================================================================
   {
   ResiduePool result;

   //Implementation
   result.standing = standing * y;
   result.lying = lying * y;

   return result;
   }

//===========================================================================
ResiduePool ResiduePool::operator/ (const ResiduePool &divisor)  const
//===========================================================================


   {
   ResiduePool quotient;

   //Implementation
   quotient.standing = standing / divisor.standing;
   quotient.lying = lying / divisor.lying;

   return quotient;
   }

//===========================================================================
ResiduePool ResiduePool::operator/ (const float divisor)  const
//===========================================================================


   {
   ResiduePool quotient;

   //Implementation
   quotient.standing = standing / divisor;
   quotient.lying = lying / divisor;

   return quotient;
   }

//===========================================================================
ResiduePool ResiduePool::divide (const ResiduePool &dividend, const ResiduePool &divisor, double default_value)
//===========================================================================

/*Definition
 *   Returns (dividend / divisor) if the division can be done
 *   without overflow or underflow.  If divisor is zero or
 *   overflow would have occurred, a specified default is returned.
 *   If underflow would have occurred, zero is returned.
 *Assumptions
 *   largest/smallest real number is 1.0e+/-30
 *Parameters
 *   dividend:     dividend
 *   divisor:      divisor
 *   defaultValue: default value to return if overflow
 *Calls
 *   reals_are_equal
 */

   {
   ResiduePool quotient;

   //Implementation
   quotient.standing = dividend.standing / divisor.standing;
   quotient.lying = dividend.lying / divisor.lying;

   return quotient;
   }


//===========================================================================
float ResiduePool::divide (float dividend, float divisor, float default_value) const
//===========================================================================

/*Definition
 *   Returns (dividend / divisor) if the division can be done
 *   without overflow or underflow.  If divisor is zero or
 *   overflow would have occurred, a specified default is returned.
 *   If underflow would have occurred, zero is returned.
 *Assumptions
 *   largest/smallest real number is 1.0e+/-30
 *Parameters
 *   dividend:     dividend
 *   divisor:      divisor
 *   defaultValue: default value to return if overflow
 *Calls
 *   reals_are_equal
 */

   {
   //Constant Values
   const float LARGEST = 1.0e30f;    //largest acceptable no. for quotient
   const float SMALLEST = 1.0e-30f;  //smallest acceptable no. for quotient
   const float nought = 0.0f;
   const float one = 1.0f;
   const float granularity = 1.0e-6f;

   //Local Varialbes
   float quotient;

   //Implementation
   if(floatsAreEqual(dividend, nought, granularity))      //multiplying by 0
      {
      quotient = nought;
      }
   else if(floatsAreEqual(divisor, nought, granularity))  //dividing by 0
      {
      quotient = default_value;
      }
   else if(fabs(divisor) < one)            //possible overflow
      {
      if(fabs(dividend) > fabs(LARGEST * divisor)) //overflow
         {
         quotient = default_value;
         }
      else
         {
         quotient = dividend / divisor;          //ok
         }
      }
   else if(fabs(divisor) > one)             //possible underflow
      {
      if(fabs(dividend) < fabs(SMALLEST * divisor))    //underflow
         {
         quotient = nought;
         }
      else
         {
         quotient = dividend / divisor;                //ok
         }
      }
   else
      {
      quotient = dividend / divisor;                   //ok
      }
   return quotient;
   }

// Query
float ResiduePool::total(void) const
{

	return standing.total() + lying.total();
}

void ResiduePool::display(ostream &os) const
{
	os << "ResiduePool: " << endl;
	standing.display(os);
	lying.display(os);
}


