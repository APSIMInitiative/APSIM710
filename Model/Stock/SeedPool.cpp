
// Modification log
// 6 Aug 97 J. Hargreaves  Implementation

#define YES 1
#define NO 0
#define TEST_SeedPool NO                            // build unit test?
#include <math.h>

#include <iostream>
using namespace std;

#include "SeedPool.H"

      inline bool floatsAreEqual(float A, float B, float C) {return(fabs(A-B)<C);}

// default constructor
//    initialise data members.
// ----------------------------- SeedPartType ---------------------------------

SeedPartType::SeedPartType()                   // member initialisation
{
   meal = 0.0;
   oil = 0.0;
}

SeedPartType::SeedPartType(float mealInit, float oilInit)
{

      meal = mealInit;
      oil = oilInit;
}

// destructor
SeedPartType::~SeedPartType()
{
}

ostream &operator<<(ostream &output, const SeedPartType &part)
{
      output << "         meal:     " << part.meal << endl;
      output << "         oil:      " << part.oil << endl;
      return output;
}

// copy constructor
//    copy data members of object
SeedPartType::SeedPartType(const SeedPartType &SeedPartType)
{
      meal = SeedPartType.meal;
      oil=SeedPartType.oil;
}


// Assigment operator
//    assign data members of object
const SeedPartType &SeedPartType::operator=(const SeedPartType &other)
{
      if (&other != this)                 // don't self-assign
      {
            // copy data members over
            meal = other.meal;
            oil = other.oil;
      }
      return *this;
}

const SeedPartType &SeedPartType::operator=(const float &y)
{
            // copy data members over
            meal = y;
            oil = y;
      return *this;
}

//===========================================================================
SeedPartType SeedPartType::operator+ (const SeedPartType &y) const
//===========================================================================

   {
   //Constant Values

   //Local Varialbes
   SeedPartType result;

   //Implementation
   result.meal = meal + y.meal;
   result.oil = oil + y.oil;

   return result;
   }


//===========================================================================
SeedPartType SeedPartType::operator- (const SeedPartType &y) const
//===========================================================================

/*Definition
 */

   {
   //Constant Values

   //Local Varialbes
   SeedPartType result;

   //Implementation
   result.meal = meal - y.meal;
   result.oil = oil - y.oil;

   return result;
   }

//===========================================================================
SeedPartType SeedPartType::operator* (const SeedPartType &y) const
//===========================================================================

/*Definition
 */

   {
   //Constant Values

   //Local Varialbes
   SeedPartType result;

   //Implementation
   result.meal = meal * y.meal;
   result.oil = oil * y.oil;

   return result;
   }

//===========================================================================
SeedPartType SeedPartType::operator* (const float &y) const
//===========================================================================

/*Definition
 */

   {
   //Constant Values

   //Local Varialbes
   SeedPartType result;

   //Implementation
   result.meal = meal * y;
   result.oil = oil * y;

   return result;
   }

//===========================================================================
SeedPartType SeedPartType::multiply (const SeedPartType &y)
//===========================================================================

/*Definition
 */

   {
   //Constant Values

   //Local Varialbes
   SeedPartType result;

   //Implementation
   result.meal = meal * y.meal;
   result.oil = oil * y.oil;

   return result;
   }

//===========================================================================
SeedPartType SeedPartType::operator/ (const SeedPartType &divisor)  const
//===========================================================================


   {
   //Constant Values

   //Local Varialbes
   SeedPartType quotient;

   //Implementation
   quotient.meal = divide(meal, divisor.meal, 0.0);
   quotient.oil = divide(oil, divisor.oil, 0.0);

   return quotient;
   }

//===========================================================================
SeedPartType SeedPartType::operator/ (const float &divisor)  const
//===========================================================================


   {
   //Constant Values

   //Local Varialbes
   SeedPartType quotient;

   //Implementation
   quotient.meal = divide(meal, divisor, 0.0);
   quotient.oil = divide(oil, divisor, 0.0);

   return quotient;
   }

//===========================================================================
SeedPartType SeedPartType::divide (SeedPartType &dividend, SeedPartType &divisor, float default_value)
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

   //Local Varialbes
   SeedPartType quotient;

   //Implementation
   quotient.meal = divide(dividend.meal, divisor.meal, default_value);
   quotient.oil = divide(dividend.oil, divisor.oil, default_value);

   return quotient;
   }

void SeedPartType::setValue(float mealValue, float oilValue)
//===========================================================================
{

      meal = mealValue;
      oil = oilValue;
}

float SeedPartType::total() const
//===========================================================================
{

      return (meal + oil);
}

//===========================================================================
float SeedPartType::divide (const float dividend, const float divisor, float default_value) const
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



// ----------------------------- SeedPool ---------------------------------

// default constructor
//    initialise data members.
SeedPool::SeedPool()                     // member initialisation
{
   green.setValue(0.0, 0.0);
   senesced.setValue(0.0, 0.0);

}

SeedPool::SeedPool(float greenmeal, float greenoil, float senescedmeal, float senescedoil)
{

      green.setValue(greenmeal, greenoil);
      senesced.setValue(senescedmeal, senescedoil);

}

// destructor
SeedPool::~SeedPool()
{
}

ostream &operator<<(ostream &output, const SeedPool &pool)
{
      output << "SeedPool:" << endl;
      output << "   Green :    " << endl << pool.green;
      output << "   Senesced : " << endl << pool.senesced;
      return output;
}

// copy constructor
//    copy data members of object
SeedPool::SeedPool(const SeedPool &SeedPool)
{
      green = SeedPool.green;
      senesced=SeedPool.senesced;

}


// Assigment operator
//    assign data members of object
const SeedPool &SeedPool::operator=(const SeedPool &other)
{
      if (&other != this)                 // don't self-assign
      {
            // copy data members over
            green = other.green;
            senesced = other.senesced;

      }
      return *this;
}

const SeedPool &SeedPool::operator=(const float &y)
{
            // copy data members over
            green = y;
            senesced = y;

      return *this;
}

//===========================================================================
SeedPool SeedPool::operator+ (const SeedPool &y) const
//===========================================================================

/*Definition
 */

   {
   //Constant Values

   //Local Varialbes
   SeedPool result;

   //Implementation
   result.green = green + y.green;
   result.senesced = senesced + y.senesced;

   return result;
   }


//===========================================================================
SeedPool SeedPool::operator- (const SeedPool &y) const
//===========================================================================

/*Definition
 */

   {
   //Constant Values

   //Local Varialbes
   SeedPool result;

   //Implementation
   result.green = green - y.green;
   result.senesced = senesced - y.senesced;

   return result;
   }

//===========================================================================
SeedPool SeedPool::operator* (const SeedPool &y) const
//===========================================================================

/*Definition
 */

   {
   //Constant Values

   //Local Varialbes
   SeedPool result;

   //Implementation
   result.green = green * y.green;
   result.senesced = senesced * y.senesced;

   return result;
   }

//===========================================================================
SeedPool SeedPool::operator* (const float &y) const
//===========================================================================

/*Definition
 */

   {
   //Constant Values

   //Local Varialbes
   SeedPool result;

   //Implementation
   result.green = green * y;
   result.senesced = senesced * y;

   return result;
   }

//===========================================================================
SeedPool SeedPool::operator/ (const SeedPool &divisor)  const
//===========================================================================


   {
   //Constant Values

   //Local Varialbes
   SeedPool quotient;

   //Implementation
   quotient.green = green / divisor.green;
   quotient.senesced = senesced / divisor.senesced;

   return quotient;
   }

//===========================================================================
SeedPool SeedPool::operator/ (const float &divisor)  const
//===========================================================================


   {
   //Constant Values

   //Local Varialbes
   SeedPool quotient;

   //Implementation
   quotient.green = green / divisor;
   quotient.senesced = senesced / divisor;

   return quotient;
   }


//===========================================================================
SeedPool SeedPool::multiply (const SeedPool &y)
//===========================================================================

/*Definition
 */

   {
   //Constant Values

   //Local Varialbes
   SeedPool result;

   //Implementation
   result.green = green * y.green;
   result.senesced = senesced * y.senesced;

   return result;
   }

void SeedPool::setValue(float greenmeal, float greenoil, float senescedmeal, float senescedoil)
{

      green.setValue(greenmeal, greenoil);
      senesced.setValue(senescedmeal, senescedoil);

}

void SeedPool::setValue(SeedPartType &greenValue, SeedPartType &senescedValue)
{

      green = greenValue;
      senesced = senescedValue;

}



// Query
float SeedPool::total() const
{
      return (green.total() + senesced.total());
}

void SeedPool::display(ostream &os) const
{
      os << "SeedPool: " << endl;
      os << "Green meal: " << green.meal << endl;
      os << "Green oil: " << green.oil << endl;
      os << "Senesced meal: " << senesced.meal << endl;
      os << "Senesced oil: " << senesced.oil << endl;
}




#if TEST_SeedPool                                          // build unit test?


// SeedPool class test harness

// Tests default constructor, copy constructor, assignment operator and
// each of the get and set functions.  Does not test the destructor.

// Modification log
// 6 Aug 97  J. Hargreaves    Initial implementation


#ifndef SeedPool_H
#include "SeedPool.h"
#endif

int main()
{
	cout << "SeedPool test started" << endl;

	SeedPool p; //, *aPtr = &p;

	cout << endl << "Test set and get functions:" << endl;
	p.setValue(10.0, 2.0, 1.0, 20.0);
	if (p.total() != 69.0)
		cout << "setValue(10.0, 2.0, 1.0, 20.0) / Total() test FAILED"
		<< endl;

	cout << endl << "Test default constructor:" << endl;
	SeedPool q;                           						// run default constructor
	if (q.total() != 0.0)
		cout << "default constructor test FAILED" << endl;

	cout << endl << "Test constructor:" << endl;
	SeedPool a(1.0, 2.0, 3.0, 4.0);                           						// run default constructor
	if (a.total() != 21.0)
		cout << "constructor test FAILED" << endl;

	cout << endl << "Test copy constructor:" << endl;
	SeedPool s = p;                       // run copy constructor
	if (s.total() != p.total())
      cout << "copy constructor test FAILED" << endl;

	cout << endl << "Test assignment operator:" << endl;
	s.setValue(50.0, 5.0, 0.5, 60.0); // change object

	if (s.total() != p.total())
	{
		s = p;                          // run operator=
		if (s.total() != p.total())
			cout << "assignment operator test FAILED" << endl;
	}
	else
		cout << "assignment operator test FAILED DIFFERENCE TEST" << endl;

	cout << endl << "Test multiply operator:" << endl;
	s.setValue(50.0, 5.0, 0.5, 60.0); // change object
	SeedPool k = p * s;
	if (k.total() != 3856.0)
		cout << "multiply operator test FAILED" << endl;

	cout << endl << "Test simple multiply operator:" << endl;
	s.setValue(50.0, 5.0, 0.5, 60.0); // change object
	 k = s * 2.0;
	if (k.total() != 396.0)
		cout << "simple multiply operator test FAILED" << endl;

	cout << endl << "Test divide operator:" << endl;
	s.setValue(50.0, 5.0, 0.5, 60.0); // change object
	 k = s/p;
	if (k.total() < 16.58332 || k.total() > 16.58334)
		cout << "divide operator test FAILED" << endl;

	cout << endl << "Test simple divide operator:" << endl;
	s.setValue(50.0, 5.0, 0.5, 60.0); // change object
	 k = s / 2.0;
	if (k.total() != 99.0)
		cout << "simple divide operator test FAILED" << endl;

	SeedPool t;
	t.setValue(50.0, 5.0, 0.5, 60.0); // change object
	cout << endl << "Display SeedPool t" << endl;
	t.display();

	SeedPool x;
	x.setValue(50.0, 5.0, 0.5, 60.0); // change object

	cout << endl << "Display SeedPool x - static binding" << endl;
	x.display();

	cout << endl << "Display SeedPool x - dynamic binding" << endl;
	SeedPool *SeedPoolPtr = &x;
	SeedPoolPtr->display();

	cout << endl << "SeedPool test finished" << endl;
	return 0;
}

#endif

