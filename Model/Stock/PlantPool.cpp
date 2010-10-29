
// Modification log
// 6 Aug 97 J. Hargreaves  Implementation

#define YES 1
#define NO 0
#define TEST_PlantPool YES                            // build unit test?
#include <math.h>
#include <iostream>

#include "PlantPool.H"

      inline bool floatsAreEqual(float A, float B, float C) {return(fabs(A-B)<C);}

// default constructor
//    initialise data members.
// ----------------------------- PlantPartType ---------------------------------

PlantPartType::PlantPartType(void)                   // member initialisation
{
   leaf = 0.0;
   stem = 0.0;
   pod = 0.0;
}

PlantPartType::PlantPartType(float leafInit, float stemInit, float podInit)
{

      leaf = leafInit;
      stem = stemInit;
      pod = podInit;
}

// destructor
PlantPartType::~PlantPartType(void)
{
}

std::ostream & operator <<(std::ostream &output, const PlantPartType &part)
{
      output << "         Leaf:     " << part.leaf << std::endl;
      output << "         Stem:     " << part.stem << std::endl;
      output << "         Pod:      " << part.pod << std::endl;
      return output;
}

// copy constructor
//    copy data members of object
PlantPartType::PlantPartType(const PlantPartType &PlantPartType)
{
      leaf = PlantPartType.leaf;
      stem=PlantPartType.stem;
      pod=PlantPartType.pod;
}


// Assigment operator
//    assign data members of object
const PlantPartType &PlantPartType::operator=(const PlantPartType &other)
{
      if (&other != this)                 // don't self-assign
      {
            // copy data members over
            leaf = other.leaf;
            stem = other.stem;
            pod = other.pod;
      }
      return *this;
}

const PlantPartType &PlantPartType::operator=(const float &y)
{
            // copy data members over
            leaf = y;
            stem = y;
            pod = y;
      return *this;
}

//===========================================================================
PlantPartType PlantPartType::operator+ (const PlantPartType &y) const
//===========================================================================

/*Definition
 */

   {
   //Constant Values

   //Local Varialbes
   PlantPartType result;

   //Implementation
   result.leaf = leaf + y.leaf;
   result.stem = stem + y.stem;
   result.pod = pod + y.pod;

   return result;
   }


//===========================================================================
PlantPartType PlantPartType::operator- (const PlantPartType &y) const
//===========================================================================

/*Definition
 */

   {
   //Constant Values

   //Local Varialbes
   PlantPartType result;

   //Implementation
   result.leaf = leaf - y.leaf;
   result.stem = stem - y.stem;
   result.pod = pod - y.pod;

   return result;
   }

//===========================================================================
PlantPartType PlantPartType::operator* (const PlantPartType &y) const
//===========================================================================

/*Definition
 */

   {
   //Constant Values

   //Local Varialbes
   PlantPartType result;

   //Implementation
   result.leaf = leaf * y.leaf;
   result.stem = stem * y.stem;
   result.pod = pod * y.pod;

   return result;
   }

//===========================================================================
PlantPartType PlantPartType::operator* (const float &y) const
//===========================================================================

/*Definition
 */

   {
   //Constant Values

   //Local Varialbes
   PlantPartType result;

   //Implementation
   result.leaf = leaf * y;
   result.stem = stem * y;
   result.pod = pod * y;

   return result;
   }

//===========================================================================
PlantPartType PlantPartType::multiply (const PlantPartType &y)
//===========================================================================

/*Definition
 */

   {
   //Constant Values

   //Local Varialbes
   PlantPartType result;

   //Implementation
   result.leaf = leaf * y.leaf;
   result.stem = stem * y.stem;
   result.pod = pod * y.pod;

   return result;
   }

//===========================================================================
PlantPartType PlantPartType::operator/ (const PlantPartType &divisor)  const
//===========================================================================


   {
   //Constant Values

   //Local Varialbes
   PlantPartType quotient;

   //Implementation
   quotient.leaf = divide(leaf, divisor.leaf, 0.0);
   quotient.stem = divide(stem, divisor.stem, 0.0);
   quotient.pod = divide(pod, divisor.pod, 0.0);

   return quotient;
   }

//===========================================================================
PlantPartType PlantPartType::operator/ (const float &divisor)  const
//===========================================================================


   {
   //Constant Values

   //Local Varialbes
   PlantPartType quotient;

   //Implementation
   quotient.leaf = divide(leaf, divisor, 0.0);
   quotient.stem = divide(stem, divisor, 0.0);
   quotient.pod = divide(pod, divisor, 0.0);

   return quotient;
   }

//===========================================================================
PlantPartType PlantPartType::divide (PlantPartType &dividend, PlantPartType &divisor, float default_value)
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
   PlantPartType quotient;

   //Implementation
   quotient.leaf = divide(dividend.leaf, divisor.leaf, default_value);
   quotient.stem = divide(dividend.stem, divisor.stem, default_value);
   quotient.pod = divide(dividend.pod, divisor.pod, default_value);

   return quotient;
   }

void PlantPartType::setValue(float leafValue, float stemValue, float podValue)
//===========================================================================
{

      leaf = leafValue;
      stem = stemValue;
      pod = podValue;
}

float PlantPartType::total(void) const
//===========================================================================
{

      return (leaf + stem + pod);
}

//===========================================================================
float PlantPartType::divide (const float dividend, const float divisor, float default_value) const
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
   const float LARGEST = 1.0e30;    //largest acceptable no. for quotient
   const float SMALLEST = 1.0e-30;  //smallest acceptable no. for quotient
   const float nought = 0.0;
   const float one = 1.0;
   const float granularity = 1.0e-6;

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



// ----------------------------- PlantPool ---------------------------------

// default constructor
//    initialise data members.
PlantPool::PlantPool(void)                     // member initialisation
{
   green.setValue(0.0, 0.0, 0.0);
   senesced.setValue(0.0, 0.0, 0.0);
}

PlantPool::PlantPool(float greenLeaf, float greenStem, float greenPod, float senescedLeaf, float senescedStem, float senescedPod)
{

      green.setValue(greenLeaf, greenStem, greenPod);
      senesced.setValue(senescedLeaf, senescedStem, senescedPod);
}

// destructor
PlantPool::~PlantPool(void)
{
}

std::ostream & operator << (std::ostream &output, const PlantPool &pool)
{
      output << "PlantPool:" << std::endl;
      output << "   Green :    " << std::endl << pool.green;
      output << "   Senesced : " << std::endl << pool.senesced;
      return output;
}

// copy constructor
//    copy data members of object
PlantPool::PlantPool(const PlantPool &PlantPool)
{
      green = PlantPool.green;
      senesced=PlantPool.senesced;
}


// Assigment operator
//    assign data members of object
const PlantPool &PlantPool::operator=(const PlantPool &other)
{
      if (&other != this)                 // don't self-assign
      {
            // copy data members over
            green = other.green;
            senesced = other.senesced;
      }
      return *this;
}

const PlantPool &PlantPool::operator=(const float &y)
{
      // copy data members over
      green = y;
      senesced = y;
      return *this;
}

//===========================================================================
PlantPool PlantPool::operator+ (const PlantPool &y) const
//===========================================================================

/*Definition
 */

   {
   //Constant Values

   //Local Varialbes
   PlantPool result;

   //Implementation
   result.green = green + y.green;
   result.senesced = senesced + y.senesced;
   return result;
   }


//===========================================================================
PlantPool PlantPool::operator- (const PlantPool &y) const
//===========================================================================

/*Definition
 */

   {
   //Constant Values

   //Local Varialbes
   PlantPool result;

   //Implementation
   result.green = green - y.green;
   result.senesced = senesced - y.senesced;
   return result;
   }

//===========================================================================
PlantPool PlantPool::operator* (const PlantPool &y) const
//===========================================================================

/*Definition
 */

   {
   //Constant Values

   //Local Varialbes
   PlantPool result;

   //Implementation
   result.green = green * y.green;
   result.senesced = senesced * y.senesced;
   return result;
   }

//===========================================================================
PlantPool PlantPool::operator* (const float &y) const
//===========================================================================

/*Definition
 */

   {
   //Constant Values

   //Local Varialbes
   PlantPool result;

   //Implementation
   result.green = green * y;
   result.senesced = senesced * y;
   return result;
   }

//===========================================================================
PlantPool PlantPool::operator/ (const PlantPool &divisor)  const
//===========================================================================


   {
   //Constant Values

   //Local Varialbes
   PlantPool quotient;

   //Implementation
   quotient.green = green / divisor.green;
   quotient.senesced = senesced / divisor.senesced;
   return quotient;
   }

//===========================================================================
PlantPool PlantPool::operator/ (const float &divisor)  const
//===========================================================================


   {
   //Constant Values

   //Local Varialbes
   PlantPool quotient;

   //Implementation
   quotient.green = green / divisor;
   quotient.senesced = senesced / divisor;
   return quotient;
   }

//===========================================================================
PlantPool PlantPool::multiply (const PlantPool &y)
//===========================================================================

/*Definition
 */

   {
   //Constant Values

   //Local Varialbes
   PlantPool result;

   //Implementation
   result.green = green * y.green;
   result.senesced = senesced * y.senesced;
   return result;
   }

void PlantPool::setValue(float greenLeaf, float greenStem, float greenPod, float senescedLeaf, float senescedStem, float senescedPod)
{

      green.setValue(greenLeaf, greenStem, greenPod);
      senesced.setValue(senescedLeaf, senescedStem, senescedPod);
}

void PlantPool::setValue(PlantPartType &greenValue, PlantPartType &senescedValue)
{

      green = greenValue;
      senesced = senescedValue;
}



// Query
float PlantPool::total(void) const
{
      return (green.total() + senesced.total());
}

void PlantPool::display(std::ostream &os) const
{
      os << "PlantPool: " << std::endl;
      os << "Green leaf: " << green.leaf << std::endl;
      os << "Green stem: " << green.stem << std::endl;
      os << "Green pod: " << green.pod << std::endl;
      os << "Senesced leaf: " << senesced.leaf << std::endl;
      os << "Senesced stem: " << senesced.stem << std::endl;
      os << "Senesced pod: " << senesced.pod << std::endl;
}




#if TEST_PlantPool                                          // build unit test?


// PlantPool class test harness

// Tests default constructor, copy constructor, assignment operator and
// each of the get and set functions.  Does not test the destructor.

// Modification log
// 6 Aug 97  J. Hargreaves    Initial implementation


#ifndef PlantPool_H
#include "PlantPool.h"
#endif

using namespace std;

int main(void)
{
      std::cout << "PlantPool test started" << std::endl;

      PlantPool p; //, *aPtr = &p;

      std::cout << std::endl << "Test set and get functions:" << endl;
      p.setValue(10.0, 2.0, 1.0, 20.0, 3.0, 1.5);
      if (p.total() != 69.0)
            cout << "setValue(10.0, 2.0, 1.0, 20.0, 3.0, 1.5) / Total() test FAILED"
            << endl;

      cout << endl << "Test default constructor:" << endl;
      PlantPool q;                                                            // run default constructor
      if (q.total() != 0.0)
            cout << "default constructor test FAILED" << endl;

      cout << endl << "Test constructor:" << endl;
      PlantPool a(1.0, 2.0, 3.0, 4.0, 5.0, 6.0);                                                               // run default constructor
      if (a.total() != 21.0)
            cout << "constructor test FAILED" << endl;

      cout << endl << "Test copy constructor:" << endl;
      PlantPool s = p;                       // run copy constructor
      if (s.total() != p.total())
      cout << "copy constructor test FAILED" << endl;

      cout << endl << "Test assignment operator:" << endl;
      s.setValue(50.0, 5.0, 0.5, 60.0, 6.0, 0.6); // change object

      if (s.total() != p.total())
      {
            s = p;                          // run operator=
            if (s.total() != p.total())
                  cout << "assignment operator test FAILED" << endl;
      }
      else
            cout << "assignment operator test FAILED DIFFERENCE TEST" << endl;

      cout << endl << "Test multiply operator:" << endl;
      s.setValue(50.0, 5.0, 0.5, 60.0, 6.0, 0.6); // change object
      PlantPool k = p * s;
      if (k.total() != 3856.0)
            cout << "multiply operator test FAILED" << endl;

      cout << endl << "Test simple multiply operator:" << endl;
      s.setValue(50.0, 5.0, 0.5, 60.0, 6.0, 0.6); // change object
       k = s * 2.0;
      if (k.total() != 396.0)
            cout << "simple multiply operator test FAILED" << endl;

      cout << endl << "Test divide operator:" << endl;
      s.setValue(50.0, 5.0, 0.5, 60.0, 6.0, 0.6); // change object
       k = s/p;
      if (k.total() < 16.58332 || k.total() > 16.58334)
            cout << "divide operator test FAILED" << endl;

      cout << endl << "Test simple divide operator:" << endl;
      s.setValue(50.0, 5.0, 0.5, 60.0, 6.0, 0.6); // change object
       k = s / 2.0;
      if (k.total() != 99.0)
            cout << "simple divide operator test FAILED" << endl;

      PlantPool t;
      t.setValue(50.0, 5.0, 0.5, 60.0, 6.0, 0.6); // change object
      cout << endl << "Display PlantPool t" << endl;
      t.display();

      PlantPool x;
      x.setValue(50.0, 5.0, 0.5, 60.0, 6.0, 0.6); // change object

      cout << endl << "Display PlantPool x - static binding" << endl;
      x.display();

      cout << endl << "Display PlantPool x - dynamic binding" << endl;
      PlantPool *PlantPoolPtr = &x;
      PlantPoolPtr->display();

      cout << endl << "PlantPool test finished" << endl;
      return 0;
}

#endif

