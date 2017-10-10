#include "StdPlant.h"
#include <algorithm>
#include <math.h>
#include "weather.h"
using namespace std;
#define GET_DAYLENGTH	0
#define GET_PHOTOPERIOD 1


CWeather::CWeather()
{
	m_nYear		=0;
	m_nMonth	=0;
	m_nDay		=0;
	m_fGlobalRad    =0.0;
	m_fTempMax	=0.0;
	m_fTempMin	=0.0;
	m_fRelHumid	=0.0;
	m_fVaporPressure=0.0;
	m_fRelHumid	=0.0;

	m_fWindSpeed    =0.0;
	m_fRainfall	=0.0;
	m_fEvaporation	=0.0;
	m_fSunHours	=0.0;
	m_fCloud	=0.0;
}

CWeather::CWeather(int iValue)
{
	m_nYear		=(unsigned)iValue;
	m_nMonth	=(unsigned)iValue;
	m_nDay		=(unsigned)iValue;
	m_fGlobalRad=(float)iValue;
	m_fTempMax	=(float)iValue;
	m_fTempMin	=(float)iValue;
	m_fRelHumid	=(float)iValue;
	m_fVaporPressure	=(float)iValue;
	m_fRelHumid	=(float)iValue;

	m_fWindSpeed=(float)iValue;
	m_fRainfall	=(float)iValue;
	m_fEvaporation	=(float)iValue;
	m_fSunHours	=(float)iValue;
	m_fCloud	=(float)iValue;
}


CWeather::CWeather(int   nYear,		int   nMonth,   int   nDay,
		 		   float fTempMax,	float fTempMin, float fRain,
				   float fRad, float fRelHumid,float fWind, float fSunshine,float fCloud)
{
	m_nYear		=(unsigned)nYear;
	m_nMonth	=(unsigned)nMonth;
	m_nDay		=(unsigned)nDay;
	m_fGlobalRad    =(float)fRad;
	m_fTempMax	=(float)fTempMax;
	m_fTempMin	=(float)fTempMin;
	m_fRelHumid	=(float)fRelHumid;
	m_fWindSpeed    =(float)fWind;
	m_fRainfall	=(float)fRain;
	m_fSunHours	=(float)fSunshine;
	m_fCloud	=(float)fCloud;
}


void CWeather::Initialization(int   nYear,		int   nMonth,   int   nDay,
		 					  float fTempMax,	float fTempMin, float fRain,
							  float fRad, float fRelHumid,float fWind, float fSunshine,float fCloud)
{
	m_nYear		=(unsigned)nYear;
	m_nMonth	=(unsigned)nMonth;
	m_nDay		=(unsigned)nDay;
	m_fGlobalRad=(float)fRad;
	m_fTempMax	=(float)fTempMax;
	m_fTempMin	=(float)fTempMin;
	m_fRelHumid	=(float)fRelHumid;
	m_fWindSpeed=(float)fWind;
	m_fRainfall	=(float)fRain;
	m_fSunHours	=(float)fSunshine;
	m_fCloud	=(float)fCloud;

	return;
}


CWeather::~CWeather()
{
}


float CWeather::GlobalRadiationDailyTotal(float fLatitude, float a, float b)
{
	if (fLatitude == (float)-99)
		return m_fGlobalRad;
	else
		return DailyGlobalRadiation(fLatitude, a, b);
}



float CWeather::GetSunshineHours(float fLatitude, float a, float b)
{
	if (fLatitude == (float)-99)
		return m_fSunHours;
	else
		return SunshineHours(fLatitude, a, b);
}


float CWeather::GetRH(float fLatitude)
{
	if ((m_fRelHumid>=(float)0)&&(m_fRelHumid<=(float)100)||(fLatitude==-99.0))
		return m_fRelHumid;
	else
		{
		float fTemp = HourlyTemperature((float)9.0, fLatitude);
		float fEs   = SaturatedVapourPressureAtTemp(fTemp);
		float fEa   = ActualVapourPressure();
		return (float)100*fEa/fEs;
		}
}


float CWeather::RelativeHumidityDailyMean()
{
        float fTemp = TemperatureDailyMean();
	float fEs   = SaturatedVapourPressureAtTemp(fTemp);
	float fEa   = ActualVapourPressure();
	return (float)100*fEa/fEs;
}


float CWeather::RelativeHumidityDaytimeMean()
{
        float fTemp = TemperatureDaytimeMean();
	float fEs   = SaturatedVapourPressureAtTemp(fTemp);
	float fEa   = ActualVapourPressure();
	return (float)100*fEa/fEs;
}



float CWeather::RelativeHumidityNighttimeMean()
{
        float fTemp = TemperatureNighttimeMean();
	float fEs   = SaturatedVapourPressureAtTemp(fTemp);
	float fEa   = ActualVapourPressure();
	return (float)100*fEa/fEs;
}



int CWeather::DateMonth(int nDayOfYear)
{

	int j, nFeb;
	int i = nDayOfYear;

	if ((m_nYear % 4)==0)  nFeb = 1;
	else				   nFeb = 0;

	if ((i>=1) &&(i<=31))				j = 1;
	if ((i>=32)&&(i<=59+nFeb))			j = 2;
	if ((i>=60+nFeb)&&(i<=90+nFeb))		j = 3;
	if ((i>=91+nFeb)&&(i<=120+nFeb))	j = 4;
	if ((i>=121+nFeb)&&(i<=151+nFeb))	j = 5;
	if ((i>=152+nFeb)&&(i<=181+nFeb))	j = 6;
	if ((i>=182+nFeb)&&(i<=212+nFeb))	j = 7;
	if ((i>=213+nFeb)&&(i<=243+nFeb))	j = 8;
	if ((i>=244+nFeb)&&(i<=273+nFeb))	j = 9;
	if ((i>=274+nFeb)&&(i<=304+nFeb))	j = 10;
	if ((i>=305+nFeb)&&(i<=334+nFeb))	j = 11;
	if (i>=335+nFeb)					j = 12;

	return j;
}


int CWeather::DateDay(int nDayOfYear)
{

	int j, nFeb;
	int i = nDayOfYear;

	if ((m_nYear % 4)==0)  nFeb = 1;
	else				   nFeb = 0;

	if ((i>=1) &&(i<=31))				j = i;
	if ((i>=32)&&(i<=59+nFeb))			j = i-31;
	if ((i>=60+nFeb)&&(i<=90+nFeb))		j = i-(59+nFeb);
	if ((i>=91+nFeb)&&(i<=120+nFeb))	j = i-(90+nFeb);
	if ((i>=121+nFeb)&&(i<=151+nFeb))	j = i-(120+nFeb);
	if ((i>=152+nFeb)&&(i<=181+nFeb))	j = i-(151+nFeb);
	if ((i>=182+nFeb)&&(i<=212+nFeb))	j = i-(181+nFeb);
	if ((i>=213+nFeb)&&(i<=243+nFeb))	j = i-(212+nFeb);
	if ((i>=244+nFeb)&&(i<=273+nFeb))	j = i-(243+nFeb);
	if ((i>=274+nFeb)&&(i<=304+nFeb))	j = i-(273+nFeb);
	if ((i>=305+nFeb)&&(i<=334+nFeb))	j = i-(304+nFeb);
	if (i>=335+nFeb)					j = i-(334+nFeb);

	return j;
}

int CWeather::GetDayOfYear()
{
	unsigned DaysOfMonth[]={0, 31,28,31,30,31,30,31,31,30,31,30,31};
	
	if ((m_nYear % 4)==0) DaysOfMonth[2]=29;

	int DayOfYear = 0; 
	for (int i=1; i<m_nMonth; i++)
		 DayOfYear += DaysOfMonth[i];

	DayOfYear += m_nDay;

	return (int)DayOfYear;

}


int CWeather::GetJulianDay()
{
	return GetDayOfYear();
}




float CWeather::Daylength(float fLatitude)
{
	int nDay = GetDayOfYear();
	
	return DaylengthAndPhotoperiod(fLatitude, nDay, GET_DAYLENGTH);
}


float CWeather::Photoperiod(float fLatitude)
{
	int nDay = GetDayOfYear();
	
	return DaylengthAndPhotoperiod(fLatitude, nDay, GET_PHOTOPERIOD);

}

float CWeather::SunriseTime(float fLatitude)
{
	return (float)12-(float)0.5*Daylength(fLatitude);
}

float CWeather::SunsetTime(float fLatitude)
{
	return (float)12 + (float)0.5*Daylength(fLatitude);

}


float CWeather::SunshineHours(float fLatitude, float a, float b)
{
	//The calculated value has a unit of J/m2/d
	return ::SunshineHours(fLatitude, m_nDay, m_fGlobalRad, a, b);

}


float CWeather::DailyGlobalRadiation (float fLatitude, float a, float b)
{
	//The calculated value has a unit of J/m2/d
	//The indicative calues for empirical constants in the Angstrom formula in relation 
	//to latitude and climate used by FAO (Frere & Popov, 1979)
	//Cold and Temperate Zones: a=0.18, b=0.55
	//Dry tropical zones:		a=0.25, b=0.45
	//Humid tropical zones:		a=0.29, b=0.42

	return ::GlobalRadiationDailyTotal(fLatitude, m_nDay, m_fSunHours, a, b);
}


float CWeather::DirectRadiationDailyTotal (float fLatitude)
{
	//The calculated value has a unit of J/m2/d
	return ::DirectRadiationDailyTotal(m_fGlobalRad, fLatitude, m_nDay);
}


float CWeather::DiffuseRadiationDailyTotal(float fLatitude)
{
	//The calculated value has a unit of J/m2/d
	return ::DiffuseRadiationDailyTotal(m_fGlobalRad, fLatitude, m_nDay);
}

float CWeather::GlobalRadiation (float fLatitude, float fHour)
{
	//The calculated value has a unit of J/m2/d
	return ::GlobalRadiation(m_fGlobalRad,fLatitude, m_nDay,fHour);
}


float CWeather::DirectRadiation (float fLatitude, float fHour)
{
	//The calculated value has a unit of J/m2/d
	return ::DirectRadiation(m_fGlobalRad,fLatitude, m_nDay,fHour);
}


float CWeather::DiffuseRadiation(float fLatitude, float fHour)
{
	//The calculated value has a unit of J/m2/d
	return ::DiffuseRadiation(m_fGlobalRad,fLatitude, m_nDay,fHour);
}


float CWeather::ExtraTerrestrialRadiation(float fLatitude, float fHour)
{
	return ::ExtraTerrestrialRadiation(fLatitude, m_nDay, fHour);
}


float CWeather::ExtraTerrestrialRadiationDailyTotal(float fLatitude)
{
	return ::ExtraTerrestrialRadiationDailyTotal(fLatitude, m_nDay);

}


float CWeather::TemperatureAt0900(float fLatitude)
{

	return HourlyTemperature((float)9.0, fLatitude);

}


float CWeather::TemperatureAtSunrise()
{
	return m_fTempMin;
}

float CWeather::TemperatureAtSunset(float fLatitude)
{
	float  fHour = SunsetTime(fLatitude);
	return HourlyTemperature(fHour,fLatitude);
}



float CWeather::HourlyTemperature(float fHour,float fLatitude,float fTminPrev, float fTmaxPrev, float fTminNext )
{
	int nDay=GetDayOfYear();

        if (fLatitude == -99.0)
           return ::HourlyTemperature(fHour,m_fTempMax,m_fTempMin);
        else
	   return ::HourlyTemperature(fLatitude, nDay, fHour, m_fTempMax,m_fTempMin,fTmaxPrev,fTminPrev,fTminNext);
}



float CWeather::TemperatureDailyMean(float fLatitude,float fTminPrev, float fTmaxPrev, float fTminNext)
{
        return ::DailyMeanTemperature(m_fTempMax,m_fTempMin, fLatitude, m_nDay, fTminPrev, fTmaxPrev, fTminNext);
}


float CWeather::TemperatureDaytimeMean(float fLatitude,float fTminPrev, float fTmaxPrev, float fTminNext)
{

	return ::DaytimeMeanTemperature(m_fTempMax, m_fTempMin, fLatitude, m_nDay, fTminPrev, fTmaxPrev, fTminNext);

}


float CWeather::TemperatureNighttimeMean(float fLatitude,float fTminPrev, float fTmaxPrev, float fTminNext)
{

	return ::NighttimeMeanTemperature(m_fTempMax, m_fTempMin, fLatitude, m_nDay, fTminPrev, fTmaxPrev, fTminNext);

}


float CWeather::TemperatureTimeInNight(float fTemp, float fLatitude,float fTsstPrev, float fSstTimePrev)
{
	if ((fTemp>fTsstPrev)||(fTemp<m_fTempMin)) return (float)-1;

	double	vTmin = (double)m_fTempMin;
	double	vTsst = (double)fTsstPrev;
	double  vTemp = (double)fTemp;
	double  vTc   = 4.0;
	double  vNL   = 24.0 - (double)Daylength(fLatitude);

	//Ew modified the following section
	double  vValue,vHour;

	if ((vTemp>=vTmin)&&(vTemp<=vTsst))
		{
		vValue = ( vTemp-vTmin +(vTsst-vTemp)*exp(-vNL/vTc))/(vTsst-vTmin);
		vHour  = log(vValue);
	        vHour  = (double)fSstTimePrev - vTc*vHour;
	        if (vHour>=24.0) vHour = vHour-24.0;
		}
	else
		{
		if (vTemp<vTmin) vHour = fSstTimePrev + vNL;
		if (vTemp>vTsst) vHour = fSstTimePrev;
		}


	return (float)vHour;
}



float CWeather::TemperatureTimeDuringDayBeforeNoon(float fTemp, float fLatitude)
{
	if ((fTemp>m_fTempMax)||(fTemp<m_fTempMin)) return (float)-1;

	double	vTmin = (double)m_fTempMin;
	double	vTmax = (double)m_fTempMax;
	double  vTemp = (double)fTemp;
	double  vPc   = 1.5;
	double  vDL   = (double)Daylength(fLatitude);

	double vHour = asin((vTemp-vTmin)/(vTmax-vTmin))*(vDL+2*vPc)/3.1415926+12-vDL/2;

	return (float)vHour;
}



float CWeather::DewPeriodDuringDayBeforeNoon(float fDewPointTemp, float fLatitude)
{
	if (fDewPointTemp<=m_fTempMin) return (float)0;

	float fSunriseTime = SunriseTime(fLatitude);
	float fEndDewTime  = TemperatureTimeDuringDayBeforeNoon(fDewPointTemp, fLatitude);

	return max((float)0, fEndDewTime - fSunriseTime);

}

float CWeather::DewPeriodLastNight(float fDewPointTemp, float fLatitude, float fTsstPrev, float fSstTimePrev)
{
        float fDewPeriod;

	if (fDewPointTemp<=m_fTempMin) return (float)0;

	float fBeginDewTime  = TemperatureTimeInNight(fDewPointTemp, fLatitude, fTsstPrev, fSstTimePrev);
	float fSunriseTime   = SunriseTime(fLatitude);

        if (fBeginDewTime<24.0)
           fDewPeriod = 24.0f - fBeginDewTime + fSunriseTime;
        else
           fDewPeriod = max((float)0, fSunriseTime - fBeginDewTime);

	return fDewPeriod;

}


float CWeather::DewPeriodDailyTotal(float fDewPointTemp, float fLatitude, float fTsstPrev, float fSstTimePrev)
{
        float fDewPeriodDay = DewPeriodDuringDayBeforeNoon(fDewPointTemp, fLatitude);
        float fDewPeriodNgt = DewPeriodLastNight(fDewPointTemp, fLatitude, fTsstPrev, fSstTimePrev);

	return fDewPeriodDay + fDewPeriodNgt;

}

float CWeather::DurationTemperatureBelow(float fTemp, float fLatitude, float fTsstPrev, float fSstTimePrev)
{

        return DewPeriodDailyTotal(fTemp, fLatitude, fTsstPrev, fSstTimePrev);

}

float CWeather::DewPointFromVapourPressureAt0900()
{
	return DewPointTemperature((float)100.0*m_fVaporPressure);
}


float CWeather::RH80TempFromVapourPressureAt0900()
{
	return DewPointTemperature((float)(100.0/0.8)*m_fVaporPressure);
}


float CWeather::DewPointTemperature()
{
	float vVap9     = 100.0f * m_fVaporPressure;  //hPa
	float vDewPoint = 0.0;

        if (vVap9 <= 0.0)
            vDewPoint = m_fTempMin;
        else
            vDewPoint = DewPointTemperature(vVap9);

	return vDewPoint;

}



float CWeather::DewPointTemperature(float fVapourPressure)
{
	double vVap9     = (double)fVapourPressure;  //Pa
	double vDewPoint = 239.0*log(vVap9/610.7)/(17.4-log(vVap9/610.7));

	return (float)vDewPoint;

}




float CWeather::ActualVapourPressure()
{
	float vVap9  = 100.0f * m_fVaporPressure;  //Pa

        if (vVap9 >0.0)
           return vVap9;
        else
	   return ::SaturatedVapourPressure(m_fTempMin);

}


float CWeather::SaturatedVapourPressureAtTemp(float fTemp)
{
	return ::SaturatedVapourPressure(fTemp);

}



float CWeather::RelativeHumidity(float fTemp)
{
	float fEs,fEa,fRH;

	fEs = SaturatedVapourPressureAtTemp(fTemp);
	fEa = ActualVapourPressure();
	fRH = (float)100*fEa/fEs;

	return fRH;
}

float CWeather::RelativeHumidity(float fHour,float fLatitude,float fTminPrev, float fTmaxPrev, float fTminNext )
{
	float fTmp, fEs,fEa,fRH;

        fTmp= HourlyTemperature(fHour,fLatitude,fTminPrev,fTmaxPrev,fTminNext);
	fEs = SaturatedVapourPressureAtTemp(fTmp);
	fEa = ActualVapourPressure();
	fRH = (float)100*fEa/fEs;

	return fRH;

}

float CWeather::SaturatedVapourPressure(float fHour,float fLatitude,float fTminPrev, float fTmaxPrev, float fTminNext )
{
	float fTmp, fEs;

        fTmp= HourlyTemperature(fHour,fLatitude,fTminPrev,fTmaxPrev,fTminNext);
	fEs = SaturatedVapourPressureAtTemp(fTmp);

	return fEs;

}



float CWeather::WindSpeedDailyMean()
{
	float  fWind = (float)(m_fWindSpeed * 1000.0/ (24.0 * 3600.0));

	return fWind; //m/s

}

float CWeather::WindSpeedDaytimeMean()
{
	float  fWind = WindSpeedDailyMean();

	return (float)(fWind *(4.0/3.0)); //m/s

}


float CWeather::WindSpeedNighttimeMean()
{
	float  fWind = WindSpeedDailyMean();

	return (float)(fWind *(2.0/3.0)); //m/s

}


float CWeather::WindSpeed(float fHour, float fLatitude)
{
	double SINLD,COSLD,vLatitude,vDay,vDec,RAD;
	double PI = 3.1415926;

	RAD 	  = PI/180.0;
	vLatitude = (double)fLatitude;
	vDay 	  = (double)m_nDay;

	//Declination of the sun as function of iJulianDaynumber (iJulianDay)
	vDec = -asin( sin(23.45*RAD)*cos(2.0*PI*(vDay+10.0)/365.0));

	//Intermediate variables
	SINLD = sin(RAD*vLatitude)*sin(vDec);
	COSLD = cos(RAD*vLatitude)*cos(vDec);

        double vSinB=SINLD+COSLD*cos(2*PI*((double)fHour-12.0)/24.0);

        float ratio  = (float)max(0.0, vSinB/(SINLD+COSLD));
	float fWdDay = WindSpeedDaytimeMean();
	float fWdNgt = WindSpeedNighttimeMean();

        float fWind  = fWdNgt + (float)2.0*(fWdDay-fWdNgt)*ratio;

	return fWind; //m/s

}









//#####################################################################################################
//THESE ARE THE GLOBAL WEATHER FUNCTIONS
//The CWeather class uses these functions
//They can be used by no object-oriented programming precedures
//#####################################################################################################

//======================================================================================
//This function calculates the astronomical daylength and photoperiod
//======================================================================================
float DaylengthAndPhotoperiod(float fLatitude, int nJulianDay, int nID)
	{
	double SINLD,COSLD,vLatitude,vDay,vDec,RAD, ratio;
	float  fReturn,fDaylength,fPhotoperiod,fDaylengthPs;
	double PI = 3.1415926;

	RAD 	  = PI/180.0;
	vLatitude = (double)fLatitude;
	vDay 	  = (double)nJulianDay;

	//Declination of the sun as function of iJulianDaynumber (iJulianDay)
	vDec = -asin( sin(23.45*RAD)*cos(2.0*PI*(vDay+10.0)/365.0));

	//Intermediate variables
	SINLD = sin(RAD*vLatitude)*sin(vDec);
	COSLD = cos(RAD*vLatitude)*cos(vDec);
	ratio = SINLD/COSLD;
        ratio = min(1.0, max(-1.0, ratio));

	//Astronomical daylength (hr)
	fDaylength   =(float)(12.0*(1.0+2.0*asin(ratio)/PI));

   	//Photoperiodically active daylength (hr)
	ratio = (-sin(-4.0*RAD)+SINLD)/COSLD;
        ratio = min(1.0, max(-1.0, ratio));
	fPhotoperiod =(float)(12.0*(1.0+2.0*asin(ratio)/PI));

   	//Photosythesis active daylength (hr)
	ratio = (-sin(8.0*RAD)+SINLD)/COSLD;
        ratio = min(1.0, max(-1.0, ratio));
	fDaylengthPs =(float)(12.0*(1.0+2.0*asin(ratio)/PI));


	fReturn=fDaylength;

	if (nID==GET_DAYLENGTH)
		fReturn=fDaylength;
	if (nID==GET_PHOTOPERIOD)
		fReturn=fPhotoperiod;
	if (nID==3)
		fReturn=fDaylengthPs;
	
	return fReturn;
	}


float ExtraTerrestrialRadiation(float fLatitude, int nDay, float fHour)
{
	double SINLD,COSLD,vLatitude,vDay,vHour,vDec,RAD;
	double PI     = 3.1415926;
	double vSolar = 1370.0;		//W/m2 - the solar constant

	RAD 	  = PI/180.0;
	vLatitude = (double)fLatitude;
	vDay 	  = (double)nDay;
	vHour	  = (double)fHour;

	//Declination of the sun as function of iJulianDaynumber (iJulianDay)
	vDec = -asin( sin(23.45*RAD)*cos(2.0*PI*(vDay +10.0)/365.0));

	//Intermediate variables
	SINLD = sin(RAD*vLatitude)*sin(vDec);
	COSLD = cos(RAD*vLatitude)*cos(vDec);

	double vSinB = SINLD + COSLD * cos(2*PI*(vHour-12)/24);
	double vRad  = vSolar*vSinB*(1+0.033*cos(2*PI*(vDay-10)/365));
               vRad  = max(0.0, vRad);

	return (float)vRad;

}


float ExtraTerrestrialRadiationDailyTotal(float fLatitude, int nDay)
{
	double SINLD,COSLD,/*TANSQ,*/vLatitude,vDay,vDec,RAD;
	double PI	  = 3.1415926;
	double vSolar = 1370.0;		//W/m2 - the solar constant
	
	RAD 	  = PI/180.0;
	vLatitude = (double)fLatitude;
	vDay 	  = (double)nDay;

	//Declination of the sun as function of iJulianDaynumber (iJulianDay)
	vDec = -asin( sin(23.45*RAD)*cos(2.0*PI*(vDay+10.0)/365.0));
	
	//Intermediate variables
	SINLD = sin(RAD*vLatitude)*sin(vDec);
	COSLD = cos(RAD*vLatitude)*cos(vDec);

	//TANSQ = sqrt(1-tan(RAD*vLatitude)*tan(RAD*vLatitude)*tan(vDec)*tan(vDec));
 	//double vSumSinB	  = 3600*(vDaylength*SINLD + (24/PI)*COSLD*cos(TANSQ));

	double vDaylen = (double)DaylengthAndPhotoperiod(fLatitude, nDay,GET_DAYLENGTH);

        double vSunriseTime = 12.0 - 0.5*vDaylen;
        double vSunsetTime  = 12.0 + 0.5*vDaylen;
        double vStep        = 0.01;

        //Calculation of SumSinB from sunrise to sunset
        double vSumSinB     = 0.0;
        double vSinBB       = 0.0;
        for (double h=vSunriseTime; h<vSunsetTime; h = h+vStep)
            {
            vSinBB   = SINLD + COSLD * cos(2*PI*(h-12)/24);
            vSumSinB = vSumSinB + max(0.0, vSinBB * vStep); //hours
            }

         vSumSinB = vSumSinB * 3600;  //seconds


	double vRad  = vSolar*1.0E-6*(1+0.033*cos(2*PI*(vDay-10)/365))*vSumSinB; //MJ/d
               vRad  = max(0.0, vRad);

	return (float)vRad;

}


float GlobalRadiationDailyTotal(float fLatitude, int nDay, float fSunHours, float a, float b)
{
	//The calculated value has a unit of MJ/m2/d
	//The indicative calues for empirical constants in the Angstrom formula in relation 
	//to latitude and climate used by FAO (Frere & Popov, 1979)
	//Cold and Temperate Zones: a=0.18, b=0.55
	//Dry tropical zones:		a=0.25, b=0.45
	//Humid tropical zones:		a=0.29, b=0.42

	float fExtRad    = ExtraTerrestrialRadiationDailyTotal(fLatitude, nDay);
	float fDaylength = DaylengthAndPhotoperiod(fLatitude, nDay,GET_DAYLENGTH);
	float fGlbRad    = fExtRad*(a+b*fSunHours/fDaylength); 

	return fGlbRad;
}


float SunshineHours(float fLatitude, int nDay, float fGlbRadDay, float a, float b)
{
	//The calculated value has a unit of J/m2/d
	//The indicative calues for empirical constants in the Angstrom formula in relation 
	//to latitude and climate used by FAO (Frere & Popov, 1979)
	//Cold and Temperate Zones: a=0.18, b=0.55
	//Dry tropical zones:		a=0.25, b=0.45
	//Humid tropical zones:		a=0.29, b=0.42

	float fExtRadDay = ExtraTerrestrialRadiationDailyTotal(fLatitude, nDay);
	float fDaylength = DaylengthAndPhotoperiod(fLatitude, nDay,GET_DAYLENGTH);
	float fHours     = fDaylength*(fGlbRadDay/fExtRadDay-a)/b;
    
	return fHours;
}

float GlobalRadiation(float fGlbRadDay, float fLatitude, int nDay, float fHour)
{
	double SINLD,COSLD,vLatitude,vDay,vHour,vDec,RAD,vRad;
	double vDaylen,vSinB,vSumSinB,vSinBB;
	double PI     = 3.1415926;
	double vSolar = 1370.0;		//W/m2 - the solar constant
	double c      = 0.4;

	RAD 	  = PI/180.0;
	vLatitude = (double)fLatitude;
	vDay 	  = (double)nDay;
	vHour	  = (double)fHour;

	//Declination of the sun as function of iJulianDaynumber (iJulianDay)
	vDec = -asin( sin(23.45*RAD)*cos(2.0*PI*(vDay+10.0)/365.0));

	//Intermediate variables
	SINLD = sin(RAD*vLatitude)*sin(vDec);
	COSLD = cos(RAD*vLatitude)*cos(vDec);

	vDaylen = (double)DaylengthAndPhotoperiod(fLatitude, nDay,GET_DAYLENGTH);
	vSinB   = SINLD + COSLD * cos(2*PI*(vHour-12)/24);


        //double TANSQ   = sqrt(1-tan(RAD*vLatitude)*tan(RAD*vLatitude)*tan(vDec)*tan(vDec));
        //double vSumSinB = 3600*(vDaylen*(SINLD+c*(SINLD*SINLD+0.5*COSLD*COSLD))
        //					  *(24/PI)*COSLD*cos(1+1.5*c*SINLD)*TANSQ);

        double vSunriseTime = 12.0 - 0.5*vDaylen;
        double vSunsetTime  = 12.0 + 0.5*vDaylen;
        double vStep        = 0.01;

        //Calculation of Sum of SinB(1+c*SinB) from sunrise to sunset
        vSumSinB  = 0.0;

        for (double h=vSunriseTime; h<vSunsetTime; h = h+vStep)
            {
            vSinBB   = SINLD + COSLD * cos(2*PI*(h-12)/24);
            vSumSinB = vSumSinB + max(0.0, vSinBB*(1+c*vSinBB) * vStep); //hours
            }

         vSumSinB = vSumSinB * 3600;  //seconds

         vRad = (double)fGlbRadDay*1.0E+6*vSinB*(1+c*vSinB)/vSumSinB; //J/m2/s

         vRad = max(0.0, vRad);

	return (float)vRad;
}



float RadiationDiffuseComponent(float fGlbRadDay, float fLatitude, int nDay, float fHour)
{

	float fExtRad = ExtraTerrestrialRadiation(fLatitude, nDay,fHour);
	float fGlbRad = GlobalRadiation(fGlbRadDay,fLatitude,nDay,fHour);

	double vTrans = 0.0;

        if (fExtRad != (float)0.0) vTrans = (double)(fGlbRad/fExtRad);

	double vFdif;

	if (vTrans<=0.22)
		vFdif = 1.0;
	if ((vTrans>0.22)&&(vTrans<=0.35))
		vFdif = 1.0-6.4*(vTrans-0.22)*(vTrans-0.22);
	if (vTrans>0.35)
		vFdif = 1.47-1.66*vTrans;

	return (float)vFdif;
}


float DiffuseRadiation(float fGlbRadDay, float fLatitude, int nDay, float fHour)
{
	float fFdif   = RadiationDiffuseComponent(fGlbRadDay,fLatitude,nDay,fHour);
	float fGlbRad = GlobalRadiation(fGlbRadDay,fLatitude,nDay,fHour);
	float fDifRad = fGlbRad * fFdif;

	return fDifRad;
}


float DirectRadiation(float fGlbRadDay, float fLatitude, int nDay, float fHour)
{
	float fFdif   = RadiationDiffuseComponent(fGlbRadDay,fLatitude,nDay,fHour);
	float fGlbRad = GlobalRadiation(fGlbRadDay,fLatitude,nDay,fHour);
	float fDirRad = fGlbRad * ((float)1.0-fFdif);

	return fDirRad;
}


float RadiationDailyTotalDiffuseComponent(float fGlbRadDay, float fLatitude, int nDay)
{

	float fExtRadDay = ExtraTerrestrialRadiationDailyTotal(fLatitude, nDay);

	double vTrans = (double)(fGlbRadDay/fExtRadDay);

	double vFdif;

	if (vTrans<=0.07)
		vFdif = 1.0;
	if ((vTrans>0.07)&&(vTrans<=0.35))
		vFdif = 1.0-2.3*(vTrans-0.07)*(vTrans-0.07);
	if ((vTrans>0.35)&&(vTrans<=0.75))
		vFdif = 1.33-1.46*vTrans;
	if (vTrans>0.75)
		vFdif = 0.23;

	return (float)vFdif;
}


float DiffuseRadiationDailyTotal(float fGlbRadDay, float fLatitude, int nDay)
{
	float fFdif      = RadiationDailyTotalDiffuseComponent(fGlbRadDay,fLatitude,nDay);
	float fDifRadDay = fGlbRadDay * fFdif;

	return fDifRadDay;
}



float DirectRadiationDailyTotal(float fGlbRadDay, float fLatitude, int nDay)
{
	float fFdif      = RadiationDailyTotalDiffuseComponent(fGlbRadDay,fLatitude,nDay);
	float fDirRadDay = fGlbRadDay * ((float)1.0-fFdif);

	return fDirRadDay;
}



//======================================================================================
//This function calculates the hourly temperature from daily temperature maximum and 
//minimum based on a sin curve. It assumes the lowest temperature at 0 O'clock and the 
//maximum temperature occurs at 12 O'clock
//======================================================================================

float HourlyTemperature(float fHour,float fTmax,float fTmin)
{
	float fTemp;

	fTemp = fTmin +(float)0.5*(fTmax-fTmin)
			*(float)(1.0+sin(3.1415926*((double)fHour-12+0.5*12)/12));

	return fTemp;
}


//======================================================================================
//This function calculates the hourly temperature from daily temperature maximum and
//minimum. It uses the temperature maximum and minimum from yesterday and today and the
//temperature minimum of tommorrow. It can give a very precise calculation .
//From sunrise to sunset a SIN function is used assuming the maximum temperature
//occurs after noon. From Sunset to sunrise a exponential function is used letting the
//minimum temperature occur at sunrise.
//======================================================================================
float HourlyTemperature(float fLatitude, int nDay, float fHour, float fTmax, float fTmin,
						float fTmaxPrev, float fTminPrev,float fTminNext)
{
double vTc   = 5.0;//4.0
double vPc   = 2.5;//1.5

double vDayLenPrev   = (double)DaylengthAndPhotoperiod(fLatitude, max(1,nDay-1), 0);
double vNghtLenPrev  = 24.0 - vDayLenPrev;
double vSsetTimePrev = 12.0 + 0.5*vDayLenPrev;
double vTssetPrev    = (double)fTminPrev+(double)(fTmaxPrev-fTminPrev)
					   *sin(3.1415926*(vSsetTimePrev-12.0+0.5*vDayLenPrev)/(vDayLenPrev+2.0*vPc));
float  fTssetPrev    = (float)vTssetPrev;

double vDayLen   = (double)DaylengthAndPhotoperiod(fLatitude, nDay, 0);
double vNghtLen  = 24.0 -     vDayLen;
double vSsetTime = 12.0 + 0.5*vDayLen;
double vSriseTime= 12.0 - 0.5*vDayLen;
float  fTsset    = fTmin+(fTmax-fTmin)*(float)sin(3.1415926*(vSsetTime-12.0+0.5*vDayLen)/(vDayLen+2.0*vPc));

double vHour;

if (fHour<(float)vSriseTime)
	vHour=24.0+(double)fHour;
else
	vHour=(double)fHour;

float a,b,fTemp;

if (fHour<(float)vSriseTime)
	{
	a = (float)exp(-vNghtLenPrev/vTc);
	b = (float)exp(-(vHour-vSsetTimePrev)/vTc);

	fTemp = (fTmin-fTssetPrev*a + (fTssetPrev-fTmin)*b)/((float)1.0-a);
	}


if ((fHour>=(float)vSriseTime)&&(fHour<=(float)vSsetTime))
	{
	fTemp = fTmin +(fTmax-fTmin)*(float)sin(3.1415926*(vHour-12.0+0.5*vDayLen)/(vDayLen+2.0*vPc));
	}


if ((fHour>(float)vSsetTime)&&(fHour<(float)24))
	{
	a = (float)exp(-vNghtLen/vTc);
	b = (float)exp(-(vHour-vSsetTime)/vTc);

	fTemp = (fTminNext-fTsset*a + (fTsset-fTminNext)*b)/((float)1.0-a);
	}

return fTemp;

}


float DailyMeanTemperature(float fTmax,float fTmin,float fLatitude,int nDay, float fTminPrev, float fTmaxPrev, float fTminNext)
{
        float fSum = 0.0;
        float fTemp= 0.0;

        if (fLatitude == -99.0)
            fTemp = (float)0.5*(fTmax+fTmin);
        else
            {
            fSum = 0.0;
            for (int h=0; h<=23; h++)
                 {
                 fTemp = HourlyTemperature(fLatitude, nDay, (float)h, fTmax, fTmin,
				           fTmaxPrev, fTminPrev, fTminNext);
                 fSum = fSum + fTemp;
                 }
            fTemp = fSum/24.0f;

            }

        return fTemp;
}


float DaytimeMeanTemperature(float fTmax,float fTmin,float fLatitude,int nDay, float fTminPrev, float fTmaxPrev, float fTminNext)
{
        float fSum    = 0.0;
        float fTemp   = 0.0;

        if (fLatitude == -99.0)
            fTemp = (float)0.71*fTmax+ (float)0.29*fTmin;
        else
            {
            fSum = 0.0;

            float fDaylength = DaylengthAndPhotoperiod(fLatitude, nDay, GET_DAYLENGTH);
            float fSunrise   = (float)12-(float)0.5*fDaylength;
            float fSunset    = (float)12+(float)0.5*fDaylength;

            for (float h=fSunrise; h<fSunset; h=h+(fSunset-fSunrise)/(float)12.0)
                 {
                 fTemp = HourlyTemperature(fLatitude, nDay, (float)h, fTmax, fTmin,
				           fTmaxPrev, fTminPrev, fTminNext);
                 fSum = fSum + fTemp;
                 }
            fTemp = fSum/12.0f;

            }

        return fTemp;
}


float NighttimeMeanTemperature(float fTmax,float fTmin,float fLatitude,int nDay, float fTminPrev, float fTmaxPrev, float fTminNext)
{
        float fSum    = 0.0;
        float fTemp   = 0.0;

        if (fLatitude == -99.0)
            fTemp = (float)0.29*fTmax+ (float)0.71*fTmin;
        else
            {
            fSum = 0.0;

            float fDaylength    = DaylengthAndPhotoperiod(fLatitude, nDay,   GET_DAYLENGTH);
            float fDaylengthPrev= DaylengthAndPhotoperiod(fLatitude, nDay-1, GET_DAYLENGTH);
            float fSunrise      = (float)12-(float)0.5*fDaylength;
            float fSunsetPrev   = (float)12+(float)0.5*fDaylengthPrev;

            float fStep         = ((float)24.0 - fSunsetPrev + fSunrise)/(float)12.0;
            float fCount        = 0.0;

            for (float h=fSunsetPrev; h<(float)24.0; h=h+fStep)
                 {
                 fTemp = HourlyTemperature(fLatitude, nDay-1, (float)h, fTmaxPrev, fTminPrev,
				           0.0, 0.0, fTmin);
                 fSum = fSum + fTemp;
                 fCount ++;
                 }

            for (float h=0.0; h<fSunrise; h=h+fStep)
                 {
                 fTemp = HourlyTemperature(fLatitude, nDay, (float)h, fTmax, fTmin,
				           fTmaxPrev, fTminPrev, fTminNext);
                 fSum = fSum + fTemp;
                 fCount ++;
                 }

            fTemp = fSum/fCount;

            }

        return fTemp;
}



//======================================================================================
//This function calculates the temperature dependent saturated vapour pressure in
//the air. The calculated value has an unit of Pa (1mb=100Pa)
//======================================================================================
float SaturatedVapourPressure(float fTemp)
{
	//The calculated value has a unit of Pa
	double vEs, vTemp;

	vTemp = (double)fTemp;
	vEs   = 610.7*exp(17.4*vTemp/(239.0+vTemp));

	return (float)vEs;
}










