{--
More list, list comprehension
filter/map

Based on the [(cityName, tempreature, month, year)] named with cityInformation
			  [([Char], Number, Integer, Integer)] and month should be in range [0,11]
--}

-- write funciion getCityNames [cityInformation], retursns [cityName] withtout duplications
-- write function getYearInformation [cityInformation] cityName Year, returning (tempreature, month) for a spesific Year
-- write function invalidValues [cityInformation], returns [cityInformation] with invalind months value of dublication of the information for yeat and month of a city
-- write function getValidYearInformation [cityInformation] cityName Year returning (tempreature, month) for a spesific Year
-- write function averageYearTemperature [cityInformation] Year, returns the average temperature for the current city in case that we dont have enough information, such as missing month returns -1, otherwise the correct value
-- write function mostHotCity [cityInformation] Year, returning the most hot city for a current year.
-- write function citiesByTemperature [cityInformation] Year, returning a list [(City, temperature)] based on the average temperature fir the current year, sorted acs.

testData = [ 
    ("Burgas", -19, 0, 2010),
    ("Burgas", 7, 1, 2010),
    ("Burgas", -6, 2, 2010),
    ("Burgas", 0, 3, 2010),
    ("Burgas", -11, 4, 2010),
    ("Burgas", -16, 5, 2010),
    ("Burgas", 18, 6, 2010),
    ("Burgas", 18, 7, 2010),
    ("Burgas", -18, 8, 2010),
    ("Burgas", 4, 9, 2010),
    ("Burgas", 5, 10, 2010),
    ("Burgas", 5, 11, 2010),
    ("Varna", -19, 0, 2010),
    ("Varna", 7, 1, 2010),
    ("Varna", -19, 2, 2010),
    ("Varna", -9, 3, 2010),
    ("Varna", 15, 4, 2010),
    ("Varna", 2, 5, 2010),
    ("Varna", 7, 6, 2010),
    ("Varna", 16, 7, 2010),
    ("Varna", 11, 8, 2010),
    ("Varna", -16, 9, 2010),
    ("Varna", 2, 10, 2010),
    ("Varna", 13, 11, 2010),
    ("Plovdiv", -8, 0, 2010),
    ("Plovdiv", 2, 1, 2010),
    ("Plovdiv", 1, 2, 2010),
    ("Plovdiv", 16, 3, 2010),
    ("Plovdiv", 18, 4, 2010),
    ("Plovdiv", -5, 5, 2010),
    ("Plovdiv", -13, 6, 2010),
    ("Plovdiv", -14, 7, 2010),
    ("Plovdiv", -9, 8, 2010),
    ("Plovdiv", -2, 9, 2010),
    ("Plovdiv", 9, 10, 2010),
    ("Plovdiv", 12, 11, 2010),
    ("Sofia", 7, 0, 2010),
    ("Sofia", -1, 1, 2010),
    ("Sofia", 15, 2, 2010),
    ("Sofia", -6, 3, 2010),
    ("Sofia", 3, 4, 2010),
    ("Sofia", -9, 5, 2010),
    ("Sofia", -18, 6, 2010),
    ("Sofia", -7, 7, 2010),
    ("Sofia", 13, 8, 2010),
    ("Sofia", 4, 9, 2010),
    ("Sofia", 1, 10, 2010),
    ("Sofia", 11, 11, 2010),
    ("Vratsa", -7, 0, 2010),
    ("Vratsa", 8, 1, 2010),
    ("Vratsa", 7, 2, 2010),
    ("Vratsa", -16, 3, 2010),
    ("Vratsa", 2, 4, 2010),
    ("Vratsa", 17, 5, 2010),
    ("Vratsa", 17, 6, 2010),
    ("Vratsa", -1, 7, 2010),
    ("Vratsa", -17, 8, 2010),
    ("Vratsa", 1, 9, 2010),
    ("Vratsa", -11, 10, 2010),
    ("Vratsa", -2, 11, 2010),
    ("Burgas", 16, 0, 2011),
    ("Burgas", 15, 1, 2011),
    ("Burgas", 10, 2, 2011),
    ("Burgas", -18, 3, 2011),
    ("Burgas", -12, 4, 2011),
    ("Burgas", 6, 5, 2011),
    ("Burgas", -20, 6, 2011),
    ("Burgas", 2, 7, 2011),
    ("Burgas", 4, 8, 2011),
    ("Burgas", -12, 9, 2011),
    ("Burgas", -14, 10, 2011),
    ("Burgas", -15, 11, 2011),
    ("Varna", -10, 0, 2011),
    ("Varna", -11, 1, 2011),
    ("Varna", -10, 2, 2011),
    ("Varna", 10, 3, 2011),
    ("Varna", -14, 4, 2011),
    ("Varna", 1, 5, 2011),
    ("Varna", 13, 6, 2011),
    ("Varna", 8, 7, 2011),
    ("Varna", 9, 8, 2011),
    ("Varna", 3, 9, 2011),
    ("Varna", -16, 10, 2011),
    ("Varna", 14, 11, 2011),
    ("Plovdiv", 16, 0, 2011),
    ("Plovdiv", -20, 1, 2011),
    ("Plovdiv", -14, 2, 2011),
    ("Plovdiv", -4, 3, 2011),
    ("Plovdiv", -9, 4, 2011),
    ("Plovdiv", 8, 5, 2011),
    ("Plovdiv", 4, 6, 2011),
    ("Plovdiv", 19, 7, 2011),
    ("Plovdiv", 6, 8, 2011),
    ("Plovdiv", -17, 9, 2011),
    ("Plovdiv", -3, 10, 2011),
    ("Plovdiv", -2, 11, 2011),
    ("Sofia", 18, 0, 2011),
    ("Sofia", -18, 1, 2011),
    ("Sofia", -11, 2, 2011),
    ("Sofia", 1, 3, 2011),
    ("Sofia", 13, 4, 2011),
    ("Sofia", 15, 5, 2011),
    ("Sofia", 19, 6, 2011),
    ("Sofia", -2, 7, 2011),
    ("Sofia", 4, 8, 2011),
    ("Sofia", -10, 9, 2011),
    ("Sofia", -3, 10, 2011),
    ("Sofia", 6, 11, 2011),
    ("Vratsa", 13, 0, 2011),
    ("Vratsa", 6, 1, 2011),
    ("Vratsa", 1, 2, 2011),
    ("Vratsa", 5, 3, 2011),
    ("Vratsa", -16, 4, 2011),
    ("Vratsa", 12, 5, 2011),
    ("Vratsa", 10, 6, 2011),
    ("Vratsa", 9, 7, 2011),
    ("Vratsa", -3, 8, 2011),
    ("Vratsa", -7, 9, 2011),
    ("Vratsa", -3, 10, 2011),
    ("Vratsa", 12, 11, 2011),
    ("Burgas", 6, 0, 2012),
    ("Burgas", -10, 1, 2012),
    ("Burgas", -19, 2, 2012),
    ("Burgas", 16, 3, 2012),
    ("Burgas", 15, 4, 2012),
    ("Burgas", -13, 5, 2012),
    ("Burgas", -5, 6, 2012),
    ("Burgas", -6, 7, 2012),
    ("Burgas", 11, 8, 2012),
    ("Burgas", -8, 9, 2012),
    ("Burgas", 10, 10, 2012),
    ("Burgas", 10, 11, 2012),
    ("Varna", 1, 0, 2012),
    ("Varna", -16, 1, 2012),
    ("Varna", -14, 2, 2012),
    ("Varna", 10, 3, 2012),
    ("Varna", 7, 4, 2012),
    ("Varna", 11, 5, 2012),
    ("Varna", -13, 6, 2012),
    ("Varna", -3, 7, 2012),
    ("Varna", -3, 8, 2012),
    ("Varna", -13, 9, 2012),
    ("Varna", 13, 10, 2012),
    ("Varna", 3, 11, 2012),
    ("Plovdiv", 5, 0, 2012),
    ("Plovdiv", 9, 1, 2012),
    ("Plovdiv", -11, 2, 2012),
    ("Plovdiv", 18, 3, 2012),
    ("Plovdiv", 1, 4, 2012),
    ("Plovdiv", 8, 5, 2012),
    ("Plovdiv", 2, 6, 2012),
    ("Plovdiv", 6, 7, 2012),
    ("Plovdiv", 6, 8, 2012),
    ("Plovdiv", 10, 9, 2012),
    ("Plovdiv", -7, 10, 2012),
    ("Plovdiv", -12, 11, 2012),
    ("Sofia", 0, 0, 2012),
    ("Sofia", 11, 1, 2012),
    ("Sofia", -18, 2, 2012),
    ("Sofia", -5, 3, 2012),
    ("Sofia", -10, 4, 2012),
    ("Sofia", 19, 5, 2012),
    ("Sofia", 4, 6, 2012),
    ("Sofia", -3, 7, 2012),
    ("Sofia", 8, 8, 2012),
    ("Sofia", -17, 9, 2012),
    ("Sofia", 15, 10, 2012),
    ("Sofia", -19, 11, 2012),
    ("Vratsa", -18, 0, 2012),
    ("Vratsa", 10, 1, 2012),
    ("Vratsa", -9, 2, 2012),
    ("Vratsa", 16, 3, 2012),
    ("Vratsa", -6, 4, 2012),
    ("Vratsa", 0, 5, 2012),
    ("Vratsa", 16, 6, 2012),
    ("Vratsa", 1, 7, 2012),
    ("Vratsa", 8, 8, 2012),
    ("Vratsa", 19, 9, 2012),
    ("Vratsa", 8, 10, 2012),
    ("Vratsa", -16, 11, 2012),
    ("Burgas", -19, 0, 2013),
    ("Burgas", -6, 1, 2013),
    ("Burgas", -7, 2, 2013),
    ("Burgas", 19, 3, 2013),
    ("Burgas", -2, 4, 2013),
    ("Burgas", -2, 5, 2013),
    ("Burgas", 0, 6, 2013),
    ("Burgas", 8, 7, 2013),
    ("Burgas", -13, 8, 2013),
    ("Burgas", 7, 9, 2013),
    ("Burgas", -12, 10, 2013),
    ("Burgas", -7, 11, 2013),
    ("Varna", -12, 0, 2013),
    ("Varna", -17, 1, 2013),
    ("Varna", -13, 2, 2013),
    ("Varna", 1, 3, 2013),
    ("Varna", 10, 4, 2013),
    ("Varna", -3, 5, 2013),
    ("Varna", -7, 6, 2013),
    ("Varna", 14, 7, 2013),
    ("Varna", 9, 8, 2013),
    ("Varna", -4, 9, 2013),
    ("Varna", -5, 10, 2013),
    ("Varna", -9, 11, 2013),
    ("Plovdiv", -20, 0, 2013),
    ("Plovdiv", -11, 1, 2013),
    ("Plovdiv", 19, 2, 2013),
    ("Plovdiv", 16, 3, 2013),
    ("Plovdiv", 18, 4, 2013),
    ("Plovdiv", 3, 5, 2013),
    ("Plovdiv", 4, 6, 2013),
    ("Plovdiv", -12, 7, 2013),
    ("Plovdiv", -16, 8, 2013),
    ("Plovdiv", -11, 9, 2013),
    ("Plovdiv", 9, 10, 2013),
    ("Plovdiv", 2, 11, 2013),
    ("Sofia", 15, 0, 2013),
    ("Sofia", -15, 1, 2013),
    ("Sofia", -7, 2, 2013),
    ("Sofia", 3, 3, 2013),
    ("Sofia", -17, 4, 2013),
    ("Sofia", 7, 5, 2013),
    ("Sofia", 14, 6, 2013),
    ("Sofia", 3, 7, 2013),
    ("Sofia", -12, 8, 2013),
    ("Sofia", -20, 9, 2013),
    ("Sofia", -2, 10, 2013),
    ("Sofia", -2, 11, 2013),
    ("Vratsa", 0, 0, 2013),
    ("Vratsa", 16, 1, 2013),
    ("Vratsa", 18, 2, 2013),
    ("Vratsa", -19, 3, 2013),
    ("Vratsa", 9, 4, 2013),
    ("Vratsa", 18, 5, 2013),
    ("Vratsa", -11, 6, 2013),
    ("Vratsa", 17, 7, 2013),
    ("Vratsa", 12, 8, 2013),
    ("Vratsa", 2, 9, 2013),
    ("Vratsa", -2, 10, 2013),
    ("Vratsa", -8, 11, 2013),
    ("Burgas", 18, 0, 2014),
    ("Burgas", -1, 1, 2014),
    ("Burgas", 10, 2, 2014),
    ("Burgas", -3, 3, 2014),
    ("Burgas", 18, 4, 2014),
    ("Burgas", 11, 5, 2014),
    ("Burgas", -5, 6, 2014),
    ("Burgas", -12, 7, 2014),
    ("Burgas", 16, 8, 2014),
    ("Burgas", 11, 9, 2014),
    ("Burgas", -18, 10, 2014),
    ("Burgas", 14, 11, 2014),
    ("Varna", 12, 0, 2014),
    ("Varna", -5, 1, 2014),
    ("Varna", -12, 2, 2014),
    ("Varna", -14, 3, 2014),
    ("Varna", -18, 4, 2014),
    ("Varna", -14, 5, 2014),
    ("Varna", 15, 6, 2014),
    ("Varna", 13, 7, 2014),
    ("Varna", 9, 8, 2014),
    ("Varna", 2, 9, 2014),
    ("Varna", -16, 10, 2014),
    ("Varna", -4, 11, 2014),
    ("Plovdiv", -19, 0, 2014),
    ("Plovdiv", 18, 1, 2014),
    ("Plovdiv", -18, 2, 2014),
    ("Plovdiv", -9, 3, 2014),
    ("Plovdiv", 1, 4, 2014),
    ("Plovdiv", -1, 5, 2014),
    ("Plovdiv", 17, 6, 2014),
    ("Plovdiv", 16, 7, 2014),
    ("Plovdiv", -8, 8, 2014),
    ("Plovdiv", 9, 9, 2014),
    ("Plovdiv", 15, 10, 2014),
    ("Plovdiv", 12, 11, 2014),
    ("Sofia", -20, 0, 2014),
    ("Sofia", 10, 1, 2014),
    ("Sofia", -17, 2, 2014),
    ("Sofia", 9, 3, 2014),
    ("Sofia", 1, 4, 2014),
    ("Sofia", -12, 5, 2014),
    ("Sofia", -19, 6, 2014),
    ("Sofia", 9, 7, 2014),
    ("Sofia", -5, 8, 2014),
    ("Sofia", 3, 9, 2014),
    ("Sofia", -18, 10, 2014),
    ("Sofia", 5, 11, 2014),
    ("Vratsa", 2, 0, 2014),
    ("Vratsa", -15, 1, 2014),
    ("Vratsa", -12, 2, 2014),
    ("Vratsa", 6, 3, 2014),
    ("Vratsa", -3, 4, 2014),
    ("Vratsa", 17, 5, 2014),
    ("Vratsa", 12, 6, 2014),
    ("Vratsa", -8, 7, 2014),
    ("Vratsa", -11, 8, 2014),
    ("Vratsa", 14, 9, 2014),
    ("Vratsa", -19, 10, 2014),
    ("Vratsa", 9, 11, 2014),
    ("Burgas", -4, 0, 2015),
    ("Burgas", -11, 1, 2015),
    ("Burgas", -12, 2, 2015),
    ("Burgas", -8, 3, 2015),
    ("Burgas", 5, 4, 2015),
    ("Burgas", 15, 5, 2015),
    ("Burgas", 14, 6, 2015),
    ("Burgas", 9, 7, 2015),
    ("Burgas", -19, 8, 2015),
    ("Burgas", 12, 9, 2015),
    ("Burgas", 5, 10, 2015),
    ("Burgas", 0, 11, 2015),
    ("Varna", 18, 0, 2015),
    ("Varna", 13, 1, 2015),
    ("Varna", -1, 2, 2015),
    ("Varna", 3, 3, 2015),
    ("Varna", 19, 4, 2015),
    ("Varna", 16, 5, 2015),
    ("Varna", -13, 6, 2015),
    ("Varna", -11, 7, 2015),
    ("Varna", 9, 8, 2015),
    ("Varna", 17, 9, 2015),
    ("Varna", 6, 10, 2015),
    ("Varna", 9, 11, 2015),
    ("Plovdiv", 13, 0, 2015),
    ("Plovdiv", 15, 1, 2015),
    ("Plovdiv", -3, 2, 2015),
    ("Plovdiv", -4, 3, 2015),
    ("Plovdiv", -14, 4, 2015),
    ("Plovdiv", 5, 5, 2015),
    ("Plovdiv", -12, 6, 2015),
    ("Plovdiv", -18, 7, 2015),
    ("Plovdiv", -5, 8, 2015),
    ("Plovdiv", -6, 9, 2015),
    ("Plovdiv", 14, 10, 2015),
    ("Plovdiv", 1, 11, 2015),
    ("Sofia", 16, 0, 2015),
    ("Sofia", 11, 1, 2015),
    ("Sofia", 6, 2, 2015),
    ("Sofia", 3, 3, 2015),
    ("Sofia", 13, 4, 2015),
    ("Sofia", 15, 5, 2015),
    ("Sofia", 5, 6, 2015),
    ("Sofia", -7, 7, 2015),
    ("Sofia", 12, 8, 2015),
    ("Sofia", -12, 9, 2015),
    ("Sofia", 12, 10, 2015),
    ("Sofia", 5, 11, 2015),
    ("Vratsa", 13, 0, 2015),
    ("Vratsa", 16, 1, 2015),
    ("Vratsa", -19, 2, 2015),
    ("Vratsa", 18, 3, 2015),
    ("Vratsa", -14, 4, 2015),
    ("Vratsa", 2, 5, 2015),
    ("Vratsa", -19, 6, 2015),
    ("Vratsa", 4, 7, 2015),
    ("Vratsa", 16, 8, 2015),
    ("Vratsa", 2, 9, 2015),
    ("Vratsa", -11, 10, 2015),
    ("Vratsa", -19, 11, 2015),
    ("Burgas", -5, 0, 2016),
    ("Burgas", -10, 1, 2016),
    ("Burgas", -7, 2, 2016),
    ("Burgas", 6, 3, 2016),
    ("Burgas", -16, 4, 2016),
    ("Burgas", -1, 5, 2016),
    ("Burgas", -8, 6, 2016),
    ("Burgas", 19, 7, 2016),
    ("Burgas", -7, 8, 2016),
    ("Burgas", 4, 9, 2016),
    ("Burgas", 14, 10, 2016),
    ("Burgas", 10, 11, 2016),
    ("Varna", 5, 0, 2016),
    ("Varna", -11, 1, 2016),
    ("Varna", 6, 2, 2016),
    ("Varna", 13, 3, 2016),
    ("Varna", 14, 4, 2016),
    ("Varna", 2, 5, 2016),
    ("Varna", -12, 6, 2016),
    ("Varna", -2, 7, 2016),
    ("Varna", 7, 8, 2016),
    ("Varna", 5, 9, 2016),
    ("Varna", 18, 10, 2016),
    ("Varna", 11, 11, 2016),
    ("Plovdiv", -18, 0, 2016),
    ("Plovdiv", 5, 1, 2016),
    ("Plovdiv", 17, 2, 2016),
    ("Plovdiv", -6, 3, 2016),
    ("Plovdiv", 14, 4, 2016),
    ("Plovdiv", 4, 5, 2016),
    ("Plovdiv", -6, 6, 2016),
    ("Plovdiv", 14, 7, 2016),
    ("Plovdiv", -8, 8, 2016),
    ("Plovdiv", 19, 9, 2016),
    ("Plovdiv", 13, 10, 2016),
    ("Plovdiv", 10, 11, 2016),
    ("Sofia", -13, 0, 2016),
    ("Sofia", -3, 1, 2016),
    ("Sofia", 18, 2, 2016),
    ("Sofia", -3, 3, 2016),
    ("Sofia", -7, 4, 2016),
    ("Sofia", 10, 5, 2016),
    ("Sofia", -17, 6, 2016),
    ("Sofia", 8, 7, 2016),
    ("Sofia", 12, 8, 2016),
    ("Sofia", 5, 9, 2016),
    ("Sofia", 2, 10, 2016),
    ("Sofia", -20, 11, 2016),
    ("Vratsa", -7, 0, 2016),
    ("Vratsa", 7, 1, 2016),
    ("Vratsa", -18, 2, 2016),
    ("Vratsa", -1, 3, 2016),
    ("Vratsa", -13, 4, 2016),
    ("Vratsa", 5, 5, 2016),
    ("Vratsa", 3, 6, 2016),
    ("Vratsa", -16, 7, 2016),
    ("Vratsa", 3, 8, 2016),
    ("Vratsa", -8, 9, 2016),
    ("Vratsa", 1, 10, 2016),
    ("Vratsa", 1, 11, 2016),
    ("Burgas", -17, 0, 2017),
    ("Burgas", 12, 1, 2017),
    ("Burgas", 5, 2, 2017),
    ("Burgas", 13, 3, 2017),
    ("Burgas", -15, 4, 2017),
    ("Burgas", 11, 5, 2017),
    ("Burgas", -8, 6, 2017),
    ("Burgas", 2, 7, 2017),
    ("Burgas", 2, 8, 2017),
    ("Burgas", -14, 9, 2017),
    ("Burgas", 4, 10, 2017),
    ("Burgas", 0, 11, 2017),
    ("Varna", 7, 0, 2017),
    ("Varna", -20, 1, 2017),
    ("Varna", -7, 2, 2017),
    ("Varna", -6, 3, 2017),
    ("Varna", 10, 4, 2017),
    ("Varna", -10, 5, 2017),
    ("Varna", 15, 6, 2017),
    ("Varna", 13, 7, 2017),
    ("Varna", 11, 8, 2017),
    ("Varna", -20, 9, 2017),
    ("Varna", -4, 10, 2017),
    ("Varna", 7, 11, 2017),
    ("Plovdiv", -15, 0, 2017),
    ("Plovdiv", 10, 1, 2017),
    ("Plovdiv", 0, 2, 2017),
    ("Plovdiv", -6, 3, 2017),
    ("Plovdiv", -5, 4, 2017),
    ("Plovdiv", 4, 5, 2017),
    ("Plovdiv", -1, 6, 2017),
    ("Plovdiv", -15, 7, 2017),
    ("Plovdiv", -4, 8, 2017),
    ("Plovdiv", -6, 9, 2017),
    ("Plovdiv", -2, 10, 2017),
    ("Plovdiv", 2, 11, 2017),
    ("Sofia", -9, 0, 2017),
    ("Sofia", 6, 1, 2017),
    ("Sofia", 18, 2, 2017),
    ("Sofia", 13, 3, 2017),
    ("Sofia", -9, 4, 2017),
    ("Sofia", -16, 5, 2017),
    ("Sofia", -2, 6, 2017),
    ("Sofia", 4, 7, 2017),
    ("Sofia", 19, 8, 2017),
    ("Sofia", 12, 9, 2017),
    ("Sofia", -20, 10, 2017),
    ("Sofia", -13, 11, 2017),
    ("Vratsa", 0, 0, 2017),
    ("Vratsa", -14, 1, 2017),
    ("Vratsa", -10, 2, 2017),
    ("Vratsa", 17, 3, 2017),
    ("Vratsa", -10, 4, 2017),
    ("Vratsa", 15, 5, 2017),
    ("Vratsa", -4, 6, 2017),
    ("Vratsa", 7, 7, 2017),
    ("Vratsa", -17, 8, 2017),
    ("Vratsa", 18, 9, 2017),
    ("Vratsa", -16, 10, 2017),
    ("Vratsa", 9, 11, 2017),
    ("Burgas", -18, 0, 2018),
    ("Burgas", -14, 1, 2018),
    ("Burgas", -15, 2, 2018),
    ("Burgas", -13, 3, 2018),
    ("Burgas", -3, 4, 2018),
    ("Burgas", 14, 5, 2018),
    ("Burgas", 5, 6, 2018),
    ("Burgas", 7, 7, 2018),
    ("Burgas", 9, 8, 2018),
    ("Burgas", -12, 9, 2018),
    ("Burgas", 3, 10, 2018),
    ("Burgas", -20, 11, 2018),
    ("Varna", 2, 0, 2018),
    ("Varna", -18, 1, 2018),
    ("Varna", -17, 2, 2018),
    ("Varna", 16, 3, 2018),
    ("Varna", -3, 4, 2018),
    ("Varna", 1, 5, 2018),
    ("Varna", 15, 6, 2018),
    ("Varna", -15, 7, 2018),
    ("Varna", 4, 8, 2018),
    ("Varna", 0, 9, 2018),
    ("Varna", -18, 10, 2018),
    ("Varna", 16, 11, 2018),
    ("Plovdiv", 10, 0, 2018),
    ("Plovdiv", -14, 1, 2018),
    ("Plovdiv", -9, 2, 2018),
    ("Plovdiv", -9, 3, 2018),
    ("Plovdiv", -9, 4, 2018),
    ("Plovdiv", 7, 5, 2018),
    ("Plovdiv", 13, 6, 2018),
    ("Plovdiv", -20, 7, 2018),
    ("Plovdiv", 10, 8, 2018),
    ("Plovdiv", -16, 9, 2018),
    ("Plovdiv", 8, 10, 2018),
    ("Plovdiv", -17, 11, 2018),
    ("Sofia", 0, 0, 2018),
    ("Sofia", -9, 1, 2018),
    ("Sofia", 2, 2, 2018),
    ("Sofia", 9, 3, 2018),
    ("Sofia", 0, 4, 2018),
    ("Sofia", 13, 5, 2018),
    ("Sofia", 18, 6, 2018),
    ("Sofia", -2, 7, 2018),
    ("Sofia", -15, 8, 2018),
    ("Sofia", -13, 9, 2018),
    ("Sofia", 17, 10, 2018),
    ("Sofia", -20, 11, 2018),
    ("Vratsa", -2, 0, 2018),
    ("Vratsa", 19, 1, 2018),
    ("Vratsa", 3, 2, 2018),
    ("Vratsa", -20, 3, 2018),
    ("Vratsa", 17, 4, 2018),
    ("Vratsa", -16, 5, 2018),
    ("Vratsa", 17, 6, 2018),
    ("Vratsa", 8, 7, 2018),
    ("Vratsa", 13, 8, 2018),
    ("Vratsa", -13, 9, 2018),
    ("Vratsa", -19, 10, 2018),
    ("Vratsa", -10, 11, 2018),
    ("Burgas", 0, 0, 2019),
    ("Burgas", 8, 1, 2019),
    ("Burgas", 13, 2, 2019),
    ("Burgas", 4, 3, 2019),
    ("Burgas", -15, 4, 2019),
    ("Burgas", 0, 5, 2019),
    ("Burgas", 11, 6, 2019),
    ("Burgas", 4, 7, 2019),
    ("Burgas", 15, 8, 2019),
    ("Burgas", 16, 9, 2019),
    ("Burgas", 12, 10, 2019),
    ("Burgas", 10, 11, 2019),
    ("Varna", 3, 0, 2019),
    ("Varna", -15, 1, 2019),
    ("Varna", 16, 2, 2019),
    ("Varna", -4, 3, 2019),
    ("Varna", 6, 4, 2019),
    ("Varna", 17, 5, 2019),
    ("Varna", -14, 6, 2019),
    ("Varna", 17, 7, 2019),
    ("Varna", -3, 8, 2019),
    ("Varna", 11, 9, 2019),
    ("Varna", 9, 10, 2019),
    ("Varna", -19, 11, 2019),
    ("Plovdiv", -4, 0, 2019),
    ("Plovdiv", 2, 1, 2019),
    ("Plovdiv", -3, 2, 2019),
    ("Plovdiv", 12, 3, 2019),
    ("Plovdiv", 17, 4, 2019),
    ("Plovdiv", -4, 5, 2019),
    ("Plovdiv", 5, 6, 2019),
    ("Plovdiv", -19, 7, 2019),
    ("Plovdiv", 3, 8, 2019),
    ("Plovdiv", -11, 9, 2019),
    ("Plovdiv", 9, 10, 2019),
    ("Plovdiv", -15, 11, 2019),
    ("Sofia", 19, 0, 2019),
    ("Sofia", -8, 1, 2019),
    ("Sofia", -4, 2, 2019),
    ("Sofia", -5, 3, 2019),
    ("Sofia", -7, 4, 2019),
    ("Sofia", -18, 5, 2019),
    ("Sofia", 4, 6, 2019),
    ("Sofia", -6, 7, 2019),
    ("Sofia", -6, 8, 2019),
    ("Sofia", -8, 9, 2019),
    ("Sofia", -3, 10, 2019),
    ("Sofia", -11, 11, 2019),
    ("Vratsa", -8, 0, 2019),
    ("Vratsa", -17, 1, 2019),
    ("Vratsa", -13, 2, 2019),
    ("Vratsa", -17, 3, 2019),
    ("Vratsa", 11, 4, 2019),
    ("Vratsa", 15, 5, 2019),
    ("Vratsa", 7, 6, 2019),
    ("Vratsa", -12, 7, 2019),
    ("Vratsa", 15, 8, 2019),
    ("Vratsa", -2, 9, 2019),
    ("Vratsa", 3, 10, 2019),
    ("Vratsa", 2, 11, 2019),
    ("Burgas", 14, 0, 2020),
    ("Burgas", 11, 1, 2020),
    ("Burgas", 1, 2, 2020),
    ("Burgas", -5, 3, 2020),
    ("Burgas", -1, 4, 2020),
    ("Burgas", 5, 5, 2020),
    ("Burgas", 1, 6, 2020),
    ("Burgas", 10, 7, 2020),
    ("Burgas", 6, 8, 2020),
    ("Burgas", -6, 9, 2020),
    ("Burgas", -15, 10, 2020),
    ("Burgas", 3, 11, 2020),
    ("Varna", -10, 0, 2020),
    ("Varna", 18, 1, 2020),
    ("Varna", 19, 2, 2020),
    ("Varna", 1, 3, 2020),
    ("Varna", 13, 4, 2020),
    ("Varna", -6, 5, 2020),
    ("Varna", 17, 6, 2020),
    ("Varna", -6, 7, 2020),
    ("Varna", 16, 8, 2020),
    ("Varna", 13, 9, 2020),
    ("Varna", -4, 10, 2020),
    ("Varna", 5, 11, 2020),
    ("Plovdiv", -18, 0, 2020),
    ("Plovdiv", 8, 1, 2020),
    ("Plovdiv", -19, 2, 2020),
    ("Plovdiv", 0, 3, 2020),
    ("Plovdiv", -7, 4, 2020),
    ("Plovdiv", -19, 5, 2020),
    ("Plovdiv", -5, 6, 2020),
    ("Plovdiv", -5, 7, 2020),
    ("Plovdiv", 2, 8, 2020),
    ("Plovdiv", 2, 9, 2020),
    ("Plovdiv", -9, 10, 2020),
    ("Plovdiv", 17, 11, 2020),
    ("Sofia", 4, 0, 2020),
    ("Sofia", 18, 1, 2020),
    ("Sofia", 12, 2, 2020),
    ("Sofia", -17, 3, 2020),
    ("Sofia", -4, 4, 2020),
    ("Sofia", 13, 5, 2020),
    ("Sofia", -20, 6, 2020),
    ("Sofia", 13, 7, 2020),
    ("Sofia", 15, 8, 2020),
    ("Sofia", 12, 9, 2020),
    ("Sofia", -2, 10, 2020),
    ("Sofia", -10, 11, 2020),
    ("Vratsa", -3, 0, 2020),
    ("Vratsa", -8, 1, 2020),
    ("Vratsa", 12, 2, 2020),
    ("Vratsa", -5, 3, 2020),
    ("Vratsa", -11, 4, 2020),
    ("Vratsa", 11, 5, 2020),
    ("Vratsa", -20, 6, 2020),
    ("Vratsa", -12, 7, 2020),
    ("Vratsa", -15, 8, 2020),
    ("Vratsa", -10, 9, 2020),
    ("Vratsa", -3, 10, 2020),
    ("Vratsa", 9, 11, 2020),
    ("Burgas", 10, 0, 2021),
    ("Burgas", 5, 1, 2021),
    ("Burgas", 13, 2, 2021),
    ("Burgas", 14, 3, 2021),
    ("Burgas", -9, 4, 2021),
    ("Burgas", 0, 5, 2021),
    ("Burgas", -16, 6, 2021),
    ("Burgas", -2, 7, 2021),
    ("Burgas", -5, 8, 2021),
    ("Burgas", 19, 9, 2021),
    ("Burgas", 12, 10, 2021),
    ("Burgas", -15, 11, 2021),
    ("Varna", 4, 0, 2021),
    ("Varna", 1, 1, 2021),
    ("Varna", 3, 2, 2021),
    ("Varna", 9, 3, 2021),
    ("Varna", -5, 4, 2021),
    ("Varna", -12, 5, 2021),
    ("Varna", -8, 6, 2021),
    ("Varna", 17, 7, 2021),
    ("Varna", 9, 8, 2021),
    ("Varna", 16, 9, 2021),
    ("Varna", -19, 10, 2021),
    ("Varna", 7, 11, 2021),
    ("Plovdiv", 7, 0, 2021),
    ("Plovdiv", 1, 1, 2021),
    ("Plovdiv", -11, 2, 2021),
    ("Plovdiv", -20, 3, 2021),
    ("Plovdiv", -7, 4, 2021),
    ("Plovdiv", -6, 5, 2021),
    ("Plovdiv", -19, 6, 2021),
    ("Plovdiv", 17, 7, 2021),
    ("Plovdiv", -5, 8, 2021),
    ("Plovdiv", -17, 9, 2021),
    ("Plovdiv", -7, 10, 2021),
    ("Plovdiv", 12, 11, 2021),
    ("Sofia", 4, 0, 2021),
    ("Sofia", -19, 1, 2021),
    ("Sofia", 12, 2, 2021),
    ("Sofia", 19, 3, 2021),
    ("Sofia", 10, 4, 2021),
    ("Sofia", 8, 5, 2021),
    ("Sofia", 7, 6, 2021),
    ("Sofia", -16, 7, 2021),
    ("Sofia", 15, 8, 2021),
    ("Sofia", 6, 9, 2021),
    ("Sofia", -2, 10, 2021),
    ("Sofia", -10, 11, 2021),
    ("Vratsa", -13, 0, 2021),
    ("Vratsa", -13, 1, 2021),
    ("Vratsa", -16, 2, 2021),
    ("Vratsa", 3, 3, 2021),
    ("Vratsa", 1, 4, 2021),
    ("Vratsa", 3, 5, 2021),
    ("Vratsa", 6, 6, 2021),
    ("Vratsa", -17, 7, 2021),
    ("Vratsa", -10, 8, 2021),
    ("Vratsa", -9, 9, 2021),
    ("Vratsa", -11, 10, 2021),
    ("Vratsa", -20, 11, 2021)]
