This folder contains metrics for the Zone Activity zones within the named analysis.

The trips that are analyzed for each zone are determined by the pass-through designation for the zone. For zones with Is Pass-Through = "Yes", the analysis is done for all trips that pass-through the zone. For zones with Is Pass-Through = "No", there are two separate analyses done - one for trips that start in the zone and another for trips that end in the zone.


TERMS
=====
Pass-Through: A zone setting indicating how to analyze how trips interact with the zone. Zones marked as pass-through use trips that pass through the zone but do not start or stop in it. Zones not marked as pass-through use trips that start or stop in the zone.

Zone Direction: A pass-through zone can have applied direction which limits the trips analyzed for the zone: only trips that pass through the zone within -20/+20 degrees of the direction will be analyzed for the zone. Values are provided in degrees from 0 to 359, where 0 is due north, 90 is east, 180 is due south, etc. A value of "Null" refers to zones with no applied direction therefore all trips that pass through the zone will be used.

Vehicle Weight: The weight of the vehicles analyzed. Weight values can either be “Medium” or “Heavy”. This column is present only if the commercial analysis is segmented by weight class.


OUTPUT UNIT TERMS
=================
StreetLight Volume: The estimated trip counts as calculated by StreetLight Data's machine learning algorithm.

StreetLight Index: The relative trip activity. The StreetLight Index does not indicate the actual number of trips or vehicles. Trip Index values for different modes of travel (All Vehicles, Trucks, Bicycle, Pedestrian, etc.)  weight classes (such as Trucks Heavy-Duty/Medium-Duty), and countries are based on different sample populations and therefore cannot be compared with each other.

StreetLight Calibrated Index: The estimated number of trips or vehicles derived from StreetLight Index calibrated with StreetLight AADT or user-input counts.

StreetLight Sample Trip Counts: StreetLight sample trip counts for the zone (or set of zones) for all days in the entire data period.

*Note that, while most output units are represented as an average day per its day type definition, Trip Counts are not converted to an average day. For example, a Trip Count value of 100 for O-D pair A and B for average weekday in March 2017 means that the sum of all trips used from StreetLight data set from all the weekdays in March 2017 is 100.

*More information of the output unit methodology can be found in StreetLight Data's Support Center (https://support.streetlightdata.com).


FILES
=====
Analysis.txt
===========
This file lists information about the analysis as a whole, including the full analysis name, organization and user that created the analysis, and the analysis options configuration.

za_*.csv
========
These files contains the ZA metrics.

The fields are:
- Data Periods: Ranges of dates analyzed.
- Vehicle Weight: (with Truck) Commercial vehicle weight class (Medium Duty: 14,001 - 26,000 lbs, Heavy Duty: 26,001+ lbs).
- Mode of Travel: Mode of travel analyzed.
- Intersection Type: Type of trips analyzed--dependent on whether the zone Is Pass-Through. If Zone Is Pass-Through is "Yes", then Intersection Type = "Trip Pass-Through". If Zone Is Pass-Through is "No", then there is a row each for Intersection Type = "Trip Start" and "Trip End".
- Zone ID: Numeric ID for the zone as provided by the user.
- Zone Name: Name for the zone.
- Zone Is Pass-Through: Indicates if the zone is pass-through or not as described above in the Terms.
- Zone Direction (degrees): This refers to the direction in which trips pass through the zone as described above in the Terms.
- Zone is Bi-Direction: Indicates if the zones are bi-directional. Values are "Yes" or "No".
- Day Type: All Days (traffic from Monday through Sunday), and other day type traffic segmentation as defined by user.
- Day Part: Segments of the day defined by the user in intervals of hours to analyze traffic (All Day is always included as entire 24 hours).
- Zone Traffic: The volume of trips starting in, passing through, or ending in the zone based on the zone Mode of Travel, Intersection Type, and Output Type (as described above in the Output Unit Terms).
- Weight Class Percentage (percent): (with Truck) Percent of vehicle volume for a vehicle weight type, divided by all vehicle volume of zone.
- Avg Travel Time (sec): The average total trip time in seconds, for low network factor* trips.
- Avg All Travel Time (sec): The average total trip time in seconds, for all trips.
- Avg Trip Length (mi or km): The average total trip length in miles, for low network factor* trips.
- Avg All Trip Length (mi or km): The average total trip length in miles, for all trips.

*Network factor is defined as unlocked trip length / distance (trip point in origin zone, trip point in destination zone). Low network is less than 4.

sample_size.csv
===============
This file contains information about the size of the data sample that was analyzed for this analysis and its data period.

The fields are:
- Data Periods: Ranges of dates analyzed.
- Mode of Travel: Mode of travel analyzed.
- Vehicle Weight: Type of commercial vehicle analyzed.
- Approximate Device Count: Calculated as the number of unique devices in the analysis, rounded to nearest 10 (if below 100), and to nearest 100 (if above 100). N/A for analysis run with Navigaton-GPS data source.
- Approximate Trip Count: An estimated value (calculated during processing) of the number of unique device trips in the StreetLight Data database that were analyzed for this analysis and its Data Period.

zones.csv
=========
This file contains information about the zones used in this analysis.

The fields are:
- Data Periods: Ranges of dates analyzed.
- Zone Type: Indicates if the zone for this is an origin or destination zone.
- Zone ID: Numeric ID for the zone as provided by the user.
- Zone Name: Name for the zone.
- Zone is Pass-Through: Indicates if the zone is marked as pass-through or not as described above under Terms. Is either makred as “Yes” or “No."
- Zone Direction (degrees): This refers to the direction in which trips pass through the zone as described above under Terms.
- Zone is Bi-Direction: Indicates if the zones are bi-directional. Is either marked as "Yes" or "No".
- Fingerprint1: A 64-bit signed integer assigned by StreetLight based on key spatial characteristics of the zone. Combination of Fingerprint1 and Fingerprint2 make up the fingerprint of the zone and indicate if two zones are the same or unique.
- Fingerprint2: A 64-bit signed integer assigned by StreetLight based on key spatial characteristics of the zone. Combination of Fingerprint1 and Fingerprint2 make up the fingerprint of the zone and indicate if two zones are the same or unique.

*.(dbf|prj|shp|shx|cpg)
=======================
These files comprise the shapefiles for the analysis's zone sets.

A shapefile consists of the following several files:
.shp file contains the feature geometries and can be viewed in a geographic information systems application such as QGIS.
.dbf file contains the attributes in dBase format and can be opened in Microsoft Excel.
.shx file contains the data index.
.prj file contains the projection information.
.cpg file contains the encoding applied to create the shapefile.

These shapefiles have the following attributes/columns:
- id: Numeric ID for the zone as provided by the user. This may be null as the field is optional.
- name: Name for the zone.*
- direction (degrees): This refers to the direction in which trips pass through the zone as described above under Terms.
- is_pass: Indicates if the zone is pass-through or not as described above under Terms. 1 = “Yes” and 0 = “No”.
- geom: Polygon of the zone.
- is_bidi: A value of 1 indicates traffic measured in two opposite directions for a single set of Metric values. 0 value indicates traffic is in single direction specified by users.

*_line.(dbf|prj|shp|shx|cpg)
============================
These shapefiles have the following attributes/columns:
- id: Numeric ID for the zone as provided by the user. This may be null as the field is optional.
- name: Name for the zone.*
- geom: LineString of the zone.
- road_type: Type of the road that is represented by the LineString, as entered upon creation of the zone.
- gate_lat: The latitude for the location of the gate on the line segment, as entered upon creation of the zone.
- gate_lon: The longitude for the location of the gate on the line segment, as entered upon creation of the zone.
- gate_width: The total width of the gate, in meters, as entered upon creation of the zone.

* If the zone_name format is [OSM Name] / [OSM ID] / #### (for example: 'Market Street / 9570488 / 1'), the zone file is an OSM Derivative Database, and subject to OSM terms. Refer to https://www.streetlightdata.com/open-source/ for more information.


NOTES
=====
ZA with No Values
=================
If the output unit values for a zone for a specific time period (e.g. Average Weekday, Early AM) are below StreetLight's significance threshold, no results will be shown in the za_*.csv file.

Day Part Calculations
=====================
The Day Part calculations are done in relation to the zones used in the analysis. The Day Part is determined by when trips either Start in the zone or pass by the centroid of the zone, if the zone is designated as pass-through.

Trip Type
=========
The file Analysis.txt specifies the type of trips used in the analysis: Locked to Route Trips or Unlocked Trips. Unlocked Trips may not consistently align with roads depending upon the device ping rate, the speed of the vehicle, and how curvy the roads are. Locked to Route Trips address this by aligning to the road segments of the most likely path taken for the set of points that comprise the Unlocked Trip.


Copyright © 2011 - 2023, StreetLight Data, Inc. All rights reserved.
