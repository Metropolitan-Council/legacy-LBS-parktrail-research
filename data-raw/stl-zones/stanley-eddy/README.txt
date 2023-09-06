This folder contains the shapefile for a Zone Set containing Polygons.

A shapefile consists of the following several files:
.shp file contains the feature geometries and can be viewed in a geographic information systems application such as QGIS.
.dbf file contains the attributes in dBase format and can be opened in Microsoft Excel.
.shx file contains the data index.
.prj file contains the projection information.


The shapefile has the following attributes/columns:

* id            ID for the Zone as entered upon creation of Zone Set. This may be null as the field is optional.

* name          Name for the Zone.

* is_pass       Value = 1 or 0.
                Value = 1 indicates trips are expected to pass through the Zone (typically, for a Zone that is a road segment).
                Value = 0 indicates trips are expected to start or end in the Zone.

* direction     Direction of travel in degrees where the trip passes through the Zone.
                Value is >= 0 and < 360. For example, north is 0, east is 90, south-west is 225.
                If the value is null, then trips in all directions at the Zone are analyzed.

* geom          Polygon for the Zone, in SRID 4326.

* is_bidi  	Value = 1 or 0.
		Value = 1 indicates traffic is in two opposite directions in a single set of Metric values.
		Value = 0 indicates traffic is in single direction specified by users.


Copyright © 2011 - 2022, StreetLight Data, Inc. All rights reserved.
