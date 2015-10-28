Introduction
============

These GIS shapefiles were generated from raw polling booth point
localities published by the Election Commission of India using an
automated algorithm described more fully below. Their accuracy cannot
be guaranteed and varies from district to district; please check
the comments below and the files themselves thoroughly before use. 

The shapefiles are published under the Creative Commons Attribution
NonCommercial ShareAlike 4.0 International License, a copy of which
can be found in the attached LICENSE.txt, or else be downloaded from
http://creativecommons.org/licenses/by-nc-sa/4.0/, or be obtained by
sending a letter to Creative Commons, 444 Castro Street, Suite 900, 
Mountain View, California, 94041, USA. Please attribute to:

  Susewind, Raphael (2014). GIS shapefiles for India's parliamentary
  and assembly constituencies including polling booth localities.
  Published under a CC-BY-NC-SA 4.0 license. Available from 
  http://dx.doi.org/10.4119/unibi/2674065

A draft version of these shapefiles had earlier been published on
http://www.raphael-susewind.de/blog/2014/mapping-indias-election; I am
thankful to all commenters for their suggestions and improvements.

Likewise, I thank the members of the GIS Stackexchange community for
their initial assistance, in particular in reply to this post:
https://gis.stackexchange.com/questions/89067

Finally, the files make use of district and state boundaries from the
GADM dataset (http://gadm.org, version 1.0, 2009), the Indian part of
which was published under a Creative Commons Attribution Noncommercial 
Share Alike 3.0 United States License. I am thankful to them, too.


Raw data sources
================

The shapefiles were created using raw data originally extracted from
http://www.eci-polldaymonitoring.nic.in/psleci/default.aspx in January
2014 (published under no apparent license restrictions); these are 
reproduced in the *.boothraw.shp layers. All ID codes mirror those
used during the 2014 general elections, and might be incompatible with 
earlier as well as later codes. 

Since polling booth locality data originally contained only assembly
but not parliamentary constituency information, the latter was added
from http://eci.nic.in/eci_main/CurrentElections/CONSOLIDATED_ORDER%20_ECI%20.pdf


Algorithm
=========

This raw data was processed in multiple steps (each state or union
territory separately), to arrive at constituency polygons:

1) Using the district code given for each polling station, polling
stations that were clearly out of place (GPS errors, typos, etc) were
discarded by matching them against the outer extent of GADM's
district boundary shapefiles (with some manual adjustment for cases of
re-districting). This is by the way one of the reasons why these
shapefiles are released under a non-commercial license - they are
'tainted' by GADM material. The resulting cleaned up point cloud can
be found in the *.booth.shp layers.

2) Heatmaps were calculated for each assembly constituency point
cloud, and subsequently compared with each other to determine the
'hottest' constituency for each coordinate across a given state or union
territory. This already rendered fairly accurate boundary lines along
those coordinates where the heatmap of one constituency fades
sufficiently to be overpowered by that of an adjacent constituency. 

3) This was followed by a series of further cleanup steps, making sure
that small 'islands' were deleted, that each assembly constituency was
represented by only one contingent area polygon, etc. The last of
these steps was to cut the result with GADM's state boundaries. The
resulting polygons can be found in the *.assembly.shp layers.

4) Finally, the assembly polygons were dissolved by parliamentary
constituency codes to derive the *.parliamentary.shp layers.


Accuracy
========

A key advantage of this algorithm-driven method over manual geocoding
is speed, but this comes at the disadvantage of reduced accuracy. More 
specifically, two sources of inaccuracy have to be kept in mind. 

Firstly, the quality of constituency polygons depends largely on the 
quality of the Election Commission's raw point data, which varies from 
district to district (the level on which these data were collected). 

Secondly, the heatmap approach itself tends to create 'smoother'
boundaries, particularly where there are very few polling stations (in
really remote areas such as northern Sikkim) or many points in one
place (in crowded urban areas such as Bangalore). 

Additional auto-generated caveats for specific states might be
available in state-wise README.txt files. Improvements to these
shapefiles are of course welcome; given the CC-BY-NC-SA share alike clause, 
these have to be shared with the general public on equal terms.


Contact
=======

Raphael Susewind, mail@raphael-susewind.de, GPG ID 0xA5ED49AE
