#!/bin/sh

/bin/rm gdal

ldc2 -v -L-lstdc++ -cpp-args -I/usr/include gdal.d -L-lgdal 

/bin/rm *.o
/bin/rm calypso*
