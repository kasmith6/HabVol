#!/bin/sh
#
# LTRANS compile script
#
echo "Compiling Modules:"
echo "  Parameter"
ifort -vec-report0 -c Parameter_Module.f90
echo "  Point-in-Polygon"
ifort -vec-report0 -c point_in_polygon_module.f90
echo "  Hydro Data"
ifort -vec-report0 -c Hydro_Data_Module.f90
echo "  Constraints"
ifort -vec-report0 -c Constraints_Module.f90
echo "  Hydrodynamic"
ifort -vec-report0 -c -I/usr/local/include Hydrodynamic_Module.f90 
echo "  Volume"
ifort -vec-report0 -c Volume_Module.f90
echo "  "
echo "Compiling Habitat Volume Model"
ifort -vec-report0 -o Volume.exe HabitatVolumeModel.f90 Parameter_Module.o point_in_polygon_module.o Hydro_Data_Module.o Constraints_Module.o Hydrodynamic_Module.o Volume_Module.o -L/usr/local/lib -lnetcdf -lhdf5_hl -lhdf5 -lz -lm -lcurl
echo "  "
echo "Compilation Complete"
rm *.mod
rm *.o