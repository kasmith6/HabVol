# HabVol
Habitat Volume Model

Estimating habitat volume of living resources using three-dimensional circulation and biogeochemical models

Katharine A. Smith, Zachary Schlag, Elizabeth W. North
Computers and Geosciences, 2018

HabVol Model Description
-----------------------------------------------------------------------------------------------------
The Habitat Volume Model (HabVol) calculates a geometric volume of suitable habitat for a specific organism and life stage (e.g., adult striped bass, juvenile soft clam) as it changes over time. It can be run using fixed criteria or bioenergetics calculations. When run with fixed criteria, physiological tolerances of the organisms are used to calculate the volume of suitable habitat in terms of salinity, temperature and dissolved oxygen, as well as their intersection. When run in bioenergetics mode, the volume of habitat is calculated that corresponds to a user-defined threshold in potential growth.

HabVol is an open-source offline model written in Fortran and is based on an algorithm that calculates the volume of habitat given one or more constraints (Smith et al. 2018). HabVol uses output from a coupled 3D hydrodynamic model, the Regional Ocean Modeling System (ROMS), and a dissolved oxygen model implemented within ROMS (Li 2012) to calculate the volume of suitable habitat in each model grid cell. The HabVol model grid cells are the same as the ROMS model grid cells.For more information on the application of HabVol, please see Schlenger et al. (2013) and Schlenger (2012). Please cite Smith et al. (2018) when referring to HabVol. 

Zachary Schlag is the developer of HabVol with input from Katharine Smith, Adam Schlenger, and Elizabeth North. Katharine Smith is the developer of the algorithm for calculating volume from ROMS grid cells (Smith et al. 2009). Adam Schlenger helped adapt and test the bioenergetics models for use in HabVol. HabVol was developed with support from National Oceanic and Atmospheric Administration (NOAA) Coastal Hypoxia Research Program (NA07NOS4780191). 


HabVol Code
---------------------------------------------------------------------------------------------------------
HabVol Open Source Code

-
HabVol_License.txt

License file. This license was based on the ROMS license. Please note that this license applies to all sections of LTRANS v.2b except those listed in the 'External Dependencies and Programs' section below. 

-
/HabVol

HabVol Code. This folder contains the HabVol code and license. Before using LTRANS v.2b, please read the External Dependencies and Programs section below. This version of LTRANS is parameterized to run with the input files that are available in the HabVol Example Input Files section below, which also contains a tar ball with this code and the example input files.

-
HabVol_UsersGuide_17June13.pdf

This is the latest version of the HabVol User's Guide.


-
External Dependencies and Programs

HabVol requires NetCDF libraries. Because HabVol reads in ROMS-generated NetCDF (.nc) files, it requires that the appropriate NetCDF libraries be installed on your computer (see files and links below).

-
VF-NetCDF.zip
http://northweb.hpl.umces.edu/LTRANS/LTRANS-v2/VF-NetCDF.zip

Windows Visual Fortran NetCDF libraries. These NetCDF files that are compatible with Visual Fortran were downloaded from the Unidata NetCDF Binaries Website for LTRANS v.1. The NetCDF 90 files were downloaded from Building the F90 API for Windows for the Intel ifort compiler website. The VF-NetCDF.zip folder contains README.txt that describes where to place the enclosed files. If these files do not work, you may have to download updated versions or build your own by following the instructions at the UCAR Unidata NetCDF website. 

-
NetCDF website
http://www.unidata.ucar.edu/software/netcdf/

Linux NetCDF libraries. Linux users will likely have to build their own Fortran 90 libraries using the source code/binaries that are available on the UCAR Unidata NetCDF website. 


-
HabVol Example Input Files

These files can be used to test HabVol. They include example ROMS grid and history files (.nc) that are needed to run HabVol. Many thanks to Yun Lifor sharing the ROMS .nc files. TheHabVol code above is configured to run with these input files. Note: please download the tar (HabVol.tgz) and history file between the hours of 5 pm and 6 am Eastern Standard Time because of their large size.

-
CBP_GRID_wUV.nc
http://northweb.hpl.umces.edu/open_source_code/HabVol/Input/CPB_GRID_wUV.nc

ROMS grid file (for HabVol input). File is from the Chesapeake Bay ROMS model with simplified biogeochemistry (implemented by Yun Li and Ming Li). 

-
ly04_0351.nc
http://northweb.hpl.umces.edu/open_source_code/HabVol/Input/ly04_0351.nc

ROMS predictions (history) file (for HabVol input). File is from the Chesapeake Bay ROMS model with simplified biogeochemistry (implemented by Yun Li and Ming Li). 

-
HabitatVolumeBoundary.csv	
http://northweb.hpl.umces.edu/open_source_code/HabVol/Input/HabitatVolumeBoundary.csv

Input file that contains the latitude and longitude coordinates that specify the boundaries of the Habitat Volume Model (if they are a subset of the ROMS model domain).

-
STD_Volumes.csv
http://northweb.hpl.umces.edu/open_source_code/HabVol/Output/STD_Volumes.csv
StTp_Volumes.csv
http://northweb.hpl.umces.edu/open_source_code/HabVol/Output/StTp_Volumes.csv
output_variable_definitions.txt
http://northweb.hpl.umces.edu/open_source_code/HabVol/Output/output_variable_definitions.txt

HabVol output files for this test case (.csv) and a description of the variables that appear in them (.txt).

-
HabVol.tgz
http://northweb.hpl.umces.edu/open_source_code/HabVol/HabVol.tgz

This tar ball (0.5 GB) contains the LTRANS code as well as all example input files and the User's Guide. Use the included makefile to compile and then run LTRANS.exe on your Linux machine. DOWNLOAD AFTER WORKING HOURS (Eastern Standard Time) PLEASE!!



HabVol User's Guide and Bug Reoports 
-----------------------------------------------------------------------------------
Please send comments or suggestions regarding the User's Guide and Bug Reports to enorth@umces.edu. 


HabVol_UsersGuide_17June13.pdf

Schlenger, A. J., Z. Schlag, and E. W. North. 2013. Habitat Volume Model (HabVol) User’s Guide. University of Maryland Center for Environmental Science, Horn Point Laboratory. Cambridge, MD. 14 pp. (.pdf)


Bug Reports and Fixes:

Report No. 1.
Bug fix for special case (edge of habitat) calculations, by Katharine Smith, 3/31,2018

Report No. 2. 
Bug fix for special case (edge of habitat) calculations, "pie problem", by Katharine Smith, 3/31,2018

 
 
Other HabVol Publications
-------------------------------------------------------------------------------------------------------------------

Schlenger, A. J. , E. W. North, Z. Schlag, Y. Li, D. H. Secor, K. A. Smith, and E. J. Niklitschek. 2013. Modeling the influence of hypoxia on the potential habitat of Atlantic sturgeon (Acipenser oxyrinchus): a comparison of two methods. Marine Ecology Progress Series 483:257-272. To request a .pdf, please contact aschleng@umces.edu.

Smith, K.A., E. W. North, and D. H. Secor. 2009. Estimating habitat volume based on physical and biogeochemical models. ICES CM/K:09. To request a .pdf, please contact enorth@umces.edu.
