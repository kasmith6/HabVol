!--these are the grid dimension definitions, their values will be found in the NetCDF Grid File

  INTEGER :: ui                     ! number u points in x direction
  INTEGER :: uj                     !        u           y 
  INTEGER :: vi                     !        v           x
  INTEGER :: vj                     !        v           y
  INTEGER :: us                     ! number of s-levels

  INTEGER :: rho_nodes              ! number rho nodes
  INTEGER :: rho_elements           ! number of rho elements
  INTEGER :: wet_elements           ! number of rho elements with at least 1 vertex as water

!group the grid info section in a namelist:

  namelist/gridinfo/ ui, uj, vi, vj, us ,rho_nodes, rho_elements, wet_elements
