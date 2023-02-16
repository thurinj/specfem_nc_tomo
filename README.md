# specfem_nc_tomo

Before running the compilation, please download the following model from Iris:
```
wget -O model.nc https://ds.iris.edu/files/products/emc/emc-files/SCEC-CVM-H-v15-1-n4.nc
```
(should be working with any model though, but we need to be able to work with any regional model, as they will be a challenge).

The MAJOR problem to deal with, is to find a way to fill the gaps in the regional model, as all the models already pre-implemented are full (no gaps, no NaN). They are either 1D projected onto all lat-lon-depths, or full-global models (potentially curated to be included in SPECFEM3D_GLOBE).
A solution would be to convert any model to be a perturbation of one of the reference model implemented in Globe, so that we would be able to superimpose the NetCDF model as a perturbation to the reference model, without having to deal with holes and gap.

The code is a bare prototype that uses NetCDF Fortran and does a basic reading operation on the model. 
