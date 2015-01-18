# HDF5 example.
library(rhdf5)
# h5ls shows the structure of the data.
h5ls(file = "acos_L2s_120924_40_Production_v150151_L2s20900_r01_PolB_121005123556.h5")
# Read a particular group.
hdf5data <- h5read("acos_L2s_120924_40_Production_v150151_L2s20900_r01_PolB_121005123556.h5", "Metadata/AerosolTypes")
str(hdf5data)
class(hdf5data)
hdf5data2 <- h5read("acos_L2s_120924_40_Production_v150151_L2s20900_r01_PolB_121005123556.h5", "RetrievalResults/vector_pressure_levels")
dim(hdf5data2)
class(hdf5data2)
str(hdf5data2)
