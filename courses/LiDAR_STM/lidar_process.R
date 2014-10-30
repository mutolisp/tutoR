Sys.setenv(HADOOP_CMD="/usr/local/bin/hadoop")

library(rhdfs)
library(rmr2)
hdfs.init();

#hdfs.ls("/")
# import data into hdfs via hdfs.file()
HDFS_r = hdfs.file("ST000.xyz", "r",buffersize = 104857600)
HDFS_f = hdfs.read(HDFS_r)
data = read.csv(textConnection(rawToChar(HDFS_f)), sep=" ", header=F)
# generate test dataset
data_seq = 1:dim(data)[1]

# res = resolution (cm)
# bound = outer boundary, default is 30 m
proc_dtm = function(dataframe, res) {
  r_cm = 100
  trunc = cbind(trunc(dataframe[,1:2]*r_cm/res)*res, dataframe[,3])
  row.names(trunc) = 1:dim(trunc)[1]
  colnames(trunc) = c("x", "y", "z")
  aout = aggregate(trunc["z"], by=trunc[c("x","y")], FUN=min)
  #output = aout[(aout[,1]/100<bound & aout[,2]/100<bound),]
  return(aout)
}
output.xyz = proc_dtm(data, 10)

# limit the target area to 30 by 30 meters
bound=15
output_cm.xyz = output.xyz[(abs(output.xyz[,1]/100) < bound & abs(output.xyz[,2]/100) < bound),]
output_m.xyz = cbind(output_cm.xyz[,1:2]/100, output_cm.xyz[,3])
# use raster library to output raster file
library(raster)
rast = rasterFromXYZ(output_m.xyz)
raster::writeRaster(rast, filename = 'output.tif', overwrite=TRUE)

# test for processing time
time.measure = rep(NA,6)
for ( i in 1:6 ) {
  print(10^i)
  testdata = data[1:10^i,1:3]
  init_time = proc.time()
  output_xyz = proc_dtm(testdata, 1)
  proc_time = proc.time() - init_time
  time.measure[i] = proc_time
}


