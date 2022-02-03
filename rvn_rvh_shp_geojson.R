#' @title Converts a basin shapefile into a geojson 
#'
#' @description
#' This function compares a basin shapefile with it's Raven compliant rvh file. The function copies a 
#' number of rvh file columns to the shapefile attribute table based on a provided argument, \code{matchingColumns}. 
#'
#' @param shpfile the full path for the shapefile
#' @param rvhfile the full path for the rvh file
#' @param outputfile the full path for the geojson file to be created
#' @param CRSshp (optional) coordinate system of \code{shpfile} if missing. Throws error if the \code{shpfile} coordinate system is missing and \code{CRSshp} has been left with default
#' @param matchingColumns a list of matching columns in \code{shpfile} and \code{rvhfile}. The provided sublists must be labeled as \code{list(shpfile="",rvhfile="")}
#' @param outletCoords (optional) a list of longitude and latitude of the subbasin outlet. Must be provided in the GCS format and have to be labeld with the following format: \code{list(outletLat=NA,outletLng=NA)}
#' @param simplifyGeometry (optional) Logical: to simplify the polygons in the geojson file or not. Default to \code{TRUE}
#'
#' @returns a geojson file
#'
#' @seealso \code{\link{rvn_rvh_read}} to read rvh file
#'
#' @examples
#'
#' data_url<-"https://github.com/rarabzad/shp2geojson/raw/main/test%20cases.zip"
#' download.file(data_url,destfile = "test cases.zip")
#' unzip(zipfile="test cases.zip",exdir = "test cases")
#' shpfile<-"./test cases/Liard/subbasin_20180718.shp"
#' rvhfile<-"./test cases/Liard/Liard.rvh"
#' matchingColumns<-list(shpfile="Sub_B",rvhfile="Name")
#' rvn_rvh_shp_geojson(shpfile=shpfile,rvhfile=rvhfile,
#'   				   matchingColumns=matchingColumns)
rvn_rvh_shp_geojson<-function(shpfile,
                              rvhfile,
                              outputfile=sprintf("%s/output.json",getwd()),
                              CRSshp=NA,
			      matchingColumns=list(shpfile="subid",rvhfile="SBID"),
			      outletCoords=list(outletLat=NA,outletLng=NA),
			      simplifyGeometry=TRUE)
{
   # loading libraries
   suppressPackageStartupMessages(library(geojsonio))
   suppressPackageStartupMessages(library(RavenR))
   suppressPackageStartupMessages(library(raster))
   suppressPackageStartupMessages(library(stringdist))
   suppressPackageStartupMessages(library(rmapshaper))
   # checking missing arguments
   if(any(c(missing(shpfile),missing(rvhfile))))
   {
      stop ("any ofthe followings: [shpfile, rvhfile] are missing with no default!")
   }
   if(!file.exists(shpfile)) stop("shpe file doesn't exist!")
   if(!file.exists(rvhfile)) stop("rvh file doesn't exist!")
   if(!dir.exists(gsub(basename(outputfile),"",outputfile))) stop("output directory doesn't exist!")

   # reading shp file and its projection
   basins <- shapefile(shpfile)
   if(is.na(crs(basins)@projargs) & is.na(CRSshp)) stop("shapefile's CRS is unknown while CRSshp is missing")
   if(is.na(crs(basins)@projargs)) crs(basins)<-CRSshp
   WGS<-crs("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84")
   basins<-spTransform(basins,CRSobj=WGS)

   # read rvh file 
   rvh<-rvn_rvh_read(rvhfile)
   rvhColumns<-list(SubId="SBID",DowSubId="Downstream_ID",rvhName="Name",BasArea="Area")
   SubId<-rvhColumns$SubId
   DowSubId<-rvhColumns$DowSubId
   rvhName<-rvhColumns$rvhName
   BasArea<-rvhColumns$BasArea

   # matching a column from rvh file with a column from shp file
   id<-match(basins@data[,matchingColumns$shpfile],rvh$SBtable[,matchingColumns$rvhfile])

   # preserving non matched subbasins IDs
   naIds<-is.na(id)
   id[naIds]<-1

   # finding and removing duplicates columns, if there are/is any
   id1<-amatch("basinarea",colnames(basins@data)      ,maxDist=10)[1]
   id2<-amatch("subbasinid",colnames(basins@data)     ,maxDist=10)[1]
   id3<-amatch("downstreamid",colnames(basins@data)   ,maxDist=10)[1]
   redundants<-c(id1,id2,id3)
   redundants<-redundants[!is.na(redundants)]
   basins@data<-basins@data[,-redundants]

   # copying matched subbasins charactristics from rvh file to shp file
   basins@data$rvhName <-rvh$SBtable[id,rvhName]     ; if(any(naIds)) basins@data$rvhName[naIds] <--9999
   basins@data$SubId   <-rvh$SBtable[id,SubId]       ; if(any(naIds)) basins@data$SubId[naIds]   <--9999
   basins@data$DowSubId<-rvh$SBtable[id,DowSubId]    ; if(any(naIds)) basins@data$DowSubId[naIds]<--9999
   basins@data$BasArea <-rvh$SBtable[id,BasArea]*10^6; if(any(naIds)) basins@data$BasArea[naIds] <--9999

   outletId<-rvh[[1]][which(rvh[[1]][,DowSubId]=="-1"),SubId]
   if(length(outletId)>1)
   {
      warning("the provided rvh file contains a watershed with more than one outlets. The first matching one is selcted as the outlet!")
   }

   outletLat<- outletCoords$outletLat
   outletLng<- outletCoords$outletLng
   subbasinsIds<-rvh[[1]][,SubId]
   
   if(any(all(is.na(outletLat)),all(is.na(outletLng))))
   {
	  outletLng<-outletLat<-rep(NA,length(subbasinsIds))
	  for(i in 1:length(subbasinsIds))
	  {
	     subWatershed<-rvh[[2]][subbasinsIds[i]==rvh[[2]][,SubId],]
		 outletLat[i]<-mean(subWatershed$Latitude)
		 outletLng[i]<-mean(subWatershed$Longitude)
	  }
	  warning("Outlet coordinates were not provided. They were calculated by averaging the coordinates of HRUs within the outlet subbasin!")
   }
   basins@data$outletLat<-outletLat[id]
   basins@data$outletLng<-outletLng[id]
   basins@data$outletLat[naIds]<--9999
   basins@data$outletLng[naIds]<--9999

   # creating geojson file
   basins_json <- geojson_json(basins)
   
   if (simplifyGeometry)
   {
      basins_sim <- ms_simplify(basins_json,keep_shapes = TRUE)
   }else{
      basins_sim<-basins_json
   }
   geojson_write(basins_sim, file = outputfile)
   cat("Successfully Converted!\n")
   return("Successfully Converted!")
}
