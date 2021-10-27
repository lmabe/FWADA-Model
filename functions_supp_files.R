#### Functions for the Supplementary Data in Github

# FW_kmeans() runs the kmeans clustering. Calculates FW_gen & FW_dis for each cluster
# @param Bus_pnts - business points w/ FW to cluster
# @param AD_cap - the capacity of the ADs to be placed
# It returns a list 
# [1] business points w/ cluster assignment spdf (BUS)
# [2] cluster centroids spdf (AD)
FW_kmeans <- function(Bus_pnts, AD_cap) {
    
    
    # calculate number of ADs needed
    # rounded up, the 1.0 means we are capturing 100% of FW
    cent <- ceiling((sum(Bus_pnts$FW_dis) * 1.0) / AD_cap)
    
    
    message(paste0(cent, " clusters"))
    
    # Convert to matrix
    mBUS <- as.matrix(Bus_pnts@coords)
    
    
    # kmeans - used same params as we did in class bc i dont know what else to do
    # We want to create k clusters, allow 500 iterations, start with 5 random sets using "Hartigan/Wong" (default) method
    kmcluster <- kmeans(mBUS, centers = cent, iter.max = 500, nstart = 1)
    
    
    # Create df to add coords to
    AD <- data.frame(AD_code = paste0("x",1:nrow(kmcluster$centers)))
    coordinates(AD) <- kmcluster$centers
    
    AD@proj4string <- Bus_pnts@proj4string
    
    AD$initial_K <- cent
    AD$AD_cap <- AD_cap
    
    
    # Add cluster group to each BUS point
    BUS <- "Poop"
    BUS <- cbind(Bus_pnts, paste0("x",as.factor(kmcluster$cluster))) 
    colnames(BUS@data)[ncol(BUS@data)] <- c("AD_code")
    
    
    # calculate FW in each cluster
    AD$FW_gen <- sapply(AD$AD_code, function(x) sum(BUS$FW_gen[BUS$AD_code == x]))
    AD$FW_dis <- sapply(AD$AD_code, function(x) sum(BUS$FW_dis[BUS$AD_code == x]))
    
    
    
    poop <- list(AD, BUS)
    names(poop) <- c("AD", "BUS")
    
    
    return(poop)
} #close FW_kmeans
