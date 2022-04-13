# function that calculates statistics about each iteration

### IF YOU ADD THINGS TO ITERSUM, ALSO ADD THEM TO THE CORRECT PLOTTING GROUP in plot_m3_MC.Rmd

# I removed the @data slot from some places w/in the functions. 
# see GHG_iter_sum(), the system total section
# this is to run the itersum over the cluster only DF
# if running this where we want only the itersum after running the model, we may need to put those back

iter_sum <- function(ADs, break_pct = 0.7) {
    
    AD_cap <- ADs$AD_cap[1]
    
    # A function that finds mean and SD for the GHG related columns
    GHG_iter_sum <- function(AD) {
        
        # a list of GHG related columns
        # if the names of the cols change in update stats of m3, this needs to change too
        GHG_cols <- names(ADs)[grep(".1|.2|.3", names(ADs))]
        
        #~~~~ system total ~~~~~~~~~~#
        # removed @data after the AD being subset from
        sys_df <- apply(AD[,names(AD) %in% GHG_cols], 2, sum)
        names(sys_df) <- paste0("sys_", GHG_cols)
        sys_df <- as.data.frame(t(sys_df))
        
        
        #~~~~~~ means ~~~~~~~~~~~~~~#
        # get the means of the GHG cols
        means_df <- apply(AD[, names(AD) %in% GHG_cols], 2, mean)
        
        # colnames - same as OG, but with mean_ in front
        names(means_df) <- paste0("mean_", GHG_cols)
        
        # transpose and turn to long, 1 row df
        means_df <- as.data.frame(t(means_df))
        
        #~~~~~~~ standard devs ~~~~~~~~~#
        # get stand devs
        sd_df <- apply(AD[, names(AD) %in% GHG_cols], 2, sd)
        
        # give names w/ sd_
        names(sd_df) <- paste0("sd_", GHG_cols)
        
        # transpose and turn to long, 1 row df
        sd_df <- as.data.frame(t(sd_df))
        
        ret <- cbind(means_df, sd_df, sys_df)
        
        
        return(ret)
    } # close GHG_iter_sum()
    
    chull_iter_sum <- function(AD) {
        
        # list of chull related columns
        # if the names of the cols change in update stats of m3, this needs to change too
        chull_cols <- c("area", "FW_dens_sqm", "BUS_dens_sqm", 
                        "FW_p_BUS", "FW_p_BUS_sqmi", "collect_dist")
        
        #~~~~~~ means ~~~~~~~~~~~~#
        
        # get means
        means_df <- apply(AD[, names(AD) %in% chull_cols], 2, mean)
        
        # colnames
        names(means_df) <- paste0("mean_", chull_cols)
        
        # transpose and turn to long, 1 row df
        means_df <- as.data.frame(t(means_df))
        
        #~~~~~~ st devs ~~~~~~~~~~#
        sd_df <- apply(AD[,names(AD) %in% chull_cols], 2, sd)
        names(sd_df) <- paste0("sd_", chull_cols)
        sd_df <- as.data.frame(t(sd_df))
        
        #~~~~~~~ median ~~~~~~~~#
        med_df <- apply(AD[, names(AD) %in% chull_cols], 2, median)
        names(med_df) <- paste0("med_", chull_cols)
        med_df <- as.data.frame(t(med_df))
        
        #~~~~~~~ min ~~~~~~~~#
        min_df <- apply(AD[, names(AD) %in% chull_cols], 2, min)
        names(min_df) <- paste0("min_", chull_cols)
        min_df <- as.data.frame(t(min_df))
        
        #~~~~~~ max ~~~~~~~~~~~#
        max_df <- apply(AD[, names(AD) %in% chull_cols], 2, max)
        names(max_df) <- paste0("max_", chull_cols)
        max_df <- as.data.frame(t(max_df))
        
        ret <- cbind(means_df, sd_df, med_df, min_df, max_df)
        
        return(ret)
        
    } # close chull_iter_sum()
    
    
    # calculate the statistics
    # There is a better way to do this, but I wrote this before I knew that lol
    iter_summary <- data.frame(num_ADs = nrow(ADs),
                               initial_k = ADs$initial_K[1],
                               AD_cap = AD_cap,
                               min_FW_pct = (min(ADs$FW_dis) / AD_cap) * 100,
                               max_FW_pct = (max(ADs$FW_dis) / AD_cap) * 100,
                               mean_FW_pct = (mean(ADs$FW_dis) / AD_cap) * 100,
                               med_FW_pct = (median(ADs$FW_dis) / AD_cap) * 100,
                               count_over = sum(ADs$FW_dis > AD_cap),
                               pctADs_over = sum(ADs$FW_dis > AD_cap) / nrow(ADs) * 100,
                               count_underbrk = sum(ADs$FW_dis < AD_cap * break_pct),
                               pct_ADs_under = sum(ADs$FW_dis < AD_cap * break_pct) / nrow(ADs) * 100,
                               mean_FWpct_over = (mean(ADs$FW_dis[ADs$FW_dis > AD_cap]) / AD_cap) *100,
                               med_FWpct_over = (median(ADs$FW_dis[ADs$FW_dis > AD_cap]) / AD_cap) *100,
                               sum_FW_over = sum(ADs$FW_dis[ADs$FW_dis > AD_cap] - AD_cap),
                               mean_num_BUS = mean(ADs$num_BUS, na.rm = TRUE), 
                               sd_num_BUS = sd(ADs$num_BUS),
                               abs_min_FW = round(min(ADs$FW_dis)),
                               abs_max_FW = round(max(ADs$FW_dis)),
                               mean_FW = mean(ADs$FW_dis),
                               sd_FW_pct = (sd(ADs$FW_dis) / AD_cap) * 100,
                               mean_FW_over = mean(ADs$FW_dis[ADs$FW_dis > AD_cap]),
                               med_FW_over = median(ADs$FW_dis[ADs$FW_dis > AD_cap]),
                               sd_FW = round(sd(ADs$FW_dis)),
                               total_polys = sum(ADs$num_Polys, na.rm = TRUE),
                               total_jumps = sum(ADs$num_Polys, na.rm = TRUE) - nrow(ADs),
                               num_w_jumps = sum(ADs$num_Polys > 1, na.rm = TRUE),
                               pct_w_jumps = sum(ADs$num_Polys > 1, na.rm = TRUE) / nrow(ADs) * 100,
                               chull_NAs = nrow(ADs[is.na(ADs$num_Polys) == TRUE,]),
                               min_wss = min(ADs$withinss),
                               max_wss = max(ADs$withinss),
                               mean_wss = mean(ADs$withinss),
                               sd_wss = sd(ADs$withinss),
                               tot.wss = ADs$tot.withinss[1],
                               betss = ADs$betweenss[1],
                               avg_betss = ADs$avg_betss[1],
                               sys_collect_dist = sum(ADs$collect_dist))
    
    iter_summary <- cbind(iter_summary, GHG_iter_sum(ADs), chull_iter_sum(ADs))
    # changing
    return(iter_summary)
}# close iter_sum()

# standardizes the units of the model, output
# this could also go into the actual model,
# but I've already ran it without it so it stays outside
standardizeUnits <- function(itersum) {
    
    # _dist stuff from [Feet] to [Miles]
    itersum[, grep("_dist", colnames(itersum))] <-
        itersum[, grep("_dist", colnames(itersum))] / 5280
    
    # perim stuff from [Feet] to [Miles]
    itersum[, grep("perim", colnames(itersum))] <- 
        itersum[, grep("perim", colnames(itersum))] / 5280
    
    # area stuff from [Sq Feet] to [Sq Miles]
    itersum[, grep("area", colnames(itersum))] <- 
        itersum[, grep("area", colnames(itersum))] / 2.788e+7
    
    # sum squares stuff from [Feet] to [Miles]
    itersum[,grep("wss|betss", colnames(itersum))] <- 
        itersum[,grep("wss|betss", colnames(itersum))] / 5280
    
    # GHG stuff from [kg CO2e] to [MT CO2e]
    # search for the ones that end in .num bc those area the GHG scenarios
    itersum[, grep(".0|.1|.2|.3", colnames(itersum))] <- 
        itersum[, grep(".0|.1|.2|.3", colnames(itersum))] / 1000
    
    # fix sum_FW_over in itersum. Current MC run is sum of ALL FW in ADs over capacity
    # need to remove the FW up to capacity so we only have sum of the FW allocated over capacity
    # This has been fixed in the itersum function
    # MC runs after 6/30 do not need this fix
    itersum$sum_FW_over <- itersum$sum_FW_over - (itersum$count_over * itersum$AD_cap)
    
    return(itersum)
    
} # close standardizeUnits(itersum)


plot_kmeans_under_cap <- function(kmeans_result, tract_outline, FW_type = "FW_dis") {
    
    kmeans_result$AD@proj4string <- tract_outline@proj4string
    kmeans_result$BUS@proj4string <- tract_outline@proj4string
    
    poopAD <- st_as_sf(kmeans_result$AD)
    poopBUS <- st_as_sf(kmeans_result$BUS)
    tract_sf <- st_as_sf(tract_outline)
    
    if (FW_type == "FW_dis") { 
        clust_col <- ggplot() +
            geom_sf(mapping = aes(geometry = tract_sf$geometry), color = "black", fill = NA, data = tract_sf) +
            geom_sf(mapping = aes(geometry = poopBUS$geometry, color = poopBUS$AD_code), data  = poopBUS,  size = 1) +
            geom_sf(mapping = aes(geometry = poopAD$geometry), data = poopAD, color = "black", size = 3) +
            ggtitle(paste0(poopAD$inds[1], ", Num Bus = ", nrow(poopBUS), ", Num ADs = ", nrow(poopAD)),
                    subtitle = paste0("Initial K = ", ceiling(calcADsNeeded(poopBUS$FW_dis, poopAD$AD_cap[1], 1.0)),
                                      ", AD Cap = ", poopAD$AD_cap[1])) +
            theme_bw() +
            theme(legend.position = "none")
        
        AD_col <-  ggplot() +
            geom_sf(mapping = aes(geometry = tract_sf$geometry), color = "black", fill = NA, data = tract_sf) +
            geom_sf(mapping = aes(geometry = poopBUS$geometry),color = "grey", data  = poopBUS,  size = 1) +
            geom_sf(mapping = aes(geometry = poopAD$geometry, color = -poopAD$FW_dis), data = poopAD, size = 5) +
            ggtitle(paste0(poopAD$inds[1], ", Num Bus = ", nrow(poopBUS), ", Num ADs = ", nrow(poopAD)),
                    subtitle = paste0("Initial K = ", ceiling(calcADsNeeded(poopBUS$FW_dis, poopAD$AD_cap[1], 1.0)),
                                      ", AD Cap = ", poopAD$AD_cap[1])) +
            theme_bw()
        
        
    } else if (FW_type == "FW_gen") {
        clust_col <- ggplot() +
            geom_sf(mapping = aes(geometry = tract_sf$geometry), color = "black", fill = NA, data = tract_sf) +
            geom_sf(mapping = aes(geometry = poopBUS$geometry, color = poopBUS$AD_code), data  = poopBUS,  size = 1) +
            geom_sf(mapping = aes(geometry = poopAD$geometry), data = poopAD, color = "black", size = 3) +
            ggtitle(paste0(poopAD$inds[1], ", Num Bus = ", nrow(poopBUS), ", Num ADs = ", nrow(poopAD)),
                    subtitle = paste0("Initial K = ", ceiling(calcADsNeeded(poopBUS$FW_gen, poopAD$AD_cap[1], 1.0)),
                                      ", AD Cap = ", poopAD$AD_cap[1])) +
            theme_bw() +
            theme(legend.position = "none")
        
        AD_col <-  ggplot() +
            geom_sf(mapping = aes(geometry = tract_sf$geometry), color = "black", fill = NA, data = tract_sf) +
            geom_sf(mapping = aes(geometry = poopBUS$geometry),color = "grey", data  = poopBUS,  size = 1) +
            geom_sf(mapping = aes(geometry = poopAD$geometry, color = -poopAD$FW_gen), data = poopAD, size = 5) +
            ggtitle(paste0(poopAD$inds[1], ", Num Bus = ", nrow(poopBUS), ", Num ADs = ", nrow(poopAD)),
                    subtitle = paste0("Initial K = ", ceiling(calcADsNeeded(poopBUS$FW_gen, poopAD$AD_cap[1], 1.0)),
                                      ", AD Cap = ", poopAD$AD_cap[1])) +
            theme_bw()
    } else {
        print("Choose FW_dis or FW_gen") }
    
    ret_list <- list(clust_col, AD_col)
    names(ret_list) <- c("clust_col", "AD_col")
    return(ret_list)
    
} # return plot_kmeans

# creates a plot of the clusters
# @param km_res - a km/m3 result. list(AD=AD,BUS=BUS)
# @param tract_sp - a tract outline sp object
# @param samp_size - takes a sample of BUS points for faster plotting. Integer (n) takes 1/n points
# returns - a ggplot object (can be saved w/ ggsave)
clust_plot <- function(km_res, tract_sp = tract_outline, samp_size = 3) {
    
    AD_cap <- km_res$AD$AD_cap[1]
    
    # take a sample of BUS points to speed up mapping
    BUS_samp <- km_res$BUS[sample(nrow(km_res$BUS), nrow(km_res$BUS) / samp_size),]
    
    # determine % of total FW allocated to an AD over its capacity
    overcap <- sum(km_res$AD$FW_dis[km_res$AD$FW_dis > AD_cap] - AD_cap)
    
    
    ret <- ggplot() +
        layer_spatial(data = tract_sp, mapping = aes(), fill = "white") +
        layer_spatial(data = BUS_samp, mapping = aes(color = BUS_samp$AD_code), pch = 20) +
        layer_spatial(data = km_res$AD, mapping = aes(), color = "black", pch = 19) +
        annotation_scale(data = tract_sp,
                         mapping = aes(unit_category = "imperial", style = "ticks"), plot_unit = "ft") +
        annotation_north_arrow(data = tract_sp, mapping = aes(location = "br"), width = unit(0.75, "cm")) +
        theme_bw() +
        theme(legend.position = "none") +
        ggtitle(paste0("AD Cap: ", km_res$AD$AD_cap[1], " Num ADs: ", nrow(km_res$AD)),
                subtitle = paste0("FW Over Cap: ", round(overcap),
                                  " (", round(overcap/sum(km_res$AD$FW_dis)*100, 2), "%)"))
    
    
    return(ret)
} # close clust_plot





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

# This function get the statistics for each AD.
# @param kmean_res - a kmean result w/ ADs and BUS points (AD=AD, BUS=BUS). Cluster centers and FW_dis are already calculated
# returns - a df with statistics
update_stats <- function(kmean_res) {
    
    AD <- kmean_res$AD
    BUS <- kmean_res$BUS
    
    # get only AD_code column
    AD <- AD[,which(names(AD) == "AD_code")]
    
    ###### Update stats#####
    message("updating stats")
    
    # update stats for ADs
    ## number of businesses, for each AD_code in AD, count the number of rows in BUS with that AD code.
    AD$num_BUS <- sapply(AD$AD_code, function(x) nrow(BUS[BUS$AD_code == x,]), USE.NAMES = FALSE)
    
    
    ## min, max, mean, sd for distance from AD to BUS
    ## @param x = AD_code
    ## @param value = character vector of what we want to calculate
    update_AD <- function(x, value) {
        
        # get the AD
        AD_dist <- AD[AD$AD_code == x,]
        
        # get corresponding BUS cluster
        BUS_clust <- BUS[BUS$AD_code == x,]
        
        # find point distance between the AD and its BUS
        pd <- raster::pointDistance(BUS_clust, AD_dist, lonlat = FALSE)
        
        if (value == "min_dist") {
            ret <- min(pd)
        } else if (value == "max_dist") {
            ret <- max(pd)
        } else if (value == "mean_dist") {
            ret <- mean(pd)
        } else if (value == "sd_dist") {
            ret <- sd(pd)
        } else if (value == "total_dist") {
            ret <- sum(pd)
        } else if (value == "withinss") {
            # calculate SS for this cluster
            # sum of dist from AD to BUS, divided by number of BUS
            ret <- sum(pd) / nrow(BUS_clust)
        } # close ifelse
        
        return(ret)
    } # close update_AD()
    
    
    ## within cluster sum of squares for each AD
    AD$withinss <- sapply(AD$AD_code, FUN = update_AD, USE.NAMES = FALSE, value = "withinss")
    
    ## total within cluster sum of squares - this will be same for all ADs - global value
    AD$tot.withinss <- sum(AD$withinss)
    
    ## between cluster sum of squares - how spread out clusters are - same for all ADs, global value
    ## sum of distances between all ADs
    AD$betweenss <- sum(pointDistance(AD, lonlat = FALSE))
    
    ## Average between cluster sum of squares - betweenss divided by number of clusters
    AD$avg_betss <- AD$betweenss[1] / nrow(AD)
    
    ## the silouette value for each cluster takes too long to calculate
    
    message("chull stats")
    # chull related stats
    chullStats <- function(cluster, stat) {
        
        c_hull <- polygons(convHull(cluster))
        # return stats
        
        if (stat == "area") {
            return(c_hull@polygons[[1]]@area)
        } else if (stat == "FW_density") {
            return(sum(cluster$FW_dis) / (c_hull@polygons[[1]]@area/2.788e+7)) 
        } else if (stat == "BUS_density") {
            return(nrow(cluster) / (c_hull@polygons[[1]]@area /2.788e+7))
        } else if (stat == "FW_per_BUS") {
            return(sum(cluster$FW_dis) / nrow(cluster))
        }else if (stat == "FW_per_BUS_sqmi") {
            return((sum(cluster$FW_dis) / nrow(cluster)) / (c_hull@polygons[[1]]@area /2.788e+7))
        } else {
            print("choose a stat") 
        } # close if/else
        
        
    } # close chullStats()
    
    AD$area <- sapply(AD$AD_code, function(x) chullStats(BUS[BUS$AD_code == x,], stat = "area"))
    AD$FW_dens_sqm <- sapply(AD$AD_code, function(x) chullStats(BUS[BUS$AD_code == x,], stat = "FW_density"))
    AD$BUS_dens_sqm <- sapply(AD$AD_code, function(x) chullStats(BUS[BUS$AD_code == x,], stat = "BUS_density"))
    AD$FW_p_BUS <- sapply(AD$AD_code, function(x) chullStats(BUS[BUS$AD_code == x,], stat = "FW_per_BUS"))
    AD$FW_p_BUS_sqmi <- sapply(AD$AD_code, function(x) chullStats(BUS[BUS$AD_code == x,], stat = "FW_per_BUS_sqmi"))
    
    
    
    
    #~~~~~ Jumping ~~~~~#
    # message("jumping stats")
    # 
    # # finds jumps
    # # @param t_code: the AD_code of the target AD
    # # @param km_res: a kmean result
    # # returns: a value - this is the number of polygons in the target cluster
    # #                   after the intersecting clusters have been erased
    # #                   if this number is greater than 1, there is a jump
    # #                   if returned an NA, then the target cluster got completely erased in the function
    # chullDetectJumping <- function(t_code, km_res) {
    #   
    #   
    #   # function to run chull and create polygon
    #   # @param cluster - a BUS point cluster
    #   # returns - a polygon of the convex hull of the cluster
    #   chullPoly <- function(cluster) {
    #     
    #     outline <- polygons(convHull(cluster))
    #     
    #     return(outline)
    #   } # close chullPoly()
    #   
    #   
    #   out <- tryCatch(
    #     {
    #       #message("this is the try part")
    #       
    #       # create target cluster outline
    #       t_outline <- chullPoly(km_res$BUS[km_res$BUS$AD_code == t_code,])
    #       
    #       
    #       in_chull <- km_res$BUS[t_outline,] # clip BUS points to t_outline
    #       in_chull <- levels(as.factor(in_chull$AD_code)) # get the AD_codes
    #       in_chull <- in_chull[in_chull != t_code] # remove t_code from list
    #       
    #       # get the c_hull for all of those clusters
    #       # this is a list of polygons
    #       clip_polys <- lapply(in_chull, function(x) chullPoly(km_res$BUS[km_res$BUS$AD_code == x,]))
    #       
    #       # clip the target cluster to the others
    #       # also clip to tract_outline outline
    #       # not sure if need loop, could use lapply?
    #       
    #       if (length(clip_polys) > 0) {
    #         for (i in 1:length(clip_polys)) {
    #           
    #           # if we completely erase t_outline, return NA
    #           t_outline <- erase(t_outline, clip_polys[[i]])
    #           
    #           
    #         } # close for loop
    #       } # close length if
    #       
    #       
    #       t_outline <- intersect(t_outline, tract_outline) # clip to tract outline (remove ocean)
    #       
    #       t_outline <- aggregate(t_outline) # get back to one polygon (intersect returns the tract outlines)
    #       
    #       # remove slivers
    #       t_outline <- buffer(t_outline, width = -10)
    #       
    #       # just Polygons
    #       t_polys <- t_outline@polygons[[1]]@Polygons
    #       
    #       # get which Polys are holes
    #       # returns T/F vector - holes = TRUE
    #       holes <- sapply(t_polys, slot, "hole")
    #       
    #       # get which Polys are small
    #       # returns T/F vector = less than 1sq mi = TRUE
    #       smalls <- sapply(t_polys, function(x) round(x@area/5280) < 1)
    #       
    #       ret <- sum((holes + smalls) < 1)
    #       # from tryCatch site: the return value here is the actual value returned if no condition
    #       # no need for return() as code in "try" part is not wrapped in function
    #       
    #       
    #       # return number of Polys for cluster
    #       # sum of holes + islands for "real" Polys will be 0, so need number of those
    #       # return(sum((holes + smalls) < 1))
    #     }, # close try part
    #     error=function(cond) {
    #        message("we hit an error in jumping stats")
    #       # message("heres original error message: ")
    #       # message(cond)
    #       # 
    #       # choose new return value if theres an error
    #       return(NA)
    #     }, # close error part
    #     finally = {
    #       # everything that should be executed at the end
    #       # i dont think we need anything here
    #       #message("FINALLY")
    #     } # close finally
    #   ) # close tryCatch()
    #   
    #   return(out) # return the results from tryCatch
    #   
    # } # close chullDetectJumping
    # 
    # # number of polygons for the cluster
    # AD$num_Polys <- sapply(AD$AD_code, FUN = chullDetectJumping, km_res = list(AD = AD, BUS = BUS))
    AD$num_Polys <- 5
    # lat/long data
    AD$lat <- AD@coords[,1]
    AD$long <- AD@coords[,2]
    
    
    return(AD@data)
    
    
} # close update_stats()




# GHG stats calculates GHG statistics for each AD
# @param kmean_res - the AD and BUS sp objects. In the list like a kmeans result (AD=AD, BUS=BUS)
# @param alt_waste_facs - sp object. Alternate waste facilities for use w/ scenario 0 and 1
# returns: a df of GHG stats
GHG_stats <- function(kmean_res, alt_waste_facs = waste_facs) {
    
    
    ### Functions #####
    
    # finds the TSP collection distance of a cluster going in a circle
    # @param cluster - the cluster to find the dist for - sp object
    # @param AD_coords - the coords for the clusters AD - matrix
    # @param maxclust - the maximium size a cluster can be
    #                   clusters over maxclust are split into quadrants before runnng TSP
    # returns: the distance of the route in miles - a number
    TSP_circle <- function(cluster, AD_coords, maxclust) {
        
        # cardinalSplit() splits the given cluster along the cardinal directions
        # @param cluster2 - the point cluster - sp object
        # @param nsplit2 - the size of the AD that needs to be split
        # returns - a list of sp objects - each is a point cluster
        cardinalSplit <- function(cluster2, nsplit2) {
            
            # find the weighted center of the AD
            # can't just use the AD coordinates if its a second split
            AD_coords2 <- matrix(c(weighted.mean(x = cluster2@coords[,1],
                                                 w = cluster2$FW_dis),
                                   weighted.mean(x = cluster2@coords[,2],
                                                 w = cluster2$FW_dis)),
                                 nrow = 2, ncol = 1)
            
            # if the cluster if over 10000 BUS points, it needs to be split into 4
            if (nrow(cluster2) > nsplit2) {
                poop <- cluster2@bbox
                
                # combine the bbox coords with the weighted center
                poop <- cbind(poop, AD_coords2)
                
                rownames(poop) <- c("x", "y")
                colnames(poop) <- c("min", "max", "med")
                
                
                
                # make a polygon for each quadrant
                # there's probably a way to do this automatically, but since I only need 4, its by hand
                # the points creating the polygons are the top left (NW) point in that poly, going clockwise
                # (should have started at center point, oh well)
                polys_list <- list(NA, NA, NA, NA)
                
                polys_list[[1]] <- Polygon(matrix(c(poop["x", "min"], poop["x", "med"],
                                                    poop["x", "med"], poop["x", "min"], 
                                                    poop["y", "max"], poop["y", "max"],
                                                    poop["y", "med"], poop["y", "med"]),
                                                  nrow = 4, ncol = 2))
                polys_list[[1]] <- Polygons(list(polys_list[[1]]), 1)
                polys_list[[1]] <- SpatialPolygons(list(polys_list[[1]]))
                proj4string(polys_list[[1]]) <- tract_outline@proj4string
                
                polys_list[[2]] <- Polygon(matrix(c(poop["x", "med"], poop["x", "max"],
                                                    poop["x", "max"], poop["x","med"],
                                                    poop["y", "max"], poop["y", "max"],
                                                    poop["y", "med"], poop["y", "med"]),
                                                  nrow = 4, ncol = 2))
                polys_list[[2]] <- Polygons(list(polys_list[[2]]), 1)
                polys_list[[2]] <- SpatialPolygons(list(polys_list[[2]]))
                proj4string(polys_list[[2]]) <- tract_outline@proj4string
                
                polys_list[[3]] <- Polygon(matrix(c(poop["x", "med"], poop["x", "max"],
                                                    poop["x", "max"], poop["x", "med"],
                                                    poop["y", "med"], poop["y", "med"],
                                                    poop["y", "min"], poop["y", "min"]),
                                                  nrow = 4, ncol = 2))
                polys_list[[3]] <- Polygons(list(polys_list[[3]]), 1)
                polys_list[[3]] <- SpatialPolygons(list(polys_list[[3]]))
                proj4string(polys_list[[3]]) <- tract_outline@proj4string
                
                polys_list[[4]] <- Polygon(matrix(c(poop["x", "min"], poop["x", "med"],
                                                    poop["x", "med"], poop["x", "min"],
                                                    poop["y", "med"], poop["y", "med"],
                                                    poop["y", "min"], poop["y", "min"]),
                                                  nrow = 4, ncol = 2))
                polys_list[[4]] <- Polygons(list(polys_list[[4]]), 1)
                polys_list[[4]] <- SpatialPolygons(list(polys_list[[4]]))
                proj4string(polys_list[[4]]) <- tract_outline@proj4string
                
                
                # Get the points in each quadrant
                # create a list w/ 4 different groups of points
                points_list <- lapply(polys_list, function(x) {cluster2$split <- over(cluster2, x)
                cluster2[is.na(cluster2$split) == FALSE,]})
                
                names(points_list) <- c("1", "2", "3", "4")
                
                # if none of the points need to be split,
                # just make a list with the whole cluster
                # needs to be a list, thats whats expected
            } else {
                
                points_list <- list(cluster)
                names(points_list) <- c("all")
                
            } # close if/else
            
            return(points_list)
            
        } # close cardinalSplit()
        
        
        # run_TSP() runs the TSP problem over the given point cluster
        # @param clust - the point cluster - sp object
        # @param close_pnt - the point used to find the start point - coords
        # returns - the tour length in miles
        run_TSP <- function(clust, close_pnt) {
            # print(paste0("clust size ", nrow(clust@coords)))
            # sometimes splitting the cluster returns an empty quadrant
            # if that happens, return a 0 for the length
            if (length(clust@coords) > 0) {
                
                # get the starting point for the TSP, it is closest to the
                d <- pointDistance(close_pnt, clust@coords, lonlat = FALSE)
                start_idx <- which(d == min(d))
                
                # get ETSP object with cluster, solve it, find tour length
                tsp_obj <- as.ETSP(clust@coords)
                tour <- solve_TSP(tsp_obj, method = "nn", control = c(start = start_idx))
                
                # length of tour + distance to travel between quadrants
                tour_dist <- tour_length(tour, tsp_obj)
                tour_dist <- tour_dist + (min(d))
                
                # return the ending points idx
                tour <- as.integer(tour)   
                end_idx <- tour[length(tour)]
                
                
                #res <- list(start_idx = start_idx, end_idx = end_idx, tour_dist = tour_dist)
                res <- list(end_idx = end_idx, tour_dist = tour_dist)
            } else {
                
                #res <- list(start_idx = 1, end_idx = NA, tour_dist = 0)
                res <- list(end_idx = NA, tour_dist = 0)
            } # close if else
            return(res)
        } # close run_TSP()
        
        #~~~~~~~~~~~~~~~~~~~~~~~~~Begin TSP_circle() Function ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
        
        # split the cluster as needed
        points_list <- cardinalSplit(cluster, nsplit2 = maxclust)
        
        
        # run through the list, get size of each list
        num_points <- lapply(points_list, function(x) nrow(x))
        
        # if needed, split the ones that need to be split again
        # remove the original from the points_list
        # and add the new split clusters back into the points_list
        if (sum(num_points > maxclust) > 0) {
            points_list2 <- lapply(which(num_points > maxclust),
                                   function(x) cardinalSplit(points_list[[x]], nsplit2 = maxclust))
            points_list2 <- unlist(points_list2)
            
            # manually change the names of points_list2 if they came from
            # NW or SW quadrants
            # this will allow us to order them the way we want and the TSP will make a nice loop
            names(points_list2)[grep("1.", names(points_list2))] <- c("1.3", "1.4", "1.1", "1.2")
            names(points_list2)[grep("4.", names(points_list2))] <- c("4.3", "4.4", "4.1", "4.2")
            
            points_list <- points_list[-which(num_points > maxclust)]
            points_list <- c(points_list, points_list2)
        } # close if
        
        # order the list by their name to get circle to go the right way
        points_list <- points_list[order(names(points_list))]
        
        # the first time its ran, start at the point closest to the cluster AD
        close2 <- AD_coords
        #start_idx_list <- list()
        end_idx_list <- list()
        #start_sp <- list()
        end_sp <- list()
        route_dist <- list()
        
        # run the TSP over the whole thing
        for (i in 1:length(points_list)) {
            
            poop <- run_TSP(clust = points_list[[i]], close_pnt = close2)
            
            # separate the results and save
            #start_idx_list[[i]] <- poop$start_idx
            #end_idx_list[[i]] <- poop$end_idx
            route_dist[[i]] <- poop$tour_dist
            
            # actual sp object
            #start_sp[[i]] <- points_list[[i]][start_idx_list[[i]],]
            
            # if the quadrant is empty, duplicate the end SP of the previous quadrant
            # if the NW quadrant is empty (first evaluated), end sp is the AD
            # this is a lot of ifs....maybe should have split on PD to prevent empty quadrants??
            if (is.na(poop$end_idx) == TRUE) {
                if (i == 1) {
                    end_sp[[i]] <- SpatialPoints(AD_coords, cluster@proj4string)
                } else {
                    end_sp[[i]] <- end_sp[[i-1]]
                }
            } else {
                end_sp[[i]] <- points_list[[i]][poop$end_idx,]
            } # close ifelse
            
            # if the cluster is empty, the end_idx wil return as NA
            # in that case, dont change the close2 point
            if (is.na(poop$end_idx) == TRUE) {
                close2 <- close2
            } else {
                # reset the close2 variable as the end point
                close2 <- points_list[[i]][poop$end_idx,]
            } # close ifelse
            
        } # close for loop
        
        # add tour lengths together
        route_dist <- do.call(sum, route_dist)
        
        # also add the distance from the last stop back to the AD
        d <- pointDistance(AD_coords, end_sp[[length(end_sp)]]@coords, lonlat = FALSE)
        route_dist <- route_dist + (d)
        
        return(route_dist)
        
        
    } # close TSP_circle(cluster, AD_coords, maxclust)
    
    
    
    
    ### ~~~~~~~~~~~~~~~~~~~~~Begin GHG_stats function~~~~~~~~~~~~~ ####
    
    # separate the kmeans result
    AD <-  kmean_res$AD
    BUS <- kmean_res$BUS
    
    # Get collection distance
    message("collection/hauling stats")
    
    AD$collect_dist <- sapply(AD$AD_code, function(x) {
        clust <- BUS[BUS$AD_code == x,]
        AD_coords <- matrix(c(weighted.mean(x = clust@coords[,1],
                                            w = clust$FW_dis),
                              weighted.mean(x = clust@coords[,2],
                                            w = clust$FW_dis)),
                            nrow = 1, ncol = 2)
        TSP_circle(clust, AD_coords, 10000)})
    
    
    
    message("GHG stats")
    #~~~~~~~ Unit Conversions ~~~~~~~~~~~#
    
    # convert tons FW into kg
    AD$AD_cap_kg <- AD$AD_cap * 907.185
    AD$FW_dis_kg <- AD$FW_dis * 907.185
    
    # convert feet to miles
    AD$collect_dist_mi <- AD$collect_dist / 5280
    
    
    #~~~~~~ Set up constants ~~~~~~~~~~~~#
    
    
    # nitrogen in feedstock - [kg]
    # assuming N content is 2.8% of dry matter (DM)
    # assuming DM content 18.1% wet weight
    # Source: Fisgativa et. al. (2016)
    AD$N_fdstck <- AD$FW_dis_kg * 0.181 * 0.028
    
    
    # Avoided LFG emissions [kgCO2e/kg waste]
    # Food waste avoided landfill emissions factor [0.39 MTCO2e/short ton waste]
    ### Source: CCI Emissions Factor database - Food waste tab/Landfill tab. They cite:
    ### "CARB Method for Estimating Greenhouse Gas Emission Reductions from Diversion
    ###   of Organic Waste from Landfills to Compost Facilities (2017)"
    GHG_FLW_prevent <- 0.39 * 1000*2.2/2000
    
    # Transportation (Medium & Heavy Duty Vehicles Diesel Vehicles) - [kg CO2e per kg/mi]
    ## Emissions factors: CO2 [10.21 kg CO2e/gal fuel], CH4 & N20 [0.0000051/0.0000048 kg CO2e/mile]
    ### Source: https://www.epa.gov/sites/default/files/2018-03/documents/emission-factors_mar_2018_0.pdf
    ## Truck MPG: [0.22727 gal/mile]
    ### Source: https://www.tandfonline.com/doi/pdf/10.1080/10962247.2014.990587?needAccess=true
    ## Truck capacity: [25,000lbs/11340kg]
    ### Source: https://refusetrucks.scrantonmfg.com/automated-side-loader/roto-pac 
    ## GWP 100 - IPCC
    GHG_fuel_use <- ((0.22727 * 10.21) + (0.0000051 * 25) + (0.0000048 * 298))/11340
    
    # Avoided natural gas (NG) consumption emissions factor - [kg CO2e/m3 of NG]
    # Citation: 53.12 from https://www.eia.gov/environment/emissions/co2_vol_mass.php, and 28.32 m3 per tcf
    avoid_NG_ef <- 53.12/28.32
    
    # Avoided Nitrogen fertilizer production - [kg CO2e/kg of N]
    # Source: Moller 2009
    avoid_Nfert_ef <- 8.9
    
    #~~~~~~ Calculate GHG Values Scenario 1 ~~~~~~~#
    # Scenario 1, all FW goes to digestor, but AD less efficient
    
    # methane generation for from AD - m3
    # Based on Organic Loading Rate
    # For non-overfilled digesters, baseline assumptions:
    # OLR_base = digester size (55,000 ton; 10,000 ton; 5,000 ton) / 365 days = [ton/days] (OLR - Organic loading rate)
    # example:  55,000 ton / 365 days = 150.7 ton/day
    # Discount Factor = 1
    
    # Overfilled Digesters:
    # OLR_new = tons / 365 days
    # example: 65,000 ton/365 days = 178 ton/day
    # Discount Factor (DF) = OLR_base / OLR_new
    # example:  150.7 ton/day / 178 ton/day = 0.85
    
    # Methane Production Calculation - add discount for all calculations
    # AD_cap_kg*0.26*0.45*DF
    
    OLR_base <- AD$AD_cap_kg[1] / 365
    
    # Methane generation by AD - [m3]
    # Function of volatile solids in food waste
    #assume 26% volatile solids (Zhang et al 2007, Fisgativa et al 2016)
    #assume  0.45 m3 ch4/kg VS (Bong et al 2018, Fisgativa et al 2016)
    # if FW allocated to AD higher than AD capacity, use a discount factor of OLR_base/OLR_new
    # otherwise, discount factor is 1
    AD$methane <- ifelse(AD$FW_dis_kg > AD$AD_cap_kg, 
                         AD$FW_dis_kg * 0.26 * 0.45 * (OLR_base / (AD$FW_dis_kg/365)),
                         AD$FW_dis_kg * 0.26 * 0.45 * 1)
    
    # assuming 60% methane is NG
    # [m3]
    AD$NG_equiv <- AD$methane * 0.6
    
    # all units kg C02e / year
    # collect GHG calculate GHG for 1 weekly pickup, then multiply by 52
    GHG_calcs_df1 <- data.frame(LFG_avoid.1 = AD$FW_dis_kg * GHG_FLW_prevent,
                                CO2e_collect.1 = 52 * ((AD$FW_dis_kg/52) * AD$collect_dist_mi * GHG_fuel_use), 
                                avoid_NG_CO2e.1 = AD$NG_equiv * avoid_NG_ef, 
                                avoid_N_CO2e.1 = AD$N_fdstck * avoid_Nfert_ef)
    
    
    GHG_calcs_df1$total_CO2e_kg.1 <- GHG_calcs_df1$CO2e_collect.1 - GHG_calcs_df1$LFG_avoid.1-GHG_calcs_df1$avoid_NG_CO2e.1-GHG_calcs_df1$avoid_N_CO2e.1
    
    
    # cut transport weight in half to account for linear increasing FW volume
    GHG_calcs_df3 <- data.frame(LFG_avoid.3 = AD$FW_dis_kg * GHG_FLW_prevent,
                                CO2e_collect.3 = 52 * (((AD$FW_dis_kg/52)/2) * AD$collect_dist_mi * GHG_fuel_use), 
                                avoid_NG_CO2e.3 = AD$NG_equiv * avoid_NG_ef, 
                                avoid_N_CO2e.3 = AD$N_fdstck * avoid_Nfert_ef)
    GHG_calcs_df3$total_CO2e_kg.3 <- GHG_calcs_df3$CO2e_collect.3 - GHG_calcs_df3$LFG_avoid.3-GHG_calcs_df3$avoid_NG_CO2e.3-GHG_calcs_df3$avoid_N_CO2e.3
    
    #~~~~~~ Calculate GHG Values Scenario 2 ~~~~~~~#
    # Scenario 2, a larger AD is placed there, all FW gets digested
    # all FW digested at 100% efficiency
    
    # Methane generation by AD - [m3]
    # Function of volatile solids in food waste
    #assume 26% volatile solids (Zhang et al 2007, Fisgativa et al 2016)
    #assume  0.45 m3 ch4/kg VS (Bong et al 2018, Fisgativa et al 2016)
    AD$methane <- AD$FW_dis_kg * 0.26 * 0.45
    
    # natural gas equivalent - [m3]
    # assuming 60% methane
    AD$NG_equiv <- AD$methane * 0.6
    
    # all units kg C02e / year
    # collect GHG calculate GHG for 1 weekly pickup, then multiply by 52
    GHG_calcs_df2 <- data.frame(LFG_avoid.2 = AD$FW_dis_kg * GHG_FLW_prevent,
                                CO2e_collect.2 = 52 * ((AD$FW_dis_kg/52) * AD$collect_dist_mi * GHG_fuel_use), 
                                avoid_NG_CO2e.2 = AD$NG_equiv * avoid_NG_ef, 
                                avoid_N_CO2e.2 = AD$N_fdstck * avoid_Nfert_ef)
    
    
    GHG_calcs_df2$total_CO2e_kg.2 <- GHG_calcs_df2$CO2e_collect.2 - GHG_calcs_df2$LFG_avoid.2- GHG_calcs_df2$avoid_NG_CO2e.2 -GHG_calcs_df2$avoid_N_CO2e.2
    
    
    
    
    
    #~~~~~~~~ Merge together ~~~~~~~~~~~~~~~~~~~~~~~#
    
    
    # merge all scenarios DFs together
    ret <- cbind(AD[,which(names(AD) %in% c("AD_code", "collect_dist"))],
                 GHG_calcs_df1, 
                 GHG_calcs_df2, 
                 GHG_calcs_df3)
    return(ret)
    
} # close GHG stats









##### Model 3 ######
# model 3 is model 3
# @param kmean_result - the result of the initial kmeans run
# @param adj_limit - the distance to be considered adjacent - NOT USED
# @param peak_value - used in getAdjADs_dist()
#                   - used to determine what is considered a peak based on the distance difference plot
#                   - this value is multipled by the standard dev of distance diff. 
#                   - anything above sd_diff * peak_value is a large enough jump to be considered a peak
#                   - points below the first peak are considered adjacent
# @param break_pct - the % fill we want all ADs over
# @param draw_map - T/F, do we draw maps during the model run? - increases runtime significantly
# @param radius - the "radius" used to move the split line in splitCluster()
#               - the line is straight, the name comes from FWd, bc its a similar concept
# @param split_value - the tolerance from center in splitCluster().
#                    - defaults to 0.025 (5% of total FW in cluster)
#                    - If the split is w/in X_pct of total FW, its close enough
#                    - EX: if you want split to be w/in 10% of total FW in cluster, use 0.05 (cut in half)
# @param print_progress: T/F to print updates on model progress
model_3 <- function(kmean_result, adj_limit = 10560,
                    peak_val = 0.9, break_pct = 0.70, draw_map = FALSE,
                    radius, split_tolerance = 0.025, print_progress = FALSE) {
    
    ############### Functions ########
    
    # quick_map_save() saves a base plot map of the model
    # @param km_res - kmean_result
    # @param tract_outline - the tract outline
    # @param mess - a message to print with the map
    # no return - saves to disk
    quick_map_save <- function(km_res, tract_outline, mess) {
        
        AD_cap2 <- km_res$AD$AD_cap[1]
        
        png(filename = paste0("C:/Users/Lauren Mabe/Box Sync/Food Waste Mapping/R_Stuff/ESRI_BA_Data/",
                              "Estimated_Pnt_Location/Model_3/Maps/M3_", AD_cap2, "_", rep_num, ".png"))
        
        plot(tract_outline, main = mess)
        points(km_res$BUS, pch = 19, col = substr(gsub("-", sample(1:10, 1), km_res$BUS$AD_code), 2,3))
        points(km_res$AD, pch = 21, col = "black", bg = "white")
        
        dev.off()
        
    } # close quick_map_save()
    
    
    # weighted_center finds the weighted center of a given cluster
    # @param code - the AD code to change
    # no return, changes AD@coords in the global env
    weighted_center <- function(code) {
        
        # find the weighted centroid of the new business cluster
        # the x coordinate for the AD eveything got added to = weighted mean of its cluster's x coords
        AD@coords[AD$AD_code == code, 1] <<- weighted.mean(x = BUS@coords[BUS$AD_code == code, 1],
                                                           w = BUS$FW_dis[BUS$AD_code == code])
        
        # the y coordinate for the AD everything got added to = weighted mean of its clusters y coords
        AD@coords[AD$AD_code == code, 2] <<- weighted.mean(x = BUS@coords[BUS$AD_code == code, 2],
                                                           w = BUS$FW_dis[BUS$AD_code == code])
    } #close weighted_center()
    
    #FWd_func_one_point() - returns the cumulative FW as a function of distance from the a specified AD point.
    # I basically just modified the one_point Kd function
    #@param point - the point in question (usually lowest filled AD)
    #@param all_ADs - the other ADs/points to measure to
    #@param radius - the radius of the FW_func and the breaks. Defaults to 50mi @ 1000 ft intervals
    #returns - a df that can be plotted in ggplot
    FWd_func_one_point <- function(point, all_ADs, radius = seq(1, 264000, 1000)) {
        
        all_ADs2 <- all_ADs
        # point distance bt lowest point and all others
        d <- raster::pointDistance(point, all_ADs2, lonlat = FALSE)
        
        # a series of circles around events - every 100 feet from 0 to 15 miles
        distance2 <- radius
        
        # attach the pointdistance (d) to the good_NN df
        all_ADs$d_from_low <- d
        
        # 
        # for every distance, want the sum of FW for points within that distance
        FWd <- sapply(distance2, function(x) sum(all_ADs$FW_dis[all_ADs$d_from_low <= x]))
        
        FWd_df <- data.frame(kdist = distance2, FWd = FWd, num_ADs = nrow(all_ADs))
        
        return(FWd_df)
    } # close FWd_one_point
    
    
    
    #splitCluster - splits the cluster perpendicular to the principle direction
    # @param cluster_code - the BUS cluster, this is just the code
    # @param kmean_res - a kmean result. This creates a copy for internal calculations.
    # @param radius - the distance to move the split line to create equal clusters
    # @ param X_pct - the tolerance from center in split cluster. If the split is w/in X_pct of total FW, its close enough
    #                   EX: if you want split to be w/in 10% of total FW in cluster, use 0.05 (cut in half)
    # returns - the BUS cluster
    # also modifies AD in the global environment
    splitCluster <- function(clust_code, kmean_res, radius, X_pct = split_tolerance) {
        
        
        # this function gets the coordinates for the a given theta. 
        # for a given theta, it puts the PD point onto the correct side of the bounding box of the cluster
        # @param theta - an angle from the east centered on wc that the PD point goes to
        # @param bb1 - the bounding box from the cluster
        # returns - the coordinates for a given theta
        # it gets lapplied over the list of thetas
        getPDpointCoords <- function(theta, bb1 = bb) {
            
            if (theta > 45 && theta < 135) {
                # on north side
                
                # find length of x
                # adj = opp/tan(theta)
                # length of opp (y) is distance from weighted center to N bbox
                d <- (bb1["NS", "max"] - wc[2]) / tan(theta*pi/180)
                
                # add/subtract that from center's x coord - this becomes the x coord
                x_coord <- wc[1] + d
                
                # y coord is north bbox y coord
                y_coord <- bb1["NS", "max"]
                
                # return coords
                return(c(x_coord, y_coord))
                
            } else if (theta > 135 && theta < 225) {
                # on east
                
                # change theta to interior angle
                theta <- 180 - theta
                
                # find length of y
                d <- (bb1["EW", "min"] - wc[1]) * tan(theta*pi/180)
                
                # add/sub that from centers y coord - this becomes the y coord
                y_coord <- wc[2] - d # this was + before, but this specific point kept coming out backwards, switched to -
                
                # x coord is bbox x coord
                x_coord <- bb1["EW", "min"]
                
                # return coords
                return(c(x_coord, y_coord))
                
            } else if (theta > 225 && theta < 315) {
                # on south bbox
                
                # change theta to interior angle
                theta <- 270 - theta
                
                # find length of x
                d <- (bb1["NS", "min"] - wc[2]) * tan(theta*pi/180)
                
                # add/sub from center x coord - this becomes the x coord
                x_coord <- wc[1] + d 
                
                # y coord is bbox's y coord
                y_coord <- bb1["NS", "min"]
                
                # return coords
                return(c(x_coord, y_coord))
                
            } else if (theta > 315 || theta < 45) {
                
                # find length of y
                d <- (bb1["EW", "max"] - wc[1]) * tan(theta*pi/180)
                
                # add/sub from center y coord - this becomes the y coord
                y_coord <- wc[2] + d
                
                # x coord is bbox's x coord
                x_coord <- bb1["EW", "max"]
                
                # return coords
                return(c(x_coord, y_coord))
                
            } # close ifelse
            
        } # close getPDpointCoords
        
        
        # createSplitPoly creates the polygon to use to split the cluster
        # @param pd_pnts - sp object representing the PD points of the cluster
        # @param bb_pnts - an sp object representing the bbox corners
        # returns a list
        # split_poly - the polygon used to split the cluster w/ over()
        # poly_points - points at the corners of the polygon, this holds the data for which point is which
        createSplitPoly <- function(pd_pnts, bb_pnts) {
            
            split_line <- pd_pnts[pd_pnts$type == "split",]
            
            
            # add the PD_point (point on other line furthest from weighted center)
            poop <- pd_pnts[pd_pnts$length == max(pd_pnts$length),]
            split_line <- rbind(split_line, poop[poop$dist_to_wc == max(poop$dist_to_wc),])
            
            # add the bbox points where the coordinates of bbox_sp match the PD_point
            # https://stackoverflow.com/questions/28233561/finding-rows-containing-a-value-or-values-in-any-column
            # the apply goes over the rows, checks if any() of the values match
            # its within a which() so arr.ind = TRUE returns the row indexes that match
            # this is within selecting from bbox_sp using [row,col] - selecting the rows w/ index returned from which()
            poly_points <- rbind(split_line[,1:3],
                                 bb_pnts[which(apply(bb_pnts@coords, 1,
                                                     function(r) any(r %in% split_line[3,]@coords)) == TRUE, 
                                               arr.ind = TRUE),])
            
            # the points need to be in the right order. need to go in a circle
            poly_points$OID <- 1:5
            poly_points <- poly_points[order(poly_points$theta),]
            
            
            # create the polygon
            # create a polygon with opp_pd box we made
            split_poly <- Polygon(poly_points)
            split_poly <- Polygons(list(split_poly),1)
            split_poly <- SpatialPolygons(list(split_poly))
            proj4string(split_poly) <- tract_outline@proj4string
            
            ret <- list(split_poly = split_poly, poly_points = poly_points)
            
            return(ret)
        } # close getSplitPoly()
        
        
        
        # copies of the global environment for calculations. Not sure if best way, but we got it
        cluster <- kmean_res$BUS[kmean_res$BUS$AD_code == clust_code,]
        AD2 <- kmean_res$AD[kmean_res$AD$AD_code == clust_code,]
        
        # trying to split evenly in half
        # get the target FW before splitting the cluster, will need later
        target_FW <- sum(cluster$FW_dis) / 2
        
        # run principal direction
        pc <- princomp(cluster@coords)
        
        # get the loadings for the two vectors 
        pc_mat <- matrix(as.numeric(pc$loadings), ncol = 2)
        
        # row/col names
        colnames(pc_mat) <- c("x", "y")
        
        # radian to degree function
        # https://stackoverflow.com/questions/32370485/convert-radians-to-degree-degree-to-radians
        rad2deg <- function(rad) {(rad * 180) / (pi)}
        
        # get theta function. Returns theta when given the loadings matrix
        get_theta <- function(i) {
            t <- atan2(i["y"], i["x"]) # find angle from x axis
            t <- rad2deg(t) # convert to degrees
            #t <- 90 - t # angle from north, commented out, want angle from east
            return(t)
        } # close get_theta()
        
        # get the angle from x axis of the PD
        # PD is the eigenvector with max standard dev
        theta1 <- get_theta(pc_mat[which(pc$sdev == max(pc$sdev)),])
        
        # get the other angles by adding 90
        theta1 <- c(theta1, theta1 + 90, theta1 + 180, theta1 + 270)
        
        # if the principle direction is in Q2, then the 4th theta (one that would be in Q1) is over 360
        # subtract 360 from it to get angles between 0-360 degrees
        if (theta1[1] > 90) {
            theta1[4] <- theta1[4] - 360
        }
        
        
        
        
        # get weighted center
        weightedCenter <- function(clust) {
            wcx <- weighted.mean(x = clust@coords[,1], 
                                 w = clust$FW_dis)
            
            wcy <- weighted.mean(x = clust@coords[,2], 
                                 w = clust$FW_dis)
            wc <- c(wcx, wcy)
            
            return(wc)
        } # close weightedCenter()
        
        wc <- weightedCenter(cluster)
        
        # get bounding box
        bb <- cluster@bbox
        
        # change rownames to remember which one is which
        rownames(bb) <- c("EW", "NS")
        
        
        
        # get the PD points for each theta
        coord_mat <- do.call(rbind, lapply(theta1, getPDpointCoords))
        rownames(coord_mat) <- round(theta1) # not super needed, just helps keep things straight
        
        # create SP object with these points
        # create dataframe to go with the points
        coord_df <- data.frame(OID = 1:4,
                               theta = round(theta1),
                               type = c("PD", "opp", "PD", "opp"),
                               PC_sdev = c(max(pc$sdev), min(pc$sdev), max(pc$sdev), min(pc$sdev)))
        # create sp object
        pd <- SpatialPointsDataFrame(coords = coord_mat, data = coord_df, proj4string = tract_outline@proj4string)
        
        # determine if we want to split using opp or PD line
        # this is determined by the distance between the two points, get split on shorter one
        pd$length <- 999
        
        pd$length[pd$type == "PD"] <- pointDistance(pd[1,], pd[3,], lonlat = FALSE)
        pd$length[pd$type == "opp"] <- pointDistance(pd[2,], pd[4,], lonlat = FALSE)
        
        # determine distance to weighted center
        pd$dist_to_wc <- pointDistance(wc, pd, lonlat = FALSE)
        
        # determine the split line (shortest line)
        pd$type <- as.character(pd$type)
        pd$type[pd$length == min(pd$length)] <- "split"
        
        # form splitting polygon
        
        # create bbox corners sp object
        bbox_df <- data.frame(OID = 1:4, 
                              theta = c(135, 225, 315, 45),
                              type = "bbox")
        bbox_coords <- matrix(data = c(bb["EW", "min"], bb["NS", "max"],  
                                       bb["EW", "min"], bb["NS", "min"], 
                                       bb["EW", "max"], bb["NS", "min"], 
                                       bb["EW", "max"], bb["NS", "max"]), ncol = 2, byrow = TRUE)
        bbox_sp <- SpatialPointsDataFrame(coords = bbox_coords, data = bbox_df, proj4string = tract_outline@proj4string)
        
        # a check to check the direction we moved the line
        move_direc <- c("none", "none")
        
        # split the cluster, check FW, move split line as needed until cluster is split in half (or close enough)
        repeat {
            
            
            # split the polygon using the pd and bbox_sp points
            split_poly <- createSplitPoly(pd, bbox_sp)
            
            # separate return- poly points holds the dataframe we need for this
            poly_points <- split_poly$poly_points
            split_poly <- split_poly$split_poly
            
            # split the cluster polygon 
            
            # get what points are in the split_poly
            cluster$split <- over(cluster, split_poly)
            
            # get the centers of the clusters
            wc1 <- weightedCenter(cluster[is.na(cluster$split) == TRUE,])
            wc2 <- weightedCenter(cluster[is.na(cluster$split) == FALSE,])
            
            check_df <- data.frame(x = c(wc1[1], wc2[1]),
                                   y = c(wc1[2], wc2[2]),
                                   FW_dis = c(sum(cluster$FW_dis[is.na(cluster$split) == TRUE]), 
                                              sum(cluster$FW_dis[is.na(cluster$split) == FALSE])))
            
            # the if we get to the point where we flipflop bt N/S or E/W, we need to raise X_pct
            X_pct <- ifelse(move_direc[2] == "flip", X_pct * 2, X_pct)
            
            # check the FW in each cluster, determine if it needs to be moved
            # if the difference between the target cluster and the FW in a cluster half is outside X_pct% of total FW
            if (abs(target_FW - sum(cluster$FW_dis[is.na(cluster$split) == TRUE])) > ((target_FW * 2) * X_pct)) {
                # move pd and restart repeat loop
                
                # if the split line gets moved back and forth repeatedly, the radius is too large
                # 
                
                
                # determine which direction to move the split_line
                # based on bbox points, if vertical, thetas add to 360
                if (sum(poly_points$theta[poly_points$type == "bbox"]) == 360) { # if bbox lines are vertical
                    # move the split along x axis
                    
                    # if the larger AD is east of the wc (has a higher x coord)
                    if (check_df$x[check_df$FW_dis == max(check_df$FW_dis)] > wc[1]) {
                        
                        
                        # add to split lines x coords
                        if(print_progress == TRUE) {
                            print(paste0("moving line east. move_direc[1]: ", move_direc[1]))}
                        
                        # determine if we can use usual radius of need to cut it in half
                        #radius2 <- ifelse(move_direc == "west", radius/3, radius)
                        if (move_direc[1] == "west") {
                            radius2 <- radius/3
                            
                            # change the second move direc so we know we had to move
                            move_direc[2] <- "flip"
                            
                        } else {
                            radius2 <- radius
                        }
                        
                        # change the coords of the split line
                        pd@coords[pd$type == "split", 1] <- pd@coords[pd$type == "split", 1] + radius2
                        move_direc[1] <- "east"
                        
                        
                    } else {
                        # subtract from split lines x coords
                        if(print_progress == TRUE) {
                            print(paste0("moving line west. move_direc[1]: ", move_direc[1]))}
                        
                        #radius2 <- ifelse(move_direc == "east", radius/3, radius)
                        if (move_direc[1] == "east") {
                            radius2 <- radius/3
                            
                            # change the second move direc so we know we had to move
                            move_direc[2] <- "lip"
                            
                        } else {
                            radius2 <- radius
                        }
                        
                        
                        pd@coords[pd$type == "split", 1] <- pd@coords[pd$type == "split", 1] - radius2
                        move_direc[1] <- "west"
                    } # close inner if/else
                    
                } else { # else, bbox lines are horizontal
                    # move split along y axis
                    
                    # if the larger AD is north of the wc (has a higher y value)
                    if (check_df$y[check_df$FW_dis == max(check_df$FW_dis)] > wc[2]) {
                        
                        # add to split lines y coords
                        if(print_progress == TRUE) {
                            print(paste0("moving line north. move_direc[1]: ", move_direc[1]))}
                        
                        #radius2 <- ifelse(move_direc == "south", radius/3, radius)
                        if (move_direc[1] == "south") {
                            radius2 <- radius/3
                            
                            # change the second move direc so we know we had to move
                            move_direc[2] <- "flip"
                            
                        } else {
                            radius2 <- radius
                        }
                        
                        
                        pd@coords[pd$type == "split", 2] <- pd@coords[pd$type == "split", 2] + radius2
                        move_direc[1] <- "north"
                    } else {
                        
                        # subtract from split lines y coords
                        if(print_progress == TRUE){
                            print(paste0("moving line south. move_direc[1]: ", move_direc[1]))}
                        
                        #radius2 <- ifelse(move_direc == "north", radius/3, radius)
                        if (move_direc[1] == "north") {
                            radius2 <- radius/3
                            # change the second move direc so we know we had to move
                            move_direc[2] <- "flip"
                        } else {
                            radius2 <- radius
                        }
                        
                        
                        pd@coords[pd$type == "split", 2] <- pd@coords[pd$type == "split", 2] - radius2
                        move_direc[1] <- "south"
                    } # close inner if/else
                    
                } # close if/else  deciding to move EW/NS
                
                
                next # restart repeat loop to split and check again
                
            } else { 
                # if clusters are even, stop the repeat and create the new clusters
                
                # # exit repeat loop to actually split cluster
                break } #close if/else
        } # close repeat
        
        
        # change AD_codes of the BUS cluster
        # vector of FIDs for is.na == TRUE and FALSE
        clust_1_FIDS <- cluster$FID[is.na(cluster$split) == TRUE]
        clust_2_FIDS <- cluster$FID[is.na(cluster$split) == FALSE]
        
        # In the global environment, change the BUS AD_codes
        BUS$AD_code[BUS$FID %in% clust_1_FIDS] <<- paste0(clust_code, "-1")
        BUS$AD_code[BUS$FID %in% clust_2_FIDS] <<- paste0(clust_code, "-2")
        
        cluster$AD_code[is.na(cluster$split) == TRUE] <- paste0(clust_code, "-1")
        cluster$AD_code[is.na(cluster$split) == FALSE] <- paste0(clust_code, "-2")
        
        # change FW & AD_code and AD location in main cluster
        # this is done in global environment w/ <<-
        AD$FW_dis[AD$AD_code == clust_code] <<- sum(cluster$FW_dis[cluster$AD_code == paste0(clust_code, "-1")])
        AD@coords[AD$AD_code == clust_code, 1] <<- weighted.mean(x = cluster@coords[cluster$AD_code == 
                                                                                        paste0(clust_code, "-1"),1],
                                                                 w = cluster$FW_dis[cluster$AD_code == paste0(clust_code, "-1")]) 
        AD@coords[AD$AD_code == clust_code, 2] <<- weighted.mean(x = cluster@coords[cluster$AD_code ==
                                                                                        paste0(clust_code, "-1"),2],
                                                                 w = cluster$FW_dis[cluster$AD_code == paste0(clust_code, "-1")])
        AD$AD_code[AD$AD_code == clust_code] <<- paste0(clust_code, "-1")
        
        
        # create a new AD for the new cluster
        # location doesnt matter, weighted centers are found while updating stats
        # create a copy of the AD that was split's data - this is a template for new ADs
        d <- AD@data[AD$AD_code == paste0(clust_code, "-1"),]
        d$AD_code <- paste0(clust_code, "-2") # change AD_code to new one
        
        
        # change coordinates
        d_coord <- AD[AD$AD_code == paste0(clust_code, "-1"),]@coords
        d_coord[,1] <- weighted.mean(x = cluster@coords[cluster$AD_code == paste0(clust_code, "-2"),1],
                                     w = cluster$FW_dis[cluster$AD_code == paste0(clust_code, "-2")])
        
        d_coord[,2] <- weighted.mean(x = cluster@coords[cluster$AD_code == paste0(clust_code, "-2"),2],
                                     w = cluster$FW_dis[cluster$AD_code == paste0(clust_code, "-2")])
        
        
        
        
        # calculate FW for this cluster
        d$FW_dis <- sum(cluster$FW_dis[cluster$AD_code == paste0(clust_code, "-2")])
        d$FW_gen <- sum(cluster$FW_gen[cluster$AD_code == paste0(clust_code, "-2")])
        
        # this AD to the AD pool - 
        AD <<- rbind(AD, SpatialPointsDataFrame(coords = d_coord,
                                                data = d,
                                                proj4string = crs(AD)))
        
        
        
        
        
        
        
    } # close splitCluster()
    
    
    
    
    # getAdjADs_dist() gets adjacent ADs for a given cluster based on the distance matrix only
    # it orders the dist_mat for a given cluster, then calculates the change in dist between each point
    # it then chooses the cutoff points if that change in distance is greater that the St Dev of the changes
    # only the ADs within that first cutoff is taken as adjacent
    # @param code - the AD_code to find adjacents for
    # @param km_res - a kmean result. This allows us to draw maps from a copy and not mess w/ the main AD/BUS pool
    # @param peak_value - used to determine what is considered a peak based on the distance difference plot
    #                   - this value is multipled by the standard dev of distance diff. 
    #                   - anything above sd_diff * peak_value is a large enough jump to be considered a peak
    #                   - points below the first peak are considered adjacent
    # returns - an sp object of adjacent ADs, ordered by FW
    getAdjADs_dist <- function(code, km_res, peak_value = peak_val) {
        
        
        ### function ####
        # calculates the dist_mat row for a given AD_code
        # @param AD_code - AD to get adjacents for
        # @param kmean_res - the ADs and BUSs
        getDistMatRow <- function(AD_code, kmean_res) {
            
            simpleClosePoints <- function(OG_code, T_code, km_res) {
                
                AD <- km_res$AD
                BUS <- km_res$BUS
                
                # get ADs
                OG_AD <- AD[AD$AD_code == OG_code,]
                T_AD <- AD[AD$AD_code == T_code,]
                
                # get BUS
                OG_BUS <- BUS[BUS$AD_code == OG_code,]
                T_BUS <- BUS[BUS$AD_code == T_code,]
                
                # point distance from OG_BUS to to T_AD
                OG_BUS$pdist <- pointDistance(OG_BUS, T_AD, lonlat = FALSE)
                
                # pnt dist from T_BUS to OG_AD
                T_BUS$pdist <- pointDistance(T_BUS, OG_AD, lonlat = FALSE)
                
                
                p1 <- tryCatch(
                    {
                        
                        # your code goes to try goes here
                        OG_BUS[OG_BUS$pdist == min(OG_BUS$pdist, na.rm = TRUE),]
                        
                    }, # close try part
                    error = function(cond) {
                        message("we hit an error 1")
                        message("error message: ")
                        message(cond)
                        
                        # choose a return value if theres an error
                        return(NA)
                        
                    } # close error part
                ) # close tryCatch()
                
                p <- tryCatch(
                    {
                        T_BUS[T_BUS$pdist == min(T_BUS$pdist, na.rm = TRUE),] 
                    }, #close try part
                    error = function(cond) {
                        message("hit an error 2")
                        message("error message: ")
                        message(cond)
                        
                        return(NA)
                    } # close error part
                ) # close tryCatch()
                
                #p1 <- OG_BUS[OG_BUS$pdist == min(OG_BUS$pdist, na.rm = TRUE),]
                #p <- T_BUS[T_BUS$pdist == min(T_BUS$pdist, na.rm = TRUE),]
                
                
                # pnt dist from two closest BUS points
                ret <- pointDistance(p1, p, lonlat = FALSE)
                #names(ret) <- OG_code
                # return the distance between the closest two points
                return(ret)
            } # close simpleClosePoints()
            
            # list of AD codes
            code_list <- as.character(kmean_res$AD$AD_code)
            
            
            # loop through list of AD codes,
            # find dist from closest BUS points to T_code (the code being updated for)
            # there will be a 0 in this vec. In a dist_mat, this 0 would be the diagonal
            dist_vec <- sapply(code_list, simpleClosePoints, T_code = AD_code, km_res = kmean_res)
            names(dist_vec) <- code_list
            return(dist_vec)
        } # close getDistMatRow()
        
        
        # get the sorted dist_mat row for the AD_code we need
        sorted <- sort(getDistMatRow(code, km_res), FALSE)
        
        # remove the 0 from the dist_vec (its been sorted to the front).
        # This is the diagonal on a distance matrix
        sorted <- sorted[2:length(sorted)]
        
        
        # make a df of the distance between each point
        diff_df <- data.frame(diff = diff(sorted),
                              node = 2:length(sorted))
        
        # find mean and standard dev
        mean_diff <- mean(diff_df$diff)
        sd_diff <- sd(diff_df$diff)
        
        # get points above standard dev * peak_value
        diff_df$above_SD <- FALSE
        diff_df$above_SD[diff_df$diff >= (sd_diff * peak_value)] <- TRUE
        
        # get the index of the first point above standard dev
        # the points with distances below the cutoff point this will be the adjacents
        # ex: cutoff = 7, then first 6 points (based on dist mat) will be adjacents - this is why we subtract one
        cutoff <- min(diff_df$node[diff_df$above_SD == TRUE], na.rm = TRUE)
        cutoff <- cutoff - 1
        
        # get the AD_codes that will be adjacent
        adj_ADs <- names(sorted[1:cutoff])
        
        # get the actual ADs that will be adjacent
        # get ADs that match
        adj_ADs <- AD[AD$AD_code %in% adj_ADs,]
        
        # order by FW
        adj_ADs <- adj_ADs[order(adj_ADs$FW_dis),]
        if(print_progress == TRUE){
            print(paste0("num Adjacents: ", nrow(adj_ADs)))}
        
        
        return(adj_ADs)
        
    } #close getAdjADs_dist()
    
    # reduceLowestAD shares FW from the lowest AD to its adjacent ADs.
    # @param adj_AD one of the adjacent ADs - sp object
    # @param lowest AD - sp object
    # returns the lowest AD sp object, but with different FW
    # it also changes the AD and BUS points from the global environment
    reduceLowestAD <- function(sing_adj_AD, lowest_AD) {
        
        # weighted_center finds the weighted center of a given cluster
        # @param code - the AD code to change
        # no return, changes AD@coords in the global env
        weighted_center <- function(code) {
            
            # find the weighted centroid of the new business cluster
            # the x coordinate for the AD eveything got added to = weighted mean of its cluster's x coords
            AD@coords[AD$AD_code == code, 1] <<- weighted.mean(x = BUS@coords[BUS$AD_code == code, 1],
                                                               w = BUS$FW_dis[BUS$AD_code == code])
            
            # the y coordinate for the AD everything got added to = weighted mean of its clusters y coords
            AD@coords[AD$AD_code == code, 2] <<- weighted.mean(x = BUS@coords[BUS$AD_code == code, 2],
                                                               w = BUS$FW_dis[BUS$AD_code == code])
        } #close weighted_center()
        
        # if the lowest and the adjacent can be merged, do that
        if (lowest_AD$FW_dis + sing_adj_AD$FW_dis <= AD_cap) {
            
            # if the lowest filled + its nearest neighbor is less than AD_cap,
            # merge the lowest filled to the NN
            
            # in the main AD pool, the NN_AD's FW_dis is lowest_ADs FW + its own FW
            # use <<- to access variables in the outside environment (the main adj_bus_corrector function)
            AD$FW_dis[AD$AD_code == sing_adj_AD$AD_code] <<- sing_adj_AD$FW_dis + lowest_AD$FW_dis
            
            # remove the FW from lowest AD
            lowest_AD$FW_dis <- lowest_AD$FW_dis - lowest_AD$FW_dis
            
            
            # give the business points that have been merged a new AD code.
            # This is just the AD_code of the sing_adj_AD in question
            new_AD_code <- sing_adj_AD$AD_code[1]
            
            ## the lowest ADs BUS points
            BUS$AD_code[BUS$AD_code %in% lowest_AD$AD_code] <<- new_AD_code
            
            # drop the lowest_AD from the main AD pool
            AD <<- AD[AD$AD_code != lowest_AD$AD_code,]
            
            # lowest_AD is gone
            
            # if the two added together would be over the AD_cap, need to separate the lowest_ADs BUS
        } else if (lowest_AD$FW_dis + sing_adj_AD$FW_dis > AD_cap) {
            
            # run FWd function.
            # The center point is the NN_AD
            # FW being added is the BUS points associated with the lowest filled AD
            FWd <- FWd_func_one_point(point = sing_adj_AD,
                                      all_ADs = BUS[BUS$AD_code == lowest_AD$AD_code,])
            
            
            # the FWd function is meant for ADs to ADs.
            # When used this way, it doesn't count the FW already in the sing_adj_AD
            # therefore, need to add that FW back in manually
            FWd$FWd_plus <- FWd$FWd + sing_adj_AD$FW_dis
            
            
            #### problem is when FWd_plus is over AD_cap
            #### then when we go to find the xint using max, there's nothing in the subset
            # if the min FWd_plus value is over AD_cap, go to next adjacent AD
            if (min(FWd$FWd_plus) > AD_cap) {
                if(print_progress == TRUE){
                    print(paste0("lowest filled adjacent cannot accept FW, leaving one underfilled @ ",
                                 round(lowest_AD$FW_dis)))}
                
                # break the while loop to leave underfilled
                return(lowest_AD) } #close if
            
            
            # determine x/y axis for FWd
            # find the distance (x axis) where FWd (cumulative FW) crosses AD_cap
            FWd_xint <- max(FWd$kdist[FWd$FWd_plus <= AD_cap])
            
            # give the sing_adj_AD the cumulative FW. (do this in main AD pool)
            # <<- to access "global" variable
            AD$FW_dis[AD$AD_code == sing_adj_AD$AD_code] <<- FWd$FWd_plus[FWd$kdist == FWd_xint]
            
            # remove the FW from lowest_AD that was just given to the NN
            lowest_AD$FW_dis <- lowest_AD$FW_dis - FWd$FWd[FWd$kdist == FWd_xint]
            
            # change AD_codes for the BUS points that have been given to the sing_adj_AD
            new_AD_code <- sing_adj_AD$AD_code[1]
            
            ## change AD_code for BUS points associated with lowest_AD that are given to the sing_adj_AD
            ### find dist from lowest_AD's BUS to sing_adj_AD
            d <- raster::pointDistance(sing_adj_AD, BUS[BUS$AD_code == lowest_AD$AD_code,], lonlat = FALSE)
            
            ### attach this distance to the BUS df
            BUS$pt_dist[BUS$AD_code %in% lowest_AD$AD_code] <<- d
            
            ### change AD_codes for points w/in FWd_xint distance to the sing_adj_AD code
            ### BUS where code is same as lowest AD and within the distance to add to the NN_AD
            BUS$AD_code[BUS$AD_code %in% lowest_AD$AD_code & BUS$pt_dist <= FWd_xint] <<- new_AD_code
            
            
            # lowest_AD still exists, but has way less FW. Need to give those businesses to other sing_adj_ADs
            
            
            # change location of lowest_AD@coords
            weighted_center(lowest_AD$AD_code)
            
            
            
            # change the loop control variable in the global env
            # if the amount of FW moved is more than 0
            if (round(FWd$FWd[FWd$kdist == FWd_xint]) > 0) {
                moved_FW <<- TRUE
            }
            
        } #close if/else
        
        
        # change location of AD that has been merged to - this will happen in both cases
        weighted_center(sing_adj_AD$AD_code)
        
        
        
        return(lowest_AD)
    } #close reduceLowestAD
    
    
    
    # reduceOverfilledAD() reduces the overfilled AD back down to AD_cap by giving FW to adjacents
    # @param sing_adj_AD: a single AD that is adjacent. sp object
    # @param of_AD: the overfilled AD. sp object
    reduceOverfilledAD <- function(sing_adj_AD, of_AD) {
        
        # weighted_center finds the weighted center of a given cluster
        # @param code - the AD code to change
        # no return, changes AD@coords in the global env
        weighted_center <- function(code) {
            
            # find the weighted centroid of the new business cluster
            # the x coordinate for the AD eveything got added to = weighted mean of its cluster's x coords
            AD@coords[AD$AD_code == code, 1] <<- weighted.mean(x = BUS@coords[BUS$AD_code == code, 1],
                                                               w = BUS$FW_dis[BUS$AD_code == code])
            
            # the y coordinate for the AD everything got added to = weighted mean of its clusters y coords
            AD@coords[AD$AD_code == code, 2] <<- weighted.mean(x = BUS@coords[BUS$AD_code == code, 2],
                                                               w = BUS$FW_dis[BUS$AD_code == code])
        } #close weighted_center()
        
        of_code <- of_AD$AD_code
        
        # run FWd function from the adjacent to grab of_code's FW
        FWd <- FWd_func_one_point(point = sing_adj_AD, all_ADs = BUS[BUS$AD_code == of_code,])
        
        # add FW already in adjacent AD to the FWd df
        FWd$FWd_plus <- FWd$FWd + sing_adj_AD$FW_dis
        
        # if the adjacent cannot accept FW, try next one
        if (min(FWd$FWd_plus) > AD_cap) {
            
            # move on to next adjacent
            return(of_AD) } #close if
        
        # determine x/y axis for FWd
        # if the amount of FW to get overfilled back to AD_cap
        # is smaller than the amount of FW the adjacent AD can accept:
        to_remove <- of_AD$FW_dis - AD_cap
        if (to_remove < (AD_cap - sing_adj_AD$FW_dis)) {
            # remove all extra FW,
            FWd_xint <- max(FWd$kdist[FWd$FWd <= to_remove])
            
        } else {
            # remove as much FW as adjacent can take
            FWd_xint <- max(FWd$kdist[FWd$FWd_plus <= AD_cap])
            
        } # close ifelse
        
        
        # give the adj_AD the cumulative FW. (do this in main AD pool)
        AD$FW_dis[AD$AD_code == sing_adj_AD$AD_code] <<- FWd$FWd_plus[FWd$kdist == FWd_xint]
        
        # remove the FW from of_AD that was just given to the NN (in main AD pool)
        if(print_progress == TRUE){
            print(paste0("giving ", sing_adj_AD$AD_code, " ", FWd$FWd[FWd$kdist == FWd_xint], " FW"))}
        
        AD$FW_dis[AD$AD_code == of_AD$AD_code] <<- AD$FW_dis[AD$AD_code == of_AD$AD_code] - FWd$FWd[FWd$kdist == FWd_xint]
        
        # remove the FW from of_AD that was just given to NN (in the local sp object)
        of_AD$FW_dis <- of_AD$FW_dis - FWd$FWd[FWd$kdist == FWd_xint]
        
        # change AD_codes for the BUS points that have been given to the kth of_adj_AD
        new_AD_code <- sing_adj_AD$AD_code
        
        ## change AD_code for BUS points associated with lowest_AD that are given to the of_adj_AD[it]
        
        ### find dist from overfilled_AD's BUS to of_adj_AD[it]
        d <- raster::pointDistance(sing_adj_AD, BUS[BUS$AD_code == of_AD$AD_code,], lonlat = FALSE)
        
        ### attach this distance to the BUS df
        BUS$pt_dist[BUS$AD_code %in% of_AD$AD_code] <<- d
        
        ### change AD_codes for points w/in FWd_xint distance to the of_adj_AD[it] code
        ### BUS where code is same as lowest AD and within the distance to add to the NN_AD
        BUS$AD_code[BUS$AD_code == of_AD$AD_code & BUS$pt_dist <= FWd_xint] <<- new_AD_code
        
        
        # change AD location for of_AD
        weighted_center(of_AD$AD_code)
        
        # change AD location for adjacent
        weighted_center(sing_adj_AD$AD_code)
        
        
        
        
        
        # if the amount of FW moved is more than 0, change loop control var
        if (FWd$FWd[FWd$kdist == FWd_xint] > 0) {
            moved_FW <<- TRUE
        }
        
        # real changes made in global env BUT
        
        return(of_AD)
        
    } # close reduceOverfilled()
    
    
    
    ########### main function #######################
    AD <- kmean_result$AD
    BUS <- kmean_result$BUS
    
    BUS$AD_code <- as.character(BUS$AD_code)
    AD$AD_code <- as.character(AD$AD_code)
    
    AD_cap <- AD$AD_cap[1]
    
    rep_num <- 1 # for drawing maps
    
    
    ################## Overfilled Clusters####################
    
    # check if there are overfilled clusters
    if (max(AD$FW_dis) > AD_cap) {
        message("Fixing overfilled clusters")
        
        
        # pull the overfilled ADs
        of_ADs <- AD[AD$FW_dis > AD_cap,]
        if(print_progress == TRUE){
            print(paste0(nrow(of_ADs), " overfilled"))}
        
        # loop through overfilled ADs
        for (i in 1:nrow(of_ADs)) {
            
            of_code <- of_ADs$AD_code[i]
            
            
            # if FW can be split by 2 and still be above 70% AD_cap:
            if (AD$FW_dis[AD$AD_code == of_code] / 2 >= AD_cap * break_pct) {
                
                splitCluster(clust_code = of_code, kmean_res = list(AD = AD, BUS = BUS), radius = radius)
                if(print_progress == TRUE) {
                    print(paste0("num overfilled: ", nrow(AD[AD$FW_dis > AD_cap,])))}
                
                if (draw_map == TRUE) {
                    p <- clust_plot(list(AD = AD, BUS = BUS))
                    ggsave(paste0("C:/Users/Lauren Mabe/Box Sync/Food Waste Mapping/R_Stuff/Model_3_github/Maps/GIF_folder/M3_",
                                  AD_cap, "_", rep_num, ".png"))
                    
                    rep_num <- rep_num + 1
                } #close draw_map if
                
            } else {
                # If not nicely divisible, try to give away points to adjacents
                
                
                # get adjacent ADs for the overfilled AD.
                of_adj_ADs <- getAdjADs_dist(of_code, list(AD=AD, BUS=BUS))
                
                
                # a loop control variable
                # if the sing_adj_AD cant take FW, it will return the of_AD 
                # AND it will flip a switch in the global env to show that no FW moved
                # this will allow the loop to keep moving onto the next adj_AD changing the dist_mat or adj_ADs list
                moved_FW <- FALSE 
                
                # this whole thing needs to be run in a loop
                # since it needs to check if lowest_AD is gone after each adjacent
                k <- 1
                while (k <= nrow(of_adj_ADs)) {
                    
                    of_ret <- reduceOverfilledAD(sing_adj_AD = of_adj_ADs[k,], of_AD = AD[AD$AD_code == of_code,])
                    
                    # update i
                    k <- k + 1
                    
                    
                    # if the of_AD gets down to AD_cap, don't check the other adjacent ADs
                    if (of_ret$FW_dis <= AD_cap) {
                        # break out of while loop (k) to not check other adj_ADs, and move on to next of_AD
                        break
                        
                        # else, if the moved FW switch goes to TRUE (FW has been moved)
                        # get new list of adj_ADs, go to top of while loop (k) with new list
                    } else if (moved_FW == TRUE) {
                        
                        if (draw_map == TRUE) {
                            p <- clust_plot(list(AD = AD, BUS = BUS))
                            ggsave(paste0("C:/Users/Lauren Mabe/Box Sync/Food Waste Mapping/R_Stuff/Model_3_github/Maps/GIF_folder/M3_",
                                          AD_cap, "_", rep_num, ".png"))
                            
                            rep_num <- rep_num + 1
                        } #close draw_map if
                        
                        # get new list of adj_ADs
                        of_adj_ADs <- getAdjADs_dist(of_code, list(AD=AD, BUS=BUS))
                        
                        
                        
                        # reset moved_FW
                        moved_FW <- FALSE
                        
                        # reset i to run reduceOverfilled w/ new adjacents
                        k <- 1
                    } # close if/else
                    
                } # close while loop running reduceOverfilled (k)
                
                #move to next of_code and try to split
                next
                
            } # close ifelse that decides to splitCluster or reduceOverfilled
            
        } # close for loop (i) looping through of_ADs
        
    } # close if for final overfilled check
    
    ########### Underfilled Clusters ############
    # a switch to let us skip choose to go straight to reduceOverfilled() 
    # even if the overfilled AD can be split
    # this is only needed if an AD's only adj_AD is a sister AD
    skip_split <- FALSE
    
    
    message("Fixing underfilled clusters")
    if(print_progress == TRUE){
        print(paste0("num underfilled: ", nrow(AD[AD$FW_dis < AD_cap * break_pct,])))}
    repeat {
        # find lowest filled AD
        lowest_AD <- AD[AD$FW_dis == min(AD$FW_dis),]
        if(print_progress == TRUE){
            print(paste0("lowest_AD: ", lowest_AD$AD_code, ", FW: ", round(lowest_AD$FW_dis)))}
        
        
        
        # check if lowest AD is above 70% AD_cap
        if (lowest_AD$FW_dis > AD_cap * break_pct) {
            message(paste0("All ADs over ", break_pct*100, "% AD_cap"))
            break # break the repeat loop
        }
        
        
        # get adjacents to the lowest_AD
        adj_ADs <- getAdjADs_dist(lowest_AD$AD_code, list(AD=AD,BUS=BUS))
        
        
        
        # a loop control variable
        # if the sing_adj_AD cant take FW, it will return lowest_AD 
        # AND it will flip a switch in the global env to show that no FW moved
        # this will allow the loop to keep moving onto the next adj_AD changing the dist_mat or adj_ADs list
        moved_FW <- FALSE 
        
        
        # Reduce lowest AD
        # this whole thing needs to be run in a loop
        # since it needs to check if lowest_AD is gone after each adjacent
        i <- 1
        while (i <= nrow(adj_ADs)) {
            #for (i in 1:nrow(adj_ADs)) {
            
            
            lowest_AD <- reduceLowestAD(sing_adj_AD = adj_ADs[i,], lowest_AD = lowest_AD)
            
            # update i
            i <- i + 1
            
            if (draw_map == TRUE) {
                p <- clust_plot(list(AD = AD, BUS = BUS))
                ggsave(paste0("C:/Users/Lauren Mabe/Box Sync/Food Waste Mapping/R_Stuff/Model_3_github/Maps/GIF_folder/M3_",
                              AD_cap, "_", rep_num, ".png"))
                
                rep_num <- rep_num + 1
            } #close draw_map if
            
            # if the lowest_AD gets down to 0, don't check the other adjacent ADs
            if (lowest_AD$FW_dis == 0) {
                # break out of for loop to not check other ADs, update dist_mat and move on to next lowest
                break
                
                # also, if the moved FW switch goes to TRUE (FW has been moved)
                # get new list of adj_ADs, restart loop with new list
            } else if (moved_FW == TRUE) {
                
                # get new list of adj_ADs
                adj_ADs <- getAdjADs_dist(lowest_AD$AD_code, list(AD=AD,BUS=BUS))
                
                
                # reset moved_FW
                moved_FW <- FALSE
                
                # reset i to restart loop with new list
                i <- 1
            } # close if/else
            
        } # close while loop running reduceLowestAD
        
        # if the lowest_AD has been completley gotten rid of, update adjacencies and restart the repeat loop
        if (lowest_AD$FW_dis == 0) {
            
            
            next # next on the repeat loop to next lowest AD
            
            # else - the lowest AD has not gone to 0
        } else {
            
            
            # find the lowest filled adjacent AD to give FW to
            # get the lowest_filled adj_AD's AD_code
            donate_to <- adj_ADs$AD_code[adj_ADs$FW_dis == min(adj_ADs$FW_dis)]
            
            # if the lowest filled AD is from the same "parent", choose the next one
            # donate_to <- ifelse(gsub(".*x\\s*|-.*", "", donate_to) == gsub(".*x\\s*|-.*", "", lowest_AD$AD_code),
            #                     yes = adj_ADs$AD_code[adj_ADs$FW_dis == sort(adj_ADs$FW_dis, TRUE)[2]],
            #                     no = donate_to)
            
            # if tests if the AD we are donating to is a sister AD
            if(print_progress == TRUE){
                print(paste0("lowest_AD: ", lowest_AD$AD_code,
                             " donate_to: ", donate_to))
                print(paste0("grep lowest_AD: ", gsub(".*x\\s*|-.*", "", lowest_AD$AD_code),
                             ", grep donate_to: ", gsub(".*x\\s*|-.*", "", donate_to)))
            } # close print_progress if
            
            
            # Commenting this out bc it would cause problems with 5k
            # basically it would get stuck in the nchar12 loop,
            # and it couldnt get out bc it would get caught here
            # # detect if sisters, 
            # if (gsub(".*x\\s*|-.*", "", donate_to) == gsub(".*x\\s*|-.*", "", lowest_AD$AD_code)) {
            #     print("SISTERS")
            #     # if it is a sister AD, this one checks how many adjacents there are
            #     if (nrow(adj_ADs) == 1) {
            #         # if theres only one adjacent, and its a sister,
            #         # let them merge, but force the overfilled to reduceOverfilled() by flipping switch
            #         skip_split <- TRUE
            #         
            #     } else {# close innner if
            #         
            #         # if first adjacent is a sister, and there are multiple adj_ADs,
            #         # then choose the second lowest filled adjacent, so we dont give to sister
            #         
            #         donate_to <- adj_ADs$AD_code[adj_ADs$FW_dis == sort(adj_ADs$FW_dis, FALSE)[2]]
            #         print(paste0("sisters not only neighbors, new donate to: ", donate_to))
            #       
            #     } # close inner if/else
            # } # close if that checks if sister
            
            # or if AD_code is too long
            if (nchar(lowest_AD$AD_code) > 12) {
                if(print_progress == TRUE) {
                    print("nchar > 12")}
                # if it is a sister AD, this one checks how many adjacents there are
                if (nrow(adj_ADs) == 1) {
                    # if theres only one adjacent, and its a sister,
                    # let them merge, but force the overfilled to reduceOverfilled() by flipping switch
                    skip_split <- TRUE
                    
                } else {# close innner if
                    
                    # if first adjacent is a sister, and there are multiple adj_ADs,
                    # then choose the second lowest filled adjacent, so we dont give to sister
                    
                    donate_to <- adj_ADs$AD_code[adj_ADs$FW_dis == sort(adj_ADs$FW_dis, FALSE)[2]]
                    if(print_progress == TRUE){
                        print(paste0("not only neighbors, new donate to: ", donate_to))}
                    
                } # close inner if/else
            } # close if that checks if sister
            
            
            # give the rest of lowest_ADs FW to the lowest filled adj_AD (do this in main AD pool)
            AD$FW_dis[AD$AD_code == donate_to] <- AD$FW_dis[AD$AD_code == donate_to] + lowest_AD$FW_dis
            lowest_AD$FW_dis <- lowest_AD$FW_dis - lowest_AD$FW_dis
            
            
            # give lowest_ADs BUS points to closest adj_AD
            BUS$AD_code[BUS$AD_code == lowest_AD$AD_code] <- donate_to
            
            if (draw_map == TRUE) {
                p <- clust_plot(list(AD = AD, BUS = BUS))
                ggsave(paste0("C:/Users/Lauren Mabe/Box Sync/Food Waste Mapping/R_Stuff/Model_3_github/Maps/GIF_folder/M3_",
                              AD_cap, "_", rep_num, ".png"))
                
                rep_num <- rep_num + 1
            } #close draw_map if
            
            
            
            # remove lowest_AD from AD
            AD <- AD[AD$AD_code != lowest_AD$AD_code,]
            
            
            # if the overfilled AD can be split in half and be above break_pct (70%) full, then do that
            if ((AD$FW_dis[AD$AD_code == donate_to] / 2) >= (AD_cap * break_pct) && skip_split == FALSE) {
                
                
                #BUS_cluster <- BUS[BUS$AD_code == donate_to,]
                # PD_split has no return value
                if(print_progress == TRUE) {
                    print(paste0("splitting ", donate_to))}
                
                splitCluster(clust_code = donate_to, kmean_res = list(AD = AD, BUS = BUS), radius = radius)
                
                if (draw_map == TRUE) {
                    p <- clust_plot(list(AD = AD, BUS = BUS))
                    ggsave(paste0("C:/Users/Lauren Mabe/Box Sync/Food Waste Mapping/R_Stuff/Model_3_github/Maps/GIF_folder/M3_",
                                  AD_cap, "_", rep_num, ".png"))
                    
                    rep_num <- rep_num + 1
                } #close draw_map if
                
                next # next on repeat loop - move to nex lowest AD
                
            } else {
                
                #message(paste0(donate_to, " overfilled, giving FW to neighbors"))
                if (skip_split == TRUE) {
                    message("FORCING REDUCE OVERFILLED()")
                }
                # get adjacent ADs for the overfilled AD.
                # This WILL include clusters adjacent to points that were once the lowest_AD
                # because the adj matrix has been updated
                of_adj_ADs <- getAdjADs_dist(donate_to, list(AD=AD,BUS=BUS))
                
                # a loop control variable
                # if the sing_adj_AD cant take FW, it will return the of_AD 
                # AND it will flip a switch in the global env to show that no FW moved
                # this will allow the loop to keep moving onto the next adj_AD changing the dist_mat or adj_ADs list
                moved_FW <- FALSE 
                
                # dummy sp object to feed into reduceOverfilledAD
                donate_to_AD <- AD[AD$AD_code == donate_to,]
                
                # this whole thing needs to be run in a loop
                # since it needs to check if lowest_AD is gone after each adjacent
                k <- 1
                while (k <= nrow(of_adj_ADs)) {
                    if(print_progress == TRUE) {
                        print("reduce overfilled")}
                    
                    donate_to_AD <- reduceOverfilledAD(sing_adj_AD = of_adj_ADs[k,], of_AD = donate_to_AD)
                    
                    # update i
                    k <- k + 1
                    
                    
                    
                    # if the of_AD gets down to AD_cap, don't check the other adjacent ADs
                    if (donate_to_AD$FW_dis <= AD_cap) {
                        # break out of while loop (k) to not check other adj_ADs, and move to next of_AD
                        break
                        
                        # else, if the moved FW switch goes to TRUE (FW has been moved)
                        # get new list of adj_ADs, restart while loop (k) with new list
                    } else if (moved_FW == TRUE) {
                        
                        if (draw_map == TRUE) {
                            p <- clust_plot(list(AD = AD, BUS = BUS))
                            ggsave(paste0("C:/Users/Lauren Mabe/Box Sync/Food Waste Mapping/R_Stuff/Model_3_github/Maps/GIF_folder/M3_",
                                          AD_cap, "_", rep_num, ".png"))
                            
                            rep_num <- rep_num + 1
                        } #close draw_map if
                        
                        # get new list of adj_ADs
                        of_adj_ADs <- getAdjADs_dist(donate_to, list(AD=AD,BUS=BUS))
                        
                        # reset moved_FW
                        moved_FW <- FALSE
                        
                        # reset i to run reduceOverfilled w/ new adjacents
                        k <- 1
                    } # close if/else
                    
                } # close while loop running reduceOverfilled (k)
                
                # reset skip_split
                skip_split <- FALSE
                #message("moving to next lowest")
                next # next on repeat loop - move to next lowest AD
                
            } # close if else to get the overfilled back down
            
        } # close if else after reduce lowest has been run
        
    } # close main repeat for underfilled clusters
    
    ####### Final Overfilled Check #####
    
    # check if there are overfilled clusters
    if (max(AD$FW_dis) > AD_cap) {
        message("Final Overfilled Check")
        
        # want to try do the final overfilled check 3 times
        for (rep in 1:3) {
            if(print_progress == TRUE){
                print(paste0("Round ", rep))}
            
            # pull the overfilled ADs
            of_ADs <- AD[AD$FW_dis > AD_cap,]
            
            if(print_progress == TRUE) {
                print(paste0(nrow(of_ADs), " overfilled"))
                print(paste0(nrow(AD[AD$FW_dis < (AD_cap * 0.7),]), " under"))
            }#close print_progress if
            
            if (nrow(of_ADs) == 0) {
                break } # if none overfilled, break the rep for loop
            
            for (i in 1:nrow(of_ADs)) {
                
                of_code <- of_ADs$AD_code[i]
                
                # if FW can be split by 2 and still be above 80% AD_cap:
                # want it to be 75% so ADs do not get left underfilled if 
                if (AD$FW_dis[AD$AD_code == of_code] / 2 >= AD_cap * (break_pct + 0.05)) {
                    
                    
                    splitCluster(clust_code = of_code, kmean_res = list(AD = AD, BUS = BUS), radius = radius)
                    
                    if (draw_map == TRUE) {
                        p <- clust_plot(list(AD = AD, BUS = BUS))
                        ggsave(paste0("C:/Users/Lauren Mabe/Box Sync/Food Waste Mapping/R_Stuff/Model_3_github/Maps/GIF_folder/M3_",
                                      AD_cap, "_", rep_num, ".png"))
                        
                        rep_num <- rep_num + 1
                    } #close draw_map if
                    
                } else {
                    # If not nicely divisible, try to give away points to adjacents
                    
                    
                    # get adjacent ADs for the overfilled AD.
                    of_adj_ADs <- getAdjADs_dist(of_code, list(AD=AD,BUS=BUS))
                    
                    # a loop control variable
                    # if the sing_adj_AD cant take FW, it will return the of_AD 
                    # AND it will flip a switch in the global env to show that no FW moved
                    # this will allow the loop to keep moving onto the next adj_AD changing the dist_mat or adj_ADs list
                    moved_FW <- FALSE 
                    
                    # this whole thing needs to be run in a loop
                    # since it needs to check if lowest_AD is gone after each adjacent
                    k <- 1
                    while (k <= nrow(of_adj_ADs)) {
                        
                        
                        of_ret <- reduceOverfilledAD(sing_adj_AD = of_adj_ADs[k,], of_AD = AD[AD$AD_code == of_code,])
                        
                        # update i
                        k <- k + 1
                        
                        
                        
                        # if the of_AD gets down to AD_cap, don't check the other adjacent ADs
                        if (of_ret$FW_dis <= AD_cap) {
                            # break out of while loop (k) to not check other adj_ADs, move to next of_AD
                            break
                            
                            # else, if the moved FW switch goes to TRUE (FW has been moved)
                            # get new list of adj_ADs, restart while loop (k) with new list
                        } else if (moved_FW == TRUE) {
                            
                            if (draw_map == TRUE) {
                                p <- clust_plot(list(AD = AD, BUS = BUS))
                                ggsave(paste0("C:/Users/Lauren Mabe/Box Sync/Food Waste Mapping/R_Stuff/Model_3_github/Maps/GIF_folder/M3_",
                                              AD_cap, "_", rep_num, ".png"))
                                
                                rep_num <- rep_num + 1
                            } #close draw_map if
                            
                            # get new list of adj_ADs
                            of_adj_ADs <- getAdjADs_dist(of_code, list(AD=AD,BUS=BUS))
                            
                            
                            # reset moved_FW
                            moved_FW <- FALSE
                            
                            # reset i to run reduceOverfilled w/ new adjacents
                            k <- 1
                        } # close if/else
                        
                    } # close while loop running reduceOverfilled (k)
                    
                    #move to next of_code and try to split
                    next
                    
                } # close ifelse that decides if running splitCluster or reduceOverfilled
                
            } # close for loop looping through of_ADs (i)
        } # close rep for loop that makes this run 3 times
        
    } # close if for final overfilled check
    
    
    
    
    
    
    if (draw_map == TRUE) {
        p <- clust_plot(list(AD = AD, BUS = BUS))
        ggsave(paste0("C:/Users/Lauren Mabe/Box Sync/Food Waste Mapping/R_Stuff/Model_3_github/Maps/GIF_folder/M3_",
                      AD_cap, "_", rep_num, ".png"))
        
        rep_num <- rep_num + 1
    } #close draw_map if
    
    message("Done!")
    return(list(AD = AD, BUS= BUS))
    
} # close model 3