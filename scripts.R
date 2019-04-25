require(oro.dicom)
require(oro.nifti)
require(mritc)
library(scales)
library(RNifti)
library(neurobase)




tal2mni <- function(inpoints){
  # inpoints - coordinates, with 
  
  if (dim(inpoints)[2] != 3) stop("Input should be N*3 matrix")
  inpoints <- t(inpoints)
  
  # Transformation matrices
  M2T.rotation <- matrix(c(1,0,0,0,
                           0,0.9988,-0.05,0,
                           0, 0.05, 0.9988, 0,
                           0,0,0,1),
                         nrow = 4,
                         ncol = 4)
  
  M2T.upZ <- matrix(c(0.99,0,0,0,
                      0,0.97,0,0,
                      0, 0, 0.92, 0,
                      0,0,0,1),
                    nrow = 4,
                    ncol = 4)
  
  M2T.downZ <- matrix(c(0.99,0,0,0,
                      0,0.97,0,0,
                      0, 0, 0.84, 0,
                      0,0,0,1),
                    nrow = 4,
                    ncol = 4)
  

  inpoints <- rbind(inpoints, matrix(1, 1, dim(inpoints)[2]))
  
  tmp <- inpoints[3,] < 0
  
  if( sum(tmp) == 0){
    # No below AC
    inpoints[, !tmp] <- solve((M2T.rotation %*% M2T.upZ), inpoints[, !tmp])
  }else{
    inpoints[, tmp] <- solve((M2T.rotation %*% M2T.downZ), inpoints[, tmp])
    inpoints[, !tmp] <- solve((M2T.rotation %*% M2T.upZ), inpoints[, !tmp])
  }

  
  outpoints <- inpoints[1:3,];

  round(t(outpoints))
  
}



identify_shenROI <- function(coord, template_mat, is.tal = FALSE){
  
  if (dim(coord)[2] != 3){
    stop("MNI corrdinates should be 3 dimensional")
  }
  
  if(nrow(coord) == 0){
    stop("There's no coordinates in the input")
  }
  
  if (is.tal){
    coord <- tal2mni(coord)
  }else{
    coord <- round(coord)
  }
  n_MNI_coord <- nrow(coord)
  
  shen_ID <- c()
  res <- list()
  
  for(i in 1:n_MNI_coord){
    co <- coord[i,]
    x <- co[1]
    y <- co[2]
    z <- co[3]
    
    trans_xyz <- RNifti::worldToVoxel(c(x,y,z), image = template_mat)
    
    shen_ID <- c(shen_ID, template_mat[trans_xyz[1], trans_xyz[2], trans_xyz[3]])
  }
  
  ## Mask
  # if (length(shen_ID) == 1)
  #   masks = template_mat == shen_ID
  # else{
  #   n_ID <- length(shen_ID)
  #   masks = template_mat == shen_ID[1]
  #   for(id in 2:n_ID){
  #     masks = masks | template_mat == id
  #   }
  # }
  
  res$shen_ID <- shen_ID
  #res$masks <- masks
  
  res
  
}


demo <- function(){
  Money_network <- t(matrix(c(4.9,32.5,13.7,
                            10.5,10.8,8.8,
                            10.1,-54.3,10.3,
                            28.4,-36,-1.4,
                            32.4,-18.3,-11,
                            -17.2,-36.5,1,
                            16.6,-40.7,-1.3),
                          3, 7))
  
  Time_network <- t(matrix(c(-3.3,30.5,40.3,
                             -2.8,33.3,9.4,
                             3.2,31.2,1.3,
                             -20.3,15.4,48.4,
                             -35.7,8,42.1,
                             -48.5,11.7,22.9,
                             -34.8,43.0,7.9,
                             -45.2,-55,37.8),3,7))
  
  Choice_process_network <- t(matrix(c(1.3,20.6,34.6,
                                       -30.5,17.7,2.5,
                                       33.8,16.4,1.6,
                                       22.6,13.3,50.0,
                                       36.9,4.8,49.4,
                                       -41.6,21.3,32.1,
                                       40.6,26.8,28.8,
                                       -43.5,5.8,30.7,
                                       46.2,4.4,23.7,
                                       -32.3,47.8,8.1,
                                       30.0,51.4,7.7,
                                       -35.3,-57.7,40.1,
                                       38.4,-61.1,38.3),3,13))
  
  networks <- list()
  networks$Money <- Money_network
  networks$Time <- Time_network
  networks$Choice <- Choice_process_network
  
  networks
}


demo_get_networks <- function(choice, paper_network){
  if(choice == "money"){
    sel <- paper_network$Money
  }else if(choice == "time"){
    sel <- paper_network$Time
  }else{
    sel <- paper_network$Choice
  }
  
  return(sel)
}


get_MNI_coords <- function(templates){
  
  n_roi <- length(unique(c(templates))) - 1
  MNI <- matrix(NA, n_roi, 3)
  for (roi in 1:n_roi){
    print(roi)
    # Find index
    area_index <- which(templates == roi, arr.ind = T) # This will be a n*3 matrix
    
    # Find the mean
    mean_vox <- apply(area_index, 2, mean)
    
    MNI[roi, ] <- voxelToWorld(round(mean_vox), templates)
  }
  
  MNI
  
}

