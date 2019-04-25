#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyFiles)
require(oro.dicom)
require(oro.nifti)
require(mritc)
library(scales)
library(RNifti)
library(neurobase)
source("./scripts.R")
load(file = "./Shen_MNI152_ORG.RData")
load(file = "./Shen_MNI152_v1_5.RData")
load(file = "./shen_description.RData")


# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  #volumes <-  getVolumes()
  #shinyFileChoose(input, "file",)
  
  #shinyFileChoose(input, "Btn_GetMapping", roots = volumes)
  #shinyFileChoose(input, "coord",   defaultRoot = "C", roots = c(C = "C:/"))

  
  coords <- reactive({
    req(input$Coordinates$datapath)
    as.matrix(read.csv(input$Coordinates$datapath, header = F, sep = input$sep))
  })
  
  mapping <- reactive({
    #mapping_path<-parseFilePaths(volumes, input$Btn_GetMapping)
    mapping_choice <- switch (input$shen_parc,
      shen278 = shen_MNI152_org,
      shen286 = shen286_MNI_v15
    )
    mapping_choice
  })
  
  
  datasetInput <- reactive({
  
    coords <- coords()
    templates <- mapping()
    print(coords)

    #print(templates)
    roi_res <-  identify_shenROI(coords, templates, input$is_tal)
    description.table <- switch (input$shen_parc,
      "shen278" = shen278_description[roi_res$shen_ID, ],
      "shen286" = shen286_MNI_v15_description[roi_res$shen_ID, ]
    )
    description.table
  })
  
  
    output$ROIdescription_with_files <- renderDataTable({
      
      coords <- coords()
      print(coords)
      templates<- mapping()
      #print(templates)
      roi_res <-  identify_shenROI(coords, templates, is.tal = input$is_tal)
      #write.csv(shen278_description[roi_res$shen_ID, ], file = paste(input$output.path, "/coords_transformed.csv", sep = ""))
      print(roi_res$shen_ID)
      
      
      description.table <- switch (input$shen_parc,
                                   shen278 = shen278_description[roi_res$shen_ID, ],
                                   shen286 = shen286_MNI_v15_description[roi_res$shen_ID, ]
      )
      description.table
      
    })
  
    output$downFile <- downloadHandler(
      filename = function() {
        paste0(input$downFile, " ", Sys.Date(), ".csv") 
      },
      content = function(file) {
        write.csv(datasetInput(), file, row.names = FALSE)
      }
    )
    
    
    output$ROIdescription_with_input <- renderDataTable({
      x <- input$X
      y <- input$Y
      z <- input$Z

      templates<- mapping()
      xyz <- matrix(c(x,y,z), 1, 3)
      
      description.table <- switch (input$shen_parc,
                                   "shen278" = shen278_description[identify_shenROI(xyz, templates, input$is_tal)$shen_ID, ],
                                   "shen286" = shen286_MNI_v15_description[identify_shenROI(xyz, templates, input$is_tal)$shen_ID, ]
      )
      description.table
    })
    

    output$ROIplot <- renderPlot({
      x <- input$X
      y <- input$Y
      z <- input$Z
      

      templates<- mapping()
      
      dimXYZ <- dim_(templates)[2:4]
      orgXYZ <- origin(templates)
      xyz <- RNifti::worldToVoxel(c(x,y,z), templates)
      # LR flip
      xyz[1] <- 2*orgXYZ[1]-xyz[1]
      print(xyz)
      oro.nifti::orthographic(templates, xyz = xyz)
    })
    
    
    mni <- reactive( {
        mni_tbl <- switch (input$shen_parc,
                 "shen278" = shen278_description[,
                                                 c("Number", "Hemisphere", "Assigned.Name", "Lobe", "yeo17", "X", "Y", "Z")],
                 "shen286" = shen286_MNI_v15_description[,
                                                         c("Number", "Hemisphere", "Assigned.Name", "Lobe", "yeo17", "X", "Y", "Z")]
        )
        if (input$ShenID != ""){
          print(input$ShenID)
          mni_tbl_selected <- mni_tbl[as.integer(input$ShenID),]
        }else{
          #print("here")
          yeo_selected <- rep(TRUE, nrow(mni_tbl))
          Lobe_selected <- rep(TRUE, nrow(mni_tbl))
          hemisphere_selected <- rep(TRUE, nrow(mni_tbl))
          name_selected <- rep(TRUE, nrow(mni_tbl))

          if (input$yeo17 != ""){
            yeo_selected <- mni_tbl$yeo17 %in% c(yeo17_names[as.integer(input$yeo17)])
          }
          
          if (input$Lobe != ""){
            Lobe_selected <- grep(paste("*", input$Lobe, "*", sep = ""), mni_tbl$Lobe)
          }
          
          if (input$Hemisphere != "both"){
            hemisphere_selected <- mni_tbl$Hemisphere == input$Hemisphere
          }
          
          if (input$AssignName != ""){
            name_selected <- grep(paste("*", input$AssignName, "*", sep = ""), mni_tbl$Assigned.Name)
          }
          
          selected <- yeo_selected & Lobe_selected & hemisphere_selected & name_selected 
          print(selected)
          mni_tbl_selected <- mni_tbl[selected, ]
          
        }
        
        mni_tbl_selected
        
    })

    
    output$MNI <- renderDataTable({
      mni_table <- as.data.frame(mni())
      mni_table
    })
    
    
    output$MNI_coords <- downloadHandler(
      filename = function() {
        paste0(input$MNI_coords, " ", Sys.Date(), ".csv") 
      },
      content = function(file) {
        mni_table <- as.data.frame(mni())
        write.csv(mni_table, file, row.names = FALSE)
      }
    )
    


})
