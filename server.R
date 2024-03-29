## TODO
## save diagrams as zip file with jpegs
## compareGroups() -> overflow with horizontal scrollbar
## full info for compare groups
## or compare groups separately for shape, spread, location
## -> checkbox "combine all groups"
## -> checkbox for each group
## upload image -> integrate over arbitrary convex polygon

library(shiny)
library(shotGroups)

source("helper.R")

shinyServer(function(input, output) {
    #####-----------------------------------------------------------------------
    ## provide the data - reactive conductor
    #####-----------------------------------------------------------------------
    coords <- reactive({
        ## only change when explicitly applied
        input$applyData

        ## isolate against non-applied changes in data input UI elements
        isolate({
            if(input$datIn == '1') {
                ## built in data
                get(dataBuiltIn[input$builtInData])
            } else if(input$datIn == '2') {
                ## upload files
                if(!is.null(input$fileUpload)) {
                    fPath <- input$fileUpload$datapath

                    if(input$fileType == '1') {          ## OnTarget 1.*
                        readDataOT1(fPath=dirname(fPath), fNames=basename(fPath))
                    } else if(input$fileType == '2') {   ## OnTarget 2.*, 3.*
                        readDataOT2(fPath=dirname(fPath), fNames=basename(fPath))
                    } else if(input$fileType == '3') {   ## other
                        readDataMisc(fPath=dirname(fPath), fNames=basename(fPath))
                    }
                } else {
                    NULL
                }
            } else if(input$datIn == '3') {
                ## paste data
                fPath <- tempfile()
                writeLines(input$datPaste, fPath)
                if(input$fileType == '1') {          ## OnTarget 1.*
                    readDataOT1(dirname(fPath), fNames=basename(fPath))
                } else if(input$fileType == '2') {   ## OnTarget 2.*, 3.*
                    readDataOT2(dirname(fPath), fNames=basename(fPath))
                } else if(input$fileType == '3') {   ## other
                    readDataMisc(dirname(fPath), fNames=basename(fPath))
                }
            } else {
                NULL
            }
        })
    })

    #####---------------------------------------------------------------------------
    ## provide file information - UI element
    #####---------------------------------------------------------------------------
    output$fileInfo <- renderUI({
        xy <- coords()
        dstTrgt <- if(!is.null(xy$distance) && !all(is.na(xy$distance))) {
            xy$distance[1]
        } else {
            "not available"
        }

        nGroups <- if(!is.null(xy$series) && !all(is.na(xy$series))) {
            nlevels(xy$series)
        } else {
            nlevels(xy$group)
        }

        comment <- attributes(xy)$comment
        ammo <- if(!is.null(xy$ammunition) && !all(is.na(xy$ammunition)) && !all(xy$ammunition == "")) {
            paste(unique(xy$ammunition), collapse=", ")
        } else {
            NULL
        }

        ## isolate against non-applied changes in data input UI elements
        isolate({
            x <- if(input$datIn == "1") {
                paste0("<p>For details on this data set (measurement units etc.), see ",
                       "<a href='http://www.rdocumentation.org/packages/shotGroups/functions/",
                       dataBuiltIn[input$builtInData], "'>", dataBuiltIn[input$builtInData],
                       "</a></p>Name: ", dataBuiltIn[input$builtInData], "<br />")
            } else if(input$datIn == "2") {
                if(!is.null(input$fileUpload)) {
                    paste0("<p>Name: ", paste(basename(input$fileUpload$name), collapse=", "), "<br />")
                } else {
                    "<p>"
                }
            } else {
                paste0("<p>Pasted data<br />")
            }

            y <- paste0(x, "Number of shots: ", nrow(xy),
                   "<br />Distance to target ", dstTrgt,
                   "<br />Number of groups: ", nGroups,
                   ifelse(!is.null(comment), "<br />Additional information: ", ""), comment,
                   ifelse(!is.null(ammo),    "<br />Ammunition: ", ""), ammo, "</p>")
            HTML(y)
        })
    })

    #####---------------------------------------------------------------------------
    ## provide short file information - UI element
    #####---------------------------------------------------------------------------
    output$fileInfoShort <- renderUI({
        xy <- coords()

        ## isolate against non-applied changes in data input UI elements
        isolate({
            nGroups <- if(!is.null(xy$series) && !all(is.na(xy$series))) {
                nlevels(xy$series)
            } else {
                nlevels(xy$group)
            }

            x <- if(input$datIn == "1") {
                paste0("<p>Name: ", dataBuiltIn[input$builtInData], "<br />")
            } else if((input$datIn == "2") && !is.null(input$fileUpload)) {
                paste0("<p>Name: ", paste(basename(input$fileUpload$name), collapse=", "), "<br />")
            } else {
                paste0("<p>Pasted data<br />")
            }

            y <- paste0(x, "Number of shots: ", nrow(xy),
                        "<br />Number of groups: ", nGroups, "</p>")
            HTML(y)
        })
    })

    #####---------------------------------------------------------------------------
    ## provide file name + ammo information - reactive conductor
    #####---------------------------------------------------------------------------
    fileName <- reactive({
        xy <- coords()
        ammo <- if(!is.null(xy$ammunition) && !all(is.na(xy$ammunition)) && !all(xy$ammunition == "")) {
            paste(unique(xy$ammunition), collapse=", ")
        } else {
            NULL
        }

        ## isolate against non-applied changes in data input UI elements
        isolate({
            fName <- if(input$datIn == "1") {
                dataBuiltIn[input$builtInData]
            } else if(input$datIn == "2") {
                if(!is.null(input$fileUpload)) {
                    paste(basename(input$fileUpload$name), collapse=", ")
                } else {
                    NULL
                }
            } else {
                paste0("Pasted data")
            }
            c(fName, ammo, "")
        })
    })

    #####---------------------------------------------------------------------------
    ## distance to target, unit distance, unit xy-coords - UI element
    #####---------------------------------------------------------------------------
    output$unitDstXY <- renderUI({
        xy <- coords()
        dstTarget <- if(!is.null(xy$distance) && !all(is.na(xy$distance)) &&
                        !all(xy$distance == "")) {
            ## distance to target is given in input data
            xy$distance[1]
        } else {
            ## default distance to target
            100
        }

        if(input$task == "Angular size") {
            ## angular size -> dst to target, and unit dst, but not unit xy
            inputPanel(numericInput("dstTrgt", h5("Distance to target"),
                                    min=0, step=1, value=dstTarget),
                       selectInput("unitDst", h5("Measurement unit distance"),
                                   choices=unitsDst, selected=2))
        } else {
            ## for all except angular size -> dst to target, unit dst, unit xy
            inputPanel(numericInput("dstTrgt", h5("Distance to target"),
                                    min=0, step=1, value=dstTarget),
                       selectInput("unitDst", h5("Measurement unit distance"),
                                   choices=unitsDst, selected=2),
                       selectInput("unitXY", h5("Measurement unit coordinates"),
                                   choices=unitsXY, selected=3))
        }
    })

    #####---------------------------------------------------------------------------
    ## string for conversion argument - unit distance to unit xy-coords
    ## reactive conductor
    #####---------------------------------------------------------------------------
    conversionStr <- reactive({
        paste0(unitsDstInv[input$unitDst], "2",
               unitsXYInv[input$unitXY], collapse="")
    })

    #####---------------------------------------------------------------------------
    ## group shape
    #####---------------------------------------------------------------------------
    ## output list - reactive conductor
    shapeList <- reactive({
        xy <- coords()
        if(!is.null(xy)) {
            groupShape(xy,
                       plots=FALSE,
                       dstTarget=input$dstTrgt,
                       conversion=conversionStr(),
                       bandW=input$shapeBW,
                       outlier=c("1"="mcd", "2"="pca")[input$shapeOutlier])
        } else {
            NULL
        }
    })

    ## output - only selected list components
    output$shape <- renderPrint({
        out <- shapeList()
        out[shapeOutInv[input$shapeOut]]
    })

    ## save output to file
    output$saveShape <- downloadHandler(
        filename=function() { "groupShape.txt" },
        content=function(file) {
            writeLines(fileName(), con=file)
            out    <- shapeList()
            outSel <- out[shapeOutInv[input$shapeOut]]
            outSelNames <- names(outSel)
            Map(textOut, outSel, outSelNames, file)
        },
        contentType='text/plain' # MIME type
    )

    ## create diagrams - output UI slots
    output$shapePlot <- renderUI({
        shapePlotOutL <- lapply(seq_len(nShapePlots), function(i) {
            plotOutput(paste0("shapePlot", i))
        })

        ## convert the list to a tagList and return
        do.call(tagList, shapePlotOutL)
    })

    ## the actual plots - adapted from Winston Chang https://gist.github.com/wch/5436415
    for(i in seq_len(nShapePlots)) {
        local({
            localI <- i
            output[[paste0("shapePlot", localI)]] <- renderPlot({
                xy <- coords()
                if(!is.null(xy)) {
                    shotGroups:::groupShapePlot(xy,
                        which=localI,
                        dstTarget=input$dstTrgt,
                        conversion=conversionStr(),
                        bandW=input$shapeBW,
                        outlier=c("1"="mcd", "2"="pca")[input$shapeOutlier])
                } else {
                    NULL
                }
            })
        })
    }

    ## save diagrams to file
    output$saveShapePDF <- downloadHandler(
        filename=function() { "groupShape.pdf" },
        content=function(file) {
            xy <- coords()
            if(!is.null(xy)) {
                pdf(file)
                for(i in seq_len(nShapePlots)) {
                    shotGroups:::groupShapePlot(xy,
                        which=i,
                        dstTarget=input$dstTrgt,
                        conversion=conversionStr(),
                        bandW=input$shapeBW,
                        outlier=c("1"="mcd", "2"="pca")[input$shapeOutlier])
                }
                dev.off()
            } else {
                NULL
            }
        },
        contentType='application/pdf' # MIME type
    )

    #####---------------------------------------------------------------------------
    ## group spread / precision
    #####---------------------------------------------------------------------------
    ## output list - reactive conductor
    spreadList <- reactive({
        ## if no CEP type is selected -> fall back to default CorrNormal
        CEPtype <- if(!is.null(input$spreadCEPtype)) {
            CEPtypesInv[input$spreadCEPtype]
        } else {
            "CorrNormal"
        }

        bootCI <- if(!is.null(input$spreadCItype)) {
            CItypesInv[input$spreadCItype]
        } else {
            "none"
        }

        xy <- coords()
        if(!is.null(xy)) {
            groupSpread(xy,
                        plots=FALSE,
                        CEPtype=CEPtype,
                        CEPlevel=input$spreadCEPlevel,
                        CIlevel=input$spreadCIlevel,
                        bootCI=bootCI,
                        dstTarget=as.numeric(input$dstTrgt),
                        conversion=conversionStr())
        }
    })

    ## output - only selected list components
    output$spread <- renderPrint({
        out <- spreadList()
        outSel <- out[spreadOutInv[input$precisionOut]]
        ## paste CI/CEP level
        outSelNames <- names(outSel)
        idxCEP      <- outSelNames %in% CEPOutComps
        idxCI       <- outSelNames %in% CIOutComps
        outSelNames[idxCEP] <- paste0(outSelNames[idxCEP], "_", 100*input$spreadCEPlevel, "%")
        outSelNames[idxCI]  <- paste0(outSelNames[idxCI],  "_", 100*input$spreadCIlevel,  "%")
        setNames(outSel, outSelNames)
    })

    ## save output to file
    output$saveSpread <- downloadHandler(
        filename=function() { "groupPrecision.txt" },
        content=function(file) {
            writeLines(fileName(), con=file)
            out    <- spreadList()
            outSel <- out[spreadOutInv[input$precisionOut]]
            ## paste CI/CEP level
            outSelNames <- names(outSel)
            idxCEP      <- outSelNames %in% CEPOutComps
            idxCI       <- outSelNames %in% CIOutComps
            outSelNames[idxCEP] <- paste0(outSelNames[idxCEP], "_", 100*input$spreadCEPlevel, "%")
            outSelNames[idxCI]  <- paste0(outSelNames[idxCI],  "_", 100*input$spreadCIlevel,  "%")
            outSel <- setNames(outSel, outSelNames)
            Map(textOut, outSel, outSelNames, file)
        },
        contentType='text/plain' # MIME type
    )

    ## create diagrams - output UI slots
    output$spreadPlot <- renderUI({
        spreadPlotOutL <- lapply(1:nSpreadPlots, function(i) {
            plotOutput(paste0("spreadPlot", i))
        })

        ## convert the list to a tagList and return
        do.call(tagList, spreadPlotOutL)
    })

    ## the actual plots - adapted from Winston Chang https://gist.github.com/wch/5436415
    for(i in seq_len(nSpreadPlots)) {
        local({
            localI <- i
            output[[paste0("spreadPlot", localI)]] <- renderPlot({
                xy <- coords()
                if(!is.null(xy)) {
                    shotGroups:::groupSpreadPlot(xy,
                        which=localI,
                        CEPlevel=input$spreadCEPlevel,
                        CIlevel=input$spreadCIlevel,
                        dstTarget=as.numeric(input$dstTrgt),
                        conversion=conversionStr())
                } else {
                    NULL
                }
            })
        })
    }

    ## save diagrams to file
    output$saveSpreadPDF <- downloadHandler(
        filename=function() { "groupPrecision.pdf" },
        content=function(file) {
            xy <- coords()
            if(!is.null(xy)) {
                pdf(file)
                for(i in seq_len(nSpreadPlots)) {
                    shotGroups:::groupSpreadPlot(xy,
                        which=i,
                        CEPlevel=input$spreadCEPlevel,
                        CIlevel=input$spreadCIlevel,
                        dstTarget=as.numeric(input$dstTrgt),
                        conversion=conversionStr())
                }
                dev.off()
            } else {
                NULL
            }
        },
        contentType='application/pdf' # MIME type
    )

    #####---------------------------------------------------------------------------
    ## group location / accuracy
    #####---------------------------------------------------------------------------
    ## output list - reactive conductor
    locationList <- reactive({
        bootCI <- if(!is.null(input$locCItype)) {
            CItypesInv[input$locCItype]
        } else {
            "none"
        }

        xy <- coords()
        if(!is.null(xy)) {
            groupLocation(xy,
                          plots=FALSE,
                          level=input$locLevel,
                          bootCI=bootCI,
                          dstTarget=as.numeric(input$dstTrgt),
                          conversion=conversionStr())
        } else {
            NULL
        }
    })

    ## output - only selected list components
    output$location <- renderPrint({
        out    <- locationList()
        outSel <- out[locationOutInv[input$locationOut]]
        ## paste CI/CEP level
        outSelNames <- names(outSel)
        idx         <- outSelNames %in% CIOutComps
        outSelNames[idx] <- paste0(outSelNames[idx], "_", 100*input$locLevel, "%")
        setNames(outSel, outSelNames)
    })

    ## save output to file
    output$saveLocation <- downloadHandler(
        filename=function() { "groupAccuracy.txt" },
        content=function(file) {
            writeLines(fileName(), con=file)
            out    <- locationList()
            outSel <- out[locationOutInv[input$locationOut]]
            ## paste CI/CEP level
            outSelNames <- names(outSel)
            idx         <- outSelNames %in% CIOutComps
            outSelNames[idx] <- paste0(outSelNames[idx], "_", 100*input$locLevel, "%")
            outSel <- setNames(outSel, outSelNames)
            Map(textOut, outSel, outSelNames, file)
        },
        contentType='text/plain' # MIME type
    )

    ## show diagram
    output$locationPlot <- renderPlot({
        xy <- coords()
        if(!is.null(xy)) {
            groupLocation(xy,
                          plots=TRUE,
                          level=input$locLevel,
                          bootCI="none",
                          dstTarget=as.numeric(input$dstTrgt),
                          conversion=conversionStr())
        } else {
            NULL
        }
    })

    ## save diagram to file
    output$saveLocationPDF <- downloadHandler(
        filename=function() { "groupAccuracy.pdf" },
        content=function(file) {
            xy <- coords()
            if(!is.null(xy)) {
                pdf(file)
                groupLocation(xy,
                              plots=TRUE,
                              level=input$locLevel,
                              bootCI="none",
                              dstTarget=as.numeric(input$dstTrgt),
                              conversion=conversionStr())
                dev.off()
            } else {
                NULL
            }
        },
        contentType='application/pdf' # MIME type
    )

    #####-----------------------------------------------------------------------
    ## compare groups
    #####-----------------------------------------------------------------------
    ## output list - reactive conductor
    output$compGroups <- renderUI({
        xy <- coords()
        if(!is.null(xy)) {
            checkboxGroupInput("compGroupSel",
                               label=h5("Select groups"),
                               choices=getGroups(xy, choices=TRUE),
                               selected=c(1, 2))
        } else {
            NULL
        }
    })

    ## list of output comonents to choose from - UI element
    output$compOut <- renderUI({
#         xy <- coords()
#         if(!is.null(xy)) {
#             groupSel <- getGroups(xy)[input$compGroupSel]
#             compChoices <- if(length(groupSel) <= 2) {
#                 compOut2
#             } else {
#                 compOut3plus
#             }
            compChoices <- compOut2
            selectizeInput("compareOut", label=h5("Select the output elements you want to see"),
                           choices=compChoices, multiple=TRUE,
                           selected=c("1", "2", "5", "8", "9", "10",
                                      "12", "14", "15"), width="100%")
#         } else {
#             NULL
#         }
    })

    ## output - only selected list components
    compareList <- reactive({
        CEPtype  <- if(!is.null(input$cmpCEPtype)) {
            CEPtypesInv[input$cmpCEPtype]
        } else {
            "CorrNormal"
        }
        xy <- coords()
        if(!is.null(xy)) {
            groupSel <- getGroups(xy)[input$compGroupSel]
            xySub    <- xy[xy$series %in% groupSel , ]
            res <- compareGroups(xySub,
                          plots=FALSE,
                          xyTopLeft=input$cmpXYTL,
#                          ABalt=c('two.sided', 'less', 'greater'),
#                          Walt=c('two.sided', 'less', 'greater'),
                          CEPtype=CEPtype,
                          CEPlevel=input$compareCEPlevel,
                          CIlevel=input$compareCIlevel,
                          conversion=conversionStr())
            list(len=length(groupSel), res=res)
        } else {
            NULL
        }
    })

    ## output - only selected list components
    output$compare <- renderPrint({
        out <- compareList()
        outSel <- if(out$len <= 2) {
            out$res[compOut2Inv[input$compareOut]]
        } else {
            out$res[compOut3plusInv[input$compareOut]]
        }
        ## paste CI/CEP level
        outSelNames <- names(outSel)
        idxCEP <- outSelNames %in% CEPOutComps
        idxCI  <- outSelNames %in% CIOutComps
        outSelNames[idxCEP] <- paste0(outSelNames[idxCEP], "_", 100*input$compareCEPlevel, "%")
        outSelNames[idxCI]  <- paste0(outSelNames[idxCI],  "_", 100*input$compareCIlevel,  "%")
        setNames(outSel, outSelNames)
    })


    ## save output to text file
    output$saveCompare <- downloadHandler(
        filename=function() { "groupCompare.txt" },
        content=function(file) {
            writeLines(fileName(), con=file)
            out    <- compareList()
            outSel <- if(out$len <= 2) {
                out$res[compOut2Inv[input$compareOut]]
            } else {
                out$res[compOut3plusInv[input$compareOut]]
            }
            ## paste CI/CEP level
            outSelNames <- names(outSel)
            idxCEP <- outSelNames %in% CEPOutComps
            idxCI  <- outSelNames %in% CIOutComps
            outSelNames[idxCEP] <- paste0(outSelNames[idxCEP], "_", 100*input$compareCEPlevel, "%")
            outSelNames[idxCI]  <- paste0(outSelNames[idxCI],  "_", 100*input$compareCIlevel,  "%")
            outSel <- setNames(outSel, outSelNames)
            Map(textOut, outSel, outSelNames, file)
        },
        contentType='text/plain' # MIME type
    )

    ## create diagrams - output UI slots
    output$comparePlot <- renderUI({
        comparePlotOutL <- lapply(seq_len(nComparePlots), function(i) {
            plotOutput(paste0("comparePlot", i))
        })

        ## convert the list to a tagList and return
        do.call(tagList, comparePlotOutL)
    })

    ## the actual plots - adapted from Winston Chang https://gist.github.com/wch/5436415
    for(i in seq_len(nComparePlots)) {
        local({
            localI <- i
            output[[paste0("comparePlot", localI)]] <- renderPlot({
                xy <- coords()
                if(!is.null(xy)) {
                    groupSel <- getGroups(xy)[input$compGroupSel]
                    xySub    <- xy[(xy$series %in% groupSel), ]
                    shotGroups:::compareGroupsPlot(xySub,
                        which=localI,
                        xyTopLeft=input$cmpXYTL,
                        CEPlevel=input$compareCEPlevel,
                        CIlevel=input$compareCIlevel,
                        conversion=conversionStr())
                } else {
                    NULL
                }
            })
        })
    }

    ## save diagrams to file
    output$saveComparePDF <- downloadHandler(
        filename=function() { "groupCompare.pdf" },
        content=function(file) {
            xy <- coords()
            if(!is.null(xy)) {
                pdf(file)
                for(i in seq_len(nComparePlots)) {
                    groupSel <- getGroups(xy)[input$compGroupSel]
                    xySub    <- xy[(xy$series %in% groupSel), ]
                    shotGroups:::compareGroupsPlot(xySub,
                        which=i,
                        xyTopLeft=input$cmpXYTL,
                        CEPlevel=input$compareCEPlevel,
                        CIlevel=input$compareCIlevel,
                        conversion=conversionStr())
                }
                dev.off()
            } else {
                NULL
            }
        },
        contentType='application/pdf' # MIME type
    )

    #####---------------------------------------------------------------------------
    ## hit probability
    #####---------------------------------------------------------------------------
    ## CEP output list - reactive conductor
    hitProbCEPList <- reactive({
        xy <- coords()
        if(!is.null(xy)) {
            ## if no CEP type is selected -> fall back to default CorrNormal
            CEPtype <- if(!is.null(input$hitpCEPtype)) {
                CEPtypesInv[input$hitpCEPtype]
            } else {
                "CorrNormal"
            }

            if(input$hitpType == 1) {
                ## hit probability -> radius
                x <- getCEP(xy,
                            CEPlevel=input$hitpLevel,
                            dstTarget=input$dstTrgt,
                            conversion=conversionStr(),
                            accuracy=input$hitpAcc,
                            type=CEPtype,
                            doRob=input$hitpDoRob)$CEP
                setNames(list(x), paste0("CEP_", 100*input$hitpLevel, "%"))
            } else {
                ## radius -> hit probability
                x <- getHitProb(xy,
                                r=input$hitpR,
                                unit=hitpRUnitInv[[input$hitpUnitR]],
                                dstTarget=input$dstTrgt,
                                conversion=conversionStr(),
                                accuracy=input$hitpAcc,
                                type=CEPtype,
                                doRob=input$hitpDoRob)
                setNames(list(x), paste0("hitProbRadius_", input$hitpR))
            }
        } else {
            NULL
        }
    })

    ## confidence ellipse output list - reactive conductor
    hitProbConfEllList <- reactive({
        xy <- coords()
        if(!is.null(xy)) {
            res <- if(input$hitpType == 1) {
                ## hit probability -> radius
                getConfEll(xy,
                           level=input$hitpLevel,
                           dstTarget=input$dstTrgt,
                           conversion=conversionStr(),
                           doRob=input$hitpDoRob)
            } else {
                ## radius -> hit probability
                NULL
            }

            x <- if(input$hitpDoRob) {
                list(confEllSizeRobust=res$sizeRob, confEllShapeRobust=res$shapeRob)
            } else {
                list(confEllSize=res$size, confEllShape=res$shape)
            }

            outNames <- paste0(names(x), "_", 100*input$hitpLevel, "%")
            setNames(x, outNames)
        } else {
            NULL
        }
    })

    ## extrapolation to different distance output list
    hitProbExtraList <- reactive({
        xy <- coords()
        if(!is.null(xy)) {
            ## if no CEP type is selected -> fall back to default CorrNormal
            CEPtype <- if(!is.null(input$hitpCEPtype)) {
                CEPtypesInv[input$hitpCEPtype]
            } else {
                "CorrNormal"
            }

            if(input$hitpType == 1) {
                ## hit probability -> radius
                res1 <- getCEP(xy,
                               CEPlevel=input$hitpLevel,
                               dstTarget=input$dstTrgt,
                               conversion=conversionStr(),
                               accuracy=input$hitpAcc,
                               type=CEPtype,
                               doRob=input$hitpDoRob)$CEP["MOA", , drop=FALSE]

                res2 <- getConfEll(xy,
                                   level=input$hitpLevel,
                                   dstTarget=input$dstTrgt,
                                   conversion=conversionStr(),
                                   doRob=input$hitpDoRob)

                res3 <- if(input$hitpDoRob) {
                    res2$sizeRob["MOA", , drop=FALSE]
                } else {
                    res2$size["MOA", , drop=FALSE]
                }

                CEP <- fromMOA(res1,
                               dst=input$hitpExtraDst,
                               conversion=paste0(unitsDstInv[input$hitpUnitExtraDst], "2",
                                                 unitsXYInv[input$unitXY], collapse=""))
                ConfEll <- fromMOA(res3,
                                   dst=input$hitpExtraDst,
                                   conversion=paste0(unitsDstInv[input$hitpUnitExtraDst], "2",
                                                     unitsXYInv[input$unitXY], collapse=""))
                rownames(CEP)     <- "unit"
                rownames(ConfEll) <- "unit"
                x <- list(CEP=CEP, ConfEll=ConfEll)
                setNames(x, paste0(names(x), "_", 100*input$hitpLevel, "%", "_@",
                                   input$hitpExtraDst, unitsDstInv[input$hitpUnitExtraDst]))
            } else {
                ## radius -> hit probability
                MOA <- getMOA(input$hitpR,
                              dst=input$hitpExtraDst,
                              conversion=paste0(unitsDstInv[input$hitpUnitExtraDst], "2",
                                                unitsXYInv[input$unitXY], collapse=""))
                x <- getHitProb(xy,
                                r=MOA,
                                unit="MOA",
                                dstTarget=input$dstTrgt,
                                conversion=conversionStr(),
                                accuracy=input$hitpAcc,
                                type=CEPtype,
                                doRob=input$hitpDoRob)
                setNames(list(x), paste0("hitProbRadius_", input$hitpR, "_@",
                                         input$hitpExtraDst, unitsDstInv[input$hitpUnitExtraDst]))
            }
        } else {
            NULL
        }
    })

    ## CEP output
    output$hitProbCEP <- renderPrint({
        hitProbCEPList()
    })

    ## confidence ellipse output
    output$hitProbConfEll <- renderPrint({
        hitProbConfEllList()
    })

    ## extrapolation to different distance output
    output$hitProbExtra <- renderPrint({
        hitProbExtraList()
    })

    ## save output to text file
    output$saveHitProb <- downloadHandler(
        filename=function() { "groupHitProbability.txt" },
        content=function(file) {
            writeLines(fileName(), con=file)
            out1 <- hitProbCEPList()
            out2 <- hitProbConfEllList()
            out3 <- hitProbExtraList()
            if(input$hitpType == 1) {
                ## hit probability -> radius
                out <- c(out1, out2, out3)
            } else {
                ## radius -> hit probability - no confidence ellipse
                out <- c(out1, out3)
            }
            Map(textOut, out, names(out), file)
        },
        contentType='text/plain' # MIME type
    )

    #####---------------------------------------------------------------------------
    ## target plot
    #####---------------------------------------------------------------------------

    output$targetPlot <- renderPlot({
        xy <- coords()
        if(!is.null(xy)) {
            drawGroup(xy,
                      xyTopLeft=input$trgtXYTL,
                      bb=input$trgtBB,
                      bbMin=input$trgtBBmin,
                      bbDiag=input$trgtBBdiag,
                      minCirc=input$trgtMinCirc,
                      maxSpread=input$trgtMaxSpread,
                      meanDist=input$trgtMeanDist,
                      confEll=input$trgtConfEll,
                      CEP=input$trgtCEP,
                      ringID=input$trgtRingID,
                      scaled=input$trgtScaled,
                      caliber=input$trgtCaliber,
                      level=input$trgtCEPlevel,
                      dstTarget=input$dstTrgt,
                      conversion=conversionStr(),
                      unit=unitsPlotInv[input$trgtUnitPlot],
                      alpha=input$trgtAlpha,
                      target=targetLinv[[input$trgtTarget]])
        } else {
            NULL
        }
    })

    ## simulated ring count output
    output$simRingCount <- renderPrint({
        xy <- coords()
        if(!is.null(xy)) {
            if(grepl("DSU[ab][[:digit:]]+", targetLinv[[input$trgtTarget]])) {
                "Simulated ring count is not yet available for oval DSU targets"
            } else if(input$trgtTarget != "1") {
                simRingCount(xy,
                             caliber=input$trgtCaliber,
                             unit=unitsXYInv[input$unitXY],
                             target=targetLinv[[input$trgtTarget]])
            } else {
                NULL
            }
        } else {
            NULL
        }
    })

    ## save diagram to file
    output$saveTargetPDF <- downloadHandler(
        filename=function() { "groupTarget.pdf" },
        content=function(file) {
            xy <- coords()
            if(!is.null(xy)) {
                pdf(file)
                drawGroup(xy,
                          xyTopLeft=input$trgtXYTL,
                          bb=input$trgtBB,
                          bbMin=input$trgtBBmin,
                          bbDiag=input$trgtBBdiag,
                          minCirc=input$trgtMinCirc,
                          maxSpread=input$trgtMaxSpread,
                          meanDist=input$trgtMeanDist,
                          confEll=input$trgtConfEll,
                          CEP=input$trgtCEP,
                          ringID=input$trgtRingID,
                          scaled=input$trgtScaled,
                          caliber=input$trgtCaliber,
                          level=input$trgtCEPlevel,
                          dstTarget=input$dstTrgt,
                          conversion=conversionStr(),
                          unit=unitsPlotInv[input$trgtUnitPlot],
                          alpha=input$trgtAlpha,
                          target=targetLinv[[input$trgtTarget]])
                dev.off()
            } else {
                NULL
            }
        },
        contentType='application/pdf' # MIME type
    )

    #####---------------------------------------------------------------------------
    ## angular size
    #####---------------------------------------------------------------------------

    output$size <- renderPrint({
        dstTrgt     <- input$dstTrgt
        unitDstTrgt <- unitsDstInv[input$unitDst]
        szeAbs1     <- as.numeric(strsplit(input$angszeAbs1, "[[:blank:]]")[[1]])
        szeAng1     <- as.numeric(strsplit(input$angszeAng1, "[[:blank:]]")[[1]])
        unitAbs1    <- unitsAbsInv[input$angszeUnitAbs1]
        unitAng1    <- unitsAngInv[input$angszeUnitAng1]
        szeAbs2     <- as.numeric(strsplit(input$angszeAbs2, "[[:blank:]]")[[1]])
        szeAng2     <- as.numeric(strsplit(input$angszeAng2, "[[:blank:]]")[[1]])
        unitAbs2    <- unitsAbsInv[input$angszeUnitAbs2]
        unitAng2    <- unitsAngInv[input$angszeUnitAng2]
        unitAbsOut  <- unitsAbsInv[input$angszeUnitAbsOut]
        unitAngOut  <- unitsAngInv[input$angszeUnitAngOut]
        unitDstOut  <- unitsAbsInv[input$angszeUnitDstOut]

        getMOAmult <- function(uao) {
            x <- getMOA(szeAbs1, dst=dstTrgt,
                   conversion=paste0(unitDstTrgt, "2", unitAbs1, collapse=""),
                   type=uao)
            signif(x, 4)
        }

        fromMOAmult <- function(uao) {
            x <- fromMOA(szeAng1, dst=dstTrgt,
                    conversion=paste0(unitDstTrgt, "2", uao, collapse=""),
                    type=unitAng1)
            signif(x, 4)
        }

        getDistMult <- function(udo) {
            x <- getDistance(szeAbs2,
                        angular=szeAng2,
                        conversion=paste0(udo, "2", unitAbs2),
                        type=unitAng2)
            signif(x, 4)
        }

        if(input$angszeType == "1") {
            ## absolute size -> angular size
            values <- Map(getMOAmult, unitAngOut)
            x <- setNames(as.data.frame(values), paste("angSize", unitAngOut, sep="_"))
            if(!any(is.na(szeAbs1)) && !any(duplicated(szeAbs1))) {
                rownames(x) <- szeAbs1
            } else {
                NULL
            }
        } else if(input$angszeType == "2") {
            ## angular size -> absolute size
            values <- Map(fromMOAmult, unitAbsOut)
            x <- setNames(as.data.frame(values), paste("absSize", unitAbsOut, sep="_"))
            if(!any(is.na(szeAng1)) && !any(duplicated(szeAng1))) {
                rownames(x) <- szeAng1
            } else {
                NULL
            }
        } else if(input$angszeType == "3") {
            ## absolute size + angular size -> distance
            values <- Map(getDistMult, unitDstOut)
            x <- setNames(as.data.frame(values), paste("dist", unitDstOut, sep="_"))
            rNames <- paste0(szeAbs2, "_", szeAng2)
            if(!any(is.na(szeAbs2) | is.na(szeAng2)) && !any(duplicated(rNames))) {
                rownames(x) <- rNames
            } else {
                NULL
            }
        }

        x
    })
})
