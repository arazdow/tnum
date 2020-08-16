## retrieve taxonomies

#' Get phrase taxonomies
#'
#' @param taxonomy string, one of "subject", "property", or "tags"
#' @param pattern  a tnum path with path-wildcard #, or string-wildcard * to restrict what tree is returned.
#' @param levels   integer, how man levels down to extract
#'
#' @return a DiagrammeR graph, suitable for display
#' @export

tnum.getDatabasePhraseGraph <-
  function(taxonomy = "subject",
           pattern = "",
           levels = 10) {
    # node and edge styles for DiagrammeR
    adjAes <-
      DiagrammeR::node_aes(
        shape = "rectangle",
        fillcolor = "white",
        fixedsize = FALSE,
        color = "black"
      )
    posAes <-
      DiagrammeR::node_aes(
        shape = "ellipse",
        fixedsize = FALSE,
        fillcolor = "white",
        color = "black"
      )
    adjEdgeAes <-
      DiagrammeR::edge_aes(
        label = "adj",
        fontcolor = "black",
        color = "black",
        dir = "back"
      )
    posEdgeAes <-
      DiagrammeR::edge_aes(
        label = "of",
        fontcolor = "black",
        color = "black",
        style = "dashed",
        dir = "back"
      )

    # recursive descent and other utilities

    tnToNodeWalker <- function(grph, tnNode, gNodeId) {
      newGrph <- grph
      tkids <- tnNode$childrenCount
      if (tkids > 0) {
        for (i in 1:tnNode$childrenCount) {
          tkid <- tnNode$children[[i]]
          nodeLabel <- tkid$fullName
          sep <- ""
          naes <- posAes
          eaes <- posEdgeAes
          if (stringr::str_count(nodeLabel, "[:/]") > 0) {
            nodeLabel <- stringr::str_extract(nodeLabel, "[/:][^/^:]+$")
            sep <- substr(nodeLabel, 1, 1)
          }
          if (sep == ":") {
            naes <- adjAes
            eaes <- adjEdgeAes
          }
          newGrph <-
            DiagrammeR::add_node(
              newGrph,
              label = nodeLabel,
              from = gNodeId,
              node_aes = naes,
              edge_aes = eaes
            )
          nextId <- get_last_node_id(newGrph)
          newGrph <- tnToNodeWalker(newGrph, tkid, nextId)
        }
      }
      return(newGrph)
    }

    # hack for getting id of the last node added tp a graph
    get_last_node_id <- function(grph) {
      ndf <- DiagrammeR::get_node_df(grph)
      return(tail(ndf$id, 1))
    }

    # get taxonomy as nested list
    args <-
      list(
        numberspace = tnum.env$tnum.var.nspace,
        type = taxonomy,
        srd = pattern,
        depth = levels
      )
    result <- httr::GET(
      query = args,
      paste0(
        "http://",
        tnum.env$tnum.var.ip,
        "/v1/numberspace/taxonomy"
      ),
      httr::add_headers(Authorization = paste0("Bearer ", tnum.env$tnum.var.token)),
      httr::accept("application/json"),
      httr::content_type("application/json")
    )
    #build a data.tree from the result

    tnApiRoot <- httr::content(result)$data
    dGraph <- DiagrammeR::create_graph()
    dGraph <-
      DiagrammeR::add_node(dGraph, label = tnApiRoot$fullName)
    dGraphRoot <- get_last_node_id(dGraph)
    dGraph <- tnToNodeWalker(dGraph, tnApiRoot, dGraphRoot)

    return(dGraph)
  }

#' Get a DiagrammeR tree for rendering, from a list of SRD paths
#'
#' @param pathList list of phrase path strings
#' @param rootLabel  a lable for the root of the graph
#' @param levels  limit for how many levels down paths to graph
#'
#' @return  Diagrammer graph object
#' @export

tnum.makePhraseGraphFromPathList <-
  function(pathList = list(),
           rootLabel = "ROOT",
           levels = 10) {
    # node and edge styling for DiagrammeR
    adjAes <-
      DiagrammeR::node_aes(
        shape = "rectangle",
        fillcolor = "white",
        fixedsize = FALSE,
        color = "black"
      )
    posAes <-
      DiagrammeR::node_aes(
        shape = "ellipse",
        fixedsize = FALSE,
        fillcolor = "white",
        color = "black"
      )
    adjEdgeAes <-
      DiagrammeR::edge_aes(
        label = "adj.",
        fontcolor = "grey",
        color = "black",
        dir = "back"
      )
    posEdgeAes <-
      DiagrammeR::edge_aes(
        label = "pos.",
        fontcolor = "grey",
        color = "black",
        style = "dashed",
        #dir = "back"
      )
    rootAes <-
      DiagrammeR::node_aes(
        color = "lightgrey",
        fixedsize = FALSE,
        fillcolor = "white",
        fontcolor = "lightgrey",
        shape = "plaintext"
      )
    tagAes <-
      DiagrammeR::node_aes(
        fixedsize = FALSE,
        fillcolor = rgb(1,1,1,0),
        fontcolor = rgb(0.2,0.5,0.2),
        shape = "plaintext"
      )
    rootEdgeAes <-
      DiagrammeR::edge_aes(color = "lightgrey", arrowhead = "none")
    propertyEdgeAes <-
      DiagrammeR::edge_aes(color = "lightgrey", arrowhead = "none", fontcolor = "lightgrey", label = "HAS")

    # recursive descent and other utility local functions
    assign("tnum.var.parity", 0, envir = tnum.env)

    tnToNodeWalker <- function(grph, dtNode, gNodeId) {
      newGrph <- grph
      tkids <- dtNode$count

      #recurse if there re children to do
      if (tkids > 0) {
        for (tkid in dtNode$children) {
          nodeLabel <- tkid$name
          sep <- "/"
          naes <- posAes
          eaes <- posEdgeAes
          if (stringr::str_count(nodeLabel, ":") > 0) {
            nodeLabel <- stringr::str_extract(nodeLabel, "[^/^:]+$")
            sep <- ":"
          }
          if (sep == ":") {
            naes <- adjAes
            eaes <- adjEdgeAes
          }
          if (gNodeId == 1) {
            eaes <- rootEdgeAes
          }
          if(stringr::str_starts(nodeLabel,"---")){
            eaes <- propertyEdgeAes
            nodeLabel <- substr(nodeLabel,4, nchar(nodeLabel))
          }
          if(stringr::str_detect(nodeLabel,"[?;]")){
            newLineParity <- tnum.env$tnum.var.parity
            eaes <- rootEdgeAes
            naes <- tagAes
            nodeLabel <- gsub("[;]",":",gsub("[?]","/",nodeLabel))
            if(newLineParity==1){
              nodeLabel <- paste0("\\n",nodeLabel)
            } else if(newLineParity==2){
              nodeLabel <- paste0("\\n\\n",nodeLabel)
            }
            assign("tnum.var.parity", (newLineParity + 1) %% 3, envir = tnum.env)
          }
          newGrph <-
            DiagrammeR::add_node(
              newGrph,
              label = nodeLabel,
              from = gNodeId,
              node_aes = naes,
              edge_aes = eaes
            )

          nextId <- get_last_node_id(newGrph)
          newGrph <- tnToNodeWalker(newGrph, tkid, nextId)

        }
      }
      return(newGrph)
    }

    # hack for getting id of the last node added tp a graph
    get_last_node_id <- function(grph) {
      ndf <- DiagrammeR::get_node_df(grph)
      return(tail(ndf$id, 1))
    }

    # begin main function body

    # prepend the forest root, and insert data.tree separator / before :
    pList <- paste0(rootLabel, "/", gsub(":", "/:", pathList))
    df <- data.frame(paths = pList)
    tree <-
      data.tree::as.Node(df, pathName = "paths") # get node tree

    # create DiagrammeR graph and add root node
    dGraph <- DiagrammeR::create_graph()
    DiagrammeR::add_global_graph_attrs(
      dGraph,
      attr = "overlap",
      value = "false",
      attr_type = "graph")
    dGraph <-
      DiagrammeR::add_node(
        dGraph,
        label = tree$name,
        node_aes = rootAes,
        edge_aes = rootEdgeAes
      )
    dGraphRoot <- get_last_node_id(dGraph)
    # start recursion on only child of prepended root node
    dGraph <- tnToNodeWalker(dGraph, tree, dGraphRoot)

    return(dGraph)
  }

#' Make full tnum graph from tnum.query return data frame
#'
#' @param tlist list of tnum objects as returned from tnum.query
#' @param tagMatch regexp to select tags to include in graph
#' @param collectors list of gsub patterns for replacement with ### to aggregate subjects
#'
#' @return returns a DiagrammeR graph
#' @export
#'
tnum.makeTnumPhraseGraph <- function(tlist, tagMatch = "", collectors = list()) {
  # make list of full-tnum paths using --- as "has"
  len <- length(tlist)
  subjAttrs <- tnum.getAttrFromList(tlist,"subject",  NA)
  propAttrs <- tnum.getAttrFromList(tlist,"property",  NA)
  tagAttrs <- tnum.getAttrFromList(tlist,"tags",  NA)
  tnumList <- paste0(subjAttrs, "/---", propAttrs)

  # now add tags matching regexps in tags list
  if(nchar(tagMatch)>0){
    newTnumList <- list()
    for(i in 1:length(tnumList)){
      newTnumList <- append(newTnumList,tnumList[[i]])
      if(length(tagAttrs[[i]])>0){
        rowtags <- tagAttrs[[i]]
        for(tag in rowtags[[1]]){
          if(stringr::str_detect(tag,tagMatch)){
            guardedTag <- gsub("[/]","?",gsub("[:]",";",tag))
            newTnumList <- append(newTnumList, paste0(tnumList[[i]],"/",guardedTag))
          }
        }
      }
    }
    tnumList <- unique(newTnumList)
  }

  commonRoots <- list()
  for(collector in collectors){
    if((nchar(collector)>0) && (stringr::str_count(collector,pattern = "###")>0)){
      collectorSplit <- stringr::str_split(collector, "###")[[1]]
      accum <- ""
      for(seg in collectorSplit){
        if(nchar(accum)>0){
          accum <- paste0(accum,"###",seg)
        } else {
          accum <- seg
        }
        if(nchar(seg)>0)
          commonRoots <- c(commonRoots, accum)
      }
    }
  }
  if (length(commonRoots) > 0) {
    for (commonRoot in commonRoots) {
      tnumList <-
        gsub(paste0(commonRoot, "[^/^:]+"),
             paste0(commonRoot, "###"),
             tnumList)
      tnumList <- unique(tnumList)
    }
  }
  gph <-
    tnum.makePhraseGraphFromPathList(tnumList, rootLabel = "tnums")
  return(gph)
}

#' Plot DiagrammeR graph
#'
#' @param gph the graph df
#' @param style what DiagrammeR calls "layout" = tree,nicely,neato,kk, or fr
#' @param size size of plot in pixels
#'
#' @return  result of render_graph() call
#' @export

tnum.plotGraph <- function(gph,style="neato", size=0){
  if(size > 0){
    res <- DiagrammeR::render_graph(gph, layout=style, width = size, height = size)
  } else {
    res <- DiagrammeR::render_graph(gph, layout=style)
  }

  return(res)
}
