# dependencies
if (!require('pacman')) install.packages('pacman'); library(pacman) #
p_load("ggplot2", "igraph", "ggraph", "stringr")

# vertex types, package-wide constants
ROOT <- 0
CHANCE <- 1
DECISION <- 2
END <- 3

#' Create Decision Tree Function
#'
#' This function allows you to create a decision tree object.
#' @param edgelist a list of edges with the following fields, in any order:
#'     "from":             source node for this edge
#'     "to":               target node for this edge
#'     "name":             edge name (optional)
#'     "outcome":          edge outcome, i.e. target node label (optional)
#'     "type":             source target type (optional, case insensitive), options:
#'                         "D","decision" - decision node (square)
#'                         "C","chance" - chance node (circle)
#'                         "E","end" - end node (triangle, optional)
#'     "probability":      edge probability
#'     <payoff1>,<p2>,...: any number of cost/effect fields (optional)
#'
#' @return graph object with the appropriate properties and node types
#'
#' @examples
#' tree <- create_tree(read.csv("branches.csv"))
#'
#' @export

create_tree<- function (edgelist){

  # check that the correct properties are in the argument
  if( !("from" %in% colnames(edgelist) && "to" %in% colnames(edgelist))) {
    stop("Invalid edge list!")
  }
  if( !("probability" %in% colnames(edgelist))) {
    stop("Missing probability property!")
  }

  # if some probabilities weren't provided, set them to 1
  edgelist$probability[which(is.na(edgelist$probability))] <- 1

  # create an igraph object
  graph  <- graph_from_edgelist(as.matrix(edgelist[, c("from","to")]))

  # if edge names were provided, store them in the graph
  if("name" %in% colnames(edgelist)){
    graph <- set_edge_attr(graph, "name", value = as.character(edgelist[, "name"]))
  }

  # if edge outcomes were provided, use them as vertex labels
  if("outcome" %in% colnames(edgelist)){

    # first make sure they're all empty strings, then apply the labels
    graph <- set_vertex_attr(graph, "label", value = as.character(""))
    graph <- set_vertex_attr(graph, "label", index = ends(graph, E(graph))[,2],
                             value = as.character(edgelist$outcome))

  # otherwise use the vertex ids as vertex labels
  } else {
    graph <- set_vertex_attr(graph,"label", value = as.character(V(graph)$name))
  }

  # get inbound and outbound edge degrees to determine node type
  degrees_in <- degree(graph, mode = "in")   # End nodes have 0 outgoing edges
  degrees_out <- degree(graph, mode = "out") # Root node has 0 incoming edges

  # if vertex/edge types were provided, update the types
  if("type" %in% colnames(edgelist)){

    # create a list of types from the type column, match with defined types
    types <- ifelse(grepl("^(D|d)$|Decision|decision", edgelist$type), DECISION,
               ifelse(grepl("^(C|c)$|Chance|chance", edgelist$type), CHANCE, END))

    # find the corresponding source vertices and store their type
    graph <- set_vertex_attr(graph, "type", index = ends(graph, E(graph))[,1], value = types)

  # otherwise set all vertex types as CHANCE
  } else {
    graph <- set_vertex_attr(graph, "type", value = CHANCE)
  }

  # determine root and end vertices, set their type accordingly, others remain the same
  graph <- set_vertex_attr(graph, "type", value = ifelse(degrees_in == 0 , ROOT, V(graph)$type))
  graph <- set_vertex_attr(graph, "type", value = ifelse(degrees_out == 0 , END, V(graph)$type))

  # iterate through other edgelist properties and add them to the graph object
  for (i in colnames(edgelist)) {

    # unless they are one of the special names
    if (!i %in% c("from", "to", "name", "type")) {
      graph <- set_edge_attr(graph, i, value = ifelse(is.na(edgelist[, i]), 0, edgelist[, i]))
    }
  }

  # return graph
  return(graph)
}
