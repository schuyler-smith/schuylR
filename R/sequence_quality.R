#' Sequence Quality
#'
#' Reads in ".fastq.gz", ".fastq.bz2", or ".fastq" files to create a list of
#' dataframes for analysis of sequence qualities
#' @usage sequence_quality(file, n = 5e+05)
#' @param file A string with either a file name or directory containing the 
#' required fastq file types.
#' @param n The number of reads to sample when processing FASTQ files.
#' @return list
#' @importFrom ShortRead qa 
#' @import data.table
#' @export
#' 
sequence_quality <- function(file, n = 5e+05){
  if(length(file) == 1 && dir.exists(file)){ 
    file <- gsub('/$','',file)
    files <- vector()
    for(extension in c(".fastq.gz$", ".fastq.bz2$", ".fastq$")){
      files <- append(files, dir(file, extension, full.names = TRUE))
    }
  } else {files <- file}
  files <- normalizePath(files)
  graph_data <- data.table()
  stat_data <- data.table()
  annotations <- data.table()
  for(file in files){
    srqa <- qa(file, n = n)
    df <- srqa[["perCycle"]]$quality
    graph_data <- rbind(graph_data, cbind(df, file = basename(file)))
    rc <- sum(srqa[["readCounts"]]$read)
    means <- rowsum(df$Score * df$Count, df$Cycle)/rowsum(df$Count, df$Cycle)
    quantile_calc <- function(score, count, q)score[which(cumsum(count)/sum(count) >= q)][[1]]
    stat_data <- rbind(stat_data, 
                       data.table(Cycle = as.integer(rownames(means)), 
                                  Mean = as.vector(means), 
                                  Q25 = as.vector(by(df, df$Cycle, function(foo) quantile_calc(foo$Score, foo$Count, 0.25), simplify = TRUE)), 
                                  Q50 = as.vector(by(df, df$Cycle, function(foo) quantile_calc(foo$Score, foo$Count, 0.5), simplify = TRUE)),
                                  Q75 = as.vector(by(df, df$Cycle, function(foo) quantile_calc(foo$Score, foo$Count, 0.75), simplify = TRUE)), 
                                  ct = 10 * as.vector(by(df, df$Cycle, function(foo) sum(foo$Count), simplify = TRUE))/min(rc,n), 
                                  file = basename(file)))
    if (rc >= n) {rclabel <- paste("Read Count >= ", n)} else rclabel <- paste("Read Count: ", rc)
    annotations <- rbind(annotations, data.table(minScore = min(df$Score), 
                                                 label = basename(file), 
                                                 rclabel = rclabel, 
                                                 rc = rc,
                                                 file = basename(file)))
  }
  seq_qual <- list(graph_data = graph_data, stat_data = stat_data, annotations = annotations)
  attributes(seq_qual) <- list(type = "ssSQ")
  return(seq_qual)
}

#' Plot Sequence Quality
#'
#' Creates plots of sequence qualitires
#' @usage sequence_quality(file)
#' @param file a list created by sequence_quality()
#' @return ggplot-object
#' @import ggplot2
#' @export
#' 
plot_sequence_quality <- function(file){
  graph_data <- file[[1]]
  stat_data <- file[[2]]
  annotations <- file[[3]]
  g <- ggplot(data = graph_data, aes(x = Cycle, y = Score)) + 
    geom_tile(aes(fill = Count)) + 
    scale_fill_gradient(low = "#F5F5F5", high = "black") + 
    geom_line(data = stat_data, aes(y = Mean), color = "#66C2A5", size = 2) + 
    geom_line(data = stat_data, aes(y = Q25), color = "#FC8D62", size = 0.25, linetype = "dashed") + 
    geom_line(data = stat_data, aes(y = Q50), color = "#FC8D62", size = 0.25) + 
    geom_line(data = stat_data, aes(y = Q75), color = "#FC8D62", size = 0.25, linetype = "dashed") + 
    geom_text(data = annotations, aes(x = 0, label = rclabel, y = 0), color = "red", hjust = 0, size = 5) + 
    guides(fill = FALSE) + 
    facet_wrap(~file) +   
    theme_bw() + 
    theme(
      axis.line.x = element_line(colour = 'black', size = 1, linetype = 'solid'),
      axis.line.y = element_line(colour = 'black',size = 1, linetype = 'solid'),
      axis.text.x = element_text(size = 10, vjust = 1, hjust = 1),
      axis.title.x = element_text(size = 12, face = "bold"),
      axis.title.y = element_text(size = 12, face = "bold"),
      legend.text = element_text(size = 10),
      legend.title = element_text(size = 12, face = "bold"),
      legend.background = element_rect(fill = (alpha = 0)),
      legend.key.size = unit(4, "mm"),
      legend.spacing.x = unit(0.005, 'npc'),
      strip.text.x = element_text(size = 12, face = 'bold', color = 'black'),
      strip.background = element_rect(colour = 'black', size = 1.4, fill = 'white'),
      panel.grid = element_blank()
    ) + 
    ylab("Quality Score") + 
    xlab("Cycle") + 
    scale_x_continuous(breaks = seq(0,max(graph_data$Cycle),25),
                       expand = expansion(mult = 0.01, 
                                          add = c(0)))
  return(g)
}