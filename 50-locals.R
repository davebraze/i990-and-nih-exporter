
##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @title
##' @param font_size
##' @param font_family
##' @param line_size
##' @param rel_small
##' @param rel_vsmall
##' @param rel_large
##' @return
##' @author
theme_fdbplot <- function(font_size = 12, font_family = "", line_size = .5,
                          rel_small = 10/12, rel_vsmall = 9/12, rel_large = 14/12) {
    half_line <- font_size / 2
    small_size <- rel_small * font_size

  # work off of theme_grey in case something changes in element definitions
  theme_grey(base_size = font_size, base_family = font_family) %+replace%
    theme(
      line              = element_line(color = "black", size = line_size, linetype = 1, lineend = "butt"),
      rect              = element_rect(fill = NA, color = NA, size = line_size, linetype = 1),
      text              = element_text(family = font_family, face = "plain", color = "black",
                                       size = font_size, hjust = 0.5, vjust = 0.5, angle = 0, lineheight = .9,
                                       margin = margin(), debug = FALSE),

      axis.line         = element_line(color = "black", size = line_size, lineend = "square"),
      axis.line.x       = NULL,
      axis.line.y       = NULL,
      axis.text         = element_text(color = "black", size = small_size),
      axis.text.x       = element_text(margin = margin(t = small_size / 4), vjust = 1),
      axis.text.x.top   = element_text(margin = margin(b = small_size / 4), vjust = 0),
      axis.text.y       = element_text(margin = margin(r = small_size / 4), hjust = 1),
      axis.text.y.right = element_text(margin = margin(l = small_size / 4), hjust = 0),
      axis.ticks        = element_line(color = "black", size = line_size),
      axis.ticks.length = unit(half_line / 2, "pt"),
      axis.title.x      = element_text(
                            margin = margin(t = half_line / 2),
                            vjust = 1
                          ),
      axis.title.x.top  = element_text(
                            margin = margin(b = half_line / 2),
                            vjust = 0
                          ),
      axis.title.y      = element_text(
                            angle = 90,
                            margin = margin(r = half_line / 2),
                            vjust = 1
                          ),
      axis.title.y.right = element_text(
                             angle = -90,
                             margin = margin(l = half_line / 2),
                             vjust = 0
                           ),


      legend.background = element_blank(),
      legend.spacing    = unit(font_size, "pt"),
      legend.spacing.x  = NULL,
      legend.spacing.y  = NULL,
      legend.margin     = margin(0, 0, 0, 0),
      legend.key        = element_blank(),
      legend.key.size   = unit(1.1 * font_size, "pt"),
      legend.key.height = NULL,
      legend.key.width  = NULL,
      legend.text       = element_text(size = rel(rel_vsmall)),
      legend.text.align = NULL,
      legend.title      = element_text(hjust = 0),
      legend.title.align = NULL,
      legend.position   = "right",
      legend.direction  = NULL,
      legend.justification = c("left", "center"),
      legend.box        = NULL,
      legend.box.margin =  margin(0, 0, 0, 0),
      legend.box.background = element_blank(),
      legend.box.spacing = unit(font_size, "pt"),

      panel.background  = element_blank(),
      panel.border      = element_rect(color = "black", fill = NA),
      panel.grid        = element_blank(),
      panel.grid.major  = NULL,
      panel.grid.minor  = NULL,
      panel.grid.major.x = NULL,
      panel.grid.major.y = NULL,
      panel.grid.minor.x = NULL,
      panel.grid.minor.y = NULL,
      panel.spacing     = unit(half_line, "pt"),
      panel.spacing.x   = NULL,
      panel.spacing.y   = NULL,
      panel.ontop       = FALSE,

      strip.background  = element_rect(fill = "grey80"),
      strip.text        = element_text(
                            size = rel(rel_small),
                            margin = margin(half_line / 2, half_line / 2,
                                            half_line / 2, half_line / 2)
                          ),
      strip.text.x      = NULL,
      strip.text.y      = element_text(angle = -90),
      strip.placement   = "inside",
      strip.placement.x =  NULL,
      strip.placement.y =  NULL,
      strip.switch.pad.grid = unit(half_line / 2, "pt"),
      strip.switch.pad.wrap = unit(half_line / 2, "pt"),

      plot.background   = NULL, ## element_rect(fill="aliceblue"),
      plot.title        = element_text(
                            face = "bold",
                            size = rel(rel_large),
                            hjust = 0, vjust = 1,
                            margin = margin(b = half_line)
                          ),
      plot.subtitle     = element_text(
                            size = rel(rel_small),
                            hjust = 0, vjust = 1,
                            margin = margin(b = half_line)
                          ),
      plot.caption      = element_text(
                            size = rel(rel_vsmall),
                            hjust = 1, vjust = 1,
                            margin = margin(t = half_line)
                          ),
      plot.tag           = element_text(
                             face = "bold",
                             hjust = 0, vjust = 0.7
                           ),
      plot.tag.position = c(0, 1),
      plot.margin       = margin(half_line, half_line, half_line, half_line),

      complete = TRUE
    )
}

##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @title
##' @param data
##' @param x
##' @param y
##' @param color
##' @param df.benchmarks
##' @param legend.pos
##' @param jitter.height
##' @param boxplot
##' @param benchmark.offset
##' @return
##' @author
dibels_plot <- function(data, x, y, color, df.benchmarks,
                        legend.pos=c("left", "right"),
                        jitter.height=0, boxplot=TRUE,
                        benchmark.offset=0) {
    x <- enquo(x); y <- enquo(y); color <- enquo(color)
    retval <- ggplot(data, aes(x=!!x,
                               y=!!y,
                               color=!!color)) +
        geom_point(alpha=1/6, size=2,
                   position=position_jitterdodge(jitter.height=jitter.height, dodge.width=-.75)) +
        facet_grid(rows=vars(school_type)) +
        geom_segment(aes(x=as.integer(!!x)-.4, y=!!y+benchmark.offset,
                         xend=as.integer(!!x)+.4, yend=!!y+benchmark.offset),
                     color="blue", size=1, inherit.aes=FALSE, data=df.benchmarks) +
        guides(colour = guide_legend(override.aes = list(alpha=1))) +
        theme_fdbplot()

    if(boxplot) {
        retval <- retval + geom_boxplot(aes(color=NULL, fill=!!color),
                                        outlier.shape=NA, alpha=0,
                                        position=position_dodge2(padding=.3, reverse=TRUE))
    }
    if(legend.pos=="left"){
        retval <- retval+theme(legend.position = c(.008,.83))
    } else {
        retval <- retval+theme(legend.position = c(.78,.83))
    }
    retval
}


##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @title
##' @param content
##' @param out_type
##' @param digits
##' @param caption
##' @param bootstrap_options
##' @param latex_options
##' @param full_width
##' @param font_size
##' @return
##' @author
format_table <- function (content, out_type="pdf", digits=2, caption=" ", ## args to kable()
                          group_labels=NULL, group_starts=NULL, group_ends=NULL, ## args to pack_rows() via subtables()
                          ## args to kable_styling
                          bootstrap_options=c("striped", "condensed"),
                          latex_options=c("basic", "hold_position"),
                          full_width=FALSE,
                          font_size=10) {

    if(length(group_labels) != length(group_starts) ||
       length(group_starts) != (length(group_ends))) {
        stop("group_labels and group_starts and group_ends must all be the same length.")
    }

    subtables <- function(kable_table, group_labels, group_starts, group_ends) {
        if(is.null(group_labels)) return(kable_table)
        retval <- kable_table
        for (i in 1:length(group_labels)) {
            retval <- pack_rows(retval,
                                group_labels[i],
                                group_starts[i],
                                group_ends[i])
        }
        retval
    }

    if(out_type=="html") {
        ## format a table for html output.
        content %>%
            kable(format="html",
                  digits=digits,
                  caption=caption) %>%
            kable_styling(bootstrap_options=bootstrap_options,
                          full_width=full_width,
                          font_size=font_size) %>%
            column_spec(1, bold=TRUE) %>%
            subtables (group_labels, group_starts, group_ends)
    } else if (out_type=="pdf") {
        ## format a table for pdf output via latex
        content %>%
            kable(format="latex",
                  digits=digits,
                  caption=caption,
                  booktabs=TRUE) %>%
            kable_styling(latex_options=latex_options,
                          full_width=full_width,
                          font_size=font_size) %>%
            column_spec(1, bold=TRUE) %>%
            subtables (group_labels, group_starts, group_ends)
    } else {
        ## Mostly to get some output to docx files. NB. kableExtra::
        ## functions work only for html & latex.
        content %>%
            kable(format="pandoc", ## also consider "markdown"
                  digits=digits,
                  caption=caption)
    }
}
