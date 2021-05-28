#' create_html_pack
#'
#' @description
#' Create an html pack from a (possibly nested) named list(s).
#' The list(s) can contain plots, dataframes or other R objects.
#' Each list must be named as the names of each element is used as section headers.
#' The html is created via writing an rmarkdown file which is then knitted and deleted (assuming \code{clean_up=TRUE})
#'
#' @param output_list named list - Contains the output to export to html.
#' The list can be nested however all elements of all sub-lists must also be naemd
#' @param filepath string - file path and name of html. Best practive should end in html
#' @param title string - title to be used for the pack. Default is no title
#' @param clean_up logical - should intermediary files (.Rmd and .rds) be removed. Default is \code{TRUE}.
#' Could be changed to \code{FALSE} for debugging to to manually edit the pack e.g. adding comments.
#'
#' @return
#' @export
#'
#' @examples
#'
#' \donttest{
#' set.seed(666)
#'
#' output_list <- list()
#' output_list[["table 1"]] <- data.frame(a=runif(25), b=runif(25))
#' output_list[["Plot 1"]] <- ggplot(data=output_list[["table 1"]]) + geom_point(aes(x=~a, y=~b))
#'
#' create_html_pack(output_list = output_list, filepath="./create_html_pack.html")
#' }
#'
create_html_pack <- function(output_list, filepath="./created_html_pack.html", title="Title", clean_up=TRUE){

  checkmate::assert_list(output_list, names="named")
  checkmate::assert_character(filepath, len=1)
  checkmate::assert_character(title, len=1)
  checkmate::assert_logical(clean_up, len=1)

  # Check all elements of output list are named list or objects
  # This check could be done later on however if it fails we don't want anything saved
  # Recursive function
  check_named_sub_output_list <- function(sub_output_list){

    checkmate::assert_list(sub_output_list, names="named")

    for (ii in 1:length(sub_output_list)){

      if (inherits(sub_output_list[[ii]], "list")){ # If is nested list run recursively
        check_named_sub_output_list(sub_output_list[[ii]])
      }
    }
  }
  check_named_sub_output_list(output_list) # All sub-list must be named

  #Create some additional file paths
  if (grepl("\\.html$", filepath)){
    filepath_Rmd <- gsub("\\.html$", "\\.Rmd" , filepath)
    filepath_rds <- gsub("\\.html$", "\\.rds" , filepath)
  }else{
    filepath_Rmd <- paste0(filepath, ".Rmd")
    filepath_rds <- paste0(filepath, ".rds")
  }

  # Save the data so it can be read into the markdown
  saveRDS(output_list, file = filepath_rds)


  # Open Markdown file to write to
  sink(filepath_Rmd)

  # Write openng setup of markdown and then cell block to read in saved data
  cat(paste0(
"---
title: '",title,"'
output:
  html_document:
    df_print: paged
    toc: true
    number_sections: true
    toc_depth: 3
    toc_float: true
---

```{r, echo=FALSE}
output_list <- readRDS(file = '", getwd(), "/" ,filepath_rds,"')
```
"
  ))

  # Recursive function to write a cells to a notebook for each element of a nested list
  cat_cell <- function(sub_output_list, index=1, name=""){

    #checkmate::assert_list(sub_output_list, names="named")

    for (ii in 1:length(sub_output_list)){
      # Section Header
      cat(paste0("\n", paste0(rep("#", index), collapse = "")," ", names(sub_output_list)[ii], "\n"))

      if (inherits(sub_output_list[[ii]], "list")){ # If is nested list run recursively
        cat_cell(sub_output_list[[ii]], index=index+1, name=paste0(name, "[['",names(sub_output_list)[ii],"']]"))
      }else{# else output cell with plot

      cat("\n```{r, echo=FALSE}\n")
      cat(paste0("output_list", name, "[['",names(sub_output_list)[ii],"']]"))
      cat("\n```\n\n")
      }
    }
  }


  cat_cell(output_list) # Write nested list to Rmd cells
  sink() # Close markdown output file

  # knit output markdown file to html
  rmarkdown::render(filepath_Rmd)

  # Delete saved files which aren't needed. Mostly we just want the html.
  if (clean_up==TRUE){
    file.remove(filepath_rds)
    file.remove(filepath_Rmd)
  }

}
