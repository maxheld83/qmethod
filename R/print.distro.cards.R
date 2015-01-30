print.distro.cards <- function(q.distribution, paper.format = "AveryZweckformC32010.Rnw", output.pdf = TRUE, extreme.string = c("Most Disagree","Most Agree")) {

  # Input validation also check more below
  if (!is.vector(q.distribution)) {
    stop("The q.distribution specified is not a vector")
  }
  if (!all((mode(q.distribution) == "numeric") && (q.distribution > 0) && (q.distribution %% 1 == 0))) {
    stop("The q.distribution contains non-positive and or non-integers.")
  }
  available.formats <- list.files(
    path = paste(
      path.package("qmethod"),  # where is the package?
      "/cardtemplates/",
      sep = ""
    ),
    no.. = TRUE  # no dotfiles
  )
  if (!paper.format %in% available.formats) {
    stop("The paper.format specified is not available.")
  }
  if (!is.logical(output.pdf)) {
    stop("The argument output.pdf has not been specified logically.")
  }
  if (!is.character(extreme.string)) {
    stop("The extreme string specified is not a character vector.")
  }
  # Read in items =============================================================
  distro.print <- matrix(q.distribution,c("yes","no","yes"))
distro.print
  )

  q.set.print <- as.data.frame( #  read in complete q.set, all translations
    x = q.set[,study.language]
  )
  colnames(q.set.print) <- "full wording"
  # Create lookup table (same as in import.q.feedback and import.q.sorts!)=====
  if (is.null(manual.lookup)) {  # in case there is no manual lookup
    lookup.table <- apply(  # replace every language field with its hash
      X = q.set,
      MARGIN = c(1,2),
      digest,
      algo = "crc32",
      serialize = FALSE
    )
  } else {  # in case of manually entered lookup table
    lookup.table <- manual.lookup  # just assign it
  }
  if (any(duplicated(lookup.table))) {  # test lookup table
    stop ("There are duplicate IDs in the lookup table.")
  }

  # Add ids to q.set.print ====================================================
  q.set.print$id <- NA  # set up empty id
  for (handle in rownames(q.set.print)) {  # loop over all ids in q.set
    if (is.null(manual.lookup)) {  # for automatic hashing
      q.set.print[handle,"id"] <- lookup.table[handle,study.language]
    } else {
      q.set.print[handle,"id"] <- lookup.table[handle]  # plug in id as row
    }

  }
  path <- paste(  # assign path to template
    path.package("qmethod"),  # where is the package?
    # remember, "inst" is not in path, because stuff from inst get put in root of package!
    "/cardtemplates/",
    paper.format,  # hopefully will have more templates in the future
    sep = ""
  )
  wording.font.size <- wording.font.size  # dumb, but otherwise R complains about unused argument
  if (output.pdf == TRUE) {
    return(
      knit2pdf(path)
    )
  } else {
    return(
      knit(path)
    )
  }
}
