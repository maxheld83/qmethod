make.cards <- function(
  q.set,
  study.language = NULL,
  paper.format = "AveryZweckformC32010.Rnw",
  img.dir = NULL,
  output.pdf = TRUE,
  manual.lookup = NULL,
  wording.font.size = NULL,
  file.name = "QCards",
  babel.language = NULL,
  show.handles = FALSE,
  duplex.double = FALSE
) {
  # Input validation also check more below
  if (!is.matrix(q.set)) {
    stop("The q.set specified is not a matrix.")
  }
  if (!is.null(study.language)) {
    if (!(study.language %in% colnames(q.set)))
    {
      stop("The specified study language to be printed is not available in the q.set.")
    }
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
  if (!is.null(img.dir)) {
    img.dir <- normalizePath(path = img.dir)
    is.dir(path = img.dir)
  }
  if (!is.logical(output.pdf)) {
    stop("The argument output.pdf has not been specified logically.")
  }
  if (!is.logical(show.handles)) {
    stop("The argument output.pdf has not been specified logically.")
  }
  if (!is.null(manual.lookup) & !is.matrix(manual.lookup)) {
    stop("The manual.lookup specified is not a matrix.")
  }
  if (is.null(study.language)) {  # if there no languages
    study.language <- 1 # just look in column 1
  }
  if (!is.character((file.name))) {  # if filename not character
    stop("The specified filename is invalid.")
  }
  if (!is.null(babel.language) & !is.character((babel.language))) {  # if filename not character
    stop("The specified babel language is invalid.")
  }
  assert_that(is.flag(duplex.double))

  # Set up template-specific data heigh=========================================
  # TODO DEVELOPERS for all future templates, you must add the below switches
  # - maximgheight is a workaround for latex, because latex cannot (easily) infer row height in \includegraphics...
  #   so we're just hard coding it here
  #  if you have a better idea reopen https://github.com/aiorazabala/qmethod/issues/322
  # - pagerows is the number of rows per page, necessary for duplex.double
  maximgheight <- rowheight <- vmargin <- NULL  # to appease R Cmd Check
  vmargin <- 4  # measurements are all in mm
  switch(EXPR = paper.format,
    "AveryZweckformC32010.Rnw" = {
     rowheight <- 54
     pagerows <- 5
    },  #
    "2x1a4.Rnw" = {
      rowheight <- 130
      pagerows <- 1
    }
  )
  maximgheight <- rowheight - vmargin  # subtract constant margin, so that images don't completely fill cell (better for cutting)

  # Read in items =============================================================
  q.set.print <- as.data.frame( #  read in complete q.set, all translations
    x = q.set[,study.language]
  )
  colnames(q.set.print) <- "full wording"
  if (!is.null(img.dir)) {  # the following happens only it there is an img.dir
    q.set.print$`full wording` <- paste0("\\centering", "\\arraybackslash", "\\includegraphics[width=\\linewidth, height=", maximgheight, "mm,", "keepaspectratio=true]{", file_path_sans_ext(q.set), "}")
    # notice that latex includegraphics needs:
    #  - paths in quotation marks in case there are spaces in path
    #  - file path WITHOUT image extension
    #  - this also centers the image cell, which makes sense only for images, so it must be done locally rather than on column type declaration
  }
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
    if (!all(rownames(q.set) %in% rownames(manual.lookup))) {  # all q.set handles must occur inside lookup table
      stop ("The specified lookup table does not include all handles in the q.set.")
    }
    lookup.table <- as.matrix(manual.lookup[rownames(q.set), ])  # assign only those rows we are interested in
    colnames(lookup.table) <- colnames(manual.lookup)  # oddly, above code kills the colnames
  }
  if (any(is.na(lookup.table))) {  # test lookup table
    stop ("There are missing (NA) IDs in the lookup table.")
  }
  if (any(duplicated(lookup.table))) {  # test lookup table
    stop ("There are duplicate IDs in the lookup table.")
  }

  # Add ids to q.set.print ====================================================
  q.set.print$id <- NA  # set up empty id
  for (handle in rownames(q.set.print)) {  # loop over all ids in q.set
    if (show.handles) {  # this is for the researcher-facing variant
      q.set.print[handle, "id"] <- handle
    } else {
      q.set.print[handle,"id"] <- lookup.table[handle, study.language]
    }
  }
  # q.set.print$id <- unlist(lapply(X = q.set.print$id, FUN = "sanitizeTexString"))
  # the above makes sure that handle does not include active latex commands
  #    notice that we DO want latex commands in other columns, so this must be done here "by-hand" and can't be done for all columns via xtable etc.
  path <- paste(  # assign path to template
    path.package("qmethod"),  # where is the package?
    # remember, "inst" is not in path, because stuff from inst get put in root of package!
    "/cardtemplates/",
    paper.format,  # hopefully will have more templates in the future
    sep = ""
  )
  wording.font.size <- wording.font.size  # dumb, but otherwise R complains about unused argument


  # Duplicate items for easier duplex printing ================================
  if (duplex.double) {

    # nrow(q.set.print) must be divided by pagerows without remainder, otherwise
    # add NA for empty slots on last page
    # without these, the last page would not be duplex-compatible
    shortfall <- round_any(x = nrow(q.set), accuracy = pagerows, f = ceiling) - nrow(q.set)
    empty.closing.rows <- matrix(rep(x = NA, shortfall * ncol(q.set.print)), ncol = ncol(q.set.print))
    colnames(empty.closing.rows) <- colnames(q.set.print)
    q.set.print <- rbind(q.set.print, empty.closing.rows)

    # replicate entries
    rep.index <- unlist(lapply(split(c(1:nrow(q.set.print)), rep(1:(nrow(q.set.print)/pagerows), each = pagerows)), rep, 2), use.names = FALSE)
    q.set.print <- q.set.print[rep.index, ]
  }


  # Make PDF or LaTeX =========================================================
  if (output.pdf == TRUE) {
    return(
      knit2pdf(
        input = path
        ,output = paste(getwd(), "/", file.name, ".tex", sep="")
      )
    )
  } else {
    return(
      knit(
        input = path
        ,output = paste(getwd(), "/", file.name, ".tex", sep="")
      )
    )
  }
}
