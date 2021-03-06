import.q.sorts <- function(q.sorts.dir, q.set, q.distribution=NULL, conditions=NULL, manual.lookup=NULL, header = TRUE) {
  # Input validation (also see validation at the bottom!)
  assert_that(is.dir(q.sorts.dir))  # this will be further tested for conditions
  assert_that(is.matrix(q.set))
  if (!is.null(q.distribution)) {
    assert_that(is.vector(q.distribution))
    # TODO there could be more tests here on q.distribution
  }
  if (!is.null(conditions)) {
    assert_that(is.vector(conditions))
  }
  if (!is.null(manual.lookup)) {
    assert_that(is.matrix(manual.lookup))
  }
  assert_that(is.logical(header))

  # Deal with no conditions
  if (is.null(conditions)) {
    conditions <- "only.one"
  }
  conditions <- factor(conditions) #  such as before, after as factors

  # Gather participants p.set ==========================================
  p.set <- NULL  # must first exist for later appending
  for (cond in conditions) {  # gather *all* participants for all conds
    q.sorts.dir <- normalizePath(q.sorts.dir, mustWork = FALSE)  # normalize path for platform
    if (conditions != "only.one") {  # test conditions subdir only if there are conditions
      for (cond in conditions) {
        is.dir(paste(q.sorts.dir, cond, sep="/"))
        # TODO more informative error message would be nice at some point
      }
    }
    p.set.cond <- list.files(  # gather people by listing files
      path = if (cond == "only.one"){  # if no conditions
        q.sorts.dir  # this is the path
      } else {  # if more conditions
        paste (  # here comes the path
          q.sorts.dir,
          cond,  # consider condition in path
          sep = "/"
        )
      },
      no.. = TRUE,  # no dotfiles
      pattern = "\\.csv$"  # only csv
    )
    p.set.cond <- file_path_sans_ext(p.set.cond) #  kill extensions
    p.set <- append(p.set, p.set.cond) # append vector
  }
  p.set <- unique(p.set)  # make participants unique, also for no-cond (just in case)

  if (length(p.set) == 0) {
    stop("No CSV files could be found in the specified location.")
    # this tests (belatedly, somewhat) whether there are *any* CSVs to be found
    # this cannot be tested elegantly earlier because of the edge-case of some condition having NO CSVs (which is conceivable)
    # TODO(maxheld83) maybe rethink how this could be organized more elegantly
  }

  # Set up empty array =========================================================
  q.sorts <- array(
    #  participants, conditions, items makes 3 dimensions, all of which integer
    data = , #  no such thing yet, so all NAs (that should be the baseline!)
    dim = c(
      nrow(q.set), #  number of items
      length(p.set), #  number of participants
      length(conditions) #  number of conditions
    ),
    dimnames = list( #  dims should be called accordingly ...
      rownames(q.set), #  those are the items by meaningful short names
      p.set, #  those are the participants
      conditions #  those are the conditions
    )
  )

  # Create lookup table =======================================================
	if (is.null(manual.lookup)) {  # automatic hashing, same as in make.cards
    lookup.table <- apply(  # replace every language field with its hash
      X = q.set,
      MARGIN = c(1,2),
      digest,
        algo = "crc32",
        serialize = FALSE
    )
	} else {  # manually entered lookup table
    lookup.table <- manual.lookup
	}
	if (any(duplicated(lookup.table))) {  # test lookup table
	  stop ("There are duplicate IDs in the lookup table.")
	}

	# Import Loops ==============================================================
	for (cond in conditions) {  # loop over the conditions (such as before, after)
		for (part in p.set) {  # loop over participants
			path <- paste(
        q.sorts.dir,
        "/",  # always needs slash because q.sorts.dir is normalized before
        if (cond != "only.one") {
          paste0(
            cond,
            "/"
          )
        },
        part,
        ".csv",
        sep = ""
      )  # establish path
			path <- normalizePath(path, mustWork = FALSE)  # just to be safe if there are double slashes or sth
			if (!file.exists(path)) {  # there may be missing cases for some conditions
				warning(
          paste(  # it's not a deal-breaker just a warning
					  "There is no file for",
					  part,
					  "under condition",
					  cond,
					  ". NAs remains in array.",
					  sep = " "
				  )
        )
			} else {
			  if(is.null(q.distribution)) {  # do not limit read-in
			    current.sort <- read.csv(path, # let's do one sort at a time
			      header = header,  # take above option
			      stringsAsFactors = FALSE, #  would only add confusion
			      na.strings = "", #  empty cells become NAs
			      colClasses = "character"  # just to make sure R doesn't choke (mistakenly identified) irrational numbers :)
			    )
			  } else {# limit read-in
			    current.sort <- read.csv(path, # let's do one sort at a time
				    header = header,  # take above option
				    stringsAsFactors = FALSE, #  would only add confusion
				    nrows = max(q.distribution), # stuff below is ignored (item feedback, scores etc.)
				    na.strings = "", #  empty cells become NAs
				    colClasses = "character"  # just to make sure R doesn't choke (mistakenly identified) irrational numbers :)
			    )
			  }
			  current.sort <- as.matrix(current.sort) #  because read.csv makes dataframe
			  current.sort <- trimws(x = current.sort, which = "both")  # this protects against trailing and leading whitespaces
        for (id in na.omit(as.vector(current.sort))) {# loops over ids
          if (id %in% lookup.table) {  # do we know the id in the current sort?
            row <- which(lookup.table == id, arr.ind=TRUE)[,1]  # where is it in the table?
            handle <- rownames(lookup.table)[row]  # what is the short handle?
          } else {
            stop(
              paste(
                "The qsort in",
                path,
                "contains id",
                id,
                "is not defined as per manual.lookup and was ignored.",
                "Check whether you defined manual.lookup argument as intended."
              )
            )
          }
			    current.sort[current.sort==id] <- handle  # reassign it, in both cases
        }
			  # Test content of current sort for consistency ==========================
			  if (!(all(colSums(!is.na(current.sort)) == q.distribution)) & (!is.null(q.distribution))) {  # distr. ok?
  				stop(paste(
	  				"The qsort in",
		  			path,
			  		"does not conform to the set distribution.",
				  	"Offending columns:",
					  paste(names(colSums(!is.na(current.sort)) == q.distribution), collapse = " "),
					  sep = " "
				  ))
			  }
			  current.sort.handles <- c(as.vector(current.sort))  # preparation
			  current.sort.handles <- na.omit(current.sort.handles)  # NAs confuse the comparisons
			  all.handles <- rownames(q.set)  # preparation
			  in.sample.not.sort <- all.handles[!(all.handles %in% current.sort.handles)]  # all sampled items in sort?
        if (length(in.sample.not.sort) > 0) { # if there are any missing from sort
  				stop(  # must error out b/c of inconsistency
            paste(
		  			  "Some items from the sample cannot be found in",
			  		  path,
				  	  ". Missing items: "
            ),
            paste (  # give detail
              in.sample.not.sort,
              collapse = ", "
            )
				  )
        }
        in.sort.not.sample <- current.sort.handles[!(current.sort.handles %in% all.handles)]  # anything not in sample but in sort?
        if (length(in.sort.not.sample) > 0) {  # if there are items in sort, but not in sample
          stop(  # must error out b/c of inconsistency
            paste(
              "Some items from",
              path,
              "cannot be found in the sample",
              ".",
              "Orphaned items:"
            ),
            paste (  # give detail
              in.sort.not.sample,
              collapse = ", "
            )
          )
        }
			  # Transposing and aggregation of individual qsort =========================
			  for (item in rownames(q.set)) {  # loops over items
  				q.sorts[item,part,cond] <- which( #  write in qsort content of
	  				current.sort == item,  # participant cell of curr. looped item
		  			arr.ind = TRUE,  # return complete index (row, column)
			  		useNames = TRUE)[,2] # return only column
				  q.sorts[item,part,cond] <- as.integer(q.sorts[item,part,cond]-((length(q.distribution)+1)/2))
				  # make it into integers, revert to original score
			  }
		  }
		}
	}
  if (length(conditions) == 1) {
    q.sorts <- q.sorts[,,1]  # drops redundant dim for conditions
    if (length(p.set) == 1) {  # edge case of one q.sort
      q.sorts <- as.matrix(q.sorts)
      colnames(q.sorts) <- p.set
    }
  }
	return(q.sorts)
}
