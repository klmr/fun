# testthat sneakily changes the current working directory. Undo this, otherwise
# poor ‹modules› gets confused with relative script_path()s.
if (grepl('/_tests$', getwd())) {
    warning('‹testthat› has changed the CWD; I’m undoing that.')
    setwd(dirname(getwd()))
}

modules::import('..', attach = TRUE)
