##' Download the example data set from madams1/floridainmates
##'  (\url{https://github.com/madams1/floridainmates/})
##' @title Download example data set
##'
##' @param version Version number.  The default will load the most
##'   recent version on your computer or the most recent version known
##'   to the package if you have never downloaded the data before.
##'   With \code{floridainmates_del}, specifying \code{version=NULL} will
##'   delete \emph{all} data sets.
##'
##' @param path Path to store the data at.  If not given,
##'   \code{datastorr} will use \code{rappdirs} to find the best place
##'   to put persistent application data on your system.  You can
##'   delete the persistent data at any time by running
##'   \code{floridainmates_del(NULL)} (or \code{floridainmates_del(NULL, path)} if you
##'   use a different path).
##'
##' @export
floridainmates <- function(version=NULL, path=NULL) {
    datastorr::github_release_get(floridainmates_info(path), version)
}

##' @export
##' @rdname floridainmates
##'
##' @param local Logical indicating if local or github versions should
##'   be polled.  With any luck, \code{local=FALSE} is a superset of
##'   \code{local=TRUE}.  For \code{floridainmates_version_current}, if
##'   \code{TRUE}, but there are no local versions, then we do check
##'   for the most recent github version.
##'
floridainmates_versions <- function(local=TRUE, path=NULL) {
    datastorr::github_release_versions(floridainmates_info(path), local)
}

##' @export
##' @rdname floridainmates
floridainmates_version_current <- function(local=TRUE, path=NULL) {
    datastorr::github_release_version_current(floridainmates_info(path), local)
}

##' @export
##' @rdname floridainmates
floridainmates_del <- function(version, path=NULL) {
    datastorr::github_release_del(floridainmates_info(path), version)
}

## Core data:
floridainmates_info <- function(path) {
    datastorr::github_release_info("madams1/floridainmates",
                                   filename=NULL,
                                   read=readRDS,
                                   path=path)
}

##' Maintainer-only function for releasing data.  This will look at
##' the version in the DESCRIPTION file and make a data release if the
##' GitHub repository contains the same version as we have locally.
##' Requires the \code{GITHUB_TOKEN} environment variable to be set.
##'
##' @title Make a data release.
##' @param ... Parameters passed through to \code{\link{github_release_create}}
##' @param path Path to the data (see \code{\link{floridainmates}}).
##' @export
floridainmates_release <- function(..., path=NULL) {
    datastorr::github_release_create(floridainmates_info(path), ...)
}
