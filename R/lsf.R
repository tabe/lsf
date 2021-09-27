## -*- mode: R -*-
##
## Copyright (C) 2021 Takeshi Abe
##
## This program is free software: you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published by
## the Free Software Foundation, either version 3 of the License, or
## (at your option) any later version.
##
## This program is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with this program.  If not, see <https://www.gnu.org/licenses/>.

#' @exportClass lsf
new_lsf <- methods::setClass("lsf",
    slots = c(
        location = "numeric"
      , scale = "numeric"
      , d_ = "function"
      , p_ = "function"
      , q_ = "function"
      , r_ = "function"
    )
)

.validate_scale <- function(scale) {
    if (scale <= 0)
        stop(sprintf("scale must be positive: %g", scale))
}

.find_or_stop <- function(name) {
    x <- mget(name, inherits = TRUE, ifnotfound = NA)
    if (any(is.na(x))) {
        m <- paste(sprintf("failed to find object of name %s", name), collapse = "\n")
        stop(m)
    }
    x
}

#' @export lsf
lsf <- function(name, .location = 0, .scale = 1) {
    location <- as.numeric(.location)

    scale <- as.numeric(.scale)
    .validate_scale(scale)

    ns <- sapply(c("d", "p", "q", "r"), function(prefix) paste0(prefix, name))
    fs <- .find_or_stop(ns)
    d_ <- function(x, .location = location, .scale = scale, ...) {
        fs[[1]]((x - .location)/.scale, ...) / scale
    }
    p_ <- function(q, .location = location, .scale = scale, ...) {
        fs[[2]]((q - .location)/.scale, ...)
    }
    q_ <- function(p, .location = location, .scale = scale, ...) {
        .scale * fs[[3]](p, ...) + .location
    }
    r_ <- function(n, .location = location, .scale = scale, ...) {
        .scale * fs[[4]](n, ...) + .location
    }
    new_lsf(location = location,
            scale = scale,
            d_ = d_,
            p_ = p_,
            q_ = q_,
            r_ = r_)
}

methods::setGeneric("d_", function(x) standardGeneric("d_"))
methods::setGeneric("p_", function(x) standardGeneric("p_"))
methods::setGeneric("q_", function(x) standardGeneric("q_"))
methods::setGeneric("r_", function(x) standardGeneric("r_"))

#' @export
methods::setMethod("d_", "lsf", function(x) x@d_)
#' @export
methods::setMethod("p_", "lsf", function(x) x@p_)
#' @export
methods::setMethod("q_", "lsf", function(x) x@q_)
#' @export
methods::setMethod("r_", "lsf", function(x) x@r_)
