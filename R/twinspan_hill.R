#' Constants of the original TWINSPAN
#'
#' The values that Hill's FORTRAN program sets for one division.
#' They are used when `twinspan(polish = "hill")`, and are collected here
#' so that the correspondence with the original is easy to check.
#'
#' @return  A named list of the constants.
#'          `rat_lim`, `frq_lim`, `feeble`, `icw_exp`, `ipr_exp`,
#'          `cwt_min`, `cr_long`, `cr_cut`, `polish_iter`,
#'          `mz_crit`, `mz_out` and `mz_ind`.
#' @examples
#' \donttest{
#' unlist(tw_hill_const())
#' }
#' @export
tw_hill_const <- function(){
  list(rat_lim     = 3,     # RATLIM: preference is clipped at (r-1)/(r+1)
       frq_lim     = 0.2,   # FRQLIM: frequency above which no downweighting
       feeble      = 0.1,   # FEEBLE: a pseudospecies below this is no indicator
       icw_exp     = 1,     # ICWEXP: exponent of the frequency in the weight
       ipr_exp     = 4,     # IPREXP: exponent of the preference in the weight
       cwt_min     = 0.01,  # CWTMIN: smallest weight of a pseudospecies
       cr_long     = 0.2,   # CRLONG: half width of the critical zone
       cr_cut      = 0.2,   # CRCUT:  half width of the zone used in polishing
       polish_iter = 2,     # IREWT:  number of polishing steps
       mz_crit     = 8,     # MZCRIT: zones inside the critical zone
       mz_out      = 4,     # MZOUT:  zones outside the critical zone
       mz_ind      = 4)     # MZIND:  zones where the indicators decide
}

# The zoned ordination (ZONEUP): mz_out zones below the critical zone,
# mz_crit zones inside it and mz_out zones above it.
tw_zone <- function(x, crmin, crmax, mz_out, mz_crit){
  mz    <- mz_crit + 2 * mz_out
  small <- 1e-10
  axmin <- min(x)
  axmax <- max(x)
  seg1  <- (crmin - axmin) / (mz_out  + small) + small
  seg2  <- (crmax - crmin) / (mz_crit + small) + small
  seg3  <- (axmax - crmax) / (mz_out  + small) + small
  iz <- integer(length(x))
  lo <- x <  crmin
  hi <- x >  crmax
  md <- !lo & !hi
  if(any(lo))
    iz[lo] <- pmin(pmax(as.integer((x[lo] - axmin) / seg1) + 1L, 1L), mz_out)
  if(any(hi))
    iz[hi] <- pmin(pmax(as.integer((x[hi] - crmax) / seg3) + 1L, 1L), mz_out) +
              mz - mz_out
  if(any(md))
    iz[md] <- pmin(pmax(as.integer((x[md] - crmin) / seg2) + 1L, 1L), mz_crit) +
              mz_out
  return(iz)
}

# Division centre of the zoned ordination and of the indicator score (FIND).
# Chooses the zone boundary and the threshold that misclassify fewest
# stands, and breaks ties by the balance of the two groups, then by a
# threshold near zero, then by a centred band.
tw_find <- function(zone, score, opt, n_ind, n_neg){
  mz     <- opt$mz_crit + 2 * opt$mz_out
  ishift <- 1 + n_neg
  # the indicator score runs from -n_neg to n_ind - n_neg, and is held
  # as an index of 1 to n_ind + 1, whatever values actually occur
  vals <- seq_len(n_ind + 1) - ishift
  best <- NULL
  for(izd in (opt$mz_out + opt$mz_ind):(opt$mz_out + opt$mz_crit)){
    iiz <- izd - opt$mz_ind
    a   <- sum(zone <= iiz)
    b   <- sum(zone >  izd)
    bal <- if(a + b > 0) abs(a - b) / (a + b) else 0
    ib  <- abs(1 + mz - izd - iiz)
    for(is in seq_along(vals)){
      thr  <- vals[is]
      miss <- sum(zone <= iiz & score >  thr) +
              sum(zone >  izd & score <= thr)
      ia   <- abs(is - ishift)
      cur  <- list(misclassified = miss, izd = izd, iiz = iiz,
                   threshold = thr, balance = bal, ia = ia, ib = ib)
      take <- FALSE
      if(is.null(best)){
        take <- TRUE
      } else if(miss < best$misclassified){
        take <- TRUE
      } else if(miss == best$misclassified){
        if(bal < best$balance)      take <- TRUE
        else if(bal == best$balance){
          if(ia < best$ia)          take <- TRUE
          else if(ib < best$ib)     take <- TRUE
        }
      }
      if(take) best <- cur
    }
  }
  return(best)
}

# Weight of each stand for one side of a division (INDSCO).
# A stand is counted in proportion to how far it lies beyond the zone
# between cut1 and cut2, up to a full weight of one.
tw_indsco <- function(y, x, cut1, cut2, rw = NULL){
  mid <- (cut1 + cut2) / 2
  hlf <- (cut2 - cut1) / 2 + 1e-10
  ax  <- pmax(pmin((x - mid) / hlf, 1), -1)
  if(!is.null(rw)) ax <- ax * rw
  neg <- pmax(-ax, 0)
  pos <- pmax( ax, 0)
  return(list(axneg = sum(neg),
              axpos = sum(pos),
              yneg  = as.vector(crossprod(y, neg)),
              ypos  = as.vector(crossprod(y, pos))))
}

# One polishing step (POLISH).
# The polished axis is the sum of two ordinations: an additive score in
# which frequent and preferential pseudospecies weigh most, and the plain
# mean preference of the pseudospecies of the stand.
tw_polish <- function(y, x, opt, rw = NULL, cw = NULL){
  rng <- range(x)
  mid <- sum(rng) / 2
  hlf <- (rng[2] - rng[1]) * 0.5 * opt$cr_cut
  s   <- tw_indsco(y, x, mid - hlf, mid + hlf, rw)
  prlim <- (opt$rat_lim - 1) / (opt$rat_lim + 1)
  ay  <- if(s$axneg > 0) s$yneg / s$axneg else rep(0, ncol(y))
  ayy <- if(s$axpos > 0) s$ypos / s$axpos else rep(0, ncol(y))
  den <- ay + ayy
  pref <- ifelse(den > 0, (ayy - ay) / den, 0)
  freq <- pmin(den, opt$frq_lim)
  pref <- pmax(pmin(pref, prlim), -prlim)
  pref[abs(pref) < 0.001] <- 0.001
  if(is.null(cw)) cw <- rep(1, ncol(y))
  colwgt <- cw * (freq / opt$frq_lim)^opt$icw_exp *
            (abs(pref) / prlim)^opt$ipr_exp
  score  <- pref / prlim
  ord1 <- as.vector(y %*% (score * colwgt))
  mx   <- max(abs(ord1))
  if(mx > 0) ord1 <- ord1 / mx
  # the second ordination uses the basic weights of the columns only
  den2 <- as.vector(y %*% cw)
  ord2 <- ifelse(den2 > 0, as.vector(y %*% (score * cw)) / den2, 0)
  return(ord1 + ord2)
}

# Indicator pseudospecies of a division, in the way of the original.
# The number of indicators is the one that misclassifies fewest stands,
# and the stands of the critical zone are placed by the indicator score.
# The best indicator pseudospecies of a division (TOPIND).
# The strength of a pseudospecies is truncated to steps of 1/500 before
# the ordering, ties are broken by the order of the columns, and only one
# pseudospecies of a species can become an indicator.
tw_topind <- function(d, opt, sp = NULL){
  if(is.null(sp)) sp <- seq_along(d)
  rank <- as.integer(abs(d) * 500)          # IDINT: truncation
  ord  <- order(-rank, seq_along(d))
  res  <- integer(0)
  used <- integer(0)
  for(j in ord){
    if(length(res) >= opt$max_indicators) break
    if(opt$feeble - abs(d[j]) > 1e-7) next  # PRECIS
    if(sp[j] %in% used) next                # one indicator per species
    res  <- c(res, j)
    used <- c(used, sp[j])
  }
  return(res)
}

tw_indicator_hill <- function(y, x, opt, rw = NULL, sp = NULL){
  rng <- range(x)
  mid <- sum(rng) / 2
  hlf <- 0.5 * opt$cr_long * (rng[2] - rng[1])
  zone <- tw_zone(x, mid - hlf, mid + hlf, opt$mz_out, opt$mz_crit)
  cut1 <- mid - hlf * opt$mz_ind / (opt$mz_crit + 0.001)
  s    <- tw_indsco(y, x, cut1, 2 * mid - cut1, rw)
  d    <- s$ypos / max(s$axpos, 1e-12) - s$yneg / max(s$axneg, 1e-12)
  ord  <- tw_topind(d, opt, sp)
  if(!length(ord)) return(NULL)
  best <- NULL
  for(k in seq_along(ord)){
    sel   <- ord[seq_len(k)]
    sgn   <- sign(d[sel])
    score <- as.vector(y[, sel, drop = FALSE] %*% sgn)
    f     <- tw_find(zone, score, opt, n_ind = k, n_neg = sum(sgn < 0))
    if(is.null(best) || f$misclassified < best$misclassified)
      best <- c(f, list(indicators = sel, sign = sgn, score = score))
  }
  pos <- zone > best$izd
  band <- zone > best$iiz & zone <= best$izd
  pos[band] <- best$score[band] > best$threshold
  best$positive <- pos
  best$zone     <- zone
  return(best)
}

#' Data on which the species of TWINSPAN are classified
#'
#' The original TWINSPAN does not classify the species on the
#' pseudospecies table itself, but on how faithful each species is to the
#' groups of stands.
#' Every group of the hierarchy, the terminal ones and the ones that were
#' divided further, becomes three pseudo-quadrats, at the cut levels
#' 0.8, 2 and 6 of the ratio between the frequency of the species inside
#' the group and its frequency outside.
#' A species weighs as much as it occurs, and a group weighs as much as
#' it holds, multiplied by `sqrt(2)` for every level it stands above the
#' deepest one, and doubled for the two upper cut levels.
#'
#' @param object A "twinspan" object, or the result of `tw_tree()`.
#' @param psp    The pseudospecies matrix of that object.
#' @param sp_map The species of each pseudospecies.
#' @param levmax The deepest level of division.
#' @return  A list of the binary matrix ($y) of species by
#'          pseudo-quadrats, the weights of the species ($rw) and of the
#'          pseudo-quadrats ($cw), and the ratios ($ratio).
#' @examples
#' \donttest{
#' data(dune, package = "vegan")
#' tw <- twinspan(dune)
#' str(tw_species_data(tw))
#' }
#' @export
tw_species_data <- function(object, psp = object$pseudospecies,
                            sp_map = attr(psp, "species"),
                            levmax = object$max_depth){
  tree <- object
  lv   <- tw_species_counts(psp, sp_map)          # stands x species
  sp   <- colnames(lv)
  # the group of every stand, in the numbering of the original
  leaf <- vapply(tree$nodes, function(nd)
                   if(all(is.na(nd$children)))
                     strtoi(paste0("1", nd$path), base = 2L) else NA_integer_,
                 integer(1))
  id <- integer(nrow(lv))
  for(i in seq_along(tree$nodes))
    if(!is.na(leaf[i])) id[tree$nodes[[i]]$members] <- leaf[i]
  nc  <- max(id)
  mat <- matrix(0, nc, length(sp), dimnames = list(NULL, sp))
  for(k in unique(id))
    mat[k, ] <- colSums(lv[id == k, , drop = FALSE])
  # the totals of a group include those of the groups it was divided into
  if(nc >= 2)
    for(i in seq(nc, 2))
      mat[i %/% 2, ] <- mat[i %/% 2, ] + mat[i, ]
  clsum <- rowSums(mat)
  ratio <- mat * (clsum[1] - clsum) /
           (clsum * (rep(mat[1, ], each = nc) - mat) + 1e-7)
  ratio <- t(ratio)                                # species x groups
  y <- cbind(ratio >= 0.8, ratio >= 2, ratio >= 6) * 1L
  o <- as.vector(t(matrix(seq_len(3 * nc), ncol = 3)))
  y <- y[, o, drop = FALSE]                        # three columns per group
  colnames(y) <- paste0("group_", rep(seq_len(nc), each = 3), "_",
                        rep(1:3, nc))
  rownames(y) <- sp
  # weights
  rw  <- colSums(lv > 0)                           # how often a species occurs
  cwt <- clsum * sqrt(2)^(levmax - floor(log2(seq_len(nc))))
  cw  <- rep(cwt, each = 3) * c(1, 2, 2)
  return(list(y = y, rw = as.vector(rw), cw = as.vector(cw), ratio = ratio))
}

# Occurrences of every species in every stand, counted over the cut
# levels of the species. Used by tw_closer().
tw_species_counts <- function(y, sp_map = NULL){
  y <- as.matrix(y) * 1
  if(is.null(sp_map)) return(y)
  sp  <- unique(sp_map)
  res <- matrix(0, nrow(y), length(sp), dimnames = list(rownames(y), sp))
  for(k in seq_along(sp))
    res[, k] <- rowSums(y[, sp_map == sp[k], drop = FALSE])
  return(res)
}

# Path of the other child of the same parent
tw_sib_path <- function(path){
  n <- nchar(path)
  if(!n) return(NA_character_)
  paste0(substr(path, 1, n - 1),
         if(substr(path, n, n) == "0") "1" else "0")
}

# Members of the node of a path, or NULL
tw_members_of <- function(nodes, path){
  if(is.na(path)) return(NULL)
  for(nd in nodes) if(identical(nd$path, path)) return(nd$members)
  return(NULL)
}

# How much a group resembles the positive side of a division (CLOSER).
# A negative value means that the group resembles the negative side.
# The species that prefer one side count most, and the indifferent ones
# are trimmed so that they cannot outweigh the preferential ones.
tw_closer <- function(lv, ref, neg, pos, active, rw = NULL, cw = NULL){
  small <- 1e-7
  if(is.null(rw)) rw <- rep(1, nrow(lv))
  tot  <- sum(rw[ref])
  tot0 <- sum(rw[neg])
  tot1 <- sum(rw[pos])
  if(!length(ref) || !length(neg) || !length(pos) || !any(active)) return(0)
  if(tot <= 0 || tot0 <= 0 || tot1 <= 0) return(0)
  lv  <- lv[, active, drop = FALSE]
  if(!is.null(cw)) lv <- sweep(lv, 2, cw[active], "*")
  wsum <- function(i) colSums(sweep(lv[i, , drop = FALSE], 1, rw[i], "*"))
  ay  <- (wsum(ref) + small) / tot
  ay0 <- (wsum(neg) + small) / tot0
  ay1 <- (wsum(pos) + small) / tot1
  pref <- pmin(abs(ay0 - ay1) / (ay0 + ay1) / 0.3, 1)^4
  to_pos <- ay1 > ay0
  ppos <- ifelse(to_pos, pref, 0)
  pneg <- ifelse(to_pos, 0, pref)
  pind <- 1 - pref
  yneg <- sum(pneg * ay)
  yind <- sum(pind * ay)
  ypos <- sum(ppos * ay)
  xneg <- sum(pneg * ay0) + sum(pneg * ay1)
  xind <- sum(pind * ay0) + sum(pind * ay1)
  xpos <- sum(ppos * ay0) + sum(ppos * ay1)
  if(xpos > xneg){
    yind <- -yind
    xind <- -xind
    if(-xind > xpos - xneg) yind <- yind * (xpos - xneg) / (-xind)
  } else {
    if(xind > xneg - xpos) yind <- yind * (xneg - xpos) / xind
  }
  return((ypos - yneg + yind) * tot)
}

# Should the two halves of a division be swapped?
# The original puts first the half that resembles the group next to the
# one being divided, so that neighbouring groups stay together.
tw_swap <- function(ctx, neg, pos){
  if(is.null(ctx) || !nzchar(ctx$path) || is.null(ctx$sibling)) return(FALSE)
  # only the species that occur in the group being divided are compared
  active <- colSums(ctx$lv[ctx$members, , drop = FALSE]) > 0
  y1 <- tw_closer(ctx$lv, ctx$sibling, neg, pos, active, ctx$rw, ctx$cw)
  y2 <- if(nchar(ctx$path) >= 2 && !is.null(ctx$uncle))
          tw_closer(ctx$lv, ctx$uncle, neg, pos, active, ctx$rw, ctx$cw) else 0
  id <- strtoi(paste0("1", ctx$path), base = 2L)
  w  <- if((id %% 4) %in% c(1, 2)) -0.5 else 0.5
  score <- y1 + w * y2
  # the sibling is the first (even) child when this node is the second one
  if(substr(ctx$path, nchar(ctx$path), nchar(ctx$path)) == "1")
    return(score > 0)      # sibling is even: the negative half should suit it
  return(score < 0)        # sibling is odd
}

# One dichotomy in the way of the original TWINSPAN (CLASS).
tw_divide_hill <- function(y, opt, ctx = NULL){
  rw <- if(is.null(ctx$rw)) NULL else ctx$rw[ctx$members]
  w  <- if(isTRUE(opt$downweight))
          tw_downweight(y, method = "hill", frq_lim = opt$frq_lim,
                        w_min = opt$cwt_min, rw = rw) else NULL
  if(!is.null(ctx$cw)) w <- if(is.null(w)) ctx$cw else w * ctx$cw
  ra <- tw_ra(y, w = w, rw = rw)
  x  <- ra$sample
  # the original abandons a division whose axis carries next to nothing
  if(!any(x != 0) || ra$eig <= 1e-5) return(NULL)
  rng <- range(x)
  if(rng[2] <= -rng[1]) x <- -x    # the longer end of the axis is positive
  for(i in seq_len(opt$polish_iter)) x <- tw_polish(y, x, opt, rw, ctx$cw)
  rng <- range(x)
  if(rng[2] - rng[1] <= 0) return(NULL)
  mid <- sum(rng) / 2
  pos <- x >= mid
  if(all(pos) || !any(pos)) return(NULL)
  if(!is.null(ctx) &&
     tw_swap(ctx, ctx$members[x <= mid], ctx$members[x >= mid])){
    x   <- -x
    mid <- -mid
    pos <- !pos
  }
  ind <- NULL
  if(opt$max_indicators > 0){
    ind <- tw_indicator_hill(y, x, opt, rw, ctx$sp)
    if(!is.null(ind) && any(ind$positive) && !all(ind$positive))
      pos <- ind$positive
  } else {
    # no indicators (the species classification of the original): the zone
    # of the division is the middle one, so the stands are simply divided
    # at the middle of the range of the polished axis
    hlf  <- 0.5 * opt$cr_long * (rng[2] - rng[1])
    zone <- tw_zone(x, mid - hlf, mid + hlf, opt$mz_out, opt$mz_crit)
    pos  <- zone > (1 + opt$mz_crit + 2 * opt$mz_out) %/% 2
  }
  return(list(positive   = pos,
              eig        = ra$eig,
              primary    = ra$sample,
              refined    = x,
              preference = tw_preference(y, pos),
              indicator  = ind))
}
