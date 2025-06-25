# library(tidyr)
# library(dplyr)
library(R6)

game <- R6Class("eightQueens",
                public = list(
                  addQueen = function(pos) {
                    checkmate::assertIntegerish(pos, len = 2, lower = 1, upper = 8, any.missing = FALSE)
                    if (any(apply(private$.possibleMoves, MARGIN = 1, function(x) all(x == pos)))) {
                      private$.queens[[length(private$.queens) + 1]] = pos
                      private$.possibleMoves = private$.newMoves(pos, private$.possibleMoves)
                    } else {
                      stop("no possible move")
                    }
                    if (nrow(private$.possibleMoves) == 0) {
                      private$.gameOver = TRUE
                    }
                    invisible(self)
                  },
                  possibleMoves = function() {
                    private$.possibleMoves
                  },
                  queens = function() {
                    private$.queens
                  },
                  gameOver = function() {
                    private$.gameOver
                  },
                  gameWon = function() {
                    length(private$.queens) >= 8
                  },
                  initialize = function(pos = NULL) {
                    checkmate::assertIntegerish(pos, len = 2, lower = 1, upper = 8, any.missing = FALSE, null.ok = TRUE)
                    private$.gameOver = FALSE
                    if (is.null(pos)) {
                      private$.queens = list()
                      private$.possibleMoves = tidyr::expand_grid(x = 1:8, y = 1:8)
                    } else {
                      private$.queens = list(pos)
                      private$.possibleMoves = private$.newMoves(pos, tidyr::expand_grid(x = 1:8, y = 1:8))
                    }
                  }
                  ),
                private = list(
                  .gameOver = NULL,
                  .possibleMoves = NULL,
                  .queens = NULL,
                  .newMoves = function(pos, possibleMoves) {
                    underThreat = rbind(
                      tidyr::expand_grid(x = pos[[1]], y = 1:8),
                      tidyr::expand_grid(x = 1:8, y = pos[[2]])
                    )
                    diags1 = as.data.frame(t(vapply(1:8, function(x) pos - min(pos) + rep(x, 2), numeric(2))))
                    diags2 = as.data.frame(t(vapply(1:8, function(x) pos + c(-max(pos), max(pos)) + c(x, -x), numeric(2))))
                    diags = rbind(diags1, diags2)
                    colnames(diags) = c("x", "y")
                    underThreat = rbind(
                      underThreat,
                      diags[(diags$x > 0) & (diags$x <= 8) & (diags$y > 0) & (diags$y <= 8), ]
                    )
                    dplyr::anti_join(possibleMoves, underThreat, by = c("x", "y"))
                    }
                )
              )

###
# game$new()
# g1 = game$new()
# g1$possibleMoves()
# g1$addQueen(c(4,2))
# length(g1$queens())
# g1$possibleMoves()
# g1$gameOver()
# g1$gameWon()
# g1$queens()
# g2 = g1$clone()$addQueen(c(2,1))
# g2$queens()
###


parEightQueensBroad <- function(num.loops = 8) {
  next.games <- function(root) {
    moves <- root$possibleMoves()
    games <- list()
    for(i in seq_len(nrow(moves))) {
      game <- root$clone()$addQueen(as.numeric(moves[i,]))
      if (!game$gameOver() | game$gameWon()) {
        games[[length(games) + 1]] <- game
      }
    }
    games
  }
  n.cores <- parallel::detectCores()
  clust <- parallel::makeCluster(n.cores)
  Games <- next.games(game$new())
  for (k in seq_len(num.loops)) {
    len <- length(Games)
    print(sprintf("evaluating level %d: %d games", k, len))
    Games = parallel::parLapply(clust, Games, next.games)
    Games = unlist(Games)
  }
  Games
}

lapplyEightQueensBroad <- function(num.loops = 8) {
  next.games <- function(root) {
    moves <- root$possibleMoves()
    games <- list()
    for(i in seq_len(nrow(moves))) {
      game <- root$clone()$addQueen(as.numeric(moves[i,]))
      if (!game$gameOver() | game$gameWon()) {
        games[[length(games) + 1]] <- game
      }
    }
    games
  }
  Games <- next.games(game$new())
  for (k in seq_len(num.loops)) {
    len <- length(Games)
    print(sprintf("evaluating level %d: %d games", k, len))
    Games = lapply(Games, next.games)
    Games = unlist(Games)
  }
  Games
}

forEightQueensBroad <- function(num.loops = 8) {
  next.games <- function(root, min.moves = 0) {
    moves <- root$possibleMoves()
    games <- list()
    if (nrow(moves) > min.moves) {
      for(i in seq_len(nrow(moves))) {
        game <- root$clone()$addQueen(as.numeric(moves[i,]))
        if (!game$gameOver() | game$gameWon()) {
          games[[length(games) + 1]] <- game
        }
      }
    }
    games
  }
  Games <- next.games(game$new())
  for (k in seq_len(num.loops)) {
    len <- length(Games)
    print(sprintf("evaluating level %d: %d games", k+1, len))
    for (l in seq_along(Games)) {
      print(sprintf("Game %d of %d", l, len))
      Games[[l]] = next.games(Games[[l]], num.loops - k)
    }
    Games = unlist(Games)
  }
  Games
}

# t.par = system.time({test = parEightQueensBroad(3)})
# t.lapply = system.time({test = lapplyEightQueensBroad(2)})
t.for = system.time({test = forEightQueensBroad(3)})

###

# L = list(c(1,2), c(2,3), c(7,2))
# LL = vapply(L, function(x) sprintf("(%d, %d)", x[[1]], x[[2]]), character(1))
# paste(LL, collapse = ", ")
# cat("configuartion: ", paste(LL, collapse = ", "), "\n")

eightQueensDeep <- function(randomSearch = FALSE) {
  next.games <- function(root, randomSearch = FALSE) {
    moves <- root$possibleMoves()
    if (randomSearch) {
      moves <- moves[sample(seq_len(nrow(moves))), ]
    }
    games <- list()
    for(i in seq_len(nrow(moves))) {
      game <- root$clone()$addQueen(as.numeric(moves[i,]))
      if (!game$gameOver() | game$gameWon()) {
        games[[length(games) + 1]] <- game
      }
    }
    games
  }
  
  search.deep <- function(Game) {
    msg = vapply(Game$queens(), function(x) sprintf("(%d, %d)", x[[1]], x[[2]]), character(1))
    cat("configuartion: ", paste(msg, collapse = ", "), "\n")
    if (Game$gameWon()) {
      return(Game$queens())
    } else {
      newGames <- next.games(Game, randomSearch = randomSearch)
      for (newGame in newGames) {
        config <- search.deep(newGame)
        if (!is.null(config)) {
          return(config)
          break
        } 
      }
    }
    return(NULL)
  }
  search.deep(game$new())
}

as.mat <- function(queens) {
  mat <- matrix(" ", 8, 8)
  for (q in queens) {
    mat[q[[1]], q[[2]]] <- "+"
  }
  mat
}

test.deep = eightQueensDeep(TRUE)
as.mat(test.deep)

test.deep.long <- lapply(1:5, function(x) eightQueensDeep(TRUE))
lapply(test.deep.long, as.mat)
