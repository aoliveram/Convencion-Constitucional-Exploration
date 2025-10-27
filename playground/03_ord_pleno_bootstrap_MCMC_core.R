# --- Minimal Example for pscl::ideal (Version 1.5.9 Compatible) ---

# Load necessary libraries
library(pscl)
library(wnominate) # For rollcall()

# 1. Create Sample Data (Same as before)
set.seed(123)
n_legis <- 10
n_votes <- 30
fake_votes <- matrix(sample(c(0, 1, NA), n_legis * n_votes, replace = TRUE, prob = c(0.45, 0.45, 0.1)),
                     nrow = n_legis, ncol = n_votes)
fake_legis_names <- paste("Legislator", LETTERS[1:n_legis])
rownames(fake_votes) <- fake_legis_names
colnames(fake_votes) <- paste("Vote", 1:n_votes)
fake_votes_df <- as.data.frame(fake_votes)

# 2. Create Rollcall Object (Same as before)
rc_minimal <- rollcall(data = fake_votes_df, yea = 1, nay = 0, missing = NA,
                       legis.names = rownames(fake_votes_df),
                       desc = "Minimal IDEAL Test (v1.5.9)")
print(summary(rc_minimal))
stopifnot(length(unique(rownames(rc_minimal$votes))) == nrow(rc_minimal$votes))

# 3. Run pscl::ideal() - REMOVING store.scores
cat("\nRunning minimal ideal() example (v1.5.9 compatible)...\n")
ideal_minimal_result <- NULL
tryCatch({
  ideal_minimal_result <- pscl::ideal(object = rc_minimal,
                                      d = 1,              # 1 dimension
                                      maxiter = 2500,     # Fewer iterations for speed
                                      burnin = 500,       # Shorter burn-in
                                      thin = 5,           # Less thinning
                                      impute = TRUE,      # Handle NAs
                                      normalize = TRUE,   # Normalize scores
                                      # store.scores = TRUE, # <-- REMOVED
                                      verbose = TRUE)     # Show progress
}, error = function(e) {
  cat("ERROR during minimal ideal() run:\n")
  print(e)
})

# 4. Inspect the Results Object (v1.5.9)
if (!is.null(ideal_minimal_result)) {
  cat("\nideal() ran successfully (v1.5.9).\n")
  
  # Check the names of the elements within the returned object
  cat("\nNames within the ideal object:\n")
  print(names(ideal_minimal_result))
  
  # Look for legislator scores - often named 'x'
  if ("x" %in% names(ideal_minimal_result)) {
    cat("\nLegislator Point Estimates (ideal_minimal_result$x):\n")
    # This is likely a matrix of the posterior means or final MCMC estimates
    print(head(ideal_minimal_result$x))
    # Check dimensions - should be n_legis x d (e.g., 10 x 1)
    cat("\nDimensions of x:", dim(ideal_minimal_result$x), "\n")
  } else {
    cat("\nLegislator scores ('x') not found directly in the object.\n")
  }
  
  # Try running summary() and see what it returns
  cat("\nOutput of summary(ideal_minimal_result):\n")
  ideal_summary <- summary(ideal_minimal_result)
  print(ideal_summary) # Check if this contains means/sd
  
  # Check names within the summary object
  cat("\nNames within the summary object:\n")
  print(names(ideal_summary))
  # Does it have summary.x or summary.legis? Probably not.
  
} else {
  cat("\nideal() failed to run.\n")
}
