CVX-in-R
========

Temporary repository for a prototype R port of CVX

Demonstration video (< 2 minute, https://www.youtube.com/watch?v=fh_x3-gYM2E)

TODO:
  - automatically add DCP rules when curvature and monotonicity are defined for a CVX function
  - use range of CVX objects and functions to check convexity
  - allow CVX functions should have implementations to deal with numeric values
  - construct problem matrix as in CVX (then write shims for solvers)
  - deal with free variables
  - pre-solve to eliminate variables, improve problem conditioning etc.
  - evaluate objective
  - recover dual
  - feasibility (phase I) solving
  - return output status of solver
