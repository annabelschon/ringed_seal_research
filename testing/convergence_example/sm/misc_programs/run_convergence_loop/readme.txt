This is another way to assimilate some variable. It loops
through the entire SnowModel run, calculates the difference,
adjusts something, performs another SnowModel run, until it
converges on some best-fit requirement.

Other things to consider are:

1) This could use a convergence requirement.

2) If the adjustment does not converge on a single number (like
it flips between big and small numbers), you could do something
to make the parameter evolve slower. Like (in compare_swed.f):

Save ratio from the previous loop. == ratio_old

      A = 0.75 (or something)
      ratio_new = swed_obs / swed_mod

      ratio = A * ratio_new + (1.0 - A) * ratio_old
      cf_precip = cf_precip * ratio

This is called "numerical relaxation". It makes cf_precip
change more slowly than it would if A = 1.0.

