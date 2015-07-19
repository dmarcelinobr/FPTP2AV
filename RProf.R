# Profiling
# Evaluate shortFunction() for 100 times
replicate(n = 2, fptp2av(data=IR, link=party_chains))

Rprof("Rprof.out")
fptp2av(data=IR, link=party_chains)
Rprof(NULL)
summaryRprof(filename = "Rprof.out")
# Alternative to summary
proftable("Rprof.out", lines=10)