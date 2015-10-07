n = 20
p = 0.2
plot( seq(0,n), dbinom( seq(0,n), n, p), type='h', lwd=10,
      xlim=c(-1,n+1), xlab='X', ylab='Probability')