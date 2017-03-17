Gp1 = c(212, 288, 188, 276, 245, 182, 228, 221, 184, 202, 260, 278, 251, 160, 145)
Gp2 = c(219, 189, 215, 186, 158, 163, 185, 189, 250, 179, 192, 199, 192, 179, 195)

m = 100000;  d = numeric(m)
n1 = length(Gp1);  n2 = length(Gp2)
obs = mean(Gp1) - mean(Gp2)

for (i in 1:m) { 
    smp = sample(c(Gp1, Gp2), n1 + n2)
    d[i] = mean(smp[1:n1]) - mean(smp[n1+1:n2]) }

mean( abs(d) >= obs )
hist(d);  abline(v=c(obs, -obs), lty="dashed")
