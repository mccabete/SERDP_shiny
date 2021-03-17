# Data Sampling Structure

5 * 8 * 2 * 12

# Site Level (DoD Base) = 5
## Burn Unit Level = 8
### Plot Level = 2
#### 1m x 1m Quadrat Level = 12

site <- letters[1:5]
burn <- rep(letters[6:13],5)
plot <- rep(c("y","z"),40)
quad <- rep(seq(1,12,1),480)

cbind(site,burn,plot,quad)

15*16*2

480/2

