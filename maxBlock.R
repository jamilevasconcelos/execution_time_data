# Description: function for sampling the maximum values of a numeric vector
#              according to the block maxima approach
#
# Output: it returns a vector of indices that correspond to the sampled maximum values
# Input: a numeric vector v, the block maxima size sz;
#        and the number of observations nSample in the input vector to be considered starting from 
#        the first elements of v (the default is the size of v)
#
# Obs: (a) As recommended, if nSample is not a multiple of sz, the last block is discarded; 
#      (b) Block maxima is preferable than other approaches when data are not i.i.d.
#      (c) Theoretical properties can be found in EVT literature. See for example
#          Ferreira, Ana; de Haan, Laurens. On the block maxima method in extreme value theory: PWM estimators.
#          Ann. Statist. 43 (2015), no. 1, 276--298.
#          doi:10.1214/14-AOS1280. http://projecteuclid.org/euclid.aos/1418135622.
#      (d) However, it usually requires a large sample of raw data (many observations are discarded)
#          Pay attention that some data may require a large value of sz.
# Author: George Lima -- last update February/2016
#
#
maxBlock <- function(v,sz,nSample=length(v))
    {
        nBlock    = floor(nSample/sz)
        ind       = rep(0,nBlock)
        for (i in 1:nBlock) {
            a = (i-1)*sz+1
            b = i*sz
            vMax = v[a]
            iMax = a
            for (j in a:b) {
                if (vMax < v[j]) {
                    vMax = v[j]
                    iMax = j
                }
            }
            ind[i] = iMax
        }
        return(ind)
    }
