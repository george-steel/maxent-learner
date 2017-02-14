/*
Copyright © 2016-2017 George Steel and Peter Jurgec

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 2 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.
*/

#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <math.h>

// Optimized functions to perfor calculations on packed DFAs for FFI use by the WeightedDFA module.
// All functions read from and write into memory buffers allocted by Haskell using Data.Vector.Storable

int64_t transducePackedShort(const int16_t ns, const int16_t q0,
                             const int16_t* restrict tmat, const int16_t* restrict tcounts, const int16_t* restrict fcounts,
                             const int16_t* restrict cstream, int32_t* restrict fstream) {
    int64_t tacc = 0;
    int32_t wacc = 0;

    int16_t st = q0;
    const int16_t* citer = cstream;
    const int32_t* fiter = fstream;

    while(1){
        const int16_t c = *citer;
        if (c == -2){
            return tacc;
        }
        if (c == -1){
            tacc += (wacc + fcounts[st]) * (*fiter);
            wacc = 0;
            st = q0;
            ++fiter; ++citer;
        }else{
            int i = (c*ns) + st;
            st = tmat[i];
            wacc += tcounts[i];
            ++citer;
        }
    }
}

static inline int indexScalarTable(int ns, int q, int c){
    return c * ns + q;
}

static inline int indexVecTable(int ns, int dims, int q, int c){
    return (c * ns + q) * dims;
}

static inline void accumVec16(int sz, const int16_t* restrict in, int32_t* restrict out){
    for (int i = 0; i < sz; ++i){
        out[i] += in[i];
    }
}
static inline void accumTimesVec32(int sz, int32_t s, const int32_t* restrict in, int64_t* restrict out){
    for (int i = 0; i < sz; ++i){
        out[i] += s * in[i];
    }
}
static inline void zeroVec32(int sz, int32_t* v){
    memset(v,0,4*sz);
}
static inline void zeroVec64(int sz, int64_t* v){
    memset(v,0,8*sz);
}

void transducePackedMulti(const int16_t ns, const int16_t q0, const int16_t dims,
                          const int16_t* restrict tmat, const int16_t* restrict tcounts, const int16_t* restrict fcounts,
                          const int16_t* restrict cstream, int32_t* restrict fstream,
                          int64_t* restrict out) {
    zeroVec64(dims,out);
    int32_t wacc[dims];
    zeroVec32(dims,wacc);

    int16_t st = q0;
    const int16_t* citer = cstream;
    const int32_t* fiter = fstream;

    while(1){
        const int16_t c = *citer;
        if (c == -2){
            return;
        }
        if (c == -1){
            accumVec16(dims, fcounts + (st*dims), wacc);
            accumTimesVec32(dims, *fiter, wacc, out);
            zeroVec32(dims,wacc);
            st = q0;
            ++fiter; ++citer;
        }else{
            int i = (c*ns) + st;
            st = tmat[i];
            accumVec16(dims, tcounts+ (i*dims), wacc);
            ++citer;
        }
    }
}

void weightExpVec(const int16_t ns, const int16_t nc, const int16_t dims,
                  const int16_t* restrict tcounts, const int16_t* restrict fcounts,
                  const double* restrict weights,
                  double* restrict tprob, double* restrict tvec, double* restrict fprob, double* restrict fvec){
    //
    for (int t = 0; t < ns*nc; ++t){
        double lp = 0;
        for (int i = 0; i < dims; ++i)
            lp += tcounts[t*dims + i] * weights[i];
        double p = exp(-lp);
        tprob[t] = p;
        for (int i = 0; i < dims; ++i)
            tvec[t*dims + i] = p * tcounts[t*dims + i];
    }

    for (int t = 0; t < ns; ++t){
        double lp = 0;
        for (int i = 0; i < dims; ++i)
            lp += fcounts[t*dims + i] * weights[i];
        double p = exp(-lp);
        fprob[t] = p;
        for (int i = 0; i < dims; ++i)
            fvec[t*dims + i] = p * fcounts[t*dims + i];
    }
}


void weightExpPartial(const int16_t ns, const int16_t nc, const int16_t dims,
                      const int16_t* restrict tcounts, const int16_t* restrict fcounts,
                      const double* restrict weights, const double* restrict dir,
                      double* restrict tprob, double* restrict tvec, double* restrict fprob, double* restrict fvec){
    //
    for (int t = 0; t < ns*nc; ++t){
        double lp = 0;
        double v = 0;
        for (int i = 0; i < dims; ++i){
            lp += tcounts[t*dims + i] * weights[i];
            v += tcounts[t*dims + i] * dir[i];
        }
        double p = exp(-lp);
        tprob[t] = p;
        tvec[t] = p * v;
    }

    for (int t = 0; t < ns; ++t){
        double lp = 0;
        double v = 0;
        for (int i = 0; i < dims; ++i){
            lp += fcounts[t*dims + i] * weights[i];
            v += fcounts[t*dims + i] * dir[i];
        }
        double p = exp(-lp);
        fprob[t] = p;
        fvec[t] = p * v;
    }
}



inline void expVecTimesPlusAccum(int dim, double p1, const double* restrict v1, double p2, const double* restrict v2,
    double* restrict pacc, double* restrict vacc){
    *pacc += p1 * p2;
    for (int i = 0; i < dim; ++i){
        vacc[i] += (p2 *v1[i]) + (p1 * v2[i]);
    }
}

void expsByLengthVec(const int16_t ns, const int16_t nc, const int16_t dims, const int16_t q0, const int16_t maxlen,
                     const int16_t* restrict tmat, const double* restrict tprob, const double* restrict tvec, const double* restrict fprob, const double* restrict fvec,
                     double* outp, double* outv) {
    double* accump = calloc(((int)ns)*(maxlen+1), sizeof(double));
    double* accumv = calloc(((int)ns)*dims*(maxlen+1), sizeof(double));
    accump[q0] = 1;

    for (int n=1; n <= maxlen; ++n)
        for (int q = 0; q < ns; ++q)
            for (int c = 0; c < nc; ++c) {
                int qprime = tmat[c*ns+q];
                expVecTimesPlusAccum(dims, tprob[c*ns+q], tvec+(c*ns+q)*dims, accump[(n-1)*ns+q], accumv+((n-1)*ns+q)*dims,
                    accump+(n*ns+qprime), accumv+(n*ns+qprime)*dims);
            }

    for (int n=0; n <= maxlen; ++n)
        for (int q = 0; q < ns; ++q){
            expVecTimesPlusAccum(dims, accump[n*ns+q], accumv+(n*ns+q)*dims, fprob[q], fvec+q*dims,
                outp+n, outv+n*dims);
        }

    free(accump);
    free(accumv);
}

void expsByLengthDouble(const int16_t ns, const int16_t nc, const int16_t q0, const int16_t maxlen,
                        const int16_t* restrict tmat, const double* restrict tprob, const double* restrict tvec, const double* restrict fprob, const double* restrict fvec,
                        double* outp, double* outv) {
    double* accump = calloc(((int)ns)*(maxlen+1), sizeof(double));
    double* accumv = calloc(((int)ns)*(maxlen+1), sizeof(double));
    accump[q0] = 1;

    for (int n=1; n <= maxlen; ++n)
        for (int q = 0; q < ns; ++q)
            for (int c = 0; c < nc; ++c) {
                int qprime = tmat[c*ns+q];
                double p1 = tprob[c*ns+q];
                double p2 = accump[(n-1)*ns+q];
                double v1 = tvec[c*ns+q];
                double v2 = accumv[(n-1)*ns+q];
                accump[n*ns+qprime] += p1*p2;
                accumv[n*ns+qprime] += p2*v1 + p1*v2;
            }

    for (int n=0; n <= maxlen; ++n){
        outp[n] = 0;
        outv[n] = 0;
        for (int q = 0; q < ns; ++q){
            double p1 = accump[n*ns+q];
            double p2 = fprob[q];
            double v1 = accumv[n*ns+q];
            double v2 = fvec[q];
            outp[n] += p1*p2;
            outv[n] += p2*v1 + p1*v2;
        }
    }

    free(accump);
    free(accumv);
}
