#include "shortdfa.h"

int64_t transducePacked(const int16_t ns, const int16_t q0, const int16_t* tmat, const int16_t* tcounts, const int16_t* fcounts, const int16_t* cstream, int32_t* fstream) {
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
