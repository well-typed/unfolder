#!/bin/bash

GHCARGS="-Wall -fforce-recomp -ddump-to-file -ddump-simpl -ddump-stg-final -ddump-cmm"

function call_ghc() {
    case $1 in
      ghc92) ~/.ghcup/isolated/9.2.8/bin/ghc ${GHCARGS} -o ./exe/$2 -ddump-file-prefix=./dump/$2.                     "${@:3}";;
      ghc94) ~/.ghcup/isolated/9.4.8/bin/ghc ${GHCARGS} -o ./exe/$2 -ddump-file-prefix=./dump/$2  -dno-typeable-binds "${@:3}";;
    esac
}

rm -f results.md

##
## Swap
##

echo -e "\n# swap" >> results.md

for test in CASE FST_SND PREVENT_INLINING
do
    VARIANT=${test}
    echo -e "\n## ${VARIANT}" >> results.md

    for ghc in ghc92 ghc94
    do
        for opt in "O0" "O1"
        do
            TARGET="swap-${VARIANT}-${ghc}-${opt}"
            echo ${TARGET} >> results.md
            call_ghc ${ghc} ${TARGET} -main-is Swap -${opt} -D${test} ./src/Swap.hs
            echo -en "\tNumber of selector thunks: " >> results.md
            grep stg_sel_ ./dump/$TARGET.dump-cmm | wc -l >> results.md
            ./exe/${TARGET} +RTS -s 2>&1 | grep residency >> results.md
        done
    done
done

##
## LastBeforeBreak
##

echo -e "\n# lastbeforebreak" >> results.md

for last in BESPOKE_LAST PRELUDE_LAST
do
    for break in BESPOKE_BREAK PRELUDE_BREAK
    do
        for test in PRINT_EQ SEQ_EQ ONE_PRINT TWO_PRINTS
        do
            for elem in Int Integer
            do
                for sameElem in INLINE NOINLINE
                do
                    VARIANT=${test}-${elem}-${sameElem}-${last}-${break}
                    echo -e "\n## ${VARIANT}" >> results.md

                    for ghc in ghc92 ghc94
                    do
                        for opt in "O0" "O1"
                        do
                            TARGET="lastbeforebreak-${VARIANT}-${ghc}-${opt}"
                            echo ${TARGET} >> results.md
                            call_ghc ${ghc} ${TARGET} -main-is LastBeforeBreak -${opt} -DTEST_${test} -DELEM=${elem} -DSAME_ELEM_${sameElem} -D${last} -D${break} ./src/LastBeforeBreak.hs
                            echo -en "\tNumber of selector thunks: " >> results.md
                            grep stg_sel_ ./dump/$TARGET.dump-cmm | wc -l >> results.md
                            ./exe/${TARGET} +RTS -s 2>&1 | grep residency >> results.md
                        done
                    done
                done
            done
        done
    done
done