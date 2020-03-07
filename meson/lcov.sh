#! /bin/sh

GCOV_IN=$MESON_BUILD_ROOT
LCOV_OUT=$MESON_BUILD_ROOT/coverage
if [ -e $LCOV_OUT ]; then
    rm -r $LCOV_OUT
fi
mkdir -p $LCOV_OUT
lcov --base-directory $MESON_BUILD_ROOT \
     --capture --initial --directory $GCOV_IN --output-file $LCOV_OUT/base.info
ninja -C $MESON_BUILD_ROOT test
lcov --base-directory $MESON_BUILD_ROOT \
     --capture --directory $GCOV_IN --output-file $LCOV_OUT/test.info
lcov --base-directory $MESON_BUILD_ROOT \
     --add-tracefile $LCOV_OUT/base.info \
     --add-tracefile $LCOV_OUT/test.info \
     --output-file $LCOV_OUT/total.info
lcov --base-directory $MESON_BUILD_ROOT \
     --remove $LCOV_OUT/total.info '/usr/include/*' '/usr/lib/*' \
     -o $LCOV_OUT/filtered.info
genhtml --prefix src --ignore-errors source $LCOV_OUT/filtered.info\
        --legend --title "commit SHA1" --output-directory=$LCOV_OUT
