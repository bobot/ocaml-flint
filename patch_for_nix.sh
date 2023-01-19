#!/bin/bash

GMP_LIB_DIR=$(pkg-config --variable=libdir gmp)
GMP_INC_DIR=$(pkg-config --variable=includedir gmp)
MPFR_LIB_DIR=$(pkg-config --variable=libdir mpfr)
MPFR_INC_DIR=$(pkg-config --variable=includedir mpfr)

for dir in "calcium/lib/calcium" "flint/flint/flint2" "antic/antic/antic" "arb/arb"; do
    if [ "$dir" = "flint/flint/flint2" ]; then
        GMP_LOG="
-   echo \"Invalid GMP directory: \${GMP_DIR}\" >> config.log"
        MPFR_LOG="
-   echo \"Invalid MPFR directory: \${MPFR_DIR}\" >> config.log"
        LINE=41
    else
        GMP_LOG=""
        MPFR_LOG=""
        LINE=39
    fi

patch -p0 -u <<EOF
--- $dir/configure	2022-02-19 23:40:46.275007267 +0100
+++ $dir/configure	2023-01-19 21:02:12.335254402 +0100
@@ -250,$LINE +250,16 @@
 
 LIBS="m"
 
-if [ -d "\${GMP_DIR}/lib" ]; then
-   GMP_LIB_DIR="\${GMP_DIR}/lib"
-   GMP_INC_DIR="\${GMP_DIR}/include"
-elif [ -d "\${GMP_DIR}/lib64" ]; then
-   GMP_LIB_DIR="\${GMP_DIR}/lib64"
-   GMP_INC_DIR="\${GMP_DIR}/include"
-elif [ -d "\${GMP_DIR}/.libs" ]; then
-   GMP_LIB_DIR="\${GMP_DIR}/.libs"
-   GMP_INC_DIR="\${GMP_DIR}"
-else
-   echo "Invalid GMP directory"$GMP_LOG
-   exit 1
-fi
+GMP_LIB_DIR="$GMP_LIB_DIR"
+GMP_INC_DIR="$GMP_INC_DIR"
+
 LIB_DIRS="\${LIB_DIRS} \${GMP_LIB_DIR}"
 INC_DIRS="\${INC_DIRS} \${GMP_INC_DIR}"
 LIBS="\${LIBS} gmp"
 
-if [ -d "\${MPFR_DIR}/lib" ]; then
-   MPFR_LIB_DIR="\${MPFR_DIR}/lib"
-   MPFR_INC_DIR="\${MPFR_DIR}/include"
-elif [ -d "\${MPFR_DIR}/lib64" ]; then
-   MPFR_LIB_DIR="\${MPFR_DIR}/lib64"
-   MPFR_INC_DIR="\${MPFR_DIR}/include"
-elif [ -d "\${MPFR_DIR}/.libs" ]; then
-   MPFR_LIB_DIR="\${MPFR_DIR}/.libs"
-   MPFR_INC_DIR="\${MPFR_DIR}"
-elif [ -d "\${MPFR_DIR}/src/.libs" ]; then
-   MPFR_LIB_DIR="\${MPFR_DIR}/src/.libs"
-   MPFR_INC_DIR="\${MPFR_DIR}/src"
-else
-   echo "Invalid MPFR directory"$MPFR_LOG
-   exit 1
-fi
+MPFR_LIB_DIR=\"$MPFR_LIB_DIR\"
+MPFR_INC_DIR=\"$MPFR_INC_DIR\"
+
 LIB_DIRS="\${LIB_DIRS} \${MPFR_LIB_DIR}"
 INC_DIRS="\${INC_DIRS} \${MPFR_INC_DIR}"
 LIBS="\${LIBS} mpfr"
EOF

done
