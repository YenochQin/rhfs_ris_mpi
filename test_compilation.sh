#!/bin/bash

echo "========================================"
echo "Testing if modifications are compiled"
echo "========================================"

# Force a clean rebuild to ensure modifications are included
if [ -d "build" ]; then
    echo "Removing old build directory..."
    rm -rf build
fi

mkdir build && cd build

echo "Configuring with CMake..."
cmake .. -DCMAKE_BUILD_TYPE=Debug

echo "Building libraries and rhfs90mpi..."
make libmod lib9290 mpi90 rhfs90mpi 2>&1 | tee build.log

if [ -f "bin/rhfs90mpi" ]; then
    echo "✓ rhfs90mpi built successfully"
    echo "✓ Binary timestamp: $(stat -c %y bin/rhfs90mpi)"
    
    # Check if the binary contains our debug strings
    echo "Checking for our modifications in the binary..."
    if strings bin/rhfs90mpi | grep -q "cslhmpi/SETCSLL will add .c extension automatically"; then
        echo "✓ Our modifications are included in the binary"
    else
        echo "✗ Our modifications NOT found in the binary"
    fi
else
    echo "✗ Build failed"
    echo "Last few lines of build log:"
    tail -20 build.log
fi

echo "========================================"