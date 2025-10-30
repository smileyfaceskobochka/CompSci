# Windows cross-compilation toolchain using MinGW-w64
set(CMAKE_SYSTEM_NAME Windows)
set(CMAKE_SYSTEM_PROCESSOR x86_64)

# Specify the cross compiler (MinGW-w64)
set(CMAKE_C_COMPILER x86_64-w64-mingw32-gcc)
set(CMAKE_CXX_COMPILER x86_64-w64-mingw32-g++)
set(CMAKE_RC_COMPILER x86_64-w64-mingw32-windres)

# Don't try to run cross-compiled executables
set(CMAKE_CROSSCOMPILING_EMULATOR "")

# Set the root path for cross-compiled libraries
# You'll need to install Qt6 and yaml-cpp for MinGW-w64
set(CMAKE_FIND_ROOT_PATH /usr/x86_64-w64-mingw32)

# Search for programs in the build host directories
set(CMAKE_FIND_ROOT_PATH_MODE_PROGRAM NEVER)

# For libraries and headers in the target directories
set(CMAKE_FIND_ROOT_PATH_MODE_LIBRARY ONLY)
set(CMAKE_FIND_ROOT_PATH_MODE_INCLUDE ONLY)
set(CMAKE_FIND_ROOT_PATH_MODE_PACKAGE ONLY)

# Qt6 specific settings for Windows
set(Qt6_DIR /usr/x86_64-w64-mingw32/lib/cmake/Qt6)
set(yaml-cpp_DIR /usr/x86_64-w64-mingw32/lib/cmake/yaml-cpp)

# Ensure static linking for Windows deployment
set(CMAKE_EXE_LINKER_FLAGS "-static-libgcc -static-libstdc++ -static")
