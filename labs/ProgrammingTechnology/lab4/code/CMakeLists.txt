cmake_minimum_required(VERSION 3.16)
project(lab4 LANGUAGES CXX ASM)

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

set(CMAKE_AUTOMOC ON)
find_package(Qt6 COMPONENTS Widgets REQUIRED)
find_package(OpenSSL REQUIRED)
find_package(PkgConfig REQUIRED)
pkg_check_modules(PQXX REQUIRED libpqxx)

include_directories(${PQXX_INCLUDE_DIRS})
link_directories(${PQXX_LIBRARY_DIRS})

# libcrypto (explicit loading)
add_library(crypto SHARED
    libcrypto/crypto.cpp
    libcrypto/hash.S
)
target_compile_definitions(crypto PRIVATE QT_NO_KEYWORDS)
target_include_directories(crypto PRIVATE libcrypto)
target_link_libraries(crypto PRIVATE OpenSSL::SSL OpenSSL::Crypto)

# libdialogs (implicit loading)
add_library(dialogs SHARED
    libdialogs/dialogs.cpp
    libdialogs/database.cpp
)
target_compile_definitions(dialogs PRIVATE QT_NO_KEYWORDS)
target_include_directories(dialogs PRIVATE libdialogs)
target_link_libraries(dialogs PRIVATE Qt6::Widgets ${PQXX_LIBRARIES} OpenSSL::SSL OpenSSL::Crypto dl)

# main executable
add_executable(lab4
    main/main.cpp
    main/mainwindow.cpp
)
target_compile_definitions(lab4 PRIVATE QT_NO_KEYWORDS)
target_include_directories(lab4 PRIVATE main libdialogs)
target_link_libraries(lab4 PRIVATE Qt6::Widgets dialogs)
