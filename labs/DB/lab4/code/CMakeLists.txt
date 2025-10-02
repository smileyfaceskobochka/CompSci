cmake_minimum_required(VERSION 3.16)
project(pozordom-qt LANGUAGES CXX)

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

set(CMAKE_AUTOMOC ON)
find_package(Qt6 COMPONENTS Widgets REQUIRED)
find_package(OpenSSL REQUIRED)
find_package(PkgConfig REQUIRED)
pkg_check_modules(PQXX REQUIRED libpqxx)

include_directories(${PQXX_INCLUDE_DIRS})
link_directories(${PQXX_LIBRARY_DIRS})

add_executable(pozordom
    src/main.cpp
    src/mainwindow.h
    src/mainwindow.cpp
    src/dialogs.h
    src/dialogs.cpp
    src/database.h
    src/database.cpp
)

target_compile_definitions(pozordom PRIVATE QT_NO_KEYWORDS) # avoid macro collisions if needed
target_include_directories(pozordom PRIVATE src)
target_link_libraries(pozordom PRIVATE Qt6::Widgets ${PQXX_LIBRARIES} OpenSSL::SSL OpenSSL::Crypto)
