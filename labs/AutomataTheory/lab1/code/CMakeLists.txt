cmake_minimum_required(VERSION 3.16)
project(AutomataTheory-qt LANGUAGES CXX)

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

set(CMAKE_AUTOMOC ON)
set(CMAKE_AUTOUIC ON)

find_package(Qt6 COMPONENTS Widgets REQUIRED)
find_package(yaml-cpp REQUIRED)

add_executable(${PROJECT_NAME}
  src/main.cpp
  src/pyramid.cpp
  src/simulation.cpp
  src/colordialog.cpp
  src/initialfilldialog.cpp
  src/pyramidwidget.cpp
  src/mainwindow.cpp
)

target_compile_definitions(${PROJECT_NAME} PRIVATE QT_NO_KEYWORDS)
target_include_directories(${PROJECT_NAME} PRIVATE src)

target_link_libraries(${PROJECT_NAME} PRIVATE
  Qt6::Widgets
  yaml-cpp
)