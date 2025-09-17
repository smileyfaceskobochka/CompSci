cmake_minimum_required(VERSION 3.15)

project(lab1 VERSION 1.0.0 LANGUAGES C)

# Export compile commands for IDE support
set(CMAKE_EXPORT_COMPILE_COMMANDS ON)

# Default build type
if(NOT CMAKE_BUILD_TYPE)
    set(CMAKE_BUILD_TYPE Release)
endif()

# Set C standard
set(CMAKE_C_STANDARD 17)
set(CMAKE_C_STANDARD_REQUIRED ON)

# Sources
set(SOURCES
    src/main.c
    src/user_interface.c
    src/ui/topbar.c
    src/ui/components.c
    src/options.c
    src/ui/dialogs.c
    src/game/game.c
    src/game/ui_game.c
)

# Platform-specific sources
if(WIN32)
    list(APPEND SOURCES src/sys/file_dialog_win.c)
else()
    list(APPEND SOURCES src/sys/file_dialog_zenity.c)
endif()

# Executable
add_executable(lab1 ${SOURCES})

# Enable compiler warnings and features
target_compile_options(lab1 PRIVATE -Wall -Wextra -Wpedantic)
target_compile_features(lab1 PRIVATE c_std_17)

# Suppress pedantic warnings for options.c due to compound literals in global initializers
set_source_files_properties(src/options.c PROPERTIES COMPILE_FLAGS "-Wno-pedantic")

# Suppress unused parameter warnings for user_interface.c due to raygui.h
set_source_files_properties(src/user_interface.c PROPERTIES COMPILE_FLAGS "-Wno-unused-parameter")

# Include directories
target_include_directories(lab1 PRIVATE src src/ui)

# Dependencies
if(WIN32)
    # Use local MinGW-w64 raylib libraries
    set(RAYLIB_LIBRARY_DIR "${CMAKE_CURRENT_SOURCE_DIR}/lib")
    set(RAYLIB_INCLUDE_DIR "${CMAKE_CURRENT_SOURCE_DIR}/lib")
    target_include_directories(lab1 PRIVATE ${RAYLIB_INCLUDE_DIR})
    target_link_libraries(lab1 PRIVATE ${RAYLIB_LIBRARY_DIR}/libraylib.a)
else()
    # Try to find raylib on Linux
    find_package(raylib QUIET)
    if(raylib_FOUND)
        target_link_libraries(lab1 PRIVATE raylib)
    else()
        find_package(PkgConfig QUIET)
        if(PKG_CONFIG_FOUND)
            pkg_check_modules(RAYLIB raylib)
            if(RAYLIB_FOUND)
                target_include_directories(lab1 PRIVATE ${RAYLIB_INCLUDE_DIRS})
                target_link_libraries(lab1 PRIVATE ${RAYLIB_LIBRARIES})
            else()
                message(WARNING "raylib not found via pkg-config; attempting to link -lraylib")
                target_link_libraries(lab1 PRIVATE raylib)
            endif()
        else()
            message(WARNING "PkgConfig not found; attempting to link -lraylib")
            target_link_libraries(lab1 PRIVATE raylib)
        endif()
    endif()
endif()

# Additional libraries
if(UNIX AND NOT APPLE)
    target_link_libraries(lab1 PRIVATE m pthread dl)
endif()
if(WIN32)
    target_link_libraries(lab1 PRIVATE winmm gdi32 user32 shell32)
endif()

# Output directory
set_target_properties(lab1 PROPERTIES
    RUNTIME_OUTPUT_DIRECTORY "${CMAKE_BINARY_DIR}/bin"
)

# Assets: copy at build time to runtime dir
add_custom_command(TARGET lab1 POST_BUILD
    COMMAND ${CMAKE_COMMAND} -E copy_directory
        "${CMAKE_CURRENT_SOURCE_DIR}/assets"
        "$<TARGET_FILE_DIR:lab1>/assets"
)

# Post-build: copy runtime dependencies (Windows)
if(WIN32)
    add_custom_command(TARGET lab1 POST_BUILD
        COMMAND ${CMAKE_COMMAND} -E copy_if_different
            "${CMAKE_CURRENT_SOURCE_DIR}/lib/raylib.dll"
            "$<TARGET_FILE_DIR:lab1>/raylib.dll"
        VERBATIM)
endif()

# Install rules
install(TARGETS lab1
    RUNTIME DESTINATION bin
)
install(DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}/assets" DESTINATION bin)
if(WIN32)
    install(FILES "${CMAKE_CURRENT_SOURCE_DIR}/lib/raylib.dll" DESTINATION bin)
endif()

# Dist packages (simple folder copy)
add_custom_target(dist-linux
    COMMAND ${CMAKE_COMMAND} -E make_directory "${CMAKE_BINARY_DIR}/dist/linux"
    COMMAND ${CMAKE_COMMAND} -E copy "$<TARGET_FILE:lab1>" "${CMAKE_BINARY_DIR}/dist/linux/"
    COMMAND ${CMAKE_COMMAND} -E copy_directory "${CMAKE_CURRENT_SOURCE_DIR}/assets" "${CMAKE_BINARY_DIR}/dist/linux/assets"
    BYPRODUCTS "${CMAKE_BINARY_DIR}/dist/linux/lab1"
    COMMENT "Creating Linux dist folder with assets"
)
add_dependencies(dist-linux lab1)

if(WIN32)
    add_custom_target(dist-windows
        COMMAND ${CMAKE_COMMAND} -E make_directory "${CMAKE_BINARY_DIR}/dist/windows"
        COMMAND ${CMAKE_COMMAND} -E copy "$<TARGET_FILE:lab1>" "${CMAKE_BINARY_DIR}/dist/windows/"
        COMMAND ${CMAKE_COMMAND} -E copy_if_different "${CMAKE_CURRENT_SOURCE_DIR}/lib/raylib.dll" "${CMAKE_BINARY_DIR}/dist/windows/"
        COMMAND ${CMAKE_COMMAND} -E copy_directory "${CMAKE_CURRENT_SOURCE_DIR}/assets" "${CMAKE_BINARY_DIR}/dist/windows/assets"
        BYPRODUCTS "${CMAKE_BINARY_DIR}/dist/windows/lab1.exe" "${CMAKE_BINARY_DIR}/dist/windows/raylib.dll"
        COMMENT "Creating Windows dist folder with assets"
    )
    add_dependencies(dist-windows lab1)
endif()

# Custom clean target
add_custom_target(clean-all
    COMMAND ${CMAKE_COMMAND} -E remove_directory "${CMAKE_BINARY_DIR}/bin"
    COMMAND ${CMAKE_COMMAND} -E remove_directory "${CMAKE_BINARY_DIR}/dist"
    COMMENT "Cleaning all build artifacts"
)
