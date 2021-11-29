cmake_minimum_required(VERSION 3.14)

project(LazyExpression
        VERSION 0.0.1
        DESCRIPTION "Header-only library for variadic and recursive expression templates."
        HOMEPAGE_URL "https://github.com/tirimatangi/LazyExpression"
        LANGUAGES CXX)

# ---- Warning guard ----

# Protect dependents from this project's warnings if the guard isn't disabled
set(LazyExpression_warning_guard SYSTEM)
if(LazyExpression_INCLUDE_WITHOUT_SYSTEM)
  set(LazyExpression_warning_guard "")
endif()

# ---- Declare library ----
# Use release build by default.
if (NOT CMAKE_BUILD_TYPE OR CMAKE_BUILD_TYPE STREQUAL "")
    set(CMAKE_BUILD_TYPE "Release" CACHE STRING "" FORCE)
endif()

set(CMAKE_CXX_FLAGS_RELEASE "-O3 -DNDEBUG")
set(CMAKE_CXX_FLAGS_DEBUG "-Wall -ftemplate-backtrace-limit=0 -DDEBUG -g")

if(${CMAKE_BUILD_TYPE} MATCHES "Release")
  message("CMAKE_CXX_FLAGS_RELEASE is ${CMAKE_CXX_FLAGS_RELEASE}")
endif()

if(${CMAKE_BUILD_TYPE} MATCHES "Debug")
  message("CMAKE_CXX_FLAGS_DEBUG is ${CMAKE_CXX_FLAGS_DEBUG}")
endif()


add_library(LazyExpression INTERFACE)
add_library(LazyExpression::LazyExpression ALIAS LazyExpression)

target_include_directories(LazyExpression
        ${LazyExpression_warning_guard}
        INTERFACE
        "$<BUILD_INTERFACE:${PROJECT_SOURCE_DIR}/include>")

target_compile_features(LazyExpression INTERFACE cxx_std_17)

# ---- Install ----

include(CMakePackageConfigHelpers)
include(GNUInstallDirs)

set(LazyExpression_directory "LazyExpression-${PROJECT_VERSION}")
set(LazyExpression_include_directory
        "${CMAKE_INSTALL_INCLUDEDIR}/${LazyExpression_directory}")

install(DIRECTORY "${PROJECT_SOURCE_DIR}/include/"
        DESTINATION "${LazyExpression_include_directory}")

install(TARGETS LazyExpression
        EXPORT LazyExpressionTargets
        INCLUDES DESTINATION "${LazyExpression_include_directory}")

write_basic_package_version_file(
        LazyExpressionConfigVersion.cmake
        COMPATIBILITY SameMajorVersion
        ARCH_INDEPENDENT)

set(LazyExpression_install_cmakedir
        "${CMAKE_INSTALL_LIBDIR}/cmake/${LazyExpression_directory}")

install(FILES
        "${PROJECT_SOURCE_DIR}/cmake/LazyExpressionConfig.cmake"
        "${PROJECT_BINARY_DIR}/LazyExpressionConfigVersion.cmake"
        DESTINATION "${LazyExpression_install_cmakedir}")

install(EXPORT LazyExpressionTargets
        NAMESPACE LazyExpression::
        DESTINATION "${LazyExpression_install_cmakedir}")
