cmake_minimum_required(VERSION 3.14)

project(LazyExpressionExamples CXX)

include(FetchContent)
FetchContent_Declare(LazyExpression SOURCE_DIR "${PROJECT_SOURCE_DIR}/..")
FetchContent_MakeAvailable(LazyExpression)

foreach(name IN ITEMS tutorial-example coordinate-transform-example)
  add_executable("${name}" "${name}.cc")
  target_link_libraries("${name}" PRIVATE LazyExpression::LazyExpression)
  target_compile_features("${name}" PRIVATE cxx_std_17)
endforeach()
