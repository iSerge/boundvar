cmake_minimum_required(VERSION 3.29)

project("Cult of bound variable" VERSION 0.0.1)

set(CMAKE_EXPORT_COMPILE_COMMANDS ON)

add_executable(um)
target_sources(um PRIVATE src/um.cpp)
set_property(TARGET um PROPERTY CXX_STANDARD 17)

add_executable(um2)
target_sources(um2 PRIVATE src/um2.cpp)
set_property(TARGET um2 PROPERTY CXX_STANDARD 17)
