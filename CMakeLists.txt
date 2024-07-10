cmake_minimum_required(VERSION 3.29)

project("Cult of bound variable" VERSION 0.0.1)

set(CMAKE_EXPORT_COMPILE_COMMANDS ON)

add_executable(um)
target_sources(um PRIVATE src/um.cpp)
target_compile_features(um PUBLIC cxx_std_17)
