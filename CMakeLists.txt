cmake_minimum_required(VERSION 3.29)

project("Cult of bound variable" VERSION 0.0.1)

set(CMAKE_EXPORT_COMPILE_COMMANDS ON)

add_executable(um)
target_sources(um PRIVATE src/um.cpp)
set_property(TARGET um PROPERTY CXX_STANDARD 17)

add_executable(um2)
target_sources(um2 PRIVATE src/um2.cpp)
set_property(TARGET um2 PROPERTY CXX_STANDARD 17)

add_executable(umc)
target_sources(umc PRIVATE src/um.c src/stack.c src/stack.h)
set_property(TARGET umc PROPERTY C_STANDARD 23)
target_compile_definitions(umc PUBLIC __STDC_WANT_LIB_EXT1__=1)
target_compile_options(umc PUBLIC -Wall)
