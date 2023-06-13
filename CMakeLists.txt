cmake_minimum_required(VERSION 3.19)
project(EssaLang)

set(CMAKE_EXPORT_COMPILE_COMMANDS ON)
set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_STANDARD_REQUIRED true)

# Include Essa framework
include(FetchContent)
FetchContent_Declare(Essa
    GIT_REPOSITORY https://github.com/essa-software/EssaGUI.git
    GIT_TAG e3793108769b9d3f746de50ede6d84dc8cf8cd8e
    OVERRIDE_FIND_PACKAGE
)
FetchContent_MakeAvailable(Essa)

find_package(fmt REQUIRED)
add_subdirectory(bootstrap)
