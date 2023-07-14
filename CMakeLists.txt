cmake_minimum_required(VERSION 3.19)
project(EssaLang)

set(CMAKE_EXPORT_COMPILE_COMMANDS ON)
set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_STANDARD_REQUIRED true)

# Include Essa framework
include(FetchContent)
set(ESSA_UTIL_ONLY ON)
FetchContent_Declare(Essa
    GIT_REPOSITORY https://github.com/essa-software/EssaGUI.git
    GIT_TAG 12a1dea9b3a0d9f0517817a2f49fcd7ebb236ed7
    OVERRIDE_FIND_PACKAGE
)
FetchContent_MakeAvailable(Essa)

find_package(fmt REQUIRED)
add_subdirectory(bootstrap)
add_subdirectory(runtime)
