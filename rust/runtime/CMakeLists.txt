cmake_minimum_required(VERSION 3.20)
project(esl-runtime)

add_library(eslrt
    src/prelude/fmt.cpp
    src/prelude/iter.cpp
    src/prelude/ops.cpp
    src/prelude/panic.cpp
)
target_include_directories(eslrt PUBLIC include)

install(TARGETS eslrt
    LIBRARY DESTINATION lib
    ARCHIVE DESTINATION lib
    RUNTIME DESTINATION bin
    INCLUDES DESTINATION include
)
install(DIRECTORY include
    DESTINATION .
)
