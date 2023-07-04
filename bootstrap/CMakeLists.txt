add_executable(EssaLangBootstrap
    Codegen.cpp
    Esl.cpp
    Lexer.cpp
    Parser.cpp
    Typechecker.cpp
    main.cpp
)
target_link_libraries(EssaLangBootstrap Essa::Util)
essautil_setup_target(EssaLangBootstrap)
set_property(TARGET EssaLangBootstrap PROPERTY OUTPUT_NAME esl)
# FIXME: I don't have any better idea how to do this,
#        but we definitely shouldn't use absolute paths here.
target_compile_definitions(EssaLangBootstrap PRIVATE ESL_SOURCE_DIR="${CMAKE_SOURCE_DIR}")
target_compile_definitions(EssaLangBootstrap PRIVATE ESL_BINARY_DIR="${CMAKE_BINARY_DIR}")
