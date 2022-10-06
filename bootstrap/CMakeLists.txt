add_executable(EssaLangBootstrap
    Lexer.cpp
    Parser.cpp
    main.cpp
)
target_link_libraries(EssaLangBootstrap essautil)
essautil_setup_target(EssaLangBootstrap)
set_property(TARGET EssaLangBootstrap PROPERTY OUTPUT_NAME esl)
