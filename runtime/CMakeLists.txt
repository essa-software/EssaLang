add_library(EssaLangRuntime STATIC
    Buffer.cpp
    Builtins.cpp
    UString.cpp
    Utf8.cpp
)

set_target_properties(EssaLangRuntime PROPERTIES OUTPUT_NAME "esl-runtime")
target_link_libraries(EssaLangRuntime PUBLIC fmt::fmt)
target_include_directories(EssaLangRuntime PRIVATE ${CMAKE_SOURCE_DIR})

# FIXME: This breaks build of eml/AST.cpp somewhere in Value::string()
target_compile_options(EssaLangRuntime PRIVATE -Wno-restrict)
