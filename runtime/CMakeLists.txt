add_library(EssaLangRuntime SHARED
    Buffer.cpp
    UString.cpp
    Utf8.cpp
)

set_target_properties(EssaLangRuntime PROPERTIES OUTPUT_NAME "esl-runtime")
target_link_libraries(EssaLangRuntime PUBLIC fmt::fmt)

# FIXME: This breaks build of eml/AST.cpp somewhere in Value::string()
target_compile_options(EssaLangRuntime PRIVATE -Wno-restrict)
