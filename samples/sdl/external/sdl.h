#pragma once

#include <SDL3/SDL_render.h>
#include <SDL3/SDL_video.h>

#ifdef __cplusplus
extern "C" {
#endif

typedef struct _Color {
    uint8_t r;
    uint8_t g;
    uint8_t b;
    uint8_t a;
} Color;

typedef struct _Rect {
    uint32_t x;
    uint32_t y;
    uint32_t w;
    uint32_t h;
} Rect;

typedef struct _Window {
    SDL_Window* sdl_window;
} Window;

typedef struct _Renderer {
    SDL_Renderer* sdl_renderer;
} Renderer;

#ifdef __cplusplus
}
#endif
