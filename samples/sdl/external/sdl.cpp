#include "sdl.h"
#include <SDL3/SDL_events.h>
#include <SDL3/SDL_rect.h>
#include <SDL3/SDL_render.h>
#include <SDL3/SDL_timer.h>
#include <SDL3/SDL_video.h>
#include <cstddef>
#include <cstdio>

extern "C" {

Rect $s$Rect$new(uint32_t x, uint32_t y, uint32_t w, uint32_t h) {
    return Rect {
        .x = x,
        .y = y,
        .w = w,
        .h = h,
    };
}

////

Color $s$Color$new(uint32_t r, uint32_t g, uint32_t b) {
    return Color {
        .r = static_cast<uint8_t>(r),
        .g = static_cast<uint8_t>(g),
        .b = static_cast<uint8_t>(b),
    };
}

////

void $s$Window$__drop__(Window* w) {
    SDL_DestroyWindow(w->sdl_window);
}

Window $s$Window$new(const char* title, uint32_t w, uint32_t h) {
    auto sdl_window = SDL_CreateWindow(title, w, h, SDL_WINDOW_RESIZABLE);
    return Window {
        .sdl_window = sdl_window,
    };
}

void $s$Window$show(Window* w) {
    printf("show window\n");
    SDL_ShowWindow(w->sdl_window);
}

Renderer $s$Window$renderer(Window* w) {
    auto sdl_renderer = SDL_CreateRenderer(w->sdl_window, NULL);
    return { .sdl_renderer = sdl_renderer };
}

bool $s$Window$got_quit_event(Window* w) {
    SDL_Event event;
    while (SDL_PollEvent(&event)) {
        if (event.type == SDL_EVENT_QUIT) {
            return true;
        }
    }
    return false;
}

////

void $s$Renderer$__drop__(Renderer* r) {
    SDL_DestroyRenderer(r->sdl_renderer);
}

// color is ARGB
void $s$Renderer$set_draw_color(Renderer* renderer, Color color) {
    SDL_SetRenderDrawColor(renderer->sdl_renderer, color.r, color.g, color.b, color.a);
}

void $s$Renderer$clear(Renderer* r) {
    SDL_RenderClear(r->sdl_renderer);
}

void $s$Renderer$fill_rect(Renderer* r, Rect rect) {
    SDL_FRect frect {
        .x = static_cast<float>(rect.x),
        .y = static_cast<float>(rect.y),
        .w = static_cast<float>(rect.w),
        .h = static_cast<float>(rect.h),
    };
    SDL_RenderFillRect(r->sdl_renderer, &frect);
}

void $s$Renderer$present(Renderer* r) {
    SDL_RenderPresent(r->sdl_renderer);
}

void sleep_ms(uint32_t ms) {
    SDL_Delay(ms);
}
}
