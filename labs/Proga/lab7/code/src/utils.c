#include "utils.h"

#include <SDL3/SDL.h>

SDL_Color hexa_to_rgba(unsigned int hex, double opacity) {
  SDL_Color color;
  color.r = (hex >> 16) & 0xFF; // Extract the red component
  color.g = (hex >> 8) & 0xFF;  // Extract the green component
  color.b = hex & 0xFF;         // Extract the blue component
  color.a = (unsigned char)(opacity *
                            255); // Set the alpha component based on opacity
  return color;
}