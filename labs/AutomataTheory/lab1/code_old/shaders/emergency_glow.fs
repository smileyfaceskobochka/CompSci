#version 330 core

in vec2 fragTexCoord;
in vec4 fragColor;

uniform sampler2D texture0;
uniform vec4 colDiffuse;
uniform float time;
uniform float intensity;

out vec4 finalColor;

void main() {
    vec4 texColor = texture(texture0, fragTexCoord) * colDiffuse * fragColor;

    // Calculate glow based on time and intensity
    float glow = sin(time * 15.0) * 0.3 + 0.7;
    glow *= intensity;

    // Add red tint for emergency
    vec4 glowColor = vec4(1.0, 0.2, 0.2, glow);

    finalColor = texColor + glowColor * glow;
}
