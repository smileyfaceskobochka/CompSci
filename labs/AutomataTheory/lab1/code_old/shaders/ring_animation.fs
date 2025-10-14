#version 330 core

in vec2 fragTexCoord;
in vec4 fragColor;

uniform sampler2D texture0;
uniform vec4 colDiffuse;
uniform float scale;
uniform vec2 center;

out vec4 finalColor;

void main() {
    // Calculate distance from center for circular scaling
    vec2 pos = fragTexCoord - vec2(0.5, 0.5);
    float dist = length(pos);

    // Create circular scaling effect
    float ringScale = scale * (1.0 - dist * 2.0);
    ringScale = max(0.0, ringScale);

    // Apply scaling to color intensity
    if (dist > 0.4) {
        finalColor = vec4(fragColor.rgb * ringScale, fragColor.a * ringScale);
    } else {
        vec4 texColor = texture(texture0, fragTexCoord) * colDiffuse * fragColor;
        finalColor = vec4(texColor.rgb * (1.0 - dist), texColor.a) * ringScale;
    }
}
