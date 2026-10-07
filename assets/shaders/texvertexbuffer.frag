uniform sampler2D tex;

in vec2 coord;
out vec4 fragColor;

void main() {
  fragColor = texture(tex, coord);
}
