uniform sampler2D tex;

in vec2 coord;
in vec4 color;
out vec4 fragColor;

void main() {
  fragColor = texture(tex, coord) * color;
}
