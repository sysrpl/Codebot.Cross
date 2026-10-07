uniform mat4 projection;
uniform mat4 modelview;

layout(location = 0) in vec2 xy;

void main() {
  gl_Position = projection * modelview * vec4(xy, 0.0, 1.0);
}
