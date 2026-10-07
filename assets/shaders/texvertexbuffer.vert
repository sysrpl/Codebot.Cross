uniform mat4 projection;
uniform mat4 modelview;

layout(location = 0) in vec3 xyz;
layout(location = 1) in vec2 uv;

out vec2 coord;

void main() {
  coord = uv;
  gl_Position = projection * modelview * vec4(xyz, 1.0);
}
