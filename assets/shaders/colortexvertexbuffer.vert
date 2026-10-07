uniform mat4 projection;
uniform mat4 modelview;

layout(location = 0) in vec3 xyz;
layout(location = 1) in vec2 uv;
layout(location = 2) in vec4 rgba;

out vec2 coord;
out vec4 color;

void main() {
  coord = uv;
  color = rgba;
  gl_Position = projection * modelview * vec4(xyz, 1.0);
}
