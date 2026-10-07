uniform mat4 projection;
uniform mat4 modelview;

layout(location = 0) in vec3 xyz;

void main() {
  gl_Position = projection * modelview * vec4(xyz, 1.0);
}
