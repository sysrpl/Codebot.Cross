uniform sampler2D tex;

in vec2 coord;
in vec3 eyeNormal;
out vec4 fragColor;

void main() {
  float light = max(dot(normalize(eyeNormal), normalize(vec3(0.4, 0.6, 1.0))), 0.0);
  vec4 color = texture(tex, coord);
  fragColor = vec4(color.rgb * (0.35 + 0.65 * light), color.a);
}
