uniform mat4 projection;
uniform mat4 modelview;
uniform mat4 bones[64];

layout(location = 0) in vec3 xyz;
layout(location = 1) in vec3 normal;
layout(location = 2) in vec2 uv;
layout(location = 3) in vec4 bone;
layout(location = 4) in vec4 weight;

out vec2 coord;
out vec3 eyeNormal;

void main() {
  mat4 skin = bones[int(bone.x)] * weight.x + bones[int(bone.y)] * weight.y +
    bones[int(bone.z)] * weight.z + bones[int(bone.w)] * weight.w;
  if (weight.x + weight.y + weight.z + weight.w < 0.0001)
    skin = mat4(1.0);
  mat4 m = modelview * skin;
  eyeNormal = mat3(m) * normal;
  coord = uv;
  gl_Position = projection * m * vec4(xyz, 1.0);
}
