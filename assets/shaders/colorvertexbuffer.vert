uniform mat4 projection;
uniform mat4 modelview;

attribute vec3 xyz;
attribute vec4 rgba;

varying vec4 color;

void main() {
	color = rgba;
	gl_Position = projection * modelview * vec4(xyz, 1.0);
}
