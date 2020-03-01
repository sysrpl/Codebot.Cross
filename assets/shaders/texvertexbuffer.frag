uniform sampler2D tex;

varying vec2 coord;

void main() {
	gl_FragColor = texture2D(tex, coord);
}
