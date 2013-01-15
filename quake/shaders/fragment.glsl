// http://learningwebgl.com/blog/?p=28
precision mediump float;

varying vec4 vColor;
uniform float uDummy;

void main(void) {
	gl_FragColor = vColor*uDummy;
	gl_FragColor[3] = 0.5;
}
