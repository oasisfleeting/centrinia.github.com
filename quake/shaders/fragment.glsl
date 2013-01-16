// http://learningwebgl.com/blog/?p=28
precision mediump float;

varying vec4 vColor;
uniform vec4 uDummy;

void main(void) {
	gl_FragColor = vColor;
	//gl_FragColor = uDummy;
}
