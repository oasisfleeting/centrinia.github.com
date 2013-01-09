precision highp float;

attribute vec4 aVertexIndex;
attribute vec4 aVertexColor;

uniform vec3 ndMVMatrix[dimension];
uniform mat4 uMVMatrix;
uniform mat4 uPMatrix;

varying vec4 vColor;

void main(void) {
	vec3 aVertexPosition = vec3(0,0,0);

	float index = aVertexIndex[0];
	for(int i=0;i<dimension;i++) {
		float f = floor(index/2.0);
		float t = index - 2.0*f;
		index = f;
		aVertexPosition += (2.0*t-1.0) * ndMVMatrix[i];
	}
	vColor = aVertexColor;
	gl_Position = uPMatrix * uMVMatrix * vec4(aVertexPosition, 1.0);
}
