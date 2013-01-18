
attribute vec3 aVertexPosition;
attribute vec4 aVertexColor;
attribute vec4 aVertexTextureRange;
attribute vec2 aVertexTextureCoordinate;

uniform mat4 uMVMatrix;
uniform mat4 uPMatrix;
uniform vec2 texture_size;

varying vec4 vColor;
varying vec2 vTextureCoord;
varying vec4 vTextureRange;

void main(void) {
	gl_Position = uPMatrix * uMVMatrix * vec4(aVertexPosition, 1.0);
	vColor = aVertexColor;
	vec2 begin = vec2(aVertexTextureRange[0], aVertexTextureRange[1]);
	vec2 size = vec2(aVertexTextureRange[2], aVertexTextureRange[3]);

	vTextureRange = aVertexTextureRange;
	vTextureCoord = aVertexTextureCoordinate;
}
