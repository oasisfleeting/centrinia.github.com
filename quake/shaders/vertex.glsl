
attribute vec3 aVertexPosition;
//attribute vec4 aVertexColor;
attribute vec3 aVertexNormal;
attribute vec4 aVertexTextureRange;
attribute vec4 aVertexTextureCoordinate;

uniform mat4 uMVMatrix;
uniform mat3 uNMatrix;
uniform mat4 uPMatrix;

uniform vec2 texture_size;

varying vec3 vTransformedNormal;

varying vec4 vTextureCoord;
varying vec4 vTextureRange;
varying vec4 view_position;
varying vec4 vertex_position;

void main(void) {
	vertex_position = vec4(aVertexPosition,1);

	view_position = uMVMatrix * vertex_position;
	gl_Position = uPMatrix * view_position;

	//vColor = aVertexColor;
	vTransformedNormal = normalize(uNMatrix * aVertexNormal);
	//vTransformedNormal = normalize(uNMatrix * (aVertexColor.xyz*2.0-1.0));
	//vTransformedNormal = normalize(uMVMatrix*vec4(aVertexNormal,0)).xyz;
	vTextureRange = aVertexTextureRange;
	vTextureCoord = aVertexTextureCoordinate;
	//vec3 tangent = cross(vTransformedNormal,vertex_position.xyz);
	//vec3 bitangent = cross(vTransformedNormal,tangent);
	//vTextureCoord = vec4(dot(tangent,view_position.xyz), dot(bitangent,view_position.xyz), 0.0,1.0);
}
