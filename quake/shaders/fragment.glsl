// fragment.glsl

//precision lowp float;
precision mediump float;

//varying vec4 vColor;
uniform vec4 uDummy;
uniform vec2 texture_size;

uniform mat3 uNMatrix;
varying vec2 vTextureCoord;
varying vec4 vTextureRange;
varying vec3 vTransformedNormal;
varying vec4 view_position;

uniform sampler2D uSampler;

vec4 lookup_texture(vec2 uv) {
	vec2 begin = vTextureRange.xy;
	vec2 size = vTextureRange.zw;
	// Coordinates for the inside of a patch. Inside [0,1)^2
	vec2 patch_coord = fract(uv)*size+begin;
	vec2 atlas_coord = patch_coord/texture_size;
	//vec2 patch_coord = fract(vTextureCoord/vTextureRange.zw);
	//vec2 atlas_coord = (patch_coord*vTextureRange.zw+vTextureRange.xy)/texture_size;
	vec2 texcoord = vec2(atlas_coord.s,-atlas_coord.t);
	vec4 texcolor = texture2D(uSampler, texcoord);
	return texcolor;
}

void main(void) {
	vec3 uAmbientColor = vec3(0.5,0.5,0.5);
	vec3 uPointLightingColor = vec3(0.7,0.6,0.3)*7.0;
	vec3 lantern_color = vec3(0,0,0);

	vec4 texcolor = lookup_texture(vTextureCoord);

	vec3 lightDirection = normalize(vec3(0,0,0)-view_position.xyz);

	//vec3 normal_vector = normalize(normalize(uNMatrix * (texcolor.rgb*2.0-1.0))*5.0+vTransformedNormal);
	//vec3 normal_vector = vTransformedNormal;
	float directionalLightWeighting = max(dot(vTransformedNormal, lightDirection), 0.0);
	//float directionalLightWeighting = max(dot(normal_vector, lightDirection), 0.0);
	
	float inverse_square = pow(length(view_position.xyz),-2.0);
	vec3 lightWeighting = uAmbientColor + 
		uPointLightingColor * directionalLightWeighting +
		lantern_color * inverse_square;
	//texcolor = vec4(1,1,1,1)*0.5;
	//texcolor = vec4((vTransformedNormal+1.0)/2.0,1.0);
	gl_FragColor = vec4(texcolor.rgb*lightWeighting,texcolor.a);
}
