precision mediump float;

//varying vec4 vColor;
uniform vec4 uDummy;
uniform vec2 texture_size;

varying vec2 vTextureCoord;
varying vec4 vTextureRange;
varying vec3 vTransformedNormal;
varying vec4 view_position;

uniform sampler2D uSampler;

void main(void) {
	vec3 uAmbientColor = vec3(0.5,0.5,0.5);
	vec3 uPointLightingColor = vec3(0.7,0.6,0.3)*7.0;
	vec3 lantern_color = vec3(0.1,0.2,0.7)*60.0;

	vec2 begin = vec2(vTextureRange[0], vTextureRange[1]);
	vec2 size = vec2(vTextureRange[2], vTextureRange[3]);

	vec3 lightDirection = normalize(vec3(0,0,-1)-view_position.xyz);

	float directionalLightWeighting = max(dot(vTransformedNormal, lightDirection), 0.0);
	
	float inverse_square = pow(length(view_position.xyz),-2.0);
	vec3 lightWeighting = uAmbientColor + 
		uPointLightingColor * directionalLightWeighting +
		lantern_color * inverse_square;
		      

	// Coordinates for the inside of a patch. Inside [0,1)^2
	vec2 patch_coord = fract(vTextureCoord/size);
	vec2 atlas_coord = (patch_coord*size+begin)/texture_size;

	vec4 texcolor = texture2D(uSampler, vec2(atlas_coord.s,-atlas_coord.t));
	//texcolor = vec4(1,1,1,1)*0.5;
	//texcolor = vec4((vTransformedNormal+1.0)/2.0,1.0);
	gl_FragColor = vec4(texcolor.rgb*lightWeighting,texcolor.a);
}
