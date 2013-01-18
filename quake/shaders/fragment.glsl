// http://learningwebgl.com/blog/?p=28
precision mediump float;

varying vec4 vColor;
uniform vec4 uDummy;
uniform vec2 texture_size;

varying vec2 vTextureCoord;
varying vec4 vTextureRange;

uniform sampler2D uSampler;

void main(void) {
	vec2 begin = vec2(vTextureRange[0], vTextureRange[1]);
	vec2 size = vec2(vTextureRange[2], vTextureRange[3]);

	//gl_FragColor = texture2D(uSampler, mod(vTextureCoord,size)+begin);
	//gl_FragColor = texture2D(uSampler, (vTextureCoord - floor(vTextureCoord/64.0))/2048.0);
	//gl_FragColor = texture2D(uSampler, (fract((vTextureCoord-begin)/size)+begin/size)/2048.0);
	//gl_FragColor = texture2D(uSampler, ((-vTextureCoord)/2048.0));
	
	// Coordinates for the inside of a patch. Inside [0,1)^2
	//vec2 patch_coord = fract(vec2(vTextureCoord.t,vTextureCoord.s)/size);
	// Only negate the t coordinate.
	//vec2 patch_coord = fract(vec2(vTextureCoord.s,vTextureCoord.t)/size);
	vec2 patch_coord = fract(vTextureCoord/size);
	vec2 atlas_coord = (patch_coord*size+begin)/texture_size;
	gl_FragColor = texture2D(uSampler, vec2(atlas_coord.s,-atlas_coord.t));


	/*gl_FragColor = texture2D(uSampler, vec2(
		mod(vTextureCoord.s,size.s)/2048.0,
		mod(vTextureCoord.t,size.t)/2048.0
	));*/
	//gl_FragColor = texture2D(uSampler, vec2(vTextureCoord.s,vTextureCoord.t));
	//gl_FragColor = vColor;
	//gl_FragColor = uDummy;
}
