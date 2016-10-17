// Jonatan H Sundqvist
// #version 330

// #version 420

// precision mediump float;

attribute vec3 aVertexPosition;
attribute vec2 aTexCoord;
// attribute vec4 aVertexColor;

uniform mat4 uMVMatrix;
uniform mat4 uPMatrix;
uniform sampler2D uSampler;

// varying highp vec2 vTexCoord;
// varying vec4 vColor;
// out highp vec2 vTexCoord;
varying vec2 vTexCoord;


void main(void) {
	gl_Position = uPMatrix * uMVMatrix * vec4(aVertexPosition, 1.0);
  // gl_Position = vec4(0,0,-0.5,1);
	vTexCoord   = vec2(aTexCoord.s, aTexCoord.t);
	// vColor      = aVertexColor;
}