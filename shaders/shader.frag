#version 330 core
in vec2 texCoord;

out vec4 color;

uniform sampler2D texture2;

void main() {
	color = texture(texture2, texCoord);
}