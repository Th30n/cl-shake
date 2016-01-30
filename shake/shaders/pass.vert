#version 330 core

layout (location = 0) in vec3 position;
layout (location = 1) in vec3 normal;
layout (location = 2) in vec2 uv;

uniform mat4 mvp = mat4(1.0);

out VS_OUT
{
    vec2 uv;
} vs_out;

void main(void)
{
    gl_Position = mvp * vec4(position, 1.0);
    vs_out.uv = uv;
}
