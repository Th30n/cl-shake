// Copyright (C) 2016 Teon Banek
//
// This program is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 2 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License along
// with this program; if not, write to the Free Software Foundation, Inc.,
// 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.

#version 330 core

layout (points) in;
layout (triangle_strip, max_vertices = 4) out;

uniform mat4 proj = mat4(1.0);
// Half the width of a quad.
uniform float size = 1.0f;

out vec2 uv;

void main(void)
{
    vec4 pos = gl_in[0].gl_Position;

    vec4 bot_left = vec4(-size, -size, 0.0f, 0.0f) + pos;
    gl_Position = proj * bot_left;
    uv = vec2(0.0f, 0.0f);
    EmitVertex();

    vec4 bot_right = vec4(size, -size, 0.0f, 0.0f) + pos;
    gl_Position = proj * bot_right;
    uv = vec2(1.0f, 0.0f);
    EmitVertex();

    vec4 top_left = vec4(-size, size, 0.0f, 0.0f) + pos;
    gl_Position = proj * top_left;
    uv = vec2(0.0f, 1.0f);
    EmitVertex();

    vec4 top_right = vec4(size, size, 0.0f, 0.0f) + pos;
    gl_Position = proj * top_right;
    uv = vec2(1.0f, 1.0f);
    EmitVertex();

    EndPrimitive();
}
