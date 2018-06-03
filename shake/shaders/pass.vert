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

#define MAX_BATCH_SIZE 512

layout (location = 0) in vec3 position;
layout (location = 1) in vec3 color;
layout (location = 2) in vec2 uv;
layout (location = 3) in vec3 normal;
layout (location = 4) in int draw_id;

uniform mat4 mvp[MAX_BATCH_SIZE];
uniform int tex_layer[MAX_BATCH_SIZE];

out VS_OUT
{
    vec2 uv;
    float layer;
    vec3 color;
    vec3 normal;
} vs_out;

void main(void)
{
    gl_Position = mvp[draw_id] * vec4(position, 1.0);
    vs_out.uv = uv;
    vs_out.layer = float(tex_layer[draw_id]);
    vs_out.color = color;
    vs_out.normal = normal;
}
