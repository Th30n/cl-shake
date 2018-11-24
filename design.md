# Game Design

Copyright (C) 2016 Teon Banek

A short summary of required features and gameplay description.

This is still work in progress and the design may and will change over time.

## General Idea

A simple first person shooter with levels based around 2D BSP rendering,
similar to original Doom. The game is rendered in 3D and supports mouse look.
Levels are created via a custom made map editor.

The scope should be kept to a minimum, as the goals of the project are:

  * learning Common Lisp and
  * learning how to design a Doom like engine and a game.

## Gameplay

Fast, projectile based, first person shooter. The player uses a rocket
launcher to fight monsters. Monsters fight back by launching fireballs. The
challenge is in dodging the fireballs and not blowing yourself up with
rockets. There are no difficulty levels. There is only one player weapon and
one monster type.

### Rockets

Strong splash damage. Monsters die on 1 hit, except at the edges of the splash
radius. Player sustains more damage, but not much.

Rocket explosion survivors are knocked back from the explosion. (Cut this
feature if it turns out complicated.)

The rocket ammo is unlimited. Firing is limited by a rate of fire.

### Fireballs

Low splash damage and lower damage overall. Player and monsters can take some
of those.

Getting hit with a fireball produces no knock-back, but slows the movement for
a small amount of time. Both the player and monsters can get hit with flying
fireballs.

### Health

No health regeneration. No armor system. Player can heal via health packs.
Initial health should allow for surviving of about 5 fireballs. Rocket self
damage should kill the player point-blank. Monsters can survive 2-3 fireballs
with initial health. There is no fall damage.

### Movement

Player moves fast, faster than monsters.

Supported movement:

  * forward/backward
  * strafe left/right
  * mouse look

There is no jumping nor crouching.

Movement speed is only modified by getting hit by a fireball. It is possible
for a player or a monster to end up in air due to rocket knock back or
stepping off of a ledge. In such cases, influencing the movement is not
possible. The momentum is stored which led to being airborne. An entity is
affected by gravity and moves down while also moving in the direction of
the horizontal momentum. There is no fall damage.

### Items & Power-ups

Player can find and use these items:

  * health pack - small health boost, can not go over initial health
  * mega health pack - large health boost, can go over initial health
  * invincibility - unable to take damage for a small amount of time

They are automatically activated when touched. In the case of a health pack,
it isn't activated if it cannot be applied (the health is already at initial
health level).

### Level Elements

There is only one level element, a door. Doors can only activated by the
player pressing the use button. All doors are unlocked, there are no switches
nor keys which need to be used for unlocking. Neither monsters nor projectiles
can open doors. The doors are closed automatically after a certain period and
if there is no-one standing in them. Doors can have a texture similar to a
wall in order to hide them. Such doors are only to be used for optional,
secret areas.

Specially designated door serves as the end of the level. There is at least
one such door on the level. Opening the door completes the level.

There are no elevators, but the elevation can be changed via stairs.

Monster spawn points are set in the editor and all monsters spawn at the start
of the level. There is no respawning or additional spawning inside the level.

## AI

When the player is unseen, monsters move randomly around their initial
position. Initial position is set to monster's spawn point or monster's
position after losing sight of the player.

When the player is seen, monsters try to move into range and fire at the
player.

Although the monsters can hit each other, they don't have a memory of that so
there is no in-fighting.

## Graphics & Animation

### Textures

All regular textures should be 1024x1024 pixels in size. The renderer uses
top-left as origin of texture (UV) coordinates. Note, that Blender's origin is
bottom-left, so the image should be flipped.

Currently supported format is .BMP RGB only.

Todo: Style?

### 3D Models

All models should be scaled so that they fit into a cube of size 2,
coordinates from -1 to 1. This is the default model in renderer and the
initial size of cube model in Blender.

Static models:

  * health pack
  * mega health pack
  * invincibility pack
  * rocket launcher
  * rocket
  * light stand or smth.

Animated models:

  * 1 monster model
    - movement animation
    - shooting animation
    - exploded animation
    - corpse

### Particle Effects

  * rocket explosion
  * rocket engine flame
  * fireball
  * fireball explosion (reuse as smaller rocket explosion)

### Lighting

Main light is directional. Point lights on static light stands, rockets,
fireballs and explosions.

No shadows.

## Sound

No sound.

## Menus

No menus.
