# Doom collision detection

  * Map divided into grid
    - grid references things and linedefs
  * Movement in discrete steps (at maximum velocity)
    - check collision on grid for each step


# Quake collision detection

  * Everything clips to BSP
    - Map is a static entity with BSP
    - Each entity model has a BSP (even simple bounding box)
  * Multiple BSP hulls per model
    - Offset for various entity sizes
    - Up to 3, probably precalculated
  * Additional hierarchy of AABB
    - Constructed for the map via axis aligned BSP
    - Up to depth of 4 (32 nodes, 2^(4+1))
    - Entities (re)linked into smallest BB they can fit
  * Movement is one entity at a time, but continuous
    - clip movement to level BSP
    - BB from start to clipped destination
    - find first entity collision on BB
    - movement trace contains collision info
    - triggers are checked after movement is done? (possible speed bugs?)


# cl-shake collision detection

  * Like Quake but without convex hulls, as movement is 2D like in Doom.
