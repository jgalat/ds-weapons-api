# ds-weapons-api
[![Build Status](https://travis-ci.org/jgalat/ds-weapons-api.svg?branch=master)](https://travis-ci.org/jgalat/ds-weapons-api)
[![License: GPL v3](https://img.shields.io/badge/License-GPL%20v3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)

A Dark Souls weapon information static API. An example built with
[static-api-generator](https://www.github.com/jgalat/static-api-generator).
This API is currently hosted in this repository in the
`gh-pages` branch. It can be accessed at
https://jgalat.github.io/ds-weapons-api/.

All the data served was scraped from ~~http://www.darksoulsdatabase.com/~~.

### Routes

```
GET /
```

Returns all the weapons (simplified) in the game.

```
GET /weapon_type/:weapon_type
```

Returns all the weapons (simplified) in the game that are of
the specified `:weapon_type`.

```
GET /weapon/:weapon_id
```

Returns the weapon with the specified `:weapon_id`.

```
GET /weapon/:weapon_id/simplify
```

Returns the weapon (simplified) with the specified
`:weapon_id`.

```
GET /weapon/:weapon_id/name
```

Returns the in-game name of the weapon with the specied `:weapon_id`.

```
GET /weapon/:weapon_id/description
```

Returns the in-game description of the weapon with the
specied `:weapon_id`.

```
GET /weapon/:weapon_id/special_features
```

Returns the in-game special features of the weapon with the
specied `:weapon_id`.

```
GET /weapon/:weapon_id/damage
```

Returns the in-game special features of the unupgraded weapon
with the specied `:weapon_id`.

```
GET /weapon/:weapon_id/requirements
```

Returns the in-game requirements of the weapon with the
specied `:weapon_id`.

```
GET /weapon/:weapon_id/auxiliary
```

Returns the in-game auxiliary effects of the weapon with the
specied `:weapon_id`.

```
GET /weapon/:weapon_id/bonus
```

Returns the in-game parameter bonus (or scaling) of the weapon
with the specied `:weapon_id`.

```
GET /weapon/:weapon_id/upgrades
```

Returns the in-game possible upgrade paths of the weapon
with the specied `:weapon_id`.

```
GET /weapon/:weapon_id/upgrade/:upgrade_path
```

Returns the in-game stats updates for the specified upgrade
path of the `:weapon_id` weapon .
