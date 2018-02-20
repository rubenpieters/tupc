[![Build Status](https://travis-ci.org/rubenpieters/tupc.svg?branch=master)](https://travis-ci.org/rubenpieters/tupc)

# TUPC: Textbased UI Position Configuration

This library parses a textbased & visual representation to create UI positions.

For example, the following positions:

```
(0,0) ----- (100,0) ---- (150,0)
  |             |            |
  |             |            |
  |             |            |
  |             |            |
(0,100) --- (100,100) -- (150,100)
  |             |            |
  |             |            |
(0,150) ---------------- (150,150)
```

are the result of parsing the following file:

```
# scale=50
113
113
222
```

Where `1` corresponds to the section in the top-left, `2` to the section in the bottom and `3` to the section in the top-right.

Parsing will output data to a `Map`:

```
Map.fromFoldable
       [ (Tuple '1' (Pos { xLeft: 0, xRight: 100, yTop: 0, yBot: 100 }))
       , (Tuple '2' (Pos { xLeft: 0, xRight: 150, yTop: 100, yBot: 150 }))
       , (Tuple '3' (Pos { xLeft: 100, xRight: 150, yTop: 0, yBot: 100 }))
       ]
```

# Configuration

Configuration parameters are specified by lines starting with `#` and then providing `parameter = value`. These lines are called the configuration section.

The following configurations are supported:

| parameter   | info                                                                                                                   | default value     |
|-------------|------------------------------------------------------------------------------------------------------------------------|-------------------|
| scale       | scale for both axes                                                                                                    | 1                 |
| scaleX      | scale for x-axis                                                                                                       |                   |
| scaleY      | scale for y-axis                                                                                                       |                   |
| ignore      | these characters are ignored in the content section                                                                    | ['+','-','|','␣'] |
| ignoreExtra | these characters are ignored in the content section, use this if you don't want to override the default ones in ignore |                   |
| origin      | location for origin (where x=0 and y=0)                                                                                | {x: Left, y: Up}  |
| directionX  | direction where x-axis increases                                                                                       | Right             |
| directionY  | direction where y-axis increases                                                                                       | Down              |

# Content

The rest of the file is the content section. From this section the bounds for a specific character is calculated to obtain its position.

Only the outermost positions count. Meaning that this:

```
11111
11111
11111
11111
11111
```

or this:

```
1+++1
+++++
+++++
+++++
1+++1
```

are equivalent.

The calculated positions are multiplied by the `scale` setting (or `scaleX` and `scaleY` if they are set).
