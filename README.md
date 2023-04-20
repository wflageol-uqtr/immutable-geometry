# Design Pattern for Reusing Immutable Methods in Object-Oriented Languages

This repository contains code exemples of the design pattern described in the paper titled "Design Pattern for Reusing Immutable Methods in Object-Oriented Languages" sent to the EuroPlop 2023 conference.

The exemples are in four programming languages: Clojure, Java, Kotlin, and Rust. 

Each example implements a fully immutable geometry system with the types point, size, and rectangle. They showcase reusing the methods declared for point and size for rectangle without having to reimplement or create delegeting methods for rectangle.

Note that the coordinate system for each example considers the origin (0,0) as the top left corner.

Each example implement the following methods:

- For points, or "movable":
  * Move/Translate: Changes the x/y coordinates of the object by the specified delta.
  * Rotate: Rotates the object around its center 90 degrees clockwise.
  * RotateAround: Rotates the object around a specific origin 90 degrees clockwise.
- For size, or "scalable":
  * Scale: Applies a specified factor to the w and h properties of the object.
  * FitIn: Shrinks or grows the object to fit into the specified size without changing its aspect ratio.
- For rectangle only:
  * Intersect: Returns the intersection with a specified rectangle.
