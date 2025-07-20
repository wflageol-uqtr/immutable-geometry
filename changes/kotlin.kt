import kotlin.math.max
import kotlin.math.min

interface Movable<T> {
    val x: Int
    val y: Int
    val center: Point

    fun updateMoveable(newX: Int, newY: Int) : T

    fun move(moveX: Int, moveY: Int) : T {
        return updateMoveable(moveX + x, moveY + y)
    }

    fun rotate() : T {
        return rotateAround(center)
    }

    fun rotateAround(origin: Point) : T {
        val x = x - origin.x
        val y = y - origin.y

        return updateMoveable(-y + origin.x, x + origin.y)
    }
}

interface Scalable<T> {
    val w: Int
    val h: Int

    fun updateScalable(newW: Int, newH: Int) : T

    fun scale(value: Double) : T {
        return updateScalable((value * w).toInt(), (value * h).toInt())
    }

    fun fitInto(target: Size) : T {
        val scale = min(target.h / h.toDouble(), target.w / w.toDouble())
        return scale(scale)
    }
}

data class Point(override val x: Int, override val y: Int, val color: String = "black") : Movable<Point> {
    override val center = this

    override fun updateMoveable(newX: Int, newY: Int): Point {
        return copy(x = newX, y = newY)
    }

    override fun toString(): String {
        return String.format("Point(%d, %d, %s)", x, y, color)
    }
}

data class Size(override val w: Int, override val h: Int) : Scalable<Size> {
    override fun updateScalable(newW: Int, newH: Int): Size {
        return copy(w = newW, h = newH)
    }

    override fun toString(): String {
        return String.format("Size(%d, %d)", w, h)
    }
}

data class Rectangle(override val x: Int, override val y: Int, override val w: Int, override val h: Int, val color: String = "black") : Movable<Rectangle>, Scalable<Rectangle> {
    override val center = Point(x + w / 2, y + h / 2)
    private val left = x
    private val right = x + w
    private val top = y
    private val bottom = y + h

    override fun updateMoveable(newX: Int, newY: Int): Rectangle {
        return copy(x = newX, y = newY)
    }

    override fun updateScalable(newW: Int, newH: Int): Rectangle {
        return copy(w = newW, h = newH)
    }

    override fun rotateAround(origin: Point): Rectangle {
        // We only need to rotate the upper triangle of the rectangle to rebuild it, so we only need 3 points.
        val topLeft = Point(x, y)
        val topRight = Point(x + w, y)
        val botLeft = Point(x, y + h)

        val p1 = topLeft.rotateAround(origin)
        val p2 = topRight.rotateAround(origin)
        val p3 = botLeft.rotateAround(origin)

        val yList = listOf(p1.y, p2.y, p3.y)
        val xList = listOf(p1.x, p2.x, p3.x)

        val top = yList.min()
        val bot = yList.max()
        val left = xList.min()
        val right = xList.max()

        return copy(
            x = left!!,
            y = top!!,
            w = right!! - left,
            h = bot!! - top
        )
    }

    private fun doesIntersect(target: Rectangle) : Boolean {
        return target.left < right
                || target.top < bottom
                || left < target.right
                || top < target.bottom
    }

    fun intersect(target: Rectangle) : Rectangle? {
        return if(doesIntersect(target)) {
            val top = max(top, target.top)
            val bot = min(bottom, target.bottom)
            val left = max(left, target.left)
            val right = min(right, target.right)

            copy(x = left, y = top, w = right - left, h = bot - top)
        } else {
            null
        }
    }

    override fun toString(): String {
        return String.format("Rectangle(%d, %d, %d, %d, %s, color)", x, y, w, h, color)
    }
}

fun main(args: Array<String>) {
    val p = Point(2, 2, "yellow")
    val p2 = p.move(1, 2)
    val p3 = p.rotate()
    val p4 = p.rotateAround(Point(0,0))

    val s = Size(4, 6)
    val s2 = s.scale(3.0)
    val s3 = s.fitInto(Size(3, 3))

    val r = Rectangle(2, 2, 3, 4, "green")
    val r2 = r.move(1, 2)
    val r3 = r.rotate()
    val r4 = r.rotateAround(Point(0,0))

    val r5 = r.scale(5.0)
    val r6 = r.fitInto(Size(8, 8))

    val r7 = r.intersect(Rectangle(4, 3, 3, 2))

    println(p)
    println(p2)
    println(p3)
    println(p4)

    println(s)
    println(s2)
    println(s3)

    println(r)
    println(r2)
    println(r3)
    println(r4)
    println(r5)
    println(r6)
    println(r7)
}