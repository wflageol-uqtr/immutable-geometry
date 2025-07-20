trait Movable[A] {
  val x: Int
  val y: Int
  val center: Point
  
  def updateMovable(newX: Int, newY: Int): A
  
  def move(moveX: Int, moveY: Int): A =
    updateMovable(moveX + x, moveY + y)
  
  def rotate() : A = 
    rotateAround(center)
  
  def rotateAround(origin: Point) : A = {
    val dx = x - origin.x
    val dy = y - origin.y
    
    updateMovable(-dy + origin.x, dx + origin.y)
  }
}

trait Scalable[A] {
  val w: Int
  val h: Int
  
  def updateScalable(newW: Int, newH: Int): A
  
  def scale(value: Double): A =
    updateScalable((value * w).toInt, (value * h).toInt)
    
  def fitInto(target: Size): A = {
    val scale = Math.min(target.h / h.toDouble, target.w / w.toDouble)
    this.scale(scale)
  }
}

case class Point(x: Int, y: Int, color: String = "black") extends Movable[Point] {
  val center = this
  
  def updateMovable(newX: Int, newY: Int): Point =
    copy(x = newX, y = newY)
    
  override def toString(): String =
    String.format("Point(%d, %d, %s)", x, y, color)
}

case class Size(w: Int, h: Int) extends Scalable[Size] {
  def updateScalable(newW: Int, newH: Int): Size =
    copy(w = newW, h = newH)
    
  override def toString(): String =
    String.format("Size(%d, %d)", w, h)
}

case class Rectangle(x: Int, y: Int, w: Int, h: Int, color: String = "black") extends Movable[Rectangle] with Scalable[Rectangle] {
  val center = Point(x + w / 2, y + h / 2)
  private val left = x
  private val right = x + w
  private val top = y
  private val bottom = y + h
  
  def updateMovable(newX: Int, newY: Int): Rectangle =
    copy(x = newX, y = newY)
    
  def updateScalable(newW: Int, newH: Int): Rectangle =
    copy(w = newW, h = newH)
    
  override def rotateAround(origin: Point): Rectangle = {
    // We only need to rotate the upper triangle of the rectangle to rebuild it, so we only need 3 points.
    val topLeft = Point(x, y)
    val topRight = Point(x + w, y)
    val botLeft = Point(x, y + h)

    val p1 = topLeft.rotateAround(origin)
    val p2 = topRight.rotateAround(origin)
    val p3 = botLeft.rotateAround(origin)

    val yList = List(p1.y, p2.y, p3.y)
    val xList = List(p1.x, p2.x, p3.x)

    val top = yList.min
    val bot = yList.max
    val left = xList.min
    val right = xList.max
    
    copy(x = left, 
         y = right, 
         w = right - left,
         h = bot - top)
  }
  
  private def doesIntersect(target: Rectangle): Boolean =
    (target.left < right 
     || target.top < bottom 
     || left < target.right 
     || top < target.bottom)
     
  def intersect(target: Rectangle): Rectangle = {
    if (doesIntersect(target)) {
      val top = Math.max(this.top, target.top)
      val bot = Math.min(this.bottom, target.bottom)
      val left = Math.max(this.left, target.left)
      val right = Math.min(this.right, target.right)
      
      copy(x = left, y = top, w = right - left, h = bot - top)
    } else {
      null
    }
  }
  
  override def toString(): String =
    String.format("Rectangle(%d, %d, %d, %d, %s)", x, y, w, h, color)
}

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