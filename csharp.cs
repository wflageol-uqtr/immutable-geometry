using System;
using System.Collections.Generic;
using System.Linq;
using System.Security.AccessControl;
using System.Text.RegularExpressions;

namespace HelloWorld
{
    interface IMovable<T>
    {
        int X { get; }
        int Y { get; }
        Point Center { get; }

        T UpdateMovable(int newX, int newY);

        T Move(int dx, int dy)
        {
            return UpdateMovable(dx + X, dy + Y);
        }

        T Rotate()
        {
            return RotateAround(Center);
        }

        T RotateAround(Point origin)
        {
            var x = X - origin.X;
            var y = Y - origin.Y;

            return UpdateMovable(-y + origin.X, x + origin.Y);
        }
    }

    interface IScalable<T>
    {
        int W { get; }
        int H { get; }

        T UpdateScalable(int newW, int newH);

        T Scale(double value)
        {
            return UpdateScalable((int)(value * W), (int)(value * H));
        }

        T FitInto(Size target)
        {
            var scale = Math.Min(target.H / (double)H, target.W / (double)W);
            return Scale(scale);
        }
    }

    record Point(int X, int Y) : IMovable<Point>
    {
        public Point Center => this;

        public Point UpdateMovable(int newX, int newY)
        {
            return this with { X = newX, Y = newY };
        }

        public override string ToString()
        {
            return string.Format($"Point({X}, {Y})");
        }
    }

    record Size(int W, int H) : IScalable<Size>
    {
        public Size UpdateScalable(int newW, int newH)
        {
            return this with { W = newW, H = newH };
        }

        public override string ToString()
        {
            return string.Format($"Size({W}, {H})");
        }
    }

    record Rectangle(int X, int Y, int W, int H) : IMovable<Rectangle>, IScalable<Rectangle>
    {
        private int Left => X;
        private int Right => X + W;
        private int Top => Y;
        private int Bottom => Y + H;

        public Point Center => new Point(X + W / 2, Y + H / 2);

        public Rectangle UpdateMovable(int newX, int newY)
        {
            return this with { X = newX, Y = newY };
        }

        public Rectangle UpdateScalable(int newW, int newH)
        {
            return this with { W = newW, H = newH };
        }

        public Rectangle RotateAround(Point origin)
        {
            // We only need to rotate the upper triangle of the rectangle to rebuild it, so we only need 3 points.
            var topLeft = new Point(X, Y);
            var topRight = new Point(X + W, Y);
            var botLeft = new Point(X, Y + H);

            var p1 = (topLeft as IMovable<Point>).RotateAround(origin);
            var p2 = (topRight as IMovable<Point>).RotateAround(origin);
            var p3 = (botLeft as IMovable<Point>).RotateAround(origin);

            var yList = new List<int>() { p1.Y, p2.Y, p3.Y };
            var xList = new List<int>() { p1.X, p2.X, p3.X };

            var top = yList.Min();
            var bot = yList.Max();
            var left = xList.Min();
            var right = xList.Max();

            return this with
            {
                X = left,
                Y = top,
                W = right - left,
                H = bot - top
            };
        }

        private bool DoesIntersect(Rectangle target)
        {
            return target.Left < Right
                || target.Top < Bottom
                || Left < target.Right
                || Top < target.Bottom;
        }

        public Rectangle Intersect(Rectangle target)
        {
            if (DoesIntersect(target))
            {
                var top = Math.Max(Top, target.Top);
                var bot = Math.Min(Bottom, target.Bottom);
                var left = Math.Max(Left, target.Left);
                var right = Math.Min(Right, target.Right);

                return this with
                {
                    X = left,
                    Y = top,
                    W = right - left,
                    H = bot - top
                };
            } 
            else
            {
                return null;
            }
        }
        public override string ToString()
        {
            return string.Format($"Rectangle({X}, {Y}, {W}, {H})");
        }
    }

    public class Program
    {
        public static void Main(string[] args)
        {
            IMovable<Point> p = new Point(2, 2);
            var p2 = p.Move(1, 2);
            var p3 = p.Rotate();
            var p4 = p.RotateAround(new Point(0, 0));

            IScalable<Size> s = new Size(4, 6);
            var s2 = s.Scale(3);
            var s3 = s.FitInto(new Size(3, 3));

            var r = new Rectangle(2, 2, 3, 4);
            var r2 = (r as IMovable<Rectangle>).Move(1, 2);
            var r3 = (r as IMovable<Rectangle>).Rotate();
            var r4 = (r as IMovable<Rectangle>).RotateAround(new Point(0, 0));

            var r5 = (r as IScalable<Rectangle>).Scale(5);
            var r6 = (r as IScalable<Rectangle>).FitInto(new Size(8, 8));

            var r7 = r.Intersect(new Rectangle(4, 3, 3, 2));

            Console.WriteLine(p);
            Console.WriteLine(p2);
            Console.WriteLine(p3);
            Console.WriteLine(p4);

            Console.WriteLine(s);
            Console.WriteLine(s2);
            Console.WriteLine(s3);

            Console.WriteLine(r);
            Console.WriteLine(r2);
            Console.WriteLine(r3);
            Console.WriteLine(r4);
            Console.WriteLine(r5);
            Console.WriteLine(r6);
            Console.WriteLine(r7);
        }
    }
}