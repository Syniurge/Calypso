#pragma once

struct Point3D
{
    float x,y,z;

    Point3D(float x = 0.0, float y = 0.0, float z = 0.0) : x(x), y(y), z(z) {}

    Point3D operator+(Point3D const &o) {
        return Point3D(x + o.x, y + o.y, z + o.z);
    }

    Point3D& operator*=(Point3D const &o) {
        x *= o.x;
        y *= o.y;
        z *= o.z;
        return *this;
    }
};
