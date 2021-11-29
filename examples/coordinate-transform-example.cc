#include <iostream>
#include <array>
#include <vector>
#include <list>
#include <deque>
#include <cmath>
#include <numeric>
#include <cassert>

#include <LazyExpression/LazyExpression.h>

using std::array;
using std::vector;
using std::deque;
using std::list;
using std::cout;
using std::ref;

// The API to LazyExpression library is class Expression and function makeRefExpression.
using LazyExpression::Expression;
using LazyExpression::makeRefExpression;

// Container printers.
template<typename T, template <class...> class Container>
std::ostream& operator<<(std::ostream& os, const Container<T>& vec);
template<typename T, auto N, template <class, size_t> class Container>
std::ostream& operator<<(std::ostream& os, const Container<T, N>& vec);

// Print a vector- or list-like container.
template<typename T, template <class...> class Container>
std::ostream& operator<<(std::ostream& os, const Container<T>& vec)
{
    os << "{ ";
    for (auto& el : vec)
    {
        os << el << ' ';
    }
    os << "} ";
    return os;
}

// Print an array-like container.
template<typename T, auto N, template <class, size_t> class Container>
std::ostream& operator<<(std::ostream& os, const Container<T, N>& vec)
{
    os << "{ ";
    for (auto& el : vec)
    {
        os << el << ' ';
    }
    os << "} ";
    return os;
}

typedef array<double, 2> Vec2;  // 2-dimensional point
typedef array<Vec2, 2> Mat2x2;  // 2x2 matrix
typedef deque<Vec2>  PointList; // Set of points

// Make a transform matrix for 2D points (rotate by theta and scale by coeff).
static Mat2x2 makeTransform(double thetaDegrees, double coeff = 1)
{
    double theta = thetaDegrees*(std::acos(-1) / 180); // pi = acos(-1)
    Mat2x2 mat { array{ coeff * std::cos(theta), coeff * std::sin(theta)},
                 array{-coeff * std::sin(theta), coeff * std::cos(theta)} };
    return mat;
}

int main()
{
    // A vector of 3 point lists. Each list may have a different number of points.
    vector<PointList> vecPtList {
        { {1,1}, {3,4}, {5, 0}, {-1, -1} },     // List #1 (4 points)
        { {-2,2}, {3,-3}, {-5, 2} },            // List #2 (3 points)
        { {-5.25,-4.6}, {3.25,-2.4} }           // List #3 (2 points)
    };

    // A vector of 3 offsets
    vector<Vec2> vecOffsets {
        {100,200},
        {-200,-100},
        {0, 0}
    };

    // A vector of 3 rotation matrices
    vector<Mat2x2> vecTransforms {
        makeTransform(45),      // Rotate 45 degrees
        makeTransform(-30),     // Rotate -30 degrees
        makeTransform(180, 2)   // Rotate 180 degrees, scale by 2
    };

    assert(vecPtList.size() == vecOffsets.size());
    assert(vecPtList.size() == vecTransforms.size());

    // Expression factory for making an expression which applies the given
    // transform and offset to all points in the point list.
    auto exprForPointList = [](const PointList& ptList, const Mat2x2& transform, const Vec2& offset)
    {
        auto transformPoint = [transform, offset](const Vec2& pt)
        {
            Vec2 result;
            result[0] = transform[0][0] * pt[0] + transform[0][1] * pt[1] + offset[0];
            result[1] = transform[1][0] * pt[0] + transform[1][1] * pt[1] + offset[1];
            return result;
        };

        return Expression{transformPoint, ref(ptList)}; // Note: transformPoint must be copied as it is a local object.
    };

    // Expression which applies a different offset and transform to each point list.
    // The offsets, transforms and point lists are given in a vector.
    auto exprPointListTransform = makeRefExpression(exprForPointList, vecPtList, vecTransforms, vecOffsets);

    // Round the transformed points into the nearest integer coordinate
    auto exprTransformAsPixel = Expression{[](double x) { return int(std::round(x)); }, exprPointListTransform};

    // Print the transformed points

    cout << "--- Demonstrate pointwise evaluation --- \n";
    // Print the elements point by point to demonstrate how the expressions are accessed.
    for (size_t i = 0; i < vecPtList.size(); ++i) {  // Print the transformed points in list #i
        size_t numPoints = vecPtList[i].size();
        cout << "   Original " << numPoints <<" points in list #" << i << ": " << vecPtList[i] << "\n";
        cout << "Transformed " << numPoints <<" points in list #" << i << ": { ";

        // 'exprList' is an expression which applies i'th offset and i'th transform matrix to i'th point list.
        // It behaves as if it was a container of type PointList (aka. deque<array<double, 2>>)
        auto exprList = exprPointListTransform[i];
        for (size_t j = 0; j < numPoints; ++j) {
            auto point = exprList[j];
            cout << " { " << point[0] << ' ' << point[1] << " } "; // Evaluation is triggered here.
        }
        cout << " }\n";

        cout << "    Rounded " << numPoints <<" points in list #" << i << ": { ";
        // 'exprRoundedList' is an expression which applies i'th offset and i'th transform matrix to i'th point list.
        // It behaves as if it was a container of type PointList (aka. deque<array<int, 2>>)
        auto exprRoundedList = exprTransformAsPixel[i];
        for (size_t j = 0; j < numPoints; ++j) {
            auto pixel = exprRoundedList[j]();
            cout << " { " << pixel[0] << ' ' << pixel[1] << " } "; // Evaluation is triggered here.
        }
        cout << " }\n";
        cout << std::endl;
    }
    cout << std::endl;

    cout << "--- Demonstrate evaluation into a buffer --- \n";
    // Print the elements of the expression by evaluating them first into a container.
    auto allPoints = exprPointListTransform();
    auto allRounded = exprTransformAsPixel();
    assert(allPoints.size() == vecPtList.size());
    assert(allRounded.size() == vecPtList.size());
    for (size_t i = 0; i < vecPtList.size(); ++i) {  // Print the transformed points in list #i
        size_t numPoints = vecPtList[i].size();
        cout << "   Original " << numPoints <<" points in list # " << i << ": " << vecPtList[i] << "\n";
        cout << "Transformed " << numPoints <<" points in list # " << i << ": " << allPoints[i] << "\n";
        cout << "    Rounded " << numPoints <<" points in list # " << i << ": " << allRounded[i] << "\n";
        cout << std::endl;
    }
   return 0;
}

/* Output:
--- Demonstrate pointwise evaluation ---
   Original 4 points in list #0: { { 1 1 }  { 3 4 }  { 5 0 }  { -1 -1 }  }
Transformed 4 points in list #0: {  { 101.414 200 }  { 104.95 200.707 }  { 103.536 196.464 }  { 98.5858 200 }  }
    Rounded 4 points in list #0: {  { 101 200 }  { 105 201 }  { 104 196 }  { 99 200 }  }

   Original 3 points in list #1: { { -2 2 }  { 3 -3 }  { -5 2 }  }
Transformed 3 points in list #1: {  { -202.732 -99.2679 }  { -195.902 -101.098 }  { -205.33 -100.768 }  }
    Rounded 3 points in list #1: {  { -203 -99 }  { -196 -101 }  { -205 -101 }  }

   Original 2 points in list #2: { { -5.25 -4.6 }  { 3.25 -2.4 }  }
Transformed 2 points in list #2: {  { 10.5 9.2 }  { -6.5 4.8 }  }
    Rounded 2 points in list #2: {  { 10 9 }  { -7 5 }  }


--- Demonstrate evaluation into a buffer ---
   Original 4 points in list # 0: { { 1 1 }  { 3 4 }  { 5 0 }  { -1 -1 }  }
Transformed 4 points in list # 0: { { 101.414 200 }  { 104.95 200.707 }  { 103.536 196.464 }  { 98.5858 200 }  }
    Rounded 4 points in list # 0: { { 101 200 }  { 105 201 }  { 104 196 }  { 99 200 }  }

   Original 3 points in list # 1: { { -2 2 }  { 3 -3 }  { -5 2 }  }
Transformed 3 points in list # 1: { { -202.732 -99.2679 }  { -195.902 -101.098 }  { -205.33 -100.768 }  }
    Rounded 3 points in list # 1: { { -203 -99 }  { -196 -101 }  { -205 -101 }  }

   Original 2 points in list # 2: { { -5.25 -4.6 }  { 3.25 -2.4 }  }
Transformed 2 points in list # 2: { { 10.5 9.2 }  { -6.5 4.8 }  }
    Rounded 2 points in list # 2: { { 10 9 }  { -7 5 }  }
 */