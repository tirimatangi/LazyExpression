# Variadic resursive expressions with lazy evaluation which look like nested containers

**LazyExpression** is a header-only library written in C++17. It implements general variadic and recursive expression templates. The expression objects can be used as if they were ordinary (possibly nested) containers.

We first give a brief formal definition, followed by plenty of examples.

Assume you have _n_ (possibly nested) containers **C**<sub>1</sub>, ... , **C**<sub>_n_</sub>.
The types of the containers may or may not be identical. For example, **C**<sub>1</sub> could be
a vector of vectors of floats, **C**<sub>2</sub> could be a deque of arrays of ints and so on.
We assume that the containers have the same number of elements on each nesting level.
In this example there are two nesting levels but there can be one or as many levels as you want.
As you'll see later, the containers may also be other (possibly nested) expressions.

Let's denote the value type of the innermost container of **C**<sub>_k_</sub> by **T**<sub>_k_</sub> for _k_=1..._n_.
So in our example **T**<sub>1</sub> would be `float` and **T**<sub>2</sub> would be `int`.

Now assume that you have a function (or any callable object)  **f**(**T**<sub>1</sub>, ... , **T**<sub>_n_</sub>)&rarr;**S**
which maps the arguments into a value of type **S**. Such a function in our example could be
`double f(int, float)` (i.e. **S** = double).

## Expressions with ordinary container arguments

Let's define a two-dimensional expression with 2 elements on the first and 3 elements on the second level and see what we can do with it.

```c++
    vector<vector<float>> c1 = {  {1.1, 1.2, 1.3}, {2.1, 2.2, 2.3} };
    deque<array<int, 3>>  c2 = {  {1, 2, 3}, {-1, -2, -3} };

    auto f = [](float x, int i) { return std::ldexp(double(x), i); }; // Calculates x * 2^i
    auto expr = Expression{f, c1, c2};
```

Now `expr` behaves as if it was a 2-level container with value type `double`,
filled with values `f(c1[i][j], c2[i][j])`.
However, nothing at all is evaluated when `expr` is defined. It will be lazily evaluated
only when it is accessed.

Let's access the first element of the first level of our container and see what we get.

```c++
    auto ex0 = expr[0];
```

Still nothing was evaluated! `ex0` is an expression which behaves as if it was
an ordinary 1-level container with value type `double`. It is equivalent to `Expression{f, c1[0], c2[0]}`. So `ex0` essentially behaves like `vector<double>`.
You can read the first value of the container (and finally trigger an evaluation of the function) using either `ex0` or the original expression like so:

```c++
    double firstElem = ex0[0]; // == expr[0][0] == f(c1[0][0], c2[0][0])
```

The iterator category of `expr` is `random_access`
on both levels because both `vector` and `deque` as well as `array` belong to random access category.
If we had used e.g. `list` instead of `vector`, the category would have been `bidirectional`. The properties of expressions are provided by `std::iterator_traits` like with ordinary containers.

If the iterator category had been less than random access,
expr[0] could have been replaced with `*expr.begin()` as usual. Let's verify this:

```c++
    assert(firstElem == expr[0][0]);
    assert(firstElem == *ex0.begin());
    assert(firstElem == *(*expr.begin()).begin());
```

All elements of `expr` can for example be printed using ordinary and range-for loops.

```c++
    // Print the result by looping over every element.
    for (size_t i = 0; i < expr.size(); ++i) {  // Loop over the outer container
        cout << "Row " << i << ": | ";
        for (size_t j = 0; j < expr[i].size(); ++j)
            cout << c1[i][j] << " * 2^" << c2[i][j] << " = " << expr[i][j] << " | ";
        cout << "\n";
    }
// Output:
// Row 0: | 1.1 * 2^1 = 2.2 | 1.2 * 2^2 = 4.8 | 1.3 * 2^3 = 10.4 |
// Row 1: | 2.1 * 2^-1 = 1.05 | 2.2 * 2^-2 = 0.55 | 2.3 * 2^-3 = 0.2875 |
```


Let's also calculate the sums of the innermost levels to demonstrate that expressions work with _std::_ algorithms.

```c++
    // Calculate the row sums and store in a vector.
    vector<double> vecSums(expr.size());
    std::transform(expr.begin(), expr.end(), vecSums.begin(),
                    [](auto ex) { return std::accumulate(ex.begin(), ex.end(), 0.0); });

    cout << "Row sums: { ";
    for (auto s : vecSums)
        cout << s << ' ';
    cout << "}\n";
// Output:
// Row sums: { 17.4 1.8875 }
```
For runnable code, see example 1 in [tutorial-example.cc](https://github.com/tirimatangi/LazyExpression/blob/main/examples/tutorial-example.cc).

## Expressions with other expressions as arguments

Some or all of the argument containers **C**<sub>_k_</sub> can be replaced with another
compatible container. This is quite natural because, as said above, an expression
behaves just as if it was an ordinary container. The recursive expression and all its argument
expressions are still lazily evaluated.

Let's repeat the previous example using lists instead of vectors. Furthermore, let's define new expression `expr2` which adds an offset and rounds the result. The argument of `expr2` is now `expr1` which is another expression.

```c++
        // Define input containers.
        list<list<float>>    c1 = {  {1.1, 1.2, 1.3}, {2.1, 2.2, 2.3} };
        deque<array<int, 3>> c2 = {  {1, 2, 3}, {-1, -2, -3} };

        // Define a function and an expression.
        auto f = [](float x, int i) { return ldexp(double(x), i); }; // Calculates x * 2^i
        auto expr1 = Expression{f, c1, c2};

        // Define another function which maps doubles to integers.
        double offset = 4.2;
        auto roundAdd = [offset](double d) { return int(std::round(d + offset)); };

        // Define an expression whose argument is another expression.
        // It behaves as if its type was vector<vector<int>>.
        auto expr2 = Expression{roundAdd, expr1};

        // Evaluate and print the whole expression.
        cout << "Original expression:     " << expr1() << "\n";
        cout << "Rounded with offset " << offset << ": " << expr2() << "\n";
```

Now `expr2` behaves as if it was a 2-level container with value type `int`.
The output will be

```
Original expression:     { { 2.2 4.8 10.4 }  { 1.05 0.55 0.2875 }  }
Rounded with offset 4.2: { { 6 9 15 }  { 5 5 4 }  }
```
For runnable code, see example 2 in [tutorial-example.cc](https://github.com/tirimatangi/LazyExpression/blob/main/examples/tutorial-example.cc).

## Expressions defined with elementary arithmetic operators {`+` `-` `*` `/`}.

New expressions can be derived by using other expressions, compatible containers and plain numbers
as arguments to elementary operators plus, minus, multiply and divide.

Let's walk through an example. First define an ordinary expression with two input arguments.
```c++
    vector<array<int, 2>>    vecInt { {-1, 2}, {-3, 4}, {-5, 6} };
    vector<array<double, 2>> vecDbl { {10.1, 20.2}, {30.3, 40.4}, {50.5, 60.6} };
    auto f = [](int i, double d) { return i * std::sqrt(d); };
    auto expr = Expression{f, vecInt, vecDbl};

```

Then define three new expressions using arithmetic overloads.

```c++
    auto expr_e_plus_c = vecDbl + expr;     // Add an expression to a container
    auto expr_e_times_e = expr * expr;      // Multiply two expressions
    auto expr_e_minus_number = -expr - 42;  // Subtract a constant from an expression.
```

Define yet another more complicated expression by applying elementary
operations to the previous arithmetic expressions.

```c++
    auto expr_arithmetic = (expr_e_plus_c + expr_e_times_e) / (3.14 * expr_e_minus_number);
```

Finally, evaluate the most derived expression.

```c++
    cout << "Arithmetic expression:";
    for (const auto& e : expr_arithmetic)
        cout << " (" << e[0] << ',' << e[1] << ") ";
    cout << "\n";
// Output:
// Arithmetic expression: (-0.139638,-0.686979)  (-3.57986,-3.36411)  (-62.8968,-8.21746)
```

In example 3 of [tutorial-example.cc](https://github.com/tirimatangi/LazyExpression/blob/main/examples/tutorial-example.cc) we also verify that the values are the same as the values given by direct calculation.

## Evaluate the entire expression into a container

`operator()` of an Expression evaluates every element in the expression at one go
and returns a concrete container object holding the results.
Let's see an example.

Define an expression whose function depends on variable `offset` which is captured by reference.
```c++
    vector<vector<float>>  c1 = {  {1.1, 1.2, 1.3}, {2.1, 2.2, 2.3} };
    list<list<int>>        c2 = {  {1, 2, 3}, {-1, -2, -3} };

    double offset = 10;
    // Calculates x * 2^i + offset
    auto f = [&offset](float x, int i) { return std::ldexp(double(x), i) + offset; };
    auto expr = Expression{f, c1, c2};
```

The entire expression can now be evaluated and stored into a container
by simply saying

```c++
    auto evaluated_expr = expr();
```

The type of container `evaluated_expr` is the same as the type of the first argument (c1)
except that the value type of the innermost container (here float) is replaced with
the return type of the function (here double).
So in this case, `decltype(evaluated_expr) == vector<vector<double>>`.
If the first argument itself is another expression, the output type will be the type
of its innermost argument.

If you later want to re-evaluate the same expression (or another compatible expression),
you can avoid re-allocation by reusing the container object. This can be done by passing
the address of the container object to `operator()`. We demonstrate this by changing the offset in function `f` and re-evaluating the expression.

```c++
    offset = 20;
    expr(&evaluated_expr);  // Evaluate expr and store in the container

    cout << "expr evaluated with offset " << offset << ": " << evaluated_expr << "\n";
```

The output with offsets 10 and 20 will be

```
expr evaluated with offset 10: { { 12.2 14.8 20.4 }  { 11.05 10.55 10.2875 }  }
expr evaluated with offset 20: { { 22.2 24.8 30.4 }  { 21.05 20.55 20.2875 }  }
```

For runnable code, see example 4 in [tutorial-example.cc](https://github.com/tirimatangi/LazyExpression/blob/main/examples/tutorial-example.cc).

## Copies or references?

Expression object captures both the function (i.e. the callable object) and all argument containers by default as copies, just like the capture list of C++ lambda expressions does. This helps avoid dangling references which can be very difficult to debug. On the other hand it may make plenty of unnecessary copies.

If you know that the lifetimes of the function or (some of) the argument containers are longer than the lifetime of the expression, you can pass them as references with `std::ref()`. Actually we could have used references in all the above examples.

Alternatively you can use convenience function `makeRefExpression` which makes an expression assuming that all arguments can be safely captured by reference.

So in the previous example we defined
```c++
    //...
    auto expr = Expression{f, c1, c2};
```
whereas we could have said
```c++
    auto expr = Expression{std::ref(f), std:ref(c1), std::ref(c2)};
```
or
```c++
    auto expr = makeRefExpression(f, c1, c2);
```

See example 5 in [tutorial-example.cc](https://github.com/tirimatangi/LazyExpression/blob/main/examples/tutorial-example.cc).


## A real-world use case on coordinate transforms

Assume that a _coordinate list_ is represented as a deque of coordinates stored in an `array<double, 2>`. You have several such coordinate lists stored in a vector. So, the type of the container holding the coordinates is `vector< deque<array<double, 2>> >`.

Furthermore, you have as many 2-by-2 _transform matrices_ as there are as there are coordinate lists. The same transform is applied to every coordinate in the list. So, the container type for the transform matrices is a vector of 2-by-2 arrays `vector< array<array<double, 2>, 2> >`.

Also, you have as many _coordinate offsets_ as there are coordinate lists. The same offset is added to every coordinate in the list. So, the container type for the transforms is a vector of coordinates `vector< array<double, 2> >`.

Our task is to define an expression which behaves as if it was a container holding transformed and offset coordinate lists.
Furthermore, we require that the coordinates are rounded to the nearest integer.
So the expression looks like a container of type `vector< deque<array<int, 2>> >`.

Let's get started. First define aliases for a point coordinate, a 2x2 transform matrix and a point list.

```c++
#include <LazyExpression/LazyExpression.h>
using LazyExpression::Expression;
using LazyExpression::makeRefExpression;

typedef array<double, 2> Vec2;  // 2-dimensional point
typedef array<Vec2, 2> Mat2x2;  // 2x2 matrix
typedef deque<Vec2>  PointList; // Set of points
```

Define a tool function which makes a transform matrix.
The transform matrix multiplied with a point coordinate produces a new coordinate
which is rotated by _theta_ degrees and magnified by _coeff_.

```c++
// Make a transform matrix for 2D points (rotate by theta and scale by coeff).
static Mat2x2 makeTransform(double thetaDegrees, double coeff = 1)
{
    double theta = thetaDegrees*(std::acos(-1) / 180); // pi = acos(-1)
    Mat2x2 mat { array{ coeff * std::cos(theta), coeff * std::sin(theta)},
                 array{-coeff * std::sin(theta), coeff * std::cos(theta)} };
    return mat;
}
```

Define the test data. We have 3 coordinate lists holding 4, 3 and 2 coordinates, respectively.

```c++
int main()
{
    // A vector of 3 point lists.
    vector<PointList> vecPtList {
        { {1,1}, {3,4}, {5, 0}, {-1, -1} },   // List #1 (4 points)
        { {-2,2}, {3,-3}, {-5, 2} },          // List #2 (3 points)
        { {-5.25,-4.6}, {3.25,-2.4} }         // List #3 (2 points)
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
```
Define an expression factory as a lambda function.
The factory inputs a point list and a transform matrix and an offset vector and prepares an expression which, when evaluated, will look like a point list with transformed points.

```c++
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
```

Define the expressions which convert the vectors of original point lists into transformed point lists. No evaluation is triggered yet. Even the expression factory defined above is not called.
The first expression produces real-values points and the second expression produces points rounded to the nearest integers (i.e. pixel coordinates.)

```c++
    auto exprPointListTransform = makeRefExpression(exprForPointList, vecPtList, vecTransforms, vecOffsets);
    auto exprTransformAsPixel = Expression{[](double x) { return int(std::round(x)); }, exprPointListTransform};
;
```

And that's it! Now both expressions can be used as if they were vectors of coordinate lists. The coordinate type of the first one is `double` and the type of the second one is `int`.

Finally, let's evaluate both expressions first point by point and then all at once.

In the point-by-point version the evaluation is not triggered until the value is printed out.

```c++
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
```

Here the entire expressions are evaluated and stored into containers.

```c++
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

}
```

The output in both cases will be:
```
   Original 4 points in list # 0: { { 1 1 }  { 3 4 }  { 5 0 }  { -1 -1 }  }
Transformed 4 points in list # 0: { { 101.414 200 }  { 104.95 200.707 }  { 103.536 196.464 }  { 98.5858 200 }  }
    Rounded 4 points in list # 0: { { 101 200 }  { 105 201 }  { 104 196 }  { 99 200 }  }

   Original 3 points in list # 1: { { -2 2 }  { 3 -3 }  { -5 2 }  }
Transformed 3 points in list # 1: { { -202.732 -99.2679 }  { -195.902 -101.098 }  { -205.33 -100.768 }  }
    Rounded 3 points in list # 1: { { -203 -99 }  { -196 -101 }  { -205 -101 }  }

   Original 2 points in list # 2: { { -5.25 -4.6 }  { 3.25 -2.4 }  }
Transformed 2 points in list # 2: { { 10.5 9.2 }  { -6.5 4.8 }  }
    Rounded 2 points in list # 2: { { 10 9 }  { -7 5 }  }
```

Runnable code can be found in [coordinate-transform-example.cc](https://github.com/tirimatangi/LazyExpression/blob/main/examples/coordinate-transform-example.cc).

## Compilation

The easiest way to compile all examples is to do
`cmake -DCMAKE_BUILD_TYPE=Release examples` followed by `make`.
If you don't want to use cmake, the examples can be compiled manually one by one. For instance, <br>
`g++ examples/tutorial-example.cc -std=c++17 -I include/ -O3 -o tutorial-example`

The examples have been tested with g++ 10.3.0  and clang++ 12.0.0 but any compiler which complies with c++17 standard should do.

