#include <iostream>
#include <array>
#include <exception>
#include <tuple>
#include <type_traits>
#include <utility>
#include <vector>
#include <list>
#include <deque>
#include <string>
#include <iomanip>
#include <initializer_list>
#include <functional>
#include <cassert>
#include <iterator>
#include <cmath>
#include <numeric>
#include <complex>

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
using LazyExpression::asExpression;

// Container printers.
template<typename T, template <class...> class Container>
std::ostream& operator<<(std::ostream& os, const Container<T>& vec);
template<typename T, auto N, template <class, size_t> class Container>
std::ostream& operator<<(std::ostream& os, const Container<T, N>& vec);
std::ostream& operator<<(std::ostream& os, std::string str);
template<typename T>
std::ostream& operator<<(std::ostream& os, std::complex<T> c);

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

// Print a string.
std::ostream& operator<<(std::ostream& os, std::string str)
{
    os << "\'" << str.c_str() << "\'";
    return os;
}

// Print a complex number.
template<typename T>
std::ostream& operator<<(std::ostream& os, std::complex<T> c)
{
    os << c.real() << ((c.imag() >= 0) ? '+' : '-') << std::abs(c.imag()) << 'i';
    return os;
}


int main()
{
    std::cout << "\n*** Example 1 *** : Expressions with ordinary container arguments. \n";
    {
        // Define input containers.
        vector<vector<float>> c1 = {  {1.1, 1.2, 1.3}, {2.1, 2.2, 2.3} };
        deque<array<int, 3>>  c2 = {  {1, 2, 3}, {-1, -2, -3} };

        // Define the function and the expression.
        auto f = [](float x, int i) { return ldexp(double(x), i); }; // Calculates x * 2^i
        auto expr = Expression{f, c1, c2};

        // Access the first layer of the nested container.
        // The result is as if it was defined as Expression{f, c1[0], c2[0]};
        auto ex0 = expr[0];

        // Read the first element
         double firstElem = ex0[0];
         assert(firstElem == expr[0][0]);
         assert(firstElem == *ex0.begin());
         assert(firstElem == *(*expr.begin()).begin());

        // Print the result by looping over every element.
        for (size_t i = 0; i < expr.size(); ++i) {  // Loop over the outer container
            cout << "Row " << i << ": | ";
            for (size_t j = 0; j < expr[i].size(); ++j)
                cout << c1[i][j] << " * 2^" << c2[i][j] << " = " << expr[i][j] << " | ";
            cout << "\n";
        }

        // Calculate the row sums and store in a vector.
        vector<double> vecSums(expr.size());
        std::transform(expr.begin(), expr.end(), vecSums.begin(),
                       [](auto ex) { return std::accumulate(ex.begin(), ex.end(), 0.0); });

        cout << "Row sums: { ";
        for (auto s : vecSums)
            cout << s << ' ';
        cout << "}\n";
    }

    std::cout << "\n*** Example 2 *** : Expressions with expression arguments. \n";
    {
        // Define input containers.
        list<list<float>>    c1 = {  {1.1, 1.2, 1.3}, {2.1, 2.2, 2.3} };
        deque<array<int, 3>> c2 = {  {1, 2, 3}, {-1, -2, -3} };

        auto f = [](float x, int i) { return std::ldexp(double(x), i); }; // Calculates x * 2^i
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
    }

    std::cout << "\n*** Example 3 *** : Arithmetic expressions. \n";
    {
        vector<array<int, 2>>    vecInt { {-1, 2}, {-3, 4}, {-5, 6} };
        vector<array<double, 2>> vecDbl { {10.1, 20.2}, {30.3, 40.4}, {50.5, 60.6} };
        auto f = [](int i, double d) { return i * std::sqrt(d); };
        auto expr = Expression{f, vecInt, vecDbl};

        // Make new expressions with elementary operations.
        auto expr_e_plus_c = vecDbl + expr;     // Add an expression to a container
        auto expr_e_times_e = expr * expr;      // Multiply two expressions
        auto expr_e_minus_number = -expr - 42;  // Subtract a constant from an expression.

        // Make a complicated expression by combining several simpler ones.
        auto expr_arithmetic = (expr_e_plus_c + expr_e_times_e) / (3.14 * expr_e_minus_number);

        // Evaluate the arithmetic expression.
        cout << "Arithmetic expression:";
        for (const auto& e : expr_arithmetic)
            cout << " (" << e[0] << ',' << e[1] << ") ";
        cout << "\n";

        // An arithmetic function equivalent to the above arithmetic expression.
        auto func_arithmetic = [](double e, double v) {
            return  ((v + e) + (e * e)) / (3.14 * (-e - 42)); };

        // Verify that the values of the arithmetic expression are
        // the same as those in direct calculation.
        double error = 0;
        cout << "   Direct arithmetics:";
        for (size_t i = 0; i < expr.size(); ++i) {
            cout << " (" << func_arithmetic(expr[i][0], vecDbl[i][0]) <<
                     ',' << func_arithmetic(expr[i][1], vecDbl[i][1]) << ") ";

            error += std::abs(func_arithmetic(expr[i][0], vecDbl[i][0]) - expr_arithmetic[i][0]);
            error += std::abs(func_arithmetic(expr[i][1], vecDbl[i][1]) - expr_arithmetic[i][1]);
        }
        cout << "\n   --> error = " << error << "\n";
    }

    std::cout << "\n*** Example 4 *** : Evaluate expression into a container. \n";
    {
        vector<vector<float>>  c1 = {  {1.1, 1.2, 1.3}, {2.1, 2.2, 2.3} };
        list<list<int>>        c2 = {  {1, 2, 3}, {-1, -2, -3} };

        double offset = 0;
        // Calculates x * 2^i + offset
        auto f = [&offset](float x, int i) { return std::ldexp(double(x), i) + offset; };
        auto expr = Expression{f, c1, c2};

        // Evaluate the whole expression into a container.
        // The type of the container is the same as the type of the first argument (c1)
        // except that the value type of the innermost container (here float) is replaced with
        // the return type of the function (here double).
        // So the output type will be vector<vector<double>>.
        // If the first argument is another expression, the output type will be the type
        // of its innermost argument.

        auto evaluated_expr = expr();
        static_assert(std::is_same_v<decltype(evaluated_expr), vector<vector<double>>>, "Type mismatch");
        cout << "expr evaluated with offset " << offset << ": " << evaluated_expr << "\n";

        // Save allocations be reusing the result container.
        offset = 20;
        expr(&evaluated_expr);
        cout << "expr evaluated with offset " << offset << ": " << evaluated_expr << "\n";
    }

    std::cout << "\n*** Example 5 *** : Capture arguments by copy vs reference. \n";
    {
        vector<float>  c1 = {1.1, 1.2, 1.3, 2.1, 2.2, 2.3};
        vector<int>    c2 = {1, 2, 3, -1, -2, -3};

        auto f = [offset=10](float x, int i) { return std::ldexp(double(x), i) * offset; };

        // Capture the callable object and the containers by copies
        auto exprC = Expression{f, c1, c2};

        // Capture the callable object and the containers by references
        auto exprR1 = Expression{ref(f), ref(c1), ref(c2)};

        // Capture the callable object and the containers by references using library function.
        auto exprR2 = makeRefExpression(f, c1, c2);

        // Modify the input and expect the reference captures to change.
        for (auto& x : c2)
            x += 1;

        cout << " Copy capture: " << exprC() << "\n";
        cout << "Ref capture 1: " << exprR1() << "\n";
        cout << "Ref capture 2: " << exprR2() << "\n";
    }

    std::cout << "\n*** Example 6 *** : Complex matrices. \n";
    {
        // Define matrix data type
        using std::complex;
        typedef vector<std::complex<double>> CplxVector;
        typedef vector<CplxVector> CplxMatrix;

        // Function calculates inner product of two square complex matrices: C = conj(A) * B
        auto complexInnerProduct = [](const CplxMatrix& a, const CplxMatrix& b) {
            assert(a.size() == b.size());
            CplxMatrix c(a.size());
            if (c.empty())
                return c;
            const int rows = a.size(), cols = (*a.begin()).size();
            assert(rows == cols); // Deal with square matrices only
            std::fill(c.begin(), c.end(), CplxVector(cols));

            for (size_t i = 0; i < rows; ++i)
                for (size_t j = 0; j <= i; ++j) {
                    std::complex<double> tmp {0.0, 0.0};
                    for (size_t k = 0; k < cols; ++k) {
                        tmp += (std::conj(a[k][i]) * b[k][j]);
                    }
                    c[i][j] = tmp;
                    c[j][i] = std::conj(tmp);
                }
            return c;
        };

        // Define a vector of 3 matrices
        vector<CplxMatrix> vecMatX(3), vecMatY(3);

        // The size of the matrices
        const size_t matSize = 4;

        // Initialize matSize-by-matSize matrices.
        for (size_t i = 0; i < vecMatX.size(); ++i) {
            vecMatX[i] = vecMatY[i] = CplxMatrix(matSize);
            std::fill(vecMatX[i].begin(), vecMatX[i].end(), CplxVector(matSize));
            std::fill(vecMatY[i].begin(), vecMatY[i].end(), CplxVector(matSize));

            for (size_t y = 0; y < matSize; ++y)
                for (size_t x = 0; x < matSize; ++x) {
                    vecMatX[i][y][x] = {(std::rand() % 20)-100., (std::rand() % 20)-10.};
                    vecMatY[i][y][x] = {(std::rand() % 20)-10., (std::rand() % 20)-10.};
            }
        }

        // Calculate the first matrix inner product for reference.
        auto firstInnerProduct = complexInnerProduct(vecMatX[0], vecMatY[0]);

        // Expression for matrix inner products
        auto exprInner = Expression{complexInnerProduct, vecMatX, vecMatY};

        // Evaluate the first inner product matrix
        auto firstExprInnerProduct = exprInner[0];

        cout << "exprInner[0] = \n";
        for (size_t i = 0; i < matSize; ++i) {
            cout << "  ";
            for (size_t j = 0; j < matSize; ++j)
                cout << firstInnerProduct[i][j] << " \t\t";
            cout << "\n";
        }
        cout << "First element of the first product  exprInner[0][0][0] = " << exprInner[0][0][0]
             << ", direct calculation gives " << firstInnerProduct[0][0] << "\n";

        // Add a complex constant to an expression.
        auto exprPlusConst = exprInner + complex<double>{1.0, 1.0};
        cout << "Add constant 1+i: " << exprPlusConst[0][0][0] << "\n";

        // Beware that by default, the multiplication is elementwise. Not to be confused with matrix multiplication
        auto exprTimesMatrix = vecMatX * exprInner;
        cout << "Elementwise multiply: " << vecMatX[0][0][0] << " * " << exprInner[0][0][0] << " = "
             << exprTimesMatrix[0][0][0] << "\n";
    }

    std::cout << "\n*** Example 7 *** : Container algebra with identity expressions. \n";
    {
        // Any container can be turned into an expression

        vector<float> c1 = {1.1, 1.2, 1.3, 2.1, 2.2, 2.3};
        auto expr1 = asExpression(c1); // Note: capture by copy

        deque<int> c2 = { 1, 2, 3, -1, -2, -3 };
        auto expr2 = asExpression(ref(c2)); // Note: capture by reference

        auto expr3 = asExpression(vector<int>{100, 200, 300, -400, -500, -600}); // Note: copy elision + move

        // Now that the containers are expressions, you can do lazily evaluated arithmetics.
        auto expr123 = expr3 + expr1 * expr2;

        cout  << expr3() << " + " << expr1() << " * " << expr2() << " = " << expr123() << "\n";
    }

    std::cout << "\n*** Example 8 *** : Weird corner cases. \n";
    {
        // If is possible to define an expression where the "containers" are mere numbers or other objects.
        // This example maps numbers into a string.
        int offset = 100;
        auto f = [&offset](int i, int j) -> std::string { return " Value = " + std::to_string(i*j+offset) + " "; };

        int ii = 3, jj = 4;
        auto expr = Expression{f, ii, jj};

        cout << "String expression with offset  " << offset << " : " << expr() << "\n";
        offset  = 200;
        cout << "String expression with offset  " << offset << " : " << expr() << "\n";

        cout << "Elementary algebra on compatible containers.\n";
        // You can do elementary algebra with all kinds of (possibly nested) compatible containers with identity expressions.
        list<vector<int>> a { {10,20,30}, {40,50,60} };
        vector<list<int>> b { {-1,-2,-3}, {-4,-5,-6} };

        auto id = [](int x){return x;};
        auto exprA = makeRefExpression(id, a);
        auto exprB = makeRefExpression(id, b);

        // Now you can add, multiply, subtract and divide the containers a and b even though their types are different.
        cout << "Add: " << a << " +  " << b << " =  " << (exprA + exprB)() << "\n";
        cout << "Sub: " << a << " -  " << b << " =  " << (exprA - exprB)() << "\n";
        cout << "Mul: " << a << " *  " << b << " =  " << (exprA * exprB)() << "\n";
        cout << "Div: " << a << " /  " << b << " =  " << (exprA / exprB)() << "\n";
    }
    return 0;
}