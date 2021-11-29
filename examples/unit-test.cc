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

/*
 This file tests several weird and wonderful corner cases of LazyExpression library.
 If you make changes and this file still compiles, you have done something right.
 Add a test of you changes to the end of this file.

 Compilation:
 g++ examples/unit-test.cc -std=c++17 -I include/ -std=c++17 -Wall -ftemplate-backtrace-limit=0 -g -o unit-test
 */

// Debug helper for making sense of complicated types.
// Example:   whatType<decltype(myVariable)>("Type of myVariable is ")();
template <class T>
struct whatType
{
    const char* _msg;
    whatType(const char* msg = "") : _msg(msg) {};
    void operator()()
    {
        std::cout << _msg << __PRETTY_FUNCTION__ << std::endl;
    }
};

using std::array;
using std::vector;
using std::deque;
using std::cout;
using std::size_t;

typedef array<double, 2> Vec2;
typedef array<Vec2, 2> Mat2x2;
typedef deque<Vec2>  PointList;

#include <LazyExpression/LazyExpression.h>

//--\\||//--\\||//--\\||//--\\||//--\\||//--\\||//--\\||//--\\||//--\\||//--\\||

// Another debug helper. Not as good as whatType.
template <class... T>
void showType(T&&...)
{
    std::cout << __PRETTY_FUNCTION__ << std::endl;
}

/*
template<typename T, template <class...> class Container>
std::ostream& operator<<(std::ostream& os, const Container<T>& vec);

template<typename T, auto N, template <class, size_t> class Container>
std::ostream& operator<<(std::ostream& os, const Container<T, N>& vec);

std::ostream& operator<<(std::ostream& os, std::string str);
*/

using namespace LazyExpression;

template<typename T, auto N, template <class, size_t> class Container>
std::ostream& operator<<(std::ostream& os, const Container<T, N>& vec);


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

std::ostream& operator<<(std::ostream& os, std::string str)
{
    os << "\'" << str.c_str() << "\'";
    return os;
}


double fglobal1(int i) noexcept
{
    return 0.5 * i;
}

double fglobal2(int i) noexcept
{
    return 2.0 * i;
}

class aFunctor
{
    int _value = 0;
public:
    aFunctor(int value) : _value(value) {
        std::cout << "aFunctor(" << value << ") created\n";
    }

    double operator()(int i, double d) {
        return _value * 1000 + i*10 + d;
    }

    aFunctor(const aFunctor& other)
    {
        _value= other._value;
        std::cout << "aFunctor(" << _value << ") copy constructed.\n";
    }

    aFunctor& operator=(const aFunctor& other)
    {
        _value= other._value;
        std::cout << "aFunctor(" << _value << ") copy assign constructed.\n";
        return *this;
    }

    aFunctor(aFunctor&& other)
    {
        _value= other._value;
        std::cout << "aFunctor(" << _value << ") moved.\n";
    }

    ~aFunctor() {
        std::cout << "aFunctor(" << _value << ") deleted\n";
    }
};

template <class T = double>
auto functionFactory(int value)
{
    return [value](int i, double d) -> T { return value * 1000 + i*10 + d;};
}

template <class C1, class C2>
auto expressionFactory(int value, const C1& c1, const C2& c2)
{
    return Expression(aFunctor(value), c1, c2);
}

void test_a()
{
  using std::vector;
  using std::array;
  using std::deque;
  using std::list;

//  using namespace LazyExpression;
  std::cout <<  std::boolalpha;
  {
    int i = 4, j = 10;
    // Non-containers as expression parameters
    auto f3 = [](int x, int y) -> double { return 100.0*x+y; };
    auto expr3 = Expression{f3, i, j};
    cout << "expr3.dimension() = " << expr3.dimension() << "\n";
    auto x1 = *expr3.begin();
    auto x2 = expr3[0];
    auto r3all = expr3();
    cout << "f3: " << x1 << ", " << x2 << ", " << r3all << "\n";
    cout << "dim(expr3) = " << Dimension<decltype(expr3)>::value << "\n";
  }

  {
    auto f0 = [](int x1, int x2, int x3) noexcept { return (x1+x2+x3) / 3.0; };
    cout << "isNoExcept f0 = " << NoExceptType<decltype(f0)>{}() << "\n";
    vector<int> v1{0,1,2,3}, v2{10,20,30,40}, v3{100,200,300,400};
    vector<vector<int>> vv1{v1,v2,v3}, vv2{v2,v3,v1}, vv3{v3,v2,v1};
    auto expr0 = Expression{f0, vv1, vv2, vv3};
    cout << "expr0.dimension() = " << expr0.dimension() << ", size=" << expr0.size() << "\n";
    auto e0at00 = expr0[0][0];
    cout << "expr[0][0]=" << e0at00 << std::endl;
    auto e0 = expr0();
    cout << "expr0()=" << e0 << std::endl;

    int param = 100;
    auto f00 =[param](double x, int y) -> double {
        return param*x + y;
    };
    auto expr00 = Expression{f00, expr0, vv2};
    auto e00at00 = expr00[0][0];  cout << "expr00[0][0]=" << e00at00 << std::endl;
    cout << "hasexpr0 = " << expr0.hasExpressionArguments() << ", ";
    cout << "hasexpr00 = " << expr00.hasExpressionArguments() << "\n";

    auto e00 = expr00();
    cout << "expr00()=" << e00 << std::endl;

    for (auto v : expr00) {
        cout << "expr00 for: " << v() << "\n";
    }

    auto exprOper = expr00 + expr0;
    cout << "hasexpr Oper = " << exprOper.hasExpressionArguments() << "\n";
    cout << "dim(exprOper) = " << Dimension<decltype(exprOper)>::value << "\n";
    auto eOperat0 = exprOper[0];
    auto eOperat00 = eOperat0[0];
    cout << "exprOper[0][0] = " << eOperat00 << " = " <<  *(*exprOper.begin()).begin()  << std::endl;
  }

  {
    using T = int;
    using VDA = vector<deque<array<T, 3>>>;
    using VLA = vector<list<array<T, 3>>>;
    using LVA = list<vector<array<T, 3>>>;
    VDA vda1 { { { 1,2,3 }, { 4,5,6 }, { 7,8,9 } },
               { { 11,12,13 }, { 14,15,16 }, { 17,18,19 }, }};
    VDA vda2 { { { 102,103,101 }, { 105,106,104 }, { 108,109,107 } },
               { { 112,113,111 }, { 115,116,114 }, { 118,119,117 } }};

    VLA vla1 { { { 2,3,1 }, { 5,6,4 }, { 8,9,7 } },
               { { 12,13,11 }, { 15,16,14 }, { 18,19,17 }, }};
    VLA vla2 { { { 101,102,103 }, { 104,105,106 }, { 107,108,109 } },
               { { 111,112,113 }, { 114,115,116 }, { 117,118,119 } }};

    LVA lva1 { { { 2,3,1 }, { 5,6,4 }, { 8,9,7 } },
               { { 12,13,11 }, { 15,16,14 }, { 18,19,17 }, }};
    LVA lva2 { { { 101,102,103 }, { 104,105,106 }, { 107,108,109 } },
               { { 111,112,113 }, { 114,115,116 }, { 117,118,119 } }};

    auto f0 = [](T x1, T x2, T x3) noexcept { return (x1+x2+x3) / 3.0; };
    cout << "isNoExcept = " << NoExceptType<decltype(f0)>{}() << "\n";
    vector<T> v1{0,1,2,3}, v2{10,20,30,40}, v3{100,200,300,400};

    auto expr0 = Expression{f0, v1, v2, v3};
    cout << "expr0.dimension() = " << expr0.dimension() << ", size=" << expr0.size() << "\n";
    auto r0at0 = expr0[0];
    auto r0 = expr0();
    cout << "expr0() = " << r0 << "\n";
    showType(r0);
    cout << "Iterate expression 0: {";
    for(auto e : expr0)
        cout << e << ' ';
    cout << "}\n";

    { // STL algorithm compatibility
            auto sum1 = std::accumulate(r0.begin(), r0.end(), 0);
            auto sum2 = std::accumulate(expr0.begin(), expr0.end(), 0);
            auto minusExpr0 = -expr0;
            auto sum3 = std::accumulate(minusExpr0.begin(), minusExpr0.end(), 0);
            cout << "Sum expr1 = " << sum1 << " = " << sum2 << " = - " << sum3 << "\n";
    }

    auto it0 = expr0.begin();
    cout << "Equality 0: " << (it0==expr0.begin()) << ", end: " << (it0==expr0.end()) << "\n";
    cout << "it0 cat = ";
    showType(typename decltype(it0)::iterator_category{});// template Category<int>{});
    auto res0 = *it0;
    cout << "   [0] = "      << r0at0 << ", *it0=" << res0<< ", it0[0]=" << it0[0] << "\n";
    ++it0;
    cout << "*(++it0) = " << *it0 << ", expr0[1]=" << expr0[1] << "\n";
    //  it0 = expr0.begin(); // This is a deleted function.
    (it0 += 1)+=1;
    auto it0b = it0;
    (it0b -= 1)-= 1;
    cout << "expr0[2]=" << expr0[2] << ", *it0=" << *it0 << ", back to [0] " << *it0b <<  "\n";
    auto it0c = expr0.begin();
    auto it0b2 = it0c + 2;
    cout << "*(it+2)=" << *it0b2 << ", it[2] = " << it0b[2] << ", *(it-2)=" << *(it0b2-2) << ", it[-2] = " << it0b2[-2] <<  "\n";
    cout << " 0:it diff = " << (it0b2 - it0b) << "\n";

    {
        auto f00 = [](double x){ return -((x > 0) ? int(x + 0.5) : -int(-x + 0.5)); };
        auto expr00 = Expression(f00, expr0); (void)(expr00);
        auto r00at0 = expr00[0];
        cout << "1: r00at0 = " << r00at0 << "  "; showType(r00at0);
        auto r00 = expr00();
        cout << "1:r00 = " << r00 <<  "\n, 00size=" << expr00.size() <<  "\n, 0size=" << expr0.size()<< "\n";
        cout << "XXXX " << HasIteratorAccess<decltype(expr00)>{}() <<"\n";
        using XX = decltype(*expr00.begin());
        showType(XX{});
        auto it00a = expr00.begin();
        auto it00a2 = it00a + 2;
        cout << "00:it diff = " << (it00a - it00a2) << "\n";
    }
    {
        auto f00 = [](int y, double x){ return (-((x > 0) ? int(x + 0.5) : -int(-x + 0.5))) + y; };
        auto expr00 = Expression(f00, v1, expr0);
        auto r00at0 = expr00[0];
        cout << "2: r00at0 = " << r00at0 << "  "; showType(r00at0);
        auto r00 = expr00(); showType(r00);
        cout << "2:r00 = " << r00 << "\n";

        auto f000 = [](int x, int y) { return 0.25 * (3*x + y); };
        auto expr000 = Expression(f000, expr00, v1);
        cout << "expr000.dimension() = " << expr000.dimension() << "\n";
        auto r000at0 = expr000[0];
        cout << "3: r000at0 = " << r000at0 << "  "; showType(r000at0);
        auto r000 = expr000(); showType(r000);
        cout << "3:r000 = " << r000 << ", size=" << expr0.size()<< "\n";
        for (auto& r : r000) r=-999;
        expr000(&r000);
        cout << "3:r000 b = " << r000 << "\n";
        auto it000 = expr000.begin() + 1;
        assert(expr000.begin() - it000 == -1);
    }
    {
        // Test expression factory
        auto expr00 = expressionFactory(9, v1, expr0);
        auto r00at0 = expr00[0];
        cout << "expressionFactory: r00at0 = " << r00at0 << "  "; showType(r00at0);
        auto r00 = expr00();
        cout << "expressionFactory:r00 = " << r00 << "\n";
    }
    auto f1 = [](T x, T y) { return (0.2*x + 0.8*y); };
    auto expr1 = Expression{f1, vda1 /*vla1*/, vda2 /*vla1*/};
    cout << "expr1.dimension() = " << expr1.dimension() << "\n";
    auto r1all = expr1();
    showType(r1all);
    cout << "r1all=" << r1all << "\n";
    auto r1at0 = expr1[0][0][0];
    auto it1 = expr1.begin();
    cout << "   [000] = " << r1all[0][0][0] << " <-> " << r1at0 << ", it1[0][0][0]=" << it1[0][0][0] << "\n";
    cout << "Equality 1: " << (it1==expr1.begin()) << ", end: " << (it1==expr1.end()) << "\n";
    auto r1zero = expr1[0];
    // r1zero = r1zero;  // default assignments have been implicitly deleted!!
    auto& e1ref = expr1; (void)(e1ref); // this works, though.
    showType(r1zero);
    auto r1zerostar = *(++it1);
    showType(r1zerostar);
    cout << "Types of *it and expr[0] are the same? " << std::is_same_v<decltype(r1zero), decltype(r1zerostar)> << ", ";
    auto it1dec = it1--; auto it1inc = it1++;
    cout << "post dec,inc = " << (it1dec == it1) << ", " << (it1inc == expr1.begin())
         <<  ", " << (++it1 == it1dec)<< "\n";

    auto expr1b = Expression{f1, vda2, vda1};
    auto r1ball = expr1b();
    showType(r1ball);
    cout << "r1ball=" << r1ball << "\n";
    cout << "(size=" << expr1b.size() <<" * " << (*expr1b.begin()).size() << " * " << (*(*expr1b.begin()).begin()).size() << ")\n";
    auto it1b = expr1b.begin(); ++it1b; --it1b; auto it1bb(++it1b); //it1bb = ++it1b;
    showType(it1bb);
    // cout << "diff = " << (it1b-it1b); // static assert: Operator difference is defined only for random access iterators.
    cout << "it1b cat = ";
    showType(typename decltype(it1b)::iterator_category{});// template Category<int>{});
    auto res1b = *it1b;
    cout << "typeof *it1b = "; showType(res1b);
    auto it1c = res1b.begin();
    showType(it1c);
    cout << "it1c cat = ";
    showType(typename decltype(it1c)::iterator_category{});// template Category<int>{});
    auto res1c = *it1c;
    cout << "typeof *it1c = "; showType(res1c);
    auto it1d = res1c.begin();
    showType(it1d);
    cout << "it1d cat = ";
    showType(typename decltype(it1d)::iterator_category{});// template Category<int>{});
    cout << "Equality 1b: " << (it1d==res1c.begin()) << ", end: " << (it1d==res1c.end()) << "\n";
    auto res1d = *it1d;
    cout << "typeof *it1d = "; showType(res1d);
    auto res1e = *(*(*expr1b.begin()).begin()).begin();
    cout << "   [000] with iterators = " << res1d << ", *** = " << res1e << "\n";
    { // Test changing the type of target container in operator() full copy.
        auto expr1c = Expression{f1, lva1, expr1};
        auto all1c = expr1c();
        cout << "all1c = " << all1c << "\n";
        whatType<decltype(all1c)>("all1c type=")();

        // Make a vector for full copy
        vector<vector<vector<double>>> allVec;
        allVec.resize(expr1c.size());   // Resize level 1
        for (auto& v2 : allVec) {
            v2.resize((*expr1c.begin()).size()); // Resize level 2
            for (auto& v3 : v2)
                v3.resize((*(*expr1c.begin()).begin()).size()); // Resize level 3
        }
        expr1c(&allVec);
        cout << "allVec (c) = " << allVec << "\n";
        whatType<decltype(allVec)>("allVec type=")();
        cout << "First elems (c): " << allVec[0][0][0] << ", " << *(*(*expr1c.begin()).begin()).begin() << "\n";

        auto expr1d = Expression{f1, expr1, lva1};
        auto all1d = expr1d();
        cout << "all1d = " << all1d << "\n";
        whatType<decltype(all1d)>("all1d type=")();
        expr1d(&allVec);
        cout << "allVec (d) = " << allVec << "\n";
        cout << "First elems (d): " << allVec[0][0][0] << ", " << *(*(*expr1d.begin()).begin()).begin() << "\n";
    }
    { // Test convenience expressions +-*/
        cout << "CONVENIENCE EXPRESSIONS, test A ----->\n";
        cout << "dim(vda1) = " << Dimension<decltype(vda1)>::value << "\n";
        cout << "type dim(vda1) = "; showType(typename Dimension<decltype(vda1)>::ValueType{});
        cout << "dim(expr1) = " << Dimension<decltype(expr1)>::value << "\n";
        cout << "type dim(expr1) = "; showType(typename Dimension<decltype(expr1)>::ValueType());

        cout << "dim(vla1) = " << Dimension<decltype(vla1)>::value << "\n";
        cout << "type dim(vla1) = "; showType(typename Dimension<decltype(vla1)>::ValueType{});
        cout << "dim(expr1b) = " << Dimension<decltype(expr1b)>::value << "\n";
        cout << "type dim(expr1b) = "; showType(typename Dimension<decltype(expr1b)>::ValueType());
        cout << "inputs: " << expr1() << "\n" <<
                "    and " << expr1b() << "\n";
        {
            auto exprOper = expr1 + expr1b;
            cout << "dim(exprOper) = " << Dimension<decltype(exprOper)>::value << "\n";
            cout << "type dim(exprOper) = "; showType(typename Dimension<decltype(exprOper)>::ValueType());
            cout << "+++: " << *(*(*expr1.begin()).begin()).begin() << " + "
                << *(*(*expr1b.begin()).begin()).begin() << " = "
                << *(*(*exprOper.begin()).begin()).begin() << "\n";
            auto all = exprOper();
            cout << "+++ exprOper() = " << all << "\n";

            auto exprOperV = exprOper + vda1;
            exprOperV(&all);
            cout << "exprOperV() = " << all << "\n";
            auto o1 = exprOper[1];
            auto o2 = o1[1];
            auto o3 = o2();
            cout << "o3=o2()=" << o3 << "\n";
            auto o4 = o3[2] + vda1[1][1][2];
            auto o4b = exprOperV[1][1][2];
            cout << "exprOperV[1][1][2] = " << o4 << ", " << o4b <<  "\n";
            static_assert(std::is_same_v<typename decltype(exprOperV.begin())::iterator_category, std::random_access_iterator_tag>,
                          "Iterator category should be random access.");
            whatType<decltype(exprOper)::value_type>("exprOper::value_type = ")();
            whatType<decltype(exprOper[0][0])::value_type>("exprOper[0][0]::value_type = ")();

            auto exprOperVV = vla1 + exprOperV;
            exprOperVV(&all);
            cout << "exprOperVV() = " << all << "\n";
            cout << "+++: " << *(*(*vla1.begin()).begin()).begin() << " + "
                << *(*(*exprOperV.begin()).begin()).begin() << " = "
                << *(*(*exprOperVV.begin()).begin()).begin() << "\n";

            // The 1. level of "vla" is a vector -> random access iterator
            static_assert(std::is_same_v<typename decltype(exprOperVV.begin())::iterator_category, std::random_access_iterator_tag>,
                          "Iterator category should be random access.");
            // The 2. level of "vla" is a list -> bidirectional iterator.
            static_assert(std::is_same_v<typename decltype((*exprOperVV.begin()).begin())::iterator_category, std::bidirectional_iterator_tag>,
                          "Iterator category should be bidirectional.");


            auto exprOperW1 = exprOper + 10;
            exprOperW1(&all);
            cout << "exprOperW1() = " << all << "\n";

            auto exprOperW2 = 0.1 + exprOper;
            exprOperW2(&all);
            cout << "exprOperW2() = " << all << "\n";
            cout << "exprOperW1[1][1][2] = " << exprOperW1[1][1][2] << ", ";
            cout << "exprOperW2[1][1][2] = " << exprOperW2[1][1][2] << "\n";
        }
        {
            auto exprOper = expr1 - expr1b;
            cout << "dim(exprOper) = " << Dimension<decltype(exprOper)>::value << "\n";
            cout << "type dim(exprOper) = "; showType(typename Dimension<decltype(exprOper)>::ValueType());
            cout << "---: " << *(*(*expr1.begin()).begin()).begin() << " - "
                << *(*(*expr1b.begin()).begin()).begin() << " = "
                << *(*(*exprOper.begin()).begin()).begin() << "\n";
            auto all = exprOper();
            cout << "--- exprOper() = " << all << "\n";

            auto exprOperV = exprOper - vda1;
            exprOperV(&all);
            cout << "exprOperV() = " << all << "\n";
            auto exprOperVV = vla1 - exprOperV;
            exprOperVV(&all);
            cout << "exprOperVV() = " << all << "\n";
            cout << "---: " << *(*(*vla1.begin()).begin()).begin() << " - "
                << *(*(*exprOperV.begin()).begin()).begin() << " = "
                << *(*(*exprOperVV.begin()).begin()).begin() << "\n";

            auto exprOperW1 = exprOper - 10;
            exprOperW1(&all);
            cout << "exprOperW1() = " << all << "\n";

            auto exprOperW2 = 0.1 - exprOper;
            exprOperW2(&all);
            cout << "exprOperW2() = " << all << "\n";

            }
        {
            auto exprOper = expr1 * expr1b;
            cout << "dim(exprOper) = " << Dimension<decltype(exprOper)>::value << "\n";
            cout << "type dim(exprOper) = "; showType(typename Dimension<decltype(exprOper)>::ValueType());
            cout << "***: " << *(*(*expr1.begin()).begin()).begin() << " * "
                << *(*(*expr1b.begin()).begin()).begin() << " = "
                << *(*(*exprOper.begin()).begin()).begin() << "\n";
            auto all = exprOper();
            cout << "*** exprOper() = " << all << "\n";

            auto exprOperV = exprOper * vda1;
            exprOperV(&all);
            cout << "exprOperV() = " << all << "\n";
            auto exprOperVV = vla1 * exprOperV;
            exprOperVV(&all);
            cout << "exprOperVV() = " << all << "\n";
            cout << "***: " << *(*(*vla1.begin()).begin()).begin() << " * "
                << *(*(*exprOperV.begin()).begin()).begin() << " = "
                << *(*(*exprOperVV.begin()).begin()).begin() << "\n";

            auto exprOperW1 = exprOper * 10;
            exprOperW1(&all);
            cout << "exprOperW1() = " << all << "\n";

            auto exprOperW2 = 0.1 * exprOper;
            exprOperW2(&all);
            cout << "exprOperW2() = " << all << "\n";
        }
        {
            auto exprOper = expr1 / expr1b;
            cout << "dim(exprOper) = " << Dimension<decltype(exprOper)>::value << "\n";
            cout << "type dim(exprOper) = "; showType(typename Dimension<decltype(exprOper)>::ValueType());
            cout << "///: " << *(*(*expr1.begin()).begin()).begin() << " / "
                << *(*(*expr1b.begin()).begin()).begin() << " = "
                << *(*(*exprOper.begin()).begin()).begin() << "\n";
            auto all = exprOper();
            cout << "/// exprOper() = " << all << "\n";

            auto exprOperV = exprOper / vda1;
            exprOperV(&all);
            cout << "exprOperV() = " << all << "\n";
            auto exprOperVV = vla1 / exprOperV;
            exprOperVV(&all);
            cout << "exprOperVV() = " << all << "\n";
            cout << "///: " << *(*(*vla1.begin()).begin()).begin() << " / "
                << *(*(*exprOperV.begin()).begin()).begin() << " = "
                << *(*(*exprOperVV.begin()).begin()).begin() << "\n";

            auto exprOperW1 = exprOper / 10;
            exprOperW1(&all);
            cout << "exprOperW1() = " << all << "\n";

            auto exprOperW2 = 0.1 / exprOper;
            exprOperW2(&all);
            cout << "exprOperW2() = " << all << "\n";
        }
        {
            auto exprOper = -expr1;
            cout << "dim(exprOper) = " << Dimension<decltype(exprOper)>::value << "\n";
            cout << "type dim(exprOper) = "; showType(typename Dimension<decltype(exprOper)>::ValueType());
            cout << "Unary-: " << *(*(*expr1.begin()).begin()).begin() << " + "
                << *(*(*expr1b.begin()).begin()).begin() << " = "
                << *(*(*exprOper.begin()).begin()).begin() << "\n";
            cout << "Unary-: Calling all \n";
            auto all = exprOper();
            cout << "exprOper() = " << all << "\n";

            auto o1 = exprOper[1];
            auto o2 = o1[1];
            auto o3 = o2();
            cout << "o3=o2()=" << o3 << "\n";
            cout << "exprOper[1][1][2] = " << exprOper[1][1][2] <<", " << expr1[1][1][2] <<  "\n";
        }

        {
            auto exprOperA = expr1 + expr1b;
            auto exprOperB = expr1 - expr1b;
            auto exprOper = exprOperA * exprOperB;

            auto exprOperJoined = (expr1 + expr1b) * (expr1 - expr1b);

            cout << "dim(exprOper) = " << Dimension<decltype(exprOper)>::value << "\n";
            cout << "type dim(exprOper) = "; showType(typename Dimension<decltype(exprOper)>::ValueType());
            cout << "+ * -: " << *(*(*expr1.begin()).begin()).begin() << " +*-/ "
                << *(*(*expr1b.begin()).begin()).begin() << " = "
                << *(*(*exprOper.begin()).begin()).begin() << "\n";
            cout << "+ * -/ Calling all \n";
            auto all = exprOper();
            cout << "..done..\n";
            cout << "+*- exprOper() = " << all << "\n";

            auto allJoined = exprOperJoined();
            cout << "+*- exprOperJoined() = " << allJoined << "\n";
            cout << "[1][1][1] = " << exprOperJoined[1][1][1] << " ?==? "
                 << (expr1[1][1][1] + expr1b[1][1][1]) * (expr1[1][1][1] - expr1b[1][1][1]) << "\n";

            typename decltype(exprOper)::iterator ite = exprOper.begin();
            ite--;
            typename decltype(vla1)::iterator itvla = vla1.begin();
            --itvla;
        }
        cout  << "<----- CONVENIENCE EXPRESSIONS, test A\n";
    }

    auto f2 = [](array<T, 3> x, array<T, 3> y) -> float
        { return std::hypot(x[0] - y[0], x[1] - y[1], x[2] - y[2]); };
    auto expr2 = Expression{f2, vda1, vda2 /*vla1*/};
    cout << "expr2.dimension() = " << expr2.dimension() << "\n"; // Should be 2 because array is function input
    cout << "expr1.dimension() = " << expr1.dimension() << "\n"; // Should be 3 because T=int is function input so array counted.

    auto expr2b = Expression{f2, vda2, vda1 /*vla1*/};
    auto r2all = expr2();
    auto r2allcopy = r2all;
    expr2(&r2allcopy);
    showType(r2all);
    cout << "r2all=" << r2all << "\n";
    cout << "r2allcopy=" << r2allcopy << "\n";
    { // copy and equality tests
      auto xxx=expr2;
      decltype(expr2) yyy(xxx);
      cout << "XXX==YYY {" << (xxx==yyy) << ", " << (expr2 == expr2b) << "}, "; // << "\n";
    }
    auto expr2c11 = Expression(fglobal1, vla1);
    auto expr2c21 = Expression(fglobal2, vla1);
    auto expr2c12 = Expression(fglobal1, vla2);
    auto expr2c22 = Expression(fglobal2, vla2);
    auto vlacomp = vla1;
    auto expr2comp = Expression(fglobal1, vlacomp); // == expr2c11
    cout << "fglobal[1000]: {" << (expr2comp==expr2c11) << ", "<< (expr2comp==expr2c12) <<", "<< (expr2comp==expr2c21) << ", " << (expr2comp==expr2c22) << "}, ";
    (*(*vlacomp.begin()).begin())[1] = 999; // Change one value in the container and expect non-equality. Note that containers are captured by reference.
    cout << "~fglobal[111]: {" << (expr2comp!=expr2c11) << ", "<< (expr2comp!=expr2c12) <<", "<< (expr2comp!=expr2c21) << ", " << (expr2comp!=expr2c22) << "}\n";
    auto expr2c11a = expr2c11[0];

    auto r2 = expr2[0][0];
    cout << "expr2[0][0]=" << r2 << "\n";

    cout << "Arg.type f2="; showType(ArgumentTypes<decltype(f2)>());
    cout << "Res.type f2="; showType(ResultType<decltype(f2)>());

    auto it2 = expr2.begin();
    auto it2b = it2 + 1;
    cout << "Iter2 equality test: " << (it2==expr2.begin()) << ", " << (it2==expr2.end()) << ", " << (it2 == it2b) << "\n";

#if 0 // This should not compile due to incompatible function.
    auto f3 = [](std::string x, std::string y)  { return 0; };
    auto expr3 = Expression(f3, vda1, vda2);
    auto r3 = expr3[0][0][0];
#endif

    int i = 4, j = 10;
    // Non-containers as expression parameters
    auto f3 = [](int x, int y) -> std::string { return " Value = " + std::to_string(x*y+2) + " "; };
    auto expr3 = Expression{f3, i, j};
    cout << "expr3.dimension() = " << expr3.dimension() << "\n";
    auto r3all = expr3();
    auto r3allcopy = r3all;
    expr3(&r3allcopy);
    // expr3(&r2allcopy); // should not compile
    showType(r3all);
    cout << "r3all=" << r3all << ", r3allcopy=" << r3allcopy << "\n";
    { // int -> string -> int conversion chain.
      auto f30 = [offset=9](const std::string& s) ->int { return std::atoi(s.c_str()+offset); };
      int i = f30(r3all);
      auto expr30 = Expression{f30, Expression{f3, v1, v2} };
      auto r30all = expr30();
      cout << "i=" << i << ", Expr Str: " << r30all;
      cout << ", " << expr30[0] << ", is int? " <<  std::is_same_v<decltype(*expr30.begin()), int> << ", size=" << expr30.size() << "\n";
      expr30(&r30all);
      cout << "Expr Str again: " << r30all << "\n";
    }
    auto r3 = expr3[99];
    auto it3 = expr3.begin(); ++it3; --it3; ++it3;
    showType(typename decltype(it3)::iterator_category{});
    showType(typename decltype(it3)::value_type{});
    std::cout << "No container: [99]= " << r3 << ", [0]=" << expr3[0] << ", *it3=" << (*it3)<< ", it3[0]=" << it3[0]<< " \n";
    cout << "Equality 3: " << (it3==expr3.begin()) << ", end: " << (it3==expr3.end()) << ", ";
    auto it3dec = it3--; auto it3inc = it3++;
    cout << "post dec,inc = " << (it3dec == it3) << ", " << (it3inc == expr3.begin()) << ", " << (it3 == ++it3) << "\n";

    // No function args -> should not compile
    // auto expr4 = Expression{[i=10](int k) { return i * k ; }};
    // auto expr5 = Expression();
  }
} // test_a



template <bool DBG = false>
class myFunctor
{
    int _value = 0;
public:
    myFunctor(int value) : _value(value) {
        if constexpr (DBG)
            std::cout << "myFunctor(" << value << ") created\n";
    }

    double operator()(int i) const
    {
        return _value * i + 0.5;
    }

    myFunctor(const myFunctor& other)
    {
        _value= other._value;
        if constexpr (DBG)
            std::cout << "myFunctor(" << _value << ") copy constructed.\n";
    }

    myFunctor& operator=(const myFunctor& other)
    {
        _value= other._value;
        if constexpr (DBG)
            std::cout << "myFunctor(" << _value << ") copy assign constructed.\n";
        return *this;
    }

    myFunctor& operator=(myFunctor&& other)
    {
        _value= other._value;
        other._value = -1;
        if constexpr (DBG)
            std::cout << "myFunctor(" << _value << ") copy assign constructed.\n";
        return *this;
    }

    myFunctor(myFunctor&& other)
    {
        _value= other._value;
        other._value = -1;
        if constexpr (DBG)
            std::cout << "myFunctor(" << _value << ") moved.\n";
    }

    ~myFunctor() {
        if constexpr (DBG)
            std::cout << "myFunctor(" << _value << ") deleted\n";
    }
};

double fglobal(int i) noexcept
{
    return 100 * i + 0.25;
}


Mat2x2 makeTransform(double thetaDegrees, double coeff = 1)
{
    double theta = thetaDegrees*(std::acos(-1) / 180); // pi = acos(-1)
    Mat2x2 mat { array{ coeff * std::cos(theta), coeff * std::sin(theta)},
                 array{-coeff * std::sin(theta), coeff * std::cos(theta)} };
    return mat;
}

auto makeFunction(const Mat2x2 transform, const Vec2 offset)
{
//    cout << "SETUP: offset = " << offset << "\n";
    auto transformPoint = [transform, offset](const Vec2& pt) -> Vec2
        {
            // whatType<decltype(offset)>("decltype(offset) = ")();
//            cout << "LAMBDA: offset = " << offset << ", pt = " << pt;

            Vec2 result;
            result[0] = transform[0][0] * pt[0] + transform[0][1] * pt[1] + offset[0];
            result[1] = transform[1][0] * pt[0] + transform[1][1] * pt[1] + offset[1];
//            cout << ", result = " << result << "\n";
            return result;
        };

    // Vec2 xx {111,222};
    // cout << "SETUP done: " << xx << " = " << transformPoint(xx) << "\n";
    return transformPoint;
}

void test_b()
{
    vector<PointList> vecPtList { { {1,2}, {3,4} }, { {-5,-4}, {-3,-2}} };
    cout << vecPtList << "\n";

    vector<Vec2> vecOffsets { {100,200}, {-200,-100} };
    cout << vecOffsets << "\n";

    vector<Mat2x2> vecTransforms { makeTransform(45), makeTransform(-45) };
    cout << vecTransforms << "\n";

    {
        auto f = [c = 10](double x){return x+c;};
        auto expr = Expression{f, vecPtList };
        cout << "simple expr:" << expr() << "\n";

        whatType<decltype(expr)>("Simple expr type = ")();
        whatType<typename decltype(expr)::TupleOfArguments>("Simple expr args type = ")();

        auto vecCopy = vecPtList;
        auto exprMove = Expression{std::move(f), std::move(vecCopy)};
        whatType<decltype(expr)>("Simple exprMove type = ")();
    }

    auto exprForPointList = [](const PointList& ptList, const Mat2x2& transform, const Vec2& offset)
    {
#if 1
        auto transformPoint = [transform, offset](const Vec2& pt)
        {
            Vec2 result;
            result[0] = transform[0][0] * pt[0] + transform[0][1] * pt[1] + offset[0];
            result[1] = transform[1][0] * pt[0] + transform[1][1] * pt[1] + offset[1];
            return result;
        };
        return Expression{transformPoint, ptList};
#elif 0
//        cout << "SETUP: " << std::hex << (unsigned long)(&transform) << ", " << (unsigned long)(&offset) << std::dec <<"\n";
//        cout << "      offset = " << offset << "\n";
        auto transformPoint = [pTransform = &transform, pOffset = &offset](const Vec2& pt)
        {
//            cout << "transformPoint, pt = " << pt << ": " << std::hex << (unsigned long)(pTransform) << ", " << (unsigned long)(pOffset) << std::dec << ":";
//            cout << "pTransform0 = " << (*pTransform)[0] << "\n";
            Vec2 result;
            result[0] = (*pTransform)[0][0] * pt[0] + (*pTransform)[0][1] * pt[1] + (*pOffset)[0];
            result[1] = (*pTransform)[1][0] * pt[0] + (*pTransform)[1][1] * pt[1] + (*pOffset)[1];
//            cout << "transformPoint result = " << result << "\n";
            return result;
        };
        return Expression{transformPoint, ptList};
#else
        return Expression{makeFunction(transform, offset), ptList};
#endif
    };
    cout << "-----\n";
    auto expr = Expression{exprForPointList, vecPtList, vecTransforms, vecOffsets};
    whatType<decltype(expr)>("expr=")();
    whatType<typename decltype(expr)::TupleOfArguments>("\nargs expr=")();
    // vector<decltype(expr)> eVec {expr, expr};

    cout << "Starting e0 = expr[0]...\n";
    auto e0 = expr[0];
    whatType<decltype(e0)>("\ne0=")();
    whatType<typename decltype(e0)::TupleOfArguments>("\nargs e0=")();

    {
       auto e0Copy = e0;
       auto e0Copy2 = std::move(e0Copy);
       static_assert(std::is_same_v<decltype(e0Copy2), decltype(e0)>, "Type check fails.");
    }

    cout << "\nStarting eb = *expr.begin()...\n";
    auto eb = *expr.begin();

    // auto eb = (expr.begin())[0];
    whatType<decltype(eb)>("\neb=")();


    cout << "\ntypeof e0 == typeof eb ? " << std::is_same_v<decltype(e0), decltype(eb)> << " : ";
    if (e0 == eb)
        cout << "e0 == eb\n";
    else
        cout << "e0 != eb  !!! \n\n";

    cout << "Doing e00 = e0[0]...\n";  //TÄMÄ Ei TOIMI
    auto e00 = e0[0];
    cout << "Done e00 = e0[0]...\n";
    cout << "e00 = " << e00 << "\n";

    e00 = eb[0];
    cout << "eb[0] = " << e00 << "\n";

    whatType<decltype(e00)>("e00=")();

    cout << " [][] all: " << expr[0][0] << ", " << expr[0][1] << ", " << expr[1][0] << ", " << expr[1][1] << "\n";
    auto eAll = expr();
    cout << "--> expr(): " << eAll << "\n";
    cout << "expr[0][0][0] = " << expr[0][0][0] << "\n";
    eAll[0][0][0] = 0;
    expr(&eAll);
    cout << "-->   eAll: " << eAll << "\n";

    if (1){
        cout << "IIIII int begin IIIII\n";
        auto exprToInt = Expression{[](double x){ return int(std::round(x)); }, /*eAll*/ expr};
        whatType<decltype(exprToInt)>("exprToInt=")();
        whatType<typename decltype(exprToInt)::TupleOfArguments>("args exprToInt=")();

        cout << "++>  exprToInt() " << exprToInt() << "  Going to ie0\n";
        auto ie0 = exprToInt[0]; // *exprToInt.begin(); // exprToInt[0];  // MOLEMMAT ANTAA SAMAN TYYPIN ie0:lle.
        whatType<decltype(ie0)>("ie0=")();
        whatType<typename decltype(ie0)::TupleOfArguments>("args ie0=")();

        auto it = std::get<0>(ie0.beginArgIterators());
        whatType<decltype(it)>("ie0 it = ")();

        cout << "going to val\n";
        auto val = *it;
        whatType<decltype(val)>("ie0 it val= ")();
        cout << "  val = " << val << "\n";
        cout << " ie0() = " << ie0() << "\n";


        auto ie00 = ie0[0]; // *ie0.begin(); // ie0[0];
        cout << "...done ie0[0] = " << ie00() << "\n";
        whatType<decltype(ie00)>("ie00=")();
        whatType<typename decltype(ie00)::TupleOfArguments>("args ie00 =")();

        cout << "Going to ie00[0]...\n";
        auto ie000 = ie00[0]; // *ie00.begin(); // ie00[0];
        cout << "...done. ie000 = " << ie000 << "\n";
        whatType<decltype(ie000)>("ie000 type = ")();
        cout << "++> Firsts: " << expr[0][0][0] << " ?==? " << ie000 << "\n";
        cout << "IIIII int end IIIII\n";
    }

    auto eFirst = *e0.begin();
    assert(eFirst == e00);
    cout << "dim expr=" << expr.dimension() << ", dim e0 = " << e0.dimension() << "\n";

    vector<Vec2> q(2);
    e0(&q);
    auto e0All = e0();
    cout << "e0all = " << e0All << ", q= " << q << "\n";

    auto expr_e_plus_c = vecPtList + expr;  // Add an expression to a container
    auto expr_e_times_e = expr * expr;      // Multiply two expressions
    auto expr_e_minus_number = -expr - 42;  // Subtract a constant from an expression.

    // Make a complicated expression out of several simpler ones.
    auto expr_arithmetic = (expr_e_plus_c + expr_e_times_e) / (3.14 * expr_e_minus_number);
    double eLocal = expr[1][1][1];
    eLocal =  ((vecPtList[1][1][1] + eLocal) + (eLocal * eLocal)) / (3.14 * (-eLocal - 42));
    cout << "eLocal = " << eLocal << " ?==? " << expr_arithmetic[1][1][1] << "\n";
    cout << "expr_arithmetic() = " << expr_arithmetic() << "\n";
}


void test_c()
{
    vector<int> fv {100};
    auto f = [coeff=fv](int x){ return double(coeff[0]*x); };

    vector<vector<int>> c{ {1,2,3}, {10,20,30} };
    const myFunctor mfC(200);
          myFunctor mf(100);
    auto e1 = Expression{f, std::cref(c)};
    auto e2 = Expression{f, std::ref(c)};
    auto e3 = Expression{std::cref(f), std::cref(c)};
    auto e4 = Expression{fglobal, std::cref(c)};
    auto e5 = Expression{mfC, c};
    auto e6 = Expression{std::cref(mf), std::cref(c)};
    auto e7 = Expression{std::ref(mf), c};
    auto e8 = Expression{myFunctor/* <true> */(300), std::cref(c)}; // print debug if <true>
    auto eA = makeRefExpression(f, c);          (void)eA; // Get rid of clang's unused variable warnings.
    auto eB = makeRefExpression(mf, c);         (void)eB;
    auto eC = makeRefExpression(fglobal, c);    (void)eC;
//    auto eD = Expression{std::move(f), std::ref(c)};   // Kill the original f
//    auto eE = Expression{std::move(mf), std::move(c)}; // Kills the original c.

    auto e = eA; // e5; // e2;

    whatType<decltype(e)>("  e = ")();
    whatType<typename decltype(e)::TupleOfArguments>("  e._args = ")();
    whatType<typename decltype(e)::FuncPtr>("  e._pf = ")();

    cout << "size c = " << c.size() << ", e.size = " << e.size() << "\n";

    auto e0=e[0];
    whatType<decltype(e0)>("  e0 = ")();
    whatType<typename decltype(e0)::TupleOfArguments>("  e0._args = ")();
    whatType<typename decltype(e0)::FuncPtr>("  e0._pf = ")();

    auto e0b = *e.begin();
    assert(e0==e0b);

    auto e00=e0[0];
    auto e00b = *e0b.begin();

    assert(e00==e00b);
    whatType<decltype(e00)>("  e00 = ")();
    cout << "e = " << e() << ", e00 = " << e00 << "\n";
}

int main()
{
    cout << "##### doing test_a() #####\n";
    test_a();
    cout << "##### doing test_b() #####\n";
    test_b();
    cout << "##### doing test_c() #####\n";
    test_c();
    cout << "##### DONE #####\n";
}
