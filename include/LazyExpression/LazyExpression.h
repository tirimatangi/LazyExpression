#ifndef LAZYEXPRESSION_H
#define LAZYEXPRESSION_H

#include <cstddef>
#include <type_traits>
#include <algorithm>
#include <tuple>
#include <iterator>
#include <utility>
#include <functional>

namespace LazyExpression
{
using std::size_t;

// The expression type to be used by the user.
template <class Func, class... Args> class Expression;

// If type T is a container with an iterator, this is the value type of the container.
template <class T>
using IteratorAccessType = decltype(*std::begin(std::declval<T>()));

// Check if type T is a container which can be accessed with an iterator.
template <class T, class = void>
struct HasIteratorAccess : std::false_type
{
    using Type = void;   // T is not a container so mark it as such by setting Type as void.
    using IteratorType = const std::decay_t<T>*; // Pointer is the iterator of the non-container type T.
    using ValueType = decltype(*std::declval<IteratorType>()); // Value type of the iterator
};

template <class T>
struct HasIteratorAccess<T, std::void_t<IteratorAccessType<std::decay_t<T>>>> : std::true_type
{
    using Type = IteratorAccessType<std::decay_t<T>>; // Possibly reference to the value type of container T
    using IteratorType = decltype(std::begin(std::declval<T>()));   // Iterator type of container T
    using ValueType = decltype(*std::declval<IteratorType>());  // Value type of the iterator
};

// If type T can be accessed with operator[](IntegralType), this is the return type of operator[].
template <class T>
using RandomAccessType =  decltype(std::declval<T>()[0]);

// Check if type T is a container which can be accessed with operator[](IntegralType).
template <class T, class = void>
struct HasRandomAccess : std::false_type
{
    using Type = void; // No random access.
};

template <class T>
struct HasRandomAccess<T, std::void_t<RandomAccessType<std::decay_t<T>>>> : std::true_type
{
    using Type = RandomAccessType<std::decay_t<T>>;
};

// Helper class for storing a variable number of containers.
template <class Func, class... Args> class VariadicExpression;

// Check if type T is an expression or not.
template <class T>
struct IsExpression : std::false_type {};

template <class Func, class... Args>
struct IsExpression<VariadicExpression<Func, Args...>> : std::true_type {};

template <class Func, class... Args>
struct IsExpression<Expression<Func, Args...>> : std::true_type {};

// Check if type T is a reference wrapper or not.
template <class T>
struct IsRefWrapper : std::false_type
{
    template <class U>
    static constexpr auto wrap(U&& u)
    {
        return std::forward<U>(u);
    }
};

template <class T>
struct IsRefWrapper<std::reference_wrapper<T>> : std::true_type
{
    template <class U>
    static constexpr auto wrap(U&& u)
    {
        return std::ref(std::forward<U>(u));
    }
};


// Determines the dimension of a nested container as if it was a multidimensional matrix.
// E.g. For a scalar, the dimension=0, for a vector, the dimension=1, for avector of vectors, the dimension=2 etc.
// Dimension::ValueType is the value type of the innermost container.
// E.g. if T = list<vector<int>>, then ValueType = int and Dimension() == 2.
template <class T> struct Dimension;

template <>
struct Dimension<void> : std::integral_constant<int, -1>
{
    using ValueType = void;
};

template <class T>
struct Dimension : std::integral_constant<int, Dimension<typename HasIteratorAccess<T>::Type>::value + 1>
{
    using ValueType = std::conditional_t<std::is_same_v<typename HasIteratorAccess<T>::Type, void>, T,
        typename Dimension<typename HasIteratorAccess<T>::Type>::ValueType>;
};

// Becomes true_type if all classes have the same dimension.
template <class T, class... Ts>
struct DimensionCheck : std::bool_constant<((Dimension<T>() == Dimension<Ts>()) && ...)> {};

// Solves the return type, argument types and noexcept status of callable F.
template <class F>
struct TypeSolver
{
    template< class R, class... Args >
    static R resultOfFunction(std::function<R(Args...)>);

    template< class R, class... Args >
    static std::tuple<Args...> argumentsAsTupleOf(std::function<R(Args...)>);

    template< class R, class... Args >
    static std::is_nothrow_invocable<F, Args...> isNoExcept(std::function<R(Args...)>);

    constexpr TypeSolver(F) {}; // Dummy constructor for class template argument deduction.
    using ResultType = decltype(resultOfFunction(std::function{std::declval<F>()}));
    using ArgumentTypes = decltype(argumentsAsTupleOf(std::function{std::declval<F>()}));
    using NoExceptType = decltype(isNoExcept(std::function{std::declval<F>()}));
};

// Result type of callable F.
template <class F>
using ResultType = typename TypeSolver<F>::ResultType;

// Arguments of callable F as a tuple.
template <class F>
using ArgumentTypes = typename TypeSolver<F>::ArgumentTypes;

// Check if callable F is noexcept.
template <class F>
using NoExceptType = typename TypeSolver<F>::NoExceptType;

// Rebind converts a container of its current value type into a container of NewType.
// E.g. vector<int> to vector<double>.
template <class Container, class NewType> struct Rebind;

// Specialization for a general dynamic container (e.g. vector, list,...)
template <class ValueType, class... Args, template <class...> class Container, class NewType>
struct Rebind<Container<ValueType, Args...>, NewType>
{
  typedef Container<NewType, typename Rebind<Args, NewType>::Type...> Type;
  // IsDynamic is true if the container constructor takes size parameter (e.g. vector<int> v(10).)
  static constexpr bool IsDynamic = std::is_constructible_v<Type, size_t>;
};

// Spcialization for an std::array-type container with the size given as a template argument.
template <class ValueType, auto N, template <class, size_t> class Container, class NewType>
struct Rebind<Container<ValueType, N>, NewType>
{
  typedef Container<NewType, N> Type;
  static constexpr bool IsDynamic = std::is_constructible_v<Type, size_t>;
};

// Makes an empty nested container whose type is otherwise the same as the type of container C
// except that the value type has changed from FROM to TO.
// E.g. makeEmptyAs<int, double>(vector<vector<int>>(10)) --> vector<vector<double>>(10)
// E.g. makeEmptyAs<vector<int>, double>(list<vector<int>>()) --> list<double>(10) (i.e. vector<int> is replaced with double)
template <class Tfrom, class Tto, class Tcont>
auto makeEmptyAs(const Tcont& c)
{
    using FROM = std::decay_t<Tfrom>;
    using TO = std::decay_t<Tto>;
    using C = std::decay_t<Tcont>;

    static_assert((Dimension<C>{}() > 0 || std::is_convertible_v<FROM, C>), "Type FROM not found in container C");
    static_assert(!IsExpression<TO>{}(), "Target type TO can not be an expression");

    if constexpr (std::is_same_v<C, FROM>) { // C == FROM so no need to descend into container value type.
        return TO();
    }
    else if constexpr (std::is_convertible_v<FROM, typename HasIteratorAccess<C>::Type>) {  // C is a container of FROMs
        using CTO = typename Rebind<C, TO>::Type; // New container with value type TO
        if constexpr  (Rebind<C, TO>::IsDynamic) // The number of elements is given in constructor call
            return CTO(c.size());
        else // The number of elements is baked into the type
            return CTO();
    }
    else { // C is not a container of FROMs so proceed the search to the value type of C.
        using ToType = decltype(makeEmptyAs<FROM, TO>(*std::begin(c))); // Type of the lower dimensional container.
        using CTO = typename Rebind<C, ToType>::Type; // Type of the container otherwise similar to C but the value is ToType.
        if constexpr  (Rebind<C, ToType>::IsDynamic) { // The number of elements is given in constructor call (e.g. vector)
            CTO result(c.size());
            auto itC = std::begin(c);
            auto itResult = std::begin(result);
            while (itC != std::end(c)) {
                *itResult = makeEmptyAs<FROM, TO>(*itC);  // Every element of c may have different size.
                ++itC;
                ++itResult;
            }
            return result;
        }
        else { // The number of elements is baked into the type (e.g. array)
            auto emptyElement = makeEmptyAs<FROM, TO>(*std::begin(c));
            CTO result;
            std::fill(std::begin(result), std::end(result), emptyElement);
            return result;
        }
    }
}

// Determines the common base of the given nested types.
// If there is no common base, the Type will be nullptr_t.
template<typename... Types>
struct LeastDerived;

template<>
struct LeastDerived<> {
    using Type = std::nullptr_t;
};

template<typename T>
struct LeastDerived<T> {
    using Type = T;
};

template<typename T>
struct LeastDerived<T, T> {
    using Type = T;
};

// Determines the least derived of the given nested types T and U.
// Nested means that we assume that either T::U or U::T.
template<typename T, typename U>
struct LeastDerived<T, U> {
    static constexpr bool noLeastDerived = !(std::is_base_of_v<T, U> || std::is_base_of_v<U, T>);
    using Type = std::conditional_t<noLeastDerived, std::nullptr_t,
                                    std::conditional_t<std::is_base_of_v<T, U>, T, U>>;
};

template<typename T, typename U, typename... Rest>
struct LeastDerived<T, U, Rest...> : LeastDerived<typename LeastDerived<T, U>::Type, Rest...> {};

// Get rid of reference wrapper.
template <class T>
struct UnwrapRefwrapper
{
    using type = T;
};

template <class T>
struct UnwrapRefwrapper<std::reference_wrapper<T>>
{
    using type = const T&;
};

template <class T>
using UnwrapRef = typename UnwrapRefwrapper<T>::type;

template <class T>
using UnwrapDecay = typename UnwrapRefwrapper<std::decay_t<T>>::type;

// The Type will be T if T is a container and vector<T*> if it is not.
// So the type will always be a container of a kind.
template <class T>
using ContainerHelper = std::conditional_t<Dimension<T>{}()==0, std::vector<std::decay_t<T>*>, std::decay_t<T>>;

// Category is the iterator category tag of type T if T is a container.
// Otherwise, it is a plain input_iterator_tag.
template <class T>
using Category =
    std::conditional_t<Dimension<T>{}()==0,
                        std::input_iterator_tag,
                        typename std::iterator_traits<typename ContainerHelper<T>::iterator>::iterator_category>;

// Variadic expression template for nested containers.
// Func is the object function with sizeof...(Args) parameters.
// Args are nested containers (or other expressions) from which the function parameters are extracted.
template <class Func, class... Args>
class VariadicExpression
{
public:
    // Argument container types as a tuple.
    using TupleOfArguments = std::tuple<const UnwrapDecay<Args>...>;

    // Check if the type in Args corresponding to the tuple index is a reference wrapper.
    using TupleOfRefInfos = std::tuple<IsRefWrapper<Args>...>;

    // Iterators of the argument containers as a tuple.
    using TupleOfIterators = std::tuple<typename HasIteratorAccess<UnwrapDecay<Args>>::IteratorType...>;

    typedef Func* const FuncPtr;

protected:
    FuncPtr _pf; // Pointer to the function of the expression.

    TupleOfArguments _args; // Tuple of copies or references to container arguments.

    // Apply the function using element at index `pos` of the argument containers.
    template <size_t... I>
    auto applyAt(size_t pos, std::index_sequence<I...>) const
    {
        return std::invoke(*_pf, std::get<I>(beginArgIterators())[pos]...);
    }

    // Apply the function using the elements of the argument containers pointed by the iterators.
    template <size_t... I>
    auto applyAt(const TupleOfIterators& iterators, std::index_sequence<I...>) const
    {
        return std::invoke(*_pf, *std::get<I>(iterators)...);
    }

    // Make a new expression using the given function *pf and argument containers ts...
    template <class F, class... Ts>
    static auto makeExpression(F* pf, Ts&&... ts)
    {
        return VariadicExpression<F, std::decay_t<Ts>...>(pf, std::forward<Ts>(ts)...);
    }

    // Make an expression whose dimension is one step smaller than the dimension
    // of the objects in args. The base object is the one at index `pos`.
    // It is assumed that all iterators have random access property.
    template <size_t... I>
    auto lowerExpressionDimension(size_t pos, std::index_sequence<I...>) const
    {
        // If the argument containers of the this expression are reference wrappers,
        // make the argument containers of the lower dimension expression also reference wrappers
        // So if the original containers were not copied, any of the lower dimension containers
        // will not be copier either.
        return makeExpression(_pf, std::tuple_element_t<I, TupleOfRefInfos>::wrap(std::get<I>(beginArgIterators())[pos])...);
    }

    // This overload uses the given set of iterators pointing to the values
    // which are passed to the function as parameters.
    // Hence, any forward iterator will do. Random access property is not needed.
    template <size_t... I>
    auto lowerExpressionDimension(const TupleOfIterators& iterators, std::index_sequence<I...>) const
    {
        // See the comment in the above overload of this function.
        return makeExpression(_pf, std::tuple_element_t<I, TupleOfRefInfos>::wrap(*std::get<I>(iterators))...);
    }

    // If the element at _args<I> is an expression, expand it with operator().
    // Otherwise, return reference to the Ith argument.
    template <size_t I>
    decltype(auto) expandExpressionAtIndex() const
    {
        if constexpr (IsExpression<std::decay_t<std::tuple_element_t<I, TupleOfArguments>>>{}())
            return std::get<I>(_args)();
        else
            return std::cref(std::get<I>(_args));
    }

    // Return a tuple which is otherwise similar to _args but elements
    // which are expressions have been expanded with operator().
    template <size_t... I>
    auto expandExpressionArguments(std::index_sequence<I...>) const
    {
        return std::make_tuple(expandExpressionAtIndex<I>()...);
    }

    // Makes a new expression using the function of this expression
    // and the given tuple. Obviously the type of the new tuple tpl
    // must be compatible with the argument types of function *(_pf).
    template <class TPL, size_t... I>
    auto makeExpressionFromTuple(const TPL& tpl, std::index_sequence<I...>) const
    {
        return makeExpression(_pf, std::get<I>(tpl)...);
    }

    // Increments all iterators in the iterator pack.
    template <size_t... I>
    static void incrementIterators(TupleOfIterators& iterators, std::index_sequence<I...>)
    {
        (++std::get<I>(iterators),...);
    }

    // Decrements all iterators in the iterator pack.
    template <size_t... I>
    static void decrementIterators(TupleOfIterators& iterators, std::index_sequence<I...>)
    {
        (--std::get<I>(iterators),...);
    }

    // Add n to all iterators in the iterator pack.
    template <size_t... I>
    static void addToIterators(TupleOfIterators& iterators, std::ptrdiff_t n, std::index_sequence<I...>)
    {
        ((std::get<I>(iterators) += n), ...);
    }

    // Friend declaration needed because fillData calls itself with a different
    // set of class template arguments.
    template <class F, class... Ts>
    friend class VariadicExpression;

    // Fills nested container `result` with data calculated by applying the function to the tuple arguments.
    template <class T>
    void fillData(T& result) const
    {
        if constexpr (std::is_invocable_v<FunctionType, Args...>) { // The function can be called with the arguments so
            result = std::apply(*(this->_pf), this->_args);         // further container expansion is not needed.
        }
        else if constexpr (std::is_invocable_v<FunctionType, std::decay_t<IteratorAccessType<UnwrapDecay<Args>>>...>) {
            // Fill in 1D-container using the 1D-containers in the arguments.
            TupleOfIterators iters = beginArgIterators();
            for (auto& res : result) {
                res = applyAt(iters, std::index_sequence_for<Args...>{});
                incrementIterators(iters, std::index_sequence_for<Args...>{});
            }
        }
        else if constexpr (Dimension<T>{}() > 1) {
            // The dimension N > 1 so apply dimension N-1 matrix to each element of dimension N.
            TupleOfIterators iters = beginArgIterators();
            for (auto& res : result) {
                auto lowerDimExpression = lowerExpressionDimension(iters, std::index_sequence_for<Args...>{});
                lowerDimExpression.fillData(res);
                incrementIterators(iters, std::index_sequence_for<Args...>{});
            }
        }
        else { // Dimension is either 0 or 1. Both cases are errors because we can't do recursion anymore.
            static_assert(Dimension<T>{}() > 0, "It looks like the function is not invocable with _args");
            static_assert(Dimension<T>{}() != 1, "It looks like the function argument types and the value types of the iterators do not match");
        }
    }

    // Returns iterator to the beginning of the container if applicaple. Otherwise returns a T*.
    template <class T>
    static auto getBeginIterator(const T& x)
    {
      if constexpr (HasIteratorAccess<T>())
          return std::begin(x);
      else
          return &x;
    }

    // Returns a tuple of iterators pointing to the beginning of each container.
    template <size_t... I>
    TupleOfIterators getAllBeginIterators(std::index_sequence<I...>) const
    {
        return std::make_tuple(getBeginIterator(std::get<I>(this->_args))...);
    }

    // Returns iterator to the end of the container if applicaple. Otherwise returns a T*.
    template <class T>
    static auto getEndIterator(const T& x)
    {
      if constexpr (HasIteratorAccess<T>())
          return std::end(x);
      else
          return &x;
    }

    // Returns a tuple of iterators pointing to the end of each container.
    template <size_t... I>
    TupleOfIterators getAllEndIterators(std::index_sequence<I...>) const
    {
        return std::make_tuple(getEndIterator(std::get<I>(this->_args))...);
    }

public:
     // Function type with reference wrapper and plain function type converted to function pointer (e.g. int(int) --> int(*)(int))
    using FunctionType = std::remove_reference_t<UnwrapDecay<Func>>;

    // Returns a tuple of iterators pointing to the beginning of argument containers.
    TupleOfIterators beginArgIterators() const
    {
        return getAllBeginIterators(std::index_sequence_for<Args...>{});
    }

    // Returns a tuple of iterators pointing to the end of argument containers.
    TupleOfIterators endArgIterators() const
    {
        return getAllEndIterators(std::index_sequence_for<Args...>{});
    }

    friend class iterator;

    // Data accessor for VariadicExpression<Func, Args...> iterator.
    template <class FuncIt, class TupleOfIteratorsIt>
    class IteratorAccessor  {
    protected:
        // using VariadicExpressionType = VariadicExpression<Func, Args...>;
        FuncIt* const _pFunction; // Pointer to the function of the expression.
        TupleOfIteratorsIt _argIterators; // Iterators pointing to the argument containers of the parent expression.

        // Apply the function using the elements of the argument containers pointed by the iterators.
        template <size_t... I>
        auto applyFunctionAt(const TupleOfIteratorsIt& iterators, std::index_sequence<I...>) const
        {
            return std::invoke(*_pFunction, *std::get<I>(iterators)...);
        }

        // Makes a new expression using the function of this expression.
        // The arguments are read by accessing the given pack of iterators.
        template <size_t... I>
        auto lowerExpressionDimensionFromIterator(const TupleOfIteratorsIt& iterators, std::index_sequence<I...>) const
        {
            // If the original containers were not copied, any of the lower dimension containers
            // will not be copier either. Reference wrappers will be used instead.
            // The information on the original referenceness status is carried in type TupleOfRefInfos.
            return makeExpression(_pFunction, std::tuple_element_t<I, TupleOfRefInfos>::wrap(*std::get<I>(iterators))...);
        }
    public:
        IteratorAccessor() : _pFunction(nullptr), _argIterators{} {}

        IteratorAccessor(FuncIt* pFunc, const TupleOfIteratorsIt& tupleOfIters) :
            _pFunction(pFunc),  _argIterators(tupleOfIters) {}

        IteratorAccessor(FuncIt* pFunc, TupleOfIteratorsIt&& tupleOfIters) :
            _pFunction(pFunc),  _argIterators(std::forward<TupleOfIteratorsIt>(tupleOfIters)) {}

        // Iterator access operator
        auto operator*() const
        {
            if constexpr (std::is_invocable_v<FunctionType, typename HasIteratorAccess<UnwrapDecay<Args>>::ValueType...>) {
                // This is what we want: the function can be called with the argument types which are compatible
                // with the value type of the container.
                return applyFunctionAt(_argIterators, std::index_sequence_for<Args...>{});
            }
            else if constexpr (Dimension<std::decay_t<std::tuple_element_t<0, TupleOfArguments>>>{}() > 1) {
                // Recurse to the next level in the nested container type (e.g. from vector<deque<int>> to deque<int>)
                return lowerExpressionDimensionFromIterator(_argIterators, std::index_sequence_for<Args...>{});
            }
            else { // constexpr (Dimension<std::decay_t<std::tuple_element_t<0, TupleOfArguments>>>{}() <= 1
                static_assert(Dimension<std::decay_t<decltype(std::get<0>(_args))>>{}() > 1,
                              "It looks like the function is not compatible with the given arguments. Sorry.");
                return nullptr; // Dummy return value.
            }
        }

    }; // class IteratorAccessor

    // Iterator has to be defined in two parts to appease clang.
    // clang requires that IteratorAccessor is a complete type before
    // the return type of (implicitly recursive) operator* can be resolved.
    // g++ is not so picky.
    // So iterator can access the container data only by calling
    // IteratorAccessor::operator*().
    struct iterator : IteratorAccessor<Func, TupleOfIterators>
    {
        using IteratorAccessor<Func, TupleOfIterators>::IteratorAccessor;

        typedef typename LeastDerived<Category<UnwrapDecay<Args>>...>::Type iterator_category;
        typedef std::decay_t<decltype(std::declval<IteratorAccessor<Func, TupleOfIterators>>().operator*())>   value_type;
        typedef std::ptrdiff_t                                              difference_type;
        typedef value_type*                                                 pointer;
        typedef value_type&                                                 reference;

        // Increment / decrement operators.
        iterator& operator++()
        {
            if constexpr (std::is_invocable_v<FunctionType, Args...>)
                return *this;
            else {
                incrementIterators(this->_argIterators, std::index_sequence_for<Args...>{});
                return *this;
            }
        }

        iterator operator++(int)
        {
            iterator it(*this);
            operator++();
            return it;
        }

        iterator& operator--()
        {
            if constexpr (std::is_invocable_v<FunctionType, Args...>)
                return *this;
            else {
                decrementIterators(this->_argIterators, std::index_sequence_for<Args...>{});
                return *this;
            }
        }

        iterator operator--(int)
        {
            iterator it(*this);
            operator--();
            return it;
        }

        iterator& operator+=(std::ptrdiff_t n)
        {
            if constexpr (std::is_invocable_v<FunctionType, Args...>)
                return *this;
            else {
                addToIterators(this->_argIterators, n, std::index_sequence_for<Args...>{});
                return *this;
            }
        }

        iterator& operator-=(std::ptrdiff_t n)
        {
            return operator+=(-n);
        }

        // Distance between two random access iterators.
        std::ptrdiff_t operator-(const iterator& other) const
        {
            static_assert(std::is_same_v<typename std::iterator_traits<iterator>::iterator_category, std::random_access_iterator_tag>,
                          "Operator difference is defined only for random access iterators.");
            if constexpr (std::is_invocable_v<FunctionType, Args...>)
                return 0;
            else
                return (std::get<0>(this->_argIterators) - std::get<0>(other._argIterators));
        }

        // Add / subtract an integer offset to/from an iterator.
        friend iterator operator+(iterator itCopy, std::ptrdiff_t n)
        {
            itCopy += n;
            return itCopy;
        }

        friend iterator operator-(iterator itCopy, std::ptrdiff_t n)
        {
            itCopy -= n;
            return itCopy;
        }

        // Random access operator. Uses operator* from the accessor class.
        auto operator[](std::ptrdiff_t pos) const
        {
            return *(*this + pos);
        }

        // Two iterators are equal if they refer to the same function and iterators to
        // the expression arguments are equal.
        friend bool operator==(const iterator& a, const iterator& b)
        {
            return (a._pFunction == b._pFunction) && (a._argIterators == b._argIterators);
        }
        friend bool operator!=(const iterator& a, const iterator& b)
        {
            return !(a==b);
        }

    };

    using const_iterator = iterator;   // Expression can not be modified with an iterator so everything is constant.
    using value_type = typename std::iterator_traits<iterator>::value_type;

    VariadicExpression(Func* pf, const Args&... args) : _pf(pf), _args(args...)
    {}

    VariadicExpression(Func* pf, Args&&... args) : _pf(pf), _args(std::forward<Args>(args)...)
    {}

    // Lazy evaluation of the result at index `pos`.
    // Index access operator is noexcept if the function it calls is also noexcept.
    auto operator[](size_t pos) const noexcept(NoExceptType<FunctionType>{}())
    {
        if constexpr (std::is_invocable_v<FunctionType, Args...>) {
            // The function can be called with the arguments so no need to access iterators.
            (void)pos; // Unused
            return std::apply(*_pf, _args);
        }
        else if constexpr (std::is_invocable_v<FunctionType, std::decay_t<IteratorAccessType<UnwrapDecay<Args>>>...>) {
            static_assert(std::is_same_v<typename std::iterator_traits<decltype(this->begin())>::iterator_category, std::random_access_iterator_tag>,
                          "operator[] requires random access iterators (iterator_category == std::random_access_iterator_tag).");
            // This is what we want: the function can be called with the argument types which are compatible
            // with the value type of the container.
            return applyAt(pos, std::index_sequence_for<Args...>{});
        }
        else if constexpr (Dimension<std::decay_t<std::tuple_element_t<0, TupleOfArguments>>>{}() > 1) {
            // Recurse to the next level in the nested container type (e.g. from vector<deque<int>> to deque<int>)
            static_assert(std::is_same_v<typename std::iterator_traits<decltype(this->begin())>::iterator_category, std::random_access_iterator_tag>,
                          "operator[] requires random access iterators (iterator_category == std::random_access_iterator_tag).");
            return lowerExpressionDimension(pos, std::index_sequence_for<Args...>{});
        }
         else {
           static_assert(Dimension<std::decay_t<std::tuple_element_t<0, TupleOfArguments>>>{}() > 1,
                         "It looks like the function is not compatible with the given arguments. Sorry.");
           return nullptr; // Dummy return value.
         }
    }

    // Applies the function to each argument container and returns a container
    // which is otherwise similar to the first argument container except that
    // the innermost value type is replaced with the return type of the function.
    auto operator()() const  // May allocate so is not noexcept.
    {
        // Type of the first argument of the function
        using FromType =  std::tuple_element_t<0, ArgumentTypes<FunctionType>>;
        // Return type of the function.
        using ToType = ResultType<FunctionType>;
        if constexpr (hasExpressionArguments()) {
            // Some arguments are expressions themselves.
            // First expand the expression arguments into a tuple...
            auto tupleOfNonExpressions = expandExpressionArguments(std::index_sequence_for<Args...>{});
            //... and make a new expression using the expanded arguments.
            auto expandedExpression = makeExpressionFromTuple(tupleOfNonExpressions, std::index_sequence_for<Args...>{});
            return expandedExpression();
        }
        else if constexpr (IsExpression<ToType>{}()) {
            // So the function of this expression returns another expression. Let's call it REX.
            // The result type of operator() of REX is RexResultType.
            using RexResultType = decltype(std::declval<ToType>()());
            // The objects returned by REX will be stored in a container of type OutputContainerType.
            // It is the type of the container which is used to store objects which are passed to
            // the function of this expression except that the value type of the container is changed from
            // the argument type of the function of this expression to the result type of REX::operator().
            using OutputContainerType = decltype(makeEmptyAs<FromType, RexResultType>(std::get<0>(this->_args)));

            OutputContainerType result;
            TupleOfIterators iters = beginArgIterators();
            auto itEnd = endArgIterators();
            while (iters != itEnd) {
                // The output container must have push_back method because the sizes of subcontainers are not known.
                // E.g. if OutputContainerType is a vector of deques of ints, each deque
                // returned by applyAt(...)() below may have a different size.
                result.push_back(applyAt(iters, std::index_sequence_for<Args...>{})());
                incrementIterators(iters, std::index_sequence_for<Args...>{});
            }
            return result;
        }
        else { // Arguments are not expressions.
            // Make an empty nested container which is otherwise like the nested container of the first
            // variadic argument c0 given to Expression{f, c0,...} but the innermost value type which matches
            // the first argument type of the function is replaced by the return type of the function.
            auto result = makeEmptyAs<FromType, ToType>(std::get<0>(this->_args));
            fillData(result);
            return result;
        }
    }

    // Applies the function to each element
    // and stores the result in the pre-allocated container object.
    // The type of the object where the result is stored must be
    // compatible with the return type of the function.
    template <class T>
    void operator()(T* p) const noexcept(NoExceptType<FunctionType>{}())
    {
        static_assert(Dimension<T>{}() == Dimension<decltype(this->operator()())>{}(),
                      "Dimension of result object *p is not correct.");
        if constexpr (std::tuple_size_v<decltype(this->_args)> == 0) // The function does not take any arguments
            *p = (*(this->_pf))();
        else if constexpr (hasExpressionArguments()) {
            // See operator()() above for explanation.
            auto tupleOfNonExpressions = expandExpressionArguments(std::index_sequence_for<Args...>{});
            auto expandedExpression = makeExpressionFromTuple(tupleOfNonExpressions, std::index_sequence_for<Args...>{});
            expandedExpression(p);
        }
        else if constexpr (IsExpression<ResultType<FunctionType>>{}()) {
            // See operator()() above for explanation. The result container is now given so it's easier than above.
            TupleOfIterators iters = beginArgIterators();
            auto itTarget = (*p).begin();
            auto itEnd = endArgIterators();
            while (iters != itEnd) {
                // assert(itTarget != (*p).end());
                *itTarget = applyAt(iters, std::index_sequence_for<Args...>{})();
                incrementIterators(iters, std::index_sequence_for<Args...>{});
                ++itTarget;
            }
        }
        else
          fillData(*p);
    }

    // Returns the dimension of the nested container returned by operator(),
    // excluding the dimension of the return type of the function.
    // For example, if the arguments are of type vector<array<int,2>> and
    // the function maps array<int,2> --> array<int,3>, the result of operator()
    // will be vector<array<int,3>>. However, dimension() will be 1, not 2, because
    // the dimension of the return type (i.e. array<int,3>) is excluded.
    static constexpr size_t dimension() noexcept
    {
        if constexpr (std::is_invocable_v<FunctionType, Args...>)
            return 0;
        else if constexpr (std::is_invocable_v<FunctionType, typename HasIteratorAccess<UnwrapDecay<Args>>::ValueType...>)
            return 1;
        else
            return VariadicExpression<Func, std::decay_t<IteratorAccessType<UnwrapDecay<Args>>>...>::dimension() + 1;
    }

    // Return begin and end iterators to the expression.
    // The expression can not be modified through the iterators so they are always constant.
    auto begin() const
    {
        return iterator(_pf, beginArgIterators());
    }

    auto end() const
    {
        return iterator(_pf, endArgIterators());
    }

    auto cbegin() const { return this->begin(); }
    auto cend() const { return this->end(); }

    // Returns size of the outermost of the nested containers.
    constexpr size_t size() const noexcept
    {
        if constexpr (std::tuple_size_v<decltype(_args)> == 0)
            // The function does not take any argument.
            return 0;
        else if constexpr (std::is_invocable_v<FunctionType, Args...>)
            // The arguments either are not containers or the function argument is a container.
            return 0;
        else // Note: recursive call if _args<0> is an expression.
            return std::get<0>(_args).size();
    }

    // Returns true if argument pack Args... has at least one argument which itself is an expression.
    static constexpr bool hasExpressionArguments()
    {
        return std::disjunction_v<IsExpression<std::decay_t<UnwrapDecay<Args>>>...>;
    }

    friend bool operator==(const VariadicExpression<Func, Args...>& a, const VariadicExpression<Func, Args...>& b) noexcept
    {
        return (a._pf == b._pf) && (a._args == b._args);
    }

    friend bool operator!=(const VariadicExpression<Func, Args...>& a, const VariadicExpression<Func, Args...>& b) noexcept
    {
        return !(a==b);
    }

}; // class VariadicExpression

// Structure which holds a copy of the callable function of an expression.
// The expression will have a reference to the copy.
// This helps avoid dangling references if the callable is a functor which goes out of scope.
// Note that type F can also be a reference_wrapper.
template <class F>
class FunctionHolder
{
public:
    FunctionHolder(F&& f) noexcept(std::is_nothrow_move_assignable_v<F>) : _function(std::forward<F>(f)) {}
    FunctionHolder(const F& f) noexcept(std::is_nothrow_copy_assignable_v<F>) : _function(f) {}

protected:
    F _function;
};

template <class Func, class... Args>
class Expression : public FunctionHolder<std::decay_t<Func>>, public VariadicExpression<std::decay_t<Func>, Args...>
{
public:
    using FuncType = std::decay_t<Func>;
    // Copy the function, copy the args.
    Expression(const Func& f, const Args&... args) :
        FunctionHolder<FuncType>(f),
        VariadicExpression<FuncType, Args...>(&this->_function, args...)
    {
        static_assert(std::tuple_size_v<ArgumentTypes<UnwrapDecay<Func>>> == sizeof...(Args),
                      "The number of function arguments must be the same as the number of containers.");
    }

    // Move the function, copy the args.
    Expression(Func&& f, const Args&... args) :
        FunctionHolder<FuncType>(std::forward<Func>(f)),
        VariadicExpression<FuncType, Args...>(&this->_function, args...)
    {
        static_assert(std::tuple_size_v<ArgumentTypes<UnwrapDecay<Func>>> == sizeof...(Args),
                      "The number of function arguments must be the same as the number of containers.");
    }

    // Copy the function, move the args.
    Expression(const Func& f, Args&&... args) :
        FunctionHolder<FuncType>(f),
        VariadicExpression<FuncType, Args...>(&this->_function, std::forward<Args>(args)...)
    {
        static_assert(std::tuple_size_v<ArgumentTypes<UnwrapDecay<Func>>> == sizeof...(Args),
                      "The number of function arguments must be the same as the number of containers.");
    }

    // Move the function, move the args.
    Expression(Func&& f, Args&&... args) :
        FunctionHolder<FuncType>(std::forward<Func>(f)),
        VariadicExpression<FuncType, Args...>(&this->_function, std::forward<Args>(args)...)
    {
        static_assert(std::tuple_size_v<ArgumentTypes<UnwrapDecay<Func>>> == sizeof...(Args),
                      "The number of function arguments must be the same as the number of containers.");
    }

    // Copy the entire expression.
    Expression(const Expression& other) noexcept(std::is_nothrow_copy_assignable_v<FuncType>):
        FunctionHolder<FuncType>(other._function),
        VariadicExpression<FuncType, Args...>(other)
    {
        // A copy of the function was made so refresh the function pointer VariadicExpression._pf
        *const_cast<FuncType**>(&(this->_pf)) = &this->_function;
    }

    // Move the entire expression.
    Expression(Expression&& other) noexcept(std::is_nothrow_move_assignable_v<FuncType>) :
        FunctionHolder<FuncType>(std::move(other._function)),
        VariadicExpression<FuncType, Args...>(other)
    {
        // A copy of the function may have been made so refresh the function pointer VariadicExpression._pf
        *const_cast<FuncType**>(&(this->_pf)) = &this->_function;
    }

    // Expression can not be copied or moved because of const-members.
    // It just becomes too complicated.
    // Use copy and move constructors if possible.
    Expression& operator=(const Expression&) = delete;
    Expression& operator=(Expression&&) = delete;

    ~Expression() = default;
};

// Catch cases where too few arguments are provided.
template <class T>
Expression(T) -> Expression<void, void>;

template <class...>
Expression() -> Expression<void, void>;

// Too few arguments were provided.
template <>
class Expression<void, void>
{
public:
    template <class... T>
    Expression(T...)
    {
        static_assert(sizeof...(T) > 1, "Too few arguments in an expression. The first argument of an Expression must be a function and at least one container argument is needed.");
    }
};

// Convenience expressions for {X+Y,  X-Y,  X*Y, X/Y} for 5 type combinations of X and Y.
// 1. Both X and Y are expressions of the same dimension (i.e. they have equally many nesting levels)
// 2. X is an expression and Y is a container of the same dimension.
// 3. X is an expression and Y is a number.
// 4. X is a container and Y is an expression of the same dimension.
// 5. X is a number and Y an expression.
template <class F1, class F2, class... Args1, class... Args2>
auto operator+(const Expression<F1, Args1...>& e1, const Expression<F2, Args2...>& e2)  // Combination 1
{
    static_assert(Dimension<Expression<F2, Args2...>>{}() == Dimension<Expression<F1, Args1...>>{}(),
                  "Dimensions of the expressions do not match");
    // Value types of the innermost containers in the chain of nested containers
    using T1 = typename Dimension<decltype(e1)>::ValueType;
    using T2 = typename Dimension<decltype(e2)>::ValueType;
    static_assert(!std::is_same_v<T1, void>, "Value type of the first expression is not valid.");
    static_assert(!std::is_same_v<T2, void>, "Value type of the second expression is not valid.");
    auto add = [](const T1& x1, const T2& x2) { return x1 + x2;};
    return Expression(add, e1, e2);
}

template <class F1, class... Args1, class T>
auto operator+(const Expression<F1, Args1...>& e1, const T& t) // Combinations 2 and 3
{
    if constexpr (std::is_arithmetic_v<std::decay_t<T>>) {
        auto f = [coeff=t](const ResultType<F1>& x){ return x + coeff; };
        return Expression{f, e1};
    } else { // T should be a container compatible with the expression.
        static_assert(Dimension<T>{}() == Dimension<Expression<F1, Args1...>>{}(),
                      "Dimensions of the expression and the container do not match");
        using ValueType = typename Dimension<T>::ValueType;
        auto f = [](const ValueType& x){ return x; };
        auto e2 = Expression{f, t};
        return e1 + e2;
    }
}

template <class T, class F2, class... Args2>
auto operator+(const T& t, const Expression<F2, Args2...>& e2) // Combinations 4 and 5
{
    if constexpr (std::is_arithmetic_v<std::decay_t<T>>) {
        auto f = [coeff=t](const ResultType<F2>& x){ return coeff + x; };
        return Expression{f, e2};
    } else { // T should be a container compatible with the expression.
        static_assert(Dimension<T>{}() == Dimension<Expression<F2, Args2...>>{}(),
                      "Dimensions of the expression and the container do not match");
        using ValueType = typename Dimension<T>::ValueType;
        auto f = [](const ValueType& x){ return x; };
        auto e1 = Expression{f, t};
        return e1 + e2;
    }
}


template <class F1, class F2, class... Args1, class... Args2>
auto operator-(const Expression<F1, Args1...>& e1, const Expression<F2, Args2...>& e2)  // Combination 1
{
    static_assert(Dimension<Expression<F2, Args2...>>{}() == Dimension<Expression<F1, Args1...>>{}(),
                  "Dimensions of the expressions do not match");
    // Value types of the innermost containers in the chain of nested containers
    using T1 = typename Dimension<decltype(e1)>::ValueType;
    using T2 = typename Dimension<decltype(e2)>::ValueType;
    static_assert(!std::is_same_v<T1, void>, "Value type of the first expression is not valid.");
    static_assert(!std::is_same_v<T2, void>, "Value type of the second expression is not valid.");
    auto subtract = [](const T1& x1, const T2& x2){ return x1 - x2; };
    return Expression(subtract, e1, e2);
}

template <class F1, class... Args1, class T>
auto operator-(const Expression<F1, Args1...>& e1, const T& t) // Combinations 2 and 3
{
    if constexpr (std::is_arithmetic_v<std::decay_t<T>>) {
        auto f = [coeff=t](const ResultType<F1>& x){ return x - coeff; };
        return Expression{f, e1};
    } else { // T should be a container compatible with the expression.
        static_assert(Dimension<T>{}() == Dimension<Expression<F1, Args1...>>{}(),
                      "Dimensions of the expression and the container do not match");
        using ValueType = typename Dimension<T>::ValueType;
        auto f = [](const ValueType& x){ return x; };
        auto e2 = Expression{f, t};
        return e1 - e2;
    }
}

template <class T, class F2, class... Args2>
auto operator-(const T& t, const Expression<F2, Args2...>& e2) // Combinations 4 and 5
{
    if constexpr (std::is_arithmetic_v<std::decay_t<T>>) {
        auto f = [coeff=t](const ResultType<F2>& x){ return coeff - x; };
        return Expression{f, e2};
    } else { // T should be a container compatible with the expression.
        static_assert(Dimension<T>{}() == Dimension<Expression<F2, Args2...>>{}(),
                      "Dimensions of the expression and the container do not match");
        using ValueType = typename Dimension<T>::ValueType;
        auto f = [](const ValueType& x){ return x; };
        auto e1 = Expression{f, t};
        return e1 - e2;
    }
}


template <class F1, class F2, class... Args1, class... Args2>
auto operator*(const Expression<F1, Args1...>& e1, const Expression<F2, Args2...>& e2)  // Combination 1
{
    static_assert(Dimension<Expression<F2, Args2...>>{}() == Dimension<Expression<F1, Args1...>>{}(),
                  "Dimensions of the expressions do not match");
    // Value types of the innermost containers in the chain of nested containers
    using T1 = typename Dimension<decltype(e1)>::ValueType;
    using T2 = typename Dimension<decltype(e2)>::ValueType;
    static_assert(!std::is_same_v<T1, void>, "Value type of the first expression is not valid.");
    static_assert(!std::is_same_v<T2, void>, "Value type of the second expression is not valid.");
    auto multiply = [](const T1& x1, const T2& x2){ return x1 * x2; };
    return Expression(multiply, e1, e2);
}

template <class F1, class... Args1, class T>
auto operator*(const Expression<F1, Args1...>& e1, const T& t) // Combinations 2 and 3
{
    if constexpr (std::is_arithmetic_v<std::decay_t<T>>) {
        auto f = [coeff=t](const ResultType<F1>& x){ return x * coeff; };
        return Expression{f, e1};
    } else { // T should be a container compatible with the expression.
        static_assert(Dimension<T>{}() == Dimension<Expression<F1, Args1...>>{}(),
                      "Dimensions of the expression and the container do not match");
        using ValueType = typename Dimension<T>::ValueType;  // Valuetype of container T
        auto f = [](const ValueType& x){ return x; };
        auto e2 = Expression{f, t};
        return e1 * e2;
    }
}

template <class T, class F2, class... Args2>
auto operator*(const T& t, const Expression<F2, Args2...>& e2) // Combinations 4 and 5
{
    if constexpr (std::is_arithmetic_v<std::decay_t<T>>) {
        auto f = [coeff=t](const ResultType<F2>& x){ return coeff * x; };
        return Expression{f, e2};
    } else { // T should be a container compatible with the expression.
        static_assert(Dimension<T>{}() == Dimension<Expression<F2, Args2...>>{}(),
                      "Dimensions of the expression and the container do not match");
        using ValueType = typename Dimension<T>::ValueType;
        auto f = [](const ValueType& x){ return x; };
        auto e1 = Expression{f, t};
        return e1 * e2;
    }
}


template <class F1, class F2, class... Args1, class... Args2>
auto operator/(const Expression<F1, Args1...>& e1, const Expression<F2, Args2...>& e2)  // Combination 1
{
    static_assert(Dimension<Expression<F2, Args2...>>{}() == Dimension<Expression<F1, Args1...>>{}(),
                  "Dimensions of the expressions do not match");
    // Value types of the innermost containers in the chain of nested containers
    using T1 = typename Dimension<decltype(e1)>::ValueType;
    using T2 = typename Dimension<decltype(e2)>::ValueType;
    static_assert(!std::is_same_v<T1, void>, "Value type of the first expression is not valid.");
    static_assert(!std::is_same_v<T2, void>, "Value type of the second expression is not valid.");
    auto divide = [](const T1& x1, const T2& x2){ return x1 / x2; };
    return Expression(divide, e1, e2);
}

template <class F1, class... Args1, class T>
auto operator/(const Expression<F1, Args1...>& e1, const T& t) // Combinations 2 and 3
{
    if constexpr (std::is_arithmetic_v<std::decay_t<T>>) {
        auto f = [coeff=t](const ResultType<F1>& x){ return x / coeff; };
        return Expression{f, e1};
    } else { // T should be a container compatible with the expression.
        static_assert(Dimension<T>{}() == Dimension<Expression<F1, Args1...>>{}(), "Dimensions of the expression and the container do not match");
        using ValueType = typename Dimension<T>::ValueType;
        auto f = [](const ValueType& x){ return x; };
        auto e2 = Expression{f, t};
        return e1 / e2;
    }
}

template <class T, class F2, class... Args2>
auto operator/(const T& t, const Expression<F2, Args2...>& e2) // Combinations 4 and 5
{
    if constexpr (std::is_arithmetic_v<std::decay_t<T>>) {
        auto f = [coeff=t](const ResultType<F2>& x){ return coeff / x; };
        return Expression{f, e2};
    } else { // T should be a container compatible with the expression.
        static_assert(Dimension<T>{}() == Dimension<Expression<F2, Args2...>>{}(), "Dimensions of the expression and the container do not match");
        using ValueType = typename Dimension<T>::ValueType;
        auto f = [](const ValueType& x){ return x; };
        auto e1 = Expression{f, t};
        return e1 / e2;
    }
}

// Unary minus
template <class F, class... Args>
auto operator-(const Expression<F, Args...>& e)
{
    using T = typename Dimension<decltype(e)>::ValueType;
    auto changeSign = [](const T& x){ return -x; };
    return  Expression{changeSign, e};
}

// Makes an expression assuming that the lifetime of both the functor
// and the container arguments are longer than the lifetime of the expression.
template <class F, class... Args>
auto makeRefExpression(const F& f, const Args&... args)
{
  return Expression(std::cref(f), std::cref(args)...);
}

} // namespace LazyExpression

#endif // LAZYEXPRESSION_H
