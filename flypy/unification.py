# -*- coding: utf-8 -*-
from __future__ import print_function, division, absolute_import
import re
import sys
import operator
from functools import partial, reduce

from pykit.utils import hashable

#===------------------------------------------------------------------===
# Types
#===------------------------------------------------------------------===

class TypeConstructor(object):
    """
    This is a type constructor, which can be indexed with concrete types.
    A type constructor has parameters which are type variables along with
    any potential constraints.

    This is a type of kind (*, *) -> *, where '*' is a concrete type,
    and '(*, *)' a tuple of N type variables or constraints.

    Attributes
    ==========
    cls: type
        The class implementing the type. We typically create type
        constructors from implementation classes.

    params: tuple([ TypeVar ])
        A type variable with potential constraints.

    supertypes: set([ TypeConstructor ])
        Super types according to the flypy subtype relations. This includes
        only flypy type constructors from @jit classes.

    Examples
    ========

    Type constructor taking anything in type variable `a`:

        Foo[a]

    Type constructor taking a subtype of `Bar`:

        Foo[a <= Bar]
    """

    def __init__(self, cls, params, supertype):
        self.impl = cls
        self.params = tuple(params)
        self.supertype = supertype
        self.supertypes = set([self])
        if supertype is not None:
            self.supertypes |= supertype.supertypes

    @property
    def name(self):
        if self.impl:
            return self.impl.__name__
        return "<Unnamed type constructor>"

    def __getitem__(self, concrete_params):
        assert len(concrete_params) == len(self.params)
        return Type(self, concrete_params)

    def __repr__(self):
        if not self.params:
            return self.name
        return "%s[%s]" % (self.name, self.params)


class Type(object):
    """
    This is a concrete type, i.e. an instantiated type constructor.

    Examples
    ========

        Foo[int8]
    """

    def __init__(self, tcon, params, layout={}, fields={}, virtual=False):
        self.tcon = tcon
        self.params = tuple(params)

        self.layout = dict(layout)
        self.fields = dict(fields)

        self.virtual = virtual

    def __repr__(self):
        return "%s[%s]" % (self.tcon.name, self.params)


class TypeVar(object):
    """
    Type variable, often created through the `tvars` constructor.
    """

    def __init__(self, name):
        self.name = name

    def __repr__(self):
        return "tvar(%r)" % (self.name,)


def tvars(*names):
    """Construct type variables with the given names"""
    if len(names) == 1:
        return TypeVar(names[0])
    return [TypeVar(name) for name in names]


def normalize(typ):
    """
    Normalize the given type to always be a Type, and not a TypeConstructor
    """
    if isinstance(typ, TypeConstructor):
        tcon = typ
        return Type(tcon, tcon.params)
    else:
        assert isinstance(typ, Type)
        return typ

#===------------------------------------------------------------------===
# Subtyping
#===------------------------------------------------------------------===

def superclass(cls):
    """
    Return the super class of the given class.

    NOTE: This breaks for multiple inheritance, but we don't support that.
    """
    mro = cls.__mro__
    if len(mro) > 1:
        return mro[1]
    return None


def supertype(tcon):
    """
    Return the supertype of the given type constructor.
    """
    return tcon.supertype


def issubtype(tcon1, tcon2):
    """
    Check whether tcon1 <: tcon2
    """
    return tcon2 in tcon1.supertypes


def issupertype(tcon1, tcon2):
    """
    Check whether tcon1 >: tcon2
    """
    return issubtype(tcon2, tcon2)


def join_constructors(tcon1, tcon2):
    """
    Take the join of two type constructors. For classes `A` and `B`, this
    is class `Super` such that issubclass(A, Super) and issubclass(B, Super).
    """
    result = tcon1
    while result and not issubtype(tcon2, result):
        result = supertype(result)
    return result


def typejoin(a, b):
    """
    Take the join of two types `a` and `b` according to the subtype relation.

    We have the following relations:

        1) Type constructor relation:

            If Foo <: Bar, then Foo[a] <: Bar[b], if a == b

    TODO: co- and contra-variance

    TODO: The below case, where a subtype uses more type variables than
          the subtype.

        a = tvar('a')

        @jit(params=[a])
        class Iterator(object):
            pass

        a, b = tvar('a', 'b')

        @jit(params=[a, b])
        class MyIterator(Iterator[a]):
            pass
    """
    if a.params != b.params:
        return Object

    tcon = join_constructors(a.tcon, b.tcon)
    return tcon[a.params]

#===------------------------------------------------------------------===
# Constraints
#===------------------------------------------------------------------===

class Constraint(object):
    """
    Define a constraint that can be checked for a pair of types.
    When the constraint is satisfied, it will return either the result type
    and otherwise None.
    """

    def __init__(self, type, satisfier):
        self.rhs_type = type
        self.satisfier = satisfier

    def satisfy(self, lhs_type):
        return self.satisfier(self.rhs_type, lhs_type)


def compose(op, c1, c2):
    """Compose two constraints under some (logical) operator"""
    return lambda t1, t2: op(c1(t1, t2), c2(t1, t2))


And = partial(compose, operator.and_)
Or  = partial(compose, operator.or_)
Subtype = lambda t1, t2: t1 if issubtype(t1, t2) else None
Supertype = lambda t1, t2: t1 if issupertype(t1, t2) else None
Coerce = lambda t1, t2: t2 if coerces(t1, t2) else None

def toplevel(typ):
    """
    Constraint for top-level types in a function signature.

    Examples
    ========

    Subtyping:

        def f(x: Foo[a]):
            ...

        Now `f` will also accept a `Bar[a]` if Bar <: Foo

    Coercion:

        def f(x: int64):
            ...

        Now `f` will also accept an `int32`
    """
    satisfier = Or(Subtype, Coerce)
    return Constraint(typ, satisfier)

#===------------------------------------------------------------------===
# Coercion
#===------------------------------------------------------------------===

def promote(type1, type2):
    """
    Promote two types to some common representation both are compatible with.
    """
    if numeric(type1) and numeric(type2):
        return promote_numeric(type1, type2)
    else:
        return typejoin(type1, type2)


def coerce(type1, type2):
    """
    Check whether type1 can coerce to type2.
    """
    raise NotImplementedError


def numeric(typ):
    """Check whether `typ` is a numeric type"""
    return typ.tcon in (Bool, Int, Float, Complex)

#===------------------------------------------------------------------===
# Unification
#===------------------------------------------------------------------===

def unify(type1, type2):
    """
    Unify two types. `t1` must be a concrete type (no type variables), and `type2`
    may have free type variables.

    We allow coercion from `type1` to `type2` if the `coercion` contraint is active.
    We allow subtyping if the subtype constraint is active, and supertyping if the
    supertype constraint is active.

    Examples
    ========

    (int8, int16) unifies if the coercion constraint is active

        The result is int16

    (Foo[float32], Bar[a]) unifies if Foo <: Bar and the subtype constraint
    is active

        The result is Foo[float32]

    (Foo[float32], Bar[a]) unifies if Foo >: Bar and the supertype constraint
    is active

        The result is Foo[float32]
    """
    assert not free(type1), free(type1)
    assert isinstance(type1, Type)
    assert isinstance(type2, (Type, TypeVar))

    solution, typ = solve(type1, type2)
    return substitute(solution, typ)


def solve(type1, type2, constraints):
    """
    Unify two types. Constraints are pushed up, and solutions trickle down.

    There's roughly 3 cases:

        1) TypeVar

            >>> solve(Foo, TypeVar('a'), {})
            { TypeVar('a') : Foo }, TypeVar('a')

        2) Constraint: push the subtype constraint up

            >>> solve(Child, +Parent)
            {}, Child

        3) Types: recurse

            solve(Foo[a], Foo[b]) = solve(a, b)

    Result
    ======
    The pair (solution, type)

    solution: { TypeVar: Type }
    type: type containing free variables ready for substitution

    Note that substitution needs to be differed, since we first need to promote
    all inputs. This happens in the merge function.
    """
    # Case 1)
    if isinstance(type2, TypeVar):
        return { type2: type1 }, type2

    # Case 2)
    if isinstance(type2, Constraint):
        constraints.add(type2.constraint)
        return solve(type1, type2.type, constraints)

    # Case 3)
    if len(type1.params) != len(type2.params):
        # TODO: subtypes and different number of parameters
        raise UnificationError(
            "Got different number of parameters for types %s and %s" % (type1, type2))

    if type1.impl != type2.impl:
        tcon = determine_constructor(type1, type2, constraints)
    else:
        tcon = type1.tcon

    result = [solve(t1, t2, {}) for t1, t2 in zip(type1.params, type2.params)]
    params, solutions = unzip(result)

    solution = reduce(merge, solutions)
    typ = tcon[params]
    return solution, typ


def merge(solution1, solution2):
    """
    Merge two solution contexts, promoting duplicate solutions along the way.

    Parameters
    ==========
    solution1, solution2: { TypeVar: Type }
        A (partial) solution (a type) for each type variable
    """
    for var, typ in solution2.items():
        if var in solution1:
            typ = promote(solution1[var], typ)
        solution1[var] = typ

    return solution1


def determine_constructor(type1, type2, constraints):
    """
    Determine which of the type constructors to use.
    """
    if '<:' in constraints and issubtype(type1, type2):
        tcon = type1.tcon
    elif '>:' in constraints and issupertype(type1, type2):
        tcon = type1.tcon
    elif 'coerces' in constraints and coerces(type1, type2):
        tcon = type2.tcon
    else:
        # Not an allowed subtype, supertype and no possible coercion
        raise UnificationError("Incompatible types: %s and %s" % (type1, type2))

    return tcon


def substitute(solution, typ):
    """
    Substitute a typing solution for a type, resolving all free type variables.
    """
    return typemap(lambda t: solution.get(t, t), typ)


def typemap(f, typ):
    """
    Map function `f` over Type `typ`, reconstructing it as it goes with the
    results returned by `f`
    """
    if isinstance(typ, Type):
        return f(Type(typ.tcon, [typemap(f, param) for param in params]))
    else:
        return f(type)


def unzip(lst):
    """Unzip a zipped list"""
    return list(zip(*lst))