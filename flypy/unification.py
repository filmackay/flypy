# -*- coding: utf-8 -*-
from __future__ import print_function, division, absolute_import
import re
import sys

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

    def __init__(self, tcon, params, layout={}, fields={}):
        self.tcon = tcon
        self.params = tuple(params)

        self.layout = dict(layout)
        self.fields = dict(fields)

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


def unify(a, b):
    pass
