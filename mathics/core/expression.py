#!/usr/bin/env python
# -*- coding: utf-8 -*-

from __future__ import unicode_literals
from __future__ import absolute_import

import sympy
import mpmath
import math
import re
from itertools import chain
from bisect import bisect_left

from mathics.core.numbers import get_type, dps, prec, min_prec, machine_precision
from mathics.core.convert import sympy_symbol_prefix, SympyExpression

import six
from six.moves import map
from six.moves import range
from six.moves import zip


def fully_qualified_symbol_name(name):
    return (isinstance(name, six.string_types) and
            '`' in name and
            not name.startswith('`') and
            not name.endswith('`') and
            '``' not in name)


def valid_context_name(ctx, allow_initial_backquote=False):
    return (isinstance(ctx, six.string_types) and
            ctx.endswith('`') and
            '``' not in ctx and
            (allow_initial_backquote or not ctx.startswith('`')))


def ensure_context(name):
    assert isinstance(name, six.string_types)
    assert name != ''
    if '`' in name:
        # Symbol has a context mark -> it came from the parser
        assert fully_qualified_symbol_name(name)
        return name
    # Symbol came from Python code doing something like
    # Expression('Plus', ...) -> use System`
    return 'System`' + name


def strip_context(name):
    if '`' in name:
        return name[name.rindex('`') + 1:]
    return name


# system_symbols('A', 'B', ...) -> ['System`A', 'System`B', ...]
def system_symbols(*symbols):
    return [ensure_context(s) for s in symbols]


# system_symbols_dict({'SomeSymbol': ...}) -> {'System`SomeSymbol': ...}
def system_symbols_dict(d):
    return {ensure_context(k): v for k, v in six.iteritems(d)}


class BoxError(Exception):
    def __init__(self, box, form):
        super(BoxError, self).__init__(
            'Box %s cannot be formatted as %s' % (box, form))
        self.box = box
        self.form = form


class ExpressionPointer(object):
    def __init__(self, parent, position):
        self.parent = parent
        self.position = position

    def replace(self, new):
        if self.position == 0:
            self.parent.set_head(new)
        else:
            self.parent.set_leaf(self.position - 1, new)

    def __str__(self):
        return '%s[[%s]]' % (self.parent, self.position)


def from_python(arg):
    number_type = get_type(arg)
    if isinstance(arg, six.integer_types) or number_type == 'z':
        return Integer(arg)
    elif isinstance(arg, float) or number_type == 'f':
        return Real(arg)
    elif number_type == 'q':
        return Rational(arg)
    elif isinstance(arg, complex):
        return Complex(Real(arg.real), Real(arg.imag))
    elif number_type == 'c':
        return Complex(arg.real, arg.imag)
    elif isinstance(arg, six.string_types):
        return String(arg)
        # if arg[0] == arg[-1] == '"':
        #     return String(arg[1:-1])
        # else:
        #     return Symbol(arg)
    elif isinstance(arg, BaseExpression):
        return arg
    elif isinstance(arg, list) or isinstance(arg, tuple):
        return Expression('List', *[from_python(leaf) for leaf in arg])
    else:
        raise NotImplementedError


class KeyComparable(object):
    def get_sort_key(self):
        raise NotImplemented

    def __lt__(self, other):
        return self.get_sort_key() < other.get_sort_key()

    def __gt__(self, other):
        return self.get_sort_key() > other.get_sort_key()

    def __le__(self, other):
        return self.get_sort_key() <= other.get_sort_key()

    def __ge__(self, other):
        return self.get_sort_key() >= other.get_sort_key()

    def __eq__(self, other):
        return self.get_sort_key() == other.get_sort_key()

    def __ne__(self, other):
        return self.get_sort_key() != other.get_sort_key()


class EvaluationToken:
    def __init__(self, time=None, symbols=None):
        self.time = time
        self.symbols = symbols

    def copy(self):
        return EvaluationToken(self.time, self.symbols)

    def reordered(self):
        return EvaluationToken(None, self.symbols)

    @staticmethod
    def union(tokens, evaluation):
        definitions = evaluation.definitions
        for token in tokens:
            if definitions.changed(token):
                return None
        return EvaluationToken(
            definitions.now, set.union(*[token.symbols for token in tokens]))


class BaseExpression(KeyComparable):
    def __new__(cls, *args, **kwargs):
        self = object.__new__(cls)
        self.options = None
        self.pattern_sequence = False
        self.unformatted = self
        self._token = None
        return self

    def clear_token(self):
        self._token = None

    def get_token(self):
        return None

    def sequences(self):
        return None

    def flatten_sequence(self, evaluation):
        return self

    def flatten_pattern_sequence(self, evaluation):
        return self

    def get_attributes(self, definitions):
        return set()

    def evaluate_next(self, evaluation):
        return self.evaluate(evaluation), False

    def evaluate(self, evaluation):
        evaluation.check_stopped()
        return self

    def get_atoms(self, include_heads=True):
        return []

    def get_name(self):
        " Returns symbol's name if Symbol instance "

        return ''

    def is_symbol(self):
        return False

    def is_machine_precision(self):
        return False

    def get_lookup_name(self):
        " Returns symbol name of leftmost head "

        return self.get_name()

    def get_head(self):
        return None

    def get_head_name(self):
        return self.get_head().get_name()

    def get_leaves(self):
        return []

    def get_int_value(self):
        return None

    def get_float_value(self, permit_complex=False):
        return None

    def get_string_value(self):
        return None

    def is_atom(self):
        return False

    def is_true(self):
        return False

    def is_numeric(self):
        # used by NumericQ and expression ordering
        return False

    def flatten(self, head, pattern_only=False, callback=None):
        return self

    def __hash__(self):
        """
        To allow usage of expression as dictionary keys,
        as in Expression.get_pre_choices
        """
        raise NotImplementedError

    def user_hash(self, update):
        # whereas __hash__ is for internal Mathics purposes like using Expressions as dictionary keys and fast
        # comparison of elements, user_hash is called for Hash[]. user_hash should strive to give stable results
        # across versions, whereas __hash__ must not. user_hash should try to hash all the data available, whereas
        # __hash__ might only hash a sample of the data available.
        raise NotImplementedError

    def same(self, other):
        pass

    def get_sequence(self):
        if self.get_head().get_name() == 'System`Sequence':
            return self.leaves
        else:
            return [self]

    def evaluate_leaves(self, evaluation):
        return self

    def apply_rules(self, rules, evaluation, level=0, options=None):
        if options:
            l1, l2 = options['levelspec']
            if level < l1:
                return self, False
            elif l2 is not None and level > l2:
                return self, False

        for rule in rules:
            result = rule.apply(self, evaluation, fully=False)
            if result is not None:
                return result, True
        return self, False

    def do_format(self, evaluation, form):
        formats = system_symbols(
            'InputForm', 'OutputForm', 'StandardForm',
            'FullForm', 'TraditionalForm', 'TeXForm', 'MathMLForm')

        evaluation.inc_recursion_depth()
        try:
            expr = self
            head = self.get_head_name()
            include_form = False
            if head in formats and len(self.get_leaves()) == 1:
                expr = self.leaves[0]
                if not (form == 'System`OutputForm' and head == 'System`StandardForm'):
                    form = head

                    include_form = True
            unformatted = expr

            def format_expr(expr):
                if not(expr.is_atom()) and not(expr.head.is_atom()):
                    # expr is of the form f[...][...]
                    return None
                name = expr.get_lookup_name()
                formats = evaluation.definitions.get_formats(name, form)
                for rule in formats:
                    result = rule.apply(expr, evaluation)
                    if result is not None and result != expr:
                        return result.evaluate(evaluation)
                return None

            if form != 'System`FullForm':
                formatted = format_expr(expr)
                if formatted is not None:
                    result = formatted.do_format(evaluation, form)
                    if include_form:
                        result = Expression(form, result)
                    result.unformatted = unformatted
                    return result

                head = expr.get_head_name()
                if head in formats:
                    expr = expr.do_format(evaluation, form)
                elif (head != 'System`NumberForm' and not expr.is_atom() and
                      head != 'System`Graphics'):
                    new_leaves = [leaf.do_format(evaluation, form)
                                  for leaf in expr.leaves]
                    expr = Expression(
                        expr.head.do_format(evaluation, form), *new_leaves)

            if include_form:
                expr = Expression(form, expr)
            expr.unformatted = unformatted
            return expr
        finally:
            evaluation.dec_recursion_depth()

    def format(self, evaluation, form):
        expr = self.do_format(evaluation, form)
        result = Expression(
            'MakeBoxes', expr, Symbol(form)).evaluate(evaluation)
        return result

    def is_free(self, form, evaluation):
        from mathics.builtin.patterns import item_is_free
        return item_is_free(self, form, evaluation)

    def is_inexact(self):
        return self.get_precision() is not None

    def get_precision(self):
        return None

    def get_option_values(self, evaluation, allow_symbols=False,
                          stop_on_error=True):
        options = self
        if options.has_form('List', None):
            options = options.flatten(Symbol('List'))
            values = options.leaves
        else:
            values = [options]
        option_values = {}
        for option in values:
            symbol_name = option.get_name()
            if allow_symbols and symbol_name:
                options = evaluation.definitions.get_options(symbol_name)
                option_values.update(options)
            else:
                if not option.has_form(('Rule', 'RuleDelayed'), 2):
                    if stop_on_error:
                        return None
                    else:
                        continue
                name = option.leaves[0].get_name()
                if not name:
                    if stop_on_error:
                        return None
                    else:
                        continue
                option_values[name] = option.leaves[1]
        return option_values

    def get_rules_list(self):
        from mathics.core.rules import Rule

        list_expr = self.flatten(Symbol('List'))
        list = []
        if list_expr.has_form('List', None):
            list.extend(list_expr.leaves)
        else:
            list.append(list_expr)
        rules = []
        for item in list:
            if not item.has_form(('Rule', 'RuleDelayed'), 2):
                return None
            rule = Rule(item.leaves[0], item.leaves[1])
            rules.append(rule)
        return rules

    def to_sympy(self, **kwargs):
        raise NotImplementedError

    def to_mpmath(self):
        return None

    def round_to_float(self, evaluation=None, permit_complex=False):
        '''
        Try to round to python float. Return None if not possible.
        '''
        if evaluation is None:
            value = self
        else:
            value = Expression('N', self).evaluate(evaluation)
        if isinstance(value, Number):
            value = value.round()
            return value.get_float_value(permit_complex=permit_complex)

    def __abs__(self):
        return Expression('Abs', self)

    def __pos__(self):
        return self

    def __neg__(self):
        return Expression('Times', self, -1)

    def __add__(self, other):
        return Expression('Plus', self, other)

    def __sub__(self, other):
        return Expression('Plus', self, Expression('Times', other, -1))

    def __mul__(self, other):
        return Expression('Times', self, other)

    def __truediv__(self, other):
        return Expression('Divide', self, other)

    def __floordiv__(self, other):
        return Expression('Floor', Expression('Divide', self, other))

    def __pow__(self, other):
        return Expression('Power', self, other)


class Monomial(object):
    """
    An object to sort monomials, used in Expression.get_sort_key and
    Symbol.get_sort_key.
    """

    def __init__(self, exps_dict):
        self.exps = exps_dict

    def __lt__(self, other):
        return self.__cmp(other) < 0

    def __gt__(self, other):
        return self.__cmp(other) > 0

    def __le__(self, other):
        return self.__cmp(other) <= 0

    def __ge__(self, other):
        return self.__cmp(other) >= 0

    def __eq__(self, other):
        return self.__cmp(other) == 0

    def __ne__(self, other):
        return self.__cmp(other) != 0

    def __cmp(self, other):
        self_exps = self.exps.copy()
        other_exps = other.exps.copy()
        for var in self.exps:
            if var in other.exps:
                dec = min(self_exps[var], other_exps[var])
                self_exps[var] -= dec
                if not self_exps[var]:
                    del self_exps[var]
                other_exps[var] -= dec
                if not other_exps[var]:
                    del other_exps[var]
        self_exps = sorted((var, exp) for var, exp in six.iteritems(self_exps))
        other_exps = sorted((var, exp) for var, exp in six.iteritems(other_exps))

        index = 0
        self_len = len(self_exps)
        other_len = len(other_exps)
        while True:
            if index >= self_len and index >= other_len:
                return 0
            if index >= self_len:
                return -1   # self < other
            if index >= other_len:
                return 1    # self > other
            self_var, self_exp = self_exps[index]
            other_var, other_exp = other_exps[index]
            if self_var < other_var:
                return -1
            if self_var > other_var:
                return 1
            if self_exp != other_exp:
                if index + 1 == self_len or index + 1 == other_len:
                    # smaller exponents first
                    if self_exp < other_exp:
                        return -1
                    elif self_exp == other_exp:
                        return 0
                    else:
                        return 1
                else:
                    # bigger exponents first
                    if self_exp < other_exp:
                        return 1
                    elif self_exp == other_exp:
                        return 0
                    else:
                        return -1
            index += 1
        return 0


def _sequences(leaves):
    for i, leaf in enumerate(leaves):
        if leaf.get_head_name() == 'System`Sequence' or leaf.sequences():
            yield i


class Expression(BaseExpression):
    def __new__(cls, head, *leaves):
        self = super(Expression, cls).__new__(cls)
        if isinstance(head, six.string_types):
            head = Symbol(head)
        self._head = head
        self._leaves = tuple(from_python(leaf) for leaf in leaves)
        self._sequences = None
        return self

    @property
    def head(self):
        return self._head

    @head.setter
    def head(self, value):
        raise ValueError('Expression.head is write protected.')

    @property
    def leaves(self):
        return self._leaves

    @leaves.setter
    def leaves(self, value):
        raise ValueError('Expression.leaves is write protected.')

    def slice(self, head, py_slice, evaluation):
        # faster equivalent to: Expression(head, *self.leaves[py_slice])
        return Structure(head, self, evaluation).slice(self, py_slice)

    def filter(self, head, cond, evaluation):
        # faster equivalent to: Expression(head, [leaf in self.leaves if cond(leaf)])
        return Structure(head, self, evaluation).filter(self, cond)

    def restructure(self, head, leaves, evaluation, cache=None, deps=None):
        # faster equivalent to: Expression(head, *leaves)

        # the caller guarantees that _all_ elements in leaves are either from
        # self.leaves (or its sub trees) or from one of the expression given
        # in the tuple "deps" (or its sub trees).

        # if this method is called repeatedly, and the caller guarantees
        # that no definitions change between subsequent calls, then cache
        # may be passed an initially empty dict to speed up calls.

        if deps is None:
            deps = self
        s = Structure(head, deps, evaluation, cache=cache)
        return s(list(leaves))

    def _no_symbol(self, symbol_name):
        # if this return True, it's safe to say that self.leaves or its
        # sub leaves contain no Symbol with symbol_name. if this returns
        # False, such a Symbol might or might not exist.

        token = self._token
        if token is None:
            return False

        symbols = token.symbols
        if symbols is not None and symbol_name not in symbols:
            return True
        else:
            return False

    def sequences(self):
        seq = self._sequences
        if seq is None:
            if self._no_symbol('System`Sequence'):
                seq = tuple()
            else:
                seq = tuple(_sequences(self._leaves))
            self._sequences = seq
        return seq

    def _flatten_sequence(self, sequence, evaluation):
        indices = self.sequences()
        if not indices:
            return self

        leaves = self._leaves

        flattened = []
        extend = flattened.extend

        k = 0
        for i in indices:
            extend(leaves[k:i])
            extend(sequence(leaves[i]))
            k = i + 1
        extend(leaves[k:])

        return self.restructure(self._head, flattened, evaluation)

    def flatten_sequence(self, evaluation):
        def sequence(leaf):
            if leaf.get_head_name() == 'System`Sequence':
                return leaf._leaves
            else:
                return [leaf]

        return self._flatten_sequence(sequence, evaluation)

    def flatten_pattern_sequence(self, evaluation):
        def sequence(leaf):
            flattened = leaf.flatten_pattern_sequence(evaluation)
            if leaf.get_head_name() == 'System`Sequence' and leaf.pattern_sequence:
                return flattened._leaves
            else:
                return [flattened]

        expr = self._flatten_sequence(sequence, evaluation)
        if hasattr(self, 'options'):
            expr.options = self.options
        return expr

    def _prepare_symbols(self):
        token = self._token

        if token is None:
            time = None
        else:
            sym = token.symbols
            if sym is not None:
                return token
            time = token.time

        sym = set((self.get_head_name(),))

        for leaf in self._leaves:
            if isinstance(leaf, Symbol):
                sym.add(leaf.get_name())
            elif isinstance(leaf, Expression):
                leaf_token = leaf._prepare_symbols()
                sym.update(leaf_token.symbols)

        token = EvaluationToken(time, sym)
        self._token = token
        return token

    def get_token(self):
        # make sure that EvaluationToken is complete (i.e. has symbols).
        # returns None if token does not exist or has no time.

        token = self._token

        if token is None:
            return None

        time = token.time

        if time is None:
            return None

        return self._prepare_symbols()

    def update_token(self, evaluation):
        token = self._token

        if token is None:
            sym = None
        else:
            sym = token.symbols

        self._token = EvaluationToken(evaluation.definitions.now, sym)

    def copy(self, reevaluate=False):
        expr = Expression(self._head.copy(reevaluate))
        expr._leaves = tuple(leaf.copy(reevaluate) for leaf in self._leaves)
        if not reevaluate:
            self._prepare_symbols()  # First[Timing[Fold[#1+#2&, Range[750]]]]
            expr._token = self._token
        expr._sequences = self._sequences
        expr.options = self.options
        expr.original = self
        return expr

    def shallow_copy(self):
        # this is a minimal, shallow copy: head, leaves are shared with
        # the original, only the Expression instance is new.
        expr = Expression(self._head)
        expr._leaves = self._leaves
        self._prepare_symbols()  # First[Timing[Fold[#1+#2&, Range[750]]]]
        expr._token = self._token
        expr._sequences = self._sequences
        expr.options = self.options
        return expr

    def set_positions(self, position=None):
        self.position = position
        self._head.set_positions(ExpressionPointer(self, 0))
        for index, leaf in enumerate(self._leaves):
            leaf.set_positions(ExpressionPointer(self, index + 1))

    def get_head(self):
        return self._head

    def set_head(self, head):
        self._head = head
        self._token = None

    def get_leaves(self):
        return self._leaves

    def get_mutable_leaves(self):  # shallow, mutable copy of the leaves array
        return list(self._leaves)

    def set_leaf(self, index, value):  # leaves are removed, added or replaced
        leaves = list(self._leaves)
        leaves[index] = value
        self._leaves = tuple(leaves)
        self._token = None
        self._sequences = None

    def set_reordered_leaves(self, leaves):  # same leaves, but in a different order
        self._leaves = tuple(leaves)
        self._sequences = None
        if self._token:
            self._token = self._token.reordered()

    def get_lookup_name(self):
        return self._head.get_lookup_name()

    def has_form(self, heads, *leaf_counts):
        """
        leaf_counts:
            (,):        no leaves allowed
            (None,):    no constraint on number of leaves
            (n, None):  leaf count >= n
            (n1, n2, ...):    leaf count in {n1, n2, ...}
        """

        head_name = self._head.get_name()
        if isinstance(heads, (tuple, list, set)):
            if head_name not in [ensure_context(h) for h in heads]:
                return False
        else:
            if head_name != ensure_context(heads):
                return False
        if not leaf_counts:
            return False
        if leaf_counts and leaf_counts[0] is not None:
            count = len(self._leaves)
            if count not in leaf_counts:
                if (len(leaf_counts) == 2 and   # noqa
                    leaf_counts[1] is None and count >= leaf_counts[0]):
                    return True
                else:
                    return False
        return True

    def has_symbol(self, symbol_name):
        if self._no_symbol(symbol_name):
            return False
        return self._head.has_symbol(symbol_name) or any(
            leaf.has_symbol(symbol_name) for leaf in self._leaves)

    def to_sympy(self, **kwargs):
        from mathics.builtin import mathics_to_sympy

        if 'converted_functions' in kwargs:
            functions = kwargs['converted_functions']
            if len(self._leaves) > 0 and self.get_head_name() in functions:
                sym_args = [leaf.to_sympy() for leaf in self._leaves]
                if None in sym_args:
                    return None
                func = sympy.Function(str(
                    sympy_symbol_prefix + self.get_head_name()))(*sym_args)
                return func

        lookup_name = self.get_lookup_name()
        builtin = mathics_to_sympy.get(lookup_name)
        if builtin is not None:
            sympy_expr = builtin.to_sympy(self, **kwargs)
            if sympy_expr is not None:
                return sympy_expr

        return SympyExpression(self)

    def to_python(self, *args, **kwargs):
        """
        Convert the Expression to a Python object:
        List[...]  -> Python list
        DirectedInfinity[1] -> inf
        DirectedInfinity[-1] -> -inf
        True/False -> True/False
        Null       -> None
        Symbol     -> '...'
        String     -> '"..."'
        numbers    -> Python number
        If kwarg n_evaluation is given, apply N first to the expression.
        """

        n_evaluation = kwargs.get('n_evaluation')
        if n_evaluation is not None:
            value = Expression('N', self).evaluate(n_evaluation)
            return value.to_python()
        head_name = self._head.get_name()
        if head_name == 'System`List':
            return [leaf.to_python(*args, **kwargs) for leaf in self._leaves]
        if head_name == 'System`DirectedInfinity' and len(self._leaves) == 1:
            direction = self._leaves[0].get_int_value()
            if direction == 1:
                return float('inf')
            if direction == -1:
                return -float('inf')
        return self

    def get_sort_key(self, pattern_sort=False):

        if pattern_sort:
            """
            Pattern sort key structure:
            0: 0/2:        Atom / Expression
            1: pattern:    0 / 11-31 for blanks / 1 for empty Alternatives /
                               40 for OptionsPattern
            2: 0/1:        0 for PatternTest
            3: 0/1:        0 for Pattern
            4: 0/1:        1 for Optional
            5: head / 0 for atoms
            6: leaves / 0 for atoms
            7: 0/1:        0 for Condition
            """

            name = self._head.get_name()
            pattern = 0
            if name == 'System`Blank':
                pattern = 1
            elif name == 'System`BlankSequence':
                pattern = 2
            elif name == 'System`BlankNullSequence':
                pattern = 3
            if pattern > 0:
                if self._leaves:
                    pattern += 10
                else:
                    pattern += 20
            if pattern > 0:
                return [2, pattern, 1, 1, 0, self._head.get_sort_key(True),
                        tuple(leaf.get_sort_key(True) for leaf in self._leaves), 1]

            if name == 'System`PatternTest':
                if len(self._leaves) != 2:
                    return [3, 0, 0, 0, 0, self._head, self._leaves, 1]
                sub = self._leaves[0].get_sort_key(True)
                sub[2] = 0
                return sub
            elif name == 'System`Condition':
                if len(self._leaves) != 2:
                    return [3, 0, 0, 0, 0, self._head, self._leaves, 1]
                sub = self._leaves[0].get_sort_key(True)
                sub[7] = 0
                return sub
            elif name == 'System`Pattern':
                if len(self._leaves) != 2:
                    return [3, 0, 0, 0, 0, self._head, self._leaves, 1]
                sub = self._leaves[1].get_sort_key(True)
                sub[3] = 0
                return sub
            elif name == 'System`Optional':
                if len(self._leaves) not in (1, 2):
                    return [3, 0, 0, 0, 0, self._head, self._leaves, 1]
                sub = self._leaves[0].get_sort_key(True)
                sub[4] = 1
                return sub
            elif name == 'System`Alternatives':
                min_key = [4]
                min = None
                for leaf in self._leaves:
                    key = leaf.get_sort_key(True)
                    if key < min_key:
                        min = leaf
                        min_key = key
                if min is None:
                    # empty alternatives -> very restrictive pattern
                    return [2, 1]
                return min_key
            elif name == 'System`Verbatim':
                if len(self._leaves) != 1:
                    return [3, 0, 0, 0, 0, self._head, self._leaves, 1]
                return self._leaves[0].get_sort_key(True)
            elif name == 'System`OptionsPattern':
                return [2, 40, 0, 1, 1, 0, self._head, self._leaves, 1]
            else:
                # Append [4] to leaves so that longer expressions have higher
                # precedence
                return [
                    2, 0, 1, 1, 0, self._head.get_sort_key(True),
                    tuple(chain((leaf.get_sort_key(True) for leaf in self._leaves), ([4],))),
                    1]
        else:
            exps = {}
            head = self._head.get_name()
            if head == 'System`Times':
                for leaf in self._leaves:
                    name = leaf.get_name()
                    if leaf.has_form('Power', 2):
                        var = leaf._leaves[0].get_name()
                        exp = leaf._leaves[1].round_to_float()
                        if var and exp is not None:
                            exps[var] = exps.get(var, 0) + exp
                    elif name:
                        exps[name] = exps.get(name, 0) + 1
            elif self.has_form('Power', 2):
                var = self._leaves[0].get_name()
                exp = self._leaves[1].round_to_float()
                if var and exp is not None:
                    exps[var] = exps.get(var, 0) + exp
            if exps:
                return [1 if self.is_numeric() else 2, 2, Monomial(exps), 1,
                        self._head, self._leaves, 1]
            else:
                return [1 if self.is_numeric() else 2, 3, self._head,
                        self._leaves, 1]

    def same(self, other):
        if self.get_head_name() != other.get_head_name():
            return False
        if not self._head.same(other.get_head()):
            return False
        if len(self._leaves) != len(other.get_leaves()):
            return False
        for leaf, other in zip(self._leaves, other.get_leaves()):
            if not leaf.same(other):
                return False
        return True

    def flatten(self, head, pattern_only=False, callback=None, level=None):
        if level is not None and level <= 0:
            return self
        if self._no_symbol(head.get_name()):
            return self
        sub_level = None if level is None else level - 1
        do_flatten = False
        for leaf in self._leaves:
            if leaf.get_head().same(head) and (not pattern_only or leaf.pattern_sequence):
                do_flatten = True
                break
        if do_flatten:
            new_leaves = []
            for leaf in self._leaves:
                if leaf.get_head().same(head) and (not pattern_only or leaf.pattern_sequence):
                    new_leaf = leaf.flatten(head, pattern_only, callback, level=sub_level)
                    if callback is not None:
                        callback(new_leaf._leaves, leaf)
                    new_leaves.extend(new_leaf._leaves)
                else:
                    new_leaves.append(leaf)
            return Expression(self._head, *new_leaves)
        else:
            return self

    def evaluate(self, evaluation):
        from mathics.core.evaluation import ReturnInterrupt

        expr = self
        reevaluate = True
        limit = None
        iteration = 1
        names = set()
        definitions = evaluation.definitions

        old_options = evaluation.options
        evaluation.inc_recursion_depth()

        try:
            while reevaluate:
                # changed before last evaluated?
                if not definitions.changed(expr.get_token()):
                    break

                names.add(expr.get_lookup_name())

                if hasattr(expr, 'options') and expr.options:
                    evaluation.options = expr.options

                expr, reevaluate = expr.evaluate_next(evaluation)
                if not reevaluate:
                    break

                iteration += 1

                if limit is None:
                    limit = definitions.get_config_value('$IterationLimit')
                    if limit is None:
                        limit = 'inf'
                if limit != 'inf' and iteration > limit:
                    evaluation.error('$IterationLimit', 'itlim', limit)
                    return Symbol('$Aborted')

        # "Return gets discarded only if it was called from within the r.h.s.
        # of a user-defined rule."
        # http://mathematica.stackexchange.com/questions/29353/how-does-return-work
        # Otherwise it propogates up.
        #
        except ReturnInterrupt as ret:
            if names.intersection(definitions.user.keys()):
                return ret.expr
            else:
                raise ret
        finally:
            evaluation.options = old_options
            evaluation.dec_recursion_depth()

        return expr

    def evaluate_next(self, evaluation):
        head = self._head.evaluate(evaluation)
        attributes = head.get_attributes(evaluation.definitions)
        leaves = self.get_mutable_leaves()

        def rest_range(indices):
            if 'System`HoldAllComplete' not in attributes:
                if self._no_symbol('System`Evaluate'):
                    return
                for index in indices:
                    leaf = leaves[index]
                    if leaf.has_form('Evaluate', 1):
                        leaves[index] = leaf.evaluate(evaluation)

        def eval_range(indices):
            for index in indices:
                leaf = leaves[index]
                if not leaf.has_form('Unevaluated', 1):
                    leaves[index] = leaf.evaluate(evaluation)

        if 'System`HoldAll' in attributes or 'System`HoldAllComplete' in attributes:
            # eval_range(range(0, 0))
            rest_range(range(len(leaves)))
        elif 'System`HoldFirst' in attributes:
            rest_range(range(0, min(1, len(leaves))))
            eval_range(range(1, len(leaves)))
        elif 'System`HoldRest' in attributes:
            eval_range(range(0, min(1, len(leaves))))
            rest_range(range(1, len(leaves)))
        else:
            eval_range(range(len(leaves)))
            # rest_range(range(0, 0))

        new = Expression(head)
        new._leaves = tuple(leaves)

        if ('System`SequenceHold' not in attributes and    # noqa
            'System`HoldAllComplete' not in attributes):
            new = new.flatten_sequence(evaluation)
            leaves = new._leaves

        for leaf in leaves:
            leaf.unevaluated = False

        if 'System`HoldAllComplete' not in attributes:
            dirty_leaves = None

            for index, leaf in enumerate(leaves):
                if leaf.has_form('Unevaluated', 1):
                    if dirty_leaves is None:
                        dirty_leaves = list(leaves)
                    dirty_leaves[index] = leaf._leaves[0]
                    dirty_leaves[index].unevaluated = True

            if dirty_leaves:
                new = Expression(head)
                new._leaves = tuple(dirty_leaves)
                leaves = dirty_leaves

        def flatten_callback(new_leaves, old):
            for leaf in new_leaves:
                leaf.unevaluated = old.unevaluated

        if 'System`Flat' in attributes:
            new = new.flatten(new._head, callback=flatten_callback)
        if 'System`Orderless' in attributes:
            new.sort()

        new.update_token(evaluation)

        if 'System`Listable' in attributes:
            done, threaded = new.thread(evaluation)
            if done:
                if threaded.same(new):
                    new.update_token(evaluation)
                    return new, False
                else:
                    return threaded, True

        def rules():
            rules_names = set()
            if 'System`HoldAllComplete' not in attributes:
                for leaf in leaves:
                    name = leaf.get_lookup_name()
                    if len(name) > 0:  # only lookup rules if this is a symbol
                        if name not in rules_names:
                            rules_names.add(name)
                            for rule in evaluation.definitions.get_upvalues(name):
                                yield rule
            lookup_name = new.get_lookup_name()
            if lookup_name == new.get_head_name():
                for rule in evaluation.definitions.get_downvalues(lookup_name):
                    yield rule
            else:
                for rule in evaluation.definitions.get_subvalues(lookup_name):
                    yield rule

        for rule in rules():
            result = rule.apply(new, evaluation, fully=False)
            if result is not None:
                if result.same(new):
                    new.update_token(evaluation)
                    return new, False
                else:
                    return result, True

        dirty_leaves = None

        # Expression did not change, re-apply Unevaluated
        for index, leaf in enumerate(new._leaves):
            if leaf.unevaluated:
                if dirty_leaves is None:
                    dirty_leaves = list(new._leaves)
                dirty_leaves[index] = Expression('Unevaluated', leaf)

        if dirty_leaves:
            new = Expression(head)
            new._leaves = tuple(dirty_leaves)

        new.unformatted = self.unformatted
        new.update_token(evaluation)
        return new, False

    def evaluate_leaves(self, evaluation):
        leaves = [leaf.evaluate(evaluation) for leaf in self._leaves]
        head = self._head.evaluate_leaves(evaluation)
        return Expression(head, *leaves)

    def __str__(self):
        return '%s[%s]' % (
            self._head, ', '.join([six.text_type(leaf) for leaf in self._leaves]))

    def __repr__(self):
        return '<Expression: %s>' % self

    def process_style_box(self, options):
        if self.has_form('StyleBox', 1, None):
            rules = self._leaves[1:]
            for rule in rules:
                if rule.has_form('Rule', 2):
                    name = rule._leaves[0].get_name()
                    value = rule._leaves[1]
                    if name == 'System`ShowStringCharacters':
                        value = value.is_true()
                        options = options.copy()
                        options['show_string_characters'] = value
                    elif name == 'System`ImageSizeMultipliers':
                        if value.has_form('List', 2):
                            m1 = value._leaves[0].round_to_float()
                            m2 = value._leaves[1].round_to_float()
                            if m1 is not None and m2 is not None:
                                options = options.copy()
                                options['image_size_multipliers'] = (m1, m2)
            return True, options
        else:
            return False, options

    def boxes_to_text(self, **options):
        from mathics.builtin import box_constructs
        from mathics.builtin.base import BoxConstructError

        is_style, options = self.process_style_box(options)
        if is_style:
            return self._leaves[0].boxes_to_text(**options)
        head = self._head.get_name()
        box_construct = box_constructs.get(head)
        if box_construct is not None:
            try:
                return box_construct.boxes_to_text(self._leaves, **options)
            except BoxConstructError:
                raise BoxError(self, 'text')
        if (self.has_form('RowBox', 1) and  # nopep8
            self._leaves[0].has_form('List', None)):
            return ''.join([leaf.boxes_to_text(**options)
                            for leaf in self._leaves[0]._leaves])
        elif self.has_form('SuperscriptBox', 2):
            return '^'.join([leaf.boxes_to_text(**options)
                               for leaf in self._leaves])
        else:
            raise BoxError(self, 'text')

    def boxes_to_xml(self, **options):
        from mathics.builtin import box_constructs
        from mathics.builtin.base import BoxConstructError

        is_style, options = self.process_style_box(options)
        if is_style:
            return self._leaves[0].boxes_to_xml(**options)
        head = self._head.get_name()
        box_construct = box_constructs.get(head)
        if box_construct is not None:
            try:
                return box_construct.boxes_to_xml(self._leaves, **options)
            except BoxConstructError:
                # raise # uncomment this to see what is going wrong in
                # constructing boxes
                raise BoxError(self, 'xml')
        name = self._head.get_name()
        if (name == 'System`RowBox' and len(self._leaves) == 1 and  # nopep8
            self._leaves[0].get_head_name() == 'System`List'):
            result = []
            inside_row = options.get('inside_row')
            # inside_list = options.get('inside_list')
            options = options.copy()

            def is_list_interior(content):
                if (content.has_form('List', None) and
                    all(leaf.get_string_value() == ','
                        for leaf in content._leaves[1::2])):
                    return True
                return False

            is_list_row = False
            if (len(self._leaves[0]._leaves) == 3 and     # nopep8
                self._leaves[0]._leaves[0].get_string_value() == '{' and
                self._leaves[0]._leaves[2].get_string_value() == '}' and
                self._leaves[0]._leaves[1].has_form('RowBox', 1)):
                content = self._leaves[0]._leaves[1]._leaves[0]
                if is_list_interior(content):
                    is_list_row = True

            if not inside_row and is_list_interior(self._leaves[0]):
                is_list_row = True

            if is_list_row:
                options['inside_list'] = True
            else:
                options['inside_row'] = True

            for leaf in self._leaves[0].get_leaves():
                result.append(leaf.boxes_to_xml(**options))
            return '<mrow>%s</mrow>' % ' '.join(result)
        else:
            options = options.copy()
            options['inside_row'] = True
            if name == 'System`SuperscriptBox' and len(self._leaves) == 2:
                return '<msup>%s %s</msup>' % (
                    self._leaves[0].boxes_to_xml(**options),
                    self._leaves[1].boxes_to_xml(**options))
            if name == 'System`SubscriptBox' and len(self._leaves) == 2:
                return '<msub>%s %s</msub>' % (
                    self._leaves[0].boxes_to_xml(**options),
                    self._leaves[1].boxes_to_xml(**options))
            if name == 'System`SubsuperscriptBox' and len(self._leaves) == 3:
                return '<msubsup>%s %s %s</msubsup>' % (
                    self._leaves[0].boxes_to_xml(**options),
                    self._leaves[1].boxes_to_xml(**options),
                    self._leaves[2].boxes_to_xml(**options))
            elif name == 'System`FractionBox' and len(self._leaves) == 2:
                return '<mfrac>%s %s</mfrac>' % (
                    self._leaves[0].boxes_to_xml(**options),
                    self._leaves[1].boxes_to_xml(**options))
            elif name == 'System`SqrtBox' and len(self._leaves) == 1:
                return '<msqrt>%s</msqrt>' % (
                    self._leaves[0].boxes_to_xml(**options))
            else:
                raise BoxError(self, 'xml')

    def boxes_to_tex(self, **options):
        from mathics.builtin import box_constructs
        from mathics.builtin.base import BoxConstructError

        def block(tex, only_subsup=False):
            if len(tex) == 1:
                return tex
            else:
                if not only_subsup or '_' in tex or '^' in tex:
                    return '{%s}' % tex
                else:
                    return tex

        is_style, options = self.process_style_box(options)
        if is_style:
            return self._leaves[0].boxes_to_tex(**options)
        head = self._head.get_name()
        box_construct = box_constructs.get(head)
        if box_construct is not None:
            try:
                return box_construct.boxes_to_tex(self._leaves, **options)
            except BoxConstructError:
                raise BoxError(self, 'tex')
        name = self._head.get_name()
        if (name == 'System`RowBox' and len(self._leaves) == 1 and  # nopep8
            self._leaves[0].get_head_name() == 'System`List'):
            return ''.join([leaf.boxes_to_tex(**options)
                            for leaf in self._leaves[0].get_leaves()])
        elif name == 'System`SuperscriptBox' and len(self._leaves) == 2:
            tex1 = self._leaves[0].boxes_to_tex(**options)
            sup_string = self._leaves[1].get_string_value()
            if sup_string == '\u2032':
                return "%s'" % tex1
            elif sup_string == '\u2032\u2032':
                return "%s''" % tex1
            else:
                return '%s^%s' % (
                    block(tex1, True),
                    block(self._leaves[1].boxes_to_tex(**options)))
        elif name == 'System`SubscriptBox' and len(self._leaves) == 2:
            return '%s_%s' % (
                block(self._leaves[0].boxes_to_tex(**options), True),
                block(self._leaves[1].boxes_to_tex(**options)))
        elif name == 'System`SubsuperscriptBox' and len(self._leaves) == 3:
            return '%s_%s^%s' % (
                block(self._leaves[0].boxes_to_tex(**options), True),
                block(self._leaves[1].boxes_to_tex(**options)),
                block(self._leaves[2].boxes_to_tex(**options)))
        elif name == 'System`FractionBox' and len(self._leaves) == 2:
            return '\\frac{%s}{%s}' % (
                self._leaves[0].boxes_to_tex(**options),
                self._leaves[1].boxes_to_tex(**options))
        elif name == 'System`SqrtBox' and len(self._leaves) == 1:
            return '\\sqrt{%s}' % self._leaves[0].boxes_to_tex(**options)
        else:
            raise BoxError(self, 'tex')

    def default_format(self, evaluation, form):
        return '%s[%s]' % (self._head.default_format(evaluation, form),
                           ', '.join([leaf.default_format(evaluation, form)
                                      for leaf in self._leaves]))

    def sort(self, pattern=False):
        " Sort the leaves according to internal ordering. "
        leaves = list(self._leaves)
        if pattern:
            leaves.sort(key=lambda e: e.get_sort_key(pattern_sort=True))
        else:
            leaves.sort()
        self.set_reordered_leaves(leaves)

    def filter_leaves(self, head_name):
        # TODO: should use sorting
        head_name = ensure_context(head_name)

        if self._no_symbol(head_name):
            return []
        else:
            return [leaf for leaf in self._leaves if leaf.get_head_name() == head_name]

    def apply_rules(self, rules, evaluation, level=0, options=None):
        """for rule in rules:
            result = rule.apply(self, evaluation, fully=False)
            if result is not None:
                return result"""

        # to be able to access it inside inner function
        new_applied = [False]

        def apply_leaf(leaf):
            new, sub_applied = leaf.apply_rules(
                rules, evaluation, level + 1, options)
            new_applied[0] = new_applied[0] or sub_applied
            return new

        def descend(expr):
            return Expression(expr._head, *[apply_leaf(leaf) for leaf in expr._leaves])

        if options is None:  # default ReplaceAll mode; replace breadth first
            result, applied = super(
                Expression, self).apply_rules(rules, evaluation, level, options)
            if applied:
                return result, True
            head, applied = self._head.apply_rules(rules, evaluation, level, options)
            new_applied[0] = applied
            return descend(Expression(head, *self._leaves)), new_applied[0]
        else:  # Replace mode; replace depth first
            expr = descend(self)
            expr, applied = super(
                Expression, expr).apply_rules(rules, evaluation, level, options)
            new_applied[0] = new_applied[0] or applied
            if not applied and options['heads']:
                # heads in Replace are treated at the level of the arguments, i.e. level + 1
                head, applied = expr._head.apply_rules(rules, evaluation, level + 1, options)
                new_applied[0] = new_applied[0] or applied
                expr = Expression(head, *expr._leaves)
            return expr, new_applied[0]


    def replace_vars(self, vars, options=None,
                     in_scoping=True, in_function=True):
        from mathics.builtin.scoping import get_scoping_vars

        if not in_scoping:
            if (self._head.get_name() in ('System`Module', 'System`Block', 'System`With') and
                len(self._leaves) > 0):  # nopep8

                scoping_vars = set(name for name, new_def in get_scoping_vars(self._leaves[0]))
                """for var in new_vars:
                    if var in scoping_vars:
                        del new_vars[var]"""
                vars = {var: value for var, value in six.iteritems(vars)
                        if var not in scoping_vars}

        leaves = self._leaves
        if in_function:
            if (self._head.get_name() == 'System`Function' and
                len(self._leaves) > 1 and
                (self._leaves[0].has_form('List', None) or
                 self._leaves[0].get_name())):
                if self._leaves[0].get_name():
                    func_params = [self._leaves[0].get_name()]
                else:
                    func_params = [leaf.get_name()
                                   for leaf in self._leaves[0]._leaves]
                if '' not in func_params:
                    body = self._leaves[1]
                    replacement = {name: Symbol(name + '$') for name in func_params}
                    func_params = [Symbol(name + '$') for name in func_params]
                    body = body.replace_vars(replacement, options, in_scoping)
                    leaves = chain([Expression('List', *func_params), body], self._leaves[2:])

        if not vars:  # might just be a symbol set via Set[] we looked up here
            return self.shallow_copy()

        return Expression(
            self._head.replace_vars(
                vars, options=options, in_scoping=in_scoping),
            *[leaf.replace_vars(vars, options=options, in_scoping=in_scoping)
              for leaf in leaves])

    def replace_slots(self, slots, evaluation):
        if self._head.get_name() == 'System`Slot':
            if len(self._leaves) != 1:
                evaluation.message_args('Slot', len(self._leaves), 1)
            else:
                slot = self._leaves[0].get_int_value()
                if slot is None or slot < 0:
                    evaluation.message('Function', 'slot', self._leaves[0])
                elif slot > len(slots) - 1:
                    evaluation.message('Function', 'slotn', slot)
                else:
                    return slots[int(slot)]
        elif self._head.get_name() == 'System`SlotSequence':
            if len(self._leaves) != 1:
                evaluation.message_args('SlotSequence', len(self._leaves), 1)
            else:
                slot = self._leaves[0].get_int_value()
                if slot is None or slot < 1:
                    evaluation.error('Function', 'slot', self._leaves[0])
            return Expression('Sequence', *slots[slot:])
        elif (self._head.get_name() == 'System`Function' and
              len(self._leaves) == 1):
            # do not replace Slots in nested Functions
            return self
        return Expression(self._head.replace_slots(slots, evaluation),
                          *[leaf.replace_slots(slots, evaluation)
                            for leaf in self._leaves])

    def thread(self, evaluation, head=None):
        if head is None:
            head = Symbol('List')

        items = []
        dim = None
        for leaf in self._leaves:
            if leaf.get_head().same(head):
                if dim is None:
                    dim = len(leaf._leaves)
                    items = [(items + [leaf]) for leaf in leaf._leaves]
                elif len(leaf._leaves) != dim:
                    evaluation.message('Thread', 'tdlen')
                    return True, self
                else:
                    for index in range(dim):
                        items[index].append(leaf._leaves[index])
            else:
                if dim is None:
                    items.append(leaf)
                else:
                    for item in items:
                        item.append(leaf)
        if dim is None:
            return False, self
        else:
            leaves = [Expression(self._head, *item) for item in items]
            return True, Expression(head, *leaves)

    def is_numeric(self):
        return (self._head.get_name() in system_symbols(
            'Sqrt', 'Times', 'Plus', 'Subtract', 'Minus', 'Power', 'Abs',
            'Divide', 'Sin') and
            all(leaf.is_numeric() for leaf in self._leaves))
        # TODO: complete list of numeric functions, or access NumericFunction
        # attribute

    def numerify(self, evaluation):
        _prec = None
        for leaf in self._leaves:
            if leaf.is_inexact():
                leaf_prec = leaf.get_precision()
                if _prec is None or leaf_prec < _prec:
                    _prec = leaf_prec
        if _prec is not None:
            new_leaves = self.get_mutable_leaves()
            for index in range(len(new_leaves)):
                leaf = new_leaves[index]
                # Don't "numerify" numbers: they should be numerified
                # automatically by the processing function,
                # and we don't want to lose exactness in e.g. 1.0+I.
                if not isinstance(leaf, Number):
                    n_expr = Expression('N', leaf, Integer(dps(_prec)))
                    n_result = n_expr.evaluate(evaluation)
                    if isinstance(n_result, Number):
                        new_leaves[index] = n_result
            return Expression(self._head, *new_leaves)
        else:
            return self

    def get_atoms(self, include_heads=True):
        if include_heads:
            atoms = self._head.get_atoms()
        else:
            atoms = []
        for leaf in self._leaves:
            atoms.extend(leaf.get_atoms())
        return atoms

    def __hash__(self):
        return hash(('Expression', self._head) + tuple(self._leaves))

    def user_hash(self, update):
        update(("%s>%d>" % (self.get_head_name(), len(self._leaves))).encode('utf8'))
        for leaf in self._leaves:
            leaf.user_hash(update)

    def __getnewargs__(self):
        return (self._head, self._leaves)


class Atom(BaseExpression):

    def is_atom(self):
        return True

    def has_form(self, heads, *leaf_counts):
        if leaf_counts:
            return False
        name = self.get_atom_name()
        if isinstance(heads, tuple):
            return name in heads
        else:
            return heads == name

    def has_symbol(self, symbol_name):
        return False

    def get_head(self):
        return Symbol(self.get_atom_name())

    def get_atom_name(self):
        return self.__class__.__name__

    def __repr__(self):
        return '<%s: %s>' % (self.get_atom_name(), self)

    def replace_vars(self, vars, options=None, in_scoping=True):
        return self

    def replace_slots(self, slots, evaluation):
        return self

    def numerify(self, evaluation):
        return self

    def copy(self, reevaluate=False):
        result = self.do_copy()
        result.original = self
        return result

    def set_positions(self, position=None):
        self.position = position

    def get_sort_key(self, pattern_sort=False):
        if pattern_sort:
            return [0, 0, 1, 1, 0, 0, 0, 1]
        else:
            raise NotImplementedError

    def get_atoms(self, include_heads=True):
        return [self]

    def atom_to_boxes(self, f, evaluation):
        raise NotImplementedError


class Symbol(Atom):
    def __new__(cls, name, sympy_dummy=None):
        self = super(Symbol, cls).__new__(cls)
        self.name = ensure_context(name)
        self.sympy_dummy = sympy_dummy
        return self

    def __str__(self):
        return self.name

    def do_copy(self):
        return Symbol(self.name)

    def boxes_to_text(self, **options):
        return str(self.name)

    def atom_to_boxes(self, f, evaluation):
        return String(evaluation.definitions.shorten_name(self.name))

    def to_sympy(self, **kwargs):
        from mathics.builtin import mathics_to_sympy

        if self.sympy_dummy is not None:
            return self.sympy_dummy

        builtin = mathics_to_sympy.get(self.name)
        if (builtin is None or not builtin.sympy_name or    # nopep8
            not builtin.is_constant()):
            return sympy.Symbol(sympy_symbol_prefix + self.name)
        else:
            return builtin.to_sympy(self)

    def to_python(self, *args, **kwargs):
        if self.name == 'System`True':
            return True
        if self.name == 'System`False':
            return False
        if self.name == 'System`Null':
            return None
        n_evaluation = kwargs.get('n_evaluation')
        if n_evaluation is not None:
            value = Expression('N', self).evaluate(n_evaluation)
            return value.to_python()

        # return name as string (Strings are returned with quotes)
        return self.name

    def default_format(self, evaluation, form):
        return self.name

    def get_attributes(self, definitions):
        return definitions.get_attributes(self.name)

    def get_name(self):
        return self.name

    def is_symbol(self):
        return True

    def get_sort_key(self, pattern_sort=False):
        if pattern_sort:
            return super(Symbol, self).get_sort_key(True)
        else:
            return [1 if self.is_numeric() else 2,
                    2, Monomial({self.name: 1}), 0, self.name, 1]

    def same(self, other):
        return isinstance(other, Symbol) and self.name == other.name

    def replace_vars(self, vars, options={}, in_scoping=True):
        assert all(fully_qualified_symbol_name(v) for v in vars)
        var = vars.get(self.name, None)
        if var is None:
            return self
        else:
            return var

    def has_symbol(self, symbol_name):
        return self.name == ensure_context(symbol_name)

    def evaluate(self, evaluation):
        rules = evaluation.definitions.get_ownvalues(self.name)
        for rule in rules:
            result = rule.apply(self, evaluation, fully=True)
            if result is not None and result != self:
                return result.evaluate(evaluation)
        return self

    def is_true(self):
        return self.name == 'System`True'

    def is_numeric(self):
        return self.name in system_symbols(
            'Pi', 'E', 'EulerGamma', 'GoldenRatio',
            'MachinePrecision', 'Catalan')

    def __hash__(self):
        return hash(('Symbol', self.name))  # to distinguish from String

    def user_hash(self, update):
        update(b'System`Symbol>' + self.name.encode('utf8'))

    def __getnewargs__(self):
        return (self.name, self.sympy_dummy)


class Number(Atom):
    def __str__(self):
        return str(self.value)

    @staticmethod
    def from_mpmath(value, prec=None):
        'Converts mpf or mpc to Number.'
        if isinstance(value, mpmath.mpf):
            if prec is None:
                return MachineReal(float(value))
            else:
                # HACK: use str here to prevent loss of precision
                return PrecisionReal(sympy.Float(str(value), prec))
        elif isinstance(value, mpmath.mpc):
            real = Number.from_mpmath(value.real, prec)
            imag = Number.from_mpmath(value.imag, prec)
            return Complex(real, imag)
        else:
            raise TypeError(type(value))

    def is_numeric(self):
        return True


def _ExponentFunction(value):
    n = value.get_int_value()
    if -5 <= n <= 5:
        return Symbol('Null')
    else:
        return value


def _NumberFormat(man, base, exp, options):
    if exp.get_string_value():
        if options['_Form'] in ('System`InputForm', 'System`OutputForm', 'System`FullForm'):
            return Expression('RowBox', Expression('List', man, String('*^'), exp))
        else:
            return Expression('RowBox', Expression('List', man, String(options['NumberMultiplier']),
                                                   Expression('SuperscriptBox', base, exp)))
    else:
        return man


_number_form_options = {
    'DigitBlock': [0, 0],
    'ExponentFunction': _ExponentFunction,
    'ExponentStep': 1,
    'NumberFormat': _NumberFormat,
    'NumberPadding': ['', '0'],
    'NumberPoint': '.',
    'NumberSigns': ['-', ''],
    'SignPadding': False,
    'NumberMultiplier': '\u00d7',
}

class Integer(Number):
    def __new__(cls, value):
        n = int(value)
        self = super(Integer, cls).__new__(cls)
        self.value = n
        return self

    def boxes_to_text(self, **options):
        return str(self.value)

    def boxes_to_xml(self, **options):
        return self.make_boxes('MathMLForm').boxes_to_xml(**options)

    def boxes_to_tex(self, **options):
        return str(self.value)

    def make_boxes(self, form):
        return String(str(self.value))

    def atom_to_boxes(self, f, evaluation):
        return self.make_boxes(f.get_name())

    def default_format(self, evaluation, form):
        return str(self.value)

    def to_sympy(self, **kwargs):
        return sympy.Integer(self.value)

    def to_mpmath(self):
        return mpmath.mpf(self.value)

    def to_python(self, *args, **kwargs):
        return self.value

    def round(self, d=None):
        if d is None:
            return MachineReal(float(self.value))
        else:
            return PrecisionReal(sympy.Float(self.value, d))

    def get_int_value(self):
        return self.value

    def same(self, other):
        return isinstance(other, Integer) and self.value == other.value

    def evaluate(self, evaluation):
        evaluation.check_stopped()
        return self

    def get_sort_key(self, pattern_sort=False):
        if pattern_sort:
            return super(Integer, self).get_sort_key(True)
        else:
            return [0, 0, self.value, 0, 1]

    def do_copy(self):
        return Integer(self.value)

    def __hash__(self):
        return hash(('Integer', self.value))

    def user_hash(self, update):
        update(b'System`Integer>' + str(self.value).encode('utf8'))

    def __getnewargs__(self):
        return (self.value,)

    def __neg__(self):
        return Integer(-self.value)

    @property
    def is_zero(self):
        return self.value == 0


class Rational(Number):
    def __new__(cls, numerator, denominator=None):
        self = super(Rational, cls).__new__(cls)
        self.value = sympy.Rational(numerator, denominator)
        return self

    def atom_to_boxes(self, f, evaluation):
        return self.format(evaluation, f.get_name())

    def to_sympy(self, **kwargs):
        return self.value

    def to_mpmath(self):
        return mpmath.mpf(self.value)

    def to_python(self, *args, **kwargs):
        return float(self.value)

    def round(self, d=None):
        if d is None:
            return MachineReal(float(self.value))
        else:
            return PrecisionReal(self.value.n(d))

    def same(self, other):
        return isinstance(other, Rational) and self.value == other.value

    def numerator(self):
        return Integer(self.value.as_numer_denom()[0])

    def denominator(self):
        return Integer(self.value.as_numer_denom()[1])

    def do_format(self, evaluation, form):
        assert fully_qualified_symbol_name(form)
        if form == 'System`FullForm':
            return Expression(
                Expression('HoldForm', Symbol('Rational')), self.numerator(),
                self.denominator()).do_format(evaluation, form)
        else:
            numerator = self.numerator()
            minus = numerator.value < 0
            if minus:
                numerator = Integer(-numerator.value)
            result = Expression('Divide', numerator, self.denominator())
            if minus:
                result = Expression('Minus', result)
            result = Expression('HoldForm', result)
            return result.do_format(evaluation, form)

    def default_format(self, evaluation, form):
        return 'Rational[%s, %s]' % self.value.as_numer_denom()

    def evaluate(self, evaluation):
        evaluation.check_stopped()
        return self

    def get_sort_key(self, pattern_sort=False):
        if pattern_sort:
            return super(Rational, self).get_sort_key(True)
        else:
            # HACK: otherwise "Bus error" when comparing 1==1.
            return [0, 0, sympy.Float(self.value), 0, 1]

    def do_copy(self):
        return Rational(self.value)

    def __hash__(self):
        return hash(("Rational", self.value))

    def user_hash(self, update):
        update(b'System`Rational>' + ('%s>%s' % self.value.as_numer_denom()).encode('utf8'))

    def __getnewargs__(self):
        return (self.numerator().get_int_value(), self.denominator().get_int_value())

    def __neg__(self):
        return Rational(-self.numerator().get_int_value(), self.denominator().get_int_value())

    @property
    def is_zero(self):
        return self.numerator == 0


class Real(Number):
    def __new__(cls, value, p=None):
        if isinstance(value, six.string_types):
            value = str(value)
            if p is None:
                digits = (''.join(re.findall('[0-9]+', value))).lstrip('0')
                if digits == '':     # Handle weird Mathematica zero case
                    p = max(prec(len(value.replace('0.', ''))),
                            machine_precision)
                else:
                    p = prec(len(digits.zfill(dps(machine_precision))))
        elif isinstance(value, sympy.Float):
            if p is None:
                p = value._prec + 1
        elif isinstance(value,
                        (Integer, sympy.Number, mpmath.mpf, float, int)):
            if p is not None and p > machine_precision:
                value = str(value)
        else:
            raise TypeError('Unknown number type: %s (type %s)' % (
                value, type(value)))

        # return either machine precision or arbitrary precision real
        if p is None or p == machine_precision:
            return MachineReal.__new__(MachineReal, value)
        else:
            return PrecisionReal.__new__(PrecisionReal, value)

    def boxes_to_text(self, **options):
        return self.make_boxes('System`OutputForm').boxes_to_text(**options)

    def boxes_to_xml(self, **options):
        return self.make_boxes('System`MathMLForm').boxes_to_xml(**options)

    def boxes_to_tex(self, **options):
        return self.make_boxes('System`TeXForm').boxes_to_tex(**options)

    def atom_to_boxes(self, f, evaluation):
        return self.make_boxes(f.get_name())

    def evaluate(self, evaluation):
        evaluation.check_stopped()
        return self

    def get_sort_key(self, pattern_sort=False):
        if pattern_sort:
            return super(Real, self).get_sort_key(True)
        return [0, 0, self.value, 0, 1]

    def __eq__(self, other):
        if isinstance(other, Real):
            # MMA Docs: "Approximate numbers that differ in their last seven
            # binary digits are considered equal"
            _prec = min_prec(self, other)
            with mpmath.workprec(_prec):
                rel_eps = 0.5 ** (_prec - 7)
                return mpmath.almosteq(self.to_mpmath(), other.to_mpmath(), abs_eps=0, rel_eps=rel_eps)
        else:
            return self.get_sort_key() == other.get_sort_key()

    def __ne__(self, other):
        # Real is a total order
        return not (self == other)

    def __hash__(self):
        # ignore last 7 binary digits when hashing
        _prec = self.get_precision()
        return hash(("Real", self.to_sympy().n(dps(_prec))))

    def user_hash(self, update):
        # ignore last 7 binary digits when hashing
        _prec = self.get_precision()
        update(b'System`Real>' + str(self.to_sympy().n(dps(_prec))).encode('utf8'))

    def get_atom_name(self):
        return 'Real'


class MachineReal(Real):
    '''
    Machine precision real number.

    Stored internally as a python float.
    '''
    def __new__(cls, value):
        self = Number.__new__(cls)
        self.value = float(value)
        if math.isinf(self.value) or math.isnan(self.value):
            raise OverflowError
        return self

    def to_python(self, *args, **kwargs):
        return self.value

    def to_sympy(self):
        return sympy.Float(self.value)

    def to_mpmath(self):
        return mpmath.mpf(self.value)

    def round(self, d=None):
        return self

    def same(self, other):
        if isinstance(other, MachineReal):
            return self.value == other.value
        elif isinstance(other, PrecisionReal):
            return self.to_sympy() == other.value
        return False

    def is_machine_precision(self):
        return True

    def get_precision(self):
        return machine_precision

    def get_float_value(self, permit_complex=False):
        return self.value

    def make_boxes(self, form):
        from mathics.builtin.inout import number_form
        _number_form_options['_Form'] = form    # passed to _NumberFormat
        if form in ('System`InputForm', 'System`FullForm'):
            n = None
        else:
            n = 6
        return number_form(self, n, None, None, _number_form_options)

    def __getnewargs__(self):
        return (self.value,)

    def do_copy(self):
        return MachineReal(self.value)

    def __neg__(self):
        return MachineReal(-self.value)

    @property
    def is_zero(self):
        return self.value == 0.0


class PrecisionReal(Real):
    '''
    Arbitrary precision real number.

    Stored internally as a sympy.Float.
    '''
    def __new__(cls, value):
        self = Number.__new__(cls)
        self.value = sympy.Float(value)
        return self

    def to_python(self, *args, **kwargs):
        return float(self.value)

    def to_sympy(self):
        return self.value

    def to_mpmath(self):
        return mpmath.mpf(self.value)

    def round(self, d=None):
        if d is None:
            return MachineReal(float(self.value))
        else:
            d = min(dps(self.get_precision()), d)
            return PrecisionReal(self.value.n(d))

    def same(self, other):
        if isinstance(other, PrecisionReal):
            return self.value == other.value
        elif isinstance(other, MachineReal):
            return self.value == other.to_sympy()
        return False

    def get_precision(self):
        return self.value._prec + 1

    def make_boxes(self, form):
        from mathics.builtin.inout import number_form
        _number_form_options['_Form'] = form    # passed to _NumberFormat
        return number_form(self, dps(self.get_precision()), None, None, _number_form_options)

    def __getnewargs__(self):
        return (self.value,)

    def do_copy(self):
        return PrecisionReal(self.value)

    def __neg__(self):
        return PrecisionReal(-self.value)

    @property
    def is_zero(self):
        return self.value == 0.0


class Complex(Number):
    '''
    Complex wraps two real-valued Numbers.
    '''
    def __new__(cls, real, imag):
        self = super(Complex, cls).__new__(cls)
        if isinstance(real, Complex) or not isinstance(real, Number):
            raise ValueError("Argument 'real' must be a real number.")
        if isinstance(imag, Complex) or not isinstance(imag, Number):
            raise ValueError("Argument 'imag' must be a real number.")

        if imag.same(Integer(0)):
            return real

        if isinstance(real, MachineReal) and not isinstance(imag, MachineReal):
            imag = imag.round()
        if isinstance(imag, MachineReal) and not isinstance(real, MachineReal):
            real = real.round()

        self.real = real
        self.imag = imag
        return self

    def atom_to_boxes(self, f, evaluation):
        return self.format(evaluation, f.get_name())

    def __str__(self):
        return str(self.to_sympy())

    def to_sympy(self, **kwargs):
        return self.real.to_sympy() + sympy.I * self.imag.to_sympy()

    def to_python(self, *args, **kwargs):
        return complex(self.real.to_python(), self.imag.to_python())

    def to_mpmath(self):
        return mpmath.mpc(self.real.to_mpmath(), self.imag.to_mpmath())

    def do_format(self, evaluation, form):
        if form == 'System`FullForm':
            return Expression(Expression('HoldForm', Symbol('Complex')),
                              self.real, self.imag).do_format(evaluation, form)

        result = []
        if self.is_machine_precision() or not self.real.is_zero:
            result.append(self.real)
        if self.imag.same(Integer(1)):
            result.append(Symbol('I'))
        else:
            result.append(Expression('Times', self.imag, Symbol('I')))
        if len(result) == 1:
            result = result[0]
        else:
            result = Expression('Plus', *result)
        return Expression('HoldForm', result).do_format(evaluation, form)

    def default_format(self, evaluation, form):
        return 'Complex[%s, %s]' % (self.real.default_format(evaluation, form),
                                    self.imag.default_format(evaluation, form))

    def get_sort_key(self, pattern_sort=False):
        if pattern_sort:
            return super(Complex, self).get_sort_key(True)
        else:
            return [0, 0, self.real.get_sort_key()[2],
                    self.imag.get_sort_key()[2], 1]

    def same(self, other):
        return (isinstance(other, Complex) and self.real == other.real and
                self.imag == other.imag)

    def evaluate(self, evaluation):
        evaluation.check_stopped()
        return self

    def round(self, d=None):
        real = self.real.round(d)
        imag = self.imag.round(d)
        return Complex(real, imag)

    def is_machine_precision(self):
        if self.real.is_machine_precision() or self.imag.is_machine_precision():
            return True
        return False

    def get_float_value(self, permit_complex=False):
        if permit_complex:
            real = self.real.get_float_value()
            imag = self.imag.get_float_value()
            if real is not None and imag is not None:
                return complex(real, imag)
        else:
            return None

    def get_precision(self):
        real_prec = self.real.get_precision()
        imag_prec = self.imag.get_precision()
        if imag_prec is None or real_prec is None:
            return None
        return min(real_prec, imag_prec)

    def do_copy(self):
        return Complex(self.real.do_copy(), self.imag.do_copy())

    def __hash__(self):
        return hash(('Complex', self.real, self.imag))

    def user_hash(self, update):
        update(b'System`Complex>')
        update(self.real)
        update(self.imag)

    def __eq__(self, other):
        if isinstance(other, Complex):
            return self.real == other.real and self.imag == other.imag
        else:
            return self.get_sort_key() == other.get_sort_key()

    def __getnewargs__(self):
        return (self.real, self.imag)

    def __neg__(self):
        return Complex(-self.real, -self.imag)

    @property
    def is_zero(self):
        return self.real.is_zero and self.imag.is_zero


def encode_mathml(text):
    text = text.replace('&', '&amp;').replace('<', '&lt;').replace('>', '&gt;')
    text = text.replace('"', '&quot;').replace(' ', '&nbsp;')
    return text.replace('\n', '<mspace linebreak="newline" />')

TEX_REPLACE = {
    '{': r'\{',
    '}': r'\}',
    '_': r'\_',
    '$': r'\$',
    '%': r'\%',
    '#': r'\#',
    '&': r'\&',
    '\\': r'\backslash{}',
    '^': r'{}^{\wedge}',
    '~': r'\sim{}',
    '|': r'\vert{}',
}
TEX_TEXT_REPLACE = TEX_REPLACE.copy()
TEX_TEXT_REPLACE.update({
    '<': r'$<$',
    '>': r'$>$',
    '~': r'$\sim$',
    '|': r'$\vert$',
    '\\': r'$\backslash$',
    '^': r'${}^{\wedge}$',
})
TEX_REPLACE_RE = re.compile('([' + ''.join(
    [re.escape(c) for c in TEX_REPLACE]) + '])')


def encode_tex(text, in_text=False):
    def replace(match):
        c = match.group(1)
        repl = TEX_TEXT_REPLACE if in_text else TEX_REPLACE
        # return TEX_REPLACE[c]
        return repl.get(c, c)

    text = TEX_REPLACE_RE.sub(replace, text)
    text = text.replace('\n', '\\newline\n')
    return text

extra_operators = set((',', '(', ')', '[', ']', '{', '}',
                       '\u301a', '\u301b', '\u00d7', '\u2032',
                       '\u2032\u2032', ' ', '\u2062', '\u222b', '\u2146'))


class String(Atom):
    def __new__(cls, value):
        self = super(String, cls).__new__(cls)
        self.value = six.text_type(value)
        return self

    def __str__(self):
        return '"%s"' % self.value

    def boxes_to_text(self, show_string_characters=False, **options):
        value = self.value
        if (not show_string_characters and      # nopep8
            value.startswith('"') and value.endswith('"')):
            value = value[1:-1]
        return value

    def boxes_to_xml(self, show_string_characters=False, **options):
        from mathics.core.parser import is_symbol_name
        from mathics.builtin import builtins

        operators = set()
        for name, builtin in six.iteritems(builtins):
            operator = builtin.get_operator_display()
            if operator is not None:
                operators.add(operator)

        text = self.value

        def render(format, string):
            return format % encode_mathml(string)

        if text.startswith('"') and text.endswith('"'):
            if show_string_characters:
                return render('<ms>%s</ms>', text[1:-1])
            else:
                return render('<mtext>%s</mtext>', text[1:-1])
        elif text and ('0' <= text[0] <= '9' or text[0] == '.'):
            return render('<mn>%s</mn>', text)
        else:
            if text in operators or text in extra_operators:
                if text == '\u2146':
                    return render(
                        '<mo form="prefix" lspace="0.2em" rspace="0">%s</mo>', text)
                if text == '\u2062':
                    return render(
                        '<mo form="prefix" lspace="0" rspace="0.2em">%s</mo>', text)
                return render('<mo>%s</mo>', text)
            elif is_symbol_name(text):
                return render('<mi>%s</mi>', text)
            else:
                return render('<mtext>%s</mtext>', text)

    def boxes_to_tex(self, show_string_characters=False, **options):
        from mathics.builtin import builtins

        operators = set()
        for name, builtin in six.iteritems(builtins):
            operator = builtin.get_operator_display()
            if operator is not None:
                operators.add(operator)

        text = self.value

        def render(format, string, in_text=False):
            return format % encode_tex(string, in_text)

        if text.startswith('"') and text.endswith('"'):
            if show_string_characters:
                return render(r'\text{"%s"}', text[1:-1], in_text=True)
            else:
                return render(r'\text{%s}', text[1:-1], in_text=True)
        elif text and text[0] in '0123456789-.':
            return render('%s', text)
        else:
            if text == '\u2032':
                return "'"
            elif text == '\u2032\u2032':
                return "''"
            elif text == '\u2062':
                return ' '
            elif text == '\u221e':
                return r'\infty '
            elif text == '\u00d7':
                return r'\times '
            elif text in ('(', '[', '{'):
                return render(r'\left%s', text)
            elif text in (')', ']', '}'):
                return render(r'\right%s', text)
            elif text == '\u301a':
                return r'\left[\left['
            elif text == '\u301b':
                return r'\right]\right]'
            elif text == ',' or text == ', ':
                return text
            elif text == '\u222b':
                return r'\int'
            elif text == '\u2146':
                return r'\, d'
            elif text == '\u2211':
                return r'\sum'
            elif text == '\u220f':
                return r'\prod'
            elif len(text) > 1:
                return render(r'\text{%s}', text, in_text=True)
            else:
                return render('%s', text)

    def atom_to_boxes(self, f, evaluation):
        return String('"' + six.text_type(self.value) + '"')

    def do_copy(self):
        return String(self.value)

    def default_format(self, evaluation, form):
        value = self.value.replace('\\', '\\\\').replace('"', '\\"')
        return '"%s"' % value

    def get_sort_key(self, pattern_sort=False):
        if pattern_sort:
            return super(String, self).get_sort_key(True)
        else:
            return [0, 1, self.value, 0, 1]

    def same(self, other):
        return isinstance(other, String) and self.value == other.value

    def get_string_value(self):
        return self.value

    def to_sympy(self, **kwargs):
        return None

    def to_python(self, *args, **kwargs):
        return '"%s"' % self.value  # add quotes to distinguish from Symbols

    def __hash__(self):
        return hash(("String", self.value))

    def user_hash(self, update):
        # hashing a String is the one case where the user gets the untampered
        # hash value of the string's text. this corresponds to MMA behavior.
        update(self.value.encode('utf8'))

    def __getnewargs__(self):
        return (self.value,)


def get_default_value(name, evaluation, k=None, n=None):
    pos = []
    if k is not None:
        pos.append(k)
    if n is not None:
        pos.append(n)
    for pos_len in reversed(list(range(len(pos) + 1))):
        # Try patterns from specific to general
        defaultexpr = Expression('Default', Symbol(name),
                                 *[Integer(index) for index in pos[:pos_len]])
        result = evaluation.definitions.get_value(
            name, 'System`DefaultValues', defaultexpr, evaluation)
        if result is not None:
            if result.same(defaultexpr):
                result = result.evaluate(evaluation)
            return result
    return None


def print_parenthesizes(precedence, outer_precedence=None,
                        parenthesize_when_equal=False):
    return (outer_precedence is not None and (
        outer_precedence > precedence or (
            outer_precedence == precedence and parenthesize_when_equal)))


def _is_safe_head(head, cache, evaluation):
    if not isinstance(head, Symbol):
        return False

    head_name = head.get_name()

    if cache:
        r = cache.get(head_name)
        if r is not None:
            return r

    definitions = evaluation.definitions

    definition = definitions.get_definition(head_name, only_if_exists=True)
    if definition is None:
        r = True
    else:
        r = all(len(definition.get_values_list(x)) == 0 for x in ('up', 'sub', 'down', 'own'))

    if cache:
        cache[head_name] = r

    return r


_optimize_sequences = True  # more complex code, gains primarily for Partition[]


class Structure:
    # performance test case: x = Range[50000]; First[Timing[Partition[x, 15, 1]]]

    def __init__(self, head, orig, evaluation, cache=None):
        if isinstance(head, six.string_types):
            head = Symbol(head)
        self.head = head

        if isinstance(orig, (Expression, Structure)):
            token = orig._token
            if token is not None and not _is_safe_head(head, cache, evaluation):
                token = None
        elif isinstance(orig, (list, tuple)):
            if _is_safe_head(head, cache, evaluation):
                token = EvaluationToken.union([expr.get_token() for expr in orig], evaluation)
            else:
                token = None
        else:
            raise ValueError('expected Expression, Structure, tuple or list as orig param')

        self._token = token

    def __call__(self, leaves):  # create from leaves
        # IMPORTANT: caller guarantees that leaves originate from orig.leaves or its sub trees!

        expr = Expression(self.head)
        expr._leaves = tuple(leaves)
        expr._token = self._token
        return expr

    def filter(self, expr, cond):
        # IMPORTANT: caller guarantees that expr is from origins!
        return self([leaf for leaf in expr._leaves if cond(leaf)])

    def slice(self, expr, py_slice):
        # IMPORTANT: caller guarantees that expr is from origins!

        leaves = expr._leaves
        lower, upper, step = py_slice.indices(len(leaves))
        if step != 1:
            raise ValueError('Structure.slice only supports slice steps of 1')

        expr = self(leaves[lower:upper])

        if _optimize_sequences:
            seq = expr._sequences
            if seq:
                a = bisect_left(seq, lower)  # all(val >= i for val in seq[a:])
                b = bisect_left(seq, upper)  # all(val >= j for val in seq[b:])

                expr._sequences = tuple(x - lower for x in seq[a:b])  # (O2)
            elif seq is not None:
                expr._sequences = ()

        return expr
