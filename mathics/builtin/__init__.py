#!/usr/bin/env python
# -*- coding: utf-8 -*-

from __future__ import unicode_literals
from __future__ import absolute_import

import importlib
from contextlib import contextmanager
from demandimport import _demandmod as lazy_module

module_names = [
    'algebra', 'arithmetic', 'assignment', 'attributes', 'calculus', 'combinatorial', 'compilation',
    'comparison', 'control', 'datentime', 'diffeqns', 'evaluation', 'exptrig', 'functional',
    'graphics', 'graphics3d', 'graphs', 'image', 'inout', 'integer', 'linalg', 'lists', 'logic', 'manipulate', 'natlang',
    'numbertheory', 'numeric', 'options', 'patterns', 'plot', 'physchemdata', 'randomnumbers', 'recurrence',
    'specialfunctions', 'scoping', 'strings', 'structure', 'system', 'tensors', 'xmlformat', 'htmlformat']

from mathics.builtin.base import (
    Builtin, SympyObject, BoxConstruct, Operator, PatternObject)

from mathics.settings import ENABLE_FILES_MODULE, BENCHMARK_STARTUP

if ENABLE_FILES_MODULE:
    module_names += ['files', 'importexport']

builtins = []
builtins_by_module = {}

if BENCHMARK_STARTUP:
    class Loader:
        def __init__(self, section):
            self._benchmarks = []
            self._section = section

        def __enter__(self):
            @contextmanager
            def load(name):
                from time import time
                t0 = time()
                yield
                t1 = time()
                self._benchmarks.append((name, t1 - t0))
            return load

        def __exit__(self, type, value, tb):
            duration = sum(map(lambda rec: rec[1], self._benchmarks))
            print('%s took %.1f s:' % (self._section, duration))
            self._benchmarks.sort(key=lambda rec: rec[1], reverse=True)
            for name, dt in self._benchmarks[:10]:
                print('    %s %f' % (name, dt))
            print('    ...')
            print()
else:
    @contextmanager
    def _load(name):
        yield

    class Loader:
        def __init__(self, section):
            pass

        def __enter__(self):
            return _load

        def __exit__(self, type, value, tb):
            pass


def is_builtin(var):
    if var == Builtin:
        return True
    if hasattr(var, '__bases__'):
        return any(is_builtin(base) for base in var.__bases__)
    return False


def load_module(name):
    module = importlib.import_module("mathics.builtin.%s" % name)

    builtins_by_module[module.__name__] = []
    vars = dir(module)
    for name in vars:
        if name.startswith('_'):
            continue

        var = getattr(module, name)
        if isinstance(var, lazy_module):
            continue

        if (hasattr(var, '__module__') and
                var.__module__.startswith('mathics.builtin.') and
                    var.__module__ != 'mathics.builtin.base' and
                is_builtin(var) and
                    var.__module__ == module.__name__):  # nopep8

            instance = var(expression=False)

            if isinstance(instance, Builtin):
                builtins.append((instance.get_name(), instance))
                builtins_by_module[module.__name__].append(instance)

    return module


def load_modules():
    with Loader('import') as loader:
        for name in module_names:
            with loader(name):
                yield load_module(name)

modules = list(load_modules())

# builtins = dict(builtins)

mathics_to_sympy = {}
sympy_to_mathics = {}

box_constructs = {}
pattern_objects = {}
builtins_precedence = {}


def add_builtins(new_builtins):
    for var_name, builtin in new_builtins:
        name = builtin.get_name()
        if isinstance(builtin, SympyObject):
            mathics_to_sympy[name] = builtin
            for sympy_name in builtin.get_sympy_names():
                sympy_to_mathics[sympy_name] = builtin
        if isinstance(builtin, BoxConstruct):
            box_constructs[name] = builtin
        if isinstance(builtin, Operator):
            builtins_precedence[name] = builtin.precedence
        if isinstance(builtin, PatternObject):
            pattern_objects[name] = builtin.__class__
    builtins.update(dict(new_builtins))

new_builtins = builtins
builtins = {}
add_builtins(new_builtins)


def get_module_doc(module):
    doc = module.__doc__
    if doc is not None:
        doc = doc.strip()
    if doc:
        title = doc.splitlines()[0]
        text = '\n'.join(doc.splitlines()[1:])
    else:
        title = module.__name__
        for prefix in ('mathics.builtin.', 'mathics.optional.'):
            if title.startswith(prefix):
                title = title[len(prefix):]
        title = title.capitalize()
        text = ''
    return title, text


def contribute(definitions):
    with Loader('contribute') as loader:
        # let MakeBoxes contribute first
        builtins['System`MakeBoxes'].contribute(definitions)
        for name, item in builtins.items():
            if name != 'System`MakeBoxes':
                with loader(name):
                    item.contribute(definitions)

    from mathics.core.expression import ensure_context
    from mathics.core.parser import all_operator_names
    from mathics.core.definitions import Definition

    # All builtins are loaded. Create dummy builtin definitions for
    # any remaining operators that don't have them. This allows
    # operators like \[Cup] to behave correctly.
    for operator in all_operator_names:
        if not definitions.have_definition(ensure_context(operator)):
            op = ensure_context(operator)
            definitions.builtin[op] = Definition(name=op)
