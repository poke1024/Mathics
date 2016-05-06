#!/usr/bin/env python
# -*- coding: utf-8 -*-

from __future__ import unicode_literals
from __future__ import absolute_import

from mathics.core.expression import String, strip_context
from mathics import settings
from mathics.core.evaluation import Evaluation

from mathics.builtin.base import Builtin
from mathics.core.expression import Expression, Symbol, Integer, from_python

try:
    from ipykernel.kernelbase import Kernel
    _jupyter = True
except ImportError:
    _jupyter = False

try:
    from ipywidgets import (IntSlider, FloatSlider, ToggleButtons, Box, DOMWidget)
    _ipywidgets = True
except ImportError:
    # fallback to non-Manipulate-enabled build if we don't have ipywidgets installed.
    _ipywidgets = False


"""
A basic implementation of Manipulate[]. There is currently no support for Dynamic[] elements.
This implementation is basically a port from ipywidget.widgets.interaction for Mathics.
"""

def _interactive(interact_f, kwargs_widgets):
    # this is a modified version of interactive() in ipywidget.widgets.interaction

    container = Box(_dom_classes=['widget-interact'])
    container.children = [w for w in kwargs_widgets if isinstance(w, DOMWidget)]

    def call_f(name=None, old=None, new=None):
        kwargs = dict((widget._kwarg, widget.value) for widget in kwargs_widgets)
        try:
            interact_f(**kwargs)
        except Exception as e:
            container.log.warn("Exception in interact callback: %s", e, exc_info=True)

    for widget in kwargs_widgets:
        widget.on_trait_change(call_f, 'value')

    container.on_displayed(lambda _: call_f(None, None, None))

    return container


class IllegalWidgetArguments(Exception):
    def __init__(self, var):
        super(IllegalWidgetArguments, self).__init__()
        self.var = var


class ManipulateParameter(Builtin): # parses one Manipulate[] parameter spec, e.g. {x, 1, 2}, see _WidgetInstantiator
    context = 'System`Private`'

    rules = {
        # detect x and {x, default} and {x, default, label}.
        'System`Private`ManipulateParameter[{s_Symbol, r__}]':
            'System`Private`ManipulateParameter[{Symbol -> s, Label -> s}, {r}]',
        'System`Private`ManipulateParameter[{{s_Symbol, d_}, r__}]':
            'System`Private`ManipulateParameter[{Symbol -> s, Label -> s, Default -> d}, {r}]',
        'System`Private`ManipulateParameter[{{s_Symbol, d_, l_}, r__}]':
            'System`Private`ManipulateParameter[{Symbol -> i, Label -> l, Default -> d}, {r}]',

        # detect different kinds of widgets. on the use of the duplicate key "Default ->", see _WidgetInstantiator.add()
        'System`Private`ManipulateParameter[var_, {min_?RealNumberQ, max_?RealNumberQ}]':
            'Join[{Type -> "Continuous", Minimum -> min, Maximum -> max, Default -> min}, var]',
        'System`Private`ManipulateParameter[var_, {min_?RealNumberQ, max_?RealNumberQ, ds_?RealNumberQ}]':
            'Join[{Type -> "Discrete", Minimum -> min, Maximum -> max, Step -> ds, Default -> min}, var]',
        'System`Private`ManipulateParameter[var_, {opt_List}] /; Length[opt] > 0':
            'Join[{Type -> "Options", Options -> opt, Default -> Part[opt, 1]}, var]'
    }


def _manipulate_label(x):  # gets the label that is displayed for a symbol or name
    if isinstance(x, String):
        return x.get_string_value()
    elif isinstance(x, Symbol):
        return strip_context(x.get_name())
    else:
        return str(x)


class _WidgetInstantiator():
    # we do not want to have widget instances (like FloatSlider) get into the evaluation pipeline (e.g. via Expression
    # or Atom), since there might be all kinds of problems with serialization of these widget classes.  therefore, the
    # elegant recursive solution for parsing parameters (like in Table[]) is not feasible here; instead, we must create
    # and use the widgets in one "transaction" here, without holding them in expressions or atoms.

    def __init__(self):
        self._widgets = []  # the ipywidget widgets to control the manipulated variables
        self._parsers = {}  # lambdas to decode the widget values into Mathics expressions

    def add(self, expression, evaluation):
        expr = Expression('System`Private`ManipulateParameter', expression).evaluate(evaluation)
        if expr.get_head_name() != 'System`List':  # if everything was parsed ok, we get a List
            return False
        # convert the rules given us by ManipulateParameter[] into a dict. note: duplicate keys
        # will be overwritten, the latest one wins.
        kwargs = {'evaluation': evaluation}
        for rule in expr.leaves:
            if rule.get_head_name() != 'System`Rule' or len(rule.leaves) != 2:
                return False
            kwargs[strip_context(rule.leaves[0].to_python()).lower()] = rule.leaves[1]
        widget = kwargs['type'].get_string_value()
        del kwargs['type']
        getattr(self, '_add_%s_widget' % widget.lower())(**kwargs)  # create the widget
        return True

    def get_widgets(self):
        return self._widgets

    def build_callback(self, callback):
        parsers = self._parsers

        def new_callback(**kwargs):
            callback(**dict((name, parsers[name](value)) for (name, value) in kwargs.items()))

        return new_callback

    def _add_continuous_widget(self, symbol, label, default, minimum, maximum, evaluation):
        minval = minimum.to_python()
        maxval = maximum.to_python()
        if minval > maxval:
            raise IllegalWidgetArguments(symbol)
        else:
            defval = min(max(default.to_python(), minval), maxval)
            widget = FloatSlider(value=defval, min=minval, max=maxval)
            self._add_widget(widget, symbol.get_name(), lambda x: from_python(x), label)

    def _add_discrete_widget(self, symbol, label, default, minimum, maximum, step, evaluation):
        imin_value = minimum.to_python()
        imax_value = maximum.to_python()
        di_value = step.to_python()
        if imin_value > imax_value or di_value <= 0 or di_value > (imax_value - imin_value):
            raise IllegalWidgetArguments(symbol)
        else:
            idefault_value = min(max(default.to_python(), imin_value), imax_value)
            if all(isinstance(x, Integer) for x in [minimum, maximum, default, step]):
                widget = IntSlider(value=idefault_value, min=imin_value, max=imax_value,
                                   step=di_value)
            else:
                widget = FloatSlider(value=idefault_value, min=imin_value, max=imax_value,
                                     step=di_value)
            self._add_widget(widget, symbol.get_name(), lambda x: from_python(x), label)

    def _add_options_widget(self, symbol, options, default, label, evaluation):
        formatted_options = []
        for i, option in enumerate(options.leaves):
            data = evaluation.format_all_outputs(option)
            formatted_options.append((data['text/plain'], i))

        default_index = 0
        for i, option in enumerate(options.leaves):
            if option.same(default):
                default_index = i

        widget = ToggleButtons(options=formatted_options, value=default_index)
        self._add_widget(widget, symbol.get_name(), lambda j: options.leaves[j], label)

    def _add_widget(self, widget, name, parse, label):
        if not widget.description:
            widget.description = _manipulate_label(label)
        widget._kwarg = name  # see _interactive() above
        self._parsers[name] = parse
        self._widgets.append(widget)


class ListAnimate(Builtin):
    rules = {
        'ListAnimate[l_List]': 'Manipulate[Part[l, i], {i, 1, Length[l], 1}]'
    }


class Manipulate(Builtin):
    """
    <dl>
    <dt>'Manipulate[$expr1$, $arg1$, ...]'
        <dd>allows you to interactively compute and display an expression with different argument values.
    </dl>

    >> Manipulate[N[Sin[y]], {y, 1, 20, 2}] //FullForm
    Null

    >> Manipulate[i^3, {i, {2, x^4, a}}] //FullForm
    Null

    >> Manipulate[x^y, {x, 1, 20}, {y, 1, 3}] //FullForm
    Null

    """

    attributes = ('HoldAll',)  # we'll call ReleaseHold at the time of evaluation below

    messages = {
        'jupyter': 'Manipulate[] only works inside a Jupyter notebook.',
        'noipywidget': 'Manipulate[] needs the ipywidgets module to work.',
        'widgetargs': 'Illegal variable range or step parameters for ``.'
    }

    def apply(self, expr, args, evaluation):
        'Manipulate[expr_, args__]'
        if (not _jupyter) or (not Kernel.initialized()) or (Kernel.instance() is None):
            evaluation.error('Manipulate', 'jupyter')
            return Symbol('Null')

        if not _ipywidgets:
            evaluation.error('Manipulate', 'noipywidget')
            return Symbol('Null')

        instantiator = _WidgetInstantiator()  # knows about the arguments and their widgets

        for arg in args.get_sequence():
            try:
                if not instantiator.add(arg, evaluation):  # not a valid argument pattern?
                    return Expression(self.get_name(), expr, *args.get_sequence())  # identity
            except IllegalWidgetArguments as e:
                evaluation.message('Manipulate', 'widgetargs', strip_context(str(e.var)))

        clear_output_callback = evaluation.clear_output_callback
        display_data_callback = evaluation.display_data_callback  # for pushing updates

        def callback(**kwargs):
            clear_output_callback(wait=True)

            line_no = evaluation.definitions.get_line_no()

            new_evaluation = Evaluation(evaluation.definitions, result_callback=display_data_callback,
                                        out_callback=evaluation.out_callback)

            vars = [Expression('Set', Symbol(name), value) for name, value in kwargs.items()]
            evaluatable = Expression('ReleaseHold', Expression('Module', Expression('List', *vars), expr))
            new_evaluation.evaluate([evaluatable], timeout=settings.TIMEOUT)

            evaluation.definitions.set_line_no(line_no)  # do not increment line_no for manipulate computations

        widgets = instantiator.get_widgets()
        if len(widgets) > 0:
            box = _interactive(instantiator.build_callback(callback), widgets)  # create the widget

            # the following code is a boiled down version from IPython.core.formatters.IPythonDisplayFormatter.
            # note that '_ipython_display_' is a magic constant defined in print_method of IPythonDisplayFormatter.

            method = getattr(box, '_ipython_display_')
            if method is not None:
                method()  # make the widget appear on the Jupyter notebook

        return Symbol('Null')  # the interactive output is pushed via kernel.display_data_callback (see above)
