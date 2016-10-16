#!/usr/bin/env python
# -*- coding: utf-8 -*-

from __future__ import unicode_literals
from __future__ import absolute_import

from django.db import models
from django.contrib.auth.models import User
from django.db.models.signals import pre_delete
from django.contrib.sessions.models import Session

from mathics.core.definitions import Definitions
from mathics.core.evaluation import Evaluation, Output


class WebOutput(Output):
    pass

_evaluations = {}


def get_session_evaluation(session):
    evaluation = _evaluations.get(session.session_key)
    if evaluation is None:
        from mathics.server import layout_engine
        definitions = Definitions(add_builtin=True)
        evaluation = Evaluation(
            definitions, format='xml', output=WebOutput(layout_engine))
        _evaluations[session.session_key] = evaluation
    return evaluation


def end_session_evaluation(sender, **kwargs):
    session_key = kwargs.get('instance').session_key
    del _evaluations[session_key]

pre_delete.connect(end_session_evaluation, sender=Session)


class Query(models.Model):
    time = models.DateTimeField(auto_now_add=True)
    query = models.TextField()
    result = models.TextField(null=True)
    timeout = models.BooleanField()
    out = models.TextField()
    error = models.BooleanField()

    remote_user = models.CharField(max_length=255, null=True)
    remote_addr = models.TextField(null=True)
    remote_host = models.TextField(null=True)
    browser = models.TextField(null=True)
    meta = models.TextField(null=True)

    log = models.TextField(null=True)


class Worksheet(models.Model):
    user = models.ForeignKey(User, related_name='worksheets', null=True)
    name = models.CharField(max_length=30)
    content = models.TextField()

    class Meta:
        unique_together = (('user', 'name'),)
