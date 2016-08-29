#!/usr/bin/env python
# -*- coding: utf-8 -*-

print('routing.py loaded!!')

from channels.routing import route
from mathics.web.websocket import ws_message

channel_routing = [
    route("websocket.receive", ws_message),
]
