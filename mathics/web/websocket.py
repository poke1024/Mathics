#!/usr/bin/env python
# -*- coding: utf-8 -*-

def ws_message(message):
    # ASGI WebSocket packet-received and send-packet message types
    # both have a "text" key for their textual data.

    print('ws_message received', message.content['text'])

    message.reply_channel.send({
        "text": message.content['text'],
    })
