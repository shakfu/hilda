#!/usr/bin/env python3
"""Mock OpenAI-compatible server for the minos ACP conformance suite.

Usage: acp-mock.py PORT

POST /v1/chat/completions answers by the last message:
  - a tool result: the text "done";
  - a user prompt containing "slow": "slow reply" after 5 s;
  - a user prompt containing "permission": a bash call to `echo hi`;
  - anything else: "hello", streamed as two SSE chunks.
"""

import json
import sys
import time
from http.server import BaseHTTPRequestHandler, ThreadingHTTPServer

USAGE = {"prompt_tokens": 10, "completion_tokens": 2, "cost": 0.001}


class Handler(BaseHTTPRequestHandler):
    def do_POST(self):
        body = json.loads(self.rfile.read(int(self.headers.get("Content-Length", 0))))
        last = body["messages"][-1]
        text = last.get("content") or ""
        if last["role"] == "tool":
            self.stream("done")
        elif "slow" in text:
            time.sleep(5)
            self.stream("slow reply")
        elif "permission" in text:
            call = {
                "id": "call_1",
                "type": "function",
                "function": {"name": "bash", "arguments": json.dumps({"command": "echo hi"})},
            }
            self.send_json({"choices": [{"message": {"role": "assistant", "content": None, "tool_calls": [call]}}], "usage": USAGE})
        else:
            self.stream("hello")

    def send_json(self, obj):
        data = json.dumps(obj).encode()
        self.send_response(200)
        self.send_header("Content-Type", "application/json")
        self.send_header("Content-Length", str(len(data)))
        self.end_headers()
        self.wfile.write(data)

    def stream(self, text):
        self.send_response(200)
        self.send_header("Content-Type", "text/event-stream")
        self.end_headers()
        half = len(text) // 2
        for piece in (text[:half], text[half:]):
            chunk = {"choices": [{"index": 0, "delta": {"content": piece}}]}
            self.wfile.write(b"data: " + json.dumps(chunk).encode() + b"\n\n")
            self.wfile.flush()
        self.wfile.write(b"data: " + json.dumps({"choices": [], "usage": USAGE}).encode() + b"\n\n")
        self.wfile.write(b"data: [DONE]\n\n")

    def log_message(self, *args):
        pass


if __name__ == "__main__":
    ThreadingHTTPServer(("127.0.0.1", int(sys.argv[1])), Handler).serve_forever()
