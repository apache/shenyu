#!/usr/bin/env python3
# Licensed to the Apache Software Foundation (ASF) under one or more
# contributor license agreements.  See the NOTICE file distributed with
# this work for additional information regarding copyright ownership.
# The ASF licenses this file to You under the Apache License, Version 2.0
# (the "License"); you may not use this file except in compliance with
# the License.  You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
"""Generate the README architecture diagram as a standalone SVG."""

from pathlib import Path
import sys
from xml.etree import ElementTree as ET

W, H = 1680, 1008
TOP = 112
NS = "http://www.w3.org/2000/svg"
ET.register_namespace("", NS)
SVG = ET.Element("{%s}svg" % NS, nsmap={None: NS})
SVG.set("viewBox", f"0 {TOP} {W} {H}")
SVG.set("width", str(W))
SVG.set("height", str(H))
SVG.set("role", "img")
SVG.set("aria-label", "Apache ShenYu architecture: control plane configuration sync, gateway request path, upstream services, and observability")
SVG.append(ET.Comment("Licensed to the Apache Software Foundation (ASF) under one or more contributor license agreements. See the NOTICE file distributed with this work for additional information regarding copyright ownership. The ASF licenses this file to You under the Apache License, Version 2.0. http://www.apache.org/licenses/LICENSE-2.0"))

def el(tag, **attrs):
    return ET.SubElement(SVG, "{%s}%s" % (NS, tag), {k.replace("_", "-"): str(v) for k, v in attrs.items()})

def rect(x, y, w, h, fill, stroke="none", radius=0, sw=1, **kw):
    return el("rect", x=x, y=y, width=w, height=h, rx=radius, fill=fill, stroke=stroke, stroke_width=sw, **kw)

def line(x1, y1, x2, y2, color, width=2, dash=None, marker=None):
    a = dict(x1=x1, y1=y1, x2=x2, y2=y2, stroke=color, stroke_width=width, stroke_linecap="round")
    if dash: a["stroke_dasharray"] = dash
    if marker: a["marker_end"] = f"url(#{marker})"
    return el("line", **a)

def path(d, color, width=2, dash=None, marker=None, fill="none"):
    a = dict(d=d, stroke=color, stroke_width=width, stroke_linecap="round", stroke_linejoin="round", fill=fill)
    if dash: a["stroke_dasharray"] = dash
    if marker: a["marker_end"] = f"url(#{marker})"
    return el("path", **a)

def txt(x, y, s, size=20, color="#12213A", weight=400, anchor="start", spacing=None):
    a = dict(x=x, y=y, fill=color, font_size=size, font_weight=weight, text_anchor=anchor,
             font_family="Inter, -apple-system, BlinkMacSystemFont, Segoe UI, Arial, sans-serif")
    if spacing: a["letter_spacing"] = spacing
    t = el("text", **a)
    t.text = s
    return t

def pill(x, y, w, label, fill, color, stroke="none", h=36, size=17):
    rect(x, y, w, h, fill, stroke, h / 2)
    txt(x + w/2, y + h/2 + size * .34, label, size, color, 650, "middle")

def dot(x, y, r, color):
    el("circle", cx=x, cy=y, r=r, fill=color)

def arrow_marker(name, color):
    defs = SVG.find("{%s}defs" % NS)
    marker = ET.SubElement(defs, "{%s}marker" % NS, id=name, viewBox="0 0 10 10",
                           refX="9", refY="5", markerWidth="8", markerHeight="8", orient="auto-start-reverse")
    ET.SubElement(marker, "{%s}path" % NS, d="M 0 1 L 9 5 L 0 9 z", fill=color)

ET.SubElement(SVG, "{%s}defs" % NS)
arrow_marker("flow", "#E56A2F")
arrow_marker("sync", "#2874CE")
arrow_marker("register", "#128279")
arrow_marker("telemetry", "#7B5BC7")

# The README already has a project header; start the diagram at the control plane.
rect(0, TOP, W, H, "#F5F7FB")

# Control plane panel.
rect(50, 140, 1580, 268, "#EEF5FF", "#C9DDF8", 22, 2)
pill(78, 157, 160, "01  CONTROL", "#D9E9FF", "#1D5CA5", h=39, size=17)
txt(260, 184, "Admin owns configuration; sync updates gateway replicas", 22, "#274366", 550)

# Dashboard and client registration sources.
rect(78, 219, 230, 68, "#FFFFFF", "#B9CBE1", 13, 2)
rect(96, 239, 26, 22, "none", "#3673B8", 3, 2)
line(99, 245, 119, 245, "#3673B8", 2)
txt(139, 260, "Dashboard / API", 19, "#173556", 650)

rect(78, 310, 230, 68, "#FFFFFF", "#A7D6D0", 13, 2)
dot(108, 344, 12, "#D7F1ED")
txt(108, 350, "S", 15, "#0B766F", 750, "middle")
txt(139, 350, "Service SDK", 19, "#174C49", 650)

# Admin and its database are an explicit bounded context.
rect(370, 218, 365, 160, "#FFFFFF", "#8FB9EB", 16, 2)
rect(370, 218, 365, 48, "#DCEBFF", radius=16)
rect(370, 249, 365, 17, "#DCEBFF")
txt(390, 250, "ShenYu Admin", 22, "#174A84", 750)
txt(392, 294, "Config API · users · registration", 18, "#334E6C", 500)
rect(392, 313, 320, 48, "#F7FAFE", "#CDDDEF", 9, 1.5)
el("ellipse", cx="417", cy="328", rx="11", ry="4", fill="#D5E3F4", stroke="#5080BB", stroke_width="1.5")
path("M406 328 v15 c0 6 22 6 22 0 v-15", "#5080BB", 1.5)
txt(443, 343, "Database · persisted config", 17, "#36516F", 600)

# Sync channel.
rect(812, 231, 483, 135, "#FFFFFF", "#8FB9EB", 16, 2)
txt(838, 264, "Data sync", 22, "#174A84", 750)
txt(838, 291, "Admin listener → gateway subscriber", 17, "#45617E", 500)
pill(838, 309, 173, "WebSocket · default", "#E2F0FF", "#1F64A7", h=35, size=16)
pill(1022, 309, 246, "HTTP · ZK · Nacos · etc.", "#F0F5FB", "#45617E", h=35, size=16)

# Config payload callout.
rect(1374, 231, 229, 135, "#FFFFFF", "#C5D8ED", 16, 2)
txt(1394, 264, "Synced data", 20, "#2D506F", 700)
txt(1394, 293, "Plugin · Selector · Rule", 15.5, "#45617E", 500)
txt(1394, 319, "Metadata · Auth", 16, "#45617E", 500)
txt(1394, 345, "Upstream · AI keys", 16, "#45617E", 500)
line(308, 253, 365, 253, "#2874CE", 3, marker="sync")
line(308, 344, 365, 344, "#128279", 3, dash="7 6", marker="register")
line(735, 298, 805, 298, "#2874CE", 3, marker="sync")
line(1295, 298, 1367, 298, "#2874CE", 3, marker="sync")
txt(335, 237, "edit", 14, "#386DA9", 600, "middle")
txt(337, 332, "register", 14, "#0B766F", 600, "middle")

# Data plane panel and left edge.
rect(50, 438, 1580, 568, "#FFF8F2", "#F3D5BF", 22, 2)
pill(78, 455, 132, "02  DATA", "#FFE5D3", "#A8491C", h=39, size=17)
txt(230, 482, "HTTP request path · each gateway replica runs its own chain and cache", 22, "#75482F", 550)

rect(78, 702, 124, 105, "#FFFFFF", "#EAC4A9", 16, 2)
rect(111, 726, 58, 39, "none", "#CF6E39", 5, 2.5)
line(137, 767, 143, 767, "#CF6E39", 2.5)
line(127, 774, 154, 774, "#CF6E39", 2.5)
txt(140, 796, "Clients", 19, "#7D3D20", 700, "middle")

rect(258, 702, 150, 105, "#FFFFFF", "#EAC4A9", 16, 2)
path("M 315 745 l 19 -16 19 16 -19 17 z", "#CF6E39", 2, fill="#FFF0E3")
line(334, 761, 334, 774, "#CF6E39", 2)
txt(333, 796, "Proxy / LB", 19, "#7D3D20", 700, "middle")

# Gateway cluster: slight stacked offset conveys replicas without drawing a fake shared cache.
rect(462, 523, 700, 463, "#EAD7C9", "#EBC4A9", 18, 1.5)
rect(452, 513, 700, 463, "#F6E9DD", "#E7C3A7", 18, 1.5)
rect(442, 503, 700, 463, "#FFFFFF", "#E6B58F", 18, 2.5)
rect(442, 503, 700, 57, "#FFE3CF", radius=18)
rect(442, 542, 700, 18, "#FFE3CF")
txt(466, 540, "ShenYu gateway cluster", 23, "#96431F", 750)
pill(765, 515, 157, "N replicas", "#FFFFFF", "#A65027", "#E7B48F", h=33, size=16)

# TCP and MQTT use protocol bootstrap servers alongside the WebFlux request path.
rect(474, 581, 180, 67, "#F0FAF8", "#9CD5CD", 12, 2)
txt(492, 610, "TCP / MQTT", 18, "#15776F", 700)
txt(492, 638, "dedicated listeners", 15, "#3B7772", 500)

# Each replica's local config cache, fed from admin sync.
rect(718, 581, 398, 67, "#EFF6FF", "#9ABFE8", 12, 2)
txt(741, 610, "Local config cache", 19, "#225B98", 700)
txt(741, 638, "Plugin · Selector · Rule · Metadata", 16, "#3D648C", 500)
path("M 1054 366 V 578", "#2874CE", 3, marker="sync")
rect(1110, 425, 180, 31, "#F5F7FB", radius=9)
txt(1200, 446, "config change", 15, "#2366AB", 650, "middle")

# Request handler.
rect(474, 694, 180, 126, "#FFF7F0", "#DCA77F", 13, 2)
txt(493, 733, "Spring WebFlux", 18, "#8D441F", 700)
txt(493, 762, "ShenyuWebHandler", 15, "#75482F", 600)
txt(493, 790, "entry point", 16, "#A26C4E", 450)

# Plugin chain; category placement is explicitly examples, not an implied fixed order.
rect(718, 698, 398, 259, "#FFFFFF", "#E3A77A", 14, 2)
txt(740, 726, "Ordered ShenyuPlugin chain", 20, "#913F1C", 750)
txt(740, 751, "Configured order · dynamic plugin updates", 15, "#8D624D", 450)
pill(740, 763, 172, "Security / traffic", "#FFF0E3", "#A94F22", h=31, size=14.5)
pill(923, 763, 169, "Cache / resilience", "#E9F4F1", "#33756B", h=31, size=14.5)
pill(740, 800, 172, "Transform / AI / MCP", "#EAF2FF", "#3465A3", h=31, size=14)
pill(923, 800, 169, "Metrics / logging", "#EFE8FF", "#6846A3", h=31, size=14.5)
rect(740, 838, 352, 65, "#FFF5EC", "#F0C9AB", 9, 1.5)
txt(755, 858, "Proxy / RPC plugins", 16, "#A84C20", 700)
txt(755, 879, "Divide · Dubbo · SOFA · gRPC · Tars", 15, "#714A36", 500)
txt(755, 898, "WebSocket · RPC context / transform", 14.5, "#714A36", 500)
rect(740, 910, 352, 30, "#F5F7FB", radius=8)
txt(916, 930, "Selector / Rule: parameter + predicate matching", 14.5, "#3F536D", 550, "middle")
path("M 917 649 V 694", "#2874CE", 2.5, marker="sync")

# External request flow. Keep control and traffic arrows separate.
line(202, 755, 252, 755, "#E56A2F", 3.5, marker="flow")
line(408, 755, 468, 755, "#E56A2F", 3.5, marker="flow")
line(654, 755, 712, 755, "#E56A2F", 3.5, marker="flow")
line(1116, 755, 1194, 755, "#E56A2F", 3.5, marker="flow")
txt(434, 739, "HTTP", 15, "#B1552B", 650, "middle")

# Service landscape (actual protocol families in this checkout).
rect(1200, 545, 402, 348, "#FFFFFF", "#DFB18F", 16, 2)
rect(1200, 545, 402, 56, "#FFE8D8", radius=16)
rect(1200, 583, 402, 18, "#FFE8D8")
txt(1224, 582, "Upstream services", 22, "#914624", 750)
txt(1224, 629, "HTTP / Spring Cloud", 19, "#6B432F", 650)
line(1224, 647, 1578, 647, "#EDE2D8", 1.5)
txt(1224, 681, "RPC", 16, "#A66342", 750)
txt(1224, 708, "Dubbo · SOFA · gRPC · Tars", 18, "#6B432F", 550)
line(1224, 729, 1578, 729, "#EDE2D8", 1.5)
txt(1224, 761, "Streaming", 16, "#A66342", 750)
txt(1224, 788, "WebSocket services", 18, "#6B432F", 550)
rect(1224, 817, 354, 48, "#FFF8F2", radius=8)
txt(1401, 846, "Backend apps own their APIs", 16, "#8D624D", 500, "middle")

# Observability output is sourced from the gateway's plugins/agent hooks.
rect(1200, 912, 402, 70, "#F4EFFD", "#CBB8EB", 13, 2)
txt(1224, 941, "Observability sinks", 19, "#6746A0", 700)
txt(1224, 967, "Metrics · logs · trace IDs", 17, "#70578D", 500)
path("M 1116 884 H 1165 Q 1175 884 1175 894 V 947 H 1193", "#7B5BC7", 2.5, dash="6 5", marker="telemetry")

# Deployment artifact is called out without pretending it is in the request chain.
rect(78, 873, 330, 104, "#FFFFFF", "#D4C7BA", 13, 1.5)
pill(96, 889, 118, "DEPLOY", "#F1EBE5", "#71594A", h=30, size=15)
txt(96, 945, "shenyu-bootstrap image", 18, "#654D3F", 650)
txt(96, 966, "Docker / Kubernetes replicas", 14, "#917767", 450)

# Footer legend and concise architectural invariant.
line(62, 1040, 1618, 1040, "#DCE4ED", 1.5)
txt(68, 1077, "Flows", 17, "#51637A", 700)
line(137, 1071, 185, 1071, "#E56A2F", 3, marker="flow")
txt(199, 1077, "request", 17, "#51637A", 500)
line(337, 1071, 385, 1071, "#2874CE", 3, marker="sync")
txt(399, 1077, "config sync", 17, "#51637A", 500)
line(565, 1071, 613, 1071, "#128279", 3, dash="7 6", marker="register")
txt(627, 1077, "registration", 17, "#51637A", 500)
line(817, 1071, 865, 1071, "#7B5BC7", 3, dash="6 5", marker="telemetry")
txt(879, 1077, "telemetry", 17, "#51637A", 500)
txt(1599, 1077, "Each replica reads its own local cache", 17, "#51637A", 550, "end")

output = sys.argv[1] if len(sys.argv) > 1 else str(Path(__file__).with_name("shenyu-architecture.svg"))
ET.indent(SVG, space="  ")
ET.ElementTree(SVG).write(output, encoding="utf-8", xml_declaration=True)
print(output)
