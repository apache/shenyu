#  -*- coding: utf-8 -*-
"""One off: fix the checkstyle violations of the pii module."""
import io
import re

BASE = "shenyu-plugin/shenyu-plugin-ai/shenyu-plugin-ai-pii/src/main/java/org/apache/shenyu/plugin/ai/pii/"

FILES = [
    BASE + "detector/BuiltinPiiDetectors.java",
    BASE + "detector/PiiChecksum.java",
    BASE + "PiiAction.java",
    BASE + "PiiResponseDecorator.java",
    BASE + "profile/OpenAiChatProfile.java",
    BASE + "profile/OpenAiResponsesProfile.java",
    BASE + "profile/PiiProtocolProfiles.java",
    BASE + "profile/PiiTextSlot.java",
    BASE + "handler/PiiDetectionRule.java",
    BASE + "handler/PiiDetectionPluginDataHandler.java",
    BASE + "PiiDetectionPlugin.java",
    BASE + "PiiScanner.java",
    BASE + "PiiMatch.java",
    BASE + "audit/PiiAuditRecord.java",
    BASE + "audit/PiiAuditSink.java",
    BASE + "audit/LoggingPiiAuditSink.java",
    BASE + "detector/PiiDetector.java",
    BASE + "detector/RegexPiiDetector.java",
    BASE + "detector/DeadlineCharSequence.java",
    BASE + "detector/PiiDetectorTimeoutException.java",
]

NULL_EQ = re.compile(r'([A-Za-z][A-Za-z0-9_.]*|\))\s*==\s*null')
NULL_NE = re.compile(r'([A-Za-z][A-Za-z0-9_.]*|\))\s*!=\s*null')

for path in FILES:
    with io.open(path, "r", encoding="utf-8", newline="") as f:
        src = f.read()

    # rename the checksum method to satisfy the method naming rule
    src = src.replace("isValidMod11_2", "isValidMod11")
    # drop the unused imports
    src = src.replace("import org.springframework.http.server.reactive.ServerHttpResponse;\r\n", "")
    src = src.replace("import org.springframework.http.server.reactive.ServerHttpResponse;\n", "")
    src = src.replace("import com.google.gson.JsonArray;\r\n", "")
    src = src.replace("import com.google.gson.JsonArray;\n", "")

    # rewrite null comparisons only on lines that are not javadoc or comments
    out = []
    for line in src.splitlines(keepends=True):
        stripped = line.lstrip()
        if stripped.startswith("*") or stripped.startswith("//") or stripped.startswith("/*"):
            out.append(line)
            continue
        newline = NULL_EQ.sub(r"Objects.isNull(\1)", line)
        newline = NULL_NE.sub(r"Objects.nonNull(\1)", newline)
        # Objects.requireNonNull / Objects.isNull already present keep their style
        newline = newline.replace("Objects.isNull(Objects.requireNonNull", "Objects.requireNonNull")
        out.append(newline)
    src = "".join(out)

    # ensure java.util.Objects is imported when Objects.* is now used
    if "Objects." in src and "import java.util.Objects;" not in src:
        lines = src.splitlines(keepends=True)
        last_import = max(i for i, l in enumerate(lines) if l.startswith("import "))
        # keep import order: insert right after the last java.util import if any, else after last import
        insert_at = last_import + 1
        for i, l in enumerate(lines):
            if l.startswith("import java.util."):
                insert_at = i + 1
        lines.insert(insert_at, "import java.util.Objects;\n")
        src = "".join(lines)

    with io.open(path, "w", encoding="utf-8", newline="") as f:
        f.write(src)
    print("fixed", path)
print("done")
