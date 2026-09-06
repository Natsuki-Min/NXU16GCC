#!/usr/bin/env python3
"""Run the linked NXU16 libc test in CasioEmu through its MCP server."""

import argparse
import json
from pathlib import Path
import re
import subprocess
import sys
import time
from urllib import error, request


OPENER = request.build_opener(request.ProxyHandler({}))


def http_json(url, payload=None, headers=None, timeout=3):
    body = None if payload is None else json.dumps(payload).encode("utf-8")
    req = request.Request(url, data=body, headers=headers or {})
    if body is not None:
        req.method = "POST"
        req.add_header("Content-Type", "application/json")
    with OPENER.open(req, timeout=timeout) as response:
        return json.loads(response.read().decode("utf-8")), response.headers


def symbol_address(objdump, image, name):
    output = subprocess.check_output([str(objdump), "-t", str(image)], text=True)
    for line in output.splitlines():
        if re.search(rf"\b{re.escape(name)}$", line):
            return int(line.split()[0], 16)
    raise RuntimeError(f"symbol is missing from {image}: {name}")


def main():
    runtime = Path(__file__).resolve().parent.parent
    workspace = runtime.parent.parent
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--elf", type=Path, default=runtime / "build/runtime-test.elf")
    parser.add_argument("--binary", type=Path, default=runtime / "build/runtime-test.bin")
    parser.add_argument(
        "--release-directory",
        type=Path,
        default=workspace / "CasioEmuMsvc/build-linux-mcp/CasioEmuMsvc",
    )
    parser.add_argument("--model", default="../../../fx991cnxVirtual")
    parser.add_argument("--port", type=int, default=3001)
    parser.add_argument(
        "--trace-symbol",
        action="append",
        default=[],
        help="stop once at this symbol and report it before continuing",
    )
    parser.add_argument(
        "--trace-address",
        action="append",
        default=[],
        metavar="NAME=ADDRESS",
        help="stop once at a numeric address and report it before continuing",
    )
    parser.add_argument(
        "--trace-memory",
        action="append",
        default=[],
        metavar="ADDRESS:SIZE",
        help="dump a memory range whenever a trace breakpoint is reached",
    )
    parser.add_argument(
        "--trace-repeat-address",
        action="append",
        default=[],
        metavar="NAME=ADDRESS:COUNT",
        help="report the first COUNT executions of a numeric address",
    )
    args = parser.parse_args()

    release = args.release_directory.resolve()
    executable = release / "CasioEmuMsvc"
    plugin = release / "CasioEmuMsvc.Plugin.McpPlugin.so"
    objdump = workspace / "NXU16GNUbinutils/build-codex/binutils/objdump"
    for required in (executable, plugin, objdump, args.elf, args.binary):
        if not required.is_file():
            parser.error(f"required file not found: {required}")

    entry = symbol_address(objdump, args.elf, "_start")
    exit_loop = symbol_address(objdump, args.elf, "__nxu16_exit_loop")
    exit_code = symbol_address(objdump, args.elf, "nxu16_exit_code")
    console = symbol_address(objdump, args.elf, "nxu16_console_buffer")
    console_length = symbol_address(objdump, args.elf, "nxu16_console_length")
    image = list(args.binary.read_bytes())
    trace_addresses = {
        symbol_address(objdump, args.elf, name): name for name in args.trace_symbol
    }
    for trace_address in args.trace_address:
        try:
            name, address = trace_address.rsplit("=", 1)
            trace_addresses[int(address, 0)] = name
        except ValueError:
            parser.error(f"invalid --trace-address value: {trace_address!r}")
    trace_memory = []
    for memory_range in args.trace_memory:
        try:
            address, size = memory_range.rsplit(":", 1)
            trace_memory.append((int(address, 0), int(size, 0)))
        except ValueError:
            parser.error(f"invalid --trace-memory value: {memory_range!r}")
    trace_counts = {address: 1 for address in trace_addresses}
    for repeated_trace in args.trace_repeat_address:
        try:
            name, value = repeated_trace.rsplit("=", 1)
            address, count = value.rsplit(":", 1)
            numeric_address = int(address, 0)
            numeric_count = int(count, 0)
            if numeric_count < 1:
                raise ValueError
            trace_addresses[numeric_address] = name
            trace_counts[numeric_address] = numeric_count
        except ValueError:
            parser.error(
                f"invalid --trace-repeat-address value: {repeated_trace!r}"
            )
    base_url = f"http://127.0.0.1:{args.port}"

    process = subprocess.Popen(
        [str(executable), args.model, "paused=1"],
        cwd=release,
        stdout=subprocess.DEVNULL,
        stderr=subprocess.STDOUT,
    )
    tool = None
    try:
        deadline = time.monotonic() + 20
        while time.monotonic() < deadline:
            if process.poll() is not None:
                raise RuntimeError(f"emulator exited with status {process.returncode}")
            try:
                health, _ = http_json(f"{base_url}/health", timeout=1)
                if health.get("status") == "ok":
                    break
            except (error.URLError, TimeoutError, json.JSONDecodeError):
                time.sleep(0.2)
        else:
            raise RuntimeError("MCP health endpoint did not become ready")

        initialize = {
            "jsonrpc": "2.0",
            "id": 1,
            "method": "initialize",
            "params": {
                "protocolVersion": "2025-11-25",
                "capabilities": {},
                "clientInfo": {"name": "NXU16 runtime test", "version": "1.0"},
            },
        }
        response, headers = http_json(
            f"{base_url}/mcp",
            initialize,
            {"Accept": "application/json, text/event-stream"},
        )
        if "result" not in response:
            raise RuntimeError(f"MCP initialize failed: {response}")
        session = headers.get("MCP-Session-Id")
        if not session:
            raise RuntimeError("MCP initialize returned no session ID")
        session_headers = {
            "Accept": "application/json, text/event-stream",
            "MCP-Session-Id": session,
        }
        request_id = 2

        def call_tool(name, arguments=None):
            nonlocal request_id
            payload = {
                "jsonrpc": "2.0",
                "id": request_id,
                "method": "tools/call",
                "params": {"name": name, "arguments": arguments or {}},
            }
            request_id += 1
            result, _ = http_json(f"{base_url}/mcp", payload, session_headers)
            if result.get("error"):
                raise RuntimeError(json.dumps(result["error"]))
            body = result["result"]
            if body.get("isError"):
                raise RuntimeError(body["content"][0]["text"])
            return body["structuredContent"]

        def register_summary():
            registers = {
                register["name"].lower(): register["hex"]
                for register in tool("list_registers")["registers"]
            }
            return " ".join(
                f"{name}={registers[name]}"
                for name in ("sp", "lr", "psw", "csr", "lcsr")
                + tuple(f"r{number}" for number in range(16))
                if name in registers
            )

        tool = call_tool
        status = tool("get_status")
        if status["model_name"] != "fx-991CN X":
            raise RuntimeError(f"unexpected emulator model: {status['model_name']}")
        tool("pause")
        tool("clear_execution_breakpoints")
        # The MCP transport deliberately caps one byte-array argument at
        # 64 KiB.  Far-pointer tests make the ROM image span multiple banks,
        # so upload it in independently addressed chunks.
        chunk_size = 60 * 1024
        for offset in range(0, len(image), chunk_size):
            tool(
                "write_code",
                {"address": offset, "bytes": image[offset : offset + chunk_size]},
            )
        words = tool("read_code", {"address": entry, "count": 3})["words"]
        if len(words) != 3:
            raise RuntimeError("failed to verify injected entry point")
        tool("write_register", {"name": "sp", "value": 0xF000})
        tool("write_register", {"name": "pc", "value": entry})
        tool("add_execution_breakpoint", {"address": exit_loop})
        for address in trace_addresses:
            tool("add_execution_breakpoint", {"address": address})
        tool("resume")

        deadline = time.monotonic() + 10
        status = None
        pending_trace_step = False
        while time.monotonic() < deadline:
            time.sleep(0.05)
            status = tool("get_status")
            if status["paused"]:
                address = status["program_counter"]
                if address in trace_addresses:
                    pending_trace_step = False
                    print(
                        f"trace: {trace_addresses[address]} at {address:#07x} "
                        f"{register_summary()}"
                    )
                    for memory_address, memory_size in trace_memory:
                        values = tool(
                            "read_memory",
                            {"address": memory_address, "size": memory_size},
                        )["bytes"]
                        print(
                            f"memory: {memory_address:#06x}: "
                            + " ".join(f"{value:02x}" for value in values)
                        )
                    tool("remove_execution_breakpoint", {"address": address})
                    trace_counts[address] -= 1
                    if trace_counts[address] != 0:
                        tool("step_into")
                        tool("add_execution_breakpoint", {"address": address})
                        pending_trace_step = True
                    tool("resume")
                    continue
                if pending_trace_step:
                    pending_trace_step = False
                    tool("resume")
                    continue
                break
        if not status or not status["paused"]:
            tool("pause")
            status = tool("get_status")
            raise RuntimeError(
                f"program did not reach exit breakpoint; "
                f"PC={status['program_counter']:#x} {register_summary()}"
            )
        if status["program_counter"] != exit_loop:
            raise RuntimeError(
                f"program paused at {status['program_counter']:#x}, expected {exit_loop:#x}"
            )

        status_bytes = tool("read_memory", {"address": exit_code, "size": 2})["bytes"]
        result = status_bytes[0] | (status_bytes[1] << 8)
        length_bytes = tool("read_memory", {"address": console_length, "size": 2})["bytes"]
        length = length_bytes[0] | (length_bytes[1] << 8)
        output_bytes = tool("read_memory", {"address": console, "size": min(length, 255)})["bytes"]
        output = bytes(output_bytes).decode("ascii", errors="replace")
        if result != 0:
            raise RuntimeError(f"runtime returned {result}; console={output!r}")
        if output != "runtime:PASS\n":
            raise RuntimeError(f"unexpected console output: {output!r}")

        print("PASS")
        print(f"entry: {entry:#06x}")
        print(f"exit breakpoint: {exit_loop:#06x}")
        print(f"console: {output!r}")
        return 0
    finally:
        if tool is not None:
            try:
                tool("clear_execution_breakpoints")
                tool("hot_reload_rom")
            except Exception as cleanup_error:
                print(f"cleanup warning: {cleanup_error}", file=sys.stderr)
        process.terminate()
        try:
            process.wait(timeout=5)
        except subprocess.TimeoutExpired:
            process.kill()
            process.wait()


if __name__ == "__main__":
    try:
        sys.exit(main())
    except Exception as exception:
        print(f"FAIL: {exception}", file=sys.stderr)
        sys.exit(1)
