// The writing assistant's providers (web/src/assistant.js) over the real
// SDKs, against a fetch of this script's own that answers as Anthropic's
// and OpenAI's APIs do: no key, no network. What is tested is what the
// page relies on the SDKs for: the requests they make from what the
// application assembled (the headers a browser needs, the conversation
// sent whole each turn, append-only), the stream read as it comes, the tool
// loop going on with the tools' answers, the thinking's summary and the
// start of each tool call handed on as they stream, the request sent again
// without what a model or an organisation refuses (adaptive thinking, the
// effort, a reasoning summary), a stop, the SDKs' typed errors read as the
// application's kinds, the providers' lists of models,
// every page of them, in the one shape the application reads, and either
// provider's API at another address (a server on this machine, a
// gateway), with the key as a bearer token or without one, and the
// conversation kept there in the plain form for OpenAI's API, without the
// model's reasoning, and whole for Anthropic's.
// The browser tests use the scripted
// provider instead (wiki-app/jsbits/assistant.js); a real provider cannot
// be reached from here.
//
//   node toolchain/test-assistant.mjs
import assert from "node:assert/strict";
import { checkKey, describeError, listModels, startChat } from "../web/src/assistant.js";

let failures = 0;
const test = async (name, body) => {
  try {
    await body();
    console.log(`ok - ${name}`);
  } catch (error) {
    failures++;
    console.log(`not ok - ${name}`);
    console.log(error);
  }
};

// A fetch that answers each request with the next of `answers` (a function
// of the request, or a Response), and keeps the requests.
const fakeFetch = (answers) => {
  const requests = [];
  const fetch = async (url, init = {}) => {
    const headers = new Headers(init.headers);
    const body = init.body ? JSON.parse(init.body) : null;
    requests.push({ url: String(url), method: init.method ?? "GET", headers, body });
    if (init.signal?.aborted) throw Object.assign(new Error("aborted"), { name: "AbortError" });
    const answer = answers.shift();
    if (answer === undefined) throw new Error(`no answer left for ${url}`);
    return typeof answer === "function" ? answer(init) : answer;
  };
  return { fetch, requests };
};

const sse = (events) =>
  new Response(events.map(([event, data]) => `event: ${event}\ndata: ${JSON.stringify(data)}\n\n`).join(""), {
    status: 200,
    headers: { "content-type": "text/event-stream" },
  });

const json = (status, body) => new Response(JSON.stringify(body), { status, headers: { "content-type": "application/json" } });

// Anthropic's stream of one message: text, then tool calls, then why it
// stopped. The usage is the one message_start gives and the one
// message_delta gives at the end.
const anthropicMessage = (texts, tools, stopReason, usage = { start: { input_tokens: 1, output_tokens: 1 }, end: { output_tokens: 2 } }) => {
  const events = [
    ["message_start", { type: "message_start", message: { id: "msg", type: "message", role: "assistant", model: "m", content: [], stop_reason: null, stop_sequence: null, usage: usage.start } }],
    ["content_block_start", { type: "content_block_start", index: 0, content_block: { type: "text", text: "" } }],
    ...texts.map((text) => ["content_block_delta", { type: "content_block_delta", index: 0, delta: { type: "text_delta", text } }]),
    ["content_block_stop", { type: "content_block_stop", index: 0 }],
  ];
  tools.forEach(([id, name, input], i) => {
    events.push(
      ["content_block_start", { type: "content_block_start", index: i + 1, content_block: { type: "tool_use", id, name, input: {} } }],
      ["content_block_delta", { type: "content_block_delta", index: i + 1, delta: { type: "input_json_delta", partial_json: JSON.stringify(input) } }],
      ["content_block_stop", { type: "content_block_stop", index: i + 1 }],
    );
  });
  events.push(
    ["message_delta", { type: "message_delta", delta: { stop_reason: stopReason, stop_sequence: null }, usage: usage.end }],
    ["message_stop", { type: "message_stop" }],
  );
  return sse(events);
};

const anthropicRequest = {
  model: "claude-opus-5-5",
  max_tokens: 32000,
  output_config: { effort: "high" },
  thinking: { type: "adaptive", display: "summarized" },
  system: [{ type: "text", text: "REFERENCE", cache_control: { type: "ephemeral" } }],
  tools: [{ name: "replace", description: "d", input_schema: { type: "object", properties: {}, required: [] } }],
};

const collect = () => {
  const texts = [];
  const calls = [];
  return {
    texts,
    calls,
    handlers: {
      text: (t) => texts.push(t),
      tool: async (name, input) => {
        calls.push([name, input]);
        return { text: "Applied.", ok: true };
      },
    },
  };
};

await test("Anthropic: streams the text, runs the tool loop, and keeps the conversation", async () => {
  const { fetch, requests } = fakeFetch([
    anthropicMessage(["I will ", "fix it."], [["tu_1", "replace", { old_text: "a", new_text: "b" }]], "tool_use"),
    anthropicMessage(["Done."], [], "end_turn"),
    anthropicMessage(["Again."], [], "end_turn"),
  ]);
  const chat = startChat("anthropic", "sk-ant-test", anthropicRequest, { fetch, maxRetries: 0 });
  const { texts, calls, handlers } = collect();
  await chat.send("Fix it.", handlers, new AbortController().signal);
  assert.equal(texts.join(""), "I will fix it.Done.");
  assert.deepEqual(calls, [["replace", { old_text: "a", new_text: "b" }]]);
  assert.equal(requests.length, 2);
  const [first, second] = requests;
  assert.match(first.url, /\/v1\/messages$/);
  // The SDK sends the key as a bearer token (its 1.x releases do).
  assert.equal(first.headers.get("authorization") ?? `Bearer ${first.headers.get("x-api-key")}`, "Bearer sk-ant-test");
  assert.equal(first.headers.get("anthropic-dangerous-direct-browser-access"), "true");
  assert.equal(first.body.stream, true);
  assert.deepEqual(first.body.system, anthropicRequest.system);
  assert.deepEqual(first.body.messages, [{ role: "user", content: "Fix it." }]);
  // The tool's result goes back after the model's call, which goes back whole.
  assert.equal(second.body.messages.length, 3);
  assert.equal(second.body.messages[1].role, "assistant");
  assert.deepEqual(second.body.messages[1].content.at(-1), { type: "tool_use", id: "tu_1", name: "replace", input: { old_text: "a", new_text: "b" } });
  assert.deepEqual(second.body.messages[2], { role: "user", content: [{ type: "tool_result", tool_use_id: "tu_1", content: "Applied." }] });
  // The next message is sent after the whole conversation, unchanged, with
  // the request as it is now: another effort chosen since.
  await chat.send("More.", collect().handlers, new AbortController().signal, { ...anthropicRequest, output_config: { effort: "high" } });
  const third = requests[2];
  assert.deepEqual(third.body.messages.slice(0, 3), second.body.messages);
  assert.equal(third.body.messages[3].role, "assistant");
  assert.deepEqual(third.body.messages[4], { role: "user", content: "More." });
  assert.deepEqual(third.body.output_config, { effort: "high" });
  assert.deepEqual(third.body.system, anthropicRequest.system);
  assert.deepEqual(third.body.thinking, { type: "adaptive", display: "summarized" });
});

// Anthropic's stream of one message that thinks first: a thinking block
// whose summary streams in pieces, then text, then tool calls.
const anthropicThinking = (thoughts, texts, tools, stopReason) => {
  const events = [
    ["message_start", { type: "message_start", message: { id: "msg", type: "message", role: "assistant", model: "m", content: [], stop_reason: null, stop_sequence: null, usage: { input_tokens: 1, output_tokens: 1 } } }],
    ["content_block_start", { type: "content_block_start", index: 0, content_block: { type: "thinking", thinking: "", signature: "" } }],
    ...thoughts.map((thinking) => ["content_block_delta", { type: "content_block_delta", index: 0, delta: { type: "thinking_delta", thinking } }]),
    ["content_block_delta", { type: "content_block_delta", index: 0, delta: { type: "signature_delta", signature: "sig" } }],
    ["content_block_stop", { type: "content_block_stop", index: 0 }],
    ["content_block_start", { type: "content_block_start", index: 1, content_block: { type: "text", text: "" } }],
    ...texts.map((text) => ["content_block_delta", { type: "content_block_delta", index: 1, delta: { type: "text_delta", text } }]),
    ["content_block_stop", { type: "content_block_stop", index: 1 }],
  ];
  tools.forEach(([id, name, input], i) => {
    events.push(
      ["content_block_start", { type: "content_block_start", index: i + 2, content_block: { type: "tool_use", id, name, input: {} } }],
      ["content_block_delta", { type: "content_block_delta", index: i + 2, delta: { type: "input_json_delta", partial_json: JSON.stringify(input) } }],
      ["content_block_stop", { type: "content_block_stop", index: i + 2 }],
    );
  });
  events.push(
    ["message_delta", { type: "message_delta", delta: { stop_reason: stopReason, stop_sequence: null }, usage: { output_tokens: 2 } }],
    ["message_stop", { type: "message_stop" }],
  );
  return sse(events);
};

// Handlers that keep every event of a turn, in order.
const inOrder = () => {
  const events = [];
  return {
    events,
    handlers: {
      text: (t) => events.push(["text", t]),
      thinking: (t) => events.push(["thinking", t]),
      toolStart: (name) => events.push(["toolStart", name]),
      tool: async (name) => {
        events.push(["tool", name]);
        return { text: "Applied.", ok: true };
      },
    },
  };
};

await test("Anthropic: hands on the thinking's summary and the start of each tool call as they stream", async () => {
  const { fetch, requests } = fakeFetch([
    anthropicThinking(["Weighing ", "the words."], ["I will fix it."], [["tu_1", "replace", { old_text: "a", new_text: "b" }]], "tool_use"),
    anthropicMessage(["Done."], [], "end_turn"),
  ]);
  const chat = startChat("anthropic", "k", anthropicRequest, { fetch, maxRetries: 0 });
  const { events, handlers } = inOrder();
  await chat.send("Fix it.", handlers, new AbortController().signal);
  assert.deepEqual(events, [
    ["thinking", "Weighing "],
    ["thinking", "the words."],
    ["text", "I will fix it."],
    ["toolStart", "replace"],
    ["tool", "replace"],
    ["text", "Done."],
  ]);
  assert.deepEqual(requests[0].body.thinking, { type: "adaptive", display: "summarized" });
  // The thinking block goes back whole, with its signature.
  assert.deepEqual(requests[1].body.messages[1].content[0], { type: "thinking", thinking: "Weighing the words.", signature: "sig" });
});

await test("Anthropic: a model that refuses adaptive thinking is asked again without it, for the rest of the chat", async () => {
  const refused = json(400, { type: "error", error: { type: "invalid_request_error", message: "adaptive thinking is not supported on this model" } });
  const { fetch, requests } = fakeFetch([refused, anthropicMessage(["Fine."], [], "end_turn"), anthropicMessage(["Again."], [], "end_turn")]);
  const chat = startChat("anthropic", "k", { ...anthropicRequest, model: "claude-haiku-4-5" }, { fetch, maxRetries: 0 });
  const { texts, handlers } = collect();
  await chat.send("x", handlers, new AbortController().signal);
  await chat.send("y", handlers, new AbortController().signal);
  assert.equal(texts.join(""), "Fine.Again.");
  assert.equal(requests.length, 3);
  assert.deepEqual(requests[0].body.thinking, { type: "adaptive", display: "summarized" });
  assert.equal(requests[1].body.thinking, undefined);
  assert.equal(requests[2].body.thinking, undefined);
  assert.deepEqual(requests[1].body.messages, [{ role: "user", content: "x" }]);
});

// Claude Haiku 4.5's answer to a request with an effort, as it was on
// 2026-10-28.
const effortRefused = () =>
  json(400, { type: "error", error: { type: "invalid_request_error", message: "This model does not support the effort parameter." } });

await test("Anthropic: a model that refuses the effort is asked again without it, for the rest of the chat", async () => {
  const { fetch, requests } = fakeFetch([effortRefused(), anthropicMessage(["Fine."], [], "end_turn"), anthropicMessage(["Again."], [], "end_turn")]);
  const chat = startChat("anthropic", "k", { ...anthropicRequest, model: "claude-haiku-4-5" }, { fetch, maxRetries: 0 });
  const { texts, handlers } = collect();
  await chat.send("x", handlers, new AbortController().signal);
  await chat.send("y", handlers, new AbortController().signal, { ...anthropicRequest, model: "claude-haiku-4-5", output_config: { effort: "low" } });
  assert.equal(texts.join(""), "Fine.Again.");
  assert.equal(requests.length, 3);
  assert.deepEqual(requests[0].body.output_config, { effort: "high" });
  assert.equal(requests[1].body.output_config, undefined);
  assert.equal(requests[2].body.output_config, undefined);
  // Only the effort is left out: the thinking is still asked for.
  assert.deepEqual(requests[1].body.thinking, { type: "adaptive", display: "summarized" });
  assert.deepEqual(requests[2].body.thinking, { type: "adaptive", display: "summarized" });
  assert.deepEqual(requests[1].body.messages, [{ role: "user", content: "x" }]);
});

// Claude Haiku 4.5 refuses adaptive thinking first, and the effort in the
// request sent again without it (seen on 2026-10-28).
await test("Anthropic: a model that refuses adaptive thinking and the effort is asked again without both", async () => {
  const thinkingRefused = json(400, { type: "error", error: { type: "invalid_request_error", message: "adaptive thinking is not supported on this model" } });
  const { fetch, requests } = fakeFetch([thinkingRefused, effortRefused(), anthropicMessage(["Fine."], [], "end_turn"), anthropicMessage(["Again."], [], "end_turn")]);
  const chat = startChat("anthropic", "k", { ...anthropicRequest, model: "claude-haiku-4-5" }, { fetch, maxRetries: 0 });
  const { texts, handlers } = collect();
  await chat.send("x", handlers, new AbortController().signal);
  await chat.send("y", handlers, new AbortController().signal);
  assert.equal(texts.join(""), "Fine.Again.");
  assert.equal(requests.length, 4);
  assert.deepEqual([requests[1].body.output_config, requests[1].body.thinking], [{ effort: "high" }, undefined]);
  for (const r of requests.slice(2)) assert.deepEqual([r.body.output_config, r.body.thinking], [undefined, undefined]);
});

await test("Anthropic: a refusal of the effort repeated is not asked again", async () => {
  const { fetch, requests } = fakeFetch([effortRefused(), effortRefused()]);
  const chat = startChat("anthropic", "k", anthropicRequest, { fetch, maxRetries: 0 });
  const error = await chat.send("x", collect().handlers, new AbortController().signal).then(() => null, (e) => e);
  assert.equal(describeError(error).kind, "bad_request");
  assert.equal(requests.length, 2);
});

await test("Anthropic: another 400 is not taken for a refusal of thinking", async () => {
  const { fetch, requests } = fakeFetch([json(400, { type: "error", error: { type: "invalid_request_error", message: "max_tokens: too large" } })]);
  const chat = startChat("anthropic", "k", anthropicRequest, { fetch, maxRetries: 0 });
  const error = await chat.send("x", collect().handlers, new AbortController().signal).then(() => null, (e) => e);
  assert.equal(describeError(error).kind, "bad_request");
  assert.equal(requests.length, 1);
});

await test("Anthropic: a refused tool's answer is marked as an error", async () => {
  const { fetch, requests } = fakeFetch([
    anthropicMessage([], [["tu_1", "replace", { old_text: "x", new_text: "y" }]], "tool_use"),
    anthropicMessage(["Sorry."], [], "end_turn"),
  ]);
  const chat = startChat("anthropic", "k", anthropicRequest, { fetch, maxRetries: 0 });
  await chat.send("Go.", { text: () => {}, tool: async () => ({ text: "Not applied: not found.", ok: false }) }, new AbortController().signal);
  assert.deepEqual(requests[1].body.messages[2].content[0], { type: "tool_result", tool_use_id: "tu_1", content: "Not applied: not found.", is_error: true });
});

await test("Anthropic: the SDK's typed errors are the application's kinds", async () => {
  const cases = [
    [401, { type: "error", error: { type: "authentication_error", message: "invalid x-api-key" } }, "authentication"],
    [403, { type: "error", error: { type: "permission_error", message: "no" } }, "permission"],
    [404, { type: "error", error: { type: "not_found_error", message: "model: claude-none" } }, "not_found"],
    [429, { type: "error", error: { type: "rate_limit_error", message: "slow down" } }, "rate_limit"],
    [400, { type: "error", error: { type: "invalid_request_error", message: "bad" } }, "bad_request"],
    [529, { type: "error", error: { type: "overloaded_error", message: "Overloaded" } }, "overloaded"],
  ];
  for (const [status, body, kind] of cases) {
    const { fetch } = fakeFetch([json(status, body)]);
    const chat = startChat("anthropic", "k", anthropicRequest, { fetch, maxRetries: 0 });
    const error = await chat.send("x", collect().handlers, new AbortController().signal).then(
      () => null,
      (e) => e,
    );
    assert.deepEqual(describeError(error), { kind, status, message: body.error.message }, `status ${status}`);
  }
});

await test("Anthropic: the SDK retries an overloaded provider", async () => {
  const { fetch, requests } = fakeFetch([json(529, { type: "error", error: { type: "overloaded_error", message: "Overloaded" } }), anthropicMessage(["Fine."], [], "end_turn")]);
  const chat = startChat("anthropic", "k", anthropicRequest, { fetch, maxRetries: 1 });
  const { texts, handlers } = collect();
  await chat.send("x", handlers, new AbortController().signal);
  assert.equal(texts.join(""), "Fine.");
  assert.equal(requests.length, 2);
});

await test("Anthropic: a refusal is an error of its own", async () => {
  const { fetch } = fakeFetch([anthropicMessage([], [], "refusal")]);
  const chat = startChat("anthropic", "k", anthropicRequest, { fetch, maxRetries: 0 });
  const error = await chat.send("x", collect().handlers, new AbortController().signal).then(() => null, (e) => e);
  assert.equal(describeError(error).kind, "refusal");
});

await test("Anthropic: the network is the connection's error", async () => {
  const fetch = async () => {
    throw new TypeError("Failed to fetch");
  };
  const chat = startChat("anthropic", "k", anthropicRequest, { fetch, maxRetries: 0 });
  const error = await chat.send("x", collect().handlers, new AbortController().signal).then(() => null, (e) => e);
  assert.equal(describeError(error).kind, "connection");
});

await test("Anthropic: a stop aborts the request", async () => {
  const controller = new AbortController();
  const fetch = (url, init) =>
    new Promise((_, reject) => {
      init.signal.addEventListener("abort", () => reject(Object.assign(new Error("aborted"), { name: "AbortError" })));
      setTimeout(() => controller.abort(), 10);
    });
  const chat = startChat("anthropic", "k", anthropicRequest, { fetch, maxRetries: 0 });
  const error = await chat.send("x", collect().handlers, controller.signal).then(() => null, (e) => e);
  assert.equal(describeError(error).kind, "aborted");
});

const openAiRequest = {
  model: "gpt-6.1-sol",
  instructions: "REFERENCE\n\nINSTRUCTIONS",
  tools: [{ type: "function", name: "replace", description: "d", parameters: { type: "object", properties: {}, required: [], additionalProperties: false }, strict: true }],
  store: false,
  include: ["reasoning.encrypted_content"],
  reasoning: { effort: "low", summary: "auto" },
};

const openAiResponse = (texts, output, usage) =>
  sse([
    ["response.created", { type: "response.created", sequence_number: 0, response: { id: "resp", output: [] } }],
    ...texts.map((delta, i) => ["response.output_text.delta", { type: "response.output_text.delta", sequence_number: i + 1, item_id: "msg", output_index: 0, content_index: 0, delta }]),
    ["response.completed", { type: "response.completed", sequence_number: 99, response: { id: "resp", status: "completed", output, ...(usage ? { usage } : {}) } }],
  ]);

await test("OpenAI: streams the text, runs the tool loop, and sends the conversation whole", async () => {
  const reasoning = { type: "reasoning", id: "rs_1", summary: [], encrypted_content: "opaque" };
  const call = { type: "function_call", id: "fc_1", call_id: "call_1", name: "replace", arguments: '{"old_text":"a","new_text":"b"}', status: "completed" };
  const message = { type: "message", id: "msg_1", role: "assistant", status: "completed", content: [{ type: "output_text", text: "Done.", annotations: [] }] };
  const { fetch, requests } = fakeFetch([openAiResponse(["I will "], [reasoning, call]), openAiResponse(["Done."], [message])]);
  const chat = startChat("openai", "sk-open", openAiRequest, { fetch, maxRetries: 0 });
  const { texts, calls, handlers } = collect();
  await chat.send("Fix it.", handlers, new AbortController().signal);
  assert.equal(texts.join(""), "I will Done.");
  assert.deepEqual(calls, [["replace", { old_text: "a", new_text: "b" }]]);
  const [first, second] = requests;
  assert.match(first.url, /\/v1\/responses$/);
  assert.equal(first.headers.get("authorization"), "Bearer sk-open");
  assert.equal(first.body.store, false);
  assert.deepEqual(first.body.reasoning, { effort: "low", summary: "auto" });
  assert.equal(first.body.max_output_tokens, undefined);
  assert.equal(first.body.instructions, openAiRequest.instructions);
  assert.deepEqual(first.body.input, [{ role: "user", content: "Fix it." }]);
  assert.deepEqual(second.body.input, [
    { role: "user", content: "Fix it." },
    reasoning,
    call,
    { type: "function_call_output", call_id: "call_1", output: "Applied." },
  ]);
  assert.equal(second.body.previous_response_id, undefined);
});

await test("OpenAI: hands on the reasoning's summary, its parts apart, and the start of each tool call as they stream", async () => {
  const call = { type: "function_call", id: "fc_1", call_id: "call_1", name: "replace", arguments: '{"old_text":"a","new_text":"b"}', status: "completed" };
  const message = { type: "message", id: "msg_1", role: "assistant", status: "completed", content: [{ type: "output_text", text: "Done.", annotations: [] }] };
  const part = (i, n) => ["response.reasoning_summary_part.added", { type: "response.reasoning_summary_part.added", sequence_number: n, item_id: "rs_1", output_index: 0, summary_index: i, part: { type: "summary_text", text: "" } }];
  const delta = (i, n, d) => ["response.reasoning_summary_text.delta", { type: "response.reasoning_summary_text.delta", sequence_number: n, item_id: "rs_1", output_index: 0, summary_index: i, delta: d }];
  const first = sse([
    ["response.created", { type: "response.created", sequence_number: 0, response: { id: "resp", output: [] } }],
    part(0, 1),
    delta(0, 2, "Weighing "),
    delta(0, 3, "the words."),
    part(1, 4),
    delta(1, 5, "Then the edit."),
    ["response.output_item.added", { type: "response.output_item.added", sequence_number: 6, output_index: 1, item: { ...call, arguments: "", status: "in_progress" } }],
    ["response.completed", { type: "response.completed", sequence_number: 7, response: { id: "resp", status: "completed", output: [call] } }],
  ]);
  const { fetch } = fakeFetch([first, openAiResponse(["Done."], [message])]);
  const chat = startChat("openai", "k", openAiRequest, { fetch, maxRetries: 0 });
  const { events, handlers } = inOrder();
  await chat.send("Fix it.", handlers, new AbortController().signal);
  assert.deepEqual(events, [
    ["thinking", "Weighing "],
    ["thinking", "the words."],
    ["thinking", "\n\n"],
    ["thinking", "Then the edit."],
    ["toolStart", "replace"],
    ["tool", "replace"],
    ["text", "Done."],
  ]);
});

await test("OpenAI: an organisation refused reasoning summaries is asked again without them, for the rest of the chat", async () => {
  const refused = json(400, {
    error: { message: "Your organization must be verified to generate reasoning summaries. Please go to: https://platform.openai.com/settings/organization/general and click on Verify Organization.", type: "invalid_request_error", param: "reasoning.summary", code: "unsupported_value" },
  });
  const message = (text) => ({ type: "message", id: "msg_1", role: "assistant", status: "completed", content: [{ type: "output_text", text, annotations: [] }] });
  const { fetch, requests } = fakeFetch([refused, openAiResponse(["Fine."], [message("Fine.")]), openAiResponse(["Again."], [message("Again.")])]);
  const chat = startChat("openai", "k", openAiRequest, { fetch, maxRetries: 0 });
  const { texts, handlers } = collect();
  await chat.send("x", handlers, new AbortController().signal);
  await chat.send("y", handlers, new AbortController().signal);
  assert.equal(texts.join(""), "Fine.Again.");
  assert.equal(requests.length, 3);
  assert.deepEqual(requests[0].body.reasoning, { effort: "low", summary: "auto" });
  assert.deepEqual(requests[1].body.reasoning, { effort: "low" });
  assert.deepEqual(requests[2].body.reasoning, { effort: "low" });
});

await test("OpenAI: an answer cut short is an error, not an empty answer", async () => {
  const { fetch } = fakeFetch([
    sse([
      ["response.created", { type: "response.created", sequence_number: 0, response: { id: "resp", output: [] } }],
      ["response.incomplete", { type: "response.incomplete", sequence_number: 1, response: { id: "resp", status: "incomplete", incomplete_details: { reason: "max_output_tokens" }, output: [] } }],
    ]),
  ]);
  const chat = startChat("openai", "k", openAiRequest, { fetch, maxRetries: 0 });
  const error = await chat.send("x", collect().handlers, new AbortController().signal).then(() => null, (e) => e);
  assert.deepEqual(describeError(error), { kind: "failed", status: null, message: "The answer was cut short (max_output_tokens)." });
});

await test("OpenAI: the SDK's typed errors are the application's kinds", async () => {
  const cases = [
    [401, { error: { message: "Incorrect API key provided: sk-proj-****abcd.", type: "invalid_request_error", code: "invalid_api_key" } }, "authentication"],
    [404, { error: { message: "The model `gpt-none` does not exist", type: "invalid_request_error", code: "model_not_found" } }, "not_found"],
    [429, { error: { message: "Rate limit reached", type: "requests", code: "rate_limit_exceeded" } }, "rate_limit"],
    [500, { error: { message: "Server error", type: "server_error" } }, "overloaded"],
  ];
  for (const [status, body, kind] of cases) {
    const { fetch } = fakeFetch([json(status, body)]);
    const chat = startChat("openai", "k", openAiRequest, { fetch, maxRetries: 0 });
    const error = await chat.send("x", collect().handlers, new AbortController().signal).then(() => null, (e) => e);
    assert.deepEqual(describeError(error), { kind, status, message: body.error.message }, `status ${status}`);
  }
});

await test("Test the key: the model looked up, which costs nothing", async () => {
  const a = fakeFetch([json(200, { type: "model", id: "claude-opus-5-5", display_name: "Claude Opus 5.5", created_at: "2026-09-01T00:00:00Z" })]);
  await checkKey("anthropic", "sk-ant", "claude-opus-5-5", { fetch: a.fetch, maxRetries: 0 });
  assert.equal(a.requests[0].method, "GET");
  assert.match(a.requests[0].url, /\/v1\/models\/claude-opus-5-5$/);
  const o = fakeFetch([json(404, { error: { message: "The model `nope` does not exist", type: "invalid_request_error" } })]);
  const error = await checkKey("openai", "sk-open", "nope", { fetch: o.fetch, maxRetries: 0 }).then(() => null, (e) => e);
  assert.equal(describeError(error).kind, "not_found");
  assert.match(o.requests[0].url, /\/v1\/models\/nope$/);
});

await test("The models: Anthropic's every page, with their names and times", async () => {
  const model = (id, name, at) => ({ type: "model", id, display_name: name, created_at: at });
  const a = fakeFetch([
    json(200, {
      data: [model("claude-haiku-5-5", "Claude Haiku 5.5", "2026-10-01T00:00:00Z"), model("claude-opus-5-5", "Claude Opus 5.5", "2026-09-01T00:00:00Z")],
      has_more: true,
      first_id: "claude-haiku-5-5",
      last_id: "claude-opus-5-5",
    }),
    json(200, { data: [model("claude-haiku-4-5-20251001", "Claude Haiku 4.5", "2025-10-01T00:00:00Z")], has_more: false, first_id: "claude-haiku-4-5-20251001", last_id: "claude-haiku-4-5-20251001" }),
  ]);
  const listed = await listModels("anthropic", "sk-ant", { fetch: a.fetch, maxRetries: 0 });
  assert.deepEqual(listed, [
    { id: "claude-haiku-5-5", label: "Claude Haiku 5.5", created: Date.UTC(2026, 9, 1) / 1000 },
    { id: "claude-opus-5-5", label: "Claude Opus 5.5", created: Date.UTC(2026, 8, 1) / 1000 },
    { id: "claude-haiku-4-5-20251001", label: "Claude Haiku 4.5", created: Date.UTC(2025, 9, 1) / 1000 },
  ]);
  assert.equal(a.requests.length, 2);
  assert.match(a.requests[0].url, /\/v1\/models\?limit=1000$/);
  assert.match(a.requests[1].url, /after_id=claude-opus-5-5/);
  assert.equal(a.requests[0].headers.get("authorization"), "Bearer sk-ant");
});

await test("The models: OpenAI's, without names, and its errors typed", async () => {
  const o = fakeFetch([
    json(200, {
      object: "list",
      data: [
        { id: "gpt-6.1-sol", object: "model", created: 1790000000, owned_by: "system" },
        { id: "whisper-1", object: "model", created: 1677532384, owned_by: "openai-internal" },
      ],
    }),
  ]);
  const listed = await listModels("openai", "sk-open", { fetch: o.fetch, maxRetries: 0 });
  assert.deepEqual(listed, [
    { id: "gpt-6.1-sol", label: null, created: 1790000000 },
    { id: "whisper-1", label: null, created: 1677532384 },
  ]);
  assert.match(o.requests[0].url, /\/v1\/models$/);
  const refused = fakeFetch([json(401, { error: { message: "Incorrect API key provided", type: "invalid_request_error" } })]);
  const error = await listModels("openai", "sk-bad", { fetch: refused.fetch, maxRetries: 0 }).then(() => null, (e) => e);
  assert.equal(describeError(error).kind, "authentication");
});

await test("OpenAI's API elsewhere: the base URL reaches the client, and an empty key sends no Authorization", async () => {
  const o = fakeFetch([json(200, { object: "list", data: [{ id: "llama4:8b", object: "model", created: 1790000000, owned_by: "library" }] }), openAiResponse(["Hi."], [{ type: "message", id: "msg_1", role: "assistant", status: "completed", content: [{ type: "output_text", text: "Hi.", annotations: [] }] }])]);
  const options = { fetch: o.fetch, maxRetries: 0, baseURL: "http://localhost:11434/v1" };
  assert.deepEqual(await listModels("openai", "", options), [{ id: "llama4:8b", label: null, created: 1790000000 }]);
  assert.equal(o.requests[0].url, "http://localhost:11434/v1/models");
  assert.equal(o.requests[0].headers.has("authorization"), false);
  const chat = startChat("openai", "", { ...openAiRequest, model: "llama4:8b" }, options);
  const said = [];
  await chat.send("Hello", { text: (t) => said.push(t), tool: async () => ({ text: "", ok: false }) }, new AbortController().signal);
  assert.equal(o.requests[1].url, "http://localhost:11434/v1/responses");
  assert.equal(o.requests[1].headers.has("authorization"), false);
  assert.equal(said.join(""), "Hi.");
  // With a key, it is sent there as a bearer token (GitHub Models' tokens are).
  const g = fakeFetch([json(200, { object: "list", data: [] })]);
  await listModels("openai", "ghp_token", { fetch: g.fetch, maxRetries: 0, baseURL: "https://models.github.ai/inference" });
  assert.equal(g.requests[0].url, "https://models.github.ai/inference/models");
  assert.equal(g.requests[0].headers.get("authorization"), "Bearer ghp_token");
});

await test("Anthropic's API elsewhere: a gateway's base URL reaches the client, and the key goes as a bearer token", async () => {
  // An organisation's gateway in front of Anthropic's API (what Claude
  // Code takes as ANTHROPIC_BASE_URL) takes the key as Authorization:
  // Bearer, not as x-api-key. The SDK sends it that way.
  const gateway = "https://claude-gateway.example.com";
  const a = fakeFetch([
    json(200, { data: [{ type: "model", id: "claude-opus-5-5", display_name: "Claude Opus 5.5", created_at: "2026-09-01T00:00:00Z" }], has_more: false, first_id: "claude-opus-5-5", last_id: "claude-opus-5-5" }),
    anthropicMessage(["Hi."], [], "end_turn"),
  ]);
  const options = { fetch: a.fetch, maxRetries: 0, baseURL: gateway };
  assert.deepEqual(await listModels("anthropic", "gw-token", options), [{ id: "claude-opus-5-5", label: "Claude Opus 5.5", created: Date.UTC(2026, 8, 1) / 1000 }]);
  const chat = startChat("anthropic", "gw-token", anthropicRequest, options);
  const said = [];
  await chat.send("Hello", { text: (t) => said.push(t), tool: async () => ({ text: "", ok: false }) }, new AbortController().signal);
  assert.equal(said.join(""), "Hi.");
  assert.match(a.requests[0].url, /^https:\/\/claude-gateway\.example\.com\/v1\/models\?limit=1000$/);
  assert.equal(a.requests[1].url, `${gateway}/v1/messages`);
  for (const request of a.requests) {
    assert.equal(request.headers.get("authorization"), "Bearer gw-token");
    assert.equal(request.headers.has("x-api-key"), false);
    assert.equal(request.headers.get("anthropic-dangerous-direct-browser-access"), "true");
  }
  // A key the gateway refuses (an expired token) is the application's
  // "authentication" kind, which says to paste a new one.
  const refused = fakeFetch([json(401, { type: "error", error: { type: "authentication_error", message: "token expired" } })]);
  const error = await checkKey("anthropic", "gw-token", "claude-opus-5-5", { fetch: refused.fetch, maxRetries: 0, baseURL: gateway }).then(
    () => null,
    (e) => e,
  );
  assert.deepEqual(describeError(error), { kind: "authentication", status: 401, message: "token expired" });
  assert.equal(refused.requests[0].url, `${gateway}/v1/models/claude-opus-5-5`);
  // Without a key, nothing that looks like one is sent.
  const bare = fakeFetch([json(200, { data: [], has_more: false, first_id: null, last_id: null })]);
  assert.deepEqual(await listModels("anthropic", "", { fetch: bare.fetch, maxRetries: 0, baseURL: gateway }), []);
  assert.equal(bare.requests[0].headers.has("authorization"), false);
  assert.equal(bare.requests[0].headers.has("x-api-key"), false);
});

// What the chat keeps of a finished exchange, at the provider's own
// address and at a service elsewhere (a base URL). At the provider's own
// address the conversation goes back as the provider gave it. At a service
// elsewhere speaking OpenAI's API it goes back in the plain form every
// compatible server reads, without the model's reasoning: LiteLLM in front
// of Databricks (the owner's case of 2026-11-17) could not read back the
// reasoning item it wrote. Speaking Anthropic's API, the same gateway read
// the thinking blocks back, so Anthropic's conversation goes back whole at
// every address.
const openAiMessage = (id, content) => ({ type: "message", id, role: "assistant", status: "completed", content });
const outputText = (text) => ({ type: "output_text", text, annotations: [] });

await test("OpenAI at its own address: the next message goes after the output as it was given, reasoning included", async () => {
  const reasoning = { type: "reasoning", id: "rs_1", summary: [], encrypted_content: "opaque" };
  const message = openAiMessage("msg_1", [outputText("Hi!")]);
  const { fetch, requests } = fakeFetch([openAiResponse(["Hi!"], [reasoning, message]), openAiResponse(["Again."], [openAiMessage("msg_2", [outputText("Again.")])])]);
  const chat = startChat("openai", "k", openAiRequest, { fetch, maxRetries: 0 });
  await chat.send("hi", collect().handlers, new AbortController().signal);
  await chat.send("And now?", collect().handlers, new AbortController().signal);
  assert.deepEqual(requests[1].body, {
    ...openAiRequest,
    stream: true,
    input: [{ role: "user", content: "hi" }, reasoning, message, { role: "user", content: "And now?" }],
  });
});

await test("OpenAI elsewhere: the reasoning is not sent back, and the answer goes as plain text (a gateway that cannot read its own reasoning item)", async () => {
  // The owner's gateway, a LiteLLM proxy in front of Databricks, gave the
  // reasoning as an item of its own whose content is output_text, and
  // refused that item when it came back with the second message.
  const reasoning = {
    type: "reasoning",
    id: "rs_1",
    role: "assistant",
    status: "completed",
    content: [outputText('The user is just saying "hi" - a simple greeting. I should respond friendly and briefly.')],
  };
  const { fetch, requests } = fakeFetch([openAiResponse(["Hi!"], [reasoning, openAiMessage("msg_1", [outputText("Hi!")])]), openAiResponse(["Fine."], [openAiMessage("msg_2", [outputText("Fine.")])])]);
  const chat = startChat("openai", "gw-token", openAiRequest, { fetch, maxRetries: 0, baseURL: "https://litellm.example.com/v1" });
  await chat.send("hi", collect().handlers, new AbortController().signal);
  await chat.send("And now?", collect().handlers, new AbortController().signal);
  assert.equal(requests[1].url, "https://litellm.example.com/v1/responses");
  assert.deepEqual(requests[1].body, {
    ...openAiRequest,
    stream: true,
    input: [
      { role: "user", content: "hi" },
      { role: "assistant", content: "Hi!" },
      { role: "user", content: "And now?" },
    ],
  });
  assert.equal(JSON.stringify(requests[1].body.input).includes("output_text"), false);
});

await test("OpenAI elsewhere: a tool loop sends the call and its output plain, in the loop and with the next message", async () => {
  const reasoning = { type: "reasoning", id: "rs_1", summary: [{ type: "summary_text", text: "Weighing." }], encrypted_content: "opaque" };
  const call = { type: "function_call", id: "fc_1", call_id: "call_1", name: "replace", arguments: '{"old_text":"a","new_text":"b"}', status: "completed" };
  const done = openAiMessage("msg_1", [outputText("Done."), outputText(" All of it.")]);
  const { fetch, requests } = fakeFetch([
    openAiResponse([], [reasoning, call]),
    openAiResponse(["Done. All of it."], [{ ...reasoning, id: "rs_2" }, done]),
    openAiResponse(["Fine."], [openAiMessage("msg_2", [outputText("Fine.")])]),
  ]);
  const chat = startChat("openai", "k", openAiRequest, { fetch, maxRetries: 0, baseURL: "https://gateway.example.com/v1" });
  const { calls, handlers } = collect();
  await chat.send("Fix it.", handlers, new AbortController().signal);
  assert.deepEqual(calls, [["replace", { old_text: "a", new_text: "b" }]]);
  const plainCall = { type: "function_call", call_id: "call_1", name: "replace", arguments: '{"old_text":"a","new_text":"b"}' };
  const output = { type: "function_call_output", call_id: "call_1", output: "Applied." };
  assert.deepEqual(requests[1].body, { ...openAiRequest, stream: true, input: [{ role: "user", content: "Fix it." }, plainCall, output] });
  await chat.send("More.", collect().handlers, new AbortController().signal);
  assert.deepEqual(requests[2].body, {
    ...openAiRequest,
    stream: true,
    input: [{ role: "user", content: "Fix it." }, plainCall, output, { role: "assistant", content: "Done. All of it." }, { role: "user", content: "More." }],
  });
});

await test("OpenAI elsewhere: a refusal goes back as the assistant's text, and a message without text is left out", async () => {
  const { fetch, requests } = fakeFetch([
    openAiResponse([], [openAiMessage("msg_1", [{ type: "refusal", refusal: "I cannot help with that." }]), openAiMessage("msg_2", [])]),
    openAiResponse(["Fine."], [openAiMessage("msg_3", [outputText("Fine.")])]),
  ]);
  const chat = startChat("openai", "", openAiRequest, { fetch, maxRetries: 0, baseURL: "http://localhost:11434/v1" });
  await chat.send("x", collect().handlers, new AbortController().signal);
  await chat.send("y", collect().handlers, new AbortController().signal);
  assert.deepEqual(requests[1].body.input, [
    { role: "user", content: "x" },
    { role: "assistant", content: "I cannot help with that." },
    { role: "user", content: "y" },
  ]);
});

await test("Anthropic, at its own address and at a gateway alike: the next message goes after the content as it was given, thinking included", async () => {
  // A gateway in front of another backend reads the thinking blocks back:
  // LiteLLM in front of Databricks did (2026-11-17), where it could not
  // read back OpenAI's reasoning items.
  for (const baseURL of [undefined, "https://claude-gateway.example.com"]) {
    const { fetch, requests } = fakeFetch([anthropicThinking(["Weighing."], ["Hi."], [], "end_turn"), anthropicMessage(["Again."], [], "end_turn")]);
    const chat = startChat("anthropic", "k", anthropicRequest, { fetch, maxRetries: 0, ...(baseURL ? { baseURL } : {}) });
    await chat.send("Hello", collect().handlers, new AbortController().signal);
    await chat.send("Again", collect().handlers, new AbortController().signal);
    assert.equal(requests[1].url, `${baseURL ?? "https://api.anthropic.com"}/v1/messages`);
    assert.deepEqual(
      requests[1].body,
      {
        ...anthropicRequest,
        stream: true,
        messages: [
          { role: "user", content: "Hello" },
          {
            role: "assistant",
            content: [
              { type: "thinking", thinking: "Weighing.", signature: "sig" },
              { type: "text", text: "Hi." },
            ],
          },
          { role: "user", content: "Again" },
        ],
      },
      baseURL ?? "the provider's own address",
    );
  }
});

// The tokens of each request, as the application reads them (WP5.125):
// `input` every input token of the request, cached or not; `cached` the
// part read from the provider's cache; `written` the part written to it;
// `output` the output. Anthropic's input_tokens leaves out the tokens read
// from and written to the cache, which are beside it; OpenAI's holds them,
// with input_tokens_details naming the parts.
const usages = () => {
  const told = [];
  const { handlers } = collect();
  return { told, handlers: { ...handlers, usage: (u) => told.push(u) } };
};

await test("Anthropic: each request's usage is told, the cache's tokens counted in the input, over a tool loop", async () => {
  const { fetch } = fakeFetch([
    anthropicMessage([], [["tu_1", "replace", { old_text: "a", new_text: "b" }]], "tool_use", {
      start: { input_tokens: 40, cache_creation_input_tokens: 1200, cache_read_input_tokens: 0, output_tokens: 1 },
      end: { output_tokens: 75 },
    }),
    anthropicMessage(["Done."], [], "end_turn", {
      start: { input_tokens: 90, cache_creation_input_tokens: 0, cache_read_input_tokens: 1200, output_tokens: 1 },
      end: { output_tokens: 12 },
    }),
  ]);
  const chat = startChat("anthropic", "k", anthropicRequest, { fetch, maxRetries: 0 });
  const { told, handlers } = usages();
  await chat.send("Fix it.", handlers, new AbortController().signal);
  assert.deepEqual(told, [
    { input: 1240, cached: 0, written: 1200, output: 75 },
    { input: 1290, cached: 1200, written: 0, output: 12 },
  ]);
});

await test("Anthropic: a service that leaves out the cache's counts, or the whole usage, tells only what it gave", async () => {
  const { fetch } = fakeFetch([
    anthropicMessage(["One."], [], "end_turn", { start: { input_tokens: 300, output_tokens: 1 }, end: { output_tokens: 20 } }),
    anthropicMessage(["Two."], [], "end_turn", { start: {}, end: {} }),
  ]);
  const chat = startChat("anthropic", "k", anthropicRequest, { fetch, maxRetries: 0 });
  const { told, handlers } = usages();
  await chat.send("x", handlers, new AbortController().signal);
  await chat.send("y", handlers, new AbortController().signal);
  assert.deepEqual(told, [{ input: 300, cached: null, written: null, output: 20 }]);
});

await test("OpenAI: each request's usage is told, the cached tokens a part of the input, over a tool loop", async () => {
  const call = { type: "function_call", id: "fc_1", call_id: "call_1", name: "replace", arguments: '{"old_text":"a","new_text":"b"}', status: "completed" };
  const message = { type: "message", id: "msg_1", role: "assistant", status: "completed", content: [{ type: "output_text", text: "Done.", annotations: [] }] };
  const usage = (input, cached, output) => ({ input_tokens: input, input_tokens_details: { cached_tokens: cached }, output_tokens: output, output_tokens_details: { reasoning_tokens: 0 }, total_tokens: input + output });
  const { fetch } = fakeFetch([openAiResponse([], [call], usage(5000, 0, 80)), openAiResponse(["Done."], [message], usage(5120, 4864, 15))]);
  const chat = startChat("openai", "k", openAiRequest, { fetch, maxRetries: 0 });
  const { told, handlers } = usages();
  await chat.send("Fix it.", handlers, new AbortController().signal);
  assert.deepEqual(told, [
    { input: 5000, cached: 0, written: null, output: 80 },
    { input: 5120, cached: 4864, written: null, output: 15 },
  ]);
});

await test("OpenAI: a response without usage tells nothing, and one with the tokens written to the cache tells them", async () => {
  const message = { type: "message", id: "msg_1", role: "assistant", status: "completed", content: [{ type: "output_text", text: "Hi.", annotations: [] }] };
  const { fetch } = fakeFetch([
    openAiResponse(["Hi."], [message]),
    openAiResponse(["Hi."], [message], { input_tokens: "many", output_tokens: 3 }),
    openAiResponse(["Hi."], [message], { input_tokens: 900, input_tokens_details: { cached_tokens: 0, cache_write_tokens: 640 }, output_tokens: 3, total_tokens: 903 }),
  ]);
  const chat = startChat("openai", "", openAiRequest, { fetch, maxRetries: 0, baseURL: "http://localhost:11434/v1" });
  const { told, handlers } = usages();
  for (const m of ["x", "y", "z"]) await chat.send(m, handlers, new AbortController().signal);
  assert.deepEqual(told, [{ input: 900, cached: 0, written: 640, output: 3 }]);
});

// A response that ends incomplete or failed can still carry its usage
// (ResponseIncompleteEvent and ResponseFailedEvent hold the whole
// Response, whose usage is optional): its tokens were spent, and are told
// before the error.
const openAiEnded = (type, response) =>
  sse([
    ["response.created", { type: "response.created", sequence_number: 0, response: { id: "resp", output: [] } }],
    [type, { type, sequence_number: 1, response: { id: "resp", output: [], ...response } }],
  ]);

await test("OpenAI: an answer cut short tells its usage, and is still the error it was", async () => {
  const usage = { input_tokens: 2000, input_tokens_details: { cached_tokens: 1536 }, output_tokens: 4096, total_tokens: 6096 };
  const { fetch } = fakeFetch([openAiEnded("response.incomplete", { status: "incomplete", incomplete_details: { reason: "max_output_tokens" }, usage })]);
  const chat = startChat("openai", "k", openAiRequest, { fetch, maxRetries: 0 });
  const { told, handlers } = usages();
  const error = await chat.send("x", handlers, new AbortController().signal).then(() => null, (e) => e);
  assert.deepEqual(describeError(error), { kind: "failed", status: null, message: "The answer was cut short (max_output_tokens)." });
  assert.deepEqual(told, [{ input: 2000, cached: 1536, written: null, output: 4096 }]);
});

await test("OpenAI: a failed response tells its usage, and is still the error it was", async () => {
  const usage = { input_tokens: 700, input_tokens_details: { cached_tokens: 0 }, output_tokens: 30, total_tokens: 730 };
  const { fetch } = fakeFetch([openAiEnded("response.failed", { status: "failed", error: { code: "server_error", message: "The model crashed." }, usage })]);
  const chat = startChat("openai", "k", openAiRequest, { fetch, maxRetries: 0 });
  const { told, handlers } = usages();
  const error = await chat.send("x", handlers, new AbortController().signal).then(() => null, (e) => e);
  assert.deepEqual(describeError(error), { kind: "failed", status: null, message: "The model crashed." });
  assert.deepEqual(told, [{ input: 700, cached: 0, written: null, output: 30 }]);
});

if (failures > 0) {
  console.error(`${failures} of the assistant's provider tests failed`);
  process.exit(1);
}
console.log("The assistant's providers: all tests passed");
