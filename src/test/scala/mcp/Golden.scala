package mcp

/** The bytes each value in [[Sample]] has on the wire.
  *
  * Written by hand from the schema's field declarations — the schema file is a type declaration and
  * carries no example payloads, so there is nothing to lift — and then asserted in both directions
  * by the family suites. Larger vectors are composed from the smaller ones by interpolation; every
  * fragment is itself pinned here and golden-tested where it belongs, so nothing in this file is
  * derived from what the encoder happens to produce.
  *
  * Field order follows the rule the package documents: the members an interface inherits first, in
  * `extends` order, then its own; and for the content blocks, `type` ahead of everything.
  */
object Golden:

  val meta: String = """{"io.modelcontextprotocol/related-task":{"taskId":"t-1"},"trace":"abc"}"""

  val annotationsFull: String =
    """{"audience":["user","assistant"],"priority":0.5,"lastModified":"2025-01-12T15:00:58Z"}"""

  val iconLight: String =
    """{"src":"https://example.test/i.png","mimeType":"image/png","sizes":["48x48"],"theme":"light"}"""
  val iconDark: String = """{"src":"data:image/svg+xml;base64,PHN2Zy8+","theme":"dark"}"""
  val iconBare: String = """{"src":"https://example.test/bare.png"}"""

  val implementationMinimal: String = """{"name":"athame","version":"1.0.0"}"""
  val implementationFull: String =
    s"""{"name":"athame","title":"Athame","icons":[$iconLight,$iconDark],"version":"1.0.0","description":"A sync tool","websiteUrl":"https://example.test/"}"""

  // --- content -------------------------------------------------------------------------------

  val textMinimal: String = """{"type":"text","text":"hello"}"""
  val textFull: String = s"""{"type":"text","text":"hello","annotations":$annotationsFull,"_meta":$meta}"""
  val image: String = """{"type":"image","data":"aGk=","mimeType":"image/png","annotations":{}}"""
  val audio: String = s"""{"type":"audio","data":"aGk=","mimeType":"audio/wav","_meta":$meta}"""

  val resourceMinimal: String = """{"name":"readme","uri":"file:///readme.md"}"""
  val resourceFull: String =
    s"""{"name":"readme","title":"README","icons":[$iconBare],"uri":"file:///readme.md","description":"The readme","mimeType":"text/markdown","annotations":$annotationsFull,"size":1024,"_meta":$meta}"""

  val resourceLink: String =
    s"""{"type":"resource_link","name":"readme","title":"README","icons":[$iconBare],"uri":"file:///readme.md","description":"The readme","mimeType":"text/markdown","annotations":$annotationsFull,"size":1024,"_meta":$meta}"""
  val resourceLinkMinimal: String =
    """{"type":"resource_link","name":"readme","uri":"file:///readme.md"}"""

  val textContents: String = """{"uri":"file:///readme.md","mimeType":"text/markdown","text":"# readme"}"""
  val blobContents: String =
    s"""{"uri":"file:///logo.png","mimeType":"image/png","_meta":$meta,"blob":"aGk="}"""

  val embeddedText: String = s"""{"type":"resource","resource":$textContents}"""
  val embeddedBlob: String =
    s"""{"type":"resource","resource":$blobContents,"annotations":$annotationsFull,"_meta":$meta}"""

  val toolUse: String =
    s"""{"type":"tool_use","id":"call_1","name":"search","input":{"q":"scala"},"_meta":$meta}"""
  val toolResult: String =
    s"""{"type":"tool_result","toolUseId":"call_1","content":[$textMinimal,$resourceLink],"structuredContent":{"hits":3},"isError":false}"""

  val contentBlocks: String =
    s"[$textMinimal,$textFull,$image,$audio,$resourceLink,$resourceLinkMinimal,$embeddedText,$embeddedBlob]"

  val samplingBlocks: String = s"[$textMinimal,$textFull,$image,$audio,$toolUse,$toolResult]"

  // --- tools ---------------------------------------------------------------------------------

  val inputSchema: String =
    """{"$schema":"https://json-schema.org/draft/2020-12/schema","type":"object","properties":{"q":{"type":"string"}},"required":["q"]}"""
  val outputSchema: String = """{"type":"object","properties":{"hits":{"type":"number"}}}"""

  val toolMinimal: String = """{"name":"search","inputSchema":{"type":"object"}}"""
  val toolFull: String =
    s"""{"name":"search","title":"Search","icons":[$iconBare],"description":"Searches things","inputSchema":$inputSchema,"execution":{"taskSupport":"optional"},"outputSchema":$outputSchema,"annotations":{"title":"Search","readOnlyHint":true,"destructiveHint":false,"idempotentHint":true,"openWorldHint":false},"_meta":$meta}"""

  val listToolsMinimal: String = """{"tools":[]}"""
  val listToolsFull: String =
    s"""{"_meta":$meta,"nextCursor":"cursor-2","tools":[$toolMinimal,$toolFull]}"""

  val callToolMinimal: String = """{"name":"search"}"""
  val callToolFull: String =
    s"""{"_meta":$meta,"task":{"ttl":60000},"name":"search","arguments":{"q":"scala","limit":10}}"""

  val callToolResultMinimal: String = """{"content":[]}"""
  val callToolResultFull: String =
    s"""{"_meta":$meta,"content":$contentBlocks,"structuredContent":{"hits":3},"isError":true}"""

  // --- lifecycle -----------------------------------------------------------------------------

  val clientCapabilitiesFull: String =
    """{"experimental":{"x.custom":{}},"roots":{"listChanged":true},"sampling":{"context":{},"tools":{}},"elicitation":{"form":{},"url":{}},"tasks":{"list":{},"cancel":{},"requests":{"sampling":{"createMessage":{}},"elicitation":{"create":{}}}}}"""

  val serverCapabilitiesFull: String =
    """{"experimental":{"x.custom":{}},"logging":{},"completions":{},"prompts":{"listChanged":false},"resources":{"subscribe":true,"listChanged":true},"tools":{"listChanged":true},"tasks":{"list":{},"cancel":{},"requests":{"tools":{"call":{}}}}}"""

  val initializeMinimal: String =
    s"""{"protocolVersion":"2025-11-25","capabilities":{},"clientInfo":$implementationMinimal}"""
  val initializeFull: String =
    s"""{"_meta":$meta,"protocolVersion":"2025-11-25","capabilities":$clientCapabilitiesFull,"clientInfo":$implementationFull}"""

  val initializeResultMinimal: String =
    s"""{"protocolVersion":"2025-11-25","capabilities":{},"serverInfo":$implementationMinimal}"""
  val initializeResultFull: String =
    s"""{"_meta":$meta,"protocolVersion":"2025-11-25","capabilities":$serverCapabilitiesFull,"serverInfo":$implementationFull,"instructions":"Use the search tool."}"""

  // --- sampling ------------------------------------------------------------------------------

  val samplingMessage: String = s"""{"role":"user","content":[$textMinimal]}"""
  val samplingMessageFull: String =
    s"""{"role":"assistant","content":$samplingBlocks,"_meta":$meta}"""

  val createMessageMinimal: String = s"""{"messages":[$samplingMessage],"maxTokens":1024}"""
  val createMessageFull: String =
    s"""{"_meta":$meta,"task":{"ttl":30000},"messages":[$samplingMessage,$samplingMessageFull],"modelPreferences":{"hints":[{"name":"sonnet"},{}],"costPriority":0.1,"speedPriority":0.2,"intelligencePriority":0.9},"systemPrompt":"Be brief.","includeContext":"thisServer","temperature":0.7,"maxTokens":1024,"stopSequences":["STOP"],"metadata":{"provider":"x"},"tools":[$toolMinimal],"toolChoice":{"mode":"required"}}"""

  val createMessageResultMinimal: String =
    s"""{"role":"assistant","content":[$textMinimal],"model":"claude-x"}"""
  val createMessageResultFull: String =
    s"""{"_meta":$meta,"role":"assistant","content":$samplingBlocks,"model":"claude-x","stopReason":"toolUse"}"""

  // --- elicitation ---------------------------------------------------------------------------

  val stringSchema: String =
    """{"type":"string","title":"Name","description":"Your name","minLength":1,"maxLength":64,"format":"email","default":"a@b.test"}"""
  val stringSchemaMinimal: String = """{"type":"string"}"""
  val numberSchema: String =
    """{"type":"number","title":"Ratio","minimum":0,"maximum":1,"default":0.5}"""
  val integerSchema: String = """{"type":"integer"}"""
  val booleanSchema: String = """{"type":"boolean","title":"Agree","default":false}"""
  val untitledSingle: String =
    """{"type":"string","title":"Colour","enum":["red","green"],"default":"red"}"""
  val titledSingle: String =
    """{"type":"string","description":"Pick one","oneOf":[{"const":"r","title":"Red"},{"const":"g","title":"Green"}]}"""
  val legacyTitled: String =
    """{"type":"string","enum":["r","g"],"enumNames":["Red","Green"],"default":"r"}"""
  val untitledMulti: String =
    """{"type":"array","title":"Tags","minItems":1,"maxItems":3,"items":{"type":"string","enum":["a","b"]},"default":["a"]}"""
  val titledMulti: String = """{"type":"array","items":{"anyOf":[{"const":"a","title":"Alpha"}]}}"""

  val requestedSchema: String =
    s"""{"$$schema":"https://json-schema.org/draft/2020-12/schema","type":"object","properties":{"name":$stringSchema,"colour":$untitledSingle},"required":["name"]}"""

  val elicitForm: String =
    s"""{"mode":"form","message":"Who are you?","requestedSchema":$requestedSchema}"""
  val elicitUrl: String =
    """{"mode":"url","message":"Sign in","elicitationId":"e-1","url":"https://example.test/auth"}"""
  val elicitUrlFull: String =
    s"""{"_meta":$meta,"task":{},"mode":"url","message":"Sign in","elicitationId":"e-2","url":"https://example.test/auth"}"""

  val elicitResultMinimal: String = """{"action":"cancel"}"""
  val elicitResultFull: String =
    s"""{"_meta":$meta,"action":"accept","content":{"name":"ada","colour":"red"}}"""

  val elicitationComplete: String = """{"elicitationId":"e-1"}"""

  // --- utility -------------------------------------------------------------------------------

  val cancelledMinimal: String = """{}"""
  val cancelledFull: String = s"""{"_meta":$meta,"requestId":7,"reason":"user asked"}"""
  val progressMinimal: String = """{"progressToken":"p-1","progress":1}"""
  val progressFull: String =
    s"""{"_meta":$meta,"progressToken":42,"progress":0.5,"total":1,"message":"halfway"}"""

  val loggingMinimal: String = """{"level":"debug","data":null}"""
  val loggingFull: String =
    s"""{"_meta":$meta,"level":"emergency","logger":"db","data":{"message":"disk full"}}"""

  val setLevel: String = """{"level":"warning"}"""

  val emptyResult: String = """{}"""
