-ifndef(_AMF).
-define(_AMF, true).

-record(amf_packet, {version, headers, messages}).
-record(amf_header, {name, must_understand, body}).
-record(amf_message, {target, response, body}).
-record(amf_object, {class = <<>>, members = []}).

-endif.
