%% -*-Erlang-*-
{application, amf,
 [{description, "Action Message Format Library"},
  {vsn, "1.2.0"},
  {modules, [amf, amf0, amf3, amf_AbstractMessage, amf_AsyncMessage,
	     amf_CommandMessage, amf_AcknowledgeMessage]},
  {registered, []},
  {applications, [kernel, stdlib]},
  {env, []}
 ]}.
%% vim: set filetype=erlang:
