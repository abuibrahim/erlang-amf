dist_erl_DATA =	amf.erl	amf0.erl amf3.erl AbstractMessage.erl \
	AsyncMessage.erl CommandMessage.erl AcknowledgeMessage.erl
erldir = $(ERLANG_INSTALL_LIB_DIR_amf)/src
dist_hrl_DATA = amf.hrl
hrldir = $(ERLANG_INSTALL_LIB_DIR_amf)/include
beam_DATA = $(dist_erl_DATA:.erl=.beam) amf.app
beamdir = $(ERLANG_INSTALL_LIB_DIR_amf)/ebin
CLEANFILES = $(beam_DATA)
ERLCFLAGS += -W

$(beam_DATA): $(dist_hrl_DATA)

.erl.beam:
	@echo "Compiling $<..."
	@$(ERLC) $(ERLCFLAGS) $<
