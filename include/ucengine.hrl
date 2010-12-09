%% inherited from ucengine api

-record(uce_event, {
          %% Id
          id = none,
          % date (ms from epoch)
          datetime = none,
          %% location = [Org,Meeting]
          location = [],
          %% From: uid|brick
          from,
          %% Type event : list 
          type,
          %% parent id
          parent = "",
	  %% to id
	  to = "all",
          %% MetaData : list
          metadata = []}).

-define(UCE_MEETING_JOIN_EVENT, "internal.meeting.join").
-define(UCE_MEETING_LEAVE_EVENT, "internal.meeting.leave").
-define(UCE_STREAM_NEW_EVENT, "video.stream.new").
-define(UCE_STREAM_STOP_EVENT, "video.stream.stop").
-define(UCE_STREAM_START_EVENT, "video.stream.start").

-define(UCE_API_VERSION, "0.1").

