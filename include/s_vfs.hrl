-record(s_vfs_file,
	{filename,      % string
	 content,      % binary
	 meta           % #s_meta
	}).

-record(s_meta,
	{mtime,	        % The local time the file was last written.
	 mimetype,      % MIME-TYPE
	 size           % size of the content
	}).
