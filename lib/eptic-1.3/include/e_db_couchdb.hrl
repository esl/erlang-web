-define(CDB_READ_ALL_MAP(Prefix),
	"function(doc) {"
	"  if(doc._id.indexOf('" ++ Prefix ++ "') == 0) {"
	"    emit(doc, null);"
	"  }"
	"}").
