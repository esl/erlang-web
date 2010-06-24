<script type="text/javascript">

function lookupLocal(){
	var oSuggest = $("#<% name %>").autocompleter;

	oSuggest.findValue();

	return false;
}


$(document).ready(function() {


	$("#<% name %>").autocompleteArray([<% complete %>],
		{
			delay:1,
			minChars:0,
			matchSubset:0,
                        matchCase:0,    
			autoFill:true,
			maxItemsToShow:40
		}
	);
});

</script>
