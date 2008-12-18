<script type="text/javascript">

function lookupLocal(){
	var oSuggest = $("#<% slot %>")[0].autocompleter;

	oSuggest.findValue();

	return false;
}


$(document).ready(function() {


	$("#<% slot %>").autocompleteArray([<% slot %>],
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
