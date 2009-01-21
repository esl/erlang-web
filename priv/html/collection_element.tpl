<div>
	<div id="<% name %>_main_node">
	     <script>
	     	var <% name %>_counter = 1;
			function addElement(divName) {
				var newDiv = document.createElement('div');
				newDiv.innerHTML = '<wpart:<% type %> name="<% name %>_' + (<% name %>_counter+1) 
						 + '" <% extra %> />';
		 		document.getElementById(divName).appendChild(newDiv);
		 		<% name %>_counter++;
			}
	     </script>
	<wpart:<% type %> name="<% name %>_1" <% extra %> />
	</div>

	<a href="#" onClick="addElement('<% name %>_main_node');">Add next</a>
</div>