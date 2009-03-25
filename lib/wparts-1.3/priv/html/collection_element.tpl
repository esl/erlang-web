<div>
	<% editing_elements %>

	<div id="<% name %>_main_node">
	     <script>
	     	var <% name %>_counter = <% counter_start %>;
			function addElement(divName) {
				var newDiv = document.createElement('div');
				newDiv.innerHTML = '<wpart:<% type %> name="<% name %>_' + (<% name %>_counter+1) 
						 + '" <% extra %> />';
		 		document.getElementById(divName).appendChild(newDiv);
		 		<% name %>_counter++;
			}
	     </script>

	<% input_element %>
	</div>

	<a href="#" onClick="addElement('<% name %>_main_node');">Add next</a>	
</div>