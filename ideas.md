+ Color by shared locality (ecology)
+ For ages, calculate locality based on being at that age.
+ Allow user to actually enter age as a number
+ export csv or similar formats, in particular what format does gephi/cytoscape want??
	+ gephi website supported formats
+ allow user to give own minerals and contextualize with rruff
+ BUGS: 
	+ issue with node label color when color by cluster is on. it defaults to the skyblue.
	+ select all elements is fucked up
	```
	Warning: Error in unique: object 'rruff_redox_states' not found
  80: unique [build_network.R#13]
  79: initialize_data [build_network.R#13]
  78: <reactive:chemistry_network> [/Users/spielman/Software/mcnet/server.R#107]
  62: chemistry_network
  61: <reactive:style_network> [/Users/spielman/Software/mcnet/server.R#119]
  45: style_network
  44: <observer> [/Users/spielman/Software/mcnet/server.R#295]
   1: runApp
   ```