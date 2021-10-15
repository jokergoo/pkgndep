

library(knitr)

network_in_json = function(g) {
	node = unique(c(g[, 1], g[, 2]))
	edge = g
	node_lt = lapply(node, function(x) list(data = list(id = x)))
	edge_lt = lapply(seq_len(nrow(edge)), function(i) {
		list(data = list(id = paste(edge[i, 1], edge[i, 2], sep = "|"),
			source = edge[i, 1],
			target = edge[i, 2],
			weight = g[i, 3]))
	})
	nt_json = list(nodes = node_lt, edges = edge_lt)
	nt_json = paste0("var nt = ", rjson::toJSON(nt_json), ";\n")
	return(nt_json)
}

js_network = function(g, id = "nt") {

	if(nrow(g) == 0) return("")
	
qq("
<style>
#@{id} {
	width: 800px;
	height: 400px;
	border: 1px solid #ddd;
}
</style>
<div id='@{id}'></div>
<p><a href='#' id='reset'>Reset layout</a></p>

<script src='../_js/cytoscape.min.js'></script>
	<script src='../_js/dagre.min.js'></script>
	<script src='../_js/cytoscape-dagre.js'></script>
	<script type='text/javascript'>

@{network_in_json(g)}

	var cy = cytoscape({
		container: document.getElementById('@{id}'),
		boxSelectionEnabled: false,
        autounselectify: true,

		elements: nt,
		layout: {
			name: 'dagre',
			nodeSep: 5,
			edgeSep: 0,
			rankSep: 100,
			rankDir: 'LR'
		},
		style: [
			{
				selector: 'node',
				style: {
					'content': 'data(id)',
					'text-opacity': 0.5,
					'text-valign': 'bottom',
					'text-halign': 'center',
					'background-color': '#11479e'
				}
			},

			{
				selector: 'edge',
				style: {
					'width': 4,
					'target-arrow-shape': 'triangle',
					'line-color': '#9dbaea',
					'target-arrow-color': '#9dbaea',
					'content': 'data(weight)',
				}
			}
		]
	});

	cy.$('node').one('click', function(e){
	  var ele = e.target;
	  window.open(ele.id()+'.html')
	});

	$('#reset').click(function() {
		cy.reset();
	});
</script>

")
}


html_single_package = function(pkg) {

	css = '
body {
    font-family: -apple-system,BlinkMacSystemFont,"Segoe UI",Helvetica,Arial,sans-serif,"Apple Color Emoji","Segoe UI Emoji","Segoe UI Symbol";
    font-size: medium;
    line-height: 1.5;
    color: #24292e;
    background-color: #fff;
}
table {
    border-spacing: 0;
    border-collapse: collapse;
}
td, tr, th {
	border: 1px solid #EEEEEE;
}
td, th {
    padding: 6px 13px;
    border: 1px solid #dfe2e5;
}
'

	con = file(qq("_html/@{pkg$package}.html"), "w")
	writeLines(qq("<html>
<head>
<title>Dependency heatmap for package '@{pkg$package}'</title>
<link rel='stylesheet' href='../_css/jquery-ui.min.css'>
<script src='../_js/jquery.min.js'></script>
<script src='../_js/jquery-ui.min.js'></script>
<style>
@{css}
</style>
<script>
  $( function() {
    $('#nt').resizable();
  } );
</script>
</head>
<body>
"), con = con)
	writeLines(qq("<h2>Dependency heatmap for package '@{pkg$package}'</h2>"), con = con)
	if(pkg$bioc) {
		writeLines(qq("<p>Bioconductor link: <a href='https://bioconductor.org/packages/@{pkg$package}/' target='_blank'>@{pkg$package}</a><p>"), con = con)
	} else {
		writeLines(qq("<p>CRAN link: <a href='https://CRAN.R-project.org/package=@{pkg$package}/' target='_blank'>@{pkg$package}</a><p>"), con = con)
	}

	writeLines(readLines(qq("_image/@{pkg$package}.svg")), con = con)
	writeLines("<br><br>", con = con)
	if(!is.null(pkg$df_imports)) {
		loaded_ns = apply(pkg$m, 1, function(x) sum(!is.na(x)))
		row_order = order(pkg$pkg_category, loaded_ns)
		tb = as.data.frame(pkg$df_imports)
		tb = cbind(category = pkg$pkg_category, tb)
		tb$loaded_ns = apply(pkg$m, 1, function(x) sum(!is.na(x)))
		tb$heaviness = pkg$heaviness

		tb = tb[row_order, , drop = FALSE]
		tb = cbind("Package"= rownames(tb), tb)
		tb[, 1] = qq("<a href='@{tb[, 1]}.html'>@{tb[, 1]}</a>", collapse = FALSE)
		tb = as.matrix(tb)

		html = as.character(kable(tb, format = "html", row.names = FALSE, escape = FALSE, col.names = c("Package", "Category", "imports", "importMethods", "importClasses", "Loaded namespeces", "Heaviness")))
		html = gsub("(<td[^>]*?> Suggests </td>\\s+)<td[^>]*?> 0 </td>\\s+<td[^>]*?> 0 </td>\\s+<td[^>]*?> 0 </td>\\s+", "\\1<td colspan=3>Namespace is not imported.</td>\n", html)
		html = gsub("(<td[^>]*?> Suggests or\nEnhances </td>\\s+)<td[^>]*?> 0 </td>\\s+<td[^>]*?> 0 </td>\\s+<td[^>]*?> 0 </td>\\s+", "\\1<td colspan=3>Namespace is not imported.</td>\n", html)
		html = gsub("(<td[^>]*?> Depends </td>\\s+)<td[^>]*?> 0 </td>\\s+<td[^>]*?> 0 </td>\\s+<td[^>]*?> 0 </td>\\s+", "\\1<td colspan=3>The whole namespace is imported.</td>\n", html)
		html = gsub("(<td[^>]*?> Depends </td>\\s+)<td[^>]*?> -(\\d+) </td>\\s+<td[^>]*?> 0 </td>\\s+<td[^>]*?> 0 </td>\\s+", "\\1<td colspan=3>The whole namespace excluding \\2 objects is imported.</td>\n", html)
		html = gsub("(<td[^>]*?> Depends </td>\\s+)<td[^>]*?> -Inf </td>\\s+<td[^>]*?> 0 </td>\\s+<td[^>]*?> 0 </td>\\s+", "\\1<td colspan=3>Package is listed in 'Depends' but no object from the namespace is imported.</td>\n", html)
		html = gsub("(<td[^>]*?> Imports </td>\\s+)<td[^>]*?> 0 </td>\\s+<td[^>]*?> 0 </td>\\s+<td[^>]*?> 0 </td>\\s+", "\\1<td colspan=3>The whole namespace is imported.</td>\n", html)
		html = gsub("(<td[^>]*?> Imports </td>\\s+)<td[^>]*?> -(\\d+) </td>\\s+<td[^>]*?> 0 </td>\\s+<td[^>]*?> 0 </td>\\s+", "\\1<td colspan=3>The whole namespace excluding \\2 objects is imported.</td>\n", html)
		html = gsub("(<td[^>]*?> Imports </td>\\s+)<td[^>]*?> -Inf </td>\\s+<td[^>]*?> 0 </td>\\s+<td[^>]*?> 0 </td>\\s+", "\\1<td colspan=3>Package is listed in 'Imports' but no object from the namespace is imported.</td>\n", html)

		writeLines("<p><b>Table of dependency pacakges:</b></p>", con = con)
		writeLines(html, con = con)
		writeLines("<br>
<p><b>imports</b>: variables/functions listed in <code>import</code>/<code>importFrom</code> directive.</p>
<p><b>importMethods</b>: S4 methods listed in <code>importMethodFrom</code> directive.</p>
<p><b>importClasses</b>: S4 classes listed in <code>importClassesFrom</code> directive.</p>
", con = con)

		writeLines(js_network(nt_heaviness(pkg$package), id = "nt"), con = con)

		pp = pkg$package
		l = nt$dep == pp & nt$category %in% c("Depends", "Imports")
		if(any(l)) {
			rev_pkg = nt[l, "pkg"]
			rev_cate = nt[l, "category"]

			tb = data.frame(Package = rev_pkg, category = rev_cate)
			tb$import = sapply(lt[rev_pkg], function(pkg) {
				pkg$df_imports[pp, "imports"]
			})
			tb$importMethods = sapply(lt[rev_pkg], function(pkg) {
				pkg$df_imports[pp, "importMethods"]
			})
			tb$importClasses = sapply(lt[rev_pkg], function(pkg) {
				pkg$df_imports[pp, "importClasses"]
			})
			tb$heaviness = sapply(lt[rev_pkg], function(pkg) {
				pkg$heaviness[pp]
			})

			tb$Package = qq("<a href='@{tb$Package}.html'>@{tb$Package}</a>", collapse = FALSE)
			tb$category = paste0("Reverse ", tb$category)
			tb = tb[order(tb$category, tb$Package), , drop = FALSE]

			html = as.character(kable(tb, format = "html", row.names = FALSE, escape = FALSE, col.names = c("Package", "Category", "import", "importMethods", "importClasses", "Heaviness")))
			html = gsub("(<td[^>]*?> Reverse Depends </td>\\s+)<td[^>]*?> 0 </td>\\s+<td[^>]*?> 0 </td>\\s+<td[^>]*?> 0 </td>\\s+", qq("\\1<td colspan=3>The whole namespace of '@{pp}' is imported.</td>\n"), html)
			html = gsub("(<td[^>]*?> Reverse Imports </td>\\s+)<td[^>]*?> 0 </td>\\s+<td[^>]*?> 0 </td>\\s+<td[^>]*?> 0 </td>\\s+", qq("\\1<td colspan=3>The whole namespace of '@{pp}' is imported.</td>\n"), html)
		
			writeLines("<p><b>Table of reverse dependency pacakges:</b></p>", con = con)
			writeLines(html, con = con)
		}
	}
	writeLines("<br><br>
</body>
</html>
", con = con)
	close(con)
}


