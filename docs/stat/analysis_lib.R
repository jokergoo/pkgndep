

library(knitr)
library(igraph)

nt_heaviness = function(package, type = "end") {

	pkg = lt[[package]]
		
	if(type == "end") {
		v = pkg$heaviness[pkg$pkg_category %in% c("Depends", "Imports")]
		v = v[v > 0]

		pp = names(v)
		tb = data.frame(dep = pp, package = rep(package, length(v)), heaviness = v)

		while(1) {
			pp2 = NULL
			for(p in pp) {
				pkg = lt[[p]]
				v = pkg$heaviness[pkg$pkg_category %in% c("Depends", "Imports")]
				l = !(names(v) %in% tb$dep & p %in% tb$package) & v > 0
				v = v[l]
				if(length(v)) {
					tb = rbind(tb, data.frame(dep = names(v), package = rep(p, length(v)), heaviness = v))
					pp2 = c(pp2, names(v))
				}
			}
			if(length(pp2) == 0) {
				break
			}
			pp = pp2
		}
		tb
	} else {
		l = nt$dep == pkg$package & nt$category %in% c("Depends", "Imports")
		
		rev_pkg = nt[l, "pkg"]

		v = sapply(lt[rev_pkg], function(pkg) {
			unname(pkg$heaviness[package])
		})
		v = v[v > 0]

		pp = names(v)
		tb = data.frame(dep = rep(package, length(v)), package = pp, heaviness = v)

		while(1) {
			pp2 = NULL
			for(p in pp) {
				l = nt$dep == p & nt$category %in% c("Depends", "Imports")
				rev_pkg = nt[l, "pkg"]
				if(length(rev_pkg) == 0) next
				v = sapply(lt[rev_pkg], function(pkg) {
					unname(pkg$heaviness[p])
				})
				v = v[v > 0 & !(names(v) %in% tb$package & p %in% tb$dep)] 

				if(length(v)) {
					tb = rbind(tb, data.frame(dep = rep(p, length(v)), package = names(v), heaviness = v))
					pp2 = c(pp2, names(v))
				}
			}
			if(length(pp2) == 0) {
				break
			}
			pp = pp2
		}
		tb
	}
}




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

		html = as.character(kable(tb, format = "html", row.names = FALSE, escape = FALSE, col.names = c("Package", "Category", "import", "importMethods", "importClasses", "Loaded namespeces", "Heaviness")))
		html = gsub("(<td[^>]*?> Suggests </td>\\s+)<td[^>]*?> 0 </td>\\s+<td[^>]*?> 0 </td>\\s+<td[^>]*?> 0 </td>\\s+", "\\1<td colspan=3>Namespace is not imported.</td>\n", html)
		html = gsub("(<td[^>]*?> Suggests or\nEnhances </td>\\s+)<td[^>]*?> 0 </td>\\s+<td[^>]*?> 0 </td>\\s+<td[^>]*?> 0 </td>\\s+", "\\1<td colspan=3>Namespace is not imported.</td>\n", html)
		html = gsub("(<td[^>]*?> Depends </td>\\s+)<td[^>]*?> 0 </td>\\s+<td[^>]*?> 0 </td>\\s+<td[^>]*?> 0 </td>\\s+", "\\1<td colspan=3>The whole namespace is imported.</td>\n", html)
		html = gsub("(<td[^>]*?> Imports </td>\\s+)<td[^>]*?> 0 </td>\\s+<td[^>]*?> 0 </td>\\s+<td[^>]*?> 0 </td>\\s+", "\\1<td colspan=3>The whole namespace is imported.</td>\n", html)

		writeLines("<p><b>Table of dependency pacakges:</b></p>", con = con)
		writeLines(html, con = con)
		writeLines("<br>
<p><b>import</b>: variables/functions listed in <code>import</code>/<code>importFrom</code> directive.</p>
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
				pkg$df_imports[pp, "import"]
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


gini_index = function(pkg, a = 2) {
	l = pkg$pkg_category %in% c("Imports", "Depends")
	if(sum(l) < 2) {
		0
	} else {
		DescTools::Gini(pkg$heaviness[l] + a)
	}
}


adjust_dep = function(package = NULL) {
	lt2 = lt
	# assign value
	for(i in seq_along(lt2)) {
		if(is.null(package)) { 
			pkg = lt2[[i]]
			m = pkg$m[pkg$pkg_category %in% c("Imports", "Depends"), , drop = FALSE]
			x = rownames(m)

			if(length(x)) {
				v = heaviness(pkg, FALSE)
				v = v[x]
				move = rownames(m) == x[which.max(v)]
				qqcat("package '@{pkg$package}': move '@{x[which.max(v)]}' to Suggesets\n")
				pkg$pkg_category[move] = "Suggests"
				lt2[[i]] = pkg
			}
		} else {
			pkg = lt2[[i]]
			m = pkg$m[pkg$pkg_category %in% c("Imports", "Depends"), , drop = FALSE]
			x = rownames(m)

			if(package %in% x) {
				move = rownames(m) == package
				qqcat("package '@{pkg$package}': move '@{package}' to Suggesets\n")
				pkg$pkg_category[move] = "Suggests"
				lt2[[i]] = pkg
			}
		}
	}

	prev_hash = ""
	round = 0
	while(1) {
		round = round + 1
		# now adjust m and n_by_imports_suggests
		imp_lt = lapply(lt2, function(pkg) {
			m = pkg$m[pkg$pkg_category %in% c("Imports", "Depends"), , drop = FALSE]
			colnames(m)[apply(m, 2, function(x) any(!is.na(x)))]
		})

		hash = digest::digest(imp_lt)
		if(prev_hash == hash) {
			break
		} else {
			prev_hash = hash
		}

		for(i in seq_along(lt2)) {
			pkg = lt2[[i]]
			m = pkg$m[pkg$pkg_category %in% c("Imports", "Depends"), , drop = FALSE]

			for(nm in rownames(m)) {
				l = colnames(pkg$m) %in% imp_lt[[nm]] & !is.na(pkg$m[nm, ])
				if(any(l)) {
					qqcat("round @{round} @{pkg$package} (i = @{i}): removed @{sum(l)} namespaces for '@{nm}'\n")
					pkg$m[nm, l] = NA
				}	
			}
			
			n_by_depends_imports = sum(apply(pkg$m[pkg$pkg_category %in% c("Depends", "Imports"), , drop = FALSE], 2, function(x) any(!is.na(x))))
			if(n_by_depends_imports != pkg$n_by_depends_imports) {
				qqcat("  - @{pkg$package}: n_by_depends_imports has been adjusted from @{pkg$n_by_depends_imports} to @{n_by_depends_imports}\n")
			}
			pkg$n_by_depends_imports = n_by_depends_imports
			lt2[[i]] = pkg
		}
	}

	return(lt2)
}


