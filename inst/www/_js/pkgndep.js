
var original_svg_width;
var original_svg_height;

$(function() {

	var svg_width = $('svg').width();
	var svg_height = $('svg').height();

	var main_width = $("#main").width();

	if(svg_width > main_width) {
		svg_height = svg_height * main_width/svg_width;
		svg_width = main_width;

		$('svg').width(svg_width);
		$('svg').height(svg_height);
	}

	$("#ht_container").width(svg_width);
	$("#ht_container").height(svg_height);
	$("#ht_container").css({"position":"relative"});

	original_svg_width = svg_width;
	original_svg_height = svg_height;

	var gap = 4;

	$('svg').mouseover(function(e) {
		var parentOffset = $(this).offset();
	    ht_x = e.pageX - parentOffset.left;
	    ht_y = e.pageY - parentOffset.top;

		var right_curser = document.createElement("div");
		right_curser.setAttribute("id", "ht_right_curser");
		$("#ht_container").append(right_curser);
		$('#ht_right_curser').css({'position': 'absolute', 'right': '0px', 'top': ht_y, 'width': Math.max(0, $(this).width() - ht_x - gap), 'height': '1px', 'background-color': 'grey'});

		var left_curser = document.createElement("div");
		left_curser.setAttribute("id", "ht_left_curser");
		$("#ht_container").append(left_curser);
		$('#ht_left_curser').css({'position': 'absolute', 'left': '0px', 'top': ht_y, 'width': Math.max(0, ht_x - gap), 'height': '1px', 'background-color': 'grey'});

		var top_curser = document.createElement("div");
		top_curser.setAttribute("id", "ht_top_curser");
		$("#ht_container").append(top_curser);
		$('#ht_top_curser').css({'position': 'absolute', 'left': ht_x, 'top': '0px', 'width':'1px', 'height': Math.max(0, ht_y - gap), 'background-color': 'grey'});

		var bottom_curser = document.createElement("div");
		bottom_curser.setAttribute("id", "ht_bottom_curser");
		$("#ht_container").append(bottom_curser);
		$('#ht_bottom_curser').css({'position': 'absolute', 'left': ht_x, 'bottom': '0px', 'width':'1px', 'height': Math.max(0, $(this).height() - ht_y - gap), 'background-color': 'grey'});

	}).mousemove(function(e) {
		var parentOffset = $(this).offset();
        ht_x = e.pageX - parentOffset.left;
        ht_y = e.pageY - parentOffset.top;

		$('#ht_right_curser').css({'right': '0px', 'top': ht_y, 'width': Math.max(0, $(this).width() - ht_x - gap), 'height': '1px'});
		$('#ht_left_curser').css({'left': '0px', 'top': ht_y, 'width': Math.max(0, ht_x - gap), 'height': '1px'});
		$('#ht_top_curser').css({'left': ht_x, 'top': '0px', 'width':'1px', 'height': Math.max(0, ht_y - gap)});
		$('#ht_bottom_curser').css({'left': ht_x, 'bottom': '0px', 'width':'1px', 'height': Math.max(0, $(this).height() - ht_y - gap)});

	}).mouseout(function(e) {
		$('#ht_right_curser').remove();
		$('#ht_left_curser').remove();
		$('#ht_top_curser').remove();
		$('#ht_bottom_curser').remove();
	});

});


function update_ajax_table(which_table, package, page, records_per_page = 20, other_param = "") {

	$("#"+ which_table + "_content").html("<p>Loading content...</p>");

	if(typeof(other_param) === "string") {
		if(other_param != "") {
			other_param = other_param + "&"
		}
	} else {
		keys = Object.keys(other_param);
		values = Object.values(other_param);
		other_param = ""
		for(var i = 0; i < keys.length; i ++) {
			other_param = other_param + "&" + keys[i] + "=" + values[i]
		}
	}

	var url = which_table + "?package=" + package + "&page=" + page + "&records_per_page=" + records_per_page + other_param;

	$.ajax({
	  url: url
	}).done(function(html) {
	  $("#"+ which_table + "_content").html(html);
	});
}

function increase_ht_size() {
	var svg_width = $("#ht_container").width();
	var svg_height = $("#ht_container").height();

	var svg_asp = svg_width/svg_height;

	svg_width = svg_width + 50;
	svg_height = svg_width/svg_asp;

	$('svg').width(svg_width);
	$('svg').height(svg_height);

	$("#ht_container").width(svg_width);
	$("#ht_container").height(svg_height);

}

function decrease_ht_size() {
	var svg_width = $("#ht_container").width();
	var svg_height = $("#ht_container").height();

	var svg_asp = svg_width/svg_height;

	svg_width = svg_width - 50;
	if(svg_width < 100) {
		svg_width = 100;
	}
	svg_height = svg_width/svg_asp;

	$('svg').width(svg_width);
	$('svg').height(svg_height);

	$("#ht_container").width(svg_width);
	$("#ht_container").height(svg_height);
}

function reset_ht_size() {
	
	$('svg').width(original_svg_width);
	$('svg').height(original_svg_height);
	
	$("#ht_container").width(original_svg_width);
	$("#ht_container").height(original_svg_height);
}


function show_description(package) {
	$.ajax({
	  url: "show_description?package="+package
	}).done(function(html) {
		html = "<div id='desc_ns'><div id='desc_ns_inner'><h3>DESCRIPTION file for <span class='package'>" + package + "</span></h3>\n" + html + "<p><button class='btn btn-default' type='submit' onclick='$(\"#desc_ns\").remove();'>Close</button><button class='btn btn-default' type='submit' onclick='$(\"#desc_ns\").remove();' style='position:absolute;top:6px;right:6px;'>Close</button></p></div></div>"
	  $("body").append(html);
	});
}

function show_namespace(package) {
	$.ajax({
	  url: "show_namespace?package="+package
	}).done(function(html) {
		html = "<div id='desc_ns'><div id='desc_ns_inner'><h3>NAMESPACE file for <span class='package'>" + package + "</h3>\n" + html + "</span><p><button class='btn btn-default' type='submit' onclick='$(\"#desc_ns\").remove();'>Close</button><button class='btn btn-default' type='submit' onclick='$(\"#desc_ns\").remove();' style='position:absolute;top:6px;right:6px;'>Close</button></p></div></div>"
	  $("body").append(html);

	});
}

function load_version_change() {
	html = "<div id='desc_ns'><div id='desc_ns_inner'><p style='font-size:1.2em;'>loading the new version of the heaviness database...</p><ul><li>Load heaviness database.</li><li>Load pre-calculated dependency results of all packages.</li><li>Load pre-calculated heaviness metrics of all packages.</li><li>Load DESCRIPTION and NAMESPACE of all packages.</li><li>Load downstream dependency paths.</li></ul><p><div class='lds-ellipsis'><div></div><div></div><div></div><div></div></div></p><button class='btn btn-default' type='submit' onclick='$(\"#desc_ns\").remove();'>Close</button></div></div>"
	$("body").append(html);
}


function compare_to_other_versions(package) {

	html = "<div id='desc_ns'><div id='desc_ns_inner2'><div class='lds-ellipsis'><div></div><div></div><div></div><div></div></div></div></div>";
	 $("body").append(html);

	$.ajax({
	  url: "compare_to_other_versions?package="+package
	}).done(function(html) {
	  html = "<h3>Compare <span class='package'>" + package + "</span> in all versions</h3>\n" + html + "<p><button class='btn btn-default' type='submit' onclick='$(\"#desc_ns\").remove();'>Close</button><button class='btn btn-default' type='submit' onclick='$(\"#desc_ns\").remove();' style='position:absolute;top:6px;right:6px;'>Close</button></p>"
	  $("#desc_ns_inner2").html(html);
	});
}
