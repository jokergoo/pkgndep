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

	var gap = 6;

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


function update_ajax_table(which_table, package, page) {
	$.ajax({
	  url: which_table + "?package=" + package + "&page=" + page
	}).done(function(html) {
	  $("#"+ which_table + "_content").html(html);
	});
}
