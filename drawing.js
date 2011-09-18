diego = {};
diego.drawing = {};

(function() {
    var ns = diego.drawing;

    var context = function(paper, width, height) {
	this.lanes = {};

	this.topOffset = 20;

	this.heightRatio = (height / 20.0);
	this.widthRatio = (width / 10.0);

	this.shapes = [];

	this.paper = paper;

	this.initLine = function(pos, text) {
	    console.log('Line at' + pos);
	    pos = Math.round(pos);
	    var pcoords = [["M", 20, pos], ["L", width, pos]];

	    console.log(pcoords);

	    var path = this.paper.path(pcoords)
		.attr({stroke: '#555', 
		       "stroke-linecap": "round", 
		       opacity: 0.7, 
		       "stroke-width": 2, 
		       "stroke-dasharray": "."});
	    var text = this.paper.text(10, pos, text.toString())
		.attr({fill: "#000", "font-size": 10});
	};
	
	this.initBack = function() {
	    var currPos = 0.0;
	    var currPosInRatio = currPos * this.heightRatio;

	    while (currPosInRatio <= height) {
		this.initLine(currPosInRatio + this.topOffset, currPos);
		currPos += 1.0;
		currPosInRatio = currPos * this.heightRatio;
	    }
	};

	this.initBack();

	this.clearAllShapes = function() {
	    for (idx in this.shapes) {
	        this.shapes[idx].remove();
	    };

	    this.shapes = [];
	};

	this.getYPosFromTime = function(time) {
	    return time * this.heightRatio + this.topOffset;
	};

	this.getXPosFromLaneIndex = function(lane) {
	    return lane * this.widthRatio + this.topOffset;
	};

	this.getLengthFromDuration = function(duration) {
	    return duration * this.heightRatio;
	};

	this.createPath = function(xpos, ypos, len, pathWidth) {
	    var st = this.paper.set();
	    var pcoords = [["M", xpos, ypos], ["L", xpos, ypos + len]];
	    var path = this.paper.path(pcoords)
		//.attr({stroke: "#444"})
		.attr({stroke: Raphael.getColor(), 
		       "stroke-linecap": "round", 
		       opacity: 1, 
		       "stroke-width": pathWidth});
	    

	    path.attr({fill: Raphael.getColor()});

	    var nodeHead1 = this.paper.circle(xpos, ypos, pathWidth)
	    	.attr({fill: '#222'});
	    var nodeHead2 = this.paper.circle(xpos, ypos + len, pathWidth)
	    	.attr({fill: '#222'});

	    st.push(path);
	    st.push(nodeHead1);
	    st.push(nodeHead2);
	    return st;
	};

	this.createShape = function(lane, time, duration) {
	    var st = this.paper.set();

	    var xpos = this.getXPosFromLaneIndex(lane);
	    var ypos = this.getYPosFromTime(time);
	    var len = this.getLengthFromDuration(duration);

	    var circle = this.paper.circle(xpos, ypos, len);
	    circle.attr({fill: Raphael.getColor()});
	    circle.attr({opacity: 0.05});

	    var text = this.createPath(xpos, ypos, len, 3);

	    st.push(circle);
	    st.push(text);

	    this.shapes.push(st);

	    return st;
	};
    };

    ns.update = function(ctx) {
	var updateElements = function(data) {
	    ctx.clearAllShapes();
	    var values = data;

	    for (idx in values) {
		var val = values[idx];
		ctx.createShape(val.lane, val.time, val.length);
	    }
	};

	jQuery.ajax({
	    url: 'data.json',
	    success: function(data) {
		updateElements(data);
	    },
	    error: function(jqXHR, textStatus, errorThrown) {
		alert(errorThrown);
	    },
	    parserError: function() {
		alert('failed to parse');
	    }
	});
    };

    ns.init = function(div, width, height) {
	var paper = Raphael(div, width, height);
	return new context(paper, width, height);
    };
})();