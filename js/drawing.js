diego = {};
diego.drawing = {};

//TODO: Move away
String.prototype.lpad = function(padString, length) {
	var str = this;
    while (str.length < length)
        str = padString + str;
    return str;
};
 
String.prototype.rpad = function(padString, length) {
	var str = this;
    while (str.length < length)
        str = str + padString;
    return str;
};

(function() {
    var ns = diego.drawing;

    var context = function(paper, width, height, dt, updateUrl) {

	this.parseDate = function(dateString) {
	    console.log('Parsing ' + dateString);
	    var year = parseInt(dateString.substring(0, 4))
	    var month = parseInt(dateString.substring(5, 7));
	    var day = parseInt(dateString.substring(8, 10));

	    var hour = parseInt(dateString.substring(11, 13));
	    var minute = parseInt(dateString.substring(14, 16));

	    var dt = new Date(year, month, day, hour, minute, 0, 0);

	    console.log('Parsed date: ' + dt.toString());
	    return dt;
	};

	this.lanes = {};

	this.dt = dt;

	this.updateUrl = updateUrl;

	this.endDate = new Date(this.dt.valueOf());
	this.endDate.setDate(this.endDate.getDate() + 1);

	this.totalTime = this.endDate.valueOf() - this.dt.valueOf();

	this.topOffset = 20;
	this.leftOffset = 50;

	this.widthRatio = (width / 40.0);

	this.shapes = [];

	this.paper = paper;

	this.projectPosition = function(offset) {
	    console.log('Projecting position ' + offset + ' against total time ' + this.totalTime);
	    return ((offset * 1.0) / this.totalTime) * height;
	}

	this.getRelativePosition = function(time) {
	    var offset = time.valueOf() - this.dt.valueOf();
	    return this.projectPosition(offset) + this.topOffset;
	};

	this.initLine = function(time) {
	    var text = time.getHours().toString().lpad('0', 2) +
		':' + time.getMinutes().toString().lpad('0', 2);
	    
	    var pos = this.getRelativePosition(time);
	    
	    var pcoords = [["M", 40, pos], ["L", width, pos]];

	    console.log(pcoords);

	    var path = this.paper.path(pcoords)
		.attr({stroke: '#555', 
		       "stroke-linecap": "round", 
		       opacity: 0.7, 
		       "stroke-width": 2, 
		       "stroke-dasharray": "."});
	    var text = this.paper.text(20, pos, text)
		.attr({fill: "#000", "font-size": 10});
	};
	
	this.initBack = function() {
	    var currTime = new Date(this.dt.valueOf());
	    var limit = new Date(this.dt.valueOf());
	    limit.setDate(this.dt.getDate() + 1);
	    console.log('writing background lines');
	    while (currTime <= limit) {
		console.log(currTime.toString());
		this.initLine(currTime);
		currTime.setHours(currTime.getHours() + 1);
	    }
	};

	this.initBack();

	this.clearAllShapes = function() {
	    for (idx in this.shapes) {
	        this.shapes[idx].remove();
	    };

	    this.shapes = [];
	};

	this.getXPosFromLaneIndex = function(lane) {
	    return lane * this.widthRatio + this.leftOffset;
	};

	this.getLengthFromDuration = function(duration) {
	    //TODO: is there an easier way?
	    var hoursPart = duration.toString().substring(0, 2);
	    var minutesPart = duration.toString().substring(3, 5);

	    var hours = parseInt(hoursPart);
	    var minutes = parseInt(minutesPart);

	    var t = new Date(2001, 01, 01);
	    var t2 = new Date(2001, 01, 01, hours, minutes);

	    var difference = t2.valueOf() - t.valueOf();
	    var retVal = this.projectPosition(difference);
	    return retVal;
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
	    
	    console.log('Creating shape for ' + lane + ' ' + time + ' ' + duration);
	    var parsedDate = this.parseDate(time);

	    var xpos = this.getXPosFromLaneIndex(lane);
	    var ypos = this.getRelativePosition(parsedDate);
	    var len = this.getLengthFromDuration(duration);

	    console.log('len is ' + len.toString());

	    var circle = this.paper.circle(xpos, ypos, len);
	    circle.attr({fill: Raphael.getColor()});
	    circle.attr({opacity: 0.05});

	    var text = this.createPath(xpos, ypos, len, 3);

	    st.push(circle);
	    st.push(text);

	    this.shapes.push(st);

	    return st;
	};

	this.update = function() {
	    var ctx = this;
	    var updateElements = function(data) {
		ctx.clearAllShapes();
		var values = data;

		for (idx in values) {
		    var val = values[idx];
		    console.log(val)
		    ctx.createShape(val.lane, val.time, val.duration);
		}
	    };

	    jQuery.ajax({
		url: this.updateUrl + this.dt.toString();
		success: function(data) {
		    updateElements(data);
		},
		error: function(jqXHR, textStatus, errorThrown) {
		    //TODO: Take callback arg for this
		    alert(errorThrown);
		},
		parserError: function() {
		    //TODO: Take callback arg for this
		    alert('failed to parse');
		}
	    });
	};

	this.setDate = function(dt) {
	    this.dt = dt;
	    this.update();
	};
    };

    ns.init = function(div, width, height) {
	var paper = Raphael(div, width, height);
	return new context(paper, width, height - 40, new Date(2011, 01, 01));
    };
})();