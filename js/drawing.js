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

	    if (dateString.length > 10) {
		//Make it compatible with emacs dates which include the day 
		//of the week between the date and time
		dateString = dateString.replace(/Mon |Tue |Wed |Thu |Fri |Sat |Sun /gi, "");
		var hour = parseInt(dateString.substring(11, 13));
		var minute = parseInt(dateString.substring(14, 16));
	    }
	    else {
		var hour = 0;
		var minute = 0;
	    }
	    console.log('year: ' + year.toString() + '. month: ' + month.toString() +
			'. day: ' + day.toString());
	    var dt = new Date(year, month - 1, day, hour, minute, 0, 0);

	    console.log('Parsed date: ' + dt.toString());
	    return dt;
	};

	this.lanes = {};

	this.dt = dt;

	this.updateUrl = updateUrl;
	console.log('Context ctor. updateUrl: ' + this.updateUrl);

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


	this.getLaneIndex = function(lane) {
	    var idx = 0;
	    if (lane in this.lanes) {
		idx = this.lanes[lane];
	    }
	    else {
		//If it's an int, interpret as an index
		var asIdx = parseInt(lane);
		if (!isNaN(asIdx)) {
		    idx = asIdx;
		}
		else {
		    var max = -1;
		    for (key in this.lanes) {
			if (this.lanes[key] > max) {
			    max = this.lanes[key];
			}
		    }
		    idx = max + 1;
		}
		this.lanes[lane] = idx;
	    }
	    console.log('Index for lane ' + lane + ' is ' + idx.toString())
	    return idx;
	}

	this.getXPosFromLane = function(lane) {
	    var idx = this.getLaneIndex(lane);
	    return idx * this.widthRatio + this.leftOffset;
	};

	this.durationStringToEpoch = function(duration) {
	    //TODO: is there an easier way?
	    var hoursPart = duration.toString().substring(0, 2);
	    var minutesPart = duration.toString().substring(3, 5);

	    var hours = parseInt(hoursPart);
	    var minutes = parseInt(minutesPart);

	    var t = new Date(2001, 00, 01);
	    var t2 = new Date(2001, 00, 01, hours, minutes);

	    var difference = t2.valueOf() - t.valueOf();
	    return difference
	};

	this.getLengthFromDuration = function(durationInEpoch) {
	    var retVal = this.projectPosition(durationInEpoch);
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

	    var nodeHead1 = this.paper.circle(xpos, ypos, pathWidth * 0.75)
	    	.attr({fill: '#222'});
	    var nodeHead2 = this.paper.circle(xpos, ypos + len, pathWidth * 0.75)
	    	.attr({fill: '#222'});

	    st.push(path);
	    st.push(nodeHead1);
	    st.push(nodeHead2);
	    return st;
	};

	this.createShape = function(lane, time, durationInEpoch) {
	    var st = this.paper.set();
	    
	    console.log('Creating shape for ' + lane + ' ' + time + ' ' + durationInEpoch);
	    var parsedDate = this.parseDate(time);

	    var xpos = this.getXPosFromLane(lane);
	    var ypos = this.getRelativePosition(parsedDate);
	    var len = this.getLengthFromDuration(durationInEpoch);

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
	    thisNs = this;
	    var updateElements = function(data) {
		ctx.clearAllShapes();
		var values = data;

		for (idx in values) {
		    var val = values[idx];
		    console.log(val)
		    if ("duration" in val) {
			duration = thisNs.durationStringToEpoch(val.duration);
		    }
		    else {
			duration = (thisNs.parseDate(val.to) - thisNs.parseDate(val.from));
		    }
		    ctx.createShape(val.lane, val.from, duration);
		}
	    };
	    
	    var url = this.updateUrl + this.dt.getFullYear().toString() + '-' +
		(this.dt.getMonth() + 1).toString().lpad('0', 2) + '-' + 
		this.dt.getDate().toString().lpad('0', 2);
	    console.log('url is: ' + this.updateUrl);

	    jQuery.ajax({
		url: url,
		dataType: 'json', 
		success: function(data) {
		    updateElements(data);
		},
		error: function(jqXHR, textStatus, errorThrown) {
		    //TODO: Take callback arg for this
		    console.log(errorThrown.toString());
		},
		parserError: function() {
		    //TODO: Take callback arg for this
		    console.log('failed to parse');
		}
	    });
	};

	this.setDate = function(dt) {
	    this.dt = dt;
	    this.update();
	};
    };

    ns.init = function(div, width, height, url) {
	var paper = Raphael(div, width, height);
	return new context(paper, width, height - 40, new Date(2011, 00, 01), url);
    };
})();