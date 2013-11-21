
function jsonparser() {
	var tableRef = document.getElementById('resultList');
	while ( tableRef.rows.length > 0 )
	{
		tableRef.deleteRow(0);
	}

	$.getJSON('http://mercury.dyndns.org:5984/mercury/_design/bi/_view/nyse?startkey=%221384142400000%22&endkey=%221384172149000%22', function(url_data) {
		$.each(url_data, function (i,element) {
			if ($.isArray(element) === true) {
				sortArrayBySymbol(element);
				$.each(element, function (i,value) {
					addElement(value.value.symbol,value.value.name,value.value.change,value.value.latest);    
				});
			}
		});
	});
}      

function getstockhistory(symbol,timeframe,type) {
	duration = timeframe;
	today = Date.today();
	console.log(today.valueOf());
	if (timeframe === "week") {
		timeframe = Date.today().add(-7).days();
		timeframe = timeframe.valueOf();
		console.log(timeframe);
	}
	if (timeframe === "month") {
		timeframe = Date.today().add(-30).days();
		timeframe = timeframe.valueOf();
		console.log(timeframe);
	} if (timeframe === "today") {
		timeframe = Date.today();
		timeframe = timeframe.valueOf();
		console.log(timeframe);
	}

	var tableRef = document.getElementById('resultList');
	var date1 = null;
	var date2 = null;
	datelist = [];
	datalist = [];
	latestlist = [];
	changelist = [];
	percentlist = [];
	barcolorlist = [];
	$.getJSON('http://mercury.dyndns.org:5984/mercury/_design/bi/_view/nyse_stock?startkey=[%22'+symbol+'%22,%22'+timeframe+'%22]&endkey=[%22'+symbol+'%22,%22'+today+'%22]', function(url_data) {
		$.each(url_data, function (i,element) {


			if ($.isArray(element) === true) {
					sortArrayByTime(element);
					element.reverse();
					setStockData(element);
					// console(getStockData().value.updated);

				$.each(getStockData(), function (i,value) {
					if (duration === "today") {
						var date = new Date(parseInt(value.value.updated));
						date2=date.toString("HH:mm");
						console.log("date2 in today "+date2);
						datalist.push(value.value.openVal);
						datelist.push(date2); 
					} 
					if (duration === "month" || duration === "week") {
						date2 = dateConvert(value.value.updated);
						if (date1 === null) {
							date1 = dateConvert(value.value.updated);
							console.log("Updated from null, now: "+date1);
							console.log(value.value.openVal);
							datalist.push(value.value.openVal);
							datelist.push(date2); 
							changelist.push(parseFloat(value.value.change));

						} else if (date1 === date2) {

						} else {
							console.log("parsedint " +parseInt(value.value.change));
					console.log (value.value.latest) ;
					date1 = date2;		
					datalist.push(value.value.openVal);
					datelist.push(date2); 
					changelist.push(parseFloat(value.value.change));
					console.log(changelist);
				}
			}
		});
			}
		});

		setDataList(datalist.reverse());
		setDateList(datelist.reverse());
		setLatestList(latestlist.reverse());
		setChangeList(changelist.reverse());
		// if (type == "bar") {
		// 	paintbarchart();
		// } else {
		// 	paintlinechart();
		// }

		// // setPercentList(percentlist.reverse());
		// // paintlinechart();
		// // paintbarchart();
		chartPaintSelector();
	});

}    

function setDateList(datalist) {
	dali = datalist;
}

function getDateList() {
	return dali;
}

function setDataList(datalist) {
	console.log("setdatalist "+datalist);
	datli = datalist;
}

function getDataList() {
	return datli;
}
function setLatestList(datalist) {
	lali = datalist;
}

function getLatestList() {
	return lali;
}
function setChangeList(datalist) {
	chali = datalist;
}

function getChangeList() {
	return chali;
}


function dateConvert(timeString) {
	var date = new Date(parseInt(timeString));
	date=date.toString("MMM dd");
	return date;
}


function addElement(Symbol,Name,Change,Value) {
	var ni = document.getElementsByTagName('tbody').item(0);
	var numi = document.getElementById('theValue');
	var num = (document.getElementById('theValue').value -1)+ 2;
	numi.value = num;
	var newdiv = document.createElement('tr');
	newdiv.className='clickableRow';
	newdiv.setAttribute('onClick', 'hidelist("'+Symbol+'","today");');
	// newdiv.addEventListener('click', hidelist(Symbol));
	cell = document.createElement("td");
	cellName = document.createElement("td");
	cellChange = document.createElement("td");
	cellValue = document.createElement("td");
	textnode = document.createTextNode(Symbol);
	textName = document.createTextNode(Name);
	textChange = document.createTextNode(Change);
	textValue = document.createTextNode(Value);
	cell.appendChild(textnode);
	cellName.appendChild(textName);
	cellChange.appendChild(textChange);
	cellValue.appendChild(textValue);
	newdiv.appendChild(cell);
	newdiv.appendChild(cellName);
	newdiv.appendChild(cellChange);
	newdiv.appendChild(cellValue);
	ni.appendChild(newdiv);
}





function paintlinechart() {
		console.log(getDataList());
	var lineChartData = {
		labels : datelist,
		datasets : [
		{
			fillColor : "rgba(151,187,205,0.5)",
			strokeColor : "rgba(151,187,205,1)",
			pointColor : "rgba(151,187,205,1)",
			pointStrokeColor : "#fff",
			data : datalist
		}
		]

	}

	var myLine = new Chart(document.getElementById("canvas").getContext("2d")).Line(lineChartData);
}

function paintbarchart() {
var cl = getChangeList();

for (var i = 0;i <= cl.length;i++) {
	if (cl[i] < 0 ) {
		cl[i] = Math.abs(cl[i]);
		barcolorlist.push("#DC3522");
	} else {
		barcolorlist.push("#374140");
	}
}
	console.log(barcolorlist);
console.log(getChangeList());
var barChartData = {
	labels : getDateList(),
	datasets : [
		{
			fillColor : barcolorlist,
			strokeColor : "rgba(151,187,205,1)",
			data : cl
		}
	]
}

	var bars = new Chart(document.getElementById("canvas").getContext("2d")).Bar(barChartData);

}


function setTopic(topic) {
	Topic = topic;
	console.log(Topic);
}


function setMarket(topic) {
	Market = topic;
}
function setSymbol(symbol) {
	Symbol = symbol;
}

function getSymbol() {
	return Symbol;
}


function getVars() {
	console.log(Topic+Market);
}

function chartPaintSelector() {
	console.log("chartPaintSelector "+getChartType());
	if(getChartType() === "bar") {
		paintbarchart();
	}
	if (getChartType() === "line") {
		paintlinechart();
	}
	// con = document.querySelector("#canvas");
	// chart = document.querySelector("#barcanvas");
	// chart.className = 'visuallyhidden';
	// con.className = 'visible';
}
function hideLine() {

	// con = document.querySelector("#canvas");
	// chart = document.querySelector("#barcanvas");
	// con.className = 'visuallyhidden';
	// chart.className = 'visible';
	paintbarchart();

}

function setChartType(ctype) {
charttype = ctype;
}
function getChartType() {
	return charttype;
}

function hidelist(Symbol,timeframe) {
	setSymbol(Symbol);
	con = document.querySelector("#listdisp");
	chart = document.querySelector("#statView");
	con.className = 'visuallyhidden';
	chart.className = 'visible';
	radiobtn = document.getElementById("day");
	radiobtn.checked = true;
	setChartType("line");
	getstockhistory(Symbol,timeframe);
}


function showlist() {
	con = document.querySelector("#listdisp");
	chart = document.querySelector("#statView");
	datalist = [0];
	datelist = [0];
	chart.className = 'visuallyhidden';
	con.className = 'visible';

}

function sortArrayByTime(data) {
	data.sort(function (a, b) {
		a = a.value.updated;
		b = b.value.updated;
		return a.localeCompare(b);
	});
}

function sortArrayBySymbol(data) {
	data.sort(function (a, b) {
		a = a.value.symbol;
		b = b.value.symbol;
		return a.localeCompare(b);
	});
}


function setStockData(stockdata) {
	astockdata = stockdata;
}
function getStockData() {
	return astockdata;
}